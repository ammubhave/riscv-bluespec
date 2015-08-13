// See LICENSE for license details.

#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <limits.h>
#include "util.h"

#define SYS_write 64
#define SYS_exit 93
#define SYS_stats 1234

// initialized in crt.S
int have_vec;

static long handle_frontend_syscall(long which, long arg0, long arg1, long arg2)
{
  volatile uint64_t magic_mem[8] __attribute__((aligned(64)));
  magic_mem[0] = which;
  magic_mem[1] = arg0;
  magic_mem[2] = arg1;
  magic_mem[3] = arg2;
  __sync_synchronize();
  //write_csr(mtohost, (long)magic_mem);
  write_csr(mtohost, ((long)1 << 56) | ((long)1 << 48) | arg0);
  while (swap_csr(mfromhost, 0) == 0);
  return magic_mem[0];
}

// In setStats, we might trap reading uarch-specific counters.
// The trap handler will skip over the instruction and write 0,
// but only if a0 is the destination register.
#define read_csr_safe(reg) ({ register long __tmp asm("a0"); \
  asm volatile ("csrr %0, " #reg : "=r"(__tmp)); \
  __tmp; })

#define NUM_COUNTERS 18
static long counters[NUM_COUNTERS];
static char* counter_names[NUM_COUNTERS];
static int handle_stats(int enable)
{
  //use csrs to set stats register
  if (enable)
    asm volatile ("csrrs a0, stats, 1" ::: "a0");
  int i = 0;
#define READ_CTR(name) do { \
    while (i >= NUM_COUNTERS) ; \
    long csr = read_csr_safe(name); \
    if (!enable) { csr -= counters[i]; counter_names[i] = #name; } \
    counters[i++] = csr; \
  } while (0)
  READ_CTR(cycle);   READ_CTR(instret);
  READ_CTR(uarch0);  READ_CTR(uarch1);  READ_CTR(uarch2);  READ_CTR(uarch3);
  READ_CTR(uarch4);  READ_CTR(uarch5);  READ_CTR(uarch6);  READ_CTR(uarch7);
  READ_CTR(uarch8);  READ_CTR(uarch9);  READ_CTR(uarch10); READ_CTR(uarch11);
  READ_CTR(uarch12); READ_CTR(uarch13); READ_CTR(uarch14); READ_CTR(uarch15);
#undef READ_CTR
  if (!enable)
    asm volatile ("csrrc a0, stats, 1" ::: "a0");
  return 0;
}

void tohost_exit(long code)
{
  write_csr(mtohost, code);
  while (1);
}

/* For Software Traps of Illegal Instructions */
uint64_t umul64(uint64_t a, uint64_t b) {
  uint64_t res = 0;
  while(b != 0)             // Iterate the loop till b==0
  {
    if (b & 0x1)                // Bitwise &  of the value of b with 01
    {
      res = res + a; // Add a to result if b is odd .
    }
    a <<= 1;                   // Left shifting the value contained in 'a' by 1
                               // multiplies a by 2 for each loop
    b >>= 1;                   // Right shifting the value contained in 'b' by 1.
  }
  return res;
}
void umul64wide(uint64_t a, uint64_t b, uint64_t *hi, uint64_t *lo)
{
    uint64_t a_lo = (uint64_t)(uint32_t)a;
    uint64_t a_hi = a >> 32;
    uint64_t b_lo = (uint64_t)(uint32_t)b;
    uint64_t b_hi = b >> 32;

    uint64_t p0 = umul64(a_lo, b_lo); // a_lo * b_lo;
    uint64_t p1 = umul64(a_lo, b_hi); // a_lo * b_hi;
    uint64_t p2 = umul64(a_hi, b_lo); // a_hi * b_lo;
    uint64_t p3 = umul64(a_hi, b_hi); // a_hi * b_hi;

    uint32_t cy = (uint32_t)(((p0 >> 32) + (uint32_t)p1 + (uint32_t)p2) >> 32);

    if (lo)
      *lo = p0 + (p1 << 32) + (p2 << 32);
    if (hi)
      *hi = p3 + (p1 >> 32) + (p2 >> 32) + cy;
}
void mul64wide(int64_t a, int64_t b, int64_t *hi, int64_t *lo)
{
  umul64wide((uint64_t)a, (uint64_t)b, (uint64_t*)hi, (uint64_t*)lo);
  if (hi) {
    if (a < 0LL) *hi -= b;
    if (b < 0LL) *hi -= a;
  }
}
void sumul64wide(int64_t a, uint64_t b, int64_t *hi, int64_t *lo)
{
  umul64wide((uint64_t)a, b, (uint64_t*)hi, (uint64_t*)lo);
  if (hi) {
    if (a < 0LL) *hi -= b;
  }
}

long handle_trap(long cause, long epc, long long regs[32])
{
  int* csr_insn;
  asm ("jal %0, 1f; csrr a0, stats; 1:" : "=r"(csr_insn));
  long sys_ret = 0;

  if (cause == 0x12)
    uint32_t inst = *(uint32_t*)epc;
    int opcode = inst & 0x7F; inst >>= 7;
    int rd     = inst & 0x1F; inst >>= 5;
    int funct3 = inst & 0x7; inst >>= 3;
    int rs1    = inst & 0x1F; inst >>= 5;
    int rs2    = inst & 0x1F; inst >>= 5;
    int funct7 = inst & 0x7F;

    if (opcode == MATCH_ADD) {  // Op
      uint64_t src1 = regs[rs1];
      uint64_t src2 = regs[rs2];
      if (funct7 == 0x1) {  // MULDIV
        if (funct3 == 0x0) {  // MUL rd,rs1,rs2
          regs[rd] = umul64(src1, src2);
        } else if (funct3 == 0x1) {  // MULH rd,rs1,rs2
          mul64wide(src1, src2, &regs[rd], NULL);
        } else if (funct3 == 0x2) {  // MULHSU rd,rs1,rs2
          sumul64wide(src1, src2, &regs[rd], NULL);
        } else if (funct3 == 0x3) {  // MULHU rd,rs1,rs2
          umul64wide(src1, src2, &regs[rd], NULL);
        }
      }
    }
    return epc + 4;
  }
  else if (cause == CAUSE_ILLEGAL_INSTRUCTION)
     if ((*(int*)epc & *csr_insn) == *csr_insn);
  else if (cause != CAUSE_USER_ECALL)
    tohost_exit(1337);
  else if (regs[17] == SYS_exit)
    tohost_exit(regs[10]);
  else if (regs[17] == SYS_stats)
    sys_ret = handle_stats(regs[10]);
  else
    sys_ret = handle_frontend_syscall(regs[17], regs[10], regs[11], regs[12]);

  regs[10] = sys_ret;
  return epc+4;
}

static long syscall(long num, long arg0, long arg1, long arg2)
{
  register long a7 asm("a7") = num;
  register long a0 asm("a0") = arg0;
  register long a1 asm("a1") = arg1;
  register long a2 asm("a2") = arg2;
  asm volatile ("scall" : "+r"(a0) : "r"(a1), "r"(a2), "r"(a7));
  return a0;
}

void exit(int code)
{
  syscall(SYS_exit, code, 0, 0);
  while (1);
}

void setStats(int enable)
{
  syscall(SYS_stats, enable, 0, 0);
}

void printstr(const char* s)
{
  while (*s != '\0')
    syscall(SYS_write, *s++, 0, 0);
  //syscall(SYS_write, 1, (long)s, strlen(s));
}

void __attribute__((weak)) thread_entry(int cid, int nc)
{
  // multi-threaded programs override this function.
  // for the case of single-threaded programs, only let core 0 proceed.
  while (cid != 0);
}

int __attribute__((weak)) main(int argc, char** argv)
{
  // single-threaded programs override this function.
  printstr("Implement main(), foo!\n");
  return -1;
}

static void init_tls()
{
  register void* thread_pointer asm("tp");
  extern char _tls_data;
  extern __thread char _tdata_begin, _tdata_end, _tbss_end;
  size_t tdata_size = &_tdata_end - &_tdata_begin;
  memcpy(thread_pointer, &_tls_data, tdata_size);
  size_t tbss_size = &_tbss_end - &_tdata_end;
  memset(thread_pointer + tdata_size, 0, tbss_size);
}

void _init(int cid, int nc)
{
  init_tls();
  thread_entry(cid, nc);

  // only single-threaded programs should ever get here.
  int ret = main(0, 0);

  char buf[NUM_COUNTERS * 32] __attribute__((aligned(64)));
  char* pbuf = buf;
  for (int i = 0; i < NUM_COUNTERS; i++)
    if (counters[i])
      pbuf += sprintf(pbuf, "%s = %d\n", counter_names[i], counters[i]);
  if (pbuf != buf)
    printstr(buf);

  exit(ret);
}

#undef putchar
int putchar(int ch)
{
  syscall(SYS_write, ch, 0, 0);
  /*
  static __thread char buf[64] __attribute__((aligned(64)));
  static __thread int buflen = 0;

  buf[buflen++] = ch;

  if (ch == '\n' || buflen == sizeof(buf))
  {
    syscall(SYS_write, 1, (long)buf, buflen);
    buflen = 0;
  }*/

  return 0;
}

void printhex(uint64_t x)
{
  char str[17];
  int i;
  for (i = 0; i < 16; i++)
  {
    str[15-i] = (x & 0xF) + ((x & 0xF) < 10 ? '0' : 'a'-10);
    x >>= 4;
  }
  str[16] = 0;

  printstr(str);
}

unsigned divu10(unsigned n);
unsigned remu10(unsigned n);
unsigned mulu10(unsigned n);

static void printnum(void (*putch)(int, void**), void **putdat,
                    unsigned long long num, unsigned base, int width, int padc)
{
  unsigned digs[sizeof(num)*CHAR_BIT];
  int pos = 0;

  while (1)
  {
    if (base == 16)
      digs[pos++] = (num & 0xF);// + ((num & 0xF) < 10 ? '0' : 'a'-10);
    else if (base == 10)
      digs[pos++] = remu10(num);
    else
      digs[pos++] = num % base;

    if (num < base)
      break;

    if (base == 16)
      num >>= 4;
    else if (base == 10)
      num = divu10(num);
    else
      num /= base;
  }

  while (width-- > pos)
    putch(padc, putdat);

  while (pos-- > 0)
    putch(digs[pos] + (digs[pos] >= 10 ? 'a' - 10 : '0'), putdat);
}

static unsigned long long getuint(va_list *ap, int lflag)
{
  if (lflag >= 2)
    return va_arg(*ap, unsigned long long);
  else if (lflag)
    return va_arg(*ap, unsigned long);
  else
    return va_arg(*ap, unsigned int);
}

static long long getint(va_list *ap, int lflag)
{
  if (lflag >= 2)
    return va_arg(*ap, long long);
  else if (lflag)
    return va_arg(*ap, long);
  else
    return va_arg(*ap, int);
}

static void vprintfmt(void (*putch)(int, void**), void **putdat, const char *fmt, va_list ap)
{
  register const char* p;
  const char* last_fmt;
  register int ch, err;
  unsigned long long num;
  int base, lflag, width, precision, altflag;
  char padc;

  while (1) {
    while ((ch = *(unsigned char *) fmt) != '%') {
      if (ch == '\0')
        return;
      fmt++;
      putch(ch, putdat);
    }
    fmt++;

    // Process a %-escape sequence
    last_fmt = fmt;
    padc = ' ';
    width = -1;
    precision = -1;
    lflag = 0;
    altflag = 0;
  reswitch:
    switch (ch = *(unsigned char *) fmt++) {

    // flag to pad on the right
    case '-':
      padc = '-';
      goto reswitch;

    // flag to pad with 0's instead of spaces
    case '0':
      padc = '0';
      goto reswitch;

    // width field
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      for (precision = 0; ; ++fmt) {
        precision = precision * 10 + ch - '0';
        ch = *fmt;
        if (ch < '0' || ch > '9')
          break;
      }
      goto process_precision;

    case '*':
      precision = va_arg(ap, int);
      goto process_precision;

    case '.':
      if (width < 0)
        width = 0;
      goto reswitch;

    case '#':
      altflag = 1;
      goto reswitch;

    process_precision:
      if (width < 0)
        width = precision, precision = -1;
      goto reswitch;

    // long flag (doubled for long long)
    case 'l':
      lflag++;
      goto reswitch;

    // character
    case 'c':
      putch(va_arg(ap, int), putdat);
      break;

    // string
    case 's':
      if ((p = va_arg(ap, char *)) == NULL)
        p = "(null)";
      if (width > 0 && padc != '-')
        for (width -= strnlen(p, precision); width > 0; width--)
          putch(padc, putdat);
      for (; (ch = *p) != '\0' && (precision < 0 || --precision >= 0); width--) {
        putch(ch, putdat);
        p++;
      }
      for (; width > 0; width--)
        putch(' ', putdat);
      break;

    // (signed) decimal
    case 'd':
      num = getint(&ap, lflag);
      if ((long long) num < 0) {
        putch('-', putdat);
        num = -(long long) num;
      }
      base = 10;
      goto signed_number;

    // unsigned decimal
    case 'u':
      base = 10;
      goto unsigned_number;

    // (unsigned) octal
    case 'o':
      // should do something with padding so it's always 3 octits
      base = 8;
      goto unsigned_number;

    // pointer
    case 'p':
      static_assert(sizeof(long) == sizeof(void*));
      lflag = 1;
      putch('0', putdat);
      putch('x', putdat);
      /* fall through to 'x' */

    // (unsigned) hexadecimal
    case 'x':
      base = 16;
    unsigned_number:
      num = getuint(&ap, lflag);
    signed_number:
      printnum(putch, putdat, num, base, width, padc);
      break;

    // escaped '%' character
    case '%':
      putch(ch, putdat);
      break;

    // unrecognized escape sequence - just print it literally
    default:
      putch('%', putdat);
      fmt = last_fmt;
      break;
    }
  }
}

int printf(const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);

  vprintfmt((void*)putchar, 0, fmt, ap);

  va_end(ap);
  return 0; // incorrect return value, but who cares, anyway?
}

int sprintf(char* str, const char* fmt, ...)
{
  va_list ap;
  char* str0 = str;
  va_start(ap, fmt);

  void sprintf_putch(int ch, void** data)
  {
    char** pstr = (char**)data;
    **pstr = ch;
    (*pstr)++;
  }

  vprintfmt(sprintf_putch, (void**)&str, fmt, ap);
  *str = 0;

  va_end(ap);
  return str - str0;
}

unsigned remu10(unsigned n) {
  return n-mulu10(divu10(n));
}
unsigned divu10(unsigned n) {
    unsigned q, r;
    q = (n >> 1) + (n >> 2);
    q = q + (q >> 4);
    q = q + (q >> 8);
    q = q + (q >> 16);
    q = q >> 3;
    r = n - (((q << 2) + q) << 1);
    return q + (r > 9);
}
unsigned mulu10(unsigned x) {
  return (x << 3) + (x << 1);
}