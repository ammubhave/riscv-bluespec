// See LICENSE for license details.

#include "pk.h"
#include "atomic.h"
#include "frontend.h"
#include "sbi.h"
#include "mcall.h"
#include "syscall.h"
#include <stdint.h>
#include <string.h>

uint64_t tohost_sync(unsigned dev, unsigned cmd, uint64_t payload)
{
  uint64_t fromhost;
  __sync_synchronize();

  sbi_device_message m = {dev, cmd, payload}, *p;
  do_mcall(MCALL_SEND_DEVICE_REQUEST, &m);
  while ((p = (void*)do_mcall(MCALL_RECEIVE_DEVICE_RESPONSE)) == 0);
  kassert(p == &m);

  __sync_synchronize();
  return m.data;
}

long frontend_syscall(long n, long a0, long a1, long a2, long a3, long a4, long a5, long a6)
{
 // printmm("req %lx %lx %lx %lx\n", n, a0, a1, a2);
//  static volatile uint64_t magic_mem[8];

  static spinlock_t lock = SPINLOCK_INIT;
  spinlock_lock(&lock);
/*
  magic_mem[0] = n;
  magic_mem[1] = a0;
  magic_mem[2] = a1;
  magic_mem[3] = a2;
  magic_mem[4] = a3;
  magic_mem[5] = a4;
  magic_mem[6] = a5;
  magic_mem[7] = a6;
*/
  //tohost_sync(0, 0, (uintptr_t)magic_mem);

  long ret = n; // magic_mem[0];

  char* cha1 = (char*)a1;
  char* cha2 = (char*)a2;
  struct mainvars* args = (struct mainvars*)a0;
  char* linux_base = (char*)0x20000;
  uintptr_t* s;
  uintptr_t *d;

  switch(n) {
    case SYS_exit:
      tohost_sync(0, 0, a0);
      break;
    case SYS_write:
      while (*cha1) {
        tohost_sync(1, 1, *cha1++);
      }
      break;
    case SYS_getmainvars:
      args->argc = 4;
      args->argv[0] = "bbl";
      args->argv[1] = "-m128";
      args->argv[2] = "-p1";
      args->argv[3] = "vmlinux";
      ret = 0;
      break;
    case SYS_openat:
      if (strcmp(cha1, "vmlinux") == 0) { // loading linux kernel
        ret = 3;
      } else {
        while (*cha1) {
          tohost_sync(1, 1, *cha1++);
        }
        tohost_sync(0, 0, n);
      }
      break;
    case SYS_pread:
      if (a0 == 3) {
        memcpy(a1, linux_base + a3, a2);
        ret = a2;
        break;
      }
      tohost_sync(0, 0, n);
      break;
    case SYS_close:
      if (a0 == 3) break;
      else {
        tohost_sync(0, 0, n);
        break;
      }
    default: 
      tohost_sync(0, 0, n);
      break;
  }
  

  spinlock_unlock(&lock);
  return ret;
}

void die(int code)
{
  frontend_syscall(SYS_exit, code, 0, 0, 0, 0, 0, 0);
  while (1);
}
