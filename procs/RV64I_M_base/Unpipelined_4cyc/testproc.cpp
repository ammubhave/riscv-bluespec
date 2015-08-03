#include <errno.h>
#include <stdio.h>
#include <bitset>
#include <cassert>
#include "ProcIndication.h"
#include "ProcRequest.h"
#include "GeneratedTypes.h"
#include "VmhLoadImage.h"

static ProcRequestProxy *procRequestProxy = 0;

const size_t mem_buffer_sz = 64; // 512 Bytes
const size_t mem_buffer_words = mem_buffer_sz / sizeof(uint64_t);

uint64_t (*mem)/*[mem_buffer_words]*/ = NULL;
const  size_t mem_sz = 64*1024*1024; // 64 MB

static void call_from_host(bool isfromhost, uint64_t v)
{
    procRequestProxy->from_host(isfromhost, v);
}
static void call_imem_resp(uint64_t data)
{
    procRequestProxy->imem_resp(data);
}
static void call_dmem_resp(uint64_t data)
{
    procRequestProxy->dmem_resp(data);
}

class ProcIndication : public ProcIndicationWrapper
{
public:
    virtual void to_host(uint64_t v) {
        uint64_t magic_mem[8] __attribute__((aligned(64))) = (void*)((uint64_t)mem + v);

        uint8_t device = v >> 56;
        uint8_t cmd = v >> 48;
        uint64_t payload = v << 16 >> 16;

        if (payload & 0x1) {
            if (arg0 == 0)
                fprintf(stderr, "PASSED\n");
            else
                fprintf(stderr, "FAILED %d\n", (int)payload);
            exit(0);
        }

        switch (device) {
            case 0: // dev FRONT END
                long which = magic_mem[0];
                long arg0 = magic_mem[1];
                long arg1 = magic_mem[2];
                long arg2 = magic_mem[3];

                switch (which) {
                    case SYS_write: // SYS_write
                        fprintf(stderr, "%s", (char*)((uint64_t)mem + arg1));
                        break;
                }
                break;
        }
        call_from_host(false, 0);
    }

    virtual void imem_req(uint8_t op, uint64_t addr, uint64_t data) {
        if (op == false) { // Ld
            printf("imem_req ld: %p %p %p %p %p", (void*)addr, (void*)(addr / 8), (void*)mem[addr / 8], (void*)mem[0], (void*)mem[64]);
            call_imem_resp(mem[addr / 8]);
        } else {
            mem[addr / 8] = data;
        }
    }

    virtual void dmem_req(uint8_t op, uint64_t addr, uint64_t data) {
        if (op == false) { // Ld
            call_dmem_resp(mem[addr / 8]);
        } else {
            mem[addr / 8] = data;
        }
    }

    ProcIndication(unsigned int id) : ProcIndicationWrapper(id) {}
};

static void call_start(uint64_t startpc)
{
    printf("Starting... %p\n", (void*)startpc);
    procRequestProxy->start(startpc);
}

int main(int argc, const char **argv)
{
    long actualFrequency = 0;
    long requestedFrequency = 1e9 / MainClockPeriod;

    ProcIndication procIndication(IfcNames_ProcIndicationH2S);
    procRequestProxy = new ProcRequestProxy(IfcNames_ProcRequestS2H);

    mem = (uint64_t(*)) malloc(mem_sz);

    assert(vmhLoadImage("memory.vmh", (uint64_t*)mem, mem_sz) && "Failed to load VMH image");

    int status = setClockFrequency(0, requestedFrequency, &actualFrequency);
    printf("Requested main clock frequency %5.2f, actual clock frequency %5.2f MHz status=%d errno=%d\n",
      (double)requestedFrequency * 1.0e-6,
      (double)actualFrequency * 1.0e-6,
      status, (status != 0) ? errno : 0);

    uint64_t startpc = 0x200;
    call_start(startpc);
    while (1);
    return 0;
}
