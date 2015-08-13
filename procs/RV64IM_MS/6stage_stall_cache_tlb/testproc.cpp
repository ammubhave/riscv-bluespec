#include <errno.h>
#include <stdio.h>
#include <bitset>
#include <cassert>
#include "ProcIndication.h"
#include "ProcRequest.h"
#include "GeneratedTypes.h"
#include "VmhLoadImage.h"

#define CONNECTAL_MEMORY

static ProcRequestProxy *procRequestProxy = 0;

#ifdef CONNECTAL_MEMORY
const size_t mem_buffer_sz = 64;  // 512 Bytes
const size_t mem_buffer_words = mem_buffer_sz / sizeof(uint64_t);

uint64_t *mem = NULL;
const  size_t mem_sz = 64*1024*1024;  // 64 MB
#else
uint64_t *mem = NULL;
const size_t mem_sz = 8; // 64 bytes
sem_t done_req;
#endif

static void call_from_host(bool isfromhost, uint64_t v) {
    procRequestProxy->from_host(isfromhost, v);
}

#ifdef CONNECTAL_MEMORY
static void call_imem_resp(uint64_t data) {
    procRequestProxy->imem_resp(data);
}
static void call_dmem_resp(uint64_t data) {
    procRequestProxy->dmem_resp(data);
}
#else
/*static void call_mem_req(uint8_t op, uint64_t addr, uint64_t data) {
    procRequestProxy->mem_req(op, addr, data);
    sem_wait(&done_req);
}*/
#endif

class ProcIndication : public ProcIndicationWrapper {
 public:
    virtual void to_host(uint64_t v) {
        uint8_t device = v >> 56;
        uint8_t cmd = v >> 48;
        uint64_t payload = v & 0xFFFFFFFFFFFFULL;
        switch (device) {
            case 0:  // dev EXIT
                switch (cmd) {
                    case 0:  // cmd EXIT
                        if (payload == 0)
                            fprintf(stderr, "PASSED\n");
                        else
                            fprintf(stderr, "FAILED %d\n", (int)payload);
                        exit(0);
                }
                break;
            case 1:  // dev CONSOLE
                switch (cmd) {
                    case 1:  // cmd PUT CHAR
                        fprintf(stderr, "%c", (char)payload);
                        break;
                }
                break;
        }
        call_from_host(false, 0);
    }

#ifdef CONNECTAL_MEMORY
    virtual void imem_req(uint8_t op, uint64_t addr, uint64_t data) {
        if (op == false) {  // Ld
            //printf("imem_req ld: %p %p", (void*)addr, (void*)mem[addr / 8]);
            call_imem_resp(mem[addr / 8]);
        } else {
            mem[addr / 8] = data;
        }
    }

    virtual void dmem_req(uint8_t op, uint64_t addr, uint64_t data) {
        if (op == false) {  // Ld
            call_dmem_resp(mem[addr / 8]);
        } else {
            mem[addr / 8] = data;
        }
    }
#else
    virtual void mem_resp(uint64_t data) {
        *mem = data;
        sem_post(&done_req);
    }
#endif

    explicit ProcIndication(unsigned int id) : ProcIndicationWrapper(id) {}
};

static void call_start(uint64_t startpc) {
    printf("Starting... %p\n", (void*)startpc);
    procRequestProxy->start(startpc);
}

int main(int argc, const char **argv) {
    long actualFrequency = 0;
    long requestedFrequency = 1e9 / MainClockPeriod;

    ProcIndication procIndication(IfcNames_ProcIndicationH2S);
    procRequestProxy = new ProcRequestProxy(IfcNames_ProcRequestS2H);

#ifdef CONNECTAL_MEMORY
    mem = (uint64_t*) malloc(mem_sz);
    assert(vmhLoadImage("memory.vmh", (uint64_t*)mem, mem_sz)
            && "Failed to load VMH image");
#endif

    int status = setClockFrequency(0, requestedFrequency, &actualFrequency);
    printf("Requested main clock frequency %5.2f, actual clock frequency %5.2f MHz status=%d errno=%d\n",
      (double)requestedFrequency * 1.0e-6,
      (double)actualFrequency * 1.0e-6,
      status, (status != 0) ? errno : 0);

    uint64_t startpc = 0x200;
    call_start(startpc);
    while (1) continue;
    return 0;
}
