#include <errno.h>
#include <stdio.h>
#include <bitset>
#include <cassert>
#include "dmaManager.h"
#include "ProcIndication.h"
#include "ProcRequest.h"
#include "GeneratedTypes.h"
#include "VmhLoadImage.h"

static ProcRequestProxy *procRequestProxy = 0;

int imemAlloc;
int dmemAlloc;
uint64_t* imemBuffer = NULL;
uint64_t* dmemBuffer = NULL;
uint64_t imemAddr = 0;
uint64_t dmemAddr = 0;
const size_t mem_buffer_sz = 64; // 512 Bytes
const size_t mem_buffer_words = mem_buffer_sz / sizeof(uint64_t);

uint64_t (*mem)[mem_buffer_words] = NULL;
const  size_t mem_sz = 64*1024*1024; // 64 MB

static void call_from_host(bool isfromhost, uint64_t v)
{
    procRequestProxy->from_host(isfromhost, v);
}
static void call_imem_resp()
{
    procRequestProxy->imem_resp();
}
static void call_dmem_resp()
{
    procRequestProxy->dmem_resp();
}

class ProcIndication : public ProcIndicationWrapper
{
public:
    virtual void to_host(uint64_t v) {
        uint8_t device = v >> 56;
        uint8_t cmd = v >> 48;
        uint64_t payload = v & 0xFFFFFFFFFFFFULL;
        switch (device) {
            case 0: // dev EXIT
                switch (cmd) {
                    case 0: // cmd EXIT
                        if (payload == 0)
                            fprintf(stderr, "PASSED\n");
                        else
                            fprintf(stderr, "FAILED %d\n", (int)payload);
                        exit(0);
                }
                break;
            case 1: // dev CONSOLE
                switch (cmd) {
                    case 1: // cmd PUT CHAR
                        fprintf(stderr, "%c", (char)payload);
                        break;
                }
                break;
        }
        call_from_host(false, 0);
    }

    virtual void imem_req(uint64_t addr, ) {
        memcpy(&mem[imemAddr / sizeof(imemBuffer)], imemBuffer, mem_buffer_sz);
        memcpy(imemBuffer, &mem[addr / sizeof(imemBuffer)], mem_buffer_sz);
        portalCacheFlush(imemAlloc, imemBuffer, mem_buffer_sz, 1);
        imemAddr = addr;
        call_imem_resp();
    }

    virtual void dmem_req(uint64_t addr) {
        memcpy(&mem[dmemAddr / sizeof(dmemBuffer)], dmemBuffer, mem_buffer_sz);
        memcpy(dmemBuffer, &mem[addr / sizeof(dmemBuffer)], mem_buffer_sz);
        portalCacheFlush(dmemAlloc, dmemBuffer, mem_buffer_sz, 1);
        dmemAddr = addr;
        call_dmem_resp();
    }

    ProcIndication(unsigned int id) : ProcIndicationWrapper(id) {}
};

static void call_start(uint64_t startpc, unsigned int imp, unsigned int dmp)
{
    printf("Starting... %p %p %p\n", (void*)startpc, (void*)(uint64_t)imp, (void*)(uint64_t)dmp);
    procRequestProxy->start(startpc, imp, dmp);
}

int main(int argc, const char **argv)
{
    long actualFrequency = 0;
    long requestedFrequency = 1e9 / MainClockPeriod;

    ProcIndication procIndication(IfcNames_ProcIndicationH2S);
    procRequestProxy = new ProcRequestProxy(IfcNames_ProcRequestS2H);
    DmaManager *dma = platformInit();

    imemAlloc = portalAlloc(mem_buffer_sz, 0);
    imemBuffer = (uint64_t*)portalMmap(imemAlloc, mem_buffer_sz);

    dmemAlloc = portalAlloc(mem_buffer_sz, 0);
    dmemBuffer = (uint64_t*)portalMmap(dmemAlloc, mem_buffer_sz);

    mem = (uint64_t(*)[mem_buffer_words]) malloc(mem_sz);

    assert(vmhLoadImage("memory.vmh", (uint64_t*)mem, mem_sz) && "Failed to load VMH image");

    memcpy(imemBuffer, &mem[0], mem_buffer_sz);
    imemAddr = 0;
    memcpy(dmemBuffer, &mem[0], mem_buffer_sz);
    dmemAddr = 0;

    portalCacheFlush(imemAlloc, imemBuffer, mem_buffer_sz, 1);
    portalCacheFlush(dmemAlloc, dmemBuffer, mem_buffer_sz, 1);
    unsigned int ref_imemAlloc = dma->reference(imemAlloc);
    unsigned int ref_dmemAlloc = dma->reference(dmemAlloc);
    sleep(1);

    int status = setClockFrequency(0, requestedFrequency, &actualFrequency);
    printf("Requested main clock frequency %5.2f, actual clock frequency %5.2f MHz status=%d errno=%d\n",
      (double)requestedFrequency * 1.0e-6,
      (double)actualFrequency * 1.0e-6,
      status, (status != 0) ? errno : 0);

    uint64_t startpc = 0x200;
    call_start(startpc, ref_imemAlloc, ref_dmemAlloc);
    while (1) ;
    return 0;
}
