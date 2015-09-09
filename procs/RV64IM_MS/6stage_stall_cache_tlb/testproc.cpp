#include <errno.h>
#include <stdio.h>
#include <bitset>
#include <cassert>
#include <fcntl.h>
#include <string>
#include <iostream>
#include <sys/stat.h>
#include <vector>
#include <sstream>
#include "dmaManager.h"
#include "ProcIndication.h"
#include "ProcRequest.h"
#include "GeneratedTypes.h"
#include "VmhLoadImage.h"

#define CONNECTAL_MEMORY

static ProcRequestProxy *procRequestProxy = 0;

const  size_t mem_sz = 64*1024*1024;  // 64 MB
uint64_t *memBuffer;

static void call_from_host(uint64_t v) {
    procRequestProxy->from_host(v);
}
static void call_set_refPointer(unsigned int refPointer) {
    procRequestProxy->set_refPointer(refPointer);
}
static void to_host_respond(uint64_t tohost, uint64_t resp) {
    //uint64_t payload = tohost & 0xFFFFFFFFFFFFULL;
    call_from_host((tohost >> 48 << 48) | (resp << 16 >> 16));
    //printf("\nfromhost: %x\n", 0x100 | (uint8_t)payload);
}
/*
struct request_t
{
    uint64_t addr;
    uint64_t offset;
    uint64_t size;
    uint64_t tag;
};
int fd;
std::string id;
size_t size;
void blk_read(uint64_t v) {
    uint64_t payload = v & 0xFFFFFFFFFFFFULL;
    request_t req;
    memcpy(&req, ((char*)(uint64_t)&memBuffer + payload), sizeof(req));
    std::vector<uint8_t> buf(req.size);
    if ((size_t)::pread(fd, &buf[0], buf.size(), req.offset) != req.size)
        throw std::runtime_error("could not read");
    memcpy((char*)((uint64_t)&memBuffer + req.addr), &buf[0], buf.size());
    to_host_respond(v, req.tag);
}
void blk_write(uint64_t v) {
    uint64_t payload = v & 0xFFFFFFFFFFFFULL;
    request_t req;
    memcpy(&req, ((char*)(uint64_t)&memBuffer + payload), sizeof(req));
    std::vector<uint8_t> buf2(req.size);
    memcpy(&buf2[0], ((char*)(uint64_t)&memBuffer + req.addr), buf2.size());
    if ((size_t)::pwrite(fd, &buf2[0], buf2.size(), req.offset) != req.size)
        throw std::runtime_error("could not write");
    to_host_respond(v, req.tag);
}*/

class ProcIndication : public ProcIndicationWrapper {
 public:
    virtual void to_host(uint64_t v) {
        uint8_t device = v >> 56;
        uint8_t cmd = v >> 48;
        uint64_t payload = v & 0xFFFFFFFFFFFFULL;

        uint64_t addr = payload >> 8;
        uint8_t what = payload & 0xFF;

        if (device != 1 || cmd != 1)
            fprintf(stderr, "{%d %d %lx %lx}\n", device, cmd, payload, addr);
        switch (device) {
            case 0:  // dev EXIT
                switch (cmd) {
                    case 0:  // cmd EXIT
                        if (payload == 0)
                            fprintf(stderr, "PASSED\n");
                        else
                            fprintf(stderr, "FAILED %d\n", (int)payload);
                        exit(0);
                    case 0xFF:
                        if (what == 0xFF)
                            strcpy((char*)&memBuffer[addr / 8], "ext");
                        else
                            memBuffer[addr / 8] = 0;
                        fprintf(stderr, "Identified {%d %d %lx}\n", device, cmd, payload);
                        to_host_respond(v, 1);
                        break;
                    default:
                        fprintf(stderr, "{%d %d %lx}\n", device, cmd, payload);
                }
                break;
            case 1:  // dev CONSOLE
                switch (cmd) {
                    case 0:  // cmd READ CHAR
                        fprintf(stderr, "{%d %d %lx}\n", device, cmd, payload);
                        call_from_host(0);
                        break;
                    case 1:  // cmd PUT CHAR
                        fprintf(stderr, "%c", (char)payload);
                        to_host_respond(v, 0x100 | (uint8_t)payload);
                        break;
                    case 0xFF:
                        if (what == 0xFF)
                            strcpy((char*)&memBuffer[addr / 8], "bcd");
                        else if (what == 0x00)
                            strcpy((char*)&memBuffer[addr / 8], "read");
                        else if (what == 0x01)
                            strcpy((char*)&memBuffer[addr / 8], "write");
                        else
                            memBuffer[addr / 8] = 0;
                        fprintf(stderr, "{%d %d %lx}\n", device, cmd, payload);
                        to_host_respond(v, 1);
                        break;
                    default:
                        fprintf(stderr, "{%d %d %lx}\n", device, cmd, payload);
                }
                break;
            /*case 2:  // dev BLK
                switch (cmd) {
                    case 0:  // cmd READ CHAR
                        blk_read(v);
                        break;
                    case 1:  // cmd PUT CHAR
                        blk_write(v);
                        break;
                    case 0xFF:
                        if (what == 0xFF)
                            strcpy((char*)&memBuffer[addr / 8], id.c_str());
                        else if (what == 0x00)
                            strcpy((char*)&memBuffer[addr / 8], "read");
                        else if (what == 0x01)
                            strcpy((char*)&memBuffer[addr / 8], "write");
                        else
                            memBuffer[addr / 8] = 0;
                        to_host_respond(v, 1);
                        break;
                    default:
                        fprintf(stderr, "{%d %d %lx}\n", device, cmd, payload);
                }
                break;*/
            default:
                switch (cmd) {
                    case 0xFF:
                        memBuffer[addr / 8] = 0;
                        fprintf(stderr, "{%d %d %lx}\n", device, cmd, payload);
                        to_host_respond(v, 1);
                        break;
                    default:
                        fprintf(stderr, "{%d %d %lx}\n", device, cmd, payload);
                }
                fprintf(stderr, "{%d %d %lx}\n", device, cmd, payload);
        }
        usleep(50);
       // call_from_host(false, 0);
    }
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
    DmaManager *dma = platformInit();
    int memAlloc = portalAlloc(mem_sz, 0);
    memBuffer = (uint64_t*)portalMmap(memAlloc, mem_sz);

    assert(vmhLoadImage("memory.vmh", (uint64_t*)memBuffer, mem_sz)
            && "Failed to load VMH image");

    unsigned int ref_memAlloc = dma->reference(memAlloc);
    call_set_refPointer(ref_memAlloc);

    /*fd = ::open("disk.img", O_RDWR);
    if (fd < 0)
        throw std::runtime_error("could not open disk.img");

    struct stat st;
    if (fstat(fd, &st) < 0)
        throw std::runtime_error("could not stat disk.img");

    size = st.st_size;
    std::stringstream ss;
    ss << size;
    id = "disk size=" + ss.str();*/
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
