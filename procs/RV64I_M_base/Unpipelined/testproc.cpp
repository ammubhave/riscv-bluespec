#include <errno.h>
#include <stdio.h>
#include "ProcIndication.h"
#include "ProcRequest.h"
#include "GeneratedTypes.h"

static ProcRequestProxy *procRequestProxy = 0;

static void call_from_host(bool isfromhost, uint64_t v)
{
    procRequestProxy->from_host(isfromhost, v);
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

    int status = setClockFrequency(0, requestedFrequency, &actualFrequency);
    printf("Requested main clock frequency %5.2f, actual clock frequency %5.2f MHz status=%d errno=%d\n",
      (double)requestedFrequency * 1.0e-6,
      (double)actualFrequency * 1.0e-6,
      status, (status != 0) ? errno : 0);

    uint64_t startpc = 0x200;
    call_start(startpc);
    while (1) ;
    return 0;
}
