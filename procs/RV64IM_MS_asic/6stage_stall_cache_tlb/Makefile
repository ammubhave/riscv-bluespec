PROC=RV64IM_MS_asic/6stage_stall_cache_tlb

S2H_INTERFACES = ProcRequest:Proc.request
H2S_INTERFACES = Proc:ProcIndication
MEM_INTERFACES = lProc.dmaReadClient,lProc.dmaWriteClient
BSVFILES = Proc.bsv
CPPFILES = testproc.cpp VmhLoadImage.cpp
NUMBER_OF_MASTERS = 1
CONNECTALFLAGS += --bscflags="+RTS -K24000000 -RTS" --nocache
#MAIN_CLOCK_PERIOD = 200
CONNECTALFLAGS += --mainclockperiod=128
CONNECTALFLAGS += -D SIM_DMA_READ_LATENCY=1 -D SIM_DMA_WRITE_LATENCY=1
#RUN_ARGS = -V $(RISCV_HOME)/build/dump.vcd

include $(RISCV_HOME)/procs/Makefile.connectal
