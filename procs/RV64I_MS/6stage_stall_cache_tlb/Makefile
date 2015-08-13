PROC=RV64I_MS/6stage_stall_cache_tlb

S2H_INTERFACES = ProcRequest:Proc.request
H2S_INTERFACES = Proc:ProcIndication
BSVFILES = Proc.bsv
CPPFILES = testproc.cpp VmhLoadImage.cpp
NUMBER_OF_MASTERS = 0
CONNECTALFLAGS += --bscflags="+RTS -K24000000 -RTS" --nocache
#MAIN_CLOCK_PERIOD = 200
CONNECTALFLAGS += --mainclockperiod=24

include $(RISCV_HOME)/procs/Makefile.connectal

