PROC=RV64IM_MS/6stage_stall_cache_tlb

S2H_INTERFACES = ProcRequest:Proc.request
H2S_INTERFACES = Proc:ProcIndication
MEM_INTERFACES = lProc.dmaReadClient,lProc.dmaWriteClient
BSVFILES = Proc.bsv
CPPFILES = testproc.cpp

include $(RISCV_HOME)/procs/Makefile.connectal
