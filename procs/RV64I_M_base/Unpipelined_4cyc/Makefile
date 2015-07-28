PROC=RV64I_M_base/Unpipelined_4cyc

S2H_INTERFACES = ProcRequest:Proc.request
H2S_INTERFACES = Proc:ProcIndication
MEM_INTERFACES = lProc.dmaReadClient,lProc.dmaWriteClient
BSVFILES = Proc.bsv
CPPFILES = testproc.cpp VmhLoadImage.cpp

include $(RISCV_HOME)/procs/Makefile.connectal
