PROC=RV64I_M_base/Unpipelined_4cyc

S2H_INTERFACES = ProcRequest:Proc.request
H2S_INTERFACES = Proc:ProcIndication
BSVFILES = Proc.bsv
CPPFILES = testproc.cpp VmhLoadImage.cpp
NUMBER_OF_MASTERS = 0

include $(RISCV_HOME)/procs/Makefile.connectal
