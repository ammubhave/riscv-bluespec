// REGFILE_MEMORY, FPGA_MEMORY, CONNECTAL_MEMORY
`define REGFILE_MEMORY
`define MEMORY_NOCACHE

import AddrPred::*;
import Cache::*;
import CsrFile::*;
import ClientServer::*;
import Connectable::*;
import Decode::*;
import Ehr::*;
import Exec::*;
import ExecPriv::*;
import Fifo::*;
import GetPut::*;
import MemoryTypes::*;
import MemUtil::*;
import Types::*;
import ProcTypes::*;
import RFile::*;
import Scoreboard::*;
import Tlb::*;
import Vector::*;

`ifdef REGFILE_MEMORY
import RegfileMemory::*;
`elsif FPGA_MEMORY
import FPGAMemory::*;
`endif

interface ProcRequest;
  method Action start(Bit#(64) startpc);
  method Action from_host(Bool isfromhost, Bit#(64) v);

`ifdef CONNECTAL_MEMORY
  method Action imem_resp(Bit#(64) data);
  method Action dmem_resp(Bit#(64) data);
`else
  method Action mem_req(Bit#(1) op, Bit#(64) addr, Bit#(64) data);
`endif
endinterface

interface ProcIndication;
  method Action to_host(Bit#(64) v);

`ifdef CONNECTAL_MEMORY
  method Action imem_req(Bit#(1) op, Bit#(64) addr, Bit#(64) data);
  method Action dmem_req(Bit#(1) op, Bit#(64) addr, Bit#(64) data);
`else
  method Action mem_resp(Bit#(64) data);
`endif
endinterface

interface Proc;
  interface ProcRequest request;
endinterface

module mkProc#(ProcIndication indication)(Proc);
  Reg#(Addr) pc <- mkRegU;

  RFile      rf <- mkRFile;
  CsrFile  csrf <- mkCsrFile;

`ifdef REGFILE_MEMORY
  WideMem wideMem <- mkRegfileMemory;
`elsif FPGA_MEMORY
  WideMem wideMem <- mkFPGAMemory;
`endif

`ifndef CONNECTAL_MEMORY
  Vector#(2, WideMem) wideMems <- mkSplitWideMem( wideMem );
`endif

`ifdef MEMORY_NOCACHE
  Cache      iMem <- mkDummyCache;
  Cache      dMem <- mkDummyCache;
`else
  Cache      iMem <- mkCache;
  Cache      dMem <- mkCache;
`endif

  Reg#(Bool) started <- mkReg(False);
  AddrPred addrPred <- mkBtb;

  Fifo#(2, Fetch2Decode)   f2d <- mkCFFifo;
  Fifo#(2, Decode2RegRead) d2rf <- mkCFFifo;
  Fifo#(2, RegRead2Exec)   rf2ex <- mkCFFifo;
  Fifo#(2, Exec2Mem)      ex2m <- mkCFFifo;
  Fifo#(2, Mem2Wb)        m2wb <- mkCFFifo;
  Fifo#(2, Tuple2#(RegRead2Exec, ExecInst))       e12e2 <- mkCFFifo;

  Tlb iTlb <- mkTlb;
  Tlb dTlb <- mkTlb;

`ifndef CONNECTAL_MEMORY
  mkConnection(wideMems[0].to_proc, iMem.to_mem);
  mkConnection(wideMems[1].to_proc, dMem.to_mem);
`endif

  mkConnection(iMem.to_proc, iTlb.to_mem);
  mkConnection(dMem.to_proc, dTlb.to_mem);

  Fifo#(2, Tuple3#(Addr, Addr, Bool)) f12f2 <- mkCFFifo;// mkBypassFifo;
  Fifo#(2, Exec2Mem) m12m2 <- mkCFFifo;// mkBypassFifo;

  Scoreboard#(16)           sb <- mkCFScoreboard;
  Fifo#(2, Tuple2#(Redirect, CsrState))     execRedirect <- mkCFFifo;
  Reg#(Bool)            fEpoch <- mkReg(False);
  Reg#(Bool)            eEpoch <- mkReg(False);
  Ehr#(2, Bool)         privIn <- mkEhr(False);
  Reg#(Bool)            stallFetch <- mkReg(False);

  rule doFetch1(started &&
                (!execRedirect.notEmpty ||
                 (execRedirect.notEmpty &&
                    (!isSystem(tpl_1(execRedirect.first).brType) ||
                     (isSystem(tpl_1(execRedirect.first).brType) && !ex2m.notEmpty && !m12m2.notEmpty)))));
    if(execRedirect.notEmpty)
    begin
      if (tpl_1(execRedirect.first).brType != Interrupt)
        addrPred.update(tpl_1(execRedirect.first));
      execRedirect.deq;
      if (isSystem(tpl_1(execRedirect.first).brType)) begin
        if (tpl_1(execRedirect.first).brType == Priv || tpl_1(execRedirect.first).brType == Interrupt) begin
          iTlb.flush;
          dTlb.flush;
        end
        CsrState state = tpl_2(execRedirect.first);
        if (!isValid(state.csr)) begin
          state.csr = Valid(CSRmstatus);
          state.data = state.mstatus;
        end
        iTlb.updateCsr(validValue(state.csr), state.data);
        dTlb.updateCsr(validValue(state.csr), state.data);
      end
    end
    if(execRedirect.notEmpty && (tpl_1(execRedirect.first).mispredict || isSystem(tpl_1(execRedirect.first).brType)))
    begin
      pc <= tpl_1(execRedirect.first).nextPc;
      fEpoch <= !fEpoch;
    end
    else if (!execRedirect.notEmpty || !isSystem(tpl_1(execRedirect.first).brType))
    begin
      let ppc = addrPred.predPc(pc);
      pc <= ppc;

      iTlb.to_proc.request.put(MemReq{op: Ld, addr: pc, byteEn: replicate(True), data: ?});
      f12f2.enq(tuple3(pc, ppc, fEpoch));

      $display("Fetch1: pc: %h epoch: %d", pc, fEpoch);
    end
  endrule

  rule doFetch2;
    match {.inst_data, .cause} <- iTlb.to_proc.response.get;
    match {.pc, .ppc, .fEpoch} = f12f2.first;

    Instruction inst = truncate(gatherLoad(pc, unpack(zeroExtend(4'b1111)), True, inst_data));

    f12f2.deq;
    f2d.enq(Fetch2Decode{pc: pc, ppc: ppc, epoch: fEpoch, inst: inst, cause: cause});

    $display("Fetch2: pc: %h epoch: %d inst: (%h) expanded: ", pc, fEpoch, inst, showInst(inst));
  endrule

  rule doDecode;
    let pc = f2d.first.pc;
    let ppc = f2d.first.ppc;
    let epoch = f2d.first.epoch;
    let inst = f2d.first.inst;
    let cause = f2d.first.cause;

    let dInst = decode(inst);
    d2rf.enq(Decode2RegRead{pc: pc, ppc: ppc, epoch: epoch, dInst: dInst, cause: cause});
    f2d.deq;

    $display("Decode: pc: %h epoch: %d", pc, epoch);
  endrule

  rule doRegRead;
    let pc = d2rf.first.pc;
    let ppc = d2rf.first.ppc;
    let epoch = d2rf.first.epoch;
    let dInst = d2rf.first.dInst;
    let cause = d2rf.first.cause;

    let raw = sb.search1(dInst.src1) || sb.search2(dInst.src2) || privIn[0];

    let rVal1 = rf.rd1(validValue(dInst.src1));
    let rVal2 = rf.rd2(validValue(dInst.src2));
    let csrState = csrf.rd(dInst.csr);

    if(!raw && !isValid(cause))
    begin
      // Interrupts?
      let pi = getPendingInterrupt(csrState);
      if (isValid(pi)) begin
        $display("Pending Interrupt: %h", validValue(pi));
        dInst.iType = Interrupt;
        dInst.src1 = Invalid;
        dInst.src2 = Invalid;
        dInst.dst = Invalid;
        dInst.csr = Invalid;
        dInst.imm = pi;
        dInst.brFunc = NT;
        csrState.csr = Invalid;
      end

      rf2ex.enq(RegRead2Exec{pc: pc, ppc: ppc, dInst: dInst, epoch: epoch, rVal1: rVal1, rVal2: rVal2, csrState: csrState, cause: cause});
      sb.insert(dInst.dst);
      privIn[0] <= isSystem(dInst.iType);
      d2rf.deq;

      $display("RegRead: pc: %h", pc);
    end else if (isValid(cause)) begin
      dInst.dst = Valid(0);
      rf2ex.enq(RegRead2Exec{pc: pc, ppc: ppc, dInst: dInst, epoch: epoch, rVal1: rVal1, rVal2: rVal2, csrState: csrState, cause: cause});
      sb.insert(dInst.dst);
      d2rf.deq;
      $display("RegRead: Exception pc: %h", pc);
    end
  endrule

  rule doExec;
    let dInst  = rf2ex.first.dInst;
    let pc     = rf2ex.first.pc;
    let ppc    = rf2ex.first.ppc;
    let epoch  = rf2ex.first.epoch;
    let rVal1  = rf2ex.first.rVal1;
    let rVal2  = rf2ex.first.rVal2;
    let csrState = rf2ex.first.csrState;
    let cause = rf2ex.first.cause;
    rf2ex.deq;

    // We use a poisoned bit to denote whether an instruction is killed or not. If the epochs match, the instruction is not killed, and hence the poisoned bit is not set. Otherwise poisoned bit is set
    let poisoned = epoch != eEpoch;


    let eInst = ((isSystem(dInst.iType)) ? execPriv(dInst, rVal1, rVal2, csrState, pc, ppc) : exec(dInst, rVal1, rVal2, csrState, pc, ppc));
/*
    e12e2.enq(tuple2(rf2ex.first, eInst));
    rf2ex.deq;
  endrule

  rule doExec2;
    match {.varr, .eInst} = e12e2.first;
    let dInst  = varr.dInst;
    let pc     = varr.pc;
    let ppc    = varr.ppc;
    let epoch  = varr.epoch;
    let rVal1  = varr.rVal1;
    let rVal2  = varr.rVal2;
    let csrState = varr.csrState;
    let cause = varr.cause;

    let poisoned = epoch != eEpoch;

    e12e2.deq;*/

    if(!poisoned)
    begin
      if(eInst.iType == Unsupported || isValid(cause))
      begin
        $fwrite(stderr, "Executing unsupported instruction at pc: %x. Exiting\n", pc);
        $finish();
      end

      if(eInst.iType == J || eInst.iType == Jr || eInst.iType == Br || isSystem(eInst.iType)/* || eInst.iType == Priv*/)
        execRedirect.enq(tuple2(Redirect{pc: pc, nextPc: eInst.addr, brType: eInst.iType, taken: eInst.brTaken, mispredict: eInst.mispredict}, eInst.csrState));
      if(eInst.mispredict || isSystem(eInst.iType))
        eEpoch <= !eEpoch;

      $display("Execute: pc: %h epoch: %d", pc, epoch);
    end
    ex2m.enq(Exec2Mem{poisoned: poisoned, iType: eInst.iType, dst: eInst.dst, data: eInst.data, csrState: eInst.csrState, byteEn: eInst.byteEn, unsignedLd: eInst.unsignedLd, addr: eInst.addr});
  endrule

  rule doMem1;
    let poisoned = ex2m.first.poisoned;
    let iType = ex2m.first.iType;
    let dst = ex2m.first.dst;
    let data = ex2m.first.data;
    let csrState = ex2m.first.csrState;
    let byteEn = ex2m.first.byteEn;
    let unsignedLd = ex2m.first.unsignedLd;
    let addr = ex2m.first.addr;

    if(!poisoned)
    begin
      if(iType == Ld)
      begin
        dTlb.to_proc.request.put(MemReq{op: Ld, addr: addr, byteEn: byteEn, data: ?});
      end
      else if(iType == St)
      begin
        match {.byteEn_st, .data_st} = scatterStore(addr, byteEn, data);
        dTlb.to_proc.request.put(MemReq{op: St, addr: addr, byteEn: byteEn_st, data: data_st});
      end
    end

    m12m2.enq(ex2m.first);
    ex2m.deq;
  endrule

  rule doMem2;
    let poisoned = m12m2.first.poisoned;
    let iType = m12m2.first.iType;
    let dst = m12m2.first.dst;
    let data = m12m2.first.data;
    let csrState = m12m2.first.csrState;
    let byteEn = m12m2.first.byteEn;
    let unsignedLd = m12m2.first.unsignedLd;
    let addr = m12m2.first.addr;

    if(!poisoned)
    begin
      if(iType == Ld || iType == Lr)
      begin
        match {.d, .mcause} <- dTlb.to_proc.response.get;
        if (isValid(mcause)) begin
          $fwrite(stderr, "Load access exception at %x. Exiting\n", addr);
          $finish();
        end
        data = gatherLoad(addr, byteEn, unsignedLd, d);
      end
    end
    m2wb.enq(Mem2Wb{poisoned: poisoned, iType: iType, dst: dst, data: data, csrState: csrState});
    m12m2.deq;
  endrule

  rule doWb;
    let poisoned = m2wb.first.poisoned;
    let iType = m2wb.first.iType;
    let dst = m2wb.first.dst;
    let data = m2wb.first.data;
    let csrState = m2wb.first.csrState;

    if(!poisoned)
    begin
      if(isValid(dst))
        rf.wr(validValue(dst), data);
      csrf.wr(csrState);
    end

    sb.remove;
    if (isSystem(iType))
      privIn[1] <= False;
    m2wb.deq;
  endrule

  rule csrfToHost;
    let ret <- csrf.csrfToHost;
    indication.to_host(ret);
  endrule

`ifdef CONNECTAL_MEMORY
  rule iMemToHost;
    let d <- iMem.to_mem.request.get;
    if (d.op == Ld) begin
      indication.imem_req(0, d.addr, d.data);
    end else begin
      indication.imem_req(1, d.addr, d.data);
    end
  endrule

  rule dMemToHost;
    let d <- dMem.to_mem.request.get;
    if (d.op == Ld) begin
      indication.dmem_req(0, d.addr, d.data);
    end else begin
      indication.dmem_req(1, d.addr, d.data);
    end
  endrule
`else
  rule memToHost;
    let d <- wideMem.to_host.response.get;
    indication.mem_resp(d);
  endrule
`endif

  interface ProcRequest request;
    method Action start(Bit#(64) startpc);
      started <= True;
      pc <= startpc;
    endmethod

    method Action from_host(Bool isfromhost, Bit#(64) v);
      csrf.hostToCsrf(isfromhost, v);
    endmethod

  `ifdef CONNECTAL_MEMORY
    method Action imem_resp(Bit#(64) data);
      iMem.to_mem.response.put(data);
    endmethod

    method Action dmem_resp(Bit#(64) data);
      dMem.to_mem.response.put(data);
    endmethod
  `else
    method Action mem_req(Bit#(1) op, Bit#(64) addr, Bit#(64) data);
      if (op == 0)  // Ld
        wideMem.to_host.request.put(WideMemReq{op: Ld, addr: addr, data: data, byteEn: unpack('1)});
      else
        wideMem.to_host.request.put(WideMemReq{op: St, addr: addr, data: data, byteEn: unpack('1)});
    endmethod
  `endif
  endinterface
endmodule
