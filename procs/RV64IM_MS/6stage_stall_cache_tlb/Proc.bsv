import Types::*;
import ProcTypes::*;
import MemTypes::*;
import RFile::*;
import CsrFile::*;
//import FullWideMemory::*;
import FullMemory::*;
import Decode::*;
import Exec::*;
import ExecPriv::*;
import Cop::*;
import Fifo::*;
import Ehr::*;
import Scoreboard::*;
import AddrPred::*;
import Tlb::*;
//import WideCache::*;
import Connectable::*;
import Vector::*;

import ConnectalMemory::*;
import MemTypes::*;
import MemreadEngine::*;
import MemwriteEngine::*;

interface Proc;
  interface ProcRequest request;
  interface Vector#(1, MemReadClient#(64)) dmaReadClient;
  interface Vector#(1, MemWriteClient#(64)) dmaWriteClient;
endinterface

typedef struct {
  Addr pc;
  Addr ppc;
  Bool epoch;
  Instruction inst;
  Maybe#(Exception) cause;
} Fetch2Decode deriving (Bits, Eq);

typedef struct {
  Addr pc;
  Addr ppc;
  Bool epoch;
  DecodedInst dInst;
  Maybe#(Exception) cause;
} Decode2RegRead deriving (Bits, Eq);

typedef struct {
  Addr pc;
  Addr ppc;
  Bool epoch;
  DecodedInst dInst;
  Data rVal1;
  Data rVal2;
  CsrState csrState;
  Maybe#(Exception) cause;
} RegRead2Exec deriving (Bits, Eq);

typedef struct {
  Bool poisoned;
  IType iType;
  Maybe#(RIndx) dst;
  Data data;
  CsrState csrState;
  Addr addr;
  ByteEn byteEn;
  Bool unsignedLd;
} Exec2Mem deriving (Bits, Eq);

typedef struct {
  Bool poisoned;
  IType iType;
  Maybe#(RIndx) dst;
  Data data;
  CsrState csrState;
} Mem2Wb deriving (Bits, Eq);

module mkProc#(ProcIndication indication)(Proc);
  Reg#(Addr) pc <- mkRegU;

  RFile      rf <- mkRFile;
  CsrFile  csrf <- mkCsrFile;
  //WideMemory    mem <- mkWideMemory;
  Memory    mem <- mkMemory;
  Cop       cop <- mkCop;
  AddrPred addrPred <- mkBtb;
  
  Fifo#(2, Fetch2Decode)   f2d <- mkCFFifo;
  Fifo#(2, Decode2RegRead) d2rf <- mkCFFifo;
  Fifo#(2, RegRead2Exec)   rf2ex <- mkCFFifo;
  Fifo#(2, Exec2Mem)      ex2m <- mkCFFifo;
  Fifo#(2, Mem2Wb)        m2wb <- mkCFFifo;

  Tlb iTlb <- mkTlb;
  Tlb dTlb <- mkTlb;
  //WideCache iCache <- mkWideCache;
  //WideCache dCache <- mkWideCache;

  Fifo#(1, Tuple3#(Addr, Addr, Bool)) f12f2 <- mkBypassFifo;
  Fifo#(1, Exec2Mem) m12m2 <- mkBypassFifo;

  Scoreboard#(16)           sb <- mkCFScoreboard;
  Fifo#(1, Tuple2#(Redirect, CsrState))     execRedirect <- mkBypassFifo;
  Reg#(Bool)            fEpoch <- mkReg(False);
  Reg#(Bool)            eEpoch <- mkReg(False);
  Ehr#(2, Bool)         privIn <- mkEhr(False);
  Reg#(Bool)            stallFetch <- mkReg(False);

  rule doFetch1(cop.started && 
                (!execRedirect.notEmpty || 
                 (execRedirect.notEmpty &&
                    (!isSystem(tpl_1(execRedirect.first).brType) ||
                     (isSystem(tpl_1(execRedirect.first).brType) && !ex2m.notEmpty && !m12m2.notEmpty)))));
    if(execRedirect.notEmpty)
    begin
      addrPred.update(tpl_1(execRedirect.first));
      execRedirect.deq;
      if (isSystem(tpl_1(execRedirect.first).brType)) begin
        if (tpl_1(execRedirect.first).brType == Priv) begin
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

      iTlb.req(MemReq{op: Ld, addr: pc, byteEn: replicate(True), data: ?});
      f12f2.enq(tuple3(pc, ppc, fEpoch));

      $display("Fetch1: pc: %h epoch: %d", pc, fEpoch);
    end
  endrule

  rule doFetch2(cop.started);
    match {.inst_data, .cause} <- iTlb.resp;
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

    // We use a poisoned bit to denote whether an instruction is killed or not. If the epochs match, the instruction is not killed, and hence the poisoned bit is not set. Otherwise poisoned bit is set
    let poisoned = epoch != eEpoch;


    let eInst = ((isSystem(dInst.iType) /*|| isValid(getPendingInterrupt(csrState))*/) ? execPriv(dInst, rVal1, rVal2, csrState, pc, ppc) : exec(dInst, rVal1, rVal2, csrState, pc, ppc));

    if(!poisoned)
    begin
      if(eInst.iType == Unsupported || isValid(cause))
      begin
        $fwrite(stderr, "Executing unsupported instruction at pc: %x. Exiting\n", pc);
        $finish;
      end

      if(eInst.iType == J || eInst.iType == Jr || eInst.iType == Br || isSystem(eInst.iType)/* || eInst.iType == Priv*/)
        execRedirect.enq(tuple2(Redirect{pc: pc, nextPc: eInst.addr, brType: eInst.iType, taken: eInst.brTaken, mispredict: eInst.mispredict}, eInst.csrState));
      if(eInst.mispredict || isSystem(eInst.iType))
        eEpoch <= !eEpoch;

      $display("Execute: pc: %h epoch: %d", pc, epoch);
    end
    ex2m.enq(Exec2Mem{poisoned: poisoned, iType: eInst.iType, dst: eInst.dst, data: eInst.data, csrState: eInst.csrState, byteEn: eInst.byteEn, unsignedLd: eInst.unsignedLd, addr: eInst.addr});
    rf2ex.deq;
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
        dTlb.req(MemReq{op: Ld, addr: addr, byteEn: byteEn, data: ?});
      end
      else if(iType == St)
      begin
        match {.byteEn_st, .data_st} = scatterStore(addr, byteEn, data);
        dTlb.req(MemReq{op: St, addr: addr, byteEn: byteEn_st, data: data_st});
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
        match {.d, .mcause} <- dTlb.resp;
        if (isValid(mcause)) begin
          $fwrite(stderr, "Load access exception at %x. Exiting\n", addr);
          $finish;
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

  rule csrfToCop;
    let ret <- csrf.csrfToCop;
    cop.csrfToCop(ret);
  endrule

  /*mkConnection(iCache.req, iTlb.memReq);
  mkConnection(iCache.resp, iTlb.memResp);
  mkConnection(dCache.req, dTlb.memReq);
  mkConnection(dCache.resp, dTlb.memResp);

  mkConnection(mem.iReq, iCache.memReq);
  mkConnection(mem.iResp, iCache.memResp);
  mkConnection(mem.dReq, dCache.memReq);
  mkConnection(mem.dResp, dCache.memResp);*/
  mkConnection(mem.iReq, iTlb.memReq);
  mkConnection(mem.iResp, iTlb.memResp);
  mkConnection(mem.dReq, dTlb.memReq);
  mkConnection(mem.dResp, dTlb.memResp);

  rule csrfToHost;
    let ret <- csrf.csrfToHost;
    indication.to_host(ret);
  endrule

  interface ProcRequest request;
    method Action start(Bit#(64) startpc, Bit#(32) mp);
      cop.start;
      pc <= startpc;
      memPointer <= mp;
    endmethod
    
    method Action from_host(Bool isfromhost, Bit#(64) v);
      csrf.hostToCsrf(isfromhost, v);
    endmethod
  endinterface

  interface MemReadClient dmaReadClient = cons(re.dmaClient, nil);
  interface MemWriteClient dmaWriteClient = cons(we.dmaClient, nil);
endmodule
