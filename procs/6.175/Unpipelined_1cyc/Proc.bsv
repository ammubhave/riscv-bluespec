import Types::*;
import ProcTypes::*;
import MemoryTypes::*;
import RFile::*;
import CsrFile::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import ExecPriv::*;
import Cop::*;
import Vector::*;

interface ProcRequest;
  method Action start(Bit#(64) startpc);
  method Action from_host(Bool isfromhost, Bit#(64) v);
endinterface

interface ProcIndication;
  method Action to_host(Bit#(64) v);
endinterface

interface Proc;
   interface ProcRequest request;
endinterface

module mkProc#(ProcIndication indication)(Proc);
  Reg#(Addr)       pc <- mkRegU;
  RFile           rf <- mkRFile;
  CsrFile       csrf <- mkCsrFile;
  IMemory       iMem <- mkIMemory;
  DMemory       dMem <- mkDMemory;
  Reg#(Bool) started <- mkReg(False);

  rule doProc(started);
    Instruction inst = truncate(gatherLoad(pc, unpack(zeroExtend(4'b1111)), True, iMem.req(pc)));

    // decode
    let dInst = decode(inst);

    // trace - print the instruction
    $display("pc: %h inst: (%h) expanded: ", pc, inst, showInst(inst));

    // read register values
    let rVal1 = rf.rd1(validValue(dInst.src1));
    let rVal2 = rf.rd2(validValue(dInst.src2));
    let csrState = csrf.rd(dInst.csr);

    if (isValid(csrState.csr))
      $display("csrr %h == %h, %h", validValue(csrState.csr), csrState.data);

    // execute
    let eInst = exec(dInst, rVal1, rVal2, csrState, pc, ?));

    // Executing unsupported instruction. Exiting
    if(eInst.iType == Unsupported)
    begin
      $fwrite(stderr, "Executing unsupported instruction at pc: %x. Exiting\n", pc);
      $finish();
    end

    // memory
    if(eInst.iType == Ld)
    begin
      let data <- dMem.req(MemReq{op: Ld, addr: eInst.addr, byteEn: eInst.byteEn, data: ?});
      eInst.data = gatherLoad(eInst.addr, eInst.byteEn, eInst.unsignedLd, data);
    end
    else if(eInst.iType == St)
    begin
      match {.byteEn, .data} = scatterStore(eInst.addr, eInst.byteEn, eInst.data);
      let d <- dMem.req(MemReq{op: St, addr: eInst.addr, byteEn: byteEn, data: data});
    end

    // write back
    if(isValid(eInst.dst))
      rf.wr(validValue(eInst.dst), eInst.data);

    // update the pc depending on whether the branch is taken or not
    pc <= eInst.brTaken ? eInst.addr : pc + 4;

    // Csr write for debugging and stats
    csrf.wr(eInst.csrState);
  endrule

  rule csrfToCop;
    let ret <- csrf.csrfToHost;
    indication.to_host(ret);
  endrule

 interface ProcRequest request;
    method Action start(Bit#(64) startpc);
      started <= True;
      pc <= startpc;
    endmethod

    method Action from_host(Bool isfromhost, Bit#(64) v);
      csrf.hostToCsrf(isfromhost, v);
    endmethod
 endinterface
endmodule
