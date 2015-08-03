import Cache::*;
import ClientServer::*;
import Connectable::*;
import CsrFile::*;
import Decode::*;
import DMemory::*;
import Exec::*;
import ExecPriv::*;
import GetPut :: *;
import IMemory::*;
import MemoryTypes::*;
import ProcTypes::*;
import RFile::*;
import Types::*;
import Vector::*;

interface ProcRequest;
  method Action start(Bit#(64) startpc);
  method Action from_host(Bool isfromhost, Bit#(64) v);
  method Action imem_resp(Bit#(64) data);
  method Action dmem_resp(Bit#(64) data);
endinterface

interface ProcIndication;
  method Action to_host(Bit#(64) v);
  method Action imem_req(Bit#(1) op, Bit#(64) addr, Bit#(64) data);
  method Action dmem_req(Bit#(1) op, Bit#(64) addr, Bit#(64) data);
endinterface

interface Proc;
  interface ProcRequest request;
endinterface

typedef enum {Fetch1, Fetch2, Execute1, Execute2} State deriving (Bits, Eq);

module mkProc#(ProcIndication indication)(Proc);
  Reg#(Addr) pc <- mkRegU;
  RFile      rf <- mkRFile;
  CsrFile  csrf <- mkCsrFile;
  Cache      iMem <- mkCache;
  Cache      dMem <- mkCache;
  Reg#(Bool) started <- mkReg(False);

  Reg#(State) state <- mkReg(Fetch1);
  Reg#(ExecInst) ei <- mkRegU;

  rule doFetch1(started && state == Fetch1);
    iMem.to_proc.request.put(MemReq{op: Ld, addr: pc, byteEn: replicate(True), data: ?});
    $display("Fetch1: pc: %h", pc);
    state <= Fetch2;
  endrule

  rule doFetch2(state == Fetch2);
    let inst_data <- iMem.to_proc.response.get;
    Instruction inst = truncate(gatherLoad(pc, unpack(zeroExtend(4'b1111)), True, inst_data));

    // decode
    let dInst = decode(inst);
    //$display(fshow(dInst));

    // trace - print the instruction
    $display("Fetch2: pc: %h inst: (%h) expanded: ", pc, inst, showInst(inst));

    // read register values
    let rVal1 = rf.rd1(validValue(dInst.src1));
    let rVal2 = rf.rd2(validValue(dInst.src2));
    let csrState = csrf.rd(dInst.csr);

    if (isValid(csrState.csr))
      $display("csrr %h == %h, %h", validValue(csrState.csr), csrState.data);

    // execute
    let eInst = (isSystem(dInst.iType) ? execPriv(dInst, rVal1, rVal2, csrState, pc, ?) : exec(dInst, rVal1, rVal2, csrState, pc, ?)); // The fifth argument is the predicted pc, to detect if it was mispredicted. Since there is no branch prediction, this field is sent with a random value

    // Executing unsupported instruction. Exiting
    if(eInst.iType == Unsupported)
    begin
      $fwrite(stderr, "Executing unsupported instruction at pc: %x. Exiting\n", pc);
      $finish();
    end

    ei <= eInst;

    state <= Execute1;
  endrule

  rule doExecute1(state == Execute1);
    let eInst = ei;

    // memory
    if(eInst.iType == Ld)
    begin
      dMem.to_proc.request.put(MemReq{op: Ld, addr: eInst.addr, byteEn: eInst.byteEn, data: ?});
    end
    else if(eInst.iType == St)
    begin
      match {.byteEn, .data} = scatterStore(eInst.addr, eInst.byteEn, eInst.data);
      dMem.to_proc.request.put(MemReq{op: St, addr: eInst.addr, byteEn: byteEn, data: data});
    end

    state <= Execute2;
  endrule

  rule doExecute2(state == Execute2);
    let eInst = ei;

    if(eInst.iType == Ld)
    begin
      let data <- dMem.to_proc.response.get;
      eInst.data = gatherLoad(eInst.addr, eInst.byteEn, eInst.unsignedLd, data);

      $display("mld %h (%h), %h", eInst.data, eInst.addr, eInst.byteEn);
    end

    // write back
    if(isValid(eInst.dst))
      rf.wr(validValue(eInst.dst), eInst.data);

    // update the pc depending on whether the branch is taken or not
    pc <= eInst.brTaken ? eInst.addr : pc + 4;

    // Csr write for debugging and stats
    csrf.wr(eInst.csrState);

    state <= Fetch1;
  endrule

  rule csrfToHost;
    let ret <- csrf.csrfToHost;
    indication.to_host(ret);
  endrule

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

  interface ProcRequest request;
    method Action start(Bit#(64) startpc);
      started <= True;
      pc <= startpc;
    endmethod

    method Action from_host(Bool isfromhost, Bit#(64) v);
      csrf.hostToCsrf(isfromhost, v);
    endmethod

    method Action imem_resp(Bit#(64) data);
      iMem.to_mem.response.put(data);
    endmethod

    method Action dmem_resp(Bit#(64) data);
      dMem.to_mem.response.put(data);
    endmethod
  endinterface
endmodule
