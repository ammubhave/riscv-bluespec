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
import Pipe::*;
import ProcTypes::*;
import RFile::*;
import Types::*;
import Vector::*;

import ConnectalMemory::*;
import MemTypes::*;
import MemreadEngine::*;
import MemwriteEngine::*;

interface ProcRequest;
  method Action start(Bit#(64) startpc, Bit#(32) imp, Bit#(32) dmp);
  method Action from_host(Bool isfromhost, Bit#(64) v);
  method Action imem_resp;
  method Action dmem_resp;
endinterface

interface ProcIndication;
  method Action to_host(Bit#(64) v);
  method Action imem_req(Bit#(64) addr);
  method Action dmem_req(Bit#(64) addr);
endinterface

interface Proc;
  interface ProcRequest request;
  interface Vector#(1, MemReadClient#(WideLineSz)) dmaReadClient;
  interface Vector#(1, MemWriteClient#(WideLineSz)) dmaWriteClient;
endinterface

typedef enum {Fetch1, Fetch2, Execute1, Execute2} State deriving (Bits, Eq);

module mkProc#(ProcIndication indication)(Proc);
  Reg#(Addr) pc <- mkRegU;
  RFile      rf <- mkRFile;
  CsrFile  csrf <- mkCsrFile;
  Cache      iMem <- mkCache;
  Cache      dMem <- mkCache;
  Reg#(Bool) started <- mkReg(False);

  MemreadEngine#(WideLineSz,1,2)  re <- mkMemreadEngine;
  MemwriteEngine#(WideLineSz,2,2) we <- mkMemwriteEngine;
  Reg#(SGLId)   imemPointer <- mkReg(0);
  Reg#(SGLId)   dmemPointer <- mkReg(0);
  Reg#(Maybe#(WideMemReq))  imemHostBusy <- mkReg(Invalid);
  Reg#(Maybe#(WideMemReq))  dmemHostBusy <- mkReg(Invalid);

  Reg#(State)    state <- mkReg(Fetch1);
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

  mkConnection(toGet(re.dataPipes[0]), iMem.to_mem.response);
  mkConnection(toGet(re.dataPipes[1]), dMem.to_mem.response);

  rule iMemToHost;
    let d <- iMem.to_mem.request.get;
    if (d.op == St) begin
      we.writeServers[0].request.put(MemengineCmd{tag:0, sglId:imemPointer, base:0, len: 64, burstLen: 1});
      imemHostBusy <= Valid(d);
    end else begin
      indication.imem_req(d.addr);
    end
  endrule

  rule dMemToHost;
    let d <- dMem.to_mem.request.get;
    if (d.op == St) begin
      we.writeServers[1].request.put(MemengineCmd{tag:0, sglId:dmemPointer, base:0, len: 64, burstLen: 1});
      dmemHostBusy <= Valid(d);
    end else begin
      indication.dmem_req(d.addr);
    end
  endrule

  rule iMemDataPipeClear;
    let rv <- we.writeServers[0].response.get;
    indication.imem_req(validValue(imemHostBusy).addr);
    imemHostBusy <= Invalid;
  endrule

  rule dMemDataPipeClear;
    let rv <- we.writeServers[1].response.get;
    indication.dmem_req(validValue(dmemHostBusy).addr);
    dmemHostBusy <= Invalid;
  endrule

  interface ProcRequest request;
    method Action start(Bit#(64) startpc, Bit#(32) imp, Bit#(32) dmp);
      started <= True;
      pc <= startpc;
      imemPointer <= imp;
      dmemPointer <= dmp;
    endmethod

    method Action from_host(Bool isfromhost, Bit#(64) v);
      csrf.hostToCsrf(isfromhost, v);
    endmethod

    method Action imem_resp;
      re.readServers[0].request.put(MemengineCmd{sglId: imemPointer, base: 0, len: 64, burstLen: 1});
    endmethod

    method Action dmem_resp;
      re.readServers[1].request.put(MemengineCmd{sglId: dmemPointer, base: 0, len: 64, burstLen: 1});
    endmethod
  endinterface

  interface MemReadClient dmaReadClient = cons(re.dmaClient, nil);
  interface MemWriteClient dmaWriteClient = cons(we.dmaClient, nil);
endmodule
