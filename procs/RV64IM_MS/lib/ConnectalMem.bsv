import Types::*;
import ClientServer::*;
import GetPut::*;
import Fifo::*;
import MemoryTypes::*;
import RegFile::*;
import Vector::*;
import FIFO::*;
import HostInterface::*;
import MemTypes::*;
import BRAMFIFO::*;

interface ConnectalMem;
  interface Server#(WideMemReq, WideMemResp) to_proc;

  method Action set_refPointer(Bit#(32) refPointer);
  interface Get#(MemRequest) to_host_readReq;
  interface Put#(MemData#(DataBusWidth)) to_host_readData;

  interface Get#(MemRequest) to_host_writeReq;
  interface Get#(MemData#(DataBusWidth)) to_host_writeData;
  interface Put#(Bit#(MemTagSize)) to_host_writeDone;
endinterface

(* synthesize *)
module mkConnectalMemory(ConnectalMem);
  Reg#(SGLId)   refPointerReg <- mkReg(0);

  FIFO#(MemRequest) readReqFifo <- mkFIFO();
  FIFO#(MemRequest) writeReqFifo <- mkFIFO();
  FIFO#(MemData#(DataBusWidth))   writeDataFifo <- mkSizedBRAMFIFO(1024);

  Fifo#(2, WideMemResp)  respFifo <- mkBypassFifo;

  interface Server to_proc;
    interface Put request;
      method Action put(WideMemReq r);
        r.addr = {r.addr[valueOf(AddrSz)-1:3], 3'b0};
        //r.addr = {'b0, r.addr[25:3], 3'b0};
        //if (r.addr[63:26] == 0) begin
          if (r.op == Ld) begin
            readReqFifo.enq(MemRequest { sglId: refPointerReg, offset: truncate(r.addr), burstLen: 8, tag: 1});
          end else begin
            writeReqFifo.enq(MemRequest { sglId: refPointerReg, offset: truncate(r.addr), burstLen: 8, tag: 0});
            writeDataFifo.enq(MemData { data: r.data, tag: 0});
          end
      //  end else begin
      //    if (r.op == Ld) begin
      //      respFifo.enq(0);
     //     end
       // end
      endmethod
    endinterface

    interface Get response;
      method ActionValue#(WideMemResp) get();
        respFifo.deq;
        return respFifo.first;
      endmethod
    endinterface
  endinterface

  method Action set_refPointer(Bit#(32) refPointer);
    refPointerReg <= refPointer;
  endmethod
  interface Get to_host_readReq;
    method ActionValue#(MemRequest) get;
      readReqFifo.deq;
      return readReqFifo.first;
    endmethod
  endinterface
  interface Put to_host_readData;
    method Action put(MemData#(DataBusWidth) data);
      respFifo.enq(data.data);
    endmethod
  endinterface

  interface Get to_host_writeReq;
    method ActionValue#(MemRequest) get;
      writeReqFifo.deq;
      return writeReqFifo.first;
    endmethod
  endinterface
  interface Get to_host_writeData;
    method ActionValue#(MemData#(DataBusWidth)) get;
      writeDataFifo.deq;
      return writeDataFifo.first;
    endmethod
  endinterface
  interface Put to_host_writeDone;
    method Action put(Bit#(MemTagSize) tag);
      // pass
    endmethod
  endinterface
endmodule
