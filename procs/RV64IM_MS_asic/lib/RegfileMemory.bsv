import Types::*;
import ClientServer::*;
import GetPut::*;
import Fifo::*;
import MemoryTypes::*;
import RegFile::*;
import Vector::*;

(* synthesize *)
module mkRegfileMemory(WideMem);
  RegFile#(Bit#(22), WideLine) mem <- mkRegFileFullLoad("memory.vmh");
  Fifo#(1, WideMemResp) hitQ <- mkBypassFifo;
  Fifo#(1, WideMemResp) hitQ_host <- mkBypassFifo;

  interface Server to_proc;
    interface Put request;
      method Action put(WideMemReq r);
        Bit#(22) index = truncate(r.addr >> valueOf(WideIndxShamt));

        let data = mem.sub(index);

        $display("widemem req: %x %x", r.addr, data);
        if(r.op==St)
        begin
          $display("    st: %x %x", r.data);
          Vector#(WideNumBytes, Bit#(8)) bytes = unpack(data);
          Vector#(WideNumBytes, Bit#(8)) bytesIn = unpack(r.data);
          for(Integer i = 0; i < valueOf(WideNumBytes); i = i + 1)
          begin
            if(r.byteEn[i])
              bytes[i] = bytesIn[i];
          end
          mem.upd(index, pack(bytes));
        end
        else
          hitQ.enq(data);
      endmethod
    endinterface

    interface Get response;
      method ActionValue#(WideMemResp) get();
        hitQ.deq;
        return hitQ.first;
      endmethod
    endinterface
  endinterface

  interface Server to_host;
    interface Put request;
      method Action put(WideMemReq r);
        Bit#(22) index = truncate(r.addr >> valueOf(WideIndxShamt));

        let data = mem.sub(index);

        $display("widemem req: %x %x", r.addr, data);
        if(r.op==St)
        begin
          $display("    st: %x %x", r.data);
          Vector#(WideNumBytes, Bit#(8)) bytes = unpack(data);
          Vector#(WideNumBytes, Bit#(8)) bytesIn = unpack(r.data);
          for(Integer i = 0; i < valueOf(WideNumBytes); i = i + 1)
          begin
            if(r.byteEn[i])
              bytes[i] = bytesIn[i];
          end
          mem.upd(index, pack(bytes));
        end

        hitQ_host.enq(data);
      endmethod
    endinterface

    interface Get response;
      method ActionValue#(WideMemResp) get();
        hitQ_host.deq;
        return hitQ_host.first;
      endmethod
    endinterface
  endinterface
endmodule
