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

  interface Server to_proc;
    interface Put request;
      method Action put(WideMemReq r);
        Bit#(22) index = truncate(r.addr >> valueOf(IndxShamt));

        let data = mem.sub(index);
        if(r.op==St)
        begin
          Vector#(WideNumBytes, Bit#(8)) bytes = unpack(data);
          Vector#(WideNumBytes, Bit#(8)) bytesIn = unpack(r.data);
          for(Integer i = 0; i < valueOf(WideNumBytes); i = i + 1)
          begin
            if(r.byteEn[i])
              bytes[i] = bytesIn[i];
          end
          mem.upd(index, pack(bytes));
        end

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
endmodule
