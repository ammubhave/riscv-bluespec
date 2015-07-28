import Types::*;
import MemoryTypes::*;
import RegFile::*;

interface IMemory;
    method MemResp req(Addr a);
endinterface

(* synthesize *)
module mkIMemory(IMemory);
    RegFile#(Bit#(32), Data) mem <- mkRegFileFullLoad("memory.vmh");

    method MemResp req(Addr a);
        return mem.sub(truncate(a>>valueOf(IndxShamt)));
    endmethod
/*
    method MemResp req(Addr a);
    	let data = mem.sub(truncate(a >> 3));
    	data = (a[2] == 0 ? data : (data >> 32));
        return data;
    endmethod*/
endmodule

