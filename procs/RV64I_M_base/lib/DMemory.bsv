/*

Copyright (C) 2012 Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/


import Types::*;
import MemoryTypes::*;
import RegFile::*;
import Vector::*;

interface DMemory;
    method ActionValue#(MemResp) req(MemReq r);
endinterface

(* synthesize *)
module mkDMemory(DMemory);
  RegFile#(Bit#(22), Data) mem <- mkRegFileFullLoad("memory.vmh");

  method ActionValue#(MemResp) req(MemReq r);
    Bit#(22) index = truncate(r.addr>>valueOf(IndxShamt));
    
    let data = mem.sub(index);
    if(r.op==St)
    begin
      Vector#(NumBytes, Bit#(8)) bytes = unpack(data);
      Vector#(NumBytes, Bit#(8)) bytesIn = unpack(r.data);
      for(Integer i = 0; i < valueOf(NumBytes); i = i + 1)
      begin
        if(r.byteEn[i])
          bytes[i] = bytesIn[i];
      end
      mem.upd(index, pack(bytes));
    end
    return data;
  endmethod
/*
  method ActionValue#(MemResp) req(MemReq r);
    Bit#(26) index = truncate(r.addr >> 3);
    let datalo = mem.sub(index);
    let datahi = mem.sub(index + 1);
        
    if (r.addr[0] != 0 && r.memWidth == memH) begin
      $fwrite(stderr, "MISALIGNED memH for %h\n", r.addr);
      $finish;
    end
    if (r.addr[1:0] != 0 && r.memWidth == memW) begin
      $fwrite(stderr, "MISALIGNED memW for %h\n", r.addr);
      $finish;
    end
    if (r.addr[2:0] != 0 && r.memWidth == memD) begin
      $fwrite(stderr, "MISALIGNED memD for %h\n", r.addr);
      $finish;
    end

    if(r.op==St)
    begin
      if (r.memWidth == memB) begin
        let data = r.data[7:0];
        datalo = case(r.addr[2:0])
          'b000: {datalo[63:8], data};
          'b001: {datalo[63:16], data, datalo[7:0]};
          'b010: {datalo[63:24], data, datalo[15:0]};
          'b011: {datalo[63:32], data, datalo[23:0]};
          'b100: {datalo[63:40], data, datalo[31:0]};
          'b101: {datalo[63:48], data, datalo[39:0]};
          'b110: {datalo[63:56], data, datalo[47:0]};
          'b111: {data, datalo[55:0]};
        endcase;
      end else if (r.memWidth == memH) begin
        datalo = case(r.addr[2:1])
          'b00: {datalo[63:16], r.data[15:0]};
          'b01: {datalo[63:32], r.data[15:0], datalo[15:0]};
          'b10: {datalo[63:48], r.data[15:0], datalo[31:0]};
          'b11: {r.data[15:0], datalo[47:0]};
        endcase;
      end else begin
        datalo = case(r.memWidth)
          memW: (r.addr[2] == 0 ? { datalo[63:32], r.data[31:0]} : {r.data[31:0], datalo[31:0]});
          memD: r.data;
        endcase;
      end
      mem.upd(index, datalo);
    end

    $display("data: %h %h, r.addr: %h %h", datahi, datalo, r.addr + 8, r.addr);

    Bit#(6) shamt = zeroExtend(r.addr[2:0]); shamt = shamt << 3;
    datalo = datalo >> shamt;
    //shamt = zeroExtend(4 - r.addr[1:0]); shamt = shamt << 3;
    //datalo = datalo | (datahi << shamt);
    //shamt = zeroExtend(r.addr[1:0])

    $display("datalo: %h", datalo);

    return case(r.memWidth)
      memB: signExtend(datalo[7:0]);
      memH: signExtend(datalo[15:0]);
      memW: signExtend(datalo[31:0]);
      memD: datalo;
      memBU: zeroExtend(datalo[7:0]);
      memHU: zeroExtend(datalo[15:0]);
      memWU: zeroExtend(datalo[31:0]);
    endcase;
  endmethod*/
endmodule

