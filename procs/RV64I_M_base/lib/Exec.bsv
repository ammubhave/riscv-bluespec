/*

Copyright (C) 2012

Arvind <arvind@csail.mit.edu>
Derek Chiou <derek@ece.utexas.edu>
Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/


import Types::*;
import ProcTypes::*;
import Vector::*;

(* noinline *)
function Data alu(Data a, Data b, AluFunc func);
  Data res = case(func)
     Add   : (a + b);
     Addw  : signExtend((a + b)[31:0]);
     Sub   : (a - b);
     Subw  : signExtend((a[31:0] - b[31:0])[31:0]);
     And   : (a & b);
     Or    : (a | b);
     Xor   : (a ^ b);
     Slt   : zeroExtend( pack( signedLT(a, b) ) );
     Sltu  : zeroExtend( pack( a < b ) );
     Sll   : (a << b[5:0]);
     Sllw  : signExtend((a[31:0] << b[4:0])[31:0]);
     Srl   : (a >> b[5:0]);
     Sra   : signedShiftRight(a, b[5:0]);
     Srlw  : signExtend((a[31:0] >> b[4:0])[31:0]);
     Sraw  : signExtend(signedShiftRight(a[31:0], b[4:0])[31:0]);
  endcase;
  return res;
endfunction

(* noinline *)
function Bool aluBr(Data a, Data b, BrFunc brFunc);
  Bool brTaken = case(brFunc)
    Eq  : (a == b);
    Neq : (a != b);
    Lt  : signedLT(a, b);
    Ltu : (a < b);
    Ge  : signedGE(a, b);
    Geu : (a >= b);
    AT  : True;
    NT  : False;
  endcase;
  return brTaken;
endfunction

(* noinline *)
function Addr brAddrCalc(Addr pc, Data val, IType iType, Data imm, Bool taken);
  Addr pcPlus4 = pc + 4;
  Addr targetAddr = case (iType)
    J  : (pc + imm);
    Jr : {(val + imm)[valueOf(AddrSz)-1:1], 1'b0};
    Br : (taken? pc + imm : pcPlus4);
    Alu, Ld, St, Unsupported: pcPlus4;
  endcase;
  return targetAddr;
endfunction

(* noinline *)
function ExecInst exec(DecodedInst dInst, Data rVal1, Data rVal2, CsrState csrState, Addr pc, Addr ppc);
  ExecInst eInst = ?;
  Data aluVal2 = isValid(dInst.imm) ? validValue(dInst.imm) : rVal2;

  let aluRes = alu(rVal1, aluVal2, dInst.aluFunc);

  eInst.iType = dInst.iType;

  eInst.data = (dInst.iType==St || dInst.iType==Sc)?
                 rVal2 :
               (dInst.iType==J || dInst.iType==Jr) ?
                 (pc+4) :
               dInst.iType==Auipc?
                 (pc+validValue(dInst.imm)):
                 aluRes;

  eInst.csrState = csrState;
  eInst.byteEn = dInst.byteEn;
  eInst.unsignedLd = dInst.unsignedLd;

  let brTaken = aluBr(rVal1, rVal2, dInst.brFunc);
  let brAddr = brAddrCalc(pc, rVal1, dInst.iType, validValue(dInst.imm), brTaken);
  eInst.mispredict = brAddr != ppc;

  eInst.brTaken = brTaken;
  eInst.addr = (dInst.iType == Ld || dInst.iType == St) ? aluRes : brAddr;

  eInst.dst = dInst.dst;

  return eInst;
endfunction

function Data gatherLoad(Addr addr, ByteEn byteEn, Bool unsignedLd, Data data);
  function extend = unsignedLd? zeroExtend : signExtend;
  Bit#(IndxShamt) offset = truncate(addr);

  if(byteEn[7])
    return extend(data);
  else if(byteEn[3])
  begin
    Vector#(2, Bit#(32)) dataVec = unpack(data);
    return extend(dataVec[offset[2]]);
  end
  else if(byteEn[1])
  begin
    Vector#(4, Bit#(16)) dataVec = unpack(data);
    return extend(dataVec[offset[2:1]]);
  end
  else
  begin
    Vector#(8, Bit#(8)) dataVec = unpack(data);
    return extend(dataVec[offset]);
  end
endfunction

function Tuple2#(ByteEn, Data) scatterStore(Addr addr, ByteEn byteEn, Data data);
  Bit#(IndxShamt) offset = truncate(addr);
  if(byteEn[7])
    return tuple2(byteEn, data);
  else if(byteEn[3])
    return tuple2(unpack(pack(byteEn) << (offset)), data << {(offset), 3'b0});
  else if(byteEn[1])
    return tuple2(unpack(pack(byteEn) << (offset)), data << {(offset), 3'b0});
  else
    return tuple2(unpack(pack(byteEn) << (offset)), data << {(offset), 3'b0});
endfunction
