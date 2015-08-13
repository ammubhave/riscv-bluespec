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

function Bit#(TAdd#(DataSz, DataSz)) mul64(Data a, Data b);
  Bit#(64) a_l = zeroExtend(a[31:0]); Bit#(64) a_h = zeroExtend(a[63:0]);
  Bit#(64) b_l = zeroExtend(b[31:0]); Bit#(64) b_h = zeroExtend(b[63:0]);
  return zeroExtend(a_l * b_l) + (zeroExtend(a_l * b_h) << 32) + (zeroExtend(a_h * b_l) << 32) + (zeroExtend(a_h * b_h) << 64);
endfunction

/*void mult64to128(uint64 u, uint64 v, uint64& h, uint64 &l)
{
    uint64 u1 = (u & 0xffffffff);
    uint64 v1 = (v & 0xffffffff);
    uint64 t = (u1 * v1);
    uint64 w3 = (t & 0xffffffff);
    uint64 k = (t >> 32);

    u >>= 32;
    t = (u * v1) + k;
    k = (t & 0xffffffff);
    uint64 w1 = (t >> 32);

    v >>= 32;
    t = (u1 * v) + k;
    k = (t >> 32);

    h = (u * v) + w1 + k;
    l = (t << 32) + w3;
}*/
function Bit#(TAdd#(DataSz, DataSz)) mult64to128(Data u, Data v);
  Bit#(64) u1 = zeroExtend(u[31:0]);
  Bit#(64) v1 = zeroExtend(v[31:0]);
  Bit#(64) t = u1 * v1;
  Bit#(64) w3 = zeroExtend(t[31:0]);
  Bit#(64) k = t >> 32;

  u = u >> 32;
  t = (u * v1) + k;
  k = zeroExtend(t[31:0]);
  Bit#(64) w1 = t >> 32;

  v = v >> 32;
  t = (u1 * v) + k;
  k = t >> 32;

  return {(u * v) + w1 + k, (t << 32) + w3};
endfunction
/*
void mult128(uint128 N, uint128 M, uint128& Ans)
{
    mult64to128(N.Lo, M.Lo, Ans.Hi, Ans.Lo);
    Ans.Hi += (N.Hi * M.Lo) + (N.Lo * M.Hi);
}
*/
function Bit#(TAdd#(DataSz, DataSz)) mult128(Bit#(TAdd#(DataSz, DataSz)) n, Bit#(TAdd#(DataSz, DataSz)) m);
  Bit#(128) ans = mult64to128(n[63:0], m[63:0]);
  return {(n[127:64] * m[63:0]) + (n[63:0] + m[127:64]) + ans[127:64], ans[63:0]};
endfunction

(* noinline *)
function Data alu(Data a, Data b, AluFunc func);
  Bit#(TAdd#(DataSz, DataSz)) a_lu = zeroExtend(a); Bit#(TAdd#(DataSz, DataSz)) a_ls = signExtend(a);
  Bit#(TAdd#(DataSz, DataSz)) b_lu = zeroExtend(b); Bit#(TAdd#(DataSz, DataSz)) b_ls = signExtend(b);
  Bit#(32) a_w = truncate(a); Bit#(32) b_w = truncate(b);
  Bool s_a = a[valueOf(DataSz)-1] == 1; Bool s_b = b[valueOf(DataSz)-1] == 1;
  Bool s_a_w = a[31] == 1; Bool s_b_w = b[31] == 1;

  let data_div = (s_a ? -a : a) / (b == 0 ? 1 : (s_b ? -b : b));
  let data_rem = (s_a ? -a : a) % (b == 0 ? 1 : (s_b ? -b : b));
  let s_data_rem = data_rem[valueOf(DataSz)-1] == 1;
  let data_divw = (s_a_w ? -a_w : a_w) / (b_w == 0 ? 1 : (s_b_w ? -b_w : b_w));
  let data_remw = (s_a_w ? -a_w : a_w) % (b_w == 0 ? 1 : (s_b_w ? -b_w : b_w));
  let s_data_remw = data_remw[31] == 1;

  Data res = case(func)
     //default: 0;
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

     Mul   : (a * b);
     /*Mulh  : truncateLSB(a_ls * b_ls);
     Mulhu : truncateLSB(a_lu * b_lu);
     Mulhsu: truncateLSB(a_ls * b_lu);*/
     Mulw  : signExtend(a_w * b_w);

    /* Mulh  : truncateLSB(mult128(a_ls, b_ls));
     Mulhu : truncateLSB(mult128(a_lu, b_lu));
     Mulhsu: truncateLSB(mult128(a_ls, b_lu));*/

     Div   : (b != 0 ? (s_a != s_b ? -data_div : data_div) : signExtend(1'b1));
     Divu  : (b != 0 ? (a / (b == 0 ? 1 : b)) : signExtend(1'b1));
     Rem   : (b != 0 ? (s_a != s_data_rem ? -data_rem : data_rem) : a);
     Remu  : (b != 0 ? (a % (b == 0 ? 1 : b)) : a);
     Divw  : signExtend(b_w != 0 ? (s_a_w != s_b_w ? -data_divw : data_divw) : signExtend(1'b1));
     Divuw : signExtend(b_w != 0 ? (a_w / (b_w == 0 ? 1 : b_w)) : signExtend(1'b1));
     Remw  : signExtend(b_w != 0 ? (s_a_w != s_data_remw ? -data_remw : data_remw) : a_w);
     Remuw : signExtend(b_w != 0 ? (a_w % (b_w == 0 ? 1 : b_w)) : a_w);
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
  eInst.addr = (dInst.iType == Ld || dInst.iType == St || dInst.iType == Lr || dInst.iType == Sc) ? aluRes : brAddr;

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
