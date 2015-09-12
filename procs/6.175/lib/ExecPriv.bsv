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
function ExecInst execPriv(DecodedInst dInst, Data rVal1, Data rVal2, CsrState csrState, Addr pc, Addr ppc);
  ExecInst eInst = ?;
  eInst.iType = dInst.iType;
  eInst.addr = pc + 4;
  eInst.brTaken = True;
  eInst.dst = dInst.dst;
  eInst.byteEn = dInst.byteEn;
  eInst.unsignedLd = dInst.unsignedLd;

  case (truncate(validValue(dInst.imm)))
    privSFENCEVM:
    begin
      eInst.brTaken = False;
    end

    default:
    begin
      eInst.iType = Unsupported;
    end
  endcase

  eInst.csrState = csrState;
  eInst.mispredict = eInst.addr != ppc;

  return eInst;
endfunction
