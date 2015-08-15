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

function Data pushPrivStack(Data mstatus) = { mstatus[valueOf(DataSz)-1:17], 1'b0, mstatus[15:12], mstatus[8:0], prvM, 1'b0 };
function Data popPrivStack(Data mstatus) = { mstatus[valueOf(DataSz)-1:12], prvU, 1'b1, mstatus[11:3] };
function Data changePrivModeIe(Data mstatus, Bit#(2) prv, Bool ie) = { mstatus[valueOf(DataSz)-1:3], prv, pack(ie) };
function Data changePrivMode(Data mstatus, Bit#(2) prv) = changePrivModeIe(mstatus, prv, unpack(mstatus[0]));

// Returns: mcause
function Maybe#(Data) getPendingInterrupt(CsrState csrState);
  let prv = csrState.mstatus[2:1];
  let ie = csrState.mstatus[0] == 1;
  let interrupts = csrState.mie & csrState.mip;

  Maybe#(Data) ret = Invalid;
  if (prv < prvM || (prv == prvM && ie)) begin
    if (interrupts[3] == 1)
      ret = Valid({1'b1, 'b0, 4'd0});
    else if (interrupts[7] == 1)
      ret = Valid({1'b1, 'b0, 4'd1});
  end

  if ((ret == Invalid) && (prv < prvS || (prv == prvS && ie))) begin
    if (interrupts[1] == 1)
      ret = Valid({1'b1, 'b0, 4'd0});
    else if (interrupts[5] == 1)
      ret = Valid({1'b1, 'b0, 4'd1});
  end

  return ret;
endfunction

(* noinline *)
function ExecInst execPriv(DecodedInst dInst, Data rVal1, Data rVal2, CsrState csrState, Addr pc, Addr ppc);
  ExecInst eInst = ?;
  eInst.iType = dInst.iType;
  eInst.addr = pc + 4;
  eInst.brTaken = True;
  eInst.dst = dInst.dst;
  eInst.byteEn = dInst.byteEn;
  eInst.unsignedLd = dInst.unsignedLd;

  let prv = csrState.mstatus[2:1];

  if (dInst.iType == Interrupt)
  begin
    eInst.addr = csrState.mtvec + 'h40 * zeroExtend(csrState.mstatus[2:1]);
    csrState.mcause = validValue(dInst.imm);
    csrState.mepc = pc;
    csrState.mstatus = pushPrivStack(csrState.mstatus);
  end else if (dInst.iType == Priv)
  begin
    case (truncate(validValue(dInst.imm)))
      privSRET:
      begin
        eInst.addr = case (prv)
          prvS: csrState.sepc;
          prvM: csrState.mepc;
        endcase;

        //csrState.mstatus = (csrState.mstatus & signExtend({1'b1, 12'h000})) | zeroExtend({prvU, 1'b1, csrState.mstatus[11:3]});
        csrState.mstatus = popPrivStack(csrState.mstatus);
      end

      privSCALL:
      begin
        eInst.addr = csrState.mtvec + 'h40 * zeroExtend(csrState.mstatus[2:1]);
        csrState.mcause = case (prv)
          prvU: zeroExtend(4'd8);
          prvS: zeroExtend(4'd9);
          prvH: zeroExtend(4'd10);
          prvM: zeroExtend(4'd11);
        endcase;
        csrState.mepc = pc;

        //csrState.mstatus = (csrState.mstatus & signExtend({1'b1, 12'h000})) | zeroExtend({csrState.mstatus[8:0], 3'b000});
        csrState.mstatus = pushPrivStack(csrState.mstatus);
        //case (prv)
        //  prvU, prvM: csrState.mstatus = csrState.mstatus | zeroExtend({prvM, 1'b0});
        //endcase
      end

      privMRTS:
      begin
        if (prv == prvM) begin
          eInst.addr = csrState.stvec;
          csrState.sbadaddr = csrState.mbadaddr;
          csrState.scause = csrState.mcause;
          csrState.sepc = csrState.mepc;
          csrState.mstatus = changePrivMode(csrState.mstatus, prvS);
        end else begin
          eInst.addr = csrState.mtvec + 'h40 * zeroExtend(csrState.mstatus[2:1]);
          csrState.mcause = zeroExtend(4'd2);
          csrState.mepc = pc;
          csrState.mstatus = pushPrivStack(csrState.mstatus);
        eInst.iType = Unsupported;
        end
      end

      privMRTH:
      begin
        if (prv == prvM) begin
          eInst.addr = csrState.htvec;
          csrState.hbadaddr = csrState.mbadaddr;
          csrState.hcause = csrState.mcause;
          csrState.hepc = csrState.mepc;
          csrState.mstatus = changePrivMode(csrState.mstatus, prvH);
        end else begin
          eInst.addr = csrState.mtvec + 'h40 * zeroExtend(csrState.mstatus[2:1]);
          csrState.mcause = zeroExtend(4'd2);
          csrState.mepc = pc;
          csrState.mstatus = pushPrivStack(csrState.mstatus);
        eInst.iType = Unsupported;
        end
      end

      privWFI:
      begin
        eInst.brTaken = False;
      end

      privSFENCEVM:
      begin
        eInst.brTaken = False;
      end

      default:
      begin
        eInst.iType = Unsupported;
      end
    endcase
  end else if (dInst.iType == Fence) begin

  end else begin
    Bit#(12) csr = pack(validValue(csrState.csr));
    Data csrOperand = isValid(dInst.imm) ? validValue(dInst.imm) : rVal2;
    if (
      ((csr[11:10] == 2'b11 && csrOperand == 0) || csr[11:10] != 2'b11) && // Read-Only
      (csr[9:8] <= prv)
    )
    begin
      eInst.data = csrState.data;
      csrState.data = case(dInst.iType)
        Csrw: csrOperand;
        Csrs: (csrState.data | csrOperand);
        Csrc: (csrState.data & ~csrOperand);
      endcase;
    end else begin
      eInst.addr = csrState.mtvec + 'h40 * zeroExtend(csrState.mstatus[2:1]);
      csrState.mcause = zeroExtend(4'd2);
      csrState.mepc = pc;
      csrState.mstatus = pushPrivStack(csrState.mstatus);
      csrState.csr = Invalid;
      eInst.iType = Unsupported;
    end
  end

  eInst.csrState = csrState;
  eInst.mispredict = eInst.addr != ppc;

  return eInst;
endfunction
