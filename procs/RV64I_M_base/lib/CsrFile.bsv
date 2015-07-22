/*

Copyright (C) 2012 Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/


import Types::*;
import ProcTypes::*;
import MemTypes::*;
import Ehr::*;
import ConfigReg::*;
import Fifo::*;

interface CsrFile;
  method CsrState rd(Maybe#(CSR) csr);
  method Action wr(CsrState csrState);

  method Action hostToCsrf(Bool isfromhost, Data val);
  method ActionValue#(Data) csrfToCop;
endinterface

module mkCsrFile(CsrFile);
  // User-Level CSRs
  Reg#(Data) cycleReg <- mkConfigReg(0);
  Reg#(Data) timeReg <- mkConfigReg(0);
  Reg#(Data) instretReg <- mkConfigReg(0);
  Reg#(Data) statsReg <- mkConfigReg(0);

  // Machine-Level CSRs
  Reg#(Data) mhartidReg <- mkConfigReg(0);
  //                                      SD       VM=Mbare  MPRV    XS     FS   PRV3   IE3  PRV2   IE2  PRV1   IE1   PRV   IE
  Reg#(Data) mstatusReg <- mkConfigReg({1'b0, 'b0, 5'b00000, 1'b0, 2'b00, 2'b00, prvM, 1'b1, prvM, 1'b1, prvM, 1'b1, prvM, 1'b1});
  Reg#(Data) mtvecReg <- mkConfigReg('h100);
  Reg#(Data) mtdelegReg <- mkConfigReg(0);
  Reg#(Data) mipReg <- mkConfigReg(0);
  Reg#(Data) mieReg <- mkConfigReg(0);
  Reg#(Data) mtimecmpReg <- mkConfigReg(0);
  Reg#(Data) mtimeReg <- mkConfigReg(0);
  Reg#(Data) mscratchReg <- mkConfigReg(?);
  Reg#(Data) mepcReg <- mkConfigReg(0);
  Reg#(Data) mcauseReg <- mkConfigReg(0);
  Reg#(Data) mbadaddrReg <- mkConfigReg(?);
  Reg#(Data) mtohostReg <- mkConfigReg(0);
  Reg#(Data) mfromhostReg <- mkConfigReg(1);

  Integer timeShamt = 7;

  Fifo#(2,  Data) csrFifo <- mkCFFifo;

  rule count;
     cycleReg <= cycleReg + 1;
     $display("\nCycle %d ---------------------------------------------------- %d", cycleReg, instretReg);
  endrule

  function Data read(CSR idx);
    return (case(idx)
      CSRhartid: 0;
      CSRcycle: cycleReg;
      CSRtime: (instretReg >> timeShamt);
      CSRinstret: instretReg;
      CSRstats: statsReg;

      CSRcyclew: cycleReg;
      CSRtimew: (instretReg >> timeShamt);
      CSRinstretw: instretReg;

      // TODO: Implement Hypervisor-Level CSRs

      //           RV64           ZYXWVUTSRQPONMLKJIHGFEDCBA
      CSRmcpuid: {2'b10, 'b0, 26'b00000001000000000100000000};
      //               Anonymous   
      CSRmimpid: {'b0, 16'h8000};
      CSRmhartid: mhartidReg;
      CSRmstatus: mstatusReg;
      CSRmtvec: mtvecReg;
      CSRmtdeleg: 0; //mtdelegReg;
      CSRmie: mieReg;
      CSRmtimecmp: mtimecmpReg;
      CSRmtime: (instretReg >> timeShamt);
      CSRmscratch: mscratchReg;
      CSRmepc: mepcReg;
      CSRmcause: mcauseReg;
      CSRmbadaddr: mbadaddrReg;
      CSRmip: mipReg;
      CSRmtohost: mtohostReg;
      CSRmfromhost: mfromhostReg;

      CSRsendipi: 0;
      CSRuarch0, CSRuarch1, CSRuarch2, CSRuarch3, CSRuarch4, CSRuarch5, CSRuarch6, CSRuarch7, CSRuarch8, CSRuarch9, CSRuarch10, CSRuarch11, CSRuarch12, CSRuarch13, CSRuarch14, CSRuarch15: 0;
      default: 0;
    endcase);
  endfunction

  method CsrState rd(Maybe#(CSR) csr);
    CsrState csrState = ?;
    csrState.mstatus = mstatusReg;
    csrState.mtvec = mtvecReg;
    csrState.mepc = mepcReg;
    csrState.mcause = mcauseReg;
    csrState.csr = csr;
    csrState.data = read(validValue(csr));
    //if (validValue(csr) == CSRmfromhost) begin
    //  $fwrite(stderr, "mfromhost read");
    //end
    return csrState;
  endmethod

  method Action wr(CsrState csrState);
    if (isValid(csrState.csr)) begin
      let idx = validValue(csrState.csr);
      let val = csrState.data;
      $display("csr %h <= %h", idx, val);
      case (idx)
        CSRstats: statsReg <= val;

        CSRmtdeleg: mtdelegReg <= val;
        CSRmie:
        begin
          Data mask = _MIP_SSIP | _MIP_MSIP | _MIP_STIP;
          mieReg <= (mieReg & ~mask) | (val & mask);
        end
        CSRmscratch: mscratchReg <= val;
        CSRmbadaddr: mbadaddrReg <= val;
        CSRmip:
        begin
          Data mask = _MIP_SSIP | _MIP_MSIP;
          mipReg <= (mipReg & ~mask) | (val & mask);
        end
        CSRmtohost:
        begin
          //$fwrite(stderr, "mtohost: %b\n", val);
          //mtohostReg <= val;
          mtohostReg <= val;
          csrFifo.enq(val); // Finish code
        end
        CSRmfromhost: mfromhostReg <= val;
      endcase

      mstatusReg <= case (idx)
        CSRmstatus: (val & 'h3F0FFF);
        default: csrState.mstatus;
      endcase;

      mtvecReg <= (idx == CSRmtvec ? val : csrState.mtvec);
      mepcReg <= (idx == CSRmepc ? val : csrState.mepc);
      mcauseReg <= (idx == CSRmcause ? val : csrState.mcause);
    end else begin
      mstatusReg <= csrState.mstatus;
      $display("mstatus == %h", csrState.mstatus);
      mtvecReg <= csrState.mtvec;
      mepcReg <= csrState.mepc;
      mcauseReg <= csrState.mcause;
    end

    instretReg <= instretReg + 1;
  endmethod

  method Action hostToCsrf(Bool isfromhost, Data val);
    if (isfromhost) mfromhostReg <= val;
    else mtohostReg <= val;
  endmethod

  method ActionValue#(Data) csrfToCop;
    csrFifo.deq;
    return csrFifo.first;
  endmethod
endmodule
