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
  method ActionValue#(Data) csrfToHost;
endinterface

module mkCsrFile(CsrFile);
  Reg#(Data) cycleReg <- mkConfigReg(0);
  Reg#(Data) timeReg <- mkConfigReg(0);
  Reg#(Data) instretReg <- mkConfigReg(0);

  Reg#(Data) mtohostReg <- mkConfigReg(0);
  Reg#(Data) mfromhostReg <- mkConfigReg(1);

  Fifo#(2,  Data) csrFifo <- mkCFFifo;

  (* no_implicit_conditions *)
  rule count;
     cycleReg <= cycleReg + 1;
     $display("\nCycle %d ---------------------------------------------------- %d", cycleReg, instretReg);
  endrule

  function Data read(CSR idx);
    return (case(idx)
      CSRcycle: cycleReg;
      CSRtime: (instretReg >> 10);
      CSRinstret: instretReg;

      CSRmtohost: mtohostReg;
      CSRmfromhost: mfromhostReg;
      default: 0;
    endcase);
  endfunction

  method CsrState rd(Maybe#(CSR) csr);
    CsrState csrState = ?;
    csrState.csr = csr;
    csrState.data = read(validValue(csr));
    return csrState;
  endmethod

  method Action wr(CsrState csrState);
    if (isValid(csrState.csr)) begin
      let idx = validValue(csrState.csr);
      let val = csrState.data;
      $display("csr %h <= %h", idx, val);
      case (idx)
        CSRmtohost:
        begin
          //$fwrite(stderr, "mtohost: %b\n", val);
          mtohostReg <= val;
          csrFifo.enq(val);
        end
        CSRmfromhost: mfromhostReg <= val;
      endcase
    end

    instretReg <= instretReg + 1;
  endmethod

  method Action hostToCsrf(Bool isfromhost, Data val);
    if (isfromhost) mfromhostReg <= val;
    else mtohostReg <= val;
  endmethod

  method ActionValue#(Data) csrfToHost;
    csrFifo.deq;
    return csrFifo.first;
  endmethod
endmodule
