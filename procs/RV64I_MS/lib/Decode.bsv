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
import MemoryTypes::*;
import Vector::*;

Bit#(3) memWU   = 3'b110;
(* noinline *)
function DecodedInst decode(Instruction inst);
  DecodedInst dInst = ?;

  Opcode opcode = unpack(inst[  6 :  0 ]);
  let rd        =        inst[ 11 :  7 ];
  let funct3    =        inst[ 14 : 12 ];
  let rs1       =        inst[ 19 : 15 ];
  let rs2       =        inst[ 24 : 20 ];
  let funct7    =        inst[ 31 : 25 ];

  Data immI   = signExtend(inst[31:20]);
  Data immS   = signExtend({ inst[31:25], inst[11:7] });
  Data immB   = signExtend({ inst[31], inst[7], inst[30:25], inst[11:8], 1'b0});
  Data immU   = signExtend({ inst[31:12], 12'b0 });
  Data immJ   = signExtend({ inst[31], inst[19:12], inst[20], inst[30:21], 1'b0});

  case (opcode)
    OpImm:
    begin
      dInst.iType = Alu;
      dInst.aluFunc = case (funct3)
        fnADD: Add;
        fnSLT: Slt;
        fnSLTU: Sltu;
        fnAND: And;
        fnOR: Or;
        fnXOR: Xor;
        fnSLL: Sll;
        fnSR: (immI[10] == 0 ? Srl : Sra);
      endcase;
      dInst.dst  = Valid(rd);
      dInst.src1 = Valid(rs1);
      dInst.src2 = Invalid;
      dInst.imm = Valid(immI);
      dInst.csr = tagged Invalid;
      dInst.brFunc = NT;
    end

    OpImm32:
    begin
      dInst.iType = Alu;
      dInst.aluFunc = case (funct3)
        fnADD: Addw;
        fnSLL: Sllw;
        fnSR: (immI[10] == 0 ? Srlw : Sraw);
      endcase;
      dInst.dst  = Valid(rd);
      dInst.src1 = Valid(rs1);
      dInst.src2 = Invalid;
      dInst.imm = Valid(immI);
      dInst.csr = tagged Invalid;
      dInst.brFunc = NT;
    end

    Op:
    begin
      dInst.iType = Alu;
      dInst.dst  = Valid(rd);
      dInst.src1 = Valid(rs1);
      dInst.src2 = Valid(rs2);
      dInst.imm  = Invalid;
      dInst.csr = tagged Invalid;
      dInst.brFunc = NT;
      case (funct7)
        opALU1:
        begin
          dInst.aluFunc = case(funct3)
            fnADD: Add;
            fnSLT: Slt;
            fnSLTU: Sltu;
            fnAND: And;
            fnOR: Or;
            fnXOR: Xor;
            fnSLL: Sll;
            fnSR: Srl;
          endcase;
        end
        opALU2:
        begin
          dInst.aluFunc = case (funct3)
            fnADD: Sub;
            fnSR: Sra;
          endcase;
        end
        opMULDIV:
        begin
          dInst.iType = Interrupt;
          dInst.src1 = Invalid;
          dInst.src2 = Invalid;
          dInst.dst = Invalid;
          dInst.csr = Invalid;
          dInst.imm = Valid(2);
          dInst.brFunc = NT;
          /*
          dInst.aluFunc = case(funct3)
            fnMUL: Mul;
            fnMULH: Mulh;
            fnMULHSU: Mulhsu;
            fnMULHU: Mulhu;
            fnDIV: Div;
            fnDIVU: Divu;
            fnREM: Rem;
            fnREMU: Remu;
          endcase;*/
        end
      endcase
    end

    Op32:
    begin
      dInst.iType = Alu;
      case (funct7)
        opALU1:
        begin
          dInst.aluFunc = case(funct3)
            fnADD: Addw;
            fnSLL: Sllw;
            fnSR: Srlw;
          endcase;
        end
        opALU2:
        begin
          dInst.aluFunc = case (funct3)
            fnADD: Subw;
            fnSR: Sraw;
          endcase;
        end
        opMULDIV:
        begin
          dInst.aluFunc = case(funct3)
            fnMUL: Mulw;
            fnDIV: Divw;
            fnDIVU: Divuw;
            fnREM: Remw;
            fnREMU: Remuw;
          endcase;
        end
      endcase
      dInst.dst  = Valid(rd);
      dInst.src1 = Valid(rs1);
      dInst.src2 = Valid(rs2);
      dInst.imm  = Invalid;
      dInst.csr = tagged Invalid;
      dInst.brFunc = NT;
    end

    Lui:
    begin
      dInst.iType = Alu;
      dInst.aluFunc = Add;
      dInst.dst = Valid(rd);
      dInst.src1 = Valid(0);
      dInst.src2 = Invalid;
      dInst.imm = Valid(immU);
      dInst.csr = tagged Invalid;
      dInst.brFunc = NT;
    end

    Auipc:
    begin
      dInst.iType = Auipc;
      dInst.dst   = Valid(rd);
      dInst.src1  = Invalid;
      dInst.src2  = Invalid;
      dInst.imm   = Valid(immU);
      dInst.csr = tagged Invalid;
      dInst.brFunc = NT;
    end

    Jal:
    begin
      dInst.iType = J;
      dInst.dst  = Valid(rd);
      dInst.src1 = Invalid;
      dInst.src2 = Invalid;
      dInst.imm  = Valid(immJ);
      dInst.csr = tagged Invalid;
      dInst.brFunc = AT;
    end

    Jalr:
    begin
      dInst.iType = Jr;
      dInst.dst  = Valid(rd);
      dInst.src1 = Valid(rs1);
      dInst.src2 = Invalid;
      dInst.imm  = Valid(immI);
      dInst.csr = tagged Invalid;
      dInst.brFunc = AT;
    end

    Branch:
    begin
      dInst.iType = Br;
      dInst.brFunc = case(funct3)
        fnBEQ: Eq;
        fnBNE: Neq;
        fnBLT: Lt;
        fnBLTU: Ltu;
        fnBGE: Ge;
        fnBGEU: Geu;
      endcase;
      dInst.dst  = Invalid;
      dInst.src1 = Valid(rs1);
      dInst.src2 = Valid(rs2);
      dInst.imm  = Valid(immB);
      dInst.csr = tagged Invalid;
    end

    Load:
    begin
      dInst.iType = Ld;
      dInst.byteEn = replicate(False);
      case (funct3)
        memB, memBU: dInst.byteEn[0] = True;
        memH, memHU:
        begin
          dInst.byteEn[0] = True;
          dInst.byteEn[1] = True;
        end
        memW, memWU:
        begin
          dInst.byteEn[0] = True;
          dInst.byteEn[1] = True;
          dInst.byteEn[2] = True;
          dInst.byteEn[3] = True;
        end
        memD: dInst.byteEn = replicate(True);
      endcase
      dInst.unsignedLd = case (funct3)
        memB, memH, memW, memD: False;
        memBU, memHU, memWU: True;
      endcase;
      dInst.aluFunc = Add;
      dInst.dst  = Valid(rd);
      dInst.src1 = Valid(rs1);
      dInst.src2 = Invalid;
      dInst.imm    = Valid(immI);
      dInst.csr = tagged Invalid;
      dInst.brFunc = NT;
    end

    Store:
    begin
      dInst.iType = St;
      dInst.byteEn = replicate(False);
      case (funct3)
        memB, memBU: dInst.byteEn[0] = True;
        memH, memHU:
        begin
          dInst.byteEn[0] = True;
          dInst.byteEn[1] = True;
        end
        memW, memWU:
        begin
          dInst.byteEn[0] = True;
          dInst.byteEn[1] = True;
          dInst.byteEn[2] = True;
          dInst.byteEn[3] = True;
        end
        memD: dInst.byteEn = replicate(True);
      endcase
      dInst.aluFunc = Add;
      dInst.dst  = Invalid;
      dInst.src1 = Valid(rs1);
      dInst.src2 = Valid(rs2);
      dInst.imm    = Valid(immS);
      dInst.csr = tagged Invalid;
      dInst.brFunc = NT;
    end

    Amo:
    begin
      case (inst[31:27])
       /* fnLR:
        begin
          dInst.iType = Lr;
          dInst.byteEn = replicate(False);
          case (funct3)
            memW:
            begin
              dInst.byteEn[0] = True;
              dInst.byteEn[1] = True;
              dInst.byteEn[2] = True;
              dInst.byteEn[3] = True;
            end
            memD: dInst.byteEn = replicate(True);
          endcase
          dInst.unsignedLd = False;
          dInst.aluFunc = Add;
          dInst.dst  = Valid(rd);
          dInst.src1 = Valid(rs1);
          dInst.src2 = Valid(0);
          dInst.imm  = Invalid;
          dInst.csr  = Invalid;
          dInst.brFunc = NT;
        end

        fnSC:
        begin
          dInst.iType = Sc;
          dInst.byteEn = replicate(False);
          case (funct3)
            memW:
            begin
              dInst.byteEn[0] = True;
              dInst.byteEn[1] = True;
              dInst.byteEn[2] = True;
              dInst.byteEn[3] = True;
            end
            memD: dInst.byteEn = replicate(True);
          endcase
          dInst.aluFunc = Add;
          dInst.dst  = Valid(rd);
          dInst.src1 = Valid(rs1);
          dInst.src2 = Valid(rs2);
          dInst.imm  = Valid(0);
          dInst.csr  = Invalid;
          dInst.brFunc = NT;
        end*/

        default:
        begin
          dInst.iType = Unsupported;
          dInst.dst  = Invalid;
          dInst.src1 = Invalid;
          dInst.src2 = Invalid;
          dInst.imm  = Invalid;
          dInst.csr  = Invalid;
          dInst.brFunc = NT;
        end
      endcase
    end

    MiscMem:
    begin
      dInst.iType = Alu;
      dInst.aluFunc = Add;
      dInst.dst  = Valid(0);
      dInst.src1 = Valid(0);
      dInst.src2 = Invalid;
      dInst.imm  = Valid(0);
      dInst.csr  = Invalid;
      dInst.brFunc = NT;
    end

    System:
    begin
      if (funct3 == fnPRIV)
      begin
        dInst.iType = Priv;
        dInst.dst  = Invalid;
        dInst.src1 = Valid(rs1);
        dInst.src2 = Invalid;
        dInst.imm  = Valid(immI);
        dInst.csr = tagged Invalid;
        dInst.brFunc = NT;
      end else // fnCSRRWI, fnCSRRW, fnCSRRSI, fnCSRRS, fnCSRRCI, fnCSRRC
      begin
        dInst.iType = case (funct3)
          fnCSRRWI, fnCSRRW: Csrw;
          fnCSRRSI, fnCSRRS: Csrs;
          fnCSRRCI, fnCSRRC: Csrc;
        endcase;

        dInst.aluFunc = Add;
        dInst.dst = Valid(rd);
        dInst.src1 = Valid(0);
        //                                Reg     :          Imm
        dInst.src2 = (funct3[2] == 0 ? Valid(rs1) :                Invalid);
        dInst.imm  = (funct3[2] == 0 ?    Invalid : Valid(zeroExtend(rs1)));
        dInst.csr = Valid(unpack(truncate(immI)));
        dInst.brFunc = NT;
      end
    end

    default:
    begin
      dInst.iType = Unsupported;
      dInst.dst  = Invalid;
      dInst.src1 = Invalid;
      dInst.src2 = Invalid;
      dInst.imm  = Invalid;
      dInst.csr = tagged Invalid;
      dInst.brFunc = NT;
    end
  endcase

  if(dInst.dst matches tagged Valid .dst &&& dst == 0)
    dInst.dst = tagged Invalid;
  return dInst;
endfunction
