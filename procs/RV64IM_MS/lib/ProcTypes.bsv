/*

Copyright (C) 2012

Arvind <arvind@csail.mit.edu>
Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/


import Types::*;
import FShow::*;
import MemoryTypes::*;

typedef Bit#(5)  RIndx;

typedef enum {
  Load    = 7'b0000011,
  LoadFp  = 7'b0000100,
  MiscMem = 7'b0001111,
  OpImm   = 7'b0010011,
  Auipc   = 7'b0010111,
  OpImm32 = 7'b0011011,
  Store   = 7'b0100011,
  StoreFp = 7'b0100111,
  Amo     = 7'b0101111,
  Op      = 7'b0110011,
  Lui     = 7'b0110111,
  Op32    = 7'b0111011,
  Madd    = 7'b1000011,
  Msub    = 7'b1000111,
  Nmsub   = 7'b1001011,
  Nmadd   = 7'b1001111,
  OpFp    = 7'b1010011,
  Branch  = 7'b1100011,
  Jalr    = 7'b1100111,
  Jal     = 7'b1101111,
  System  = 7'b1110011
} Opcode deriving(Bits, Eq);

typedef enum {
  CSRfflags    = 'h001,
  CSRfrm       = 'h002,
  CSRfcsr      = 'h003,
  CSRstoreaddr = 'h008,
  CSRstore8    = 'h009,
  CSRstore16   = 'h00a,
  CSRstore32   = 'h00b,
  CSRload8     = 'h00d,
  CSRload16    = 'h00e,
  CSRload32    = 'h00f,
  CSRstats     = 'h0c0,
  CSRsstatus   = 'h100,
  CSRstvec     = 'h101,
  CSRsie       = 'h104,
  CSRstimecmp  = 'h121,
  CSRsscratch  = 'h140,
  CSRsepc      = 'h141,
  CSRsip       = 'h144,
  CSRsptbr     = 'h180,
  CSRsasid     = 'h181,
  CSRhstatus   = 'h200,
  CSRhtvec     = 'h201,
  CSRhepc      = 'h241,
  CSRmstatus   = 'h300,
  CSRmtvec     = 'h301,
  CSRmtdeleg   = 'h302,
  CSRmie       = 'h304,
  CSRmtimecmp  = 'h321,
  CSRmscratch  = 'h340,
  CSRmepc      = 'h341,
  CSRmcause    = 'h342,
  CSRmbadaddr  = 'h343,
  CSRmip       = 'h344,
  CSRmbase     = 'h380,
  CSRmbound    = 'h381,
  CSRmibase    = 'h382,
  CSRmibound   = 'h383,
  CSRmdbase    = 'h384,
  CSRmdbound   = 'h385,
  CSRsup0      = 'h500,
  CSRsup1      = 'h501,
  CSRepc       = 'h502,
  CSRbadvaddr  = 'h503,
  CSRptbr      = 'h504,
  CSRasid      = 'h505,
  CSRcount     = 'h506,
  CSRcompare   = 'h507,
  CSRevec      = 'h508,
  CSRcause     = 'h509,
  CSRstatus    = 'h50a,
  CSRhartid    = 'h50b,
  CSRimpl      = 'h50c,
  CSRfatc      = 'h50d,
  CSRsendipi   = 'h50e,
  CSRclearipi  = 'h50f,
  CSRtohost    = 'h51e,
  CSRfromhost  = 'h51f,
  CSRmtime     = 'h701,
  CSRmtimeh    = 'h741,
  CSRmtohost   = 'h780,
  CSRmfromhost = 'h781,
  CSRmreset    = 'h782,
  CSRsend_ipi  = 'h783,
  CSRcyclew    = 'h900,
  CSRtimew     = 'h901,
  CSRinstretw  = 'h902,
  CSRcyclehw   = 'h980,
  CSRtimehw    = 'h981,
  CSRinstrethw = 'h982,
  CSRstimew    = 'ha01,
  CSRstimehw   = 'ha81,
  CSRcycle     = 'hc00,
  CSRtime      = 'hc01,
  CSRinstret   = 'hc02,
  CSRcycleh    = 'hc80,
  CSRtimeh     = 'hc81,
  CSRinstreth  = 'hc82,
  CSRuarch0    = 'hcc0,
  CSRuarch1    = 'hcc1,
  CSRuarch2    = 'hcc2,
  CSRuarch3    = 'hcc3,
  CSRuarch4    = 'hcc4,
  CSRuarch5    = 'hcc5,
  CSRuarch6    = 'hcc6,
  CSRuarch7    = 'hcc7,
  CSRuarch8    = 'hcc8,
  CSRuarch9    = 'hcc9,
  CSRuarch10   = 'hcca,
  CSRuarch11   = 'hccb,
  CSRuarch12   = 'hccc,
  CSRuarch13   = 'hccd,
  CSRuarch14   = 'hcce,
  CSRuarch15   = 'hccf,
  CSRstime     = 'hd01,
  CSRscause    = 'hd42,
  CSRsbadaddr  = 'hd43,
  CSRstimeh    = 'hd81,
  CSRmcpuid    = 'hf00,
  CSRmimpid    = 'hf01,
  CSRmhartid   = 'hf10
} CSR deriving(Bits, Eq, FShow);

typedef enum {Unsupported, Alu, Ld, St, Lr, Sc, J, Jr, Br, Csrs, Csrc, Csrw, Auipc, Priv} IType deriving(Bits, Eq, FShow);
typedef enum {Eq, Neq, Lt, Ltu, Ge, Geu, AT, NT} BrFunc deriving(Bits, Eq, FShow);
typedef enum {Add, Addw, Sub, Subw, And, Or, Xor, Slt, Sltu, Sll, Sllw, Sra, Sraw, Srl, Srlw, Mul, Mulh, Mulhsu, Mulhu, Div, Divu, Rem, Remu, Mulw, Divw, Divuw, Remw, Remuw} AluFunc deriving(Bits, Eq, FShow);

//typedef void Exception;
typedef enum {
  InstAddrMisaligned = 4'd0,
  InstAccessFault    = 4'd1,
  IllegalInst        = 4'd2,
  Breakpoint         = 4'd3,
  LoadAddrMisaligned = 4'd4,
  LoadAccessFault    = 4'd5,
  StoreAddrMisaligned = 4'd6,
  StoreAccessFault    = 4'd7,
  EnvCallU            = 4'd8,
  EnvCallS            = 4'd9,
  EnvCallH            = 4'd10,
  EnvCallM            = 4'd11
} Exception deriving(Bits, Eq, FShow);

typedef struct {
  Data mstatus;

  Data stvec;
  Data htvec;
  Data mtvec;

  Data sepc;
  Data hepc;
  Data mepc;

  Data scause;
  Data hcause;
  Data mcause;

  Data sbadaddr;
  Data hbadaddr;
  Data mbadaddr;

  Maybe#(CSR) csr;
  Data data;
} CsrState deriving (Bits, Eq, FShow);

typedef struct {
  Addr pc;
  Addr nextPc;
  IType brType;
  Bool taken;
  Bool mispredict;
} Redirect deriving (Bits, Eq);

typedef struct {
  IType            iType;
  ByteEn           byteEn;
  Bool             unsignedLd;
  AluFunc          aluFunc;
  BrFunc           brFunc;
  Maybe#(RIndx)    dst;
  Maybe#(RIndx)    src1;
  Maybe#(RIndx)    src2;
  Maybe#(CSR)      csr;
  Maybe#(Data)     imm;
} DecodedInst deriving(Bits, Eq, FShow);

typedef struct {
  IType            iType;
  Maybe#(RIndx)    dst;
  Maybe#(CSR)      csr;
  ByteEn           byteEn;
  Bool             unsignedLd;
  Data             data;
  Data             csrVal;
  CsrState         csrState;
  Addr             addr;
  Bool             mispredict;
  Bool             brTaken;
} ExecInst deriving(Bits, Eq);

typedef struct {
  Bool isPaged;
  Addr base;
  Addr bound;
} PagingInfo deriving(Bits, Eq);

// Op
Bit#(3) fnADD   = 3'b000;
Bit#(3) fnSLL   = 3'b001;
Bit#(3) fnSLT   = 3'b010;
Bit#(3) fnSLTU  = 3'b011;
Bit#(3) fnXOR   = 3'b100;
Bit#(3) fnSR    = 3'b101;
Bit#(3) fnOR    = 3'b110;
Bit#(3) fnAND   = 3'b111;

Bit#(7) opALU1   = 7'b0000000;
Bit#(7) opALU2   = 7'b0100000;
Bit#(7) opMULDIV = 7'b0000001;

Bit#(3) fnMUL    = 3'b000;
Bit#(3) fnMULH   = 3'b001;
Bit#(3) fnMULHSU = 3'b010;
Bit#(3) fnMULHU  = 3'b011;
Bit#(3) fnDIV    = 3'b100;
Bit#(3) fnDIVU   = 3'b101;
Bit#(3) fnREM    = 3'b110;
Bit#(3) fnREMU   = 3'b111;

// Branch
Bit#(3) fnBEQ   = 3'b000;
Bit#(3) fnBNE   = 3'b001;
Bit#(3) fnBLT   = 3'b100;
Bit#(3) fnBGE   = 3'b101;
Bit#(3) fnBLTU  = 3'b110;
Bit#(3) fnBGEU  = 3'b111;

// Load
Bit#(3) fnLB    = 3'b000;
Bit#(3) fnLH    = 3'b001;
Bit#(3) fnLW    = 3'b010;
Bit#(3) fnLD    = 3'b011;
Bit#(3) fnLBU   = 3'b100;
Bit#(3) fnLHU   = 3'b101;
Bit#(3) fnLWU   = 3'b110;

// Store
Bit#(3) fnSB    = 3'b000;
Bit#(3) fnSH    = 3'b001;
Bit#(3) fnSW    = 3'b010;
Bit#(3) fnSD    = 3'b011;

// Amo
Bit#(5) fnLR    = 5'b00010;
Bit#(5) fnSC    = 5'b00011;

//MiscMem
Bit#(3) fnFENCE  = 3'b000;
Bit#(3) fnFENCEI = 3'b001;

// System
Bit#(3) fnPRIV   = 3'b000;
Bit#(3) fnCSRRW  = 3'b001;
Bit#(3) fnCSRRS  = 3'b010;
Bit#(3) fnCSRRC  = 3'b011;
Bit#(3) fnCSRRWI = 3'b101;
Bit#(3) fnCSRRSI = 3'b110;
Bit#(3) fnCSRRCI = 3'b111;

Bit#(12) privSCALL    = 12'h000;
Bit#(12) privSBREAK   = 12'h001;
Bit#(12) privSRET     = 12'h100;
Bit#(12) privSFENCEVM = 12'h101;
Bit#(12) privWFI      = 12'h102;
Bit#(12) privHRTS     = 12'h205;
Bit#(12) privMRTS     = 12'h305;
Bit#(12) privMRTH     = 12'h306;

Data _MSTATUS_IE        = 'h00000001;
Data _MSTATUS_PRV       = 'h00000006;
Data _MSTATUS_IE1       = 'h00000008;
Data _MSTATUS_PRV1      = 'h00000030;
Data _MSTATUS_IE2       = 'h00000040;
Data _MSTATUS_PRV2      = 'h00000180;
Data _MSTATUS_IE3       = 'h00000200;
Data _MSTATUS_PRV3      = 'h00000C00;
Data _MSTATUS_FS        = 'h00003000;
Data _MSTATUS_XS        = 'h0000C000;
Data _MSTATUS_MPRV      = 'h00010000;
Data _MSTATUS_VM        = 'h003E0000;
Data _MSTATUS_SD        = {1'b1, 'b0};

Data _SSTATUS_IE        = 'h00000001;
Data _SSTATUS_PIE       = 'h00000008;
Data _SSTATUS_PS        = 'h00000010;
Data _SSTATUS_FS        = 'h00003000;
Data _SSTATUS_XS        = 'h0000C000;
Data _SSTATUS_MPRV      = 'h00010000;
Data _SSTATUS_TIE       = 'h01000000;
Data _SSTATUS_SD        = {1'b1, 'b0};

Data _MIP_SSIP          = 'h00000002;
Data _MIP_HSIP          = 'h00000004;
Data _MIP_MSIP          = 'h00000008;
Data _MIP_STIP          = 'h00000020;
Data _MIP_HTIP          = 'h00000040;
Data _MIP_MTIP          = 'h00000080;

Data _SIP_SSIP          = _MIP_SSIP;
Data _SIP_STIP          = _MIP_STIP;

Bit#(2) prvU = 0;
Bit#(2) prvS = 1;
Bit#(2) prvH = 2;
Bit#(2) prvM = 3;

Bit#(5) vmMbare = 0;
Bit#(5) vmMbb   = 1;
Bit#(5) vmMbbid = 2;
Bit#(5) vmSv32  = 8;
Bit#(5) vmSv39  = 9;
Bit#(5) vmSv48  = 10;
Bit#(5) vmSv57  = 11;
Bit#(5) vmSv64  = 12;

function Bool dataHazard(Maybe#(RIndx) src1, Maybe#(RIndx) src2, Maybe#(RIndx) dst);
    return (isValid(dst) && ((isValid(src1) && validValue(dst)==validValue(src1)) ||
                             (isValid(src2) && validValue(dst)==validValue(src2))));
endfunction

function Bool isSystem(IType iType) = (iType == Priv || iType == Csrw || iType == Csrs || iType == Csrc || iType == Unsupported);

function Fmt showInst(Instruction inst);
  Fmt ret = fshow("");

  Opcode opcode = unpack(inst[  6 :  0 ]);
  let rd     = inst[ 11 :  7 ];
  let funct3 = inst[ 14 : 12 ];
  let rs1    = inst[ 19 : 15 ];
  let rs2    = inst[ 24 : 20 ];
  let funct7 = inst[ 31 : 25 ];

  Bit#(32) immI   = signExtend(inst[31:20]);
  Bit#(32) immS   = signExtend({ inst[31:25], inst[11:7] });
  Bit#(32) immB   = signExtend({ inst[31], inst[7], inst[30:25], inst[11:8], 1'b0});
  Bit#(32) immU   = { inst[31:12], 12'b0 };
  Bit#(32) immJ   = signExtend({ inst[31], inst[19:12], inst[20], inst[30:25], inst[24:21], 1'b0});

  case (opcode)
    OpImm:
    begin
      ret = case (funct3)
        fnADD: fshow("addi");
        fnSLT: fshow("slti");
        fnSLTU: fshow("sltiu");
        fnAND: fshow("andi");
        fnOR: fshow("ori");
        fnXOR: fshow("xori");
        fnSLL: fshow("slli");
        fnSR: (immI[10] == 0 ? fshow("srli") : fshow("srai"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ");
      ret = ret + (case (funct3)
        fnSLL, fnSR: fshow(immI[5:0]);
        default: fshow(immI);
      endcase);
    end

    OpImm32:
    begin
      ret = case (funct3)
        fnADD: fshow("addiw");
        fnSLL: fshow("slliw");
        fnSR: (immI[10] == 0 ? fshow("srliw") : fshow("sraiw"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ");
      ret = ret + (case (funct3)
        fnSLL, fnSR: fshow(immI[4:0]);
        default: fshow(immI);
      endcase);
    end

    Op:
    begin
      ret = case (funct3)
        fnADD: (immI[10] == 0 ? fshow("add") : fshow("sub"));
        fnSLT: fshow("slt");
        fnSLTU: fshow("sltu");
        fnAND: fshow("and");
        fnOR: fshow("or");
        fnXOR: fshow("xor");
        fnSLL: fshow("sll");
        fnSR: (immI[10] == 0 ? fshow("srl") : fshow("sra"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ") + fshow(rs2);
    end

    Op32:
    begin
      ret = case (funct3)
        fnADD: (immI[10] == 0 ? fshow("addw") : fshow("subw"));
        fnSLL: fshow("sllw");
        fnSR: (immI[10] == 0 ? fshow("srlw") : fshow("sraw"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ") + fshow(rs2);
    end

    Lui:
      ret = fshow("lui ") + fshow(rd) + fshow(" ") + fshow(immU);

    Auipc:
      ret = fshow("auipc ") + fshow(rd) + fshow(" ") + fshow(immU);

    Jal:
      ret = fshow("jal ") + fshow(rd) + fshow(" ") + fshow(immJ);

    Jalr:
      ret = fshow("jalr ") + fshow(rd) + fshow(" ") + fshow(rs1) + fshow(" ") + fshow(immI);

    Branch:    
    begin
      ret = case(funct3)
        fnBEQ: fshow("beq");
        fnBNE: fshow("bne");
        fnBLT: fshow("blt");
        fnBLTU: fshow("bltu");
        fnBGE: fshow("bge");
        fnBGEU: fshow("bgeu");
      endcase;
      ret = ret + fshow(" ") + fshow(rs1) + fshow(" ") + fshow(rs2) + fshow(" ") + fshow(immB);
    end

    Load:
    begin
      ret = case(funct3)
        fnLB: fshow("lb");
        fnLH: fshow("lh");
        fnLW: fshow("lw");
        fnLD: fshow("ld");
        fnLBU: fshow("lbu");
        fnLHU: fshow("lhu");
        fnLWU: fshow("lwu");
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ") + fshow(immI);
    end

    Store:
    begin
      ret = case(funct3)
        fnSB: fshow("sb");
        fnSH: fshow("sh");
        fnSW: fshow("sw");
        fnSD: fshow("sd");
      endcase;
      ret = ret + fshow(" ") + fshow(rs1) + fshow(" ") + fshow(rs2) + fshow(" ") + fshow(immS);
    end

    MiscMem:
    begin
      ret = case (funct3)
        fnFENCE: fshow("fence");
        fnFENCEI: fshow("fence.i");
      endcase;
    end

    System:
    begin
      case (funct3)
        fnCSRRW, fnCSRRS, fnCSRRC, fnCSRRWI, fnCSRRSI, fnCSRRCI:
        begin
          ret = case(funct3)
            fnCSRRW: fshow("csrrw");
            fnCSRRC: fshow("csrrc");
            fnCSRRS: fshow("csrrs");
            fnCSRRWI: fshow("csrrwi");
            fnCSRRCI: fshow("csrrci");
            fnCSRRSI: fshow("csrrsi");
          endcase;
          ret = ret + fshow(" ") + fshow(rd) + fshow(" ") + fshow(immI) + fshow(" ") + fshow(rs1);
        end

        fnPRIV:
        begin
          ret = case (truncate(immI))
            privSCALL: fshow("scall");
            privSBREAK: fshow("sbreak");
            privSRET: fshow("sret");
            privSFENCEVM: (fshow("sfence.vm ") + fshow(rs1));
            privWFI: fshow("wfi");
            privHRTS: fshow("hrts");
            privMRTS: fshow("mrts");
            privMRTH: fshow("mrth");
          endcase;
        end

        default:
          ret = fshow("SYSTEM not implemented");
      endcase
    end
    /*
    opLB: 
      ret = fshow("lb ") + fshow(rt) + fshow(" = ") + fshow(rs) + fshow(" ") + fshow(imm);
    
    opLH: 
      ret = fshow("lh ") + fshow(rt) + fshow(" = ") + fshow(rs) + fshow(" ") + fshow(imm);
    
    opLW: 
      ret = fshow("lw ") + fshow(rt) + fshow(" = ") + fshow(rs) + fshow(" ") + fshow(imm);
    
    opLBU: 
      ret = fshow("lbu ") + fshow(rt) + fshow(" = ") + fshow(rs) + fshow(" ") + fshow(imm);
    
    opLHU: 
      ret = fshow("lhu ") + fshow(rt) + fshow(" = ") + fshow(rs) + fshow(" ") + fshow(imm);
    
    opSB:
      ret = fshow("sb ") + fshow(rs) + fshow(" ") + fshow(rt) + fshow(" ") + fshow(imm);
    
    opSH:
      ret = fshow("sh ") + fshow(rs) + fshow(" ") + fshow(rt) + fshow(" ") + fshow(imm);
    
    opSW:
      ret = fshow("sw ") + fshow(rs) + fshow(" ") + fshow(rt) + fshow(" ") + fshow(imm);
*/
    default: 
      ret = fshow("nop");
  endcase

  return ret;
  
endfunction
