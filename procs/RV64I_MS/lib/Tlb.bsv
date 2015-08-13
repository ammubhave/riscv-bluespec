import ClientServer::*;
import Types::*;
import MemoryTypes::*;
import ProcTypes::*;
import GetPut::*;
import SMap::*;
import Fifo::*;
import RegFile::*;
import Vector::*;

interface Tlb;
  method Action flush;
  method Action updateCsr(CSR csr, Data data);

  interface Server#(MemReq, Tuple2#(Data, Maybe#(Exception))) to_proc;
  interface Client#(MemReq, MemResp) to_mem;
endinterface

typedef 32 TlbEntries;

typedef 27 VpnSz;
typedef Bit#(27) Vpn;
typedef 38 PpnSz;
typedef Bit#(38) Ppn;
typedef 12 OffsetSz;
typedef Bit#(OffsetSz) Offset;
typedef 9 VpnISz;
typedef Bit#(VpnISz) VpnI;

typedef enum {Ready, StartMiss, SendFillReq, WaitFillResp, EndMiss} TlbStatus deriving (Bits, Eq);

function Bool isFound(Tuple2#(Vpn, Ppn) x, Vpn k);
   if(tpl_1(x) == k)
     return True;
   else
     return False;
endfunction

(* synthesize *)
module mkTlb(Tlb ifc);
  SMap#(TlbEntries, Tuple2#(Vpn, Ppn), Vpn) f <- mkCFSMap(isFound);

  Reg#(Bool) inited <- mkReg(False);

  Fifo#(1, Tuple2#(Data, Maybe#(Exception))) hitQ <- mkBypassFifo;

  Reg#(Tuple4#(MemReq, Vpn, Bit#(2), Addr)) miss <- mkRegU;

  Reg#(TlbStatus) status <- mkReg(Ready);

  Fifo#(2, Tuple2#(MemReq, Bool)) memReqQ <- mkCFFifo;// mkBypassFifo;
  Fifo#(2, Line) memRespQ <- mkCFFifo;
  Fifo#(2, Bool) isHitQ <- mkBypassFifo;

  Reg#(Data) mstatusReg <- mkReg({1'b0, 'b0, 5'b00000, 1'b0, 2'b00, 2'b00, prvM, 1'b1, prvM, 1'b1, prvM, 1'b1, prvM, 1'b1});
  Reg#(Data) sptbrReg <- mkRegU;

  function Offset getOffset(Addr addr) = truncate(addr);
  function Vpn getVpn(Addr addr) = addr[38:12];
  function translate(Addr addr, Ppn ppn) = zeroExtend({ppn, getOffset(addr)});

  function PagingInfo ptbr;
    return case (mstatusReg[21:17])
      vmMbare: PagingInfo{isPaged: False, base: 0, bound: -1};
   //   vmMbb, vmMbbid: PagingInfo{isPaged: False, base: mdbaseReg, bound: mdboundReg};
      vmSv32, vmSv39, vmSv48, vmSv57, vmSv64:
        case (mstatusReg[2:1])
          prvM: PagingInfo{isPaged: False, base: 0, bound: -1};
          default: PagingInfo{isPaged: True, base: sptbrReg, bound: ?};
        endcase
    endcase;
  endfunction

  (* no_implicit_conditions *)
  rule initialize(!inited);
    inited <= True;
    f.clear;

    $display("Initializing TLB...");
  endrule

  (* fire_when_enabled *)
  rule startMiss(status == StartMiss);
    if (!f.notFull)
      f.deq;
    status <= SendFillReq;
  endrule

  rule sendFillReq(status == SendFillReq);
    match {.r, .vpn, .i, .a} = miss;

    VpnI vpni = truncateLSB(vpn);
    Addr addr = a + (zeroExtend(vpni) << 3);

    memReqQ.enq(tuple2(MemReq{op: Ld, addr: addr, byteEn: replicate(True), data: ?}, False));
    status <= WaitFillResp;

    $display("TLB: SendFillReq (%x) %x   a: %x, vpn: %x, vpni: %x", r.addr, addr, a, vpn, vpni);
  endrule

  rule waitFillResp(status == WaitFillResp);
    match {.r, .vpn, .i, .a} = miss;

    VpnI vpni = truncateLSB(vpn);
    Addr addr = a + (zeroExtend(vpni) << 3);

    Data pte = memRespQ.first;

    let valid = pte[0];
    if (valid == 0) begin
      hitQ.enq(tuple2(1, Valid(LoadAccessFault)));
      status <= Ready;
      //$fwrite(stderr, "TLB: invalid page table entry: %x, %x: %x, a: %x. Exiting\n", r.addr, addr, pte, a);
      //$finish;
    end else begin
      if (pte[4:2] == 0) begin
        if (i == 0) begin
          hitQ.enq(tuple2(2, Valid(LoadAccessFault)));
          status <= Ready;
          //$fwrite(stderr, "TLB: expected leaf page table entry: %x, %x. Exiting\n", r.addr, pte);
          //$finish;
        end else begin
          Addr ppn = zeroExtend(pte[47:10]);
          miss <= tuple4(r, vpn << valueOf(VpnISz), i - 1, ppn << 12);
          status <= SendFillReq;
        end
      end else begin
        pte = { pte[63:7], pte[6] | pack(r.op == St), 1'b1, pte[4:0] };
        memReqQ.enq(tuple2(MemReq{op: St, addr: addr, byteEn: replicate(True), data: pte}, False));

        Ppn ppn = case(i)
          0: pte[47:10];
          1: {pte[47:19], vpn[17:9]};
          2: {pte[47:28], truncate(vpn)};
        endcase;

        f.enq(tuple2(getVpn(r.addr), ppn));

        r.addr = translate(r.addr, ppn);
        miss <= tuple4(r, ?, ?, ?);
        status <= EndMiss;
      end
    end

    memRespQ.deq;
    $display("TLB: WaitFillReq (%x): %x %x %x %x", r.addr, addr, pte, i, vpn);
  endrule

  rule endMiss(status == EndMiss);
    match {.r, .vpn, .i, .a} = miss;
    memReqQ.enq(tuple2(r, True));
    status <= Ready;

    $display("TLB: EndMiss (%x)", r.addr);
  endrule

  method Action flush;
    inited <= False;
  endmethod

  method Action updateCsr(CSR csr, Data data);
    case (csr)
      CSRmstatus: mstatusReg <= data;
      CSRsptbr: sptbrReg <= data;
    endcase
    $display("TLB updateCsr %x %x", csr, data);
  endmethod

  interface Server to_proc;
    interface Put request;
      method Action put(MemReq r) if(status == Ready && inited);
        let pinfo = ptbr;
        if (pinfo.isPaged) begin
          let vpn = getVpn(r.addr);
          let srch = f.search(vpn);
          if (isValid(srch)) begin
            Addr trans_addr = translate(r.addr, tpl_2(validValue(srch)));
            $display("TLB req (th): addr: %x, op: %x, frame: %x", r.addr, r.op, trans_addr);
            r.addr = trans_addr;
            memReqQ.enq(tuple2(r, True));
          end else begin
            $display("TLB req (tm): addr: %x, op: %x", r.addr, r.op);
            miss <= tuple4(r, vpn, 2, pinfo.base);
            status <= StartMiss;
          end
        end else begin
          $display("TLB req (un): addr: %x, op: %x", r.addr, r.op);
          if (r.addr > pinfo.bound - pinfo.base) begin
            hitQ.enq(tuple2(0, Valid(LoadAccessFault)));
            //$fwrite(stderr, "TLB: addr exceeds bounds: %x, %x, %x. Exiting\n", r.addr, pinfo.base, pinfo.bound);
            //$finish;
          end
          r.addr = r.addr + pinfo.base;
          memReqQ.enq(tuple2(r, True));
        end
      endmethod
    endinterface

    interface Get response;
      method ActionValue#(Tuple2#(Data, Maybe#(Exception))) get();
        $display("TLB resp: data: %x %x", tpl_1(hitQ.first), tpl_2(hitQ.first));
        hitQ.deq;
        return hitQ.first;
      endmethod
    endinterface
  endinterface

  interface Client to_mem;
    interface Get request;
      method ActionValue#(MemReq) get();
        $display("TLB memReq: %x", tpl_1(memReqQ.first).addr);
        memReqQ.deq;
        if ((tpl_1(memReqQ.first)).op == Ld)
          isHitQ.enq(tpl_2(memReqQ.first));
        return tpl_1(memReqQ.first);
      endmethod
    endinterface

    interface Put response;
      method Action put(MemResp r);
        $display("TLB memResp: %x", r);
        isHitQ.deq;
        if (isHitQ.first)
          hitQ.enq(tuple2(r, Invalid));
        else
          memRespQ.enq(r);
      endmethod
    endinterface
  endinterface
endmodule
