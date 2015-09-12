import BRAM::*;
import ClientServer::*;
import Fifo::*;
import GetPut::*;
import MemoryTypes::*;
import RegFile::*;
import Types::*;
import Vector::*;

interface Cache;
  method Action flush;
  interface Server#(MemReq, MemResp) to_proc;
  interface Client#(WideMemReq, WideMemResp) to_mem;
endinterface

typedef 1024 CacheEntries;
typedef Bit#(TLog#(CacheEntries)) CacheIndex;
typedef Bit#(TSub#(TSub#(AddrSz, TLog#(CacheEntries)), 3)) CacheTag;

typedef enum {Ready, StartMiss, SendFillReq, WaitFillResp, WaitBramResp} CacheStatus deriving (Bits, Eq);

(* synthesize *)
module mkCache(Cache);
  BRAM_Configure cfg = defaultValue;

  BRAM2Port#(CacheIndex, Line) dataArrayBRAM <- mkBRAM2Server(cfg);
  BRAM2Port#(CacheIndex, Maybe#(CacheTag)) tagArrayBRAM <- mkBRAM2Server(cfg);
  BRAM2Port#(CacheIndex, Bool) dirtyArrayBRAM <- mkBRAM2Server(cfg);

  BRAMServer#(CacheIndex, Line) dataArray = dataArrayBRAM.portA;
  BRAMServer#(CacheIndex, Maybe#(CacheTag)) tagArray = tagArrayBRAM.portA;
  BRAMServer#(CacheIndex, Bool) dirtyArray = dirtyArrayBRAM.portA;

  Reg#(Bit#(TLog#(TAdd#(CacheEntries, 1)))) init <- mkReg(0);
  Reg#(Bit#(TLog#(TAdd#(CacheEntries, 1)))) flush_init <- mkReg(fromInteger(valueOf(TAdd#(CacheEntries, 1))));

  Fifo#(1, Data) hitQ <- mkCFFifo;
  Fifo#(1, MemReq) req_1to2_fifo <- mkCFFifo;

  Reg#(MemReq) miss <- mkRegU;
  Reg#(Maybe#(CacheTag)) miss_tag <- mkRegU;
  Reg#(Line) miss_data <- mkRegU;
  Reg#(Bool) miss_dirty <- mkRegU;

  Reg#(CacheStatus) status <- mkReg(Ready);

  Fifo#(2, WideMemReq) memReqQ <- mkCFFifo;
  Fifo#(2, WideMemResp) memRespQ <- mkCFFifo;

  function CacheIndex getIdx(Addr addr) = truncate(addr >> valueOf(IndxShamt));
  function CacheTag getTag(Addr addr) = truncateLSB(addr);

  let inited = truncateLSB(init) == 1'b1;
  let flushed = truncateLSB(flush_init) == 1'b1;
  Fifo#(2, Bit#(TLog#(TAdd#(CacheEntries, 1)))) flush_init_fifo <- mkCFFifo;

  rule initialize(!inited && flushed);
    init <= init + 1;
    tagArray.request.put(BRAMRequest{write: True, responseOnWrite: False, address: truncate(init), datain: Invalid});
    dirtyArray.request.put(BRAMRequest{write: True, responseOnWrite: False,address: truncate(init), datain: False});
    $display("cache initialize %x", init);
  endrule

  rule startMiss(status == StartMiss);
    let idx = getIdx(miss.addr);
    let tag = miss_tag;
    let dirty = miss_dirty;
    let data = miss_data;

    if(isValid(tag) && dirty)
    begin
      let addr = {validValue(tag), idx, 3'b0};
      memReqQ.enq(WideMemReq{op: St, addr: addr, byteEn: replicate(True), data: data});
      status <= SendFillReq;
    end
    else
    begin
      memReqQ.enq(WideMemReq{op: Ld, addr: miss.addr, byteEn: miss.byteEn, data: miss.data});
      status <= WaitFillResp;
    end
    $display("cache startmiss %x", miss.addr);
  endrule

  rule sendFillReq(status == SendFillReq);
    $display("cache sendfillreq %x", miss.addr);
    memReqQ.enq(WideMemReq{op: Ld, addr: miss.addr, byteEn: miss.byteEn, data: miss.data});
    status <= WaitFillResp;
  endrule

  rule waitFillResp(status == WaitFillResp);
    let idx = getIdx(miss.addr);
    let tag = getTag(miss.addr);
    let data = memRespQ.first;
    let dirty = False;

    if (miss.op == St) begin
      Vector#(NumBytes, Bit#(8)) bytes = unpack(data);
      Vector#(NumBytes, Bit#(8)) bytesIn = unpack(miss.data);
      for(Integer i = 0; i < valueOf(NumBytes); i = i + 1)
      begin
        if(miss.byteEn[i])
          bytes[i] = bytesIn[i];
      end
      data = pack(bytes);
      dirty = True;
    end else begin
      hitQ.enq(data);
    end

    dataArray.request.put( BRAMRequest{ write: True, responseOnWrite: False,address: idx, datain: data });
    tagArray.request.put( BRAMRequest{ write: True, responseOnWrite: False,address: idx, datain: Valid(tag) });
    dirtyArray.request.put( BRAMRequest{ write: True, responseOnWrite: False,address: idx, datain: dirty});
    memRespQ.deq;
    status <= Ready;
    $display("cache waitfillresp %x %x idx: %x tag: %x", miss.addr, data, idx, Valid(tag));
  endrule

  rule flushCache(!flushed && status == Ready);
    tagArray.request.put( BRAMRequest{ write: False, responseOnWrite: False, address: truncate(flush_init), datain: ?});
    dirtyArray.request.put( BRAMRequest{ write: False, responseOnWrite: False, address: truncate(flush_init), datain: ?});
    dataArray.request.put( BRAMRequest{ write: False, responseOnWrite: False, address: truncate(flush_init), datain: ?});

    flush_init_fifo.enq(flush_init);
    flush_init <= flush_init + 1;
    $display("cache flushCache part 1 %x", flush_init);
  endrule

  rule flushCache_part2(!flushed && status == Ready);
    let tag <- tagArray.response.get;
    let dirty <- dirtyArray.response.get;
    let data <- dataArray.response.get;

    if(isValid(tag) && dirty)
    begin
      let addr = {validValue(tag), truncate(flush_init_fifo.first), 3'b0};
      memReqQ.enq(WideMemReq{op: St, addr: addr, byteEn: replicate(True), data: data});
    end
    flush_init_fifo.deq;
    $display("cache flushCache part 2 %x", tag);
  endrule

  rule doReq_part2_St(req_1to2_fifo.first.op == St && status == WaitBramResp && inited);
    let r = req_1to2_fifo.first;
    req_1to2_fifo.deq;

    let idx = getIdx(r.addr);
    let tag = getTag(r.addr);

    let currTag <- tagArray.response.get;
    let currData <- dataArray.response.get;
    let currDirty <- dirtyArray.response.get;

    if (Valid(tag) == currTag) begin  // hit
      Vector#(NumBytes, Bit#(8)) bytes = unpack(currData);
      Vector#(NumBytes, Bit#(8)) bytesIn = unpack(r.data);
      for(Integer i = 0; i < valueOf(NumBytes); i = i + 1)
      begin
        if(r.byteEn[i])
          bytes[i] = bytesIn[i];
      end
      dataArray.request.put( BRAMRequest{ write: True, responseOnWrite: False, address: idx, datain: pack(bytes) } );
      dirtyArray.request.put( BRAMRequest{ write: True, responseOnWrite: False, address: idx, datain: True } );
      $display("cache req: st hit %x %x", r.addr, r.data);
      status <= Ready;
    end else begin  // miss
            $display("cache req: enq to mem %x", r.addr);
      miss <= r;
      miss_tag <= currTag;
      miss_data <= currData;
      miss_dirty <= currDirty;
      status <= StartMiss;
    end
  endrule

  rule doReq_part2_Ld(req_1to2_fifo.first.op == Ld && status == WaitBramResp && inited);
    let r = req_1to2_fifo.first;
    req_1to2_fifo.deq;

    let idx = getIdx(r.addr);
    let tag = getTag(r.addr);

    let currTag <- tagArray.response.get;
    let currData <- dataArray.response.get;
    let currDirty <- dirtyArray.response.get;

    if (Valid(tag) == currTag) begin  // hit
      hitQ.enq(currData);
      status <= Ready;
    end else begin  // miss
      miss <= r;
      miss_tag <= currTag;
      miss_data <= currData;
      miss_dirty <= currDirty;
      status <= StartMiss;
    end
  endrule

  method Action flush if (status == Ready && flushed);
    flush_init <= 0;
    init <= 0;
  endmethod

  interface Server to_proc;
    interface Put request;
      method Action put(MemReq r) if(status == Ready && inited);
        let idx = getIdx(r.addr);

        tagArray.request.put( BRAMRequest{ write: False, responseOnWrite: False, address: idx, datain: ? } );
        dataArray.request.put( BRAMRequest { write: False, responseOnWrite: False, address: idx, datain: ? } );
        dirtyArray.request.put( BRAMRequest { write: False, responseOnWrite: False, address: idx, datain: ? } );
        req_1to2_fifo.enq(r);
        status <= WaitBramResp;
      endmethod
    endinterface

    interface Get response;
      method ActionValue#(Data) get();
        hitQ.deq;
        $display("cache to_proc get() %x", hitQ.first);
        return hitQ.first;
      endmethod
    endinterface
  endinterface

  interface Client to_mem;
    interface Get request;
      method ActionValue#(WideMemReq) get();
        $display("cache to_mem get() %x", memReqQ.first.addr);
        memReqQ.deq;
        return memReqQ.first;
      endmethod
    endinterface

    interface Put response;
      method Action put(WideMemResp r);
        $display("cache to_mem put() %x", r);
        memRespQ.enq(r);
      endmethod
    endinterface
  endinterface
endmodule

module mkDummyCache (Cache ifc);
  Fifo#(2, WideMemReq)  reqFifo  <- mkBypassFifo;
  Fifo#(2, MemResp)     respFifo <- mkBypassFifo;

  interface Server to_proc;
    interface Put request;
      method Action put(MemReq r);
        reqFifo.enq(WideMemReq{op: r.op, addr: r.addr, data: r.data, byteEn: r.byteEn});
      endmethod
    endinterface

    interface Get response;
      method ActionValue#(MemResp) get;
        respFifo.deq;
        return respFifo.first;
      endmethod
    endinterface
  endinterface

  interface Client to_mem;
    interface Get request;
      method ActionValue#(WideMemReq) get;
        reqFifo.deq;
        return reqFifo.first;
      endmethod
    endinterface

    interface Put response;
      method Action put(WideMemResp r);
        respFifo.enq(truncate(r));
      endmethod
    endinterface
  endinterface
endmodule
