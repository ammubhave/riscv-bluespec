import Types::*;
import Vector::*;

typedef Data Line;

typedef Line MemResp;

typedef enum{Ld, St} MemOp deriving(Eq,Bits);
typedef struct{
    MemOp op;
    ByteEn byteEn;
    Addr  addr;
    Data  data;
} MemReq deriving(Eq, Bits);

typedef 512 WideLineSz;
typedef Bit#(WideLineSz) WideLine;
typedef WideLine WideMemResp;

typedef TDiv#(WideLineSz, 8) WideNumBytes;
typedef TDiv#(WideLineSz, DataSz) WideNumData;
typedef TLog#(WideNumBytes) WideIndxShamt;
typedef Vector#(WideNumBytes, Bool) WideByteEn;

typedef struct {
  MemOp op;
  WideByteEn byteEn;
  Addr addr;
  WideLine data;
} WideMemReq deriving(Eq, Bits);

typedef enum {InstMem, DataMem} MemPort deriving (Bits, Eq);

Bit#(3) memB    = 3'b000;
Bit#(3) memH    = 3'b001;
Bit#(3) memW    = 3'b010;
Bit#(3) memD    = 3'b011;
Bit#(3) memBU   = 3'b100;
Bit#(3) memHU   = 3'b101;
Bit#(3) memWU   = 3'b110;

typedef 16 NumTokens;
typedef Bit#(TLog#(NumTokens)) Token;

typedef 16 LoadBufferSz;
typedef Bit#(TLog#(LoadBufferSz)) LoadBufferIndex;

interface MemoryToHost;
  method ActionValue#(Tuple2#(MemPort, MemReq)) req;
  method Action resp(Tuple2#(MemPort, MemResp) r);
endinterface

(* synthesize *)
module mkDummyMemoryToHost(MemoryToHost);
  method ActionValue#(Tuple2#(MemPort, MemReq)) req if(False);
    return ?;
  endmethod

  method Action resp(Tuple2#(MemPort, MemResp) r);
  endmethod
endmodule

interface WideMemoryToHost;
  method ActionValue#(Tuple2#(MemPort, WideMemReq)) req;
  method Action resp(Tuple2#(MemPort, WideMemResp) r);
endinterface

(* synthesize *)
module mkDummyWideMemoryToHost(WideMemoryToHost);
  method ActionValue#(Tuple2#(MemPort, WideMemReq)) req if(False);
    return ?;
  endmethod

  method Action resp(Tuple2#(MemPort, WideMemResp) r);
  endmethod
endmodule
