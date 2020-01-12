package adaasn1rtl.encoding.uper with Spark_Mode is

   subtype OctetArray1K is OctetBuffer (1 .. 1024);
   
   
   procedure UPER_Enc_Boolean (bs : in out BitStream; Val : in  Asn1Boolean) with
     Depends => (bs => (bs, Val)),
     Pre     => bs.Current_Bit_Pos < Natural'Last and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 1;

   procedure UPER_Dec_boolean (bs : in out BitStream; val: out Asn1Boolean; result : out Boolean) with
     Depends => (bs => (bs), val => bs, result => bs),
     Pre     => bs.Current_Bit_Pos < Natural'Last and then  
                bs.Size_In_Bytes < Positive'Last/8 and then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8,
     Post    => result  and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 1;
   

   --UPER encode functions 
   procedure UPER_Enc_SemiConstraintWholeNumber (bs : in out BitStream; IntVal : in Asn1Int; MinVal : in     Asn1Int) with
     Depends => (bs => (bs, IntVal, MinVal)),
     Pre     => IntVal >= MinVal and then
                bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);
   
   procedure UPER_Dec_SemiConstraintWholeNumber (bs : in out BitStream; IntVal : out Asn1Int; MinVal : in  Asn1Int;  Result :    out Boolean) with
      Depends => ((IntVal, Result) => (bs, MinVal), bs => (bs, MinVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8) and IntVal >= MinVal ;
   
   procedure UPER_Enc_SemiConstraintPosWholeNumber (bs : in out BitStream; IntVal : in Asn1UInt; MinVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal, MinVal)),
     Pre     => IntVal >= MinVal and then
                bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);  
   
   procedure UPER_Dec_SemiConstraintPosWholeNumber (bs : in out BitStream; IntVal : out Asn1UInt; MinVal : in     Asn1UInt; Result :    out Boolean)  with
     Depends => ((IntVal, Result) => (bs, MinVal), bs => (bs, MinVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8) and IntVal >= MinVal ;   
   
   
   procedure UPER_Enc_ConstraintWholeNumber (bs : in out BitStream; IntVal : in Asn1Int; MinVal : in Asn1Int; nBits : in Integer) with
     Depends => (bs => (bs, IntVal, MinVal, nBits)),
     Pre     => IntVal >= MinVal and then
                nBits >= 0 and then nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;
   
   procedure UPER_Enc_ConstraintPosWholeNumber (bs : in out BitStream; IntVal: in Asn1UInt; MinVal : in Asn1UInt; nBits : in Integer) with
     Depends => (bs => (bs, IntVal, MinVal, nBits)),
     Pre     => IntVal >= MinVal and then
                nBits >= 0 and then nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;
   
   procedure UPER_Dec_ConstraintWholeNumber (bs : in out BitStream; IntVal : out Asn1Int; MinVal : in Asn1Int; MaxVal : in Asn1Int; nBits : in Integer; Result : out Boolean) with
     Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal, nBits)),
     Pre     => MinVal <= MaxVal and then
                nBits >= 0 and then nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
                (
                     (Result   and  (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
                     (not Result  and  (IntVal = MinVal))
                );
   
   procedure UPER_Dec_ConstraintPosWholeNumber (bs : in out BitStream; IntVal : out Asn1UInt; MinVal : in Asn1UInt; MaxVal : in Asn1UInt; nBits : in Integer; Result : out Boolean) with
     Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal, nBits)),
     Pre     => MinVal <= MaxVal and then
                nBits >= 0 and then nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
                (
                     (Result   and  (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
                     (not Result  and  (IntVal = MinVal))
                );
   procedure UPER_Dec_ConstraintWholeNumberInt
     (bs : in out BitStream;
      IntVal      :    out Integer;
      MinVal      : in     Integer;
      MaxVal      : in     Integer;
      nBits : in     Integer;
      Result      :    out Boolean) with
     Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal, nBits)),
     Pre     => MinVal <= MaxVal and then
                nBits >= 0 and then nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
                (
                     (Result   and  (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
                     (not Result  and  (IntVal = MinVal))
                );
   
   procedure UPER_Enc_UnConstraintWholeNumber (bs : in out BitStream; IntVal : in Asn1Int)  with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);  


   procedure UPER_Dec_UnConstraintWholeNumber (bs : in out BitStream; IntVal :    out Asn1Int; Result :    out Boolean) with
     Depends => ((IntVal, bs, Result) => bs),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);  
   
   procedure UPER_Dec_UnConstraintWholeNumberMax (bs : in out BitStream; IntVal : out Asn1Int;  MaxVal : in Asn1Int; Result : out Boolean) with
     Depends => ((IntVal, bs, Result) => (bs, MaxVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);  
   
   -- REAL FUNCTIONS
   
   function CalcReal (Factor : Asn1UInt; N : Asn1UInt; base : Integer;Exp : Integer) return Asn1Real 
   with
       Pre => 
         (Factor = 1 or Factor=2 or Factor=4 or Factor=8) and then
         (base = 2 or base = 8 or base = 16);
   
   procedure UPER_Enc_Real (bs : in out BitStream;  RealVal : in     Asn1Real) with
     Depends => (bs => (bs, RealVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 40) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 40),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 40);  
   
   procedure UPER_Dec_Real (bs : in out BitStream; RealVal : out Asn1Real; Result  : out ASN1_RESULT) with
     Depends => ((bs, RealVal, Result) => (bs)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 40) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 40),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 40);  
   

   
   
   procedure ObjectIdentifier_uper_encode(bs : in out BitStream; val : Asn1ObjectIdentifier) with
     Depends => (bs => (bs, val)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (16 + 8*OBJECT_IDENTIFIER_MAX_LENGTH * OctetBuffer_16'Last) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (16 + 8*OBJECT_IDENTIFIER_MAX_LENGTH * OctetBuffer_16'Last),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (16 + 8*OBJECT_IDENTIFIER_MAX_LENGTH * OctetBuffer_16'Last)  
   ;
  
   procedure RelativeOID_uper_encode(bs : in out BitStream; val : Asn1ObjectIdentifier) with
     Depends => (bs => (bs, val)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (16 + 8*OBJECT_IDENTIFIER_MAX_LENGTH * OctetBuffer_16'Last) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (16 + 8*OBJECT_IDENTIFIER_MAX_LENGTH * OctetBuffer_16'Last),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (16 + 8*OBJECT_IDENTIFIER_MAX_LENGTH * OctetBuffer_16'Last)  
   ;
   
   
   
   
   procedure ObjectIdentifier_uper_decode(bs : in out BitStream;  val :    out Asn1ObjectIdentifier;  Result  :    out ASN1_RESULT) with
     Depends => ((bs, val, Result) => (bs)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (16 + 8*OBJECT_IDENTIFIER_MAX_LENGTH * OctetBuffer_16'Last) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (16 + 8*OBJECT_IDENTIFIER_MAX_LENGTH * OctetBuffer_16'Last),
     Post    => 
       bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (16 + 8*OBJECT_IDENTIFIER_MAX_LENGTH * OctetBuffer_16'Last)  
       and 
       bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos ;
   
   procedure RelativeOID_uper_decode(bs : in out BitStream;  val :    out Asn1ObjectIdentifier;  Result  :    out ASN1_RESULT) with
     Depends => ((bs, val, Result) => (bs)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (16 + 8*OBJECT_IDENTIFIER_MAX_LENGTH * OctetBuffer_16'Last) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (16 + 8*OBJECT_IDENTIFIER_MAX_LENGTH * OctetBuffer_16'Last),
     Post    => 
       bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (16 + 8*OBJECT_IDENTIFIER_MAX_LENGTH * OctetBuffer_16'Last)  
       and 
       bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos 
   ;
   

    procedure BitStream_EncodeOctetString_no_length(bs : in out Bitstream; data : in OctetBuffer; data_length : integer) with
     Pre     => 
        data_length >= 0 and then
        data'Last  >=  data'First and then
        data'Last < Positive'Last/8 and then
        data'Last  - data'First < Positive'Last/8  and then
        data_length <= data'Last  - data'First + 1 and then
        data_length < Positive'Last/8 and  then
        bs.Size_In_Bytes < Positive'Last/8 and  then
        bs.Current_Bit_Pos < Natural'Last - 8*data_length and then  
        bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8*data_length,
     Post    => 
       bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8*data_length  
       ;


     procedure BitStream_DecodeOctetString_no_length(bs : in out Bitstream; data : in out OctetBuffer; data_length : integer; success   :    out Boolean) with
     Pre     => 
        data_length >= 0 and then
        data'Last  >=  data'First and then
        data'Last < Positive'Last/8 and then
        data'Last  - data'First < Positive'Last/8  and then
        data_length <= data'Last  - data'First + 1 and then
        data_length < Positive'Last/8 and  then
        bs.Size_In_Bytes < Positive'Last/8 and  then
        bs.Current_Bit_Pos < Natural'Last - 8*data_length and then  
        bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8*data_length,
     Post    => 
       bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8*data_length  
       ;


    procedure BitStream_EncodeOctetString(bs : in out Bitstream; data : in OctetBuffer; data_length : integer; nBits : in Integer; asn1SizeMin : Integer; asn1SizeMax : Integer) with
     Pre     => 
        asn1SizeMin >= 0 and then asn1SizeMin <= asn1SizeMax and then
        nBits >= 0 and then nBits < Asn1UInt'Size and then
        data_length >= 0 and then
        data_length >= asn1SizeMin and then
        data'Last  >=  data'First and then
        data'Last < Positive'Last/8 and then
        data'Last  - data'First < Positive'Last/8  and then
        data_length <= data'Last  - data'First + 1 and then
        data_length < Positive'Last/8 - nBits and  then
        bs.Size_In_Bytes < Positive'Last/8 and  then
        bs.Current_Bit_Pos < Natural'Last - (8*data_length + nBits)  and then  
        bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (8*data_length + nBits),
     Post    => 
       bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (8*data_length + nBits)  
       and 
       bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos      
       ;
       
    procedure BitStream_DecodeOctetString(bs : in out Bitstream; data : in out OctetBuffer; data_length : out integer; nBits : in Integer; asn1SizeMin : Integer; asn1SizeMax : Integer; success   :    out Boolean) with
     Pre     => 
        asn1SizeMin >= 0 and then asn1SizeMin <= asn1SizeMax and then
        asn1SizeMax < Positive'Last and then
        
        nBits >= 0 and then nBits < Asn1UInt'Size and then
        data'Last  >=  data'First and then
        data'Last < Positive'Last/8 and then
        data'Last  - data'First < Positive'Last/8  and then
        asn1SizeMax <= data'Last  - data'First + 1 and then
        asn1SizeMax  < Positive'Last/8 - nBits and  then
        bs.Size_In_Bytes < Positive'Last/8 and  then
        bs.Current_Bit_Pos < Natural'Last - (8*asn1SizeMax + nBits)  and then  
        bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (8*asn1SizeMax + nBits),
     Post    => 
       bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (8*asn1SizeMax + nBits)  
       and 
       bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos      
       and
       data_length >= asn1SizeMin  and data_length <= asn1SizeMax
       ;
       
       
--      procedure BitStream_EncodeBitString_no_length(bs : in out Bitstream; data : in OctetBuffer; data_length : integer) with
--       Pre     => 
--          data_length >= 0 and then
--          data'Last  >=  data'First and then
--          data'Last < Positive'Last/8 and then
--          data'Last  - data'First < Positive'Last/8  and then
--          data_length <= Positive'Last/8 and then
--          data'Last >= data'First + data_length / 8 and then
--          bs.Size_In_Bytes < Positive'Last/8 and  then
--          bs.Current_Bit_Pos < Natural'Last - data_length and then  
--          bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - data_length,
--       Post    => 
--         bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + data_length 
--         ;       
--  
--      procedure BitStream_DecodeBitString_no_length(bs : in out Bitstream; data : in out OctetBuffer; data_length : integer; success   :    out Boolean) with
--       Pre     => 
--          data_length >= 0 and then
--          data'Last  >=  data'First and then
--          data'Last < Positive'Last/8 and then
--          data'Last  - data'First < Positive'Last/8  and then
--          data_length <= 8*data'Last  - 8*data'First + 8 and then
--          data_length < Positive'Last and  then
--          bs.Size_In_Bytes < Positive'Last/8 and  then
--          bs.Current_Bit_Pos < Natural'Last - data_length and then  
--          bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - data_length,
--       Post    => 
--         bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8*data_length  
--         ;
       
    procedure BitStream_EncodeBitString(bs : in out Bitstream; data : in OctetBuffer; data_length : integer; nBits : in Integer; asn1SizeMin : Integer; asn1SizeMax : Integer) with
     Pre     => 
        asn1SizeMin >= 0 and then asn1SizeMin <= asn1SizeMax and then
        nBits >= 0 and then nBits < Asn1UInt'Size and then
        data_length >= 0 and then
        data_length >= asn1SizeMin and then
        data'Last  >=  data'First and then
        data'Last < Positive'Last and then
        data'Last  - data'First < Positive'Last  and then
        data_length <= data'Last  - data'First + 1 and then
        data_length < Positive'Last - nBits and  then
        bs.Size_In_Bytes < Positive'Last/8 and  then
        bs.Current_Bit_Pos < Natural'Last - (data_length + nBits)  and then  
        bs.Current_Bit_Pos <= bs.Size_In_Bytes*8  - (data_length + nBits),
     Post    => 
       bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (data_length + nBits)  
       and 
       bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos      
       ;
    
    procedure BitStream_DecodeBitString(bs : in out Bitstream; data : in out OctetBuffer; data_length : out integer; nBits : in Integer; asn1SizeMin : Integer; asn1SizeMax : Integer; success   :    out Boolean) with
     Pre     => 
        asn1SizeMin >= 0 and then asn1SizeMin <= asn1SizeMax and then
        asn1SizeMax < Positive'Last and then
        
        nBits >= 0 and then nBits < Asn1UInt'Size and then
        data'Last  >=  data'First and then
        data'Last < Positive'Last and then
        data'Last  - data'First < Positive'Last  and then
        asn1SizeMax <= data'Last  - data'First + 1 and then
        asn1SizeMax  < Positive'Last - nBits and  then
        bs.Size_In_Bytes < Positive'Last/8 and  then
        bs.Current_Bit_Pos < Natural'Last - (asn1SizeMax + nBits)  and then  
        bs.Current_Bit_Pos <= bs.Size_In_Bytes*8 - (asn1SizeMax + nBits),
     Post    => 
       bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (asn1SizeMax + nBits)  
       and 
       bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos      
       and
       data_length >= asn1SizeMin  and data_length <= asn1SizeMax
       ;
    
end adaasn1rtl.encoding.uper;
