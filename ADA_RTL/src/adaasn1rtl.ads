with Interfaces; 
use Interfaces;

package adaasn1rtl with Spark_Mode is


   --basic asn1scc type definitions
   type BIT is mod 2**1;
   
   subtype Asn1Byte is Interfaces.Unsigned_8;

   subtype Asn1Int is Interfaces.Integer_64;
   subtype Asn1UInt is Interfaces.Unsigned_64;
   subtype Asn1Real is Standard.Long_Float;

   subtype Asn1Boolean is Boolean;
   
   
   -- OBJECT IDENTIFIER
   OBJECT_IDENTIFIER_MAX_LENGTH : constant Integer       := 20;        -- the maximum number of components for Object Identifier
   SUBTYPE ObjectIdentifier_length_index is integer range 0..OBJECT_IDENTIFIER_MAX_LENGTH;
   SUBTYPE ObjectIdentifier_index is integer range 1..OBJECT_IDENTIFIER_MAX_LENGTH;
   type ObjectIdentifier_array is array (ObjectIdentifier_index) of Asn1UInt;

   type Asn1ObjectIdentifier is  record
      Length : ObjectIdentifier_length_index;
      values  : ObjectIdentifier_array;
   end record;
   
   
   type ASN1_RESULT is record
      Success   : Boolean;
      ErrorCode : Integer;
   end record;
   
   
   type TEST_CASE_STEP is
     (TC_VALIDATE, TC_ENCODE, TC_DECODE, TC_VALIDATE_DECODED, TC_EQUAL);

   type TEST_CASE_RESULT is record
      Step      : TEST_CASE_STEP;
      Success   : Boolean;
      ErrorCode : Integer;
   end record;
   
   subtype BIT_RANGE is Natural range 0 .. 7;
   
   
   type OctetBuffer is array (Natural range <>) of Asn1Byte;
   
   subtype OctetBuffer_0_7 is OctetBuffer (BIT_RANGE);
      
   type BitStream  (bitStreamSizeInBytes:Positive) is record
      buffer          : OctetBuffer(1..bitStreamSizeInBytes) ;
      currentBytePos  : Positive;
      currentBitPos   : BIT_RANGE;
   end record;
   
   
   --Bit strean functions
   
   function BitStream_init(bitStreamSizeInBytes : Positive) return BitStream  with
       Post    => BitStream_init'Result.currentBytePos = 1 and BitStream_init'Result.currentBitPos = 0 and BitStream_init'Result.bitStreamSizeInBytes = bitStreamSizeInBytes;
   
   
   procedure BitStream_AppendBit(bs : in out BitStream; bitValue : in BIT) with
     Depends => (bs => (bs, bitValue)),
     Pre     => (bs.currentBitPos = 7 and bs.currentBytePos < bs.bitStreamSizeInBytes) or (bs.currentBytePos = bs.bitStreamSizeInBytes and  bs.currentBitPos < 7),
     Post    => 
       (bs'Old.currentBitPos < 7 and then (bs.currentBitPos = bs'Old.currentBitPos + 1 and bs.currentBytePos = bs'Old.currentBytePos)) or
       (bs'Old.currentBitPos = 7 and then (bs.currentBitPos = 0 and bs.currentBytePos = bs'Old.currentBytePos + 1)) ;
   
   --BitStream_ReadBit
   procedure BitStream_ReadBit(bs : in out BitStream; bitValue : out BIT; result :    out Boolean) with
     Depends => (bs => (bs), bitValue => bs, result => bs),
     Pre     => (bs.currentBitPos = 7 and bs.currentBytePos < bs.bitStreamSizeInBytes) or (bs.currentBytePos = bs.bitStreamSizeInBytes and  bs.currentBitPos < 7),
     Post    => 
       (bs'Old.currentBitPos < 7 and then (bs.currentBitPos = bs'Old.currentBitPos + 1 and bs.currentBytePos = bs'Old.currentBytePos)) or
       (bs'Old.currentBitPos = 7 and then (bs.currentBitPos = 0 and bs.currentBytePos = bs'Old.currentBytePos + 1)) ;
   
   procedure BitStream_AppendByte(bs : in out BitStream; ByteValue : in Asn1Byte; negate : in Boolean) with
     Depends => (bs => (bs, byteValue, negate)),
     Pre     => bs.currentBytePos < bs.bitStreamSizeInBytes,
     Post    => bs.currentBytePos = bs'Old.currentBytePos + 1;     
   
   
   procedure BitStream_DecodeByte(bs : in out BitStream; byteValue : out Asn1Byte; success   :    out Boolean) with
     Depends => (bs => (bs), byteValue => bs, success => bs),
     Pre     => bs.currentBytePos < bs.bitStreamSizeInBytes,
     Post    => bs.currentBytePos = bs'Old.currentBytePos + 1;     
   
   

end adaasn1rtl;
