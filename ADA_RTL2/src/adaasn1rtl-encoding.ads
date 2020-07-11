with Interfaces; use Interfaces;

with Board_Config;
use Board_Config;


package adaasn1rtl.encoding with
     Spark_Mode is

   subtype BIT_RANGE is Natural range 0 .. 7;

   subtype OctetBuffer_16 is OctetBuffer (1 .. 16);
   subtype OctetArray4 is OctetBuffer (1 .. 4);
   subtype OctetArray8 is OctetBuffer (1 .. 8);

   subtype OctetBuffer_0_7 is OctetBuffer (BIT_RANGE);

   subtype Digits_Buffer is OctetBuffer (1 .. 20);

   pragma Warnings (Off, "comes too early and was moved down");
   pragma Warnings (Off,
          "component ""Buffer"" whose length depends on a discriminant");
   pragma Warnings (Off, "record layout may cause performance issues");
   type Bitstream (Size_In_Bytes : Positive) is record

      Buffer          : OctetBuffer (1 .. Size_In_Bytes);
      --  current bit for writing or reading in the bitsteam
      Current_Bit_Pos : Natural;
      pushDataPrm     : Integer := 0;
      fetchDataPrm    : Integer := 0;

   end record;
   pragma Warnings (On, "comes too early and was moved down");
   pragma Warnings (On,
          "component ""Buffer"" whose length depends on a discriminant");
   pragma Warnings (On, "record layout may cause performance issues");

   type BitstreamPtr is record
      Size_In_Bytes   : Positive;
      --  current bit for writing or reading in the bitsteam
      Current_Bit_Pos : Natural;
      pushDataPrm     : Integer := 0;
      fetchDataPrm    : Integer := 0;
   end record;

   function RequiresReverse return Boolean;
   function Long_Float_to_Float (x : Asn1Real) return Float;

   function To_UInt (IntVal : Asn1Int) return Asn1UInt;

   function To_Int
     (IntVal : Asn1UInt) return Asn1Int is
     (if IntVal > Asn1UInt (Asn1Int'Last) then -Asn1Int (not IntVal) - 1
      else Asn1Int (IntVal));

   function abs_value
     (intVal : Asn1Int) return Asn1UInt is
     (if intVal >= 0 then Asn1UInt (intVal)
      else (Asn1UInt (-(intVal + 1)) + 1));

   --  In some cases, SPARK cannot prove the following function
   --  This seems to be a bug since the function is proved if comment
   --  some irrelevant code e.g. the getStringSize function

   function max_value_with_n_bits(nBits  : Integer) return Asn1UInt is
     (if nBits = Asn1UInt'Size then Asn1UInt'Last else Shift_Left (Asn1UInt (1), nBits) - 1)
     with
       Pre => nBits > 0 and nBits <= Asn1UInt'Size;

   function To_Int_n
     (IntVal : Asn1UInt;
      nBits  : Integer) return Asn1Int is
     (if
        IntVal > Asn1UInt (Shift_Left (Asn1UInt (1), nBits - 1) - 1)
      then
      --   is given value greater than the maximum pos value in nBits space?
        -Asn1Int
        (not ((not (max_value_with_n_bits(nBits))) or IntVal)) -  1
        -- in this case the number is negative ==> prefix with 1111
      else Asn1Int (IntVal)) with
      Pre => nBits > 0 and nBits <= Asn1UInt'Size;

   function Sub (A : Asn1Int; B : Asn1Int) return Asn1UInt with
      Pre => A >= B;

   function stringContainsChar
     (str : String;
      ch  : Character) return Boolean with
      Pre => str'Last < Natural'Last;

   function GetBytes (V : Asn1UInt) return Asn1Byte with
      Post => GetBytes'Result >= 1 and GetBytes'Result <= Asn1UInt'Size/8;

   function GetLengthInBytesOfSInt (V : Asn1Int) return Asn1Byte with
      Post => GetLengthInBytesOfSInt'Result >= 1 and
      GetLengthInBytesOfSInt'Result <= Asn1UInt'Size/8;







   function PLUS_INFINITY return Asn1Real;
   function MINUS_INFINITY return Asn1Real;

   function GetExponent (V : Asn1Real) return Asn1Int;
   function GetMantissa (V : Asn1Real) return Asn1UInt;
   function RequiresReverse (dummy : Boolean) return Boolean;

   function milbus_encode
     (IntVal : Asn1Int) return Asn1Int is
     (if IntVal = 32 then 0 else IntVal);

   function milbus_decode
     (IntVal : Asn1Int) return Asn1Int is
     (if IntVal = 0 then 32 else IntVal);

   --  Bit strean functions

   function BitStream_init
     (Bitstream_Size_In_Bytes : Positive) return Bitstream with
      Pre  => Bitstream_Size_In_Bytes < Positive'Last / 8,
      Post => BitStream_init'Result.Current_Bit_Pos = 0 and
      BitStream_init'Result.Size_In_Bytes = Bitstream_Size_In_Bytes;

   function BitStream_current_length_in_bytes
     (bs : Bitstream) return Natural is
     ((bs.Current_Bit_Pos + 7) / 8)
   with Pre =>  bs.Current_Bit_Pos <= Natural'Last - 7;

   procedure BitStream_AppendBit
     (bs        : in out Bitstream;
      Bit_Value :      BIT) with
      Depends => (bs => (bs, Bit_Value)),
      Pre     => bs.Current_Bit_Pos < Natural'Last
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos < bs.Size_In_Bytes * 8,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 1;

      --  BitStream_ReadBit
   procedure BitStream_ReadBit
     (bs        : in out Bitstream;
      Bit_Value :    out BIT;
      result    :    out Boolean) with
      Depends => (bs => (bs), Bit_Value => bs, result => bs),
      Pre     => bs.Current_Bit_Pos < Natural'Last
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos < bs.Size_In_Bytes * 8,
      Post => result and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 1;

   procedure BitStream_AppendByte
     (bs         : in out Bitstream;
      Byte_Value :      Asn1Byte;
      Negate     :      Boolean) with
      Depends => (bs => (bs, Byte_Value, Negate)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8;

   procedure BitStream_DecodeByte
     (bs         : in out Bitstream;
      Byte_Value :    out Asn1Byte;
      success    :    out Boolean) with
      Depends => (bs => (bs), Byte_Value => bs, success => bs),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8,
      Post => success and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8;

   function BitStream_PeekByte
     (bs     : Bitstream;
      offset : Natural) return Asn1Byte with
      Pre => bs.Current_Bit_Pos < Natural'Last - offset - 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - offset - 8;

   procedure BitStream_AppendBits
     (bs                 : in out Bitstream;
      bitMaskAsByteArray : OctetBuffer;
      bits_to_write      : Natural) with
      Depends => (bs => (bs, bitMaskAsByteArray, bits_to_write)),
      Pre     => bitMaskAsByteArray'First >= 0
      and then bitMaskAsByteArray'Last < Natural'Last / 8
      and then bits_to_write >= (bitMaskAsByteArray'Length - 1) * 8
      and then bits_to_write <= (bitMaskAsByteArray'Length) * 8
      and then bs.Current_Bit_Pos < Natural'Last - bits_to_write
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - bits_to_write,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + bits_to_write;

   procedure BitStream_ReadBits
     (bs                 : in out Bitstream;
      bitMaskAsByteArray : in out OctetBuffer;
      bits_to_read       :        Natural;
      success            :    out Boolean) with
      Pre     => bitMaskAsByteArray'First >= 0
      and then bitMaskAsByteArray'Last < Natural'Last / 8
      and then bits_to_read <= (bitMaskAsByteArray'Length) * 8
      and then bs.Current_Bit_Pos < Natural'Last - bits_to_read
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - bits_to_read,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + bits_to_read;

   procedure BitStream_SkipBits
     (bs           : in out Bitstream;
      bits_to_skip :        Natural) with
      Depends => ((bs) => (bs, bits_to_skip)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - bits_to_skip,
      Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + bits_to_skip;

   procedure BitStream_ReadNibble
     (bs         : in out Bitstream;
      Byte_Value :    out Asn1Byte;
      success    :    out Boolean) with
      Depends => (bs => (bs), Byte_Value => bs, success => bs),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 4
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 4,
      Post => success and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 4;

   procedure BitStream_AppendPartialByte
     (bs         : in out Bitstream;
      Byte_Value :      Asn1Byte;
      nBits      :      BIT_RANGE;
      negate     :      Boolean) with
      Depends => (bs => (bs, Byte_Value, negate, nBits)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;

   procedure BitStream_ReadPartialByte
     (bs         : in out Bitstream;
      Byte_Value :    out Asn1Byte;
      nBits      :      BIT_RANGE) with
      Depends => ((bs, Byte_Value) => (bs, nBits)),
     Pre     => nBits >0
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;

   function BitStream_PeekPartialByte
     (bs     :  Bitstream;
      offset :  Natural;
      nBits  :  BIT_RANGE) return Asn1Byte with
     Pre =>
       bs.Current_Bit_Pos < Natural'Last - nBits - offset
       and then nBits > 0
       and then bs.Size_In_Bytes < Positive'Last / 8
       and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits - offset
     ;

   procedure BitStream_Encode_Non_Negative_Integer
     (bs       : in out Bitstream;
      intValue :      Asn1UInt;
      nBits    :      Integer) with
      Depends => (bs => (bs, intValue, nBits)),
      Pre     => nBits >= 0
      and then nBits <= Asn1UInt'Size
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;

   procedure BitStream_Decode_Non_Negative_Integer
     (bs       : in out Bitstream;
      IntValue :    out Asn1UInt;
      nBits    :      Integer;
      result   :    out Boolean) with
      Depends => ((bs, IntValue, result) => (bs, nBits)),
      Pre     => nBits >= 0
      and then nBits <= Asn1UInt'Size
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => result and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;

   procedure Enc_UInt
     (bs          : in out Bitstream;
      intValue    :      Asn1UInt;
      total_bytes :      Integer) with
      Depends => (bs => (bs, intValue, total_bytes)),
      Pre     => total_bytes >= 0
      and then total_bytes <= Asn1UInt'Size / 8
      and then bs.Current_Bit_Pos < Natural'Last - total_bytes * 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - total_bytes * 8,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes * 8;

   procedure Dec_UInt
     (bs          : in out Bitstream;
      total_bytes :        Integer;
      Ret         :    out Asn1UInt;
      Result      :    out Boolean) with
      Depends =>
      (Ret    => (bs, total_bytes),
       Result => (bs, total_bytes),
       bs     => (bs, total_bytes)),
      Pre => total_bytes >= 0
      and then total_bytes <= Asn1UInt'Size / 8
      and then bs.Current_Bit_Pos < Natural'Last - total_bytes * 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - total_bytes * 8,
      Post => Result and then
      bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes * 8 and then
      ((total_bytes < Asn1UInt'Size/8 and then  Ret < 256**total_bytes) or else True);

   procedure Dec_Int
     (bs          : in out Bitstream;
      total_bytes :        Integer;
      int_value   :    out Asn1Int;
      Result      :    out Boolean) with
      Depends =>
      (int_value => (bs, total_bytes),
       Result    => (bs, total_bytes),
       bs        => (bs, total_bytes)),
      Pre => total_bytes >= 0
      and then total_bytes <= Asn1UInt'Size / 8
      and then bs.Current_Bit_Pos < Natural'Last - total_bytes * 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - total_bytes * 8,
      Post => Result and
      bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes * 8;

   pragma Inline (BitStream_AppendByte);

   procedure Enc_SemiConstraintWholeNumber
     (bs     : in out Bitstream;
      IntVal :      Asn1Int;
      MinVal :      Asn1Int) with
      Depends => (bs => (bs, IntVal, MinVal)),
      Pre     => IntVal >= MinVal
      and then bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then
        bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);

   procedure Dec_SemiConstraintWholeNumber
     (bs     : in out Bitstream;
      IntVal :    out Asn1Int;
      MinVal :      Asn1Int;
      Result :    out Boolean) with
      Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then
        bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8) and
      IntVal >= MinVal;

   procedure Enc_SemiConstraintPosWholeNumber
     (bs     : in out Bitstream;
      IntVal :      Asn1UInt;
      MinVal :      Asn1UInt) with
      Depends => (bs => (bs, IntVal, MinVal)),
      Pre     => IntVal >= MinVal
      and then bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then
        bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);

   procedure Dec_SemiConstraintPosWholeNumber
     (bs     : in out Bitstream;
      IntVal :    out Asn1UInt;
      MinVal :      Asn1UInt;
      Result :    out Boolean) with
      Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then
        bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8) and
      IntVal >= MinVal;

   procedure Enc_UnConstraintWholeNumber
     (bs     : in out Bitstream;
      IntVal :      Asn1Int) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then
        bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);

   procedure Dec_UnConstraintWholeNumber
     (bs     : in out Bitstream;
      IntVal :    out Asn1Int;
      Result :    out Boolean) with
      Depends => ((IntVal, bs, Result) => bs),
      Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then
        bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);

   procedure Enc_ConstraintWholeNumber
     (bs     : in out Bitstream;
      IntVal :      Asn1Int;
      MinVal :      Asn1Int;
      nBits  :      Integer) with
      Depends => (bs => (bs, IntVal, MinVal, nBits)),
      Pre     => IntVal >= MinVal
      and then nBits >= 0
      and then nBits <= Asn1UInt'Size
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;

   procedure Enc_ConstraintPosWholeNumber
     (bs     : in out Bitstream;
      IntVal :      Asn1UInt;
      MinVal :      Asn1UInt;
      nBits  :      Integer) with
      Depends => (bs => (bs, IntVal, MinVal, nBits)),
      Pre     => IntVal >= MinVal
      and then nBits >= 0
      and then nBits <= Asn1UInt'Size
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;

   procedure Dec_ConstraintWholeNumber
     (bs     : in out Bitstream;
      IntVal :    out Asn1Int;
      MinVal :      Asn1Int;
      MaxVal :      Asn1Int;
      nBits  :      Integer;
      Result :    out Boolean) with
      Pre     => MinVal <= MaxVal
      and then nBits >= 0
      and then nBits <= Asn1UInt'Size
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
      ((Result and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result and (IntVal = MinVal)));

   procedure Dec_ConstraintPosWholeNumber
     (bs     : in out Bitstream;
      IntVal :    out Asn1UInt;
      MinVal :      Asn1UInt;
      MaxVal :      Asn1UInt;
      nBits  :      Integer;
      Result :    out Boolean) with
      Pre     => MinVal <= MaxVal
      and then nBits >= 0
      and then nBits <= Asn1UInt'Size
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
      ((Result and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result and (IntVal = MinVal)));

   procedure Dec_ConstraintWholeNumberInt
     (bs     : in out Bitstream;
      IntVal :    out Integer;
      MinVal :      Integer;
      MaxVal :      Integer;
      nBits  :      Integer;
      Result :    out Boolean) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal, nBits)),
      Pre     => MinVal <= MaxVal
      and then nBits >= 0
      and then nBits <= Asn1UInt'Size
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
      ((Result and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result and (IntVal = MinVal)));

   function BitStream_bitPatternMatches
     (bs                                  :  Bitstream;
      bit_terminated_pattern              :  OctetBuffer;
      bit_terminated_pattern_size_in_bits :    Natural) return Boolean with
      Pre => bit_terminated_pattern'First >= 0
      and then bit_terminated_pattern'Last < Natural'Last / 8
      and then
        bit_terminated_pattern_size_in_bits <=
        (bit_terminated_pattern'Length) * 8
      and then
        bs.Current_Bit_Pos <
        Natural'Last - bit_terminated_pattern_size_in_bits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then
        bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - bit_terminated_pattern_size_in_bits;

pragma Warnings (Off, """bs"" is not modified, could be IN");

   procedure bitstrean_fetch_data_if_required (bs : in out Bitstream)
     with
       Pre  => bs.Size_In_Bytes < Positive'Last - 8,
       Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos;

   procedure bitstrean_push_data_if_required (bs : in out Bitstream)
     with
       Pre  => bs.Size_In_Bytes < Positive'Last - 8,
       Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos;
pragma Warnings (On, """bs"" is not modified, could be IN");

end adaasn1rtl.encoding;
