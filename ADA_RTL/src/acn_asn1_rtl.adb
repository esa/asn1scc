with Ada.Unchecked_Conversion;
with uper_asn1_rtl;
use uper_asn1_rtl;

package body acn_asn1_rtl with Spark_Mode is

   procedure Acn_Enc_Int_PositiveInteger_ConstSize  (bs : in out BitStream; IntVal : in Asn1UInt;  sizeInBits: in Integer)
   is
   begin
      BitStream_Encode_Non_Negative_Integer (bs, IntVal, sizeInBits);
   end Acn_Enc_Int_PositiveInteger_ConstSize;
   
   procedure Acn_Enc_Int_PositiveInteger_ConstSize_8(bs : in out BitStream; IntVal : in     Asn1UInt)
   is
   begin
      BitStream_AppendByte(bs, Asn1Byte(IntVal) , false);
   end Acn_Enc_Int_PositiveInteger_ConstSize_8;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16 (bs : in out BitStream; IntVal : in     Asn1UInt)
   is
   begin
      Enc_UInt (bs, IntVal, 2);
   end Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32 (bs : in out BitStream;  IntVal : in     Asn1UInt)
   is
   begin
      Enc_UInt (bs, IntVal, 4);
   end Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64 (bs : in out BitStream;   IntVal : in     Asn1UInt)
   is
   begin
      Enc_UInt (bs, IntVal, 8);
   end Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64;
   

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N (bs : in out BitStream; IntVal : in     Asn1UInt; total_bytes : in     Integer) with
     Depends => (bs => (bs, IntVal, total_bytes)),
     Pre     => total_bytes >= 0 and then 
                total_bytes <= Asn1UInt'Size/8 and then 
                bs.Current_Bit_Pos < Natural'Last - total_bytes*8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - total_bytes*8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes*8
   is
      byteValue : Asn1Byte;
      tmp : Asn1UInt := IntVal;
   begin
      for i in  1..total_bytes loop
         byteValue := Asn1Byte(tmp mod 256);
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
         BitStream_AppendByte(bs, byteValue, false);
         tmp := tmp / 256;
      end loop;
   end Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N;
   
   
   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16 (bs : in out BitStream; IntVal : in     Asn1UInt)
   is
   begin
      Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(bs, IntVal, 2);
   end Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16;
   
   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32 (bs : in out BitStream; IntVal : in     Asn1UInt)
   is
   begin
      Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(bs, IntVal, 4);
   end Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32;
   
   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64 (bs : in out BitStream; IntVal : in     Asn1UInt)
   is
   begin
      Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(bs, IntVal, 8);
   end Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64;
   
   procedure Acn_Enc_Int_PositiveInteger_VarSize_LengthEmbedded  (bs : in out BitStream;  IntVal : in     Asn1UInt)
   is
   begin
      UPER_Enc_SemiConstraintPosWholeNumber (bs, IntVal, 0);
   end Acn_Enc_Int_PositiveInteger_VarSize_LengthEmbedded;
   
   procedure Acn_Enc_Int_TwosComplement_ConstSize (bs : in out BitStream; IntVal : in Asn1Int; sizeInBits   : in  Natural)
   is
   begin
      BitStream_Encode_Non_Negative_Integer (bs, To_UInt (IntVal), sizeInBits);
   end Acn_Enc_Int_TwosComplement_ConstSize;
   

   procedure Acn_Enc_Int_TwosComplement_ConstSize_8(bs : in out BitStream; IntVal : in     Asn1Int)
   is
   begin
      --Acn_Enc_Int_TwosComplement_ConstSize (bs, IntVal, 8);
      BitStream_AppendByte(bs, Asn1Byte(To_UInt(IntVal) and 16#FF#) , false);
   end Acn_Enc_Int_TwosComplement_ConstSize_8;
   
   
   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_16 (bs : in out BitStream; IntVal : in     Asn1Int)
   is
   begin
      Enc_UInt (bs, To_UInt(IntVal), 2);
   end Acn_Enc_Int_TwosComplement_ConstSize_big_endian_16;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_32 (bs : in out BitStream; IntVal : in     Asn1Int)
   is
   begin
      Enc_UInt (bs, To_UInt(IntVal), 4);
   end Acn_Enc_Int_TwosComplement_ConstSize_big_endian_32;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_64 (bs : in out BitStream; IntVal : in     Asn1Int)
   is
   begin
      Enc_UInt (bs, To_UInt(IntVal), 8);
   end Acn_Enc_Int_TwosComplement_ConstSize_big_endian_64;
   
   
   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_16 (bs : in out BitStream; IntVal : in Asn1Int)
   is
   begin
      Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(bs, To_UInt(IntVal), 2);
   end Acn_Enc_Int_TwosComplement_ConstSize_little_endian_16;
   
   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_32 (bs : in out BitStream; IntVal : in Asn1Int)
   is
   begin
      Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(bs, To_UInt(IntVal), 4);
   end Acn_Enc_Int_TwosComplement_ConstSize_little_endian_32;
   
   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_64 (bs : in out BitStream; IntVal : in Asn1Int)
   is
   begin
      Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(bs, To_UInt(IntVal), 8);
   end Acn_Enc_Int_TwosComplement_ConstSize_little_endian_64;
   
   procedure Acn_Enc_Int_TwosComplement_VarSize_LengthEmbedded  (bs : in out BitStream; IntVal : in Asn1Int)
   is
   begin
      UPER_Enc_UnConstraintWholeNumber (bs, IntVal);
   end Acn_Enc_Int_TwosComplement_VarSize_LengthEmbedded;
   

--   subtype OctetArray100 is OctetBuffer (1..100);
     
   procedure Acn_Enc_Int_BCD_ConstSize (bs : in out BitStream; IntVal : in Asn1UInt; nNibbles : in Integer) 
     
   is
      intValCopy   : Asn1UInt := IntVal;
      powOf10      : Asn1UInt;
      curNibble    : Asn1Byte;
   begin
      for i in 1 .. nNibbles loop
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*4);
         powOf10 := Powers_of_10(nNibbles - i);
         curNibble := Asn1Byte(intValCopy/powOf10 and 16#FF#);
         intValCopy := intValCopy mod powOf10;
         BitStream_AppendPartialByte (bs, curNibble, 4, False);
      end loop;
   end Acn_Enc_Int_BCD_ConstSize;
   
   
   procedure Acn_Enc_Int_BCD_VarSize_NullTerminated (bs : in out BitStream; IntVal : in     Asn1UInt)
   is
      totalNibbles : Integer;
   begin
      totalNibbles := Get_number_of_digits(IntVal);
      Acn_Enc_Int_BCD_ConstSize(bs, IntVal, totalNibbles);
      BitStream_AppendPartialByte (bs, 16#F#, 4, False);
   end Acn_Enc_Int_BCD_VarSize_NullTerminated;
   
   
    procedure Get_integer_digits(IntVal :in Asn1Uint; digits_array: out Digits_Buffer; totalDigits : out Asn1Byte) with
     Pre     => IntVal < Powers_of_10(19),
     Post    => totalDigits >=1 and totalDigits<=19 and IntVal < Powers_of_10(Integer(totalDigits)) and totalDigits = Asn1Byte(Get_number_of_digits(IntVal))
    is
      intValCopy   : Asn1UInt := IntVal;
      powOf10      : Asn1UInt;
      curNibble    : Asn1Byte;
    begin
      digits_array := (others => 0);
      totalDigits := Asn1Byte(Get_number_of_digits(IntVal));
      for i in 1 .. Integer(totalDigits) loop
         powOf10 := Powers_of_10(Integer(totalDigits) - i);
         curNibble := Asn1Byte(intValCopy/powOf10 and 16#F#);
         intValCopy := intValCopy mod powOf10;
         digits_array(i) := Asn1Byte(Character'Pos ('0') + curNibble);
      end loop;
    end Get_integer_digits;
   
   procedure Acn_Enc_Int_ASCII_VarSize_LengthEmbedded (bs : in out BitStream; IntVal : in     Asn1Int)
   is
        digits_array: Digits_Buffer;
        nChars : Asn1Byte;
        sing : Asn1Byte;
        absIntVal : Asn1Uint;

   begin
      
      absIntVal := Asn1Uint(abs IntVal);
      sing := (if intVal >= 0  then Character'Pos('+') else Character'Pos('-'));
      Get_integer_digits(absIntVal,digits_array, nChars);
      pragma Assert(nChars <= 18);

      -- encode length, plus 1 for sign
      BitStream_AppendByte(bs, nChars+1, False);

        -- encode sign
      BitStream_AppendByte(bs, sing, False);

      for i in 1..Integer(nChars) loop
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
         BitStream_AppendByte(bs, digits_array(i), False);
      end loop;


   end Acn_Enc_Int_ASCII_VarSize_LengthEmbedded;
   
   procedure Acn_Enc_UInt_ASCII_VarSize_LengthEmbedded (bs : in out BitStream;  IntVal : in     Asn1UInt)
   is
        digits_array: Digits_Buffer;
        nChars : Asn1Byte;
   begin
        Get_integer_digits(IntVal,digits_array, nChars);

        -- encode length
        BitStream_AppendByte(bs, nChars, False);

        for i in 1..Integer(nChars) loop
             pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
             BitStream_AppendByte(bs, digits_array(i), False);
        end loop;
      
   end Acn_Enc_UInt_ASCII_VarSize_LengthEmbedded;
   
   procedure Acn_Enc_Int_ASCII_VarSize_NullTerminated (bs : in out BitStream; IntVal : in Asn1Int; nullChar : in Asn1Byte)
   is
        digits_array: Digits_Buffer;
        nChars : Asn1Byte;
        sing : Asn1Byte;
        absIntVal : Asn1Uint;
   begin
        absIntVal := (if intVal >= 0  then Asn1Uint(intVal) else (Asn1Uint(-(intVal+1))+1));
        sing := (if intVal >= 0  then Character'Pos('+') else Character'Pos('-'));
        Get_integer_digits(absIntVal,digits_array, nChars);

        -- encode sign
        BitStream_AppendByte(bs, sing, False);

        -- encode digits
        for i in 1..Integer(nChars) loop
             pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
             BitStream_AppendByte(bs, digits_array(i), False);
        end loop;

        -- encode nullChar
        BitStream_AppendByte(bs, nullChar, False);

   end Acn_Enc_Int_ASCII_VarSize_NullTerminated;


   procedure Acn_Enc_UInt_ASCII_VarSize_NullTerminated (bs : in out BitStream; IntVal : in Asn1UInt; nullChar : in Asn1Byte)
   is
        digits_array: Digits_Buffer;
        nChars : Asn1Byte;
   begin
        Get_integer_digits(IntVal,digits_array, nChars);

        -- encode digits
        for i in 1..Integer(nChars) loop
             pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
             BitStream_AppendByte(bs, digits_array(i), False);
        end loop;

        -- encode nullChar
        BitStream_AppendByte(bs, nullChar, False);

    end Acn_Enc_UInt_ASCII_VarSize_NullTerminated;
   
   
   ---------------------- Decoding functions ------------------------------------------
   
   procedure Acn_Dec_Int_PositiveInteger_ConstSize (bs : in out BitStream; IntVal :out Asn1UInt; minVal : in Asn1UInt; maxVal : in Asn1UInt; nSizeInBits : in Integer; Result: out ASN1_RESULT)
   is
      encVal : Asn1UInt;
   begin
      Result.ErrorCode := 0;
      BitStream_Decode_Non_Negative_Integer (bs,  encVal,  nSizeInBits,    Result.Success);
      pragma Assert(Result.Success);
      IntVal := encVal;

      Result.Success := IntVal >= minVal and IntVal <= maxVal;
      if not Result.Success then
         IntVal           := minVal;
         Result.ErrorCode := ERR_INCORRECT_STREAM;
      end if;
   end Acn_Dec_Int_PositiveInteger_ConstSize;
   
   
   
   procedure Acn_Dec_Int_PositiveInteger_ConstSize_8 (bs : in out BitStream; IntVal : out Asn1UInt; minVal : in Asn1UInt; maxVal : in Asn1UInt; Result :    out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_PositiveInteger_ConstSize(bs, IntVal, minVal, maxVal, 8,    Result);
   end Acn_Dec_Int_PositiveInteger_ConstSize_8;
   

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16 (bs : in out BitStream; IntVal : out Asn1UInt; minVal : in Asn1UInt; maxVal : in Asn1UInt; Result :    out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_PositiveInteger_ConstSize(bs, IntVal, minVal, maxVal, 16,    Result);
   end Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16;

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32 (bs : in out BitStream; IntVal : out Asn1UInt; minVal : in Asn1UInt; maxVal : in Asn1UInt; Result :    out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_PositiveInteger_ConstSize(bs, IntVal, minVal, maxVal, 32,    Result);
   end Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32;
   
   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64 (bs : in out BitStream; IntVal : out Asn1UInt; minVal : in Asn1UInt; maxVal : in Asn1UInt; Result :    out ASN1_RESULT)
   is
   begin
      Result.ErrorCode := 0;
      Dec_UInt(bs, 8, IntVal, Result.Success);
      Result.Success := IntVal >= minVal and IntVal <= maxVal;
      if not Result.Success then
         IntVal           := minVal;
         Result.ErrorCode := ERR_INCORRECT_STREAM;
      end if;
   end Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64;
   

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N0 (bs : in out BitStream; IntVal : out Asn1UInt; total_bytes : in Integer) with
     Pre     => total_bytes >= 0 and then 
                total_bytes <= Asn1UInt'Size/8 and then 
                bs.Current_Bit_Pos < Natural'Last - total_bytes*8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - total_bytes*8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes*8 
   is
      byteValue : Asn1Byte;
      result : Asn1Boolean;
   begin
      IntVal := 0;
      for i in  1..total_bytes loop
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
         BitStream_DecodeByte(bs, byteValue, result);
         pragma Assert(result);
         IntVal := IntVal or Shift_Left(Asn1UInt(byteValue), (total_bytes-i)*8);
      end loop;
   end Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N0;

   
   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N (bs : in out BitStream; IntVal : out Asn1UInt; minVal : in Asn1UInt; maxVal : in Asn1UInt; Result : out ASN1_RESULT; total_bytes : in Integer) with
     Pre     => total_bytes >= 0 and then 
                total_bytes <= Asn1UInt'Size/8 and then 
                bs.Current_Bit_Pos < Natural'Last - total_bytes*8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - total_bytes*8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes*8 and 
             ( (Result.Success and IntVal >= minVal and IntVal <= maxVal) or
               (not Result.Success and IntVal = minVal))
   is
   begin
      IntVal := 0;
      Result.ErrorCode := 0;
      Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N0(bs, IntVal, total_bytes);
      Result.Success := IntVal >= minVal and IntVal <= maxVal;
      if not Result.Success then
         IntVal           := minVal;
         Result.ErrorCode := ERR_INCORRECT_STREAM;
      end if;
   end Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N;
   

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16 (bs : in out BitStream; IntVal: out Asn1UInt; minVal:in Asn1UInt; maxVal : in Asn1UInt; Result : out ASN1_RESULT) 
   is
   begin
      Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(bs, IntVal, minVal, maxVal, Result, 2);
   end Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16;
     
   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32 (bs : in out BitStream; IntVal: out Asn1UInt; minVal:in Asn1UInt; maxVal : in Asn1UInt; Result : out ASN1_RESULT) 
   is
   begin
      Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(bs, IntVal, minVal, maxVal, Result, 4);
   end Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32;

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64 (bs : in out BitStream; IntVal: out Asn1UInt; minVal:in Asn1UInt; maxVal : in Asn1UInt; Result : out ASN1_RESULT) 
   is
   begin
      Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(bs, IntVal, minVal, maxVal, Result, 8);
   end Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64;
   
   
   
   procedure Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded (bs : in out BitStream; IntVal : out Asn1UInt; minVal : in Asn1UInt; Result : out ASN1_RESULT)
   is
      NBytes : Asn1Byte;
      Ret    : Asn1UInt;
   begin

      IntVal           := minVal;
      Result.ErrorCode := 0;
      BitStream_DecodeByte(bs, NBytes, Result.Success);
      
      if Result.Success and NBytes >= 1 and NBytes <= 8 then
         Dec_UInt (bs, Integer (NBytes), Ret, Result.Success);
         pragma Assert(Result.Success);
         IntVal         := Ret;
         Result.Success := IntVal >= minVal;
         if not Result.Success then
            IntVal           := minVal;
            Result.ErrorCode := ERR_INCORRECT_STREAM;
         end if;
      else
         Result.ErrorCode := ERR_INCORRECT_STREAM;
         Result.Success   := False;
      end if;
   end Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded;
   
   procedure Acn_Dec_Int_TwosComplement_ConstSize (bs : in out BitStream;  IntVal : out Asn1Int;  minVal : in Asn1Int; maxVal : in Asn1Int; nSizeInBits : in Integer; Result : out ASN1_RESULT)
   is
      encVal : Asn1UInt;
   begin
      Result.ErrorCode := 0;
      BitStream_Decode_Non_Negative_Integer (bs,  encVal, nSizeInBits,   Result.Success);
      if Result.Success then
         IntVal         := (if nSizeInBits = 0 then 0 else To_Int_n (encVal, nSizeInBits));
         Result.Success := IntVal >= minVal and IntVal <= maxVal;
         if not Result.Success then
            IntVal           := minVal;
            Result.ErrorCode := ERR_INCORRECT_STREAM;
         end if;
      else
         IntVal           := minVal;
         Result.ErrorCode := ERR_INCORRECT_STREAM;
      end if;
   end Acn_Dec_Int_TwosComplement_ConstSize;
   
   
   procedure Acn_Dec_Int_TwosComplement_ConstSize_8 (bs : in out BitStream; IntVal : out Asn1Int; minVal : in Asn1Int;  maxVal : in Asn1Int; Result :    out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_TwosComplement_ConstSize (bs, IntVal,  minVal,  maxVal,  8, Result);
   end Acn_Dec_Int_TwosComplement_ConstSize_8;

 procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16 (bs : in out BitStream; IntVal :out Asn1Int; minVal : in Asn1Int; maxVal : in Asn1Int; Result :out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_TwosComplement_ConstSize (bs, IntVal, minVal, maxVal, 16, Result);
   end Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16;

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32 (bs : in out BitStream; IntVal :out Asn1Int; minVal : in Asn1Int; maxVal : in Asn1Int; Result :out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_TwosComplement_ConstSize (bs, IntVal, minVal, maxVal, 32, Result);
   end Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32;

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64 (bs : in out BitStream; IntVal :out Asn1Int; minVal : in Asn1Int; maxVal : in Asn1Int; Result :out ASN1_RESULT)
   is
      encVal : Asn1UInt;
   begin
      Result.ErrorCode := 0;
      Dec_UInt(bs, 8, encVal, Result.Success);
      pragma Assert(Result.Success);
      IntVal         := To_Int (encVal);
      Result.Success := IntVal >= minVal and IntVal <= maxVal;
      if not Result.Success then
         IntVal           := minVal;
         Result.ErrorCode := ERR_INCORRECT_STREAM;
      end if;
   end Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64;   
   
   
   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_N (bs : in out BitStream; IntVal : out Asn1Int; minVal : in Asn1Int; maxVal : in Asn1Int; Result : out ASN1_RESULT; total_bytes : in Integer) with
     Pre     => total_bytes > 0 and then 
                total_bytes <= Asn1Int'Size/8 and then 
                bs.Current_Bit_Pos < Natural'Last - total_bytes*8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - total_bytes*8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes*8 and 
             ( (Result.Success and IntVal >= minVal and IntVal <= maxVal) or
               (not Result.Success and IntVal = minVal))
   is
      encValue :Asn1UInt;
        
   begin
      Result.ErrorCode := 0;
      Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N0(bs, encValue, total_bytes);
      IntVal := (if total_bytes=8 then To_Int(encValue) else To_Int_n(encValue, total_bytes*8));
      Result.Success := IntVal >= minVal and IntVal <= maxVal;
      if not Result.Success then
         IntVal           := minVal;
         Result.ErrorCode := ERR_INCORRECT_STREAM;
      end if;
   end Acn_Dec_Int_TwosComplement_ConstSize_little_endian_N;
   

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16 (bs : in out BitStream; IntVal: out Asn1Int; minVal:in Asn1Int; maxVal : in Asn1Int; Result : out ASN1_RESULT) 
   is
   begin
      Acn_Dec_Int_TwosComplement_ConstSize_little_endian_N(bs, IntVal, minVal, maxVal, Result, 2);
   end Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16;
     
   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32 (bs : in out BitStream; IntVal: out Asn1Int; minVal:in Asn1Int; maxVal : in Asn1Int; Result : out ASN1_RESULT) 
   is
   begin
      Acn_Dec_Int_TwosComplement_ConstSize_little_endian_N(bs, IntVal, minVal, maxVal, Result, 4);
   end Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32;

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64 (bs : in out BitStream; IntVal: out Asn1Int; minVal:in Asn1Int; maxVal : in Asn1Int; Result : out ASN1_RESULT) 
   is
   begin
      Acn_Dec_Int_TwosComplement_ConstSize_little_endian_N(bs, IntVal, minVal, maxVal, Result, 8);
   end Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64;
   
   procedure Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded  (bs : in out BitStream; IntVal :out Asn1Int; Result : out ASN1_RESULT)
   is
   begin
      Result.ErrorCode := ERR_INCORRECT_STREAM;
      UPER_Dec_UnConstraintWholeNumber (bs, IntVal, Result.Success);
   end Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded;
   
   
   procedure Acn_Dec_Int_BCD_ConstSize (bs : in out BitStream; IntVal: out Asn1UInt; minVal : in Asn1UInt; maxVal : in Asn1UInt; nNibbles : in Integer;  Result : out ASN1_RESULT)
   is
      digit   : Asn1Byte;
   begin
      IntVal := 0;
      Result :=   ASN1_RESULT'(Success => True, ErrorCode => 0);
      
      for i in 1..nNibbles loop
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*4);
         BitStream_ReadNibble (bs, digit, Result.Success);
         pragma Assert (Result.Success);
         Result.Success :=  digit <= 9;
         if Result.Success then
            IntVal := IntVal * 10;
            IntVal := IntVal + Asn1UInt (digit);
         end if;
         exit when not Result.Success;
      end loop;
      
      Result.Success :=    Result.Success and then ((IntVal >= minVal) and (IntVal <= maxVal));
      if not Result.Success then
         result.ErrorCode := ERR_INCORRECT_STREAM;
         IntVal := minVal;
      end if;
   end Acn_Dec_Int_BCD_ConstSize;
   

   procedure Acn_Dec_Int_BCD_VarSize_NullTerminated (bs : in out BitStream; IntVal: out Asn1UInt; minVal : in Asn1UInt; maxVal : in Asn1UInt; Result : out ASN1_RESULT)
   is
      digit   : Asn1Byte;
   begin
      IntVal := 0;
      Result :=   ASN1_RESULT'(Success => True, ErrorCode => 0);
      
      for i in 1..19 loop
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*4);
         BitStream_ReadNibble (bs, digit, Result.Success);
         pragma Assert (Result.Success);
         Result.Success :=  digit <= 9 or digit = 16#F#;
         if Result.Success and digit /= 16#F# then
            IntVal := IntVal * 10;
            IntVal := IntVal + Asn1UInt (digit);
         end if;
         exit when digit = 16#F# or not Result.Success;
      end loop;
      
      if Result.Success and (not (digit = 16#F#)) then
         BitStream_ReadNibble (bs, digit, Result.Success);
         Result.Success :=    digit = 16#F# and then ((IntVal >= minVal) and (IntVal <= maxVal));
      else
         Result.Success :=    Result.Success and then ((IntVal >= minVal) and (IntVal <= maxVal));
      end if;
      
      
      if not Result.Success then
         result.ErrorCode := ERR_INCORRECT_STREAM;
         IntVal := minVal;
      end if;
   end Acn_Dec_Int_BCD_VarSize_NullTerminated;
   
   
   procedure Acn_Enc_Int_ASCII_ConstSize (bs : in out BitStream; IntVal : in     Asn1Int; nChars : in     Integer)
   is
        digits_array: Digits_Buffer;
        nDigits : Asn1Byte;
        sing : Asn1Byte;
        absIntVal : Asn1Uint;
   begin
        pragma assert (intVal > -Asn1Int(Powers_of_10(nChars)));
        pragma assert (intVal <  Asn1Int(Powers_of_10(nChars)));
        pragma assert (abs IntVal <  Asn1Int(Powers_of_10(nChars)));
                                      
        absIntVal :=  (if intVal >= 0  then Asn1Uint(intVal) else Asn1Uint(-intVal));
        pragma assert (absIntVal < Powers_of_10(nChars));               
        sing := (if intVal >= 0  then Character'Pos('+') else Character'Pos('-'));
        Get_integer_digits(absIntVal,digits_array, nDigits);
        pragma assert(absIntVal < Powers_of_10(nChars));
        pragma assert(Integer(nDigits) = Get_number_of_digits(absIntVal));
        pragma assert(Integer(nDigits) <= nChars);
        -- encode sign
        BitStream_AppendByte(bs, sing, False);

        -- encode trailing zeros
        for i in 1.. (nChars - Integer(nDigits)) loop
             pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
             BitStream_AppendByte(bs, 0, False);
        end loop;
      
        -- encode digits
        for i in 1..Integer(nDigits) loop
             pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
             BitStream_AppendByte(bs, digits_array(i), False);
        end loop;
   end Acn_Enc_Int_ASCII_ConstSize;
   

   procedure Acn_Dec_Int_ASCII_ConstSize (bs : in out BitStream; IntVal: out Asn1Int; minVal : in Asn1Int; maxVal : in Asn1Int; nChars : in Integer;  Result : out ASN1_RESULT)
   is
      digit   : Asn1Byte;
      intDigit : Integer;
      Ch    : Character;
      negative : Boolean;
   begin
      IntVal := 0;
      BitStream_DecodeByte (bs, digit, Result.Success);

      Result :=   ASN1_RESULT'(Success => digit = Character'Pos ('+') or digit = Character'Pos ('-'), ErrorCode => ERR_INCORRECT_STREAM);
      if result.Success then
      
         negative := digit = Character'Pos ('-');
         for i in 1..nChars loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8 and IntVal >= 0 );
            BitStream_DecodeByte (bs, digit, Result.Success);
            pragma Assert (Result.Success);
            Ch    := Character'Val (digit);
            intDigit := Character'Pos (Ch) - Character'Pos ('0');
   
            Result.Success := intDigit >=0 and intDigit <= 9 and IntVal <Asn1Int(Powers_of_10(i-1));
            if Result.Success then
               IntVal := IntVal * 10;
               IntVal := IntVal + Asn1Int (intDigit);
            end if;
            exit when not Result.Success;
         end loop;
         
         IntVal := (if negative then -IntVal else IntVal);
         
         Result.Success :=    Result.Success and then ((IntVal >= minVal) and (IntVal <= maxVal));
         
      end if;
      
      if not Result.Success then
         result.ErrorCode := ERR_INCORRECT_STREAM;
         IntVal := minVal;
      end if;
      
      
   end Acn_Dec_Int_ASCII_ConstSize;
   
   
   procedure Acn_Enc_UInt_ASCII_ConstSize (bs : in out BitStream; IntVal : in     Asn1Uint; nChars : in     Integer)
   is
        digits_array: Digits_Buffer;
        nDigits : Asn1Byte;
   begin
        Get_integer_digits(IntVal,digits_array, nDigits);
        pragma assert(IntVal < Powers_of_10(nChars));
        pragma assert(Integer(nDigits) = Get_number_of_digits(IntVal));
        pragma assert(Integer(nDigits) <= nChars);

        -- encode trailing zeros
        for i in 1.. (nChars - Integer(nDigits)) loop
             pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
             BitStream_AppendByte(bs, 0, False);
        end loop;
      
        -- encode digits
        for i in 1..Integer(nDigits) loop
             pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
             BitStream_AppendByte(bs, digits_array(i), False);
        end loop;
   end Acn_Enc_UInt_ASCII_ConstSize;
   
   
   procedure Acn_Dec_UInt_ASCII_ConstSize (bs : in out BitStream; IntVal: out Asn1UInt; minVal : in Asn1UInt; maxVal : in Asn1UInt; nChars : in Integer;  Result : out ASN1_RESULT)
   is
      digit   : Asn1Byte;
      intDigit : Integer;
      Ch    : Character;
   begin
      IntVal := 0;
      Result :=   ASN1_RESULT'(Success => True, ErrorCode => 0);

      for i in 1..nChars loop
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8 and IntVal >= 0 );
         BitStream_DecodeByte (bs, digit, Result.Success);
         pragma Assert (Result.Success);
         Ch    := Character'Val (digit);
         intDigit := Character'Pos (Ch) - Character'Pos ('0');

         Result.Success := intDigit >=0 and intDigit <= 9 and IntVal <Powers_of_10(i-1);
         if Result.Success then
            IntVal := IntVal * 10;
            IntVal := IntVal + Asn1UInt (intDigit);
         end if;
         exit when not Result.Success;
      end loop;
      
         
      Result.Success :=    Result.Success and then ((IntVal >= minVal) and (IntVal <= maxVal));
         
      
      if not Result.Success then
         result.ErrorCode := ERR_INCORRECT_STREAM;
         IntVal := minVal;
      end if;
      
      
   end Acn_Dec_UInt_ASCII_ConstSize;
   

   procedure Acn_Dec_UInt_ASCII_VarSize_NullTerminated (bs : in out BitStream; IntVal: out Asn1UInt; nullChar: in Asn1Byte; Result : out ASN1_RESULT)
   is
      digit   : Asn1Byte;
      intDigit : Integer;
      Ch    : Character;
   begin
      IntVal := 0;
      Result :=   ASN1_RESULT'(Success => True, ErrorCode => 0);
      
      for i in 1..19 loop
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
         BitStream_DecodeByte (bs, digit, Result.Success);
         pragma Assert (Result.Success);
         exit when digit = nullChar;
         Ch    := Character'Val (digit);
         intDigit := Character'Pos (Ch) - Character'Pos ('0');

         Result.Success := intDigit >=0 and intDigit <= 9 and IntVal <Powers_of_10(i-1);
         if Result.Success then
            IntVal := IntVal * 10;
            IntVal := IntVal + Asn1UInt (intDigit);
         end if;
         exit when not Result.Success;
      end loop;
      
      if Result.Success and (not (digit = nullChar)) then
         BitStream_DecodeByte (bs, digit, Result.Success);
         Result.Success :=    digit = nullChar;
      end if;
      
      
      if not Result.Success then
         result.ErrorCode := ERR_INCORRECT_STREAM;
      end if;
   end Acn_Dec_UInt_ASCII_VarSize_NullTerminated;

   procedure Acn_Dec_Int_ASCII_VarSize_NullTerminated (bs : in out BitStream; IntVal: out Asn1Int; nullChar: in Asn1Byte; Result : out ASN1_RESULT)
   is
      digitAscii : Asn1Byte;
      Ch    : Character;
      absIntVal : Asn1UInt;
   begin
      IntVal := 0;
      Result := ASN1_RESULT'(Success => True, ErrorCode => ERR_INCORRECT_STREAM);
      --decode sign
      BitStream_DecodeByte (bs, digitAscii, Result.Success);
      
      Ch    := Character'Val (digitAscii);

      Result.Success := Result.Success and (Ch = '+' or Ch = '-');
      if result.Success then
         Acn_Dec_UInt_ASCII_VarSize_NullTerminated(bs, absIntVal, nullChar, result);

         if result.Success then

             if Ch= '+' and absIntVal <= Asn1UInt(Asn1Int'Last) then
                 IntVal := Asn1Int(absIntVal);
             elsif Ch = '-' and absIntVal <= Asn1UInt(Asn1Int'Last)+1 then
                IntVal := -Asn1Int(absIntVal-1)-1;
             else 
               Result := ASN1_RESULT'(Success => False, ErrorCode => ERR_INCORRECT_STREAM);
             end if;
         end if;
      end if;
      
      
   end Acn_Dec_Int_ASCII_VarSize_NullTerminated;
   
   
   
   function Float_to_OctetArray4 (x : Float) return OctetArray4 is
      function do_it is new Ada.Unchecked_Conversion (Float, OctetArray4);
   begin
      return do_it (x);
   end Float_to_OctetArray4;
   
   function OctetArray4_to_Float (x : OctetArray4) return Float is
      function do_it is new Ada.Unchecked_Conversion (OctetArray4, Float);
   begin
      return do_it (x);
   end OctetArray4_to_Float;
   
   
   function Long_Float_to_OctetArray8 (x : Asn1Real) return OctetArray8 is
      function do_it is new Ada.Unchecked_Conversion (Asn1Real, OctetArray8);
   begin
      return do_it (x);
   end Long_Float_to_OctetArray8;

   function OctetArray8_to_Long_Float (x : OctetArray8) return Asn1Real is
      function do_it is new Ada.Unchecked_Conversion (OctetArray8, Asn1Real);
   begin
      return do_it (x);
   end OctetArray8_to_Long_Float;

   
   procedure Acn_Enc_Real_IEEE754_32_big_endian (bs : in out BitStream;   RealVal : in     Asn1Real)
   is
      tmp : OctetArray4;
   begin
      tmp := Float_to_OctetArray4 (Long_Float_to_Float (RealVal));
      if RequiresReverse  then
         for i in reverse 1..4 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (4-i)*8);
            BitStream_AppendByte (bs, tmp (i), False);
         end loop;
      else
         for i in 1..4 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
            BitStream_AppendByte (bs, tmp (i), False);
         end loop;
      end if;
   end Acn_Enc_Real_IEEE754_32_big_endian;
   
   procedure Acn_Dec_Real_IEEE754_32_big_endian (bs : in out BitStream; RealVal : out Asn1Real;  Result  : out ASN1_RESULT)
   is
      tmp : OctetArray4 := OctetArray4'(others => 0);
   begin
      Result :=   ASN1_RESULT'(Success => True, ErrorCode => 0);
      RealVal := 0.0;
      if RequiresReverse  then
         for i in reverse 1..4 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (4-i)*8);
            BitStream_DecodeByte (bs, tmp (I), Result.Success);
            pragma assert(Result.Success);
         end loop;
      else
         for i in 1..4 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
            BitStream_DecodeByte (bs, tmp (I), Result.Success);
            pragma assert(Result.Success);
         end loop;
      end if;
      RealVal := Asn1Real (OctetArray4_to_Float (tmp));
   end Acn_Dec_Real_IEEE754_32_big_endian;
   
   procedure Acn_Enc_Real_IEEE754_64_big_endian (bs : in out BitStream;  RealVal : in     Asn1Real)
   is
      tmp : OctetArray8;
   begin
      tmp := Long_Float_to_OctetArray8 (RealVal);
      if RequiresReverse  then
         for I in reverse 1..8 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (8-i)*8);
            BitStream_AppendByte (bs, tmp (I), False);
         end loop;
      else
         for I in 1..8 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
            BitStream_AppendByte (bs, tmp (I), False);
         end loop;
      end if;
   end Acn_Enc_Real_IEEE754_64_big_endian;

   procedure Acn_Dec_Real_IEEE754_64_big_endian (bs : in out BitStream; RealVal : out Asn1Real; Result  :    out ASN1_RESULT)
   is
      tmp : OctetArray8 := OctetArray8'(others => 0);
   begin
      Result := ASN1_RESULT'(Success => True, ErrorCode => 0);
      RealVal := 0.0;
      if RequiresReverse  then
         for I in reverse 1..8 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (8-i)*8);
            BitStream_DecodeByte (bs, tmp (I), Result.Success);
         end loop;
      else
         for I in 1..8 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
            BitStream_DecodeByte (bs, tmp (I), Result.Success);
         end loop;
      end if;
      pragma assert(Result.Success);
      RealVal := OctetArray8_to_Long_Float (tmp);
   end Acn_Dec_Real_IEEE754_64_big_endian;
   
   
   procedure Acn_Enc_Real_IEEE754_32_little_endian (bs : in out BitStream;   RealVal : in     Asn1Real)
   is
      tmp : OctetArray4;
   begin
      tmp := Float_to_OctetArray4 (Long_Float_to_Float (RealVal));
      if not RequiresReverse  then
         for i in reverse 1..4 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (4-i)*8);
            BitStream_AppendByte (bs, tmp (i), False);
         end loop;
      else
         for i in 1..4 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
            BitStream_AppendByte (bs, tmp (i), False);
         end loop;
      end if;
   end Acn_Enc_Real_IEEE754_32_little_endian;
   
   procedure Acn_Dec_Real_IEEE754_32_little_endian (bs : in out BitStream; RealVal : out Asn1Real;  Result  : out ASN1_RESULT)
   is
      tmp : OctetArray4 := OctetArray4'(others => 0);
   begin
      Result :=   ASN1_RESULT'(Success => True, ErrorCode => 0);
      RealVal := 0.0;
      if not RequiresReverse  then
         for i in reverse 1..4 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (4-i)*8);
            BitStream_DecodeByte (bs, tmp (I), Result.Success);
            pragma assert(Result.Success);
         end loop;
      else
         for i in 1..4 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
            BitStream_DecodeByte (bs, tmp (I), Result.Success);
            pragma assert(Result.Success);
         end loop;
      end if;
      RealVal := Asn1Real (OctetArray4_to_Float (tmp));
   end Acn_Dec_Real_IEEE754_32_little_endian;
   
   procedure Acn_Enc_Real_IEEE754_64_little_endian (bs : in out BitStream;  RealVal : in     Asn1Real)
   is
      tmp : OctetArray8;
   begin
      tmp := Long_Float_to_OctetArray8 (RealVal);
      if not RequiresReverse  then
         for I in reverse 1..8 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (8-i)*8);
            BitStream_AppendByte (bs, tmp (I), False);
         end loop;
      else
         for I in 1..8 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
            BitStream_AppendByte (bs, tmp (I), False);
         end loop;
      end if;
   end Acn_Enc_Real_IEEE754_64_little_endian;

   procedure Acn_Dec_Real_IEEE754_64_little_endian (bs : in out BitStream; RealVal : out Asn1Real; Result  :    out ASN1_RESULT)
   is
      tmp : OctetArray8 := OctetArray8'(others => 0);
   begin
      Result := ASN1_RESULT'(Success => True, ErrorCode => 0);
      RealVal := 0.0;
      if not RequiresReverse  then
         for I in reverse 1..8 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (8-i)*8);
            BitStream_DecodeByte (bs, tmp (I), Result.Success);
         end loop;
      else
         for I in 1..8 loop
            pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
            BitStream_DecodeByte (bs, tmp (I), Result.Success);
         end loop;
      end if;
      pragma assert(Result.Success);
      RealVal := OctetArray8_to_Long_Float (tmp);
   end Acn_Dec_Real_IEEE754_64_little_endian;   
   
   
   
   procedure Acn_Enc_Boolean_true_pattern (bs : in out BitStream; BoolVal : in Asn1Boolean; pattern : in     BitArray)
   is
   begin
      for I in Integer range pattern'Range loop
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-pattern'First));
         BitStream_AppendBit(bs, ( if BoolVal then pattern (I) else not pattern (I)));
      end loop;
   end Acn_Enc_Boolean_true_pattern;

   procedure Acn_Dec_Boolean_true_pattern (bs : in out BitStream;  BoolVal :    out Asn1Boolean;  pattern : in     BitArray; Result  :    out ASN1_RESULT)
   is
      bit_val : BIT;
   begin
      BoolVal := True;
      for I in Integer range pattern'Range loop
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-pattern'First));
         BitStream_ReadBit(bs, bit_val, Result.Success);
         BoolVal := BoolVal and bit_val = pattern (I);
      end loop;
      Result :=  ASN1_RESULT'(Success => true, ErrorCode => 0);
   end Acn_Dec_Boolean_true_pattern;

   
   procedure Acn_Enc_Boolean_false_pattern (bs : in out BitStream; BoolVal : in Asn1Boolean; pattern : in     BitArray)
   is
   begin
      for I in Integer range pattern'Range loop
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-pattern'First));
         BitStream_AppendBit(bs, ( if not BoolVal then pattern (I) else not pattern (I)));
      end loop;
   end Acn_Enc_Boolean_false_pattern;

  
  
   procedure Acn_Dec_Boolean_false_pattern (bs : in out BitStream; BoolVal :    out Asn1Boolean;  pattern : in     BitArray;  Result  :    out ASN1_RESULT)
   is
   begin
      Acn_Dec_Boolean_true_pattern (bs, BoolVal, pattern, Result);
      BoolVal := not BoolVal;
   end Acn_Dec_Boolean_false_pattern;

   
   procedure Acn_Enc_NullType_pattern (bs : in out BitStream; encVal  : in     Asn1NullType;  pattern : in     BitArray)
   is
      pragma Unreferenced (encVal);
   begin
      Acn_Enc_Boolean_true_pattern(bs, true, pattern);
   end Acn_Enc_NullType_pattern;

   procedure Acn_Dec_NullType_pattern (bs : in out BitStream; decValue :    out Asn1NullType; pattern  : in     BitArray;  Result   :    out ASN1_RESULT)
   is
      BoolVal : Boolean := True;
   begin
      Acn_Dec_Boolean_true_pattern(bs, BoolVal, pattern, Result);
      Result.Success := BoolVal;
      decValue := (if BoolVal then 0 else 1);
   end Acn_Dec_NullType_pattern;
   

   procedure Acn_Enc_NullType_pattern2 (bs : in out BitStream;   pattern : in     BitArray)
   is
   begin
      Acn_Enc_Boolean_true_pattern(bs, true, pattern);
   end Acn_Enc_NullType_pattern2;

   procedure Acn_Dec_NullType_pattern2 (bs : in out BitStream;  pattern : in     BitArray;   Result  :    out ASN1_RESULT)
   is
      BoolVal : Boolean := True;
   begin
      Acn_Dec_Boolean_true_pattern(bs, BoolVal, pattern, Result);
      Result.Success := BoolVal;
   end Acn_Dec_NullType_pattern2;
   
  
   procedure Acn_Enc_NullType(bs : in out BitStream;  encVal : in     Asn1NullType)
   is
      pragma Unreferenced (bs);
      pragma Unreferenced (encVal);
   begin
      null;
   end Acn_Enc_NullType;

   procedure Acn_Dec_NullType (bs : in out BitStream; decValue :    out Asn1NullType;   Result   :    out ASN1_RESULT)
   is
      pragma Unreferenced (bs);
   begin
      decValue := 0;
      Result   := ASN1_RESULT'(Success => True, ErrorCode => 0);
   end Acn_Dec_NullType;
   

   
   
   procedure Acn_Enc_String_Ascii_FixSize (bs : in out BitStream; strVal : in String)
   is
   begin
      for i in strVal'Range loop
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-strVal'First)*8);
         BitStream_AppendByte (bs, Asn1Byte (CharacterPos (strVal (I))),  false);
      end loop;

   end Acn_Enc_String_Ascii_FixSize;

   procedure Acn_Dec_String_Ascii_FixSize (bs : in out BitStream;  strVal : in out String; Result :    out ASN1_RESULT)
   is
      charIndex : Asn1Byte;
   begin
      Result :=    ASN1_RESULT'(Success => True, ErrorCode => 0);
      for i in strVal'Range loop
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-strVal'First)*8);
         BitStream_DecodeByte (bs, charIndex, Result.Success);
         pragma assert(Result.Success);
         strVal (I) := Character'Val (Integer(charIndex));
      end loop;
      strVal (strVal'Last) := Standard.ASCII.NUL;
   end Acn_Dec_String_Ascii_FixSize;   
   
   
   
   procedure Acn_Enc_String_Ascii_Null_Teminated (bs : in out BitStream; null_character : in     Integer;  strVal : in String)
   is
      i : Integer := strVal'First;
   begin
      while I <= strVal'Last - 1 and then strVal (I) /= Standard.ASCII.NUL loop
         pragma Loop_Invariant (i>=strVal'First and i <=strVal'Last -1 and bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-strVal'First)*8);
         BitStream_AppendByte (bs, Asn1Byte (CharacterPos (strVal (I))),  false);
         i := i + 1;
      end loop;
      BitStream_AppendByte (bs, Asn1Byte (null_character), false);

   end Acn_Enc_String_Ascii_Null_Teminated;

   procedure Acn_Dec_String_Ascii_Null_Teminated (bs : in out BitStream; null_character : in Integer; strVal : in out String; Result : out ASN1_RESULT)
   is
      I         : Integer := strVal'First;
      charIndex : Asn1Byte := 65; -- ascii code of 'A'. Let's hope that 'A' will never be null Character
   begin
      Result :=  ASN1_RESULT'(Success => True, ErrorCode => 0);
      while  I <= strVal'Last and then charIndex /= Asn1Byte(null_character)   loop
         pragma Loop_Invariant (i >= strVal'First and i <= strVal'Last and bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-strVal'First)*8);
         BitStream_DecodeByte (bs, charIndex, Result.Success);
         if charIndex /= Asn1Byte(null_character) then
            strVal (I) := Character'Val (Integer(charIndex));
         else
            strVal (I) := Standard.ASCII.NUL;
         end if;

         i := i + 1;
      end loop;
      while i <= strVal'Last loop
         pragma Loop_Invariant (i >= strVal'First and i <= strVal'Last);
         strVal (i) := Standard.ASCII.NUL;
         i          := i + 1;
      end loop;

   end Acn_Dec_String_Ascii_Null_Teminated;
   

   procedure Acn_Enc_String_Ascii_Internal_Field_Determinant (bs : in out BitStream; asn1Min : Asn1Int;  nLengthDeterminantSizeInBits : in     Integer; strVal : in     String)
   is
      I : Integer := strVal'First;
   begin
      UPER_Enc_ConstraintWholeNumber (bs, Asn1Int (getStringSize (strVal)), asn1Min,  nLengthDeterminantSizeInBits);
      while I <= strVal'Last  and then strVal (I) /= Standard.ASCII.NUL loop
         pragma Loop_Invariant (I >= strVal'First and I <= strVal'Last and bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-strVal'First)*8);
          BitStream_AppendByte (bs, Asn1Byte (CharacterPos (strVal (I))),  false);

         I := I + 1;
      end loop;

   end Acn_Enc_String_Ascii_Internal_Field_Determinant;

   procedure Acn_Dec_String_Ascii_Internal_Field_Determinant (bs : in out BitStream; asn1Min : Asn1Int; asn1Max : Asn1Int; nLengthDeterminantSizeInBits : in Integer; strVal : in out String; Result : out ASN1_RESULT)
   is
      I         : Integer := strVal'First;
      nSize     : Integer;
      charIndex : Asn1Byte;
   begin
      Result := ASN1_RESULT'(Success => True, ErrorCode => ERR_INCORRECT_STREAM);

      UPER_Dec_ConstraintWholeNumberInt (bs, nSize, Integer (asn1Min), Integer (asn1Max), nLengthDeterminantSizeInBits, Result.Success);
      
      while Result.Success and then I <= strVal'Last  and then I <= nSize  loop
         pragma Loop_Invariant (i >= strVal'First and i <= strVal'Last and bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-strVal'First)*8);
         BitStream_DecodeByte (bs, charIndex, Result.Success);
         strVal (I) := Character'Val (charIndex);

         I := I + 1;
      end loop;
      while I <= strVal'Last loop
         pragma Loop_Invariant (i >= strVal'First and i <= strVal'Last);
         strVal (I) := Standard.ASCII.NUL;
         I          := I + 1;
      end loop;

   end Acn_Dec_String_Ascii_Internal_Field_Determinant;   

   
   
   procedure Acn_Enc_String_Ascii_External_Field_Determinant(bs : in out BitStream;  strVal : in     String)
   is
      I : Integer := strVal'First;
   begin
      while I <= strVal'Last  and then strVal (I) /= Standard.ASCII.NUL loop
         pragma Loop_Invariant (I >= strVal'First and I <= strVal'Last and bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-strVal'First)*8);
          BitStream_AppendByte (bs, Asn1Byte (CharacterPos (strVal (I))),  false);

         I := I + 1;
      end loop;

   end Acn_Enc_String_Ascii_External_Field_Determinant;

   procedure Acn_Dec_String_Ascii_External_Field_Determinant (bs : in out BitStream; extSizeDeterminatFld : in     Asn1Int; strVal               : in out String;  Result               :    out ASN1_RESULT)
   is
      I         : Integer := strVal'First;
      charIndex : Asn1Byte;
   begin
      Result :=    ASN1_RESULT'(Success => True, ErrorCode => ERR_INCORRECT_STREAM);

      while Result.Success and then I <= strVal'Last  and then I <= Integer(extSizeDeterminatFld)  loop
         pragma Loop_Invariant (i >= strVal'First and i <= strVal'Last and bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-strVal'First)*8);
         BitStream_DecodeByte (bs, charIndex, Result.Success);
         strVal (I) := Character'Val (Integer(charIndex));

         I := I + 1;
      end loop;
      while I <= strVal'Last loop
         pragma Loop_Invariant (i >= strVal'First and i <= strVal'Last);
         strVal (I) := Standard.ASCII.NUL;
         I          := I + 1;
      end loop;

   end Acn_Dec_String_Ascii_External_Field_Determinant;
   
   

   procedure Acn_Enc_String_CharIndex_External_Field_Determinant (bs : in out BitStream; charSet : String; nCharSize : Integer;  strVal    : in     String)
   is
      I         : Integer := strVal'First;
      charIndex : Integer;
   begin
      while I <= strVal'Last  and then strVal (I) /= Standard.ASCII.NUL loop
         pragma Loop_Invariant (I >= strVal'First and I <= strVal'Last and bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-strVal'First)*nCharSize);

         charIndex := GetZeroBasedCharIndex (strVal (I), charSet);
         UPER_Enc_ConstraintWholeNumber (bs, Asn1Int (charIndex), 0, nCharSize);

         I := I + 1;
      end loop;

   end Acn_Enc_String_CharIndex_External_Field_Determinant;

   procedure Acn_Dec_String_CharIndex_External_Field_Determinant (bs : in out BitStream; charSet : String; nCharSize :Integer; extSizeDeterminatFld : in Asn1Int; strVal : in out String; Result : out ASN1_RESULT)
   is
      I         : Integer          := strVal'First;
      charIndex : Integer;
      asn1Max   : constant Integer := charSet'Last - charSet'First;
   begin
      Result :=  ASN1_RESULT'(Success => True, ErrorCode => ERR_INCORRECT_STREAM);

      while Result.Success  and then I <= strVal'Last - 1   and then I <= Integer (extSizeDeterminatFld)   loop
         pragma Loop_Invariant (i >= strVal'First and i <= strVal'Last and bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-strVal'First)*nCharSize);

         UPER_Dec_ConstraintWholeNumberInt (bs, charIndex, 0, asn1Max, nCharSize, Result.Success);
         strVal (I) := charSet (charIndex + charSet'First);

         I := I + 1;
      end loop;

      while I <= strVal'Last loop
         pragma Loop_Invariant (i >= strVal'First and i <= strVal'Last);
         strVal (I) := Standard.ASCII.NUL;
         I          := I + 1;
      end loop;

   end Acn_Dec_String_CharIndex_External_Field_Determinant;
   
   
   
   procedure Acn_Enc_String_CharIndex_Internal_Field_Determinant (bs : in out BitStream; charSet : String; nCharSize : Integer; asn1Min : Asn1Int; nLengthDeterminantSizeInBits : in Integer; strVal: in String)
   is
      I         : Integer := strVal'First;
      charIndex : Integer;
   begin
      UPER_Enc_ConstraintWholeNumber (bs, Asn1Int (getStringSize (strVal)), asn1Min,  nLengthDeterminantSizeInBits);

      while I <= strVal'Last  and then strVal (I) /= Standard.ASCII.NUL loop
         pragma Loop_Invariant (I >= strVal'First and I <= strVal'Last and bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-strVal'First)*nCharSize);

         charIndex := GetZeroBasedCharIndex (strVal (I), charSet);
         UPER_Enc_ConstraintWholeNumber (bs, Asn1Int (charIndex), 0, nCharSize);

         I := I + 1;
      end loop;

   end Acn_Enc_String_CharIndex_Internal_Field_Determinant;

   procedure Acn_Dec_String_CharIndex_Internal_Field_Determinant (bs : in out BitStream;  charSet : String;  nCharSize : Integer;
                                                                  asn1Min : Asn1Int; asn1Max: Asn1Int; nLengthDeterminantSizeInBits : in Integer; strVal : in out String; Result : out ASN1_RESULT)
   is
      I         : Integer := strVal'First;
      nSize     : Integer;
      charIndex : Integer;
   begin
      Result := ASN1_RESULT'(Success => True, ErrorCode => ERR_INCORRECT_STREAM);

      UPER_Dec_ConstraintWholeNumberInt (bs, nSize, Integer (asn1Min), Integer (asn1Max), nLengthDeterminantSizeInBits, Result.Success);
      
      while Result.Success  and then I <= strVal'Last - 1   and then I <= nSize   loop
         pragma Loop_Invariant (i >= strVal'First and i <= strVal'Last and bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-strVal'First)*nCharSize);

         UPER_Dec_ConstraintWholeNumberInt (bs, charIndex, 0, Integer(asn1Max), nCharSize, Result.Success);
         if result.Success and charIndex + charSet'First <= charSet'Last then
            strVal (I) := charSet (charIndex + charSet'First);
         else
            result.Success := false;
            strVal (I) := Standard.ASCII.NUL;
         end if;
         
         I := I + 1;
      end loop;

      while I <= strVal'Last loop
         pragma Loop_Invariant (i >= strVal'First and i <= strVal'Last);
         strVal (I) := Standard.ASCII.NUL;
         I          := I + 1;
      end loop;

   end Acn_Dec_String_CharIndex_Internal_Field_Determinant;
   
   
end acn_asn1_rtl;




