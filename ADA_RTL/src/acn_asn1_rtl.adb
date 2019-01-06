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
        absIntVal :=  Asn1Uint(abs IntVal); --(if intVal >= 0  then Asn1Uint(intVal) else (Asn1Uint(-(intVal+1))+1));
        sing := (if intVal >= 0  then Character'Pos('+') else Character'Pos('-'));
        Get_integer_digits(absIntVal,digits_array, nDigits);
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
   

end acn_asn1_rtl;
