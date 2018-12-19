 
package body uper_asn1_rtl with Spark_Mode  is

   -- UPER FUNCTIONS
   procedure UPER_Enc_Boolean (bs : in out BitStream; Val : in  Asn1Boolean)
   is
   begin
      BitStream_AppendBit (bs, (if val then 1 else 0));
   end UPER_Enc_Boolean;

   procedure UPER_Dec_boolean (bs : in out BitStream; val: out Asn1Boolean; result : out Boolean)
   is
      v : BIT;
   begin
      BitStream_ReadBit (bs, v, result);
      val := v = 1;
   end UPER_Dec_boolean;
   
   
   
   procedure UPER_Enc_SemiConstraintWholeNumber(bs : in out BitStream; IntVal : in Asn1Int; MinVal : in     Asn1Int)
   is
      nBytes             : Asn1Byte;
      ActualEncodedValue : Asn1UInt;
   begin
      ActualEncodedValue := Sub (IntVal, MinVal);

      nBytes := GetBytes (ActualEncodedValue);

      -- encode length
      BitStream_AppendByte (bs, nBytes, False);
      --Encode number
      Enc_UInt (bs, ActualEncodedValue, Integer(nBytes));
   end UPER_Enc_SemiConstraintWholeNumber;   
   
   
   
   procedure UPER_Dec_SemiConstraintWholeNumber (bs : in out BitStream; IntVal : out Asn1Int; MinVal : in Asn1Int; Result :    out Boolean)
   is
      NBytes : Asn1Byte;
      Ret : Asn1UInt;
   begin

      IntVal := MinVal;
      BitStream_DecodeByte(bs, NBytes, Result) ;
      if Result and NBytes >= 1 and NBytes <= 8 then
         Dec_UInt (bs, Integer (NBytes), Ret, Result);
         IntVal := To_Int (Ret + To_UInt (MinVal));
         Result := IntVal >= MinVal;
         if not Result then
            IntVal := MinVal;
         end if;
      else
         Result := False;
      end if;

   end UPER_Dec_SemiConstraintWholeNumber;
   
   
   procedure UPER_Enc_SemiConstraintPosWholeNumber (bs : in out BitStream; IntVal : in Asn1UInt; MinVal : in     Asn1UInt)   
   is
      nBytes             : Asn1Byte;
      ActualEncodedValue : Asn1UInt;
   begin
      ActualEncodedValue := IntVal - MinVal;

      nBytes := GetBytes (ActualEncodedValue);

      -- encode length
      BitStream_AppendByte (bs, nBytes, False);
      --Encode number
      Enc_UInt (bs, ActualEncodedValue, Integer(nBytes));
   end UPER_Enc_SemiConstraintPosWholeNumber;

   procedure UPER_Dec_SemiConstraintPosWholeNumber (bs : in out BitStream; IntVal : out Asn1UInt; MinVal : in     Asn1UInt; Result :    out Boolean) 
   is
      NBytes : Asn1Byte;
      Ret : Asn1UInt:=0;
   begin

      IntVal := MinVal;
      pragma Assert(IntVal >= MinVal);
      BitStream_DecodeByte(bs, NBytes, Result) ;
      Result := Result and NBytes >= 1 and NBytes <= 8;
      if Result  then
         Dec_UInt (bs, Integer (NBytes), Ret, Result);
         IntVal := Ret + MinVal;
         Result := IntVal >= MinVal;
         if not Result then
            IntVal := MinVal;
         end if;
         
      end if;
      pragma Assert(IntVal >= MinVal);

   end UPER_Dec_SemiConstraintPosWholeNumber;
   

   
   procedure UPER_Enc_ConstraintWholeNumber (bs : in out BitStream; IntVal : in Asn1Int; MinVal : in Asn1Int; nBits : in Integer)
   is
      encVal : Asn1UInt;
   begin
      encVal := Sub (IntVal, MinVal);
      BitStream_Encode_Non_Negative_Integer (bs, encVal, nBits);
   end UPER_Enc_ConstraintWholeNumber;

   procedure UPER_Enc_ConstraintPosWholeNumber (bs : in out BitStream; IntVal: in Asn1UInt; MinVal : in Asn1UInt; nBits : in Integer)
   is
      encVal : Asn1UInt;
   begin
      encVal := IntVal - MinVal;
      BitStream_Encode_Non_Negative_Integer (bs, encVal, nBits);
   end UPER_Enc_ConstraintPosWholeNumber;
   
   procedure UPER_Dec_ConstraintWholeNumber (bs : in out BitStream; IntVal : out Asn1Int; MinVal : in Asn1Int; MaxVal : in Asn1Int; nBits : in Integer; Result : out Boolean)
   is
      encVal : Asn1UInt;
   begin
      BitStream_Decode_Non_Negative_Integer (bs, encVal, nBits,  Result);
      if Result then
         IntVal := To_Int (encVal + To_UInt (MinVal));

         Result := IntVal >= MinVal and IntVal <= MaxVal;
         if not Result then
            IntVal := MinVal;
         end if;
      else
         IntVal := MinVal;
      end if;

   end UPER_Dec_ConstraintWholeNumber;   
   
   procedure UPER_Dec_ConstraintPosWholeNumber (bs : in out BitStream; IntVal : out Asn1UInt; MinVal : in Asn1UInt; MaxVal : in Asn1UInt; nBits : in Integer; Result : out Boolean)
   is
      encVal : Asn1UInt;
   begin
      BitStream_Decode_Non_Negative_Integer (bs, encVal, nBits,  Result);
      if Result then
         IntVal := encVal + MinVal;

         Result := IntVal >= MinVal and IntVal <= MaxVal;
         if not Result then
            IntVal := MinVal;
         end if;
      else
         IntVal := MinVal;
      end if;

   end UPER_Dec_ConstraintPosWholeNumber;   

   procedure UPER_Dec_ConstraintWholeNumberInt
     (bs : in out BitStream;
      IntVal      :    out Integer;
      MinVal      : in     Integer;
      MaxVal      : in     Integer;
      nBits : in     Integer;
      Result      :    out Boolean)
   is
      Ret : Asn1Int;
   begin
      UPER_Dec_ConstraintWholeNumber (bs, Ret,  Asn1Int (MinVal),  Asn1Int (MaxVal),   nBits,       Result);
      IntVal := Integer (Ret);
   end UPER_Dec_ConstraintWholeNumberInt;
   
   
   procedure UPER_Enc_UnConstraintWholeNumber (bs : in out BitStream; IntVal : in Asn1Int)
   is
      nBytes : Asn1Byte;
   begin

      nBytes := GetLengthInBytesOfSInt (IntVal);

      -- encode length
      BitStream_AppendByte (bs, nBytes, False);
      Enc_UInt (bs, To_UInt (IntVal), Integer(nBytes));
   end UPER_Enc_UnConstraintWholeNumber;
   
   procedure UPER_Dec_UnConstraintWholeNumber (bs : in out BitStream; IntVal : out Asn1Int; Result :    out Boolean)
   is
      NBytes : Asn1Byte;
   begin
      BitStream_DecodeByte(bs, NBytes, Result) ;
      Result := Result and NBytes >= 1 and NBytes <= 8;
      if Result then
         Dec_Int (bs, Integer (NBytes), IntVal, Result);
      else
         IntVal := 0;
         Result := False;
      end if;
   end UPER_Dec_UnConstraintWholeNumber;
   
   procedure UPER_Dec_UnConstraintWholeNumberMax (bs : in out BitStream; IntVal : out Asn1Int;  MaxVal : in Asn1Int; Result : out Boolean)
   is
   begin
      UPER_Dec_UnConstraintWholeNumber (bs, IntVal, Result);
      Result := Result and IntVal <= MaxVal;
      if not Result then
         IntVal := MaxVal;
      end if;
   end UPER_Dec_UnConstraintWholeNumberMax;
   

   procedure UPER_Enc_Real (bs : in out BitStream;  RealVal : in     Asn1Real)
   is
      Header   : Interfaces.Unsigned_8 := 16#80#;
      NExpLen  : Asn1Byte;
      NManLen  : Asn1Byte;
      Exp      : Asn1Int;
      Mantissa : Asn1UInt;
      V        : Asn1Real;
   begin

      if RealVal >= 0.0 and RealVal <= 0.0 then
         BitStream_AppendByte (bs, 0, False);
      elsif RealVal = PLUS_INFINITY then
         BitStream_AppendByte (bs, 1, False);
         BitStream_AppendByte (bs, 16#40#, False);
      elsif RealVal = MINUS_INFINITY then
         BitStream_AppendByte (bs, 1, False);
         BitStream_AppendByte (bs, 16#41#, False);
      else
         V := RealVal;

         if V < 0.0 then
            V      := -V;
            Header := Header or 16#40#;
         end if;

         Exp      := GetExponent (V);
         Mantissa := GetMantissa (V);

         NExpLen := GetLengthInBytesOfSInt (Exp);
         NManLen := GetBytes (Mantissa);

         if NExpLen >= 4 then
            NExpLen := 3;
         end if;

         if NExpLen = 2 then
            Header := Header or 1;
         elsif NExpLen = 3 then
            Header := Header or 2;
         end if;

         --#check NExpLen>=1 AND NExpLen<=3;

         -- encode length
         BitStream_AppendByte(bs, (1 + NExpLen) + NManLen, False); --1

         -- encode header
         BitStream_AppendByte (bs, Header, False); --1

         -- encode exponent
         Enc_UInt (bs, To_UInt (Exp), Integer(NExpLen)); --max 3 octets

         -- encode mantissa
         Enc_UInt (bs, Mantissa, Integer(NManLen)); --max 8 octets
      end if;
   end UPER_Enc_Real;
   

   function CalcReal (Factor : Asn1UInt; N : Asn1UInt; base : Integer;Exp : Integer) return Asn1Real 
   is
      pragma SPARK_Mode(Off);
   begin
     return (Asn1Real (Factor * N) * Asn1Real (base)**Exp);
   end;


   procedure UPER_Dec_Real_AsBinary_aux  
   (bs : in out BitStream;
      ExpLen  : in     Asn1Byte;
      Length  : in     Asn1Byte;
      Factor  : in     Asn1UInt;
      Sign    : in     Integer;
      Base    : in     Integer;
      RealVal :    out Asn1Real;
      Result  :    out ASN1_RESULT)
   with
     Depends => ((bs, RealVal, Result) => (bs, ExpLen, Length, Factor, Sign, Base)),
       Pre     => 
                (base=2 or base=8 or base=16) and then
                (Factor = 1 or Factor=2 or Factor=4 or Factor=8) and then
                ExpLen <= 4 and then
                Length >=0 and then Length <=11 and then
                (Sign = 1 or Sign = -1) and then
                bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 24) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 24),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 24)  
       
   is
      Exp : Asn1Int;
      N   : Asn1UInt;
   begin
      RealVal := 0.0;
      Result := ASN1_RESULT'(Success => False, ErrorCode => ERR_END_OF_STREAM);
      if ExpLen < Length and ExpLen <= 3 then
         Dec_Int (bs, Integer (ExpLen), Exp, Result.Success);

         if Result.Success and Length - ExpLen <= 8 then
            Dec_UInt (bs, Integer (Length - ExpLen), N, Result.Success);
            if Result.Success and Exp > Asn1Int (Integer'First) and Exp < Asn1Int (Integer'Last)    then
               RealVal := CalcReal (Factor, N, Base, Integer (Exp));

               if Sign < 0 then
                  RealVal := -RealVal;
               end if;

               Result := ASN1_RESULT'(Success => True, ErrorCode => 0);
            end if;
         end if;
      end if;
   end UPER_Dec_Real_AsBinary_aux;

   procedure UPER_Dec_Real_AsBinary
     (bs : in out BitStream;
      Header    : in     Asn1Byte;
      EncLength : in     Asn1Byte;
      RealVal   :    out Asn1Real;
      Result    :    out ASN1_RESULT)
   with
     Depends => ((bs, RealVal, Result) => (bs, Header, EncLength)),
       Pre     => 
                EncLength >=0 and then EncLength <=11 and then
                bs.Current_Bit_Pos < (Natural'Last - (Asn1UInt'Size + 24)) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 24),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 24)  

   is
      Sign   : Integer  := 1;
      Base   : Integer  := 2;
      F      : Asn1Byte;
      Factor : Asn1UInt := 1;
      ExpLen : Asn1Byte;
   begin

      if (Header and 16#40#) > 0 then
         Sign := -1;
      end if;

      if (Header and 16#10#) > 0 then
         Base := 8;
      elsif (Header and 16#20#) > 0 then
         Base := 16;
      end if;

      F      := (Header and 16#0C#) / 4;
      pragma Assert(F = 0 or F=1 or F=2 or F=3);
      
      Factor := Factor * (2**Integer (F));
      pragma Assert(Factor = 1 or Factor=2 or Factor=4 or Factor=8);
      

      ExpLen := (Header and 16#03#) + 1;
      pragma Assert(ExpLen <= 4);

      UPER_Dec_Real_AsBinary_aux(bs, ExpLen, EncLength, Factor, Sign, Base, RealVal, Result);

   end UPER_Dec_Real_AsBinary;

   procedure UPER_Dec_Real (bs : in out BitStream; RealVal : out Asn1Real; Result  : out ASN1_RESULT)
   is
      Header : Asn1Byte;
      Length : Asn1Byte;
   begin
      RealVal := 0.0;
      Result := ASN1_RESULT'(Success => False, ErrorCode => ERR_END_OF_STREAM);

      BitStream_DecodeByte (bs, Length, Result.Success);
      if Result.Success and Length <= 12 then
         if Length > 0 then
            BitStream_DecodeByte (bs, Header, Result.Success);
            if Result.Success then
               if Header = 16#40# then
                  RealVal := PLUS_INFINITY;
                  Result  := ASN1_RESULT'(Success => True, ErrorCode => 0);
               elsif Header = 16#41# then
                  RealVal := MINUS_INFINITY;
                  Result  := ASN1_RESULT'(Success => True, ErrorCode => 0);
               elsif (Header and 16#80#) > 0 then
                  UPER_Dec_Real_AsBinary (bs, Header, Length - 1, RealVal, Result);
               else
                  Result := ASN1_RESULT' (Success   => False, ErrorCode => ERR_UNSUPPORTED_ENCODING);
               end if;
            end if;
         else
            Result := ASN1_RESULT'(Success => True, ErrorCode => 0);
         end if;
      end if;
   end UPER_Dec_Real;
   
   
   
   
end uper_asn1_rtl;
