 
package body uper_asn1_rtl with Spark_Mode  is

   -- UPER FUNCTIONS
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
   

end uper_asn1_rtl;
