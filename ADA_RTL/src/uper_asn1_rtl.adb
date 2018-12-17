 
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
   

end uper_asn1_rtl;
