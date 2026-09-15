with Ada.Text_IO;
with adaasn1rtl;
with TEST_CASE;

procedure Null_Codecs is
   use TEST_CASE;
   use type adaasn1rtl.Asn1NullType;
   use type adaasn1rtl.OctetBuffer;

   Result : adaasn1rtl.ASN1_RESULT;
   Value : ASN1SCC_MyPDU;
   Uper_Stream : ASN1SCC_MyPDU_uPER_Stream;
   Acn_Stream : ASN1SCC_MyPDU_ACN_Stream;

   procedure Check (Condition : Boolean; Message : String);
   procedure Check (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         raise Program_Error with Message;
      end if;
   end Check;

   procedure Reset_Outputs;
   procedure Reset_Outputs is
   begin
      Value := 255;
      Result := (Success => False, ErrorCode => 123);
   end Reset_Outputs;

   procedure Check_Outputs;
   procedure Check_Outputs is
   begin
      Check (Result.Success and then Result.ErrorCode = 0,
             "NULL decode must return success independently of prior outputs");
      Check (Value = ASN1SCC_MyPDU_Init, "NULL decode must initialize value");
   end Check_Outputs;
begin
   ASN1SCC_MyPDU_Encode (ASN1SCC_MyPDU_Init, Uper_Stream, Result);
   Check (Result.Success and then Uper_Stream.Current_Bit_Pos = 0,
          "uPER NULL encode must consume zero bits");
   Reset_Outputs;
   ASN1SCC_MyPDU_Decode (Value, Uper_Stream, Result);
   Check_Outputs;
   Check (Uper_Stream.Current_Bit_Pos = 0, "uPER NULL round trip cursor");

   ASN1SCC_MyPDU_ACN_Encode (ASN1SCC_MyPDU_Init, Acn_Stream, Result);
   Check (Result.Success and then Acn_Stream.Current_Bit_Pos = 0,
          "ACN NULL encode must consume zero bits");
   Reset_Outputs;
   ASN1SCC_MyPDU_ACN_Decode (Value, Acn_Stream, Result);
   Check_Outputs;
   Check (Acn_Stream.Current_Bit_Pos = 0, "ACN NULL round trip cursor");

   --  Aux codecs must also work at an existing, non-byte-aligned cursor.
   Uper_Stream.Current_Bit_Pos := 5;
   Uper_Stream.Buffer := (others => 165);
   ASN1SCC_MyPDU_Encode_aux (ASN1SCC_MyPDU_Init, Uper_Stream, Result);
   Check (Result.Success, "uPER NULL aux encode");
   Reset_Outputs;
   ASN1SCC_MyPDU_Decode_aux (Value, Uper_Stream, Result);
   Check_Outputs;
   Check (Uper_Stream.Current_Bit_Pos = 5 and then
          Uper_Stream.Buffer = adaasn1rtl.OctetBuffer'(1 => 165),
          "uPER NULL aux codecs must preserve stream");

   Acn_Stream.Current_Bit_Pos := 5;
   Acn_Stream.Buffer := (others => 165);
   ASN1SCC_MyPDU_ACN_Encode_aux (ASN1SCC_MyPDU_Init, Acn_Stream, Result);
   Check (Result.Success, "ACN NULL aux encode");
   Reset_Outputs;
   ASN1SCC_MyPDU_ACN_Decode_aux (Value, Acn_Stream, Result);
   Check_Outputs;
   Check (Acn_Stream.Current_Bit_Pos = 5 and then
          Acn_Stream.Buffer = adaasn1rtl.OctetBuffer'(1 => 165),
          "ACN NULL aux codecs must preserve stream");

   Ada.Text_IO.Put_Line
     ("NULL public/aux codecs: outputs and zero-bit cursors OK");
end Null_Codecs;
