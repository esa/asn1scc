with Ada.Text_IO;
with adaasn1rtl;
with TEST_CASE;

procedure Containing_Bounds is
   use TEST_CASE;
   use type adaasn1rtl.OctetBuffer;
   use type adaasn1rtl.Asn1Byte;

   Result : adaasn1rtl.ASN1_RESULT;
   External_Stream : ASN1SCC_ExternalPdu_ACN_Stream;
   Embedded_Stream : ASN1SCC_EmbeddedPdu_ACN_Stream;
   Pair_Stream : ASN1SCC_PairPdu_ACN_Stream;
   External_Value : ASN1SCC_ExternalPdu;
   Embedded_Value : ASN1SCC_EmbeddedPdu;
   Pair_Value : ASN1SCC_PairPdu;

   procedure Check (Condition : Boolean; Message : String);
   procedure Check (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         raise Program_Error with Message;
      end if;
   end Check;
begin
   ASN1SCC_ExternalPdu_ACN_Encode (extOdd, External_Stream, Result);
   Check (Result.Success, "external encode");
   Check (External_Stream.Current_Bit_Pos = 48, "external padding/cursor");
   Check (External_Stream.Buffer (1 .. 6) =
          adaasn1rtl.OctetBuffer'(17, 0, 2, 2, 160, 165),
          "external wire bytes");
   External_Stream.pushDataPrm := 17;
   External_Stream.fetchDataPrm := 23;
   ASN1SCC_ExternalPdu_ACN_Decode (External_Value, External_Stream, Result);
   Check (Result.Success and then
          ASN1SCC_ExternalPdu_Equal (extOdd, External_Value),
          "external region includes following field");
   Check (External_Stream.pushDataPrm = 17 and
          External_Stream.fetchDataPrm = 23,
          "streaming context changed");

   --  A receiver must accept nonzero padding from a foreign encoder.
   External_Stream.Buffer (5) := External_Stream.Buffer (5) or 15;
   ASN1SCC_ExternalPdu_ACN_Decode (External_Value, External_Stream, Result);
   Check (Result.Success and then
          ASN1SCC_ExternalPdu_Equal (extOdd, External_Value),
          "nonzero padding rejected");

   --  Use the public, full-capacity stream: the length is the malformed input.
   External_Stream.Buffer (2) := 255;
   External_Stream.Buffer (3) := 255;
   ASN1SCC_ExternalPdu_ACN_Decode (External_Value, External_Stream, Result);
   Check (not Result.Success and then Result.ErrorCode /= 0,
          "external length beyond buffer accepted");
   Check (External_Stream.Current_Bit_Pos <= External_Stream.Size_In_Bytes * 8,
          "external cursor beyond buffer");

   ASN1SCC_EmbeddedPdu_ACN_Encode (embOdd, Embedded_Stream, Result);
   Check (Result.Success, "embedded encode");
   Check (Embedded_Stream.Current_Bit_Pos = 40, "embedded padding/cursor");
   Check (Embedded_Stream.Buffer (1 .. 5) =
          adaasn1rtl.OctetBuffer'(17, 2, 2, 160, 165), "embedded wire bytes");
   ASN1SCC_EmbeddedPdu_ACN_Decode (Embedded_Value, Embedded_Stream, Result);
   Check (Result.Success and then
          ASN1SCC_EmbeddedPdu_Equal (embOdd, Embedded_Value),
          "embedded region includes following field");
   Embedded_Stream.Buffer (2) := 255;
   ASN1SCC_EmbeddedPdu_ACN_Decode (Embedded_Value, Embedded_Stream, Result);
   Check (not Result.Success, "invalid embedded length accepted");

   ASN1SCC_PairPdu_ACN_Encode (pairOdd, Pair_Stream, Result);
   Check (Result.Success and then Pair_Stream.Current_Bit_Pos = 56,
          "adjacent padding");
   ASN1SCC_PairPdu_ACN_Decode (Pair_Value, Pair_Stream, Result);
   Check (Result.Success and then ASN1SCC_PairPdu_Equal (pairOdd, Pair_Value),
          "adjacent regions overlap");
   ASN1SCC_PairPdu_ACN_Encode (pairEmpty, Pair_Stream, Result);
   Check (Result.Success and then Pair_Stream.Current_Bit_Pos = 40,
          "empty region encode");
   ASN1SCC_PairPdu_ACN_Decode (Pair_Value, Pair_Stream, Result);
   Check (Result.Success and then
          ASN1SCC_PairPdu_Equal (pairEmpty, Pair_Value),
          "empty region consumes following data");
   Ada.Text_IO.Put_Line
     ("Containing bounds, padding, wire bytes and malformed lengths: OK");
end Containing_Bounds;
