--  Ada check of 036 on the 32-bit board msp430 (run by v4Tests/scripts/runWireTests.sh): the 64-bit fields
--  carry the sign extension in their upper 4 bytes, and the decoders reject a field whose upper bytes do not hold
--  it, since the value does not fit in 32 bits (before, the decoders skipped the upper bytes).
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with adaasn1rtl;
with TEST_CASE;
procedure Decode_Test is
   use type Interfaces.Unsigned_8;
   use type adaasn1rtl.Asn1Int;
   use type adaasn1rtl.OctetBuffer;
   Expected : constant adaasn1rtl.OctetBuffer (1 .. 32) :=
     (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FE#,
      16#FE#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#01#,
      16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#);
   Value   : constant TEST_CASE.MyPDU := (sb => -2, sl => -2, ub => 1, ul => 1);
   Encoded : TEST_CASE.MyPDU_ACN_Stream;
   R       : adaasn1rtl.ASN1_RESULT;

   procedure Fail (Msg : String) is
   begin
      Ada.Text_IO.Put_Line (Msg);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Fail;

   --  decode the encoding with byte At_Byte (0-based) set to V: must fail
   procedure Rejected (At_Byte : Natural; V : adaasn1rtl.Asn1Byte) is
      S   : TEST_CASE.MyPDU_ACN_Stream := Encoded;
      Out_Value : TEST_CASE.MyPDU;
   begin
      S.Buffer (S.Buffer'First + At_Byte) := V;
      S.Current_Bit_Pos := 0;
      TEST_CASE.MyPDU_ACN_Decode (Out_Value, S, R);
      if R.Success then
         Fail ("accepted: byte" & Natural'Image (At_Byte) & " set to" & adaasn1rtl.Asn1Byte'Image (V));
      end if;
   end Rejected;
begin
   TEST_CASE.MyPDU_ACN_Encode (Value, Encoded, R);
   if not R.Success
     or else Encoded.Buffer (Encoded.Buffer'First .. Encoded.Buffer'First + 31) /= Expected
   then
      Fail ("encoding differs");
   end if;
   declare
      S         : TEST_CASE.MyPDU_ACN_Stream := Encoded;
      Out_Value : TEST_CASE.MyPDU;
      use type TEST_CASE.MyPDU;
   begin
      S.Current_Bit_Pos := 0;
      TEST_CASE.MyPDU_ACN_Decode (Out_Value, S, R);
      if not R.Success or else Out_Value /= Value then
         Fail ("round trip failed");
      end if;
   end;
   Rejected (0, 16#00#);
   Rejected (3, 16#7F#);
   Rejected (15, 16#00#);
   Rejected (19, 16#01#);
   Rejected (28, 16#01#);
exception
   when others =>
      Fail ("exception");
end Decode_Test;
