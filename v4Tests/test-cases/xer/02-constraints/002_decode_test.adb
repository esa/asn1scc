--  Ada XER decoding of lengths outside the SIZE (run by v4Tests/scripts/runWireTests.sh): an error code, no exception.
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl;
with TEST_CASE;
procedure Decode_Test is
   procedure Check (Xml : String; Expected : Boolean) is
      S : TEST_CASE.Msg_XER_Stream;
      V : TEST_CASE.Msg;
      R : adaasn1rtl.ASN1_RESULT;
   begin
      S.Data := (others => Character'Val (0));
      S.Data (1 .. Xml'Length) := Xml;
      S.CurrentByte := 1;
      TEST_CASE.Msg_XER_Decode (V, S, R);
      if R.Success /= Expected or else (not R.Success and then R.ErrorCode = 0) then
         Ada.Text_IO.Put_Line ("unexpected result for " & Xml);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   exception
      when others =>
         Ada.Text_IO.Put_Line ("exception for " & Xml);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Check;
begin
   Check ("<Msg><o>ABCD</o><b>10101</b></Msg>", True);
   Check ("<Msg><o>AB</o><b>10101</b></Msg>", False);
   Check ("<Msg><o>ABCD</o><b>101</b></Msg>", False);
   Check ("<Msg><o>ABCDABCDAB</o><b>10101</b></Msg>", False);
end Decode_Test;
