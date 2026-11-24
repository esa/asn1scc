--  q/issues.md Issue 84: a 'size deduced' SEQUENCE OF of variable-size elements with SIZE (min .. max), min >= 1,
--  decoded from a view (End_Bit_Pos) that holds fewer than min elements. The decoder must fail with a non-zero
--  error code and without an exception (with run-time checks and with -gnatp); a valid encoding must decode.
with Ada.Text_IO;
with Ada.Exceptions;
with adaasn1rtl;
with adaasn1rtl.encoding;
with TEST_CASE; use TEST_CASE;

procedure Generated_Tests is
   Failures : Natural := 0;

   procedure Report (Name : String; Expect_Ok : Boolean; Res : adaasn1rtl.ASN1_RESULT) is
      Ok : constant Boolean :=
        (if Expect_Ok then Res.Success else not Res.Success and then Res.ErrorCode /= 0);
   begin
      Ada.Text_IO.Put_Line ((if Ok then "PASS " else "FAIL ") & Name & ": Success=" & Boolean'Image (Res.Success)
                            & " ErrorCode=" & Integer'Image (Res.ErrorCode));
      if not Ok then
         Failures := Failures + 1;
      end if;
   end Report;

   procedure Report_Exception (Name : String; E : Ada.Exceptions.Exception_Occurrence) is
   begin
      Ada.Text_IO.Put_Line ("FAIL " & Name & ": exception " & Ada.Exceptions.Exception_Name (E) & " "
                            & Ada.Exceptions.Exception_Message (E));
      Failures := Failures + 1;
   end Report_Exception;

   --  EList (top level): a view of View_Bits bits, fewer than one element (9 bits)
   procedure Decode_EList (View_Bits : Natural) is
      Name : constant String := "EList, view of" & Natural'Image (View_Bits) & " bits";
      S    : adaasn1rtl.encoding.Bitstream :=
        adaasn1rtl.encoding.BitStream_init (EList_REQUIRED_BYTES_FOR_ACN_ENCODING);
      V    : EList := EList_Init;
      Res  : adaasn1rtl.ASN1_RESULT;
   begin
      S.End_Bit_Pos := View_Bits;
      EList_ACN_Decode_aux (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_EList;

   --  Box: a view that holds one element where SIZE (2 .. 5) requires two
   procedure Decode_Box is
      Name : constant String := "Box, one element of the two required";
      S    : adaasn1rtl.encoding.Bitstream :=
        adaasn1rtl.encoding.BitStream_init (Box_REQUIRED_BYTES_FOR_ACN_ENCODING);
      V    : Box := Box_Init;
      Res  : adaasn1rtl.ASN1_RESULT;
   begin
      S.End_Bit_Pos := 16;   --  one element without b (9 bits) and 7 padding bits
      Box_ACN_Decode_aux (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_Box;

   procedure Round_Trip_EList is
      S   : adaasn1rtl.encoding.Bitstream :=
        adaasn1rtl.encoding.BitStream_init (EList_REQUIRED_BYTES_FOR_ACN_ENCODING);
      V   : EList := EList_Init;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      V.Length := 2;
      V.Data (1) := (a => 1, b => 555, Exist => (b => 1));
      V.Data (2) := (a => 2, b => 0, Exist => (b => 0));
      EList_ACN_Encode_aux (V, S, Res);
      if Res.Success then
         S.End_Bit_Pos := adaasn1rtl.encoding.BitStream_current_length_in_bytes (S) * 8;
         S.Current_Bit_Pos := 0;
         EList_ACN_Decode_aux (V, S, Res);
         if Res.Success and then V.Length /= 2 then
            Res := (Success => False, ErrorCode => 0);
         end if;
      end if;
      Report ("EList round trip", True, Res);
   exception
      when E : others => Report_Exception ("EList round trip", E);
   end Round_Trip_EList;
begin
   for Bits in 0 .. 8 loop
      Decode_EList (Bits);
   end loop;
   Decode_Box;
   Round_Trip_EList;
   if Failures > 0 then
      Ada.Text_IO.Put_Line (Natural'Image (Failures) & " failure(s)");
      raise Program_Error;
   end if;
   Ada.Text_IO.Put_Line ("all passed");
end Generated_Tests;
