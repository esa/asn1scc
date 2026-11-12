--  q/issues.md Issue 85: the Ada ACN null-terminated ASCII decoders (IA5String, INTEGER) accepted a stream where the
--  longest value is not followed by the terminator: they returned Success and consumed the next bytes as if they were
--  the terminator. The C decoders reject such a stream. The decoder must fail with a non-zero error code and without
--  an exception (with run-time checks and with -gnatp); valid encodings must decode.
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

   --  the stream holds Bytes from its first byte, the rest is zero
   generic
      type T is private;
      Required_Bytes : Positive;
      with procedure Decode_Aux (Val : in out T; Bs : in out adaasn1rtl.encoding.Bitstream;
                                 Res : out adaasn1rtl.ASN1_RESULT);
      with function Init return T;
   procedure Decode (Name : String; Bytes : String; Expect_Ok : Boolean);

   procedure Decode (Name : String; Bytes : String; Expect_Ok : Boolean) is
      S   : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (Required_Bytes);
      V   : T := Init;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      for I in Bytes'Range loop
         S.Buffer (S.Buffer'First + (I - Bytes'First)) := adaasn1rtl.Asn1Byte (Character'Pos (Bytes (I)));
      end loop;
      Decode_Aux (V, S, Res);
      Report (Name, Expect_Ok, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode;

   --  the mode of val differs between the decoders (out / in out): adapt to the generic's in out
   procedure NumT_Aux (Val : in out NumT; Bs : in out adaasn1rtl.encoding.Bitstream; Res : out adaasn1rtl.ASN1_RESULT) is
   begin
      NumT_ACN_Decode_aux (Val, Bs, Res);
   end NumT_Aux;

   procedure SNumT_Aux (Val : in out SNumT; Bs : in out adaasn1rtl.encoding.Bitstream; Res : out adaasn1rtl.ASN1_RESULT) is
   begin
      SNumT_ACN_Decode_aux (Val, Bs, Res);
   end SNumT_Aux;

   procedure D_Str is new Decode (Str, Str_REQUIRED_BYTES_FOR_ACN_ENCODING, Str_ACN_Decode_aux, Str_Init);
   procedure D_Str2 is new Decode (Str2, Str2_REQUIRED_BYTES_FOR_ACN_ENCODING, Str2_ACN_Decode_aux, Str2_Init);
   procedure D_NumT is new Decode (NumT, NumT_REQUIRED_BYTES_FOR_ACN_ENCODING, NumT_Aux, NumT_Init);
   procedure D_SNumT is new Decode (SNumT, SNumT_REQUIRED_BYTES_FOR_ACN_ENCODING, SNumT_Aux, SNumT_Init);

   NUL : constant Character := Character'Val (0);
   CR  : constant Character := Character'Val (16#0D#);
   LF  : constant Character := Character'Val (16#0A#);
   Zeros20 : constant String (1 .. 20) := (others => '0');
begin
   D_Str ("Str, 5 characters + NUL", "ABCDE" & NUL, True);
   D_Str ("Str, 2 characters + NUL", "AB" & NUL, True);
   D_Str ("Str, 5 characters without terminator", "ABCDEX", False);
   D_Str2 ("Str2, 5 characters + 0D0A", "ABCDE" & CR & LF, True);
   D_Str2 ("Str2, 5 characters + 0D X", "ABCDE" & CR & "X", False);
   D_Str2 ("Str2, 5 characters + X 0A", "ABCDE" & "X" & LF, False);
   D_NumT ("NumT, 123 + NUL", "123" & NUL, True);
   D_NumT ("NumT, 20 digits + NUL", Zeros20 & NUL, True);
   D_NumT ("NumT, 20 digits without terminator", Zeros20 & "X", False);
   D_SNumT ("SNumT, -123 + NUL", "-123" & NUL, True);
   D_SNumT ("SNumT, + 20 digits + NUL", "+" & Zeros20 & NUL, True);
   D_SNumT ("SNumT, + 20 digits without terminator", "+" & Zeros20 & "X", False);
   if Failures > 0 then
      Ada.Text_IO.Put_Line ("FAILURES:" & Natural'Image (Failures));
      raise Program_Error;
   end if;
end Generated_Tests;
