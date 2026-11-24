--  q/issues.md Issue 86: a fixed-size IA5String subtype (Sub = Str (SIZE (3)), Str = IA5String (SIZE (1 .. 10))) with
--  ACN 'encoding ASCII' shares the typedef of Str (a String (1 .. 11) in Ada). The Ada ACN encoder passed the whole
--  array to Acn_Enc_String_Ascii_FixSize, which writes strVal'Length - 1 = 10 characters, into a stream sized for 3
--  (index check failure; out-of-bounds write with -gnatp), and the decoder read 10 characters. The encoding must be the
--  3 characters (as C), and decoding it must give the value back.
with Ada.Text_IO;
with Ada.Exceptions;
with adaasn1rtl;
with adaasn1rtl.encoding;
with TEST_CASE; use TEST_CASE;

procedure Generated_Tests is
   use type adaasn1rtl.Asn1Byte;
   Failures : Natural := 0;

   procedure Check (Name : String; Ok : Boolean) is
   begin
      Ada.Text_IO.Put_Line ((if Ok then "PASS " else "FAIL ") & Name);
      if not Ok then
         Failures := Failures + 1;
      end if;
   end Check;

   --  a guard after the stream: the encoder must not write past the stream
   type Guarded is record
      S     : Sub_ACN_Stream;
      Guard : adaasn1rtl.OctetBuffer (1 .. 16);
   end record;
   G   : Guarded := (S => adaasn1rtl.encoding.BitStream_init (Sub_REQUIRED_BYTES_FOR_ACN_ENCODING),
                     Guard => (others => 16#5A#));
   V   : Sub := Sub_Init;
   D   : Sub := Sub_Init;
   Res : adaasn1rtl.ASN1_RESULT;
begin
   V := (others => adaasn1rtl.NUL);
   V (1 .. 3) := "ABC";
   begin
      Sub_ACN_Encode (V, G.S, Res);
      Check ("encode: success", Res.Success);
      Check ("encode: 24 bits written, got" & Integer'Image (G.S.Current_Bit_Pos), G.S.Current_Bit_Pos = 24);
      Check ("encode: bytes ABC",
             G.S.Buffer (G.S.Buffer'First) = Character'Pos ('A') and then
             G.S.Buffer (G.S.Buffer'First + 1) = Character'Pos ('B') and then
             G.S.Buffer (G.S.Buffer'First + 2) = Character'Pos ('C'));
      Check ("encode: nothing written after the stream", (for all B of G.Guard => B = 16#5A#));
      Sub_ACN_Decode (D, G.S, Res);
      Check ("decode: success", Res.Success);
      Check ("decode: value ABC", D (1 .. 3) = "ABC" and then (for all I in 4 .. D'Last => D (I) = adaasn1rtl.NUL));
   exception
      when E : others =>
         Check ("exception " & Ada.Exceptions.Exception_Name (E) & " " & Ada.Exceptions.Exception_Message (E), False);
   end;
   if Failures > 0 then
      Ada.Text_IO.Put_Line ("FAILURES:" & Natural'Image (Failures));
      raise Program_Error;
   end if;
end Generated_Tests;
