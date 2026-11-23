--  Byte-exact uPER checks of 001.asn1 in Ada (run by v4Tests/scripts/runWireTests.sh with -gnata, Issue 94): the same
--  values and bytes as 001_wire_test.c (X.691, cross-checked with pycrate 0.8.1).
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with UPER_WIRE; use UPER_WIRE;
procedure Wire_Test is
   Failed : Boolean := False;

   procedure Fail (What : String) is
   begin
      Ada.Text_IO.Put_Line ("FAILED: " & What);
      Failed := True;
   end Fail;

   procedure Check_Bytes
     (What : String; S : adaasn1rtl.encoding.Bitstream; Expected : OctetBuffer) is
   begin
      if (S.Current_Bit_Pos + 7) / 8 /= Expected'Length
        or else S.Buffer (1 .. Expected'Length) /= Expected
      then
         Fail (What & ": encoded bytes");
      end if;
   end Check_Bytes;

   procedure Ok (What : String; R : ASN1_RESULT) is
   begin
      if not R.Success then
         Fail (What);
      end if;
   end Ok;

   --  the length octets of a last part of n < 16K units
   function Glen (N : Natural) return OctetBuffer is
     (if N < 128 then (0 => Asn1Byte (N))
      else (Asn1Byte (16#80# + N / 256), Asn1Byte (N mod 256)));

   function Fill (N : Natural; K : Positive := 256) return OctetBuffer is
      B : OctetBuffer (1 .. N);
   begin
      for I in B'Range loop
         B (I) := Asn1Byte ((I - 1) mod K);
      end loop;
      return B;
   end Fill;

   R : ASN1_RESULT;
   I1 : constant Inner := (a => 5, b => True);
begin
   declare
      V : constant P :=
        (f => I1, g => (a => 1, s => (Length => 3, Data => (1 => True, 2 => False, 3 => True, others => False))),
         n => Nul_Init, z => True);
      D : P;
      S : P_uPER_Stream;
   begin
      P_Encode (V, S, R);
      Ok ("P encode", R);
      Check_Bytes ("P", S, (16#02#, 16#05#, 16#80#, 16#10#, 16#01#, 16#1d#, 16#01#, 16#00#, 16#80#));
      P_Decode (D, S, R);
      Ok ("P decode", R);
      if not P_Equal (V, D) then
         Fail ("P value");
      end if;
   end;
   declare
      S : NulOct_uPER_Stream;
      S2 : NulBit_uPER_Stream;
      D : UPER_WIRE.Nul;
   begin
      NulOct_Encode (Nul_Init, S, R);
      Check_Bytes ("NulOct", S, (16#01#, 16#00#));
      NulOct_Decode (D, S, R);
      Ok ("NulOct decode", R);
      NulBit_Encode (Nul_Init, S2, R);
      Check_Bytes ("NulBit", S2, (16#08#, 16#00#));
      NulBit_Decode (D, S2, R);
      Ok ("NulBit decode", R);
   end;
   declare
      V : constant T91 := (x => I1, y => False);
      D : T91;
      S : T91_uPER_Stream;
   begin
      T91_Encode (V, S, R);
      Check_Bytes ("T91", S, (16#02#, 16#05#, 16#80#, 16#00#));
      T91_Decode (D, S, R);
      Ok ("T91 decode", R);
      if not T91_Equal (V, D) then
         Fail ("T91 value");
      end if;
   end;
   declare
      V : constant Nested := (o => (m => I1));
      D : Nested;
      S : Nested_uPER_Stream;
   begin
      Nested_Encode (V, S, R);
      Check_Bytes ("Nested", S, (16#03#, 16#10#, 16#05#, 16#80#));
      Nested_Decode (D, S, R);
      Ok ("Nested decode", R);
      if not Nested_Equal (V, D) then
         Fail ("Nested value");
      end if;
   end;
   declare
      V : constant Lst := (Length => 2, Data => (1 => (a => 1, b => False), 2 => (a => 2, b => True), others => I1));
      D : Lst;
      S : Lst_uPER_Stream;
   begin
      Lst_Encode (V, S, R);
      Check_Bytes ("Lst", S, (16#40#, 16#80#, 16#40#, 16#00#, 16#80#, 16#a0#, 16#00#));
      Lst_Decode (D, S, R);
      Ok ("Lst decode", R);
      if not Lst_Equal (V, D) then
         Fail ("Lst value");
      end if;
   end;
   declare
      V : constant Ch := (kind => c2_PRESENT, c2 => (a => 7, b => True));
      D : Ch;
      S : Ch_uPER_Stream;
   begin
      Ch_Encode (V, S, R);
      Check_Bytes ("Ch", S, (16#88#, 16#03#, 16#c0#, 16#00#));
      Ch_Decode (D, S, R);
      Ok ("Ch decode", R);
      if not Ch_Equal (V, D) then
         Fail ("Ch value");
      end if;
   end;
   declare
      V1 : constant Opt := (o => Inner_Init, t => True, Exist => (o => 0));
      V2 : constant Opt := (o => (a => 16#AB#, b => False), t => False, Exist => (o => 1));
      D : Opt;
      S : Opt_uPER_Stream;
   begin
      Opt_Encode (V1, S, R);
      Check_Bytes ("Opt absent", S, (0 => 16#40#));
      Opt_Decode (D, S, R);
      Ok ("Opt absent decode", R);
      Opt_Encode (V2, S, R);
      Check_Bytes ("Opt present", S, (16#81#, 16#55#, 16#80#, 16#00#));
      Opt_Decode (D, S, R);
      Ok ("Opt present decode", R);
      if not Opt_Equal (V2, D) then
         Fail ("Opt value");
      end if;
   end;
   declare
      V : CBig;
      D : CBig;
      S : CBig_uPER_Stream;
   begin
      V.d.Length := 0;
      V.d.Data := (others => 0);
      CBig_Encode (V, S, R);
      Ok ("CBig empty encode", R);
      Check_Bytes ("CBig empty", S, (16#01#, 16#00#));
      CBig_Decode (D, S, R);
      Ok ("CBig empty decode", R);
   end;
   declare
      V127 : constant C127 := (Data => O127_array (Fill (127)));
      V128 : constant C128 := (Data => O128_array (Fill (128)));
      V16383 : constant C16383 := (Data => O16383_array (Fill (16383)));
      V16384 : constant C16384 := (Data => O16384_array (Fill (16384)));
      V16 : constant B16 := (Data => O16_array (Fill (16)));
      V2048 : constant B2048 := (Data => O2048_array (Fill (2048)));
      S127 : C127_uPER_Stream;
      S128 : C128_uPER_Stream;
      S16383 : C16383_uPER_Stream;
      S16384 : C16384_uPER_Stream;
      S16 : B16_uPER_Stream;
      S2048 : B2048_uPER_Stream;
      D127 : C127;
      D128 : C128;
      D16383 : C16383;
      D16384 : C16384;
      D16 : B16;
      D2048 : B2048;
   begin
      C127_Encode (V127, S127, R);
      Check_Bytes ("C127", S127, Glen (127) & Fill (127));
      C127_Decode (D127, S127, R);
      Ok ("C127 decode", R);
      C128_Encode (V128, S128, R);
      Check_Bytes ("C128", S128, Glen (128) & Fill (128));
      C128_Decode (D128, S128, R);
      Ok ("C128 decode", R);
      C16383_Encode (V16383, S16383, R);
      Check_Bytes ("C16383", S16383, Glen (16383) & Fill (16383));
      C16383_Decode (D16383, S16383, R);
      Ok ("C16383 decode", R);
      C16384_Encode (V16384, S16384, R);
      Check_Bytes ("C16384", S16384, OctetBuffer'(0 => 16#C1#) & Fill (16384) & OctetBuffer'(0 => 0));
      C16384_Decode (D16384, S16384, R);
      Ok ("C16384 decode", R);
      B16_Encode (V16, S16, R);
      Check_Bytes ("B16", S16, Glen (128) & Fill (16));
      B16_Decode (D16, S16, R);
      Ok ("B16 decode", R);
      B2048_Encode (V2048, S2048, R);
      Check_Bytes ("B2048", S2048, OctetBuffer'(0 => 16#C1#) & Fill (2048) & OctetBuffer'(0 => 0));
      B2048_Decode (D2048, S2048, R);
      Ok ("B2048 decode", R);
      if not (C127_Equal (V127, D127) and C128_Equal (V128, D128) and C16383_Equal (V16383, D16383)
              and C16384_Equal (V16384, D16384) and B16_Equal (V16, D16) and B2048_Equal (V2048, D2048))
      then
         Fail ("large values");
      end if;
   end;
   declare
      --  the contents (Big, 70003 octets) are sent as C4, their first 64K octets, the length of the last 4467, them
      D70 : constant OctetBuffer := Fill (70000, 251);
      Contents : constant OctetBuffer :=
        OctetBuffer'(0 => 16#C4#) & D70 (1 .. 65536) & Glen (70000 - 65536) & D70 (65537 .. 70000);
      V : CBig;
      D : CBig;
      S : CBig_uPER_Stream;
   begin
      V.d.Length := 70000;
      V.d.Data := Big_d_array (D70);
      CBig_Encode (V, S, R);
      Ok ("CBig encode", R);
      Check_Bytes ("CBig", S,
                   OctetBuffer'(0 => 16#C4#) & Contents (Contents'First .. Contents'First + 65535)
                   & Glen (Contents'Length - 65536) & Contents (Contents'First + 65536 .. Contents'Last));
      CBig_Decode (D, S, R);
      Ok ("CBig decode", R);
      if not CBig_Equal (V, D) then
         Fail ("CBig value");
      end if;
   end;

   --  decoders: any length up to the maximum, also 0 and bits that are not a multiple of 8
   declare
      procedure Decode_Nul_Oct (What : String; Bytes : OctetBuffer; Expected : Boolean) is
         S : NulOct_uPER_Stream;
         D : UPER_WIRE.Nul;
      begin
         S.Buffer := (others => 0);
         S.Buffer (1 .. Bytes'Length) := Bytes;
         NulOct_Decode (D, S, R);
         if R.Success /= Expected or else (not R.Success and then R.ErrorCode = 0) then
            Fail (What);
         end if;
      end Decode_Nul_Oct;
      procedure Decode_Oct_Box (What : String; Bytes : OctetBuffer; Expected : Boolean) is
         S : OctBox_uPER_Stream;
         D : Inner;
      begin
         S.Buffer := (others => 0);
         S.Buffer (1 .. Bytes'Length) := Bytes;
         OctBox_Decode (D, S, R);
         if R.Success /= Expected or else (not R.Success and then R.ErrorCode = 0) then
            Fail (What);
         end if;
      end Decode_Oct_Box;
      S : Ch_uPER_Stream;
      D : Ch;
      SB : NulBit_uPER_Stream;
      DB : UPER_WIRE.Nul;
   begin
      Decode_Nul_Oct ("NULL with no octet", (0 => 16#00#), True);
      Decode_Nul_Oct ("header C0", (16#C0#, 16#00#), False);
      Decode_Nul_Oct ("header C5", (16#C5#, 16#00#), False);
      Decode_Oct_Box ("contents past the region", (16#01#, 16#05#), False);
      Decode_Oct_Box ("length above the maximum", (16#03#, 16#05#, 16#80#), False);
      Decode_Oct_Box ("a length below 128 in two octets", (16#80#, 16#02#, 16#05#), False);
      S.Buffer := (others => 0);
      S.Buffer (1 .. 3) := (16#84#, 16#83#, 16#c0#);
      Ch_Decode (D, S, R);
      if not R.Success or else not Ch_Equal (D, (kind => c2_PRESENT, c2 => (a => 7, b => True))) then
         Fail ("BIT STRING of 9 bits");
      end if;
      SB.Buffer := (16#0a#, 16#05#);
      NulBit_Decode (DB, SB, R);
      if R.Success or else R.ErrorCode = 0 then
         Fail ("BIT STRING above the maximum");
      end if;
   end;

   if Failed then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Wire_Test;
