--  Checks of 016.asn1 / 016.acn in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, with and without
--  -gnata; Issue 168, Issue 92): the values and bytes of 016_wire_test.c, uPER and ACN, in streams of exactly the
--  type's REQUIRED_BYTES (before the fix too small: the RTL raised Constraint_Error on the index, and with -gnata the
--  uPER encoder of Fixed92 failed the precondition of Base92's encoder, which it calls), REQUIRED_BITS pinned.
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with acn_wire_site_sizes; use acn_wire_site_sizes;
procedure Wire_Test is

   procedure Fail (Msg : String) is
   begin
      Ada.Text_IO.Put_Line ("FAILED: " & Msg);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Fail;

   generic
      type T is private;
      Size : Positive;
      Required_Bits : Natural;
      with procedure Encode
        (V : T; S : out adaasn1rtl.encoding.Bitstream; R : out ASN1_RESULT);
      with procedure Decode
        (V : out T; S : in out adaasn1rtl.encoding.Bitstream; R : out ASN1_RESULT);
      with function Equal (V1, V2 : T) return Boolean;
   procedure Round_Trip (Name : String; V : T; Req_Bits : Natural; Expected : OctetBuffer; Bits : Natural);

   procedure Round_Trip (Name : String; V : T; Req_Bits : Natural; Expected : OctetBuffer; Bits : Natural) is
      S : adaasn1rtl.encoding.Bitstream (Size);
      D : T;
      R : ASN1_RESULT;
   begin
      if Required_Bits /= Req_Bits or else Bits > Required_Bits or else Expected'Length /= (Bits + 7) / 8 then
         Fail (Name & ": REQUIRED_BITS");
         return;
      end if;
      Encode (V, S, R);
      if not R.Success then
         Fail (Name & ": encode");
         return;
      end if;
      if S.Current_Bit_Pos /= Bits or else S.Buffer (1 .. Expected'Length) /= Expected then
         Fail (Name & ": encoded bytes");
      end if;
      S.Buffer := (others => 0);
      S.Buffer (1 .. Expected'Length) := Expected;
      S.Current_Bit_Pos := 0;
      Decode (D, S, R);
      if not R.Success or else not Equal (D, V) or else S.Current_Bit_Pos /= Bits then
         Fail (Name & ": decode");
      end if;
   end Round_Trip;

   procedure U_P9 is new Round_Trip
     (P9, P9_REQUIRED_BYTES_FOR_ENCODING, P9_REQUIRED_BITS_FOR_ENCODING, P9_Encode, P9_Decode, P9_Equal);
   procedure A_P9 is new Round_Trip
     (P9, P9_ACN_STREAM_SIZE, P9_REQUIRED_BITS_FOR_ACN_ENCODING, P9_ACN_Encode, P9_ACN_Decode, P9_Equal);
   procedure U_Fixed92 is new Round_Trip
     (Fixed92, Fixed92_REQUIRED_BYTES_FOR_ENCODING, Fixed92_REQUIRED_BITS_FOR_ENCODING, Fixed92_Encode,
      Fixed92_Decode, Fixed92_Equal);
   procedure A_Fixed92 is new Round_Trip
     (Fixed92, Fixed92_ACN_STREAM_SIZE, Fixed92_REQUIRED_BITS_FOR_ACN_ENCODING, Fixed92_ACN_Encode,
      Fixed92_ACN_Decode, Fixed92_Equal);
   procedure U_Oct18 is new Round_Trip
     (Oct18, Oct18_REQUIRED_BYTES_FOR_ENCODING, Oct18_REQUIRED_BITS_FOR_ENCODING, Oct18_Encode, Oct18_Decode,
      Oct18_Equal);
   procedure A_Oct18 is new Round_Trip
     (Oct18, Oct18_ACN_STREAM_SIZE, Oct18_REQUIRED_BITS_FOR_ACN_ENCODING, Oct18_ACN_Encode, Oct18_ACN_Decode,
      Oct18_Equal);
   procedure U_Bit18 is new Round_Trip
     (Bit18, Bit18_REQUIRED_BYTES_FOR_ENCODING, Bit18_REQUIRED_BITS_FOR_ENCODING, Bit18_Encode, Bit18_Decode,
      Bit18_Equal);
   procedure A_Bit18 is new Round_Trip
     (Bit18, Bit18_ACN_STREAM_SIZE, Bit18_REQUIRED_BITS_FOR_ACN_ENCODING, Bit18_ACN_Encode, Bit18_ACN_Decode,
      Bit18_Equal);
   procedure U_Sub15 is new Round_Trip
     (Sub15, Sub15_REQUIRED_BYTES_FOR_ENCODING, Sub15_REQUIRED_BITS_FOR_ENCODING, Sub15_Encode, Sub15_Decode,
      Sub15_Equal);
   procedure A_Sub15 is new Round_Trip
     (Sub15, Sub15_ACN_STREAM_SIZE, Sub15_REQUIRED_BITS_FOR_ACN_ENCODING, Sub15_ACN_Encode, Sub15_ACN_Decode,
      Sub15_Equal);
   procedure U_Outer10 is new Round_Trip
     (Outer10, Outer10_REQUIRED_BYTES_FOR_ENCODING, Outer10_REQUIRED_BITS_FOR_ENCODING, Outer10_Encode,
      Outer10_Decode, Outer10_Equal);
   procedure A_Outer10 is new Round_Trip
     (Outer10, Outer10_ACN_STREAM_SIZE, Outer10_REQUIRED_BITS_FOR_ACN_ENCODING, Outer10_ACN_Encode,
      Outer10_ACN_Decode, Outer10_Equal);
   procedure U_Outer12 is new Round_Trip
     (Outer12, Outer12_REQUIRED_BYTES_FOR_ENCODING, Outer12_REQUIRED_BITS_FOR_ENCODING, Outer12_Encode,
      Outer12_Decode, Outer12_Equal);
   procedure A_Outer12 is new Round_Trip
     (Outer12, Outer12_ACN_STREAM_SIZE, Outer12_REQUIRED_BITS_FOR_ACN_ENCODING, Outer12_ACN_Encode,
      Outer12_ACN_Decode, Outer12_Equal);
   procedure U_Outer9 is new Round_Trip
     (Outer9, Outer9_REQUIRED_BYTES_FOR_ENCODING, Outer9_REQUIRED_BITS_FOR_ENCODING, Outer9_Encode,
      Outer9_Decode, Outer9_Equal);
   procedure A_Outer9 is new Round_Trip
     (Outer9, Outer9_ACN_STREAM_SIZE, Outer9_REQUIRED_BITS_FOR_ACN_ENCODING, Outer9_ACN_Encode,
      Outer9_ACN_Decode, Outer9_Equal);
   procedure U_Both8 is new Round_Trip
     (Both8, Both8_REQUIRED_BYTES_FOR_ENCODING, Both8_REQUIRED_BITS_FOR_ENCODING, Both8_Encode, Both8_Decode,
      Both8_Equal);
   procedure A_Both8 is new Round_Trip
     (Both8, Both8_ACN_STREAM_SIZE, Both8_REQUIRED_BITS_FOR_ACN_ENCODING, Both8_ACN_Encode, Both8_ACN_Decode,
      Both8_Equal);
   procedure U_Outer7 is new Round_Trip
     (Outer7, Outer7_REQUIRED_BYTES_FOR_ENCODING, Outer7_REQUIRED_BITS_FOR_ENCODING, Outer7_Encode,
      Outer7_Decode, Outer7_Equal);
   procedure A_Outer7 is new Round_Trip
     (Outer7, Outer7_ACN_STREAM_SIZE, Outer7_REQUIRED_BITS_FOR_ACN_ENCODING, Outer7_ACN_Encode,
      Outer7_ACN_Decode, Outer7_Equal);
   procedure U_Sub5 is new Round_Trip
     (Sub5, Sub5_REQUIRED_BYTES_FOR_ENCODING, Sub5_REQUIRED_BITS_FOR_ENCODING, Sub5_Encode, Sub5_Decode,
      Sub5_Equal);
   procedure A_Sub5 is new Round_Trip
     (Sub5, Sub5_ACN_STREAM_SIZE, Sub5_REQUIRED_BITS_FOR_ACN_ENCODING, Sub5_ACN_Encode, Sub5_ACN_Decode,
      Sub5_Equal);
   procedure U_Outer5 is new Round_Trip
     (Outer5, Outer5_REQUIRED_BYTES_FOR_ENCODING, Outer5_REQUIRED_BITS_FOR_ENCODING, Outer5_Encode,
      Outer5_Decode, Outer5_Equal);
   procedure A_Outer5 is new Round_Trip
     (Outer5, Outer5_ACN_STREAM_SIZE, Outer5_REQUIRED_BITS_FOR_ACN_ENCODING, Outer5_ACN_Encode,
      Outer5_ACN_Decode, Outer5_Equal);

   procedure U_OctR is new Round_Trip
     (OctR, OctR_REQUIRED_BYTES_FOR_ENCODING, OctR_REQUIRED_BITS_FOR_ENCODING, OctR_Encode, OctR_Decode, OctR_Equal);
   procedure A_OctR is new Round_Trip
     (OctR, OctR_ACN_STREAM_SIZE, OctR_REQUIRED_BITS_FOR_ACN_ENCODING, OctR_ACN_Encode, OctR_ACN_Decode, OctR_Equal);
   procedure U_BitR is new Round_Trip
     (BitR, BitR_REQUIRED_BYTES_FOR_ENCODING, BitR_REQUIRED_BITS_FOR_ENCODING, BitR_Encode, BitR_Decode, BitR_Equal);
   procedure A_BitR is new Round_Trip
     (BitR, BitR_ACN_STREAM_SIZE, BitR_REQUIRED_BITS_FOR_ACN_ENCODING, BitR_ACN_Encode, BitR_ACN_Decode, BitR_Equal);

   V_P9 : P9 := P9_Init;
   V_F92 : Fixed92 := Fixed92_Init;
   V_O18 : Oct18 := Oct18_Init;
   V_B18 : Bit18 := Bit18_Init;
   V_S15 : Sub15 := Sub15_Init;
   V_O10 : Outer10 := Outer10_Init;
   V_O12 : Outer12 := Outer12_Init;
   V_O9 : Outer9 := Outer9_Init;
   V_B8 : Both8 := Both8_Init;
   V_O7 : Outer7 := Outer7_Init;
   V_S5 : Sub5 := Sub5_Init;
   V_O5 : Outer5 := Outer5_Init;
   V_OR : OctR := OctR_Init;
   V_BR : BitR := BitR_Init;
begin
   V_P9.x.s := (1 => 'a', 2 => 'b', others => NUL);
   V_P9.x.b := True;
   U_P9 ("P9 uPER", V_P9, 46, (16#38#, 16#71#, 16#40#), 18);
   A_P9 ("P9 ACN", V_P9, 18, (16#38#, 16#71#, 16#40#), 18);

   V_F92.cmds.Length := 1;
   V_F92.cmds.Data (1) := True;
   V_F92.buf.Length := 10;
   V_F92.buf.Data (1 .. 10) := (others => 16#A5#);
   V_F92.bits.Length := 20;
   V_F92.bits.Data (1 .. 20) := (others => 1);
   if Fixed92_REQUIRED_BITS_FOR_ENCODING /= Base92_REQUIRED_BITS_FOR_ENCODING then
      Fail ("Fixed92 uPER REQUIRED_BITS /= Base92's");
   end if;
   U_Fixed92 ("Fixed92 uPER", V_F92, 1830,
              (16#18#, 16#55#, 16#2D#, 16#2D#, 16#2D#, 16#2D#, 16#2D#, 16#2D#, 16#2D#, 16#2D#, 16#2D#, 16#28#,
               16#A7#, 16#FF#, 16#FF#, 16#80#), 121);
   A_Fixed92 ("Fixed92 ACN", V_F92, 101,
              (16#D2#, 16#D2#, 16#D2#, 16#D2#, 16#D2#, 16#D2#, 16#D2#, 16#D2#, 16#D2#, 16#D2#, 16#FF#, 16#FF#,
               16#F8#), 101);

   V_O18.o.s := "abc" & NUL;
   V_O18.o.k := True;
   V_O18.b := True;
   V_B18.p.s := "abc" & NUL;
   V_B18.p.k := True;
   V_B18.b := True;
   U_Oct18 ("Oct18 uPER", V_O18, 33, (16#03#, 16#B0#, 16#E2#, 16#C7#, 16#80#), 33);
   A_Oct18 ("Oct18 ACN", V_O18, 25, (16#B0#, 16#E2#, 16#C7#, 16#80#), 25);
   U_Bit18 ("Bit18 uPER", V_B18, 33, (16#18#, 16#B0#, 16#E2#, 16#C7#, 16#80#), 33);
   A_Bit18 ("Bit18 ACN", V_B18, 25, (16#B0#, 16#E2#, 16#C7#, 16#80#), 25);

   V_S15.data.Length := 10;
   V_S15.data.Data := (others => 16#55#);
   V_S15.s := (1 => 'a', 2 => 'b', others => NUL);
   U_Sub15 ("Sub15 uPER", V_S15, 158,
            (16#A5#, 16#55#, 16#55#, 16#55#, 16#55#, 16#55#, 16#55#, 16#55#, 16#55#, 16#55#, 16#51#, 16#C3#,
             16#88#), 102);
   A_Sub15 ("Sub15 ACN", V_S15, 98,
            (16#55#, 16#55#, 16#55#, 16#55#, 16#55#, 16#55#, 16#55#, 16#55#, 16#55#, 16#55#, 16#1C#, 16#38#,
             16#80#), 98);

   V_O10.x.s := (1 => 'a', 2 => 'b', 3 => 'c', others => NUL);
   V_O10.x.b := True;
   U_Outer10 ("Outer10 uPER", V_O10, 146, (16#16#, 16#1C#, 16#58#, 16#E0#), 27);
   A_Outer10 ("Outer10 ACN", V_O10, 27, (16#16#, 16#1C#, 16#58#, 16#E0#), 27);

   V_O12.x.Exist := (s => 1, bs => 1);
   V_O12.x.s := (1 => 'a', 2 => 'b', others => NUL);
   V_O12.x.bs.Length := 3;
   V_O12.x.bs.Data (1 .. 3) := (others => 1);
   V_O12.x.b := True;
   U_Outer12 ("Outer12 uPER", V_O12, 133, (16#C7#, 16#0E#, 16#20#, 16#FC#), 30);
   A_Outer12 ("Outer12 ACN", V_O12, 22, (16#1C#, 16#38#, 16#BC#), 22);

   V_O9.x.f.Length := 8;
   V_O9.x.f.Data (1 .. 8) := (others => 1);
   V_O9.x.g.Length := 4;
   V_O9.x.g.Data (1 .. 4) := (others => 1);
   U_Outer9 ("Outer9 uPER", V_O9, 177, (16#11#, 16#FE#, 16#1F#, 16#80#), 25);
   A_Outer9 ("Outer9 ACN", V_O9, 12, (16#FF#, 16#F0#), 12);

   V_B8.o.s := (1 => 'a', 2 => 'b', others => NUL);
   V_B8.o.k := True;
   V_B8.p.s := (1 => 'a', 2 => 'b', others => NUL);
   V_B8.p.k := True;
   V_B8.b := True;
   U_Both8 ("Both8 uPER", V_B8, 177, (16#03#, 16#1C#, 16#38#, 16#A0#, 16#18#, 16#1C#, 16#38#, 16#A0#, 16#80#), 65);
   A_Both8 ("Both8 ACN", V_B8, 44, (16#1C#, 16#38#, 16#A0#, 16#1C#, 16#38#, 16#B0#), 44);

   V_O7.x.s := (1 => 'z', 2 => 'z', others => NUL);
   V_O7.x.b := True;
   U_Outer7 ("Outer7 uPER", V_O7, 490033, (16#02#, 16#F5#, 16#EA#), 23);
   A_Outer7 ("Outer7 ACN", V_O7, 23, (16#02#, 16#F5#, 16#EA#), 23);

   V_S5.s := (1 => 'a', 2 => 'b', others => NUL);
   V_S5.b := True;
   U_Sub5 ("Sub5 uPER", V_S5, 146, (16#0E#, 16#1C#, 16#50#), 20);
   A_Sub5 ("Sub5 ACN", V_S5, 20, (16#0E#, 16#1C#, 16#50#), 20);

   V_O5.x.Length := 2;
   V_O5.x.Data (1).Length := 8;
   V_O5.x.Data (1).Data (1 .. 8) := (others => 1);
   V_O5.x.Data (2).Length := 8;
   V_O5.x.Data (2).Data (1 .. 8) := (0, 0, 0, 0, 1, 1, 1, 1);
   U_Outer5 ("Outer5 uPER", V_O5, 215, (16#88#, 16#FF#, 16#10#, 16#1E#), 31);
   A_Outer5 ("Outer5 ACN", V_O5, 17, (16#FF#, 16#87#, 16#80#), 17);

   --  the ACN wire change of the fix (approved): CONTAINING of a range-narrowed string (see 016_wire_test.c)
   V_OR.o.s := (1 => 'a', others => NUL);
   V_OR.o.b := True;
   V_OR.z := True;
   V_BR.p.s := (1 => 'a', others => NUL);
   V_BR.p.b := True;
   V_BR.z := True;
   U_OctR ("OctR a uPER", V_OR, 57, (16#02#, 16#18#, 16#60#, 16#80#), 25);
   A_OctR ("OctR a ACN", V_OR, 26, (16#0C#, 16#30#, 16#40#), 18);
   U_BitR ("BitR a uPER", V_BR, 57, (16#10#, 16#18#, 16#60#, 16#80#), 25);
   A_BitR ("BitR a ACN", V_BR, 22, (16#03#, 16#0E#), 15);
   V_OR.o.s := (1 => 'a', 2 => 'b', others => NUL);
   V_BR.p.s := (1 => 'a', 2 => 'b', others => NUL);
   U_OctR ("OctR ab uPER", V_OR, 57, (16#03#, 16#38#, 16#71#, 16#40#, 16#80#), 33);
   A_OctR ("OctR ab ACN", V_OR, 26, (16#9C#, 16#38#, 16#A0#, 16#40#), 26);
   U_BitR ("BitR ab uPER", V_BR, 57, (16#18#, 16#38#, 16#71#, 16#40#, 16#80#), 33);
   A_BitR ("BitR ab ACN", V_BR, 22, (16#E7#, 16#0E#, 16#2C#), 22);
end Wire_Test;
