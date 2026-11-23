--  Checks of 008.asn1 / 008.acn in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, with and without
--  -gnata; Issue 173): the values, bytes and rejected rows of 008_wire_test.c, in streams of exactly the type's
--  REQUIRED_BYTES (one octet for a type of 0 bits), REQUIRED_BITS pinned. Before the fix the subtypes had the wrong
--  range (I1 is Asn1UInt range 5 .. 10: the valid values 0 .. 4 were not representable), U1 / I2 / P64 were
--  semi-constrained and S2 / O2 / I8 / S8 / R2 did not compile.
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with uper_wire_range; use uper_wire_range;
procedure Wire_Test is

   procedure Fail (Msg : String) is
   begin
      Ada.Text_IO.Put_Line ("FAILED: " & Msg);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Fail;

   --  S padded with NUL to N characters (the string types are NUL-terminated arrays)
   function Pad (S : String; N : Positive) return String is
      R : String (1 .. N) := (others => Character'Val (0));
   begin
      R (1 .. S'Length) := S;
      return R;
   end Pad;

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

   --  encode V into a stream of Size octets: Bits bits, the bytes Expected; decode them back
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
   exception
      when others =>
         Fail (Name & ": exception");
   end Round_Trip;

   generic
      type T is private;
      Size : Positive;
      with procedure Decode
        (V : out T; S : in out adaasn1rtl.encoding.Bitstream; R : out ASN1_RESULT);
   procedure Reject (Name : String; Src : OctetBuffer; Err : Integer; Pos : Natural);

   --  decode Src (zero-padded) from exactly Size octets: FALSE, error code Err, end bit position Pos
   procedure Reject (Name : String; Src : OctetBuffer; Err : Integer; Pos : Natural) is
      S : adaasn1rtl.encoding.Bitstream (Size);
      V : T;
      R : ASN1_RESULT;
   begin
      S.Buffer := (others => 0);
      S.Buffer (1 .. Src'Length) := Src;
      S.Current_Bit_Pos := 0;
      Decode (V, S, R);
      if R.Success or else R.ErrorCode /= Err or else S.Current_Bit_Pos /= Pos then
         Fail (Name & ": decode" & Boolean'Image (R.Success) & Integer'Image (R.ErrorCode) &
               Integer'Image (S.Current_Bit_Pos));
      end if;
   exception
      when others =>
         Fail (Name & ": exception");
   end Reject;

   procedure U_U1 is new Round_Trip
     (U1, Positive'Max (1, U1_REQUIRED_BYTES_FOR_ENCODING), U1_REQUIRED_BITS_FOR_ENCODING, U1_Encode, U1_Decode, U1_Equal);
   procedure U_U2 is new Round_Trip
     (U2, Positive'Max (1, U2_REQUIRED_BYTES_FOR_ENCODING), U2_REQUIRED_BITS_FOR_ENCODING, U2_Encode, U2_Decode, U2_Equal);
   procedure U_V1 is new Round_Trip
     (V1, Positive'Max (1, V1_REQUIRED_BYTES_FOR_ENCODING), V1_REQUIRED_BITS_FOR_ENCODING, V1_Encode, V1_Decode, V1_Equal);
   procedure U_SU is new Round_Trip
     (SU, Positive'Max (1, SU_REQUIRED_BYTES_FOR_ENCODING), SU_REQUIRED_BITS_FOR_ENCODING, SU_Encode, SU_Decode, SU_Equal);
   procedure U_I1 is new Round_Trip
     (I1, Positive'Max (1, I1_REQUIRED_BYTES_FOR_ENCODING), I1_REQUIRED_BITS_FOR_ENCODING, I1_Encode, I1_Decode, I1_Equal);
   procedure U_I3 is new Round_Trip
     (I3, Positive'Max (1, I3_REQUIRED_BYTES_FOR_ENCODING), I3_REQUIRED_BITS_FOR_ENCODING, I3_Encode, I3_Decode, I3_Equal);
   procedure U_I4 is new Round_Trip
     (I4, Positive'Max (1, I4_REQUIRED_BYTES_FOR_ENCODING), I4_REQUIRED_BITS_FOR_ENCODING, I4_Encode, I4_Decode, I4_Equal);
   procedure U_I5 is new Round_Trip
     (I5, Positive'Max (1, I5_REQUIRED_BYTES_FOR_ENCODING), I5_REQUIRED_BITS_FOR_ENCODING, I5_Encode, I5_Decode, I5_Equal);
   procedure U_I3b is new Round_Trip
     (I3b, Positive'Max (1, I3b_REQUIRED_BYTES_FOR_ENCODING), I3b_REQUIRED_BITS_FOR_ENCODING, I3b_Encode, I3b_Decode, I3b_Equal);
   procedure U_B64 is new Round_Trip
     (B64, Positive'Max (1, B64_REQUIRED_BYTES_FOR_ENCODING), B64_REQUIRED_BITS_FOR_ENCODING, B64_Encode, B64_Decode, B64_Equal);
   procedure U_I8 is new Round_Trip
     (I8, Positive'Max (1, I8_REQUIRED_BYTES_FOR_ENCODING), I8_REQUIRED_BITS_FOR_ENCODING, I8_Encode, I8_Decode, I8_Equal);
   procedure U_P64 is new Round_Trip
     (P64, Positive'Max (1, P64_REQUIRED_BYTES_FOR_ENCODING), P64_REQUIRED_BITS_FOR_ENCODING, P64_Encode, P64_Decode, P64_Equal);
   procedure U_I2 is new Round_Trip
     (I2, Positive'Max (1, I2_REQUIRED_BYTES_FOR_ENCODING), I2_REQUIRED_BITS_FOR_ENCODING, I2_Encode, I2_Decode, I2_Equal);
   procedure U_I6 is new Round_Trip
     (I6, Positive'Max (1, I6_REQUIRED_BYTES_FOR_ENCODING), I6_REQUIRED_BITS_FOR_ENCODING, I6_Encode, I6_Decode, I6_Equal);
   procedure U_I7 is new Round_Trip
     (I7, Positive'Max (1, I7_REQUIRED_BYTES_FOR_ENCODING), I7_REQUIRED_BITS_FOR_ENCODING, I7_Encode, I7_Decode, I7_Equal);
   procedure U_X1 is new Round_Trip
     (X1, Positive'Max (1, X1_REQUIRED_BYTES_FOR_ENCODING), X1_REQUIRED_BITS_FOR_ENCODING, X1_Encode, X1_Decode, X1_Equal);
   procedure U_E3 is new Round_Trip
     (E3, Positive'Max (1, E3_REQUIRED_BYTES_FOR_ENCODING), E3_REQUIRED_BITS_FOR_ENCODING, E3_Encode, E3_Decode, E3_Equal);
   procedure U_C1 is new Round_Trip
     (C1, Positive'Max (1, C1_REQUIRED_BYTES_FOR_ENCODING), C1_REQUIRED_BITS_FOR_ENCODING, C1_Encode, C1_Decode, C1_Equal);
   procedure U_C2 is new Round_Trip
     (C2, Positive'Max (1, C2_REQUIRED_BYTES_FOR_ENCODING), C2_REQUIRED_BITS_FOR_ENCODING, C2_Encode, C2_Decode, C2_Equal);
   procedure U_S1 is new Round_Trip
     (S1, Positive'Max (1, S1_REQUIRED_BYTES_FOR_ENCODING), S1_REQUIRED_BITS_FOR_ENCODING, S1_Encode, S1_Decode, S1_Equal);
   procedure U_S3 is new Round_Trip
     (S3, Positive'Max (1, S3_REQUIRED_BYTES_FOR_ENCODING), S3_REQUIRED_BITS_FOR_ENCODING, S3_Encode, S3_Decode, S3_Equal);
   procedure U_S2 is new Round_Trip
     (S2, Positive'Max (1, S2_REQUIRED_BYTES_FOR_ENCODING), S2_REQUIRED_BITS_FOR_ENCODING, S2_Encode, S2_Decode, S2_Equal);
   procedure U_S8 is new Round_Trip
     (S8, Positive'Max (1, S8_REQUIRED_BYTES_FOR_ENCODING), S8_REQUIRED_BITS_FOR_ENCODING, S8_Encode, S8_Decode, S8_Equal);
   procedure U_N1 is new Round_Trip
     (N1, Positive'Max (1, N1_REQUIRED_BYTES_FOR_ENCODING), N1_REQUIRED_BITS_FOR_ENCODING, N1_Encode, N1_Decode, N1_Equal);
   procedure U_O1 is new Round_Trip
     (O1, Positive'Max (1, O1_REQUIRED_BYTES_FOR_ENCODING), O1_REQUIRED_BITS_FOR_ENCODING, O1_Encode, O1_Decode, O1_Equal);
   procedure U_O3 is new Round_Trip
     (O3, Positive'Max (1, O3_REQUIRED_BYTES_FOR_ENCODING), O3_REQUIRED_BITS_FOR_ENCODING, O3_Encode, O3_Decode, O3_Equal);
   procedure U_O2 is new Round_Trip
     (O2, Positive'Max (1, O2_REQUIRED_BYTES_FOR_ENCODING), O2_REQUIRED_BITS_FOR_ENCODING, O2_Encode, O2_Decode, O2_Equal);
   procedure U_BS1 is new Round_Trip
     (BS1, Positive'Max (1, BS1_REQUIRED_BYTES_FOR_ENCODING), BS1_REQUIRED_BITS_FOR_ENCODING, BS1_Encode, BS1_Decode, BS1_Equal);
   procedure U_BS3 is new Round_Trip
     (BS3, Positive'Max (1, BS3_REQUIRED_BYTES_FOR_ENCODING), BS3_REQUIRED_BITS_FOR_ENCODING, BS3_Encode, BS3_Decode, BS3_Equal);
   procedure U_Q2 is new Round_Trip
     (Q2, Positive'Max (1, Q2_REQUIRED_BYTES_FOR_ENCODING), Q2_REQUIRED_BITS_FOR_ENCODING, Q2_Encode, Q2_Decode, Q2_Equal);
   procedure U_Q3 is new Round_Trip
     (Q3, Positive'Max (1, Q3_REQUIRED_BYTES_FOR_ENCODING), Q3_REQUIRED_BITS_FOR_ENCODING, Q3_Encode, Q3_Decode, Q3_Equal);
   procedure A_R1 is new Round_Trip
     (R1, Positive'Max (1, R1_ACN_STREAM_SIZE), R1_REQUIRED_BITS_FOR_ACN_ENCODING, R1_ACN_Encode, R1_ACN_Decode, R1_Equal);
   procedure A_R2 is new Round_Trip
     (R2, Positive'Max (1, R2_ACN_STREAM_SIZE), R2_REQUIRED_BITS_FOR_ACN_ENCODING, R2_ACN_Encode, R2_ACN_Decode, R2_Equal);
   procedure R_I1 is new Reject (I1, I1_REQUIRED_BYTES_FOR_ENCODING, I1_Decode);
   procedure R_I3 is new Reject (I3, I3_REQUIRED_BYTES_FOR_ENCODING, I3_Decode);
   procedure R_I4 is new Reject (I4, I4_REQUIRED_BYTES_FOR_ENCODING, I4_Decode);
   procedure R_I5 is new Reject (I5, I5_REQUIRED_BYTES_FOR_ENCODING, I5_Decode);
   procedure R_B64 is new Reject (B64, B64_REQUIRED_BYTES_FOR_ENCODING, B64_Decode);
   procedure R_I8 is new Reject (I8, I8_REQUIRED_BYTES_FOR_ENCODING, I8_Decode);
   procedure R_U2 is new Reject (U2, U2_REQUIRED_BYTES_FOR_ENCODING, U2_Decode);
   procedure R_V1 is new Reject (V1, V1_REQUIRED_BYTES_FOR_ENCODING, V1_Decode);
   procedure R_E3 is new Reject (E3, E3_REQUIRED_BYTES_FOR_ENCODING, E3_Decode);
   procedure R_S3 is new Reject (S3, S3_REQUIRED_BYTES_FOR_ENCODING, S3_Decode);
   procedure R_O3 is new Reject (O3, O3_REQUIRED_BYTES_FOR_ENCODING, O3_Decode);
   procedure R_BS3 is new Reject (BS3, BS3_REQUIRED_BYTES_FOR_ENCODING, BS3_Decode);
   procedure R_Q3 is new Reject (Q3, Q3_REQUIRED_BYTES_FOR_ENCODING, Q3_Decode);

begin
   --  the reference chain: U0 ::= INTEGER (0..MAX), U1 ::= U0 (0..255) was semi-constrained (72 bits)
   U_U1 ("U1 0", 0, 8, OctetBuffer'(1 => 16#00#), 8);
   U_U1 ("U1 255", 255, 8, OctetBuffer'(1 => 16#FF#), 8);
   U_U2 ("U2 5", 5, 3, OctetBuffer'(1 => 16#00#), 3);
   U_U2 ("U2 10", 10, 3, OctetBuffer'(1 => 16#A0#), 3);
   U_V1 ("V1 5", 5, 3, OctetBuffer'(1 => 16#00#), 3);
   U_V1 ("V1 10", 10, 3, OctetBuffer'(1 => 16#A0#), 3);
   U_SU ("SU 0", SU'(f => 0, g => True), 9, OctetBuffer'(1 => 16#00#, 2 => 16#80#), 9);
   U_SU ("SU 255", SU'(f => 255, g => False), 9, OctetBuffer'(1 => 16#FF#, 2 => 16#00#), 9);
   --  MIN..m on lo..hi: lo..m (was m..hi)
   U_I1 ("I1 0", 0, 3, OctetBuffer'(1 => 16#00#), 3);
   U_I1 ("I1 3", 3, 3, OctetBuffer'(1 => 16#60#), 3);
   U_I1 ("I1 5", 5, 3, OctetBuffer'(1 => 16#A0#), 3);
   U_I3 ("I3 0", 0, 3, OctetBuffer'(1 => 16#00#), 3);
   U_I3 ("I3 5", 5, 3, OctetBuffer'(1 => 16#A0#), 3);
   U_I4 ("I4 0", 0, 3, OctetBuffer'(1 => 16#00#), 3);
   U_I4 ("I4 5", 5, 3, OctetBuffer'(1 => 16#A0#), 3);
   U_I5 ("I5 -10", I5'First, 3, OctetBuffer'(1 => 16#00#), 3);
   U_I5 ("I5 -5", I5'Last, 3, OctetBuffer'(1 => 16#A0#), 3);
   U_I3b ("I3b 0", 0, 0, OctetBuffer'(1 .. 0 => 0), 0);
   U_B64 ("B64 0", 0, 3, OctetBuffer'(1 => 16#00#), 3);
   U_B64 ("B64 5", 5, 3, OctetBuffer'(1 => 16#A0#), 3);
   U_I8 ("I8 0", 0, 2, OctetBuffer'(1 => 16#00#), 2);
   U_I8 ("I8 2", 2, 2, OctetBuffer'(1 => 16#80#), 2);
   --  n..MAX on lo..hi with n <= lo: lo..hi (was semi-constrained from n)
   U_P64 ("P64 0", 0, 64, OctetBuffer'(1 => 16#00#, 2 => 16#00#, 3 => 16#00#, 4 => 16#00#, 5 => 16#00#, 6 => 16#00#, 7 => 16#00#, 8 => 16#00#), 64);
   U_P64 ("P64 max", P64'Last, 64, OctetBuffer'(1 => 16#FF#, 2 => 16#FF#, 3 => 16#FF#, 4 => 16#FF#, 5 => 16#FF#, 6 => 16#FF#, 7 => 16#FF#, 8 => 16#FF#), 64);
   U_I2 ("I2 3", 3, 3, OctetBuffer'(1 => 16#00#), 3);
   U_I2 ("I2 10", 10, 3, OctetBuffer'(1 => 16#E0#), 3);
   U_I6 ("I6 5", 5, 3, OctetBuffer'(1 => 16#00#), 3);
   U_I6 ("I6 10", 10, 3, OctetBuffer'(1 => 16#A0#), 3);
   U_I7 ("I7 3", 3, 3, OctetBuffer'(1 => 16#00#), 3);
   U_I7 ("I7 10", 10, 3, OctetBuffer'(1 => 16#E0#), 3);
   U_X1 ("X1 5", 5, 0, OctetBuffer'(1 .. 0 => 0), 0);
   --  the root of an extensible constraint: extension bit + 0..5 (was 5..10)
   U_E3 ("E3 0", 0, 73, OctetBuffer'(1 => 16#00#), 4);
   U_E3 ("E3 5", 5, 73, OctetBuffer'(1 => 16#50#), 4);
   --  controls (unchanged)
   U_C1 ("C1 5", 5, 3, OctetBuffer'(1 => 16#00#), 3);
   U_C1 ("C1 10", 10, 3, OctetBuffer'(1 => 16#A0#), 3);
   U_C2 ("C2 0", 0, 4, OctetBuffer'(1 => 16#00#), 4);
   U_C2 ("C2 10", 10, 4, OctetBuffer'(1 => 16#A0#), 4);
   --  SIZE: MIN..m on SIZE(1..6) gives 1..m (was m..6); 0..MAX gives 1..6 (was "infinite size")
   U_S1 ("S1 a", Pad ("a", S1'Length), 15, OctetBuffer'(1 => 16#61#), 8);
   U_S1 ("S1 ab", Pad ("ab", S1'Length), 15, OctetBuffer'(1 => 16#E1#, 2 => 16#C4#), 15);
   U_S3 ("S3 a", Pad ("a", S3'Length), 23, OctetBuffer'(1 => 16#30#, 2 => 16#80#), 9);
   U_S3 ("S3 abc", Pad ("abc", S3'Length), 23, OctetBuffer'(1 => 16#B0#, 2 => 16#E2#, 3 => 16#C6#), 23);
   U_S2 ("S2 a", Pad ("a", S2'Length), 45, OctetBuffer'(1 => 16#18#, 2 => 16#40#), 10);
   U_S2 ("S2 abcdef", Pad ("abcdef", S2'Length), 45, OctetBuffer'(1 => 16#B8#, 2 => 16#71#, 3 => 16#63#, 4 => 16#C9#, 5 => 16#97#, 6 => 16#30#), 45);
   U_S8 ("S8 a", Pad ("a", S8'Length), 7, OctetBuffer'(1 => 16#C2#), 7);
   U_N1 ("N1 1", Pad ("1", N1'Length), 9, OctetBuffer'(1 => 16#10#), 5);
   U_N1 ("N1 12", Pad ("12", N1'Length), 9, OctetBuffer'(1 => 16#91#, 2 => 16#80#), 9);
   U_O1 ("O1 01", O1'(Length => 1, Data => (1 => 1, 2 => 2)), 17, OctetBuffer'(1 => 16#00#, 2 => 16#80#), 9);
   U_O1 ("O1 0102", O1'(Length => 2, Data => (1 => 1, 2 => 2)), 17, OctetBuffer'(1 => 16#80#, 2 => 16#81#, 3 => 16#00#), 17);
   U_O3 ("O3 01", O3'(Length => 1, Data => (1 => 1, 2 => 2, 3 => 3)), 26, OctetBuffer'(1 => 16#00#, 2 => 16#40#), 10);
   U_O3 ("O3 010203", O3'(Length => 3, Data => (1 => 1, 2 => 2, 3 => 3)), 26, OctetBuffer'(1 => 16#80#, 2 => 16#40#, 3 => 16#80#, 4 => 16#C0#), 26);
   U_O2 ("O2 01", O2'(Length => 1, Data => (1, 2, 3, 4, 5, 6)), 51, OctetBuffer'(1 => 16#00#, 2 => 16#20#), 11);
   U_O2 ("O2 0102030405", O2'(Length => 6, Data => (1, 2, 3, 4, 5, 6)), 51, OctetBuffer'(1 => 16#A0#, 2 => 16#20#, 3 => 16#40#, 4 => 16#60#, 5 => 16#80#, 6 => 16#A0#, 7 => 16#C0#), 51);
   U_BS1 ("BS1 1", BS1'(Length => 1, Data => (1 => 1, 2 => 0)), 3, OctetBuffer'(1 => 16#40#), 2);
   U_BS1 ("BS1 10", BS1'(Length => 2, Data => (1 => 1, 2 => 0)), 3, OctetBuffer'(1 => 16#C0#), 3);
   U_BS3 ("BS3 1", BS3'(Length => 1, Data => (1 => 1, 2 => 0, 3 => 1)), 5, OctetBuffer'(1 => 16#20#), 3);
   U_BS3 ("BS3 101", BS3'(Length => 3, Data => (1 => 1, 2 => 0, 3 => 1)), 5, OctetBuffer'(1 => 16#A8#), 5);
   U_Q2 ("Q2 T", Q2'(Length => 1, Data => (1 => True, others => False)), 3, OctetBuffer'(1 => 16#40#), 2);
   U_Q2 ("Q2 TF", Q2'(Length => 2, Data => (1 => True, others => False)), 3, OctetBuffer'(1 => 16#C0#), 3);
   U_Q3 ("Q3 T", Q3'(Length => 1, Data => (1 | 3 => True, others => False)), 5, OctetBuffer'(1 => 16#20#), 3);
   U_Q3 ("Q3 TFT", Q3'(Length => 3, Data => (1 | 3 => True, others => False)), 5, OctetBuffer'(1 => 16#A8#), 5);
   --  ACN scaled-integer REAL: R1 0.0..5.0, R2 3.0..10.0 in 8 bits
   A_R1 ("R1 0.0", 0.0, 8, OctetBuffer'(1 => 16#00#), 8);
   A_R1 ("R1 5.0", 5.0, 8, OctetBuffer'(1 => 16#FF#), 8);
   A_R2 ("R2 3.0", 3.0, 8, OctetBuffer'(1 => 16#00#), 8);
   A_R2 ("R2 10.0", 10.0, 8, OctetBuffer'(1 => 16#FF#), 8);
   --  rejected: a field above the new maximum
   R_I1 ("I1 6", OctetBuffer'(1 => 16#C0#), ERR_UPER_DECODE_I1, 3);
   R_I1 ("I1 7", OctetBuffer'(1 => 16#E0#), ERR_UPER_DECODE_I1, 3);
   R_I3 ("I3 6", OctetBuffer'(1 => 16#C0#), ERR_UPER_DECODE_I3, 3);
   R_I4 ("I4 7", OctetBuffer'(1 => 16#E0#), ERR_UPER_DECODE_I4, 3);
   R_I5 ("I5 6", OctetBuffer'(1 => 16#C0#), ERR_UPER_DECODE_I5, 3);
   R_B64 ("B64 7", OctetBuffer'(1 => 16#E0#), ERR_UPER_DECODE_B64, 3);
   R_I8 ("I8 3", OctetBuffer'(1 => 16#C0#), ERR_UPER_DECODE_I8, 2);
   R_U2 ("U2 6", OctetBuffer'(1 => 16#C0#), ERR_UPER_DECODE_U2, 3);
   R_V1 ("V1 7", OctetBuffer'(1 => 16#E0#), ERR_UPER_DECODE_V1, 3);
   R_E3 ("E3 6", OctetBuffer'(1 => 16#60#), ERR_UPER_DECODE_E3, 4);
   --  the Ada string decoders report ERR_INSUFFICIENT_DATA for a length outside the range (C: the type's code)
   R_S3 ("S3 len 4", OctetBuffer'(1 => 16#F0#, 2 => 16#E2#, 3 => 16#C6#), ERR_INSUFFICIENT_DATA, 2);
   R_O3 ("O3 len 4", OctetBuffer'(1 => 16#C0#, 2 => 16#40#, 3 => 16#80#, 4 => 16#C0#), ERR_UPER_DECODE_O3, 2);
   R_BS3 ("BS3 len 4", OctetBuffer'(1 => 16#F8#), ERR_UPER_DECODE_BS3, 2);
   R_Q3 ("Q3 len 4", OctetBuffer'(1 => 16#F8#), ERR_UPER_DECODE_Q3, 2);
end Wire_Test;
