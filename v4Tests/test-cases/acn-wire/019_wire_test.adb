--  Checks of 019.asn1 / 019.acn in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, with and without
--  -gnata; Issue 173): the values, bytes and the rejected row of 019_wire_test.c, ACN, in streams of exactly the type's
--  REQUIRED_BYTES_FOR_ACN, REQUIRED_BITS pinned. Before the fix U1p / U2p / W4 did not compile, U1 was semi-constrained
--  and the site length fields of W3 / WC / WA had the range 2..6 / 1..6.
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with acn_wire_range; use acn_wire_range;
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

   --  the same string value: the generated Equal compares the whole typedef array (InX_s, 7 characters), while the
   --  decoder of an ASCII string narrowed at a site writes only characters 1 .. max + 1 and leaves the rest as the
   --  decoder found it (an open defect of narrowed sites, also with a value set such as s ("ab" | "a"))
   function Same (A, B : InX) return Boolean is
     (getStringSize (A.s) = getStringSize (B.s)
      and then A.s (1 .. getStringSize (A.s)) = B.s (1 .. getStringSize (B.s)) and then A.b = B.b);
   function WA_Same (A, B : WA) return Boolean is (Same (A.x, B.x));
   function WC_Same (A, B : WC) return Boolean is (Same (A.x, B.x));

   procedure A_U1 is new Round_Trip
     (U1, U1_ACN_STREAM_SIZE, U1_REQUIRED_BITS_FOR_ACN_ENCODING, U1_ACN_Encode, U1_ACN_Decode, U1_Equal);
   procedure A_I1 is new Round_Trip
     (I1, I1_ACN_STREAM_SIZE, I1_REQUIRED_BITS_FOR_ACN_ENCODING, I1_ACN_Encode, I1_ACN_Decode, I1_Equal);
   procedure A_I2 is new Round_Trip
     (I2, I2_ACN_STREAM_SIZE, I2_REQUIRED_BITS_FOR_ACN_ENCODING, I2_ACN_Encode, I2_ACN_Decode, I2_Equal);
   procedure A_S1 is new Round_Trip
     (S1, S1_ACN_STREAM_SIZE, S1_REQUIRED_BITS_FOR_ACN_ENCODING, S1_ACN_Encode, S1_ACN_Decode, S1_Equal);
   procedure A_O1 is new Round_Trip
     (O1, O1_ACN_STREAM_SIZE, O1_REQUIRED_BITS_FOR_ACN_ENCODING, O1_ACN_Encode, O1_ACN_Decode, O1_Equal);
   procedure A_U1p is new Round_Trip
     (U1p, U1p_ACN_STREAM_SIZE, U1p_REQUIRED_BITS_FOR_ACN_ENCODING, U1p_ACN_Encode, U1p_ACN_Decode, U1p_Equal);
   procedure A_U2p is new Round_Trip
     (U2p, U2p_ACN_STREAM_SIZE, U2p_REQUIRED_BITS_FOR_ACN_ENCODING, U2p_ACN_Encode, U2p_ACN_Decode, U2p_Equal);
   procedure A_W3 is new Round_Trip
     (W3, W3_ACN_STREAM_SIZE, W3_REQUIRED_BITS_FOR_ACN_ENCODING, W3_ACN_Encode, W3_ACN_Decode, W3_Equal);
   procedure A_W4 is new Round_Trip
     (W4, W4_ACN_STREAM_SIZE, W4_REQUIRED_BITS_FOR_ACN_ENCODING, W4_ACN_Encode, W4_ACN_Decode, W4_Equal);
   procedure A_W1 is new Round_Trip
     (W1, W1_ACN_STREAM_SIZE, W1_REQUIRED_BITS_FOR_ACN_ENCODING, W1_ACN_Encode, W1_ACN_Decode, W1_Equal);
   procedure A_WA is new Round_Trip
     (WA, WA_ACN_STREAM_SIZE, WA_REQUIRED_BITS_FOR_ACN_ENCODING, WA_ACN_Encode, WA_ACN_Decode, WA_Same);
   procedure A_WC is new Round_Trip
     (WC, WC_ACN_STREAM_SIZE, WC_REQUIRED_BITS_FOR_ACN_ENCODING, WC_ACN_Encode, WC_ACN_Decode, WC_Same);
   procedure R_W1 is new Reject (W1, W1_ACN_STREAM_SIZE, W1_ACN_Decode);

begin
   --  default ACN = the uPER body: the reference chain U1 ::= U0 (0..255) (was 72 bits) and the A / B shapes
   A_U1 ("U1 0", 0, 8, OctetBuffer'(1 => 16#00#), 8);
   A_U1 ("U1 255", 255, 8, OctetBuffer'(1 => 16#FF#), 8);
   A_I1 ("I1 0", 0, 3, OctetBuffer'(1 => 16#00#), 3);
   A_I1 ("I1 5", 5, 3, OctetBuffer'(1 => 16#A0#), 3);
   A_I2 ("I2 3", 3, 3, OctetBuffer'(1 => 16#00#), 3);
   A_I2 ("I2 10", 10, 3, OctetBuffer'(1 => 16#E0#), 3);
   A_S1 ("S1 a", Pad ("a", S1'Length), 15, OctetBuffer'(1 => 16#61#), 8);
   A_S1 ("S1 ab", Pad ("ab", S1'Length), 15, OctetBuffer'(1 => 16#E1#, 2 => 16#C4#), 15);
   A_O1 ("O1 01", O1'(Length => 1, Data => (1 => 1, 2 => 2)), 17, OctetBuffer'(1 => 16#00#, 2 => 16#80#), 9);
   A_O1 ("O1 0102", O1'(Length => 2, Data => (1 => 1, 2 => 2)), 17, OctetBuffer'(1 => 16#80#, 2 => 16#81#, 3 => 16#00#), 17);
   --  explicit ACN specs that fit the real range (rejected before)
   A_U1p ("U1p 0", 0, 8, OctetBuffer'(1 => 16#00#), 8);
   A_U1p ("U1p 255", 255, 8, OctetBuffer'(1 => 16#FF#), 8);
   A_U2p ("U2p 5", 5, 8, OctetBuffer'(1 => 16#05#), 8);
   A_U2p ("U2p 10", 10, 8, OctetBuffer'(1 => 16#0A#), 8);
   --  WITH COMPONENTS sites: OCTET STRING / SEQUENCE OF / BIT STRING (SIZE(MIN..2)): 1-bit length fields
   A_W3 ("W3 min", W3'(x => (o => (Length => 1, Data => (1, 2, 3, 4, 5, 6)), q => (Length => 1, Data => (True, False, True, False, True, False)), b => (Length => 1, Data => (1, 0, 1, 0, 1, 0)))), 23, OctetBuffer'(1 => 16#00#, 2 => 16#A8#), 13);
   A_W3 ("W3 max", W3'(x => (o => (Length => 2, Data => (1, 2, 3, 4, 5, 6)), q => (Length => 2, Data => (True, False, True, False, True, False)), b => (Length => 2, Data => (1, 0, 1, 0, 1, 0)))), 23, OctetBuffer'(1 => 16#80#, 2 => 16#81#, 3 => 16#6C#), 23);
   --  SIZE(0..MAX) / SIZE(1..MAX) at a site: the base's 1..6 (3-bit length fields)
   A_W4 ("W4 min", W4'(x => (o => (Length => 1, Data => (1, 2, 3, 4, 5, 6)), q => (Length => 1, Data => (True, False, True, False, True, False)), b => (Length => 1, Data => (1, 0, 1, 0, 1, 0)))), 69, OctetBuffer'(1 => 16#00#, 2 => 16#22#, 3 => 16#20#), 19);
   A_W4 ("W4 max", W4'(x => (o => (Length => 6, Data => (1, 2, 3, 4, 5, 6)), q => (Length => 6, Data => (True, False, True, False, True, False)), b => (Length => 6, Data => (1, 0, 1, 0, 1, 0)))), 69, OctetBuffer'(1 => 16#A0#, 2 => 16#20#, 3 => 16#40#, 4 => 16#60#, 5 => 16#80#, 6 => 16#A0#, 7 => 16#D6#, 8 => 16#AB#, 9 => 16#50#), 69);
   --  a default-encoding string narrowed at a site: the uPER length field (1..6) stays, REQUIRED_BITS 22 (was 50)
   A_W1 ("W1 a", W1'(x => (s => Pad ("a", InA_s'Length), i => 3, b => True)), 22, OctetBuffer'(1 => 16#18#, 2 => 16#4E#), 15);
   A_W1 ("W1 ab", W1'(x => (s => Pad ("ab", InA_s'Length), i => 5, b => True)), 22, OctetBuffer'(1 => 16#38#, 2 => 16#71#, 3 => 16#2C#), 22);
   --  an ASCII string with an internal length narrowed at a site: SIZE(MIN..1) 0 bits, SIZE(MIN..2) 1 bit
   A_WA ("WA a", WA'(x => (s => Pad ("a", InX_s'Length), b => True)), 9, OctetBuffer'(1 => 16#61#, 2 => 16#80#), 9);
   A_WC ("WC a", WC'(x => (s => Pad ("a", InX_s'Length), b => False)), 18, OctetBuffer'(1 => 16#30#, 2 => 16#80#), 10);
   A_WC ("WC ab", WC'(x => (s => Pad ("ab", InX_s'Length), b => True)), 18, OctetBuffer'(1 => 16#B0#, 2 => 16#B1#, 3 => 16#40#), 18);
   --  rejected: W1 length 3 > 2, after the 3-bit length field
   R_W1 ("W1 len 3", OctetBuffer'(1 => 16#58#, 2 => 16#71#, 3 => 16#60#), ERR_ACN_DECODE_W1_X_S, 3);
end Wire_Test;
