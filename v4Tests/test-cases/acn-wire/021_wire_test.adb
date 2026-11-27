--  Checks of 021.asn1 / 021.acn in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, with and without
--  -gnata; Issue 208): the rows of 021_wire_test.c. Each value is encoded with the _aux encoder from bit 0 into a stream
--  of exactly REQUIRED_BYTES and from bit 3 into one of (3 + REQUIRED_BITS + 7) / 8 octets: success, end bit position
--  and bytes pinned (from bit 3 the bit-0 bytes shifted by 3), REQUIRED_BITS pinned; then decoded from the same stream
--  holding only the written octets (Equal, end position). Rejected rows: a region length above the new maximum,
--  decoded from exactly REQUIRED_BYTES: failure, the region's error code, the position after the length field.
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use type Interfaces.Unsigned_8;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with acn_wire_seqof_min; use acn_wire_seqof_min;
procedure Wire_Test is

   procedure Fail (Msg : String) is
   begin
      Ada.Text_IO.Put_Line ("FAILED: " & Msg);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Fail;

   --  Src (Bits bits) shifted right by Off bits, (Off + Bits + 7) / 8 octets
   function Shifted (Src : OctetBuffer; Bits : Natural; Off : Natural) return OctetBuffer is
      R : OctetBuffer (1 .. (Off + Bits + 7) / 8) := (others => 0);
   begin
      for I in 0 .. Bits - 1 loop
         if (Src (Src'First + I / 8) and Asn1Byte (2 ** (7 - I mod 8))) /= 0 then
            R (1 + (Off + I) / 8) := R (1 + (Off + I) / 8) or Asn1Byte (2 ** (7 - (Off + I) mod 8));
         end if;
      end loop;
      return R;
   end Shifted;

   generic
      type T is private;
      Required_Bits : Natural;
      with procedure Encode
        (V : T; S : in out adaasn1rtl.encoding.Bitstream; R : out ASN1_RESULT);
      with procedure Decode
        (V : in out T; S : in out adaasn1rtl.encoding.Bitstream; R : out ASN1_RESULT);
      with function Equal (V1, V2 : T) return Boolean;
      with function Init return T;
   package Rows is
      procedure Row (Name : String; V : T; Req_Bits : Natural; Expected : OctetBuffer; Bits : Natural);
      procedure Reject (Name : String; Src : OctetBuffer; Err : Integer; Pos : Natural);
   end Rows;

   package body Rows is

      procedure Round_Trip (Name : String; V : T; Expected : OctetBuffer; Bits : Natural; Off : Natural) is
         S : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init ((Off + Required_Bits + 7) / 8);
         E : constant OctetBuffer := Shifted (Expected, Bits, Off);
         D : T := Init;
         R : ASN1_RESULT;
      begin
         S.Current_Bit_Pos := Off;
         Encode (V, S, R);
         if not R.Success then
            Fail (Name & Natural'Image (Off) & ": encode");
            return;
         end if;
         if S.Current_Bit_Pos /= Off + Bits or else S.Buffer (1 .. E'Length) /= E then
            Fail (Name & Natural'Image (Off) & ": encoded bytes" & Natural'Image (S.Current_Bit_Pos));
         end if;
         S.Buffer := (others => 0);
         S.Buffer (1 .. E'Length) := E;
         S.Current_Bit_Pos := Off;
         Decode (D, S, R);
         if not R.Success or else not Equal (D, V) or else S.Current_Bit_Pos /= Off + Bits then
            Fail (Name & Natural'Image (Off) & ": decode");
         end if;
      exception
         when others =>
            Fail (Name & Natural'Image (Off) & ": exception");
      end Round_Trip;

      procedure Row (Name : String; V : T; Req_Bits : Natural; Expected : OctetBuffer; Bits : Natural) is
      begin
         if Required_Bits /= Req_Bits or else Bits > Required_Bits or else Expected'Length /= (Bits + 7) / 8 then
            Fail (Name & ": REQUIRED_BITS");
            return;
         end if;
         Round_Trip (Name, V, Expected, Bits, 0);
         Round_Trip (Name, V, Expected, Bits, 3);
      end Row;

      procedure Reject (Name : String; Src : OctetBuffer; Err : Integer; Pos : Natural) is
         S : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init ((Required_Bits + 7) / 8);
         V : T := Init;
         R : ASN1_RESULT;
      begin
         S.Buffer (1 .. Src'Length) := Src;
         Decode (V, S, R);
         if R.Success or else R.ErrorCode /= Err or else S.Current_Bit_Pos /= Pos then
            Fail (Name & ": decode" & Boolean'Image (R.Success) & Integer'Image (R.ErrorCode) &
                  Integer'Image (S.Current_Bit_Pos));
         end if;
      exception
         when others =>
            Fail (Name & ": exception");
      end Reject;

   end Rows;

   package P_R6 is new Rows (R6, R6_REQUIRED_BITS_FOR_ACN_ENCODING, R6_ACN_Encode_aux, R6_ACN_Decode_aux, R6_Equal, R6_Init);
   package P_R5 is new Rows (R5, R5_REQUIRED_BITS_FOR_ACN_ENCODING, R5_ACN_Encode_aux, R5_ACN_Decode_aux, R5_Equal, R5_Init);
   package P_B1 is new Rows (B1, B1_REQUIRED_BITS_FOR_ACN_ENCODING, B1_ACN_Encode_aux, B1_ACN_Decode_aux, B1_Equal, B1_Init);
   package P_N1 is new Rows (N1, N1_REQUIRED_BITS_FOR_ACN_ENCODING, N1_ACN_Encode_aux, N1_ACN_Decode_aux, N1_Equal, N1_Init);
   package P_E3 is new Rows (E3, E3_REQUIRED_BITS_FOR_ACN_ENCODING, E3_ACN_Encode_aux, E3_ACN_Decode_aux, E3_Equal, E3_Init);

   Ia1 : constant Elem := (kind => ia_PRESENT, ia => 1);
   Ia2 : constant Elem := (kind => ia_PRESENT, ia => 2);
   Ia3 : constant Elem := (kind => ia_PRESENT, ia => 3);
   Ib5 : constant Elem := (kind => ib_PRESENT, ib => 5);
   Oc1 : constant Elem := (kind => oc_PRESENT, oc => (Length => 2, Data => (16#12#, 16#34#)));
   Oc2 : constant Elem := (kind => oc_PRESENT, oc => (Length => 2, Data => (16#56#, 16#78#)));

   function Sa (V : Sm_sa) return Sm is (kind => sa_PRESENT, sa => V);
   function Sb (V : Sm_sb) return Sm is (kind => sb_PRESENT, sb => V);

begin
   --  R6: OCTET STRING (CONTAINING Lst): region 1..6 octets, 3-bit length field (was 3..6, 2 bits)
   P_R6.Row ("R6 ia1", (o => (Length => 1, Data => (Ia1, Ia1)), z => 16#AA#), 59,
             OctetBuffer'(16#01#, 16#15#, 16#40#), 19);
   P_R6.Row ("R6 ib5 ia3", (o => (Length => 2, Data => (Ib5, Ia3)), z => 16#55#), 59,
             OctetBuffer'(16#34#, 16#29#, 16#8A#, 16#A0#), 27);
   P_R6.Row ("R6 max", (o => (Length => 2, Data => (Oc1, Oc2)), z => 16#FF#), 59,
             OctetBuffer'(16#BA#, 16#12#, 16#34#, 16#A5#, 16#67#, 16#80#, 16#1F#, 16#E0#), 59);
   --  R5: OCTET STRING (CONTAINING L2): 3-bit length field + 1..5 octets (was a FIXED 5-octet region)
   P_R5.Row ("R5 ia1 ia2", (o => (Data => (Ia1, Ia2)), z => 16#AA#), 51,
             OctetBuffer'(16#02#, 16#55#, 16#40#), 19);
   P_R5.Row ("R5 ib5 ia3", (o => (Data => (Ib5, Ia3)), z => 16#55#), 51,
             OctetBuffer'(16#28#, 16#53#, 16#0A#, 16#A0#), 27);
   P_R5.Row ("R5 max", (o => (Data => (Oc1, Oc2)), z => 16#FF#), 51,
             OctetBuffer'(16#94#, 16#24#, 16#69#, 16#4A#, 16#CF#, 16#1F#, 16#E0#), 51);
   --  B1: BIT STRING (CONTAINING Lst): 6-bit length of 5..41 bits (was 21..41, 5 bits)
   P_B1.Row ("B1 ia1", (o => (Length => 1, Data => (Ia1, Ia1)), z => 16#AA#), 55,
             OctetBuffer'(16#00#, 16#35#, 16#40#), 19);
   P_B1.Row ("B1 ib5 ia3", (o => (Length => 2, Data => (Ib5, Ia3)), z => 16#55#), 55,
             OctetBuffer'(16#26#, 16#85#, 16#35#, 16#50#), 28);
   P_B1.Row ("B1 max", (o => (Length => 2, Data => (Oc1, Oc2)), z => 16#FF#), 55,
             OctetBuffer'(16#93#, 16#42#, 16#46#, 16#94#, 16#AC#, 16#F1#, 16#FE#), 55);
   --  N1: OCTET STRING (CONTAINING S): 2-bit length field + 2..5 octets (was FIXED 5 octets)
   P_N1.Row ("N1 min", (o => (h => True, ll => (Data => ((Length => 1, Data => (Sa (0), Sa (0))),
                                                         (Length => 1, Data => (Sa (1), Sa (0)))))), z => 16#AA#), 50,
             OctetBuffer'(16#20#, 16#20#, 16#2A#, 16#80#), 26);
   P_N1.Row ("N1 max", (o => (h => True, ll => (Data => ((Length => 2, Data => (Sb (100), Sb (101))),
                                                         (Length => 2, Data => (Sb (100), Sb (101)))))), z => 16#55#), 50,
             OctetBuffer'(16#FE#, 16#4E#, 16#5F#, 16#27#, 16#28#, 16#15#, 16#40#), 50);
   --  E3: external size n (octets) of OCTET STRING (CONTAINING S3): padded to whole octets (= legacy)
   P_E3.Row ("E3 ia1 ia2", (o => (l => (Data => (Ia1, Ia2))), z => 16#AA#), 56,
             OctetBuffer'(16#01#, 16#12#, 16#AA#), 24);
   P_E3.Row ("E3 ib5 ia3", (o => (l => (Data => (Ib5, Ia3))), z => 16#55#), 56,
             OctetBuffer'(16#02#, 16#42#, 16#98#, 16#55#), 32);
   P_E3.Row ("E3 max", (o => (l => (Data => (Oc1, Oc2))), z => 16#FF#), 56,
             OctetBuffer'(16#05#, 16#A1#, 16#23#, 16#4A#, 16#56#, 16#78#, 16#FF#), 56);
   --  rejected: region lengths above the new maximum, after the length field
   P_R6.Reject ("R6 len 7", OctetBuffer'(1 => 16#C0#), ERR_ACN_DECODE_R6_O_2, 3);
   P_R6.Reject ("R6 len 8", OctetBuffer'(1 => 16#E0#), ERR_ACN_DECODE_R6_O_2, 3);
   P_R5.Reject ("R5 len 6", OctetBuffer'(1 => 16#A0#), ERR_ACN_DECODE_R5_O_2, 3);
   P_B1.Reject ("B1 len 68", OctetBuffer'(1 => 16#FC#), ERR_ACN_DECODE_B1_O_2, 6);
end Wire_Test;
