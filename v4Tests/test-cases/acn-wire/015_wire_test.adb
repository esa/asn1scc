--  --acn-v2 checks of 015.asn1 / 015.acn in Ada (run by v4Tests/scripts/runWireTests.sh, with and without -gnata; Issue
--  141): the values and bytes of 015_wire_test.c in streams of the type's REQUIRED_BYTES (before the fix one octet too
--  small: the encoder's postcondition failed under -gnata, the encoding did not fit).
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with acn_wire_choice_region; use acn_wire_choice_region;
procedure Wire_Test is

   procedure Fail (Msg : String) is
   begin
      Ada.Text_IO.Put_Line ("FAILED: " & Msg);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Fail;

   generic
      type T is private;
      Size : Positive;
      with procedure Encode
        (V : T; S : out adaasn1rtl.encoding.Bitstream; R : out ASN1_RESULT);
      with procedure Decode
        (V : out T; S : in out adaasn1rtl.encoding.Bitstream; R : out ASN1_RESULT);
      with function Equal (V1, V2 : T) return Boolean;
   procedure Round_Trip (Name : String; V : T; Expected : OctetBuffer; Bits : Natural);

   procedure Round_Trip (Name : String; V : T; Expected : OctetBuffer; Bits : Natural) is
      S : adaasn1rtl.encoding.Bitstream (Size);
      D : T;
      R : ASN1_RESULT;
   begin
      if Size /= Expected'Length then
         Fail (Name & ": REQUIRED_BYTES");
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

   procedure RT_Doc is new Round_Trip
     (Doc, Doc_REQUIRED_BYTES_FOR_ACN_ENCODING, Doc_ACN_Encode, Doc_ACN_Decode, Doc_Equal);
   procedure RT_Doc13 is new Round_Trip
     (Doc13, Doc13_REQUIRED_BYTES_FOR_ACN_ENCODING, Doc13_ACN_Encode, Doc13_ACN_Decode, Doc13_Equal);
   procedure RT_DocL is new Round_Trip
     (DocL, DocL_REQUIRED_BYTES_FOR_ACN_ENCODING, DocL_ACN_Encode, DocL_ACN_Decode, DocL_Equal);

begin
   RT_Doc ("Doc", (pl => (kind => other_PRESENT, other => (number => 16#FFFF#)), trailer => 16#FF#, extra => True),
           (16#00#, 16#03#, 16#FF#, 16#FF#, 16#80#, 16#FF#, 16#80#), 49);
   RT_Doc13 ("Doc13", (pl => (kind => value_PRESENT, value => (number => 16#ABCD#)), trailer => 16#11#, extra => True),
             (16#00#, 16#03#, 16#2A#, 16#F3#, 16#40#, 16#11#, 16#80#), 49);
   RT_DocL ("DocL", (pl => (kind => other_PRESENT, other => (number => 16#FFFF#)), trailer => 16#FF#, extra => True),
            (16#00#, 16#12#, 16#7F#, 16#FF#, 16#FF#, 16#E0#), 43);
end Wire_Test;
