--  --acn-v2 checks of 013.asn1 / 013.acn in Ada (run by v4Tests/scripts/runWireTests.sh, with and without -gnata;
--  Issue 135): the values and bytes of 013_wire_test.c. Each value is encoded (bytes and length in bits compared with
--  the expected ones) and the expected bytes are decoded (equal value, stopped at the same bit). Streams of the type's
--  REQUIRED_BYTES, zeros after the given bytes.
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with acn_wire_contents; use acn_wire_contents;
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

   procedure RT_FixO is new Round_Trip
     (FixO, FixO_REQUIRED_BYTES_FOR_ACN_ENCODING, FixO_ACN_Encode, FixO_ACN_Decode, FixO_Equal);
   procedure RT_EmbO is new Round_Trip
     (EmbO, EmbO_REQUIRED_BYTES_FOR_ACN_ENCODING, EmbO_ACN_Encode, EmbO_ACN_Decode, EmbO_Equal);
   procedure RT_FixB is new Round_Trip
     (FixB, FixB_REQUIRED_BYTES_FOR_ACN_ENCODING, FixB_ACN_Encode, FixB_ACN_Decode, FixB_Equal);
   procedure RT_EmbB is new Round_Trip
     (EmbB, EmbB_REQUIRED_BYTES_FOR_ACN_ENCODING, EmbB_ACN_Encode, EmbB_ACN_Decode, EmbB_Equal);
   procedure RT_Eq is new Round_Trip
     (Eq, Eq_REQUIRED_BYTES_FOR_ACN_ENCODING, Eq_ACN_Encode, Eq_ACN_Decode, Eq_Equal);
   procedure RT_Tas is new Round_Trip
     (Tas, Tas_REQUIRED_BYTES_FOR_ACN_ENCODING, Tas_ACN_Encode, Tas_ACN_Decode, Tas_Equal);
   procedure RT_Ctl is new Round_Trip
     (Ctl, Ctl_REQUIRED_BYTES_FOR_ACN_ENCODING, Ctl_ACN_Encode, Ctl_ACN_Decode, Ctl_Equal);
   procedure RT_Ext is new Round_Trip
     (Ext, Ext_REQUIRED_BYTES_FOR_ACN_ENCODING, Ext_ACN_Encode, Ext_ACN_Decode, Ext_Equal);

begin
   RT_FixO ("FixO", (f => (a => 5, b => True), z => True),
            (16#00#, 16#00#, 16#00#, 16#05#, 16#80#, 16#80#), 41);
   RT_EmbO ("EmbO c absent", (f => (a => 5, c => 0, Exist => (c => 0)), z => True),
            (16#00#, 16#00#, 16#00#, 16#00#, 16#A0#, 16#20#), 43);
   RT_EmbO ("EmbO c present", (f => (a => 16#AB#, c => 16#1234#, Exist => (c => 1)), z => False),
            (16#A0#, 16#00#, 16#00#, 16#15#, 16#62#, 16#46#, 16#80#, 16#00#), 59);
   RT_FixB ("FixB", (f => (a => 5, b => True), z => True),
            (16#00#, 16#00#, 16#00#, 16#05#, 16#C0#), 34);
   RT_EmbB ("EmbB b absent", (f => (a => 5, b => False, Exist => (b => 0)), z => True),
            (16#00#, 16#00#, 16#00#, 16#01#, 16#60#), 35);
   RT_EmbB ("EmbB b present", (f => (a => 5, b => True, Exist => (b => 1)), z => False),
            (16#C0#, 16#00#, 16#00#, 16#01#, 16#60#), 36);
   RT_Eq ("Eq", (f => (a => 16#AB#, b => True), g => (a => 16#CD#, b => False), z => True),
          (16#AB#, 16#00#, 16#80#, 16#CD#, 16#00#, 16#40#), 42);
   RT_Tas ("Tas", (x => (a => 5, b => True, Exist => (b => 1)), y => (a => 6, b => False, Exist => (b => 0)),
                   z => True),
           (16#80#, 16#00#, 16#00#, 16#02#, 16#C0#, 16#00#, 16#00#, 16#00#, 16#01#, 16#A0#), 75);
   RT_Ctl ("Ctl", (f => (a => 5, b => True, Exist => (b => 1)), z => True, Exist => (f => 1)),
           (16#C1#, 16#60#, 16#40#), 18);
   RT_Ext ("Ext", (f => (a => 5, b => True, Exist => (b => 1)), z => True),
           (16#05#, 16#80#, 16#00#, 16#00#, 16#02#, 16#C0#, 16#80#), 49);
end Wire_Test;
