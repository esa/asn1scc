--  Checks of 014.asn1 / 014.acn in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, with and without
--  -gnata; Issue 138): the values and bytes of 014_wire_test.c, encoded (bytes and length in bits) and decoded.
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with acn_wire_callee; use acn_wire_callee;
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

   procedure RT_Q12 is new Round_Trip
     (Q12, Q12_REQUIRED_BYTES_FOR_ACN_ENCODING, Q12_ACN_Encode, Q12_ACN_Decode, Q12_Equal);
   procedure RT_Q9 is new Round_Trip
     (Q9, Q9_REQUIRED_BYTES_FOR_ACN_ENCODING, Q9_ACN_Encode, Q9_ACN_Decode, Q9_Equal);

begin
   RT_Q12 ("Q12", (f => 5, g => 5, z => True), (16#00#, 16#00#, 16#05#, 16#00#, 16#00#, 16#05#, 16#80#), 49);
   RT_Q9 ("Q9", (f => (r => 16#1234#, k => 2), z => True), (16#12#, 16#34#, 16#40#, 16#80#), 25);
end Wire_Test;
