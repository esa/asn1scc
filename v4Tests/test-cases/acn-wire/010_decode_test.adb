--  ACN checks of 010.asn1 / 010.acn in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, -gnata;
--  Issue 132), with 010_choice.asn1 / 010_choice.acn under --acn-v2: the streams and values of 010_decode_test.c. A
--  CONTAINING region longer than the largest encoding of its contents, and BIT STRING contents that end past their
--  region, must be rejected with a nonzero error code (they were accepted: the fields after an over-long region were
--  read past the end of the stream as zeros, the missing bits of the contents as zeros); the maximal values round trip
--  and stop at their encoded length. Streams of the type's REQUIRED_BYTES, zeros after the given bytes. Built with
--  -gnateDACN_V2=True for --acn-v2, False for legacy (GNAT integrated preprocessor).
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with acn_wire_region; use acn_wire_region;
#if ACN_V2 then
with acn_wire_region_choice;
#end if;
procedure Decode_Test is

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
   package Checks is
      --  encode V, decode it back: equal value, stopped at the encoded length
      procedure Round_Trip (Name : String; V : T);
      --  decode the given bytes (zeros after them): failure with a nonzero error code
      procedure Reject (Name : String; Bytes : OctetBuffer);
   end Checks;

   package body Checks is
      procedure Round_Trip (Name : String; V : T) is
         S : adaasn1rtl.encoding.Bitstream (Size);
         D : T;
         R : ASN1_RESULT;
         Last : Natural;
      begin
         Encode (V, S, R);
         if not R.Success then
            Fail (Name & ": encode");
            return;
         end if;
         Last := S.Current_Bit_Pos;
         S.Current_Bit_Pos := 0;
         Decode (D, S, R);
         if not R.Success or else not Equal (D, V) or else S.Current_Bit_Pos /= Last then
            Fail (Name & ": round trip");
         end if;
      end Round_Trip;

      procedure Reject (Name : String; Bytes : OctetBuffer) is
         S : adaasn1rtl.encoding.Bitstream (Size);
         D : T;
         R : ASN1_RESULT;
      begin
         S.Buffer := (others => 0);
         S.Buffer (1 .. Bytes'Length) := Bytes;
         S.Current_Bit_Pos := 0;
         S.End_Bit_Pos := Size * 8;
         Decode (D, S, R);
         if R.Success or else R.ErrorCode = 0 then
            Fail (Name & ": accepted");
         end if;
      end Reject;
   end Checks;

   package C_BBox is new Checks
     (BBox, BBox_REQUIRED_BYTES_FOR_ACN_ENCODING, BBox_ACN_Encode, BBox_ACN_Decode, BBox_Equal);
   package C_OBox is new Checks
     (OBox, OBox_REQUIRED_BYTES_FOR_ACN_ENCODING, OBox_ACN_Encode, OBox_ACN_Decode, OBox_Equal);
   package C_PBitEmb is new Checks
     (PBitEmb, PBitEmb_REQUIRED_BYTES_FOR_ACN_ENCODING, PBitEmb_ACN_Encode, PBitEmb_ACN_Decode, PBitEmb_Equal);
   package C_PBitExt is new Checks
     (PBitExt, PBitExt_REQUIRED_BYTES_FOR_ACN_ENCODING, PBitExt_ACN_Encode, PBitExt_ACN_Decode, PBitExt_Equal);
   package C_POctExt is new Checks
     (POctExt, POctExt_REQUIRED_BYTES_FOR_ACN_ENCODING, POctExt_ACN_Encode, POctExt_ACN_Decode, POctExt_Equal);
   package C_PBitExt2 is new Checks
     (PBitExt2, PBitExt2_REQUIRED_BYTES_FOR_ACN_ENCODING, PBitExt2_ACN_Encode, PBitExt2_ACN_Decode, PBitExt2_Equal);

   --  T: presence bits of b and c, a (8 bits), b (1 bit), c (8 bits): 10 .. 19 bits; Inner: 10 bits
   Max_T : constant T := (a => 16#AB#, b => True, c => 16#CD#, Exist => (b => 1, c => 1));
   Max_Inner : constant Inner := (a => 16#5A#, b => 3);
begin
   C_BBox.Round_Trip ("BBox", Max_T);
   --  length 0 (10 bits): presence bits say c present, the contents need 18 bits
   C_BBox.Reject ("BBox 04", (16#04#, 16#00#, 16#00#));
   --  length 0 (10 bits): b present, the contents need 11 bits
   C_BBox.Reject ("BBox 08", (16#08#, 16#00#, 16#00#));
   C_OBox.Round_Trip ("OBox", Max_T);
   C_PBitEmb.Round_Trip ("PBitEmb", (f => Max_T, tail => 9));
   C_PBitEmb.Reject ("PBitEmb 08", (16#08#, 16#00#, 16#00#, 16#00#));
   C_PBitExt.Round_Trip ("PBitExt", (f => Max_T, tail => 9));
   --  a region of 21 bits (> 19): it ends past the stream with the tail (success past the end)
   C_PBitExt.Reject ("PBitExt 15", (16#15#, 16#00#, 16#00#, 16#00#));
   --  a region of 20 bits (> 19), the tail still inside the stream
   C_PBitExt.Reject ("PBitExt 14", (16#14#, 16#00#, 16#00#, 16#00#));
   --  a region of 9 bits holding contents of 10
   C_PBitExt.Reject ("PBitExt 09", (16#09#, 16#00#, 16#00#, 16#00#));
   C_POctExt.Round_Trip ("POctExt", (v => Max_Inner, tail => 9));
   --  a region of 3 octets (> 2): the tail past the end of the stream
   C_POctExt.Reject ("POctExt 03", (16#03#, 16#00#, 16#00#, 16#00#));
   C_PBitExt2.Round_Trip ("PBitExt2", (v => Max_Inner, tail => 9));
   C_PBitExt2.Reject ("PBitExt2 0D", (16#0D#, 16#00#, 16#00#));  --  13 bits, the tail past the end of the stream
   C_PBitExt2.Reject ("PBitExt2 0B", (16#0B#, 16#00#, 16#00#));  --  11 bits, the tail inside the stream
   C_PBitExt2.Reject ("PBitExt2 09", (16#09#, 16#00#, 16#00#));  --  9 bits holding contents of 10
#if ACN_V2 then
   declare
      package Ch renames acn_wire_region_choice;
      package C_Doc1 is new Checks
        (Ch.Doc1, Ch.Doc1_REQUIRED_BYTES_FOR_ACN_ENCODING, Ch.Doc1_ACN_Encode, Ch.Doc1_ACN_Decode, Ch.Doc1_Equal);
      package C_Doc2B is new Checks
        (Ch.Doc2B, Ch.Doc2B_REQUIRED_BYTES_FOR_ACN_ENCODING, Ch.Doc2B_ACN_Encode, Ch.Doc2B_ACN_Decode,
         Ch.Doc2B_Equal);
   begin
      --  the region around Payload1 holds up to 4 octets (big), its CONTAINING alternatives 1
      C_Doc1.Round_Trip ("Doc1 big", (payload => (kind => Ch.big_PRESENT, big => 16#DEADBEEF#), trailer => 16#55#));
      C_Doc1.Round_Trip
        ("Doc1 other", (payload => (kind => Ch.other_PRESENT, other => (number => 7)), trailer => 16#55#));
      C_Doc1.Reject ("Doc1 region of 5 octets", (16#00#, 16#05#, 16#03#, 16#00#));
      --  the region around Payload2B holds 17 bits: its index and the contents (16 bits)
      C_Doc2B.Round_Trip
        ("Doc2B", (payload => (kind => Ch.Payload2B_selection'(Ch.other_PRESENT), other => (number => 16#1234#)),
                   trailer => 16#55#));
      C_Doc2B.Reject ("Doc2B region of 18 bits", (16#00#, 16#12#, 16#00#));
      C_Doc2B.Reject ("Doc2B region of 25 bits", (16#00#, 16#19#, 16#00#));
   end;
#end if;
end Decode_Test;
