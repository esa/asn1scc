--  Issue 125 leftover (cycle i108a round 2, run by v4Tests/scripts/runWireTests.sh with -gnata, --acn-v2): an OPTIONAL
--  fixed-size consumer inside the elements of a callee's SEQUENCE OF. The same values as 011_v2_wire_test.c.
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl;
with adaasn1rtl.encoding;
with WIRE; use WIRE;
procedure Wire_Test is
   use type adaasn1rtl.Asn1Byte;

   type Bytes is array (Positive range <>) of adaasn1rtl.Asn1Byte;

   Failed : Boolean := False;

   procedure Fail (Msg : String) is
   begin
      Failed := True;
      Ada.Text_IO.Put_Line (Msg);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Fail;

   generic
      type T is private;
      Size : Positive;
      with procedure Encode (V : T; S : out adaasn1rtl.encoding.Bitstream; R : out adaasn1rtl.ASN1_RESULT);
      with procedure Decode (V : out T; S : in out adaasn1rtl.encoding.Bitstream; R : out adaasn1rtl.ASN1_RESULT);
      with function Equal (A, B : T) return Boolean;
   package Checks is
      --  encode V: success with error code 0, exactly the bytes Expected, and V back from a decode
      procedure Enc (Name : String; V : T; Expected : Bytes);
   end Checks;

   package body Checks is
      procedure Enc (Name : String; V : T; Expected : Bytes) is
         S : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (Size);
         D : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (Size);
         W : T;
         R : adaasn1rtl.ASN1_RESULT;
         Ok : Boolean;
      begin
         Encode (V, S, R);
         Ok := R.Success and then R.ErrorCode = 0 and then (S.Current_Bit_Pos + 7) / 8 = Expected'Length;
         if Ok then
            for I in Expected'Range loop
               Ok := Ok and then S.Buffer (I - Expected'First + 1) = Expected (I);
            end loop;
         end if;
         if not Ok then
            Fail (Name & ": unexpected encoding, Success=" & Boolean'Image (R.Success) & " ErrorCode="
                  & Integer'Image (R.ErrorCode));
            return;
         end if;
         D.Buffer := S.Buffer;
         Decode (W, D, R);
         if not R.Success or else not Equal (W, V) then
            Fail (Name & ": unexpected decoding");
         end if;
      exception
         when others => Fail (Name & ": exception");
      end Enc;
   end Checks;

   package CS17 is new Checks (S17, S17_ACN_STREAM_SIZE, S17_ACN_Encode, S17_ACN_Decode, S17_Equal);
   F3 : constant adaasn1rtl.OctetBuffer (1 .. 3) := (1, 2, 3);
   VS17 : S17 := S17_Init;
begin
   --  no element: n = 0; one element without x: n = 0 (was 3); one element with x: n = 3
   VS17.tail := 7;
   VS17.pl.items.Length := 0;
   CS17.Enc ("S17 no element", VS17, (16#00#, 16#1C#));
   VS17.pl.items.Length := 1;
   VS17.pl.items.Data (1).e := 5;
   VS17.pl.items.Data (1).Exist := (x => 0);
   CS17.Enc ("S17 element without x", VS17, (16#04#, 16#0A#, 16#0E#));
   VS17.pl.items.Data (1).Exist := (x => 1);
   VS17.pl.items.Data (1).x.Data := F3;
   CS17.Enc ("S17 element with x", VS17, (16#36#, 16#02#, 16#04#, 16#06#, 16#0A#, 16#0E#));
   if not Failed then
      Ada.Text_IO.Put_Line ("011_v2 wire test OK");
   end if;
end Wire_Test;
