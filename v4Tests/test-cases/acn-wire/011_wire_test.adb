--  Issues 123 / 125 / 142 (cycle i108a, run by v4Tests/scripts/runWireTests.sh with -gnata, legacy and --acn-v2): a
--  determinant field whose consumers are all absent takes its default value, the same bytes in both modes, with
--  ErrorCode 0 (--acn-v2; Wire_Mode, written by the runner), and decodes back. The same values as 011_wire_test.c; see
--  011.asn1.
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl;
with adaasn1rtl.encoding;
with WIRE; use WIRE;
with Wire_Mode;
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
         --  legacy Ada MultiAcnUpdate leaves its error code in ErrorCode on success (master too): checked in --acn-v2 only
         Ok := R.Success and then (Wire_Mode.Legacy or else R.ErrorCode = 0)
               and then (S.Current_Bit_Pos + 7) / 8 = Expected'Length;
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

   package CS7 is new Checks (S7, S7_ACN_STREAM_SIZE, S7_ACN_Encode, S7_ACN_Decode, S7_Equal);
   package CS8 is new Checks (S8, S8_ACN_STREAM_SIZE, S8_ACN_Encode, S8_ACN_Decode, S8_Equal);
   package CS4 is new Checks (S4, S4_ACN_STREAM_SIZE, S4_ACN_Encode, S4_ACN_Decode, S4_Equal);
   package CS9 is new Checks (S9, S9_ACN_STREAM_SIZE, S9_ACN_Encode, S9_ACN_Decode, S9_Equal);
   package CS12 is new Checks (S12, S12_ACN_STREAM_SIZE, S12_ACN_Encode, S12_ACN_Decode, S12_Equal);
   package CS10 is new Checks (S10, S10_ACN_STREAM_SIZE, S10_ACN_Encode, S10_ACN_Decode, S10_Equal);
   package CS13 is new Checks (S13, S13_ACN_STREAM_SIZE, S13_ACN_Encode, S13_ACN_Decode, S13_Equal);
   package CS14 is new Checks (S14, S14_ACN_STREAM_SIZE, S14_ACN_Encode, S14_ACN_Decode, S14_Equal);
   package CS15 is new Checks (S15, S15_ACN_STREAM_SIZE, S15_ACN_Encode, S15_ACN_Decode, S15_Equal);
   package CS17 is new Checks (S17, S17_ACN_STREAM_SIZE, S17_ACN_Encode, S17_ACN_Decode, S17_Equal);
   package CS19 is new Checks (S19, S19_ACN_STREAM_SIZE, S19_ACN_Encode, S19_ACN_Decode, S19_Equal);
   package CT1 is new Checks (T1, T1_ACN_STREAM_SIZE, T1_ACN_Encode, T1_ACN_Decode, T1_Equal);
   package CT2 is new Checks (T2, T2_ACN_STREAM_SIZE, T2_ACN_Encode, T2_ACN_Decode, T2_Equal);
   package CTA is new Checks (TA, TA_ACN_STREAM_SIZE, TA_ACN_Encode, TA_ACN_Decode, TA_Equal);

   --  n octets 1, 2, ...
   function Oct (N : Natural) return adaasn1rtl.OctetBuffer is
      B : adaasn1rtl.OctetBuffer (1 .. 15) := (others => 0);
   begin
      for I in 1 .. N loop
         B (I) := adaasn1rtl.Asn1Byte (I);
      end loop;
      return B;
   end Oct;
   F3 : constant adaasn1rtl.OctetBuffer (1 .. 3) := (1, 2, 3);

   VS7 : S7 := S7_Init;
   VS8 : S8 := S8_Init;
   VS4 : S4 := S4_Init;
   VS9 : S9 := S9_Init;
   VS12 : S12 := S12_Init;
   VS10 : S10 := S10_Init;
   VS13 : S13 := S13_Init;
   VS14 : S14 := S14_Init;
   VS15 : S15 := S15_Init;
   VS17 : S17 := S17_Init;
   VS19 : S19 := S19_Init;
   VT1 : T1 := T1_Init;
   VT2 : T2 := T2_Init;
   VTA : TA := TA_Init;
begin
   --  the only consumer absent: n = 0; present: its size
   VS7.tail := 7;
   VS7.Exist := (p => 0);
   CS7.Enc ("S7 p absent", VS7, (16#00#, 16#38#));
   VS7.Exist := (p => 1);
   VS7.p := (Length => 3, Data => Oct (3));
   CS7.Enc ("S7 p 3", VS7, (16#98#, 16#08#, 16#10#, 16#18#, 16#38#));
   --  the CHOICE consumer absent: k = 0
   VS8.tail := 7;
   VS8.Exist := (c => 0);
   CS8.Enc ("S8 c absent", VS8, (16#00#, 16#38#));
   --  the holder present, its field's CHOICE consumer absent: k = 0
   VS4.tail := 7;
   VS4.Exist := (h => 1, c => 0);
   VS4.h.a := 0;
   CS4.Enc ("S4 h present c absent", VS4, (16#40#, 16#00#, 16#1C#));
   --  the callee's only consumer absent
   VS9.pl.e := 5; VS9.tail := 7;
   VS9.pl.Exist := (d => 0);
   CS9.Enc ("S9 pl.d absent", VS9, (16#00#, 16#28#, 16#38#));
   --  both consumers of the callee absent
   VS12.pl.e := 5; VS12.tail := 7;
   VS12.pl.Exist := (d1 => 0, d2 => 0);
   CS12.Enc ("S12 pl.d1 d2 absent", VS12, (16#00#, 16#14#, 16#1C#));
   --  the fixed-size o absent and the callee's d of 5 octets; both absent: n = 0
   VS10.pl.e := 5; VS10.tail := 7;
   VS10.Exist := (o => 0);
   VS10.pl.Exist := (d => 1);
   VS10.pl.d := (Length => 5, Data => Oct (5));
   CS10.Enc ("S10 o absent pl.d 5", VS10, (16#2C#, 16#04#, 16#08#, 16#0C#, 16#10#, 16#14#, 16#14#, 16#1C#));
   VS10.pl.Exist := (d => 0);
   CS10.Enc ("S10 o pl.d absent", VS10, (16#00#, 16#14#, 16#1C#));
   --  the callee's fixed-size o absent and d of 5 octets
   VS13.tail := 7;
   VS13.pl.Exist := (o => 0);
   VS13.Exist := (d => 1);
   VS13.d := (Length => 5, Data => Oct (5));
   CS13.Enc ("S13 pl.o absent d 5", VS13, (16#A8#, 16#00#, 16#04#, 16#08#, 16#0C#, 16#10#, 16#14#, 16#1C#));
   --  the callee's only consumer, fixed-size, absent: n = 0; present: 3
   VS14.tail := 7;
   VS14.pl.Exist := (o => 0);
   CS14.Enc ("S14 pl.o absent", VS14, (16#00#, 16#00#, 16#38#));
   VS14.pl.Exist := (o => 1);
   VS14.pl.o.Data := F3;
   CS14.Enc ("S14 pl.o", VS14, (16#38#, 16#08#, 16#10#, 16#18#, 16#00#, 16#38#));
   --  a fixed-size consumer inside the callee's elements: 3 also for an empty list
   VS15.tail := 7;
   VS15.pl.items.Length := 0;
   CS15.Enc ("S15 no items", VS15, (16#30#, 16#1C#));
   VS15.pl.items.Length := 1;
   VS15.pl.items.Data (1).Data := F3;
   CS15.Enc ("S15 1 item", VS15, (16#34#, 16#04#, 16#08#, 16#0C#, 16#1C#));
   --  o present, both callee consumers absent: n = 3; all absent: n = 0
   VS17.pl.e := 5; VS17.tail := 7;
   VS17.pl.Exist := (d1 => 0, d2 => 0);
   VS17.Exist := (o => 1);
   VS17.o := (Length => 3, Data => Oct (3));
   CS17.Enc ("S17 o 3", VS17, (16#98#, 16#08#, 16#10#, 16#18#, 16#0A#, 16#0E#));
   VS17.Exist := (o => 0);
   CS17.Enc ("S17 all absent", VS17, (16#00#, 16#0A#, 16#0E#));
   --  the same one level deeper
   VS19.pl.q.e := 5; VS19.tail := 7;
   VS19.pl.Exist := (m => 0);
   VS19.pl.q.Exist := (d1 => 0, d2 => 0);
   VS19.Exist := (o => 1);
   VS19.o := (Length => 3, Data => Oct (3));
   CS19.Enc ("S19 o 3", VS19, (16#98#, 16#08#, 16#10#, 16#18#, 16#05#, 16#07#));
   VS19.Exist := (o => 0);
   CS19.Enc ("S19 all absent", VS19, (16#00#, 16#05#, 16#07#));
   VS19.pl.q.Exist := (d1 => 0, d2 => 1);
   VS19.pl.q.d2 := (Length => 2, Data => Oct (2));
   CS19.Enc ("S19 pl.q.d2 2", VS19, (16#11#, 16#01#, 16#02#, 16#05#, 16#07#));
   --  Len (1..8), uPER: the default is 1 (wire 0)
   VT1.tail := 7;
   VT1.Exist := (p => 0);
   CT1.Enc ("T1 p absent", VT1, (16#00#, 16#70#));
   --  Len5 (5..8) passed to a callee: the default is 5, wire 0
   VT2.pl.e := 5; VT2.tail := 7;
   VT2.pl.Exist := (d => 0);
   CT2.Enc ("T2 pl.d absent", VT2, (16#00#, 16#A0#, 16#E0#));
   --  the CHOICE selects z: no element, n = 0
   VTA.c := (kind => z_PRESENT, z => True);
   CTA.Enc ("TA z", VTA, (16#00#, 16#C0#));
   VTA.c := (kind => list_PRESENT, list => TA_c_list_Init);
   VTA.c.list.Length := 1;
   VTA.c.list.Data (1).q.o.Data := F3;
   CTA.Enc ("TA list", VTA, (16#03#, 16#00#, 16#40#, 16#80#, 16#C0#));
   if not Failed then
      Ada.Text_IO.Put_Line ("011 wire test OK");
   end if;
end Wire_Test;
