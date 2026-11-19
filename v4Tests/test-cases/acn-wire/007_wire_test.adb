--  --acn-v2: an ACN parameter of a referenced type consumed inside the elements of a SEQUENCE OF of that type (run by
--  v4Tests/scripts/runWireTests.sh, -gnata: the RTL contracts are checked too). Valid values encode to the bytes
--  computed by hand from the ACN with ErrorCode 0 and decode back; values whose elements disagree on the determinant
--  fail with ERR_ACN_DET_CONSISTENCY_MISMATCH (q/issues.md Issue 124: the patch after the loop read
--  Data (Length + 1), a Constraint_Error with -gnata). The same values as 007_wire_test.c.
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
      --  encode V (ACN-inconsistent): a failure with ERR_ACN_DET_CONSISTENCY_MISMATCH
      procedure Rej (Name : String; V : T);
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

      procedure Rej (Name : String; V : T) is
         S : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (Size);
         R : adaasn1rtl.ASN1_RESULT;
      begin
         Encode (V, S, R);
         if R.Success or else R.ErrorCode /= adaasn1rtl.ERR_ACN_DET_CONSISTENCY_MISMATCH then
            Fail (Name & ": encoded, Success=" & Boolean'Image (R.Success) & " ErrorCode="
                  & Integer'Image (R.ErrorCode));
         end if;
      exception
         when others => Fail (Name & ": exception");
      end Rej;
   end Checks;

   package CA is new Checks (TA, TA_ACN_STREAM_SIZE, TA_ACN_Encode, TA_ACN_Decode, TA_Equal);
   package CF is new Checks (TF, TF_ACN_STREAM_SIZE, TF_ACN_Encode, TF_ACN_Decode, TF_Equal);
   package CD is new Checks (TD, TD_ACN_STREAM_SIZE, TD_ACN_Encode, TD_ACN_Decode, TD_Equal);
   package CHk is new Checks (TH, TH_ACN_STREAM_SIZE, TH_ACN_Encode, TH_ACN_Decode, TH_Equal);
   package CX2 is new Checks (TX2, TX2_ACN_STREAM_SIZE, TX2_ACN_Encode, TX2_ACN_Decode, TX2_Equal);
   package CX3 is new Checks (TX3, TX3_ACN_STREAM_SIZE, TX3_ACN_Encode, TX3_ACN_Decode, TX3_Equal);
   package CX4 is new Checks (TX4, TX4_ACN_STREAM_SIZE, TX4_ACN_Encode, TX4_ACN_Decode, TX4_Equal);
   package CE is new Checks (TE, TE_ACN_STREAM_SIZE, TE_ACN_Encode, TE_ACN_Decode, TE_Equal);
   package CM is new Checks (M1, M1_ACN_STREAM_SIZE, M1_ACN_Encode, M1_ACN_Decode, M1_Equal);
   package CV is new Checks (TV, TV_ACN_STREAM_SIZE, TV_ACN_Encode, TV_ACN_Decode, TV_Equal);

   --  n octets First, First + 1, ...
   function Oct (First : Natural; N : Natural) return adaasn1rtl.OctetBuffer is
      B : adaasn1rtl.OctetBuffer (1 .. 15) := (others => 0);
   begin
      for I in 1 .. N loop
         B (I) := adaasn1rtl.Asn1Byte (First + I - 1);
      end loop;
      return B;
   end Oct;

   VA : TA := TA_Init;
   VF : TF := TF_Init;
   VD : TD := TD_Init;
   VH : TH := TH_Init;
   VX2 : TX2 := TX2_Init;
   VX3 : TX3 := TX3_Init;
   VX4 : TX4 := TX4_Init;
   VE : TE := TE_Init;
   VM : M1 := M1_Init;
   VV : TV := TV_Init;
begin
   --  inline element SEQUENCE: full list, one element (the other one stale), empty list (the fallback 0); disagreement
   VA.a := 1; VA.tail := 7; VA.pl.items.Length := 2;
   VA.pl.items.Data (1).o := (Length => 4, Data => Oct (16#10#, 4));
   VA.pl.items.Data (2).o := (Length => 4, Data => Oct (16#20#, 4));
   CA.Enc ("TA 2", VA, (16#01#, 16#48#, 16#40#, 16#44#, 16#48#, 16#4C#, 16#80#, 16#84#, 16#88#, 16#8C#, 16#1C#));
   VA.pl.items.Length := 1;
   VA.pl.items.Data (1).o := (Length => 3, Data => Oct (1, 3));
   VA.pl.items.Data (2).o := (Length => 9, Data => Oct (1, 9));
   CA.Enc ("TA 1", VA, (16#01#, 16#34#, 16#04#, 16#08#, 16#0C#, 16#1C#));
   VA.pl.items.Length := 0;
   CA.Enc ("TA 0", VA, (16#01#, 16#00#, 16#1C#));
   VA.pl.items.Length := 2;
   VA.pl.items.Data (1).o := (Length => 2, Data => Oct (1, 2));
   VA.pl.items.Data (2).o := (Length => 4, Data => Oct (1, 4));
   CA.Rej ("TA 2 4", VA);
   --  a direct consumer next to the loop one
   VF.a := 1; VF.tail := 7;
   VF.pl.Exist := (d => 1);
   VF.pl.d := (Length => 5, Data => Oct (1, 5));
   VF.pl.items.Length := 0;
   VF.pl.items.Data (1) := (Length => 9, Data => Oct (1, 9));
   CF.Enc ("TF d", VF, (16#01#, 16#58#, 16#08#, 16#10#, 16#18#, 16#20#, 16#28#, 16#0E#));
   VF.pl.d := (Length => 2, Data => Oct (1, 2));
   VF.pl.items.Length := 2;
   VF.pl.items.Data (1) := (Length => 2, Data => Oct (3, 2));
   VF.pl.items.Data (2) := (Length => 2, Data => Oct (5, 2));
   CF.Enc ("TF d items", VF, (16#01#, 16#28#, 16#08#, 16#14#, 16#06#, 16#08#, 16#0A#, 16#0C#, 16#0E#));
   VF.pl.Exist := (d => 0);
   VF.pl.d := (Length => 0, Data => Oct (0, 0));
   VF.pl.items.Data (1) := (Length => 3, Data => Oct (3, 3));
   VF.pl.items.Data (2) := (Length => 3, Data => Oct (6, 3));
   CF.Enc ("TF items", VF, (16#01#, 16#34#, 16#06#, 16#08#, 16#0A#, 16#0C#, 16#0E#, 16#10#, 16#0E#));
   VF.pl.Exist := (d => 1);
   VF.pl.d := (Length => 3, Data => Oct (1, 3));
   VF.pl.items.Length := 1;
   VF.pl.items.Data (1) := (Length => 4, Data => Oct (1, 4));
   CF.Rej ("TF d 3 items 4", VF);
   VF.pl.Exist := (d => 0);
   VF.pl.items.Length := 2;
   VF.pl.items.Data (1) := (Length => 3, Data => Oct (1, 3));
   VF.pl.items.Data (2) := (Length => 4, Data => Oct (1, 4));
   CF.Rej ("TF items 3 4", VF);
   --  present-when in the element: all present, all absent; mixed
   VD.a := 1; VD.tail := 7; VD.pl.items.Length := 2;
   VD.pl.items.Data (1) := (x => 3, y => 1, Exist => (x => 1));
   VD.pl.items.Data (2) := (x => 4, y => 2, Exist => (x => 1));
   CD.Enc ("TD x", VD, (16#01#, 16#D9#, 16#88#, 16#1C#));
   VD.pl.items.Data (1) := (x => 0, y => 1, Exist => (x => 0));
   VD.pl.items.Data (2) := (x => 0, y => 2, Exist => (x => 0));
   CD.Enc ("TD no x", VD, (16#01#, 16#4A#, 16#07#));
   VD.pl.items.Data (1) := (x => 3, y => 1, Exist => (x => 1));
   CD.Rej ("TD x, no x", VD);
   --  two callees: they agree, the second one empty; they disagree
   VH.a := 1; VH.tail := 7;
   VH.p1.items.Length := 1;
   VH.p1.items.Data (1).o := (Length => 2, Data => Oct (1, 2));
   VH.p2.items.Length := 2;
   VH.p2.items.Data (1).o := (Length => 2, Data => Oct (3, 2));
   VH.p2.items.Data (2).o := (Length => 2, Data => Oct (5, 2));
   CHk.Enc ("TH", VH, (16#01#, 16#24#, 16#04#, 16#0A#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#));
   VH.p2.items.Length := 0;
   CHk.Enc ("TH p2 empty", VH, (16#01#, 16#24#, 16#04#, 16#08#, 16#07#));
   VH.p2.items.Length := 1;
   VH.p2.items.Data (1).o := (Length => 3, Data => Oct (3, 3));
   CHk.Rej ("TH 2 3", VH);
   --  the SEQUENCE OF under an OPTIONAL component: absent, present; disagreement
   VX2.a := 1; VX2.tail := 7;
   VX2.pl.Exist := (items => 0);
   CX2.Enc ("TX2 absent", VX2, (16#01#, 16#00#, 16#38#));
   VX2.pl.Exist := (items => 1);
   VX2.pl.items.Length := 2;
   VX2.pl.items.Data (1).o := (Length => 3, Data => Oct (1, 3));
   VX2.pl.items.Data (2).o := (Length => 3, Data => Oct (4, 3));
   CX2.Enc ("TX2", VX2, (16#01#, 16#3C#, 16#04#, 16#08#, 16#0C#, 16#10#, 16#14#, 16#18#, 16#1C#));
   VX2.pl.items.Data (2).o := (Length => 2, Data => Oct (4, 2));
   CX2.Rej ("TX2 3 2", VX2);
   --  the SEQUENCE OF in a CHOICE alternative: the other alternative selected (its Length is not read: a
   --  discriminant check with -gnata), the list; disagreement
   VX3.a := 1; VX3.tail := 7;
   VX3.pl := (kind => a_PRESENT, a => 5);
   CX3.Enc ("TX3 a", VX3, (16#01#, 16#05#, 16#07#));
   VX3.pl := (kind => items_PRESENT, items => (Length => 2, Data => (others => (o => (Length => 2, Data => Oct (1, 2))))));
   VX3.pl.items.Data (2).o := (Length => 2, Data => Oct (3, 2));
   CX3.Enc ("TX3 items", VX3, (16#01#, 16#2C#, 16#04#, 16#08#, 16#0C#, 16#10#, 16#1C#));
   VX3.pl.items.Data (2).o := (Length => 1, Data => Oct (3, 1));
   CX3.Rej ("TX3 2 1", VX3);
   --  an inline CHOICE element selected by the parameter: all k1, all k2; mixed
   VX4.a := 1; VX4.tail := 7; VX4.pl.items.Length := 2;
   VX4.pl.items.Data (1) := (kind => k1_PRESENT, k1 => 5);
   VX4.pl.items.Data (2) := (kind => k1_PRESENT, k1 => 2);
   CX4.Enc ("TX4 k1", VX4, (16#01#, 16#35#, 16#03#, 16#80#));
   VX4.pl.items.Data (1) := (kind => k2_PRESENT, k2 => True);
   VX4.pl.items.Data (2) := (kind => k2_PRESENT, k2 => False);
   CX4.Enc ("TX4 k2", VX4, (16#01#, 16#70#, 16#38#));
   VX4.pl.items.Data (2) := (kind => k1_PRESENT, k1 => 2);
   CX4.Rej ("TX4 k2 k1", VX4);
   --  nested loops; an inner element that disagrees
   VE.a := 1; VE.tail := 7; VE.pl.rows.Length := 2;
   VE.pl.rows.Data (1).cols.Length := 2;
   VE.pl.rows.Data (1).cols.Data (1) := (Length => 2, Data => Oct (1, 2));
   VE.pl.rows.Data (1).cols.Data (2) := (Length => 2, Data => Oct (3, 2));
   VE.pl.rows.Data (2).cols.Length := 1;
   VE.pl.rows.Data (2).cols.Data (1) := (Length => 2, Data => Oct (5, 2));
   CE.Enc ("TE", VE, (16#01#, 16#2C#, 16#04#, 16#08#, 16#0C#, 16#10#, 16#0A#, 16#0C#, 16#0E#));
   VE.pl.rows.Data (2).cols.Data (1) := (Length => 3, Data => Oct (5, 3));
   CE.Rej ("TE 2 3", VE);
   --  the callee in an outer SEQUENCE OF element: each outer element its own determinant; a disagreement
   VM.Length := 2;
   VM.Data (1).a := 1; VM.Data (1).pl.items.Length := 1;
   VM.Data (1).pl.items.Data (1).o := (Length => 2, Data => Oct (1, 2));
   VM.Data (2).a := 2; VM.Data (2).pl.items.Length := 2;
   VM.Data (2).pl.items.Data (1).o := (Length => 3, Data => Oct (1, 3));
   VM.Data (2).pl.items.Data (2).o := (Length => 3, Data => Oct (4, 3));
   CM.Enc ("M1", VM, (16#80#, 16#92#, 16#02#, 16#04#, 16#04#, 16#70#, 16#08#, 16#10#, 16#18#, 16#20#, 16#28#,
                      16#30#));
   VM.Data (1).pl.items.Length := 0;
   VM.Data (2).pl.items.Length := 1;
   VM.Data (2).pl.items.Data (1).o := (Length => 1, Data => Oct (9, 1));
   CM.Enc ("M1 0 1", VM, (16#80#, 16#80#, 16#04#, 16#28#, 16#48#));
   VM.Data (2).pl.items.Length := 2;
   VM.Data (2).pl.items.Data (2).o := (Length => 2, Data => Oct (9, 2));
   CM.Rej ("M1 1 2", VM);
   --  the field's holder: present, absent with the callee absent, absent with the callee's list empty
   VV.a := 1; VV.tail := 7; VV.h.b := 3;
   VV.Exist := (h => 1, pl => 1);
   VV.pl.items.Length := 2;
   VV.pl.items.Data (1).o := (Length => 2, Data => Oct (1, 2));
   VV.pl.items.Data (2).o := (Length => 2, Data => Oct (3, 2));
   CV.Enc ("TV", VV, (16#01#, 16#D9#, 16#40#, 16#20#, 16#40#, 16#60#, 16#80#, 16#E0#));
   VV.h.b := 0;
   VV.Exist := (h => 0, pl => 0);
   VV.pl.items.Length := 0;
   CV.Enc ("TV no h no pl", VV, (16#01#, 16#01#, 16#C0#));
   VV.Exist := (h => 0, pl => 1);
   CV.Enc ("TV no h", VV, (16#01#, 16#40#, 16#70#));
   if not Failed then
      Ada.Text_IO.Put_Line ("007 OK");
   end if;
end Wire_Test;
