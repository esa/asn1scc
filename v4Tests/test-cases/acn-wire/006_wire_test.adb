--  --acn-v2: an ACN parameter of a referenced type with several consumers inside it (run by
--  v4Tests/scripts/runWireTests.sh, -gnata: the RTL contracts and the pragma Assume of the patches that cannot fail are
--  checked too). Valid values encode to the bytes legacy writes with ErrorCode 0 and decode back; values whose
--  consumers disagree fail with a nonzero error code (q/issues.md Issue 121). The same values as 006_wire_test.c.
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
      --  encode V (ACN-inconsistent): a failure with a nonzero error code
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
         if R.Success or else R.ErrorCode = 0 then
            Fail (Name & ": encoded, Success=" & Boolean'Image (R.Success) & " ErrorCode="
                  & Integer'Image (R.ErrorCode));
         end if;
      exception
         when others => Fail (Name & ": exception");
      end Rej;
   end Checks;

   package CB is new Checks (TB, TB_ACN_STREAM_SIZE, TB_ACN_Encode, TB_ACN_Decode, TB_Equal);
   package CG is new Checks (TG, TG_ACN_STREAM_SIZE, TG_ACN_Encode, TG_ACN_Decode, TG_Equal);
   package CHk is new Checks (TH, TH_ACN_STREAM_SIZE, TH_ACN_Encode, TH_ACN_Decode, TH_Equal);
   package CI is new Checks (TI, TI_ACN_STREAM_SIZE, TI_ACN_Encode, TI_ACN_Decode, TI_Equal);
   package CK is new Checks (TK, TK_ACN_STREAM_SIZE, TK_ACN_Encode, TK_ACN_Decode, TK_Equal);
   package CL is new Checks (TL, TL_ACN_STREAM_SIZE, TL_ACN_Encode, TL_ACN_Decode, TL_Equal);
   package CM is new Checks (TM, TM_ACN_STREAM_SIZE, TM_ACN_Encode, TM_ACN_Decode, TM_Equal);
   package CN is new Checks (TN, TN_ACN_STREAM_SIZE, TN_ACN_Encode, TN_ACN_Decode, TN_Equal);
   package CP is new Checks (TP, TP_ACN_STREAM_SIZE, TP_ACN_Encode, TP_ACN_Decode, TP_Equal);
   package CF1 is new Checks (TF1, TF1_ACN_STREAM_SIZE, TF1_ACN_Encode, TF1_ACN_Decode, TF1_Equal);
   package CF2 is new Checks (TF2, TF2_ACN_STREAM_SIZE, TF2_ACN_Encode, TF2_ACN_Decode, TF2_Equal);
   package CC1 is new Checks (TC1, TC1_ACN_STREAM_SIZE, TC1_ACN_Encode, TC1_ACN_Decode, TC1_Equal);
   package CC2 is new Checks (TC2, TC2_ACN_STREAM_SIZE, TC2_ACN_Encode, TC2_ACN_Decode, TC2_Equal);
   package CL2 is new Checks (TL2, TL2_ACN_STREAM_SIZE, TL2_ACN_Encode, TL2_ACN_Decode, TL2_Equal);
   package CQ is new Checks (TQ, TQ_ACN_STREAM_SIZE, TQ_ACN_Encode, TQ_ACN_Decode, TQ_Equal);
   package CQV is new Checks (TQV, TQV_ACN_STREAM_SIZE, TQV_ACN_Encode, TQV_ACN_Decode, TQV_Equal);
   package CW is new Checks (TW, TW_ACN_STREAM_SIZE, TW_ACN_Encode, TW_ACN_Decode, TW_Equal);

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

   VB : TB := TB_Init;
   VG : TG := TG_Init;
   VH : TH := TH_Init;
   VI : TI := TI_Init;
   VK : TK := TK_Init;
   VL : TL := TL_Init;
   VM : TM := TM_Init;
   VN : TN := TN_Init;
   VP : TP := TP_Init;
   VF1 : TF1 := TF1_Init;
   VF2 : TF2 := TF2_Init;
   VC1 : TC1 := TC1_Init;
   VC2 : TC2 := TC2_Init;
   VL2 : TL2 := TL2_Init;
   VQ : TQ := TQ_Init;
   VQV : TQV := TQV_Init;
   VW : TW := TW_Init;
begin
   --  two OPTIONAL consumers: the first present, the second (the one patched before) absent, and the other way
   VB.a := 1; VB.tail := 7; VB.pl.e := 5;
   VB.pl.Exist := (o => 1, d => 0);
   VB.pl.o := (Length => 3, Data => Oct (3));
   CB.Enc ("TB o", VB, (16#01#, 16#38#, 16#04#, 16#08#, 16#0C#, 16#14#, 16#1C#));
   VB.pl.Exist := (o => 0, d => 1);
   VB.pl.d := (Length => 3, Data => Oct (3));
   CB.Enc ("TB d", VB, (16#01#, 16#34#, 16#04#, 16#08#, 16#0C#, 16#14#, 16#1C#));
   VB.pl.Exist := (o => 1, d => 1);
   VB.pl.o := (Length => 2, Data => Oct (2));
   CB.Rej ("TB o 2 d 3", VB);
   --  two mandatory consumers
   VG.a := 1; VG.tail := 7; VG.pl.e := 5;
   VG.pl.o := (Length => 2, Data => Oct (2));
   VG.pl.d := (Length => 2, Data => Oct (2));
   CG.Enc ("TG", VG, (16#01#, 16#20#, 16#10#, 16#20#, 16#10#, 16#20#, 16#50#, 16#70#));
   VG.pl.d := (Length => 3, Data => Oct (3));
   CG.Rej ("TG o 2 d 3", VG);
   --  the alternatives of a CHOICE
   VH.a := 1; VH.tail := 7;
   VH.pl := (kind => a_PRESENT, a => (Length => 3, Data => Oct (3)));
   CHk.Enc ("TH a", VH, (16#01#, 16#30#, 16#04#, 16#08#, 16#0C#, 16#1C#));
   VH.pl := (kind => b_PRESENT, b => (Length => 3, Data => Oct (3)));
   CHk.Enc ("TH b", VH, (16#01#, 16#34#, 16#04#, 16#08#, 16#0C#, 16#1C#));
   --  a consumer inside an OPTIONAL inline child
   VI.a := 1; VI.tail := 7; VI.pl.e := 5;
   VI.pl.Exist := (inner => 1, d => 0);
   VI.pl.inner.o := (Length => 3, Data => Oct (3));
   CI.Enc ("TI", VI, (16#01#, 16#38#, 16#04#, 16#08#, 16#0C#, 16#14#, 16#1C#));
   --  the callee passes the parameter on
   VK.a := 1; VK.tail := 7; VK.pl.x := 2; VK.pl.q.e := 5;
   VK.pl.q.Exist := (o => 1, d => 0);
   VK.pl.q.o := (Length => 3, Data => Oct (3));
   CK.Enc ("TK", VK, (16#01#, 16#35#, 16#00#, 16#81#, 16#01#, 16#82#, 16#83#, 16#80#));
   --  a mandatory consumer and an OPTIONAL one, absent
   VL.a := 1; VL.tail := 7;
   VL.pl.Exist := (d => 0);
   VL.pl.o := (Length => 3, Data => Oct (3));
   CL.Enc ("TL", VL, (16#01#, 16#30#, 16#08#, 16#10#, 16#18#, 16#38#));
   --  the callee in a SEQUENCE OF element
   VM.Length := 2;
   VM.Data (1).a := 1; VM.Data (1).tail := 7; VM.Data (1).pl.e := 5;
   VM.Data (1).pl.Exist := (o => 1, d => 0);
   VM.Data (1).pl.o := (Length => 3, Data => Oct (3));
   VM.Data (2).a := 2; VM.Data (2).tail := 8; VM.Data (2).pl.e := 6;
   VM.Data (2).pl.Exist := (o => 0, d => 1);
   VM.Data (2).pl.d := (Length => 1, Data => Oct (1));
   CM.Enc ("TM", VM, (16#80#, 16#9C#, 16#02#, 16#04#, 16#06#, 16#0A#, 16#0E#, 16#04#, 16#28#, 16#08#, 16#30#,
                      16#40#));
   --  two CHOICE determinants: the first present, and both disagreeing
   VN.a := 1; VN.tail := 7;
   VN.pl.Exist := (c1 => 1, c2 => 0);
   VN.pl.c1 := (kind => k2_PRESENT, k2 => True);
   CN.Enc ("TN", VN, (16#01#, 16#68#, 16#38#));
   VN.pl.Exist := (c1 => 1, c2 => 1);
   VN.pl.c2 := (kind => k1_PRESENT, k1 => 1);
   CN.Rej ("TN c1 k2 c2 k1", VN);
   --  two present-when flags of one BOOLEAN parameter
   VP.a := 1; VP.tail := 7; VP.pl.e := 5; VP.pl.x := 2; VP.pl.y := 3;
   VP.pl.Exist := (x => 1, y => 1);
   CP.Enc ("TP", VP, (16#01#, 16#A6#, 16#0A#, 16#0E#));
   VP.pl.Exist := (x => 1, y => 0);
   CP.Rej ("TP x present y absent", VP);
   --  fixed-size consumers: o (SIZE(3)) absent and d of 5 octets; o present, d absent; o present and d of 5 octets
   --  (inconsistent); both absent (the bytes of --acn-v2: legacy fails, q/issues.md Issue 123 note)
   VF1.a := 1; VF1.tail := 7; VF1.pl.e := 5;
   VF1.pl.Exist := (o => 0, d => 1);
   VF1.pl.d := (Length => 5, Data => Oct (5));
   CF1.Enc ("TF1 d", VF1, (16#01#, 16#54#, 16#04#, 16#08#, 16#0C#, 16#10#, 16#14#, 16#14#, 16#1C#));
   VF1.pl.Exist := (o => 1, d => 0);
   VF1.pl.o.Data := F3;
   VF1.pl.d := (Length => 0, Data => Oct (0));
   CF1.Enc ("TF1 o", VF1, (16#01#, 16#38#, 16#04#, 16#08#, 16#0C#, 16#14#, 16#1C#));
   VF1.pl.Exist := (o => 1, d => 1);
   VF1.pl.d := (Length => 5, Data => Oct (5));
   CF1.Rej ("TF1 o d 5", VF1);
   VF1.pl.Exist := (o => 0, d => 0);
   VF1.pl.o.Data := (others => 0);
   VF1.pl.d := (Length => 0, Data => Oct (0));
   CF1.Enc ("TF1 none", VF1, (16#01#, 16#00#, 16#14#, 16#1C#));
   VF2.a := 1; VF2.tail := 7; VF2.pl.e := 5;
   VF2.pl.Exist := (d => 1, o => 0);
   VF2.pl.d := (Length => 5, Data => Oct (5));
   CF2.Enc ("TF2 d", VF2, (16#01#, 16#58#, 16#04#, 16#08#, 16#0C#, 16#10#, 16#14#, 16#14#, 16#1C#));
   VF2.pl.Exist := (d => 0, o => 1);
   VF2.pl.o.Data := F3;
   VF2.pl.d := (Length => 0, Data => Oct (0));
   CF2.Enc ("TF2 o", VF2, (16#01#, 16#34#, 16#04#, 16#08#, 16#0C#, 16#14#, 16#1C#));
   VF2.pl.Exist := (d => 1, o => 1);
   VF2.pl.d := (Length => 5, Data => Oct (5));
   CF2.Rej ("TF2 o d 5", VF2);
   --  fixed-size alternative of a CHOICE next to a variable one: alternative b (5 octets) made a pragma Assume false
   --  when the constant was patched unguarded
   VC1.a := 1; VC1.tail := 7;
   VC1.pl := (kind => b_PRESENT, b => (Length => 5, Data => Oct (5)));
   CC1.Enc ("TC1 b", VC1, (16#01#, 16#54#, 16#04#, 16#08#, 16#0C#, 16#10#, 16#14#, 16#1C#));
   VC1.pl := (kind => a_PRESENT, a => (Data => F3));
   CC1.Enc ("TC1 a", VC1, (16#01#, 16#30#, 16#04#, 16#08#, 16#0C#, 16#1C#));
   VC2.a := 1; VC2.tail := 7;
   VC2.pl := (kind => b_PRESENT, b => (Length => 5, Data => Oct (5)));
   CC2.Enc ("TC2 b", VC2, (16#01#, 16#50#, 16#04#, 16#08#, 16#0C#, 16#10#, 16#14#, 16#1C#));
   VC2.pl := (kind => a_PRESENT, a => (Data => F3));
   CC2.Enc ("TC2 a", VC2, (16#01#, 16#34#, 16#04#, 16#08#, 16#0C#, 16#1C#));
   --  no consumer present: the size field takes the fallback 0 (legacy rejects these values)
   VF2.pl.Exist := (d => 0, o => 0);
   VF2.pl.o.Data := (others => 0);
   VF2.pl.d := (Length => 0, Data => Oct (0));
   CF2.Enc ("TF2 none", VF2, (16#01#, 16#00#, 16#14#, 16#1C#));
   VC2.pl := (kind => c_PRESENT, c => 4);
   CC2.Enc ("TC2 c", VC2, (16#01#, 16#0A#, 16#03#, 16#80#));
   --  a direct consumer and one inside a SEQUENCE OF of the callee: d absent with elements, and d of 3 octets
   VQ.a := 1; VQ.tail := 7;
   VQ.pl.Exist := (d => 0);
   VQ.pl.d := (Length => 0, Data => Oct (0));
   VQ.pl.items.Data (1).Data := F3;
   CQ.Enc ("TQ", VQ, (16#01#, 16#30#, 16#08#, 16#10#, 16#18#, 16#38#));
   VQ.pl.Exist := (d => 1);
   VQ.pl.d := (Length => 3, Data => Oct (3));
   CQ.Enc ("TQ d", VQ, (16#01#, 16#38#, 16#08#, 16#10#, 16#18#, 16#08#, 16#10#, 16#18#, 16#38#));
   VQV.a := 1; VQV.tail := 7;
   VQV.pl.Exist := (d => 0);
   VQV.pl.d := (Length => 0, Data => Oct (0));
   VQV.pl.items.Length := 2;
   VQV.pl.items.Data (1).Data := F3;
   VQV.pl.items.Data (2).Data := F3;
   CQV.Enc ("TQV", VQV, (16#01#, 16#34#, 16#04#, 16#08#, 16#0C#, 16#04#, 16#08#, 16#0C#, 16#1C#));
   --  two formal parameters bound to one field (pl <n, n>): x and z of 3 octets; x 2 / z 3 and y 2 / z 3 rejected
   VW.a := 1; VW.tail := 7;
   VW.pl.Exist := (x => 1, y => 0);
   VW.pl.x := (Length => 3, Data => Oct (3));
   VW.pl.y := (Length => 0, Data => Oct (0));
   VW.pl.z := (Length => 3, Data => Oct (3));
   CW.Enc ("TW", VW, (16#01#, 16#38#, 16#04#, 16#08#, 16#0C#, 16#04#, 16#08#, 16#0C#, 16#1C#));
   VW.pl.x := (Length => 2, Data => Oct (2));
   CW.Rej ("TW x 2 z 3", VW);
   VW.pl.Exist := (x => 0, y => 1);
   VW.pl.x := (Length => 0, Data => Oct (0));
   VW.pl.y := (Length => 2, Data => Oct (2));
   CW.Rej ("TW y 2 z 3", VW);
   --  an OPTIONAL consumer before a mandatory one
   VL2.a := 1; VL2.tail := 7;
   VL2.pl.Exist := (o => 0);
   VL2.pl.d := (Length => 3, Data => Oct (3));
   CL2.Enc ("TL2 d", VL2, (16#01#, 16#30#, 16#08#, 16#10#, 16#18#, 16#38#));
   VL2.pl.Exist := (o => 1);
   VL2.pl.o := (Length => 3, Data => Oct (3));
   CL2.Enc ("TL2 o d", VL2, (16#01#, 16#38#, 16#08#, 16#10#, 16#18#, 16#08#, 16#10#, 16#18#, 16#38#));
   VL2.pl.o := (Length => 2, Data => Oct (2));
   CL2.Rej ("TL2 o 2 d 3", VL2);
   if not Failed then
      Ada.Text_IO.Put_Line ("all checks passed");
   end if;
end Wire_Test;
