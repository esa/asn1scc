--  --acn-v2: a determinant field in a component the stream may say absent, read outside it by a specialised function
--  (run by v4Tests/scripts/runWireTests.sh, -gnata: the RTL contracts are checked too). Valid values whose holder is
--  absent encode to the bytes legacy writes and decode back; streams with the holder absent and the dependent present
--  are rejected (q/issues.md Issue 107; the error codes of the dependents inside specialised decoders are private to
--  the package body: any nonzero code). The same values as 005_wire_test.c.
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl;
with adaasn1rtl.encoding;
with WIRE;
procedure Wire_Test is
   use type adaasn1rtl.Asn1Byte;
   use type adaasn1rtl.BIT;
   use type adaasn1rtl.Asn1UInt;

   type Bytes is array (Positive range <>) of adaasn1rtl.Asn1Byte;

   procedure Fail (Msg : String) is
   begin
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
      --  decode Stream (the rest 16#AB#): a failure with a nonzero error code
      procedure Rej (Name : String; Stream : Bytes);
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

      procedure Rej (Name : String; Stream : Bytes) is
         S : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (Size);
         W : T;
         R : adaasn1rtl.ASN1_RESULT;
      begin
         S.Buffer := (others => 16#AB#);
         for I in Stream'Range loop
            S.Buffer (I - Stream'First + 1) := Stream (I);
         end loop;
         Decode (W, S, R);
         if R.Success or else R.ErrorCode = 0 then
            Fail (Name & ": accepted, Success=" & Boolean'Image (R.Success) & " ErrorCode="
                  & Integer'Image (R.ErrorCode));
         end if;
      exception
         when others => Fail (Name & ": exception");
      end Rej;
   end Checks;

   package Arg is new Checks (WIRE.ArgPDU, WIRE.ArgPDU_ACN_STREAM_SIZE, WIRE.ArgPDU_ACN_Encode,
                              WIRE.ArgPDU_ACN_Decode, WIRE.ArgPDU_Equal);
   package Bool is new Checks (WIRE.BoolPDU, WIRE.BoolPDU_ACN_STREAM_SIZE, WIRE.BoolPDU_ACN_Encode,
                               WIRE.BoolPDU_ACN_Decode, WIRE.BoolPDU_Equal);
   package Fix is new Checks (WIRE.FixPDU, WIRE.FixPDU_ACN_STREAM_SIZE, WIRE.FixPDU_ACN_Encode,
                              WIRE.FixPDU_ACN_Decode, WIRE.FixPDU_Equal);
   package Nest is new Checks (WIRE.NestPDU, WIRE.NestPDU_ACN_STREAM_SIZE, WIRE.NestPDU_ACN_Encode,
                               WIRE.NestPDU_ACN_Decode, WIRE.NestPDU_Equal);
   package Ch is new Checks (WIRE.ChPDU, WIRE.ChPDU_ACN_STREAM_SIZE, WIRE.ChPDU_ACN_Encode,
                             WIRE.ChPDU_ACN_Decode, WIRE.ChPDU_Equal);
   package Deep is new Checks (WIRE.DeepPDU, WIRE.DeepPDU_ACN_STREAM_SIZE, WIRE.DeepPDU_ACN_Encode,
                               WIRE.DeepPDU_ACN_Decode, WIRE.DeepPDU_Equal);
   package Str is new Checks (WIRE.StrPDU, WIRE.StrPDU_ACN_STREAM_SIZE, WIRE.StrPDU_ACN_Encode,
                              WIRE.StrPDU_ACN_Decode, WIRE.StrPDU_Equal);
   package Cont is new Checks (WIRE.ContPDU, WIRE.ContPDU_ACN_STREAM_SIZE, WIRE.ContPDU_ACN_Encode,
                               WIRE.ContPDU_ACN_Decode, WIRE.ContPDU_Equal);
   package Cw is new Checks (WIRE.CwPDU, WIRE.CwPDU_ACN_STREAM_SIZE, WIRE.CwPDU_ACN_Encode,
                             WIRE.CwPDU_ACN_Decode, WIRE.CwPDU_Equal);
   package R3 is new Checks (WIRE.R3PDU, WIRE.R3PDU_ACN_STREAM_SIZE, WIRE.R3PDU_ACN_Encode,
                             WIRE.R3PDU_ACN_Decode, WIRE.R3PDU_Equal);
   package Abs_C is new Checks (WIRE.AbsPDU, WIRE.AbsPDU_ACN_STREAM_SIZE, WIRE.AbsPDU_ACN_Encode,
                                WIRE.AbsPDU_ACN_Decode, WIRE.AbsPDU_Equal);
   package SqB is new Checks (WIRE.SqBPDU, WIRE.SqBPDU_ACN_STREAM_SIZE, WIRE.SqBPDU_ACN_Encode,
                              WIRE.SqBPDU_ACN_Decode, WIRE.SqBPDU_Equal);
   package Lx is new Checks (WIRE.LxPDU, WIRE.LxPDU_ACN_STREAM_SIZE, WIRE.LxPDU_ACN_Encode,
                             WIRE.LxPDU_ACN_Decode, WIRE.LxPDU_Equal);

   VArg : WIRE.ArgPDU := WIRE.ArgPDU_Init;
   VBool : WIRE.BoolPDU := WIRE.BoolPDU_Init;
   VFix : WIRE.FixPDU := WIRE.FixPDU_Init;
   VNest : WIRE.NestPDU := WIRE.NestPDU_Init;
   VCh : WIRE.ChPDU := WIRE.ChPDU_Init;
   VDeep : WIRE.DeepPDU := WIRE.DeepPDU_Init;
   VStr : WIRE.StrPDU := WIRE.StrPDU_Init;
   VCont : WIRE.ContPDU := WIRE.ContPDU_Init;
   VCw : WIRE.CwPDU := WIRE.CwPDU_Init;
   VR3 : WIRE.R3PDU := WIRE.R3PDU_Init;
   VAbs : WIRE.AbsPDU := WIRE.AbsPDU_Init;
   VSqB : WIRE.SqBPDU := WIRE.SqBPDU_Init;
   VLx : WIRE.LxPDU := WIRE.LxPDU_Init;
begin
   --  a size passed as an ACN argument: header absent, pl present without d
   VArg.Exist.header := 0;
   VArg.Exist.pl := 1;
   VArg.pl.Exist.d := 0;
   VArg.pl.e := 5;
   VArg.tail := 7;
   Arg.Enc ("ArgPDU", VArg, (16#40#, 16#A0#, 16#E0#));
   Arg.Rej ("ArgPDU", (16#60#, 16#00#, 16#00#, 16#A0#, 16#E0#));
   --  a BOOLEAN passed as an ACN argument, read by a present-when in the callee
   VBool.Exist.header := 0;
   VBool.Exist.pl := 1;
   VBool.pl.Exist.y := 0;
   VBool.pl.e := 5;
   VBool.tail := 7;
   Bool.Enc ("BoolPDU", VBool, (16#81#, 16#41#, 16#C0#));
   --  a fixed-size consumer in the callee, absent
   VFix.Exist.header := 0;
   VFix.Exist.pl := 1;
   VFix.pl.Exist.d := 0;
   VFix.pl.e := 5;
   VFix.tail := 7;
   Fix.Enc ("FixPDU", VFix, (16#80#, 16#A0#, 16#E0#));
   --  nested holders: h1 present, h2 absent
   VNest.Exist.h1 := 1;
   VNest.h1.a := 1;
   VNest.h1.Exist.h2 := 0;
   VNest.Exist.pl := 1;
   VNest.pl.Exist.d := 0;
   VNest.pl.e := 5;
   VNest.tail := 7;
   Nest.Enc ("NestPDU", VNest, (16#C0#, 16#40#, 16#50#, 16#70#));
   Nest.Rej ("NestPDU", (16#C0#, 16#50#, 16#00#, 16#00#, 16#50#, 16#70#));
   --  a CHOICE determinant in the callee
   VCh.Exist.header := 0;
   VCh.Exist.pl := 1;
   VCh.pl.Exist.ch := 0;
   VCh.pl.e := 5;
   VCh.tail := 7;
   Ch.Enc ("ChPDU", VCh, (16#80#, 16#A0#, 16#E0#));
   Ch.Rej ("ChPDU", (16#B0#, 16#50#, 16#70#));
   --  the reference passed through two specialised functions to a SEQUENCE OF
   VDeep.Exist.header := 0;
   VDeep.Exist.pl := 1;
   VDeep.pl.x := 9;
   VDeep.pl.inr.Exist.s := 0;
   VDeep.pl.inr.e := 5;
   VDeep.tail := 7;
   Deep.Enc ("DeepPDU", VDeep, (16#82#, 16#40#, 16#A0#, 16#E0#));
   Deep.Rej ("DeepPDU", (16#82#, 16#60#, 16#00#, 16#00#, 16#A0#, 16#E0#));
   --  an IA5String size in the callee
   VStr.Exist.header := 0;
   VStr.Exist.pl := 1;
   VStr.pl.Exist.s := 0;
   VStr.pl.e := 5;
   VStr.tail := 7;
   Str.Enc ("StrPDU", VStr, (16#80#, 16#A0#, 16#E0#));
   Str.Rej ("StrPDU", (16#B0#, 16#61#, 16#02#, 16#83#, 16#80#));
   --  a CONTAINING sized by the field
   VCont.Exist.header := 0;
   VCont.Exist.cont := 0;
   VCont.tail := 16#F7#;
   Cont.Enc ("ContPDU", VCont, (16#3D#, 16#C0#));
   Cont.Rej ("ContPDU", (16#82#, 16#41#, 16#C0#));
   --  a CONTAINING inside the callee, sized by the ACN argument
   VCw.Exist.header := 0;
   VCw.Exist.w := 1;
   VCw.w.Exist.p := 0;
   VCw.w.y := 5;
   VCw.tail := 7;
   Cw.Enc ("CwPDU", VCw, (16#80#, 16#A0#, 16#E0#));
   Cw.Rej ("CwPDU", (16#A1#, 16#20#, 16#A0#, 16#E0#));
   --  a field of an OPTIONAL component inside a mandatory referenced component, exposed to a sibling's argument
   VR3.h.Exist.hin := 0;
   VR3.Exist.pl := 1;
   VR3.pl.Exist.d := 0;
   VR3.pl.e := 5;
   VR3.tail := 7;
   R3.Enc ("R3PDU", VR3, (16#80#, 16#A0#, 16#E0#));
   R3.Rej ("R3PDU", (16#A0#, 16#00#, 16#00#, 16#A0#, 16#E0#));
   --  the component is ALWAYS ABSENT: a present dependent always fails
   VAbs.Exist.pl := 1;
   VAbs.pl.Exist.d := 0;
   VAbs.pl.e := 5;
   VAbs.tail := 7;
   Abs_C.Enc ("AbsPDU", VAbs, (16#81#, 16#41#, 16#C0#));
   Abs_C.Rej ("AbsPDU", (16#C0#, 16#00#, 16#01#, 16#41#, 16#C0#));
   --  an ASCII INTEGER field passed as an ACN argument is not deferred (no reference holds it): the callee must not
   --  check the reference; the stream legacy writes (h.a = 1, n = "+00", d of 0 octets, e = 5, tail = 7) decodes
   --  (decode only: the --acn-v2 encoder of this shape is q/issues.md Issue 101)
   declare
      Stream : constant Bytes := (16#C0#, 16#4A#, 16#CC#, 16#0C#, 16#20#, 16#A0#, 16#E0#);
      S : WIRE.AscPDU_ACN_Stream := adaasn1rtl.encoding.BitStream_init (WIRE.AscPDU_ACN_STREAM_SIZE);
      V : WIRE.AscPDU;
      R : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer := (others => 16#AB#);
      for I in Stream'Range loop
         S.Buffer (I - Stream'First + 1) := Stream (I);
      end loop;
      WIRE.AscPDU_ACN_Decode (V, S, R);
      if not R.Success or else R.ErrorCode /= 0 or else V.Exist.h /= 1 or else V.h.a /= 1 or else V.Exist.pl /= 1
        or else V.pl.Exist.d /= 1 or else V.pl.d.Length /= 0 or else V.pl.e /= 5 or else V.tail /= 7
      then
         Fail ("AscPDU: decode Success=" & Boolean'Image (R.Success) & " ErrorCode=" & Integer'Image (R.ErrorCode));
      end if;
   exception
      when others => Fail ("AscPDU: exception");
   end;
   --  SEQUENCE OF elements: element 1 holds the field (more = 1, y present), element 2 does not (y absent)
   VSqB.Data (1).Exist.h := 1;
   VSqB.Data (1).h.a := 1;
   VSqB.Data (1).Exist.pl := 1;
   VSqB.Data (1).pl.Exist.y := 1;
   VSqB.Data (1).pl.y := 9;
   VSqB.Data (1).pl.e := 5;
   VSqB.Data (1).tail := 7;
   VSqB.Data (2).Exist.h := 0;
   VSqB.Data (2).Exist.pl := 1;
   VSqB.Data (2).pl.Exist.y := 0;
   VSqB.Data (2).pl.e := 6;
   VSqB.Data (2).tail := 8;
   SqB.Enc ("SqBPDU", VSqB, (16#C0#, 16#61#, 16#20#, 16#A0#, 16#F0#, 16#30#, 16#40#));
   --  the same with the field exposed by an OPTIONAL referenced component
   VLx.Data (1).Exist.hd := 1;
   VLx.Data (1).hd.a := 1;
   VLx.Data (1).Exist.pl := 1;
   VLx.Data (1).pl.Exist.y := 1;
   VLx.Data (1).pl.y := 9;
   VLx.Data (1).pl.e := 5;
   VLx.Data (1).tail := 7;
   VLx.Data (2).Exist.hd := 0;
   VLx.Data (2).Exist.pl := 1;
   VLx.Data (2).pl.Exist.y := 0;
   VLx.Data (2).pl.e := 6;
   VLx.Data (2).tail := 8;
   Lx.Enc ("LxPDU", VLx, (16#C0#, 16#61#, 16#20#, 16#A0#, 16#F0#, 16#30#, 16#40#));
   Ada.Text_IO.Put_Line ("wire checks done");
end Wire_Test;
