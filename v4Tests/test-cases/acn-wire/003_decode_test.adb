--  A determinant field inside an OPTIONAL component, its dependent outside it (run by v4Tests/scripts/runWireTests.sh,
--  -gnata, legacy and --acn-v2). A stream that says the component is absent and the dependent present does not hold
--  the field: the decoder must fail with the dependent's error code (it raised Constraint_Error, or with -gnatp used
--  an unset local). The bytes after the stream are 16#AB#. The round trips are in 003_decode_test.c. Built with
--  -gnateDACN_V2=True for --acn-v2, False for legacy (GNAT integrated preprocessor).
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl;
with adaasn1rtl.encoding;
with WIRE;
procedure Decode_Test is
   use type adaasn1rtl.BIT;
   use type adaasn1rtl.Asn1UInt;

   type Bytes is array (Positive range <>) of adaasn1rtl.Asn1Byte;

   procedure Fail (Msg : String) is
   begin
      Ada.Text_IO.Put_Line (Msg);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Fail;

   --  decode T from the bytes given: failure with Exp_Err (Exp_Err > 0), failure with any nonzero code (Exp_Err = -1),
   --  success (Exp_Err = 0), or any result without an exception (Exp_Err = -2); the decoded value in V
   generic
      type T is private;
      Size : Positive;
      with procedure Decode
        (V : out T; S : in out adaasn1rtl.encoding.Bitstream; R : out adaasn1rtl.ASN1_RESULT);
   procedure Check (Name : String; Stream : Bytes; Exp_Err : Integer; V : out T);

   --  a stack with garbage where the decoders' locals will be (built with -gnatVa: 2 is not a valid Boolean)
   procedure Dirty is
      type Junk_T is array (1 .. 8192) of adaasn1rtl.Asn1Byte;
      J : Junk_T := (others => 16#02#);
      pragma Volatile (J);
   begin
      J (7) := J (8);
   end Dirty;

   procedure Check (Name : String; Stream : Bytes; Exp_Err : Integer; V : out T) is
      S : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (Size);
      R : adaasn1rtl.ASN1_RESULT;
      Ok : Boolean;
   begin
      S.Buffer := (others => 16#AB#);
      for I in Stream'Range loop
         S.Buffer (S.Buffer'First + I - Stream'First) := Stream (I);
      end loop;
      Dirty;
      Decode (V, S, R);
      case Exp_Err is
         when 0      => Ok := R.Success;
         when -1     => Ok := not R.Success and then R.ErrorCode /= 0;
         when -2     => Ok := True;
         when others => Ok := not R.Success and then R.ErrorCode = Exp_Err;
      end case;
      if not Ok then
         Fail (Name & ": Success=" & Boolean'Image (R.Success) & " ErrorCode=" & Integer'Image (R.ErrorCode) &
               ", expected" & Integer'Image (Exp_Err));
      end if;
   exception
      when others =>
         Fail (Name & ": exception");
   end Check;

   procedure My is new Check (WIRE.MyPDU, WIRE.MyPDU_ACN_STREAM_SIZE, WIRE.MyPDU_ACN_Decode);
   procedure Ch is new Check (WIRE.ChPDU, WIRE.ChPDU_ACN_STREAM_SIZE, WIRE.ChPDU_ACN_Decode);
   procedure Pw is new Check (WIRE.PwPDU, WIRE.PwPDU_ACN_STREAM_SIZE, WIRE.PwPDU_ACN_Decode);
   procedure PwStr is new Check (WIRE.PwStrPDU, WIRE.PwStrPDU_ACN_STREAM_SIZE, WIRE.PwStrPDU_ACN_Decode);
   procedure Bool is new Check (WIRE.BoolPDU, WIRE.BoolPDU_ACN_STREAM_SIZE, WIRE.BoolPDU_ACN_Decode);
   procedure Sq is new Check (WIRE.SqPDU, WIRE.SqPDU_ACN_STREAM_SIZE, WIRE.SqPDU_ACN_Decode);
   procedure Nest2 is new Check (WIRE.Nest2PDU, WIRE.Nest2PDU_ACN_STREAM_SIZE, WIRE.Nest2PDU_ACN_Decode);
   procedure Elem is new Check (WIRE.ElemPDU, WIRE.ElemPDU_ACN_STREAM_SIZE, WIRE.ElemPDU_ACN_Decode);
   procedure Nest is new Check (WIRE.NestPDU, WIRE.NestPDU_ACN_STREAM_SIZE, WIRE.NestPDU_ACN_Decode);
   procedure Alt is new Check (WIRE.AltPDU, WIRE.AltPDU_ACN_STREAM_SIZE, WIRE.AltPDU_ACN_Decode);
   procedure Cont is new Check (WIRE.ContPDU, WIRE.ContPDU_ACN_STREAM_SIZE, WIRE.ContPDU_ACN_Decode);
   procedure Arg is new Check (WIRE.ArgPDU, WIRE.ArgPDU_ACN_STREAM_SIZE, WIRE.ArgPDU_ACN_Decode);
   procedure OptArg is new Check (WIRE.OptArgPDU, WIRE.OptArgPDU_ACN_STREAM_SIZE, WIRE.OptArgPDU_ACN_Decode);
   procedure Col is new Check (WIRE.ColPDU, WIRE.ColPDU_ACN_STREAM_SIZE, WIRE.ColPDU_ACN_Decode);
   procedure Col2 is new Check (WIRE.Col2PDU, WIRE.Col2PDU_ACN_STREAM_SIZE, WIRE.Col2PDU_ACN_Decode);
   procedure Abs_Check is new Check (WIRE.AbsPDU, WIRE.AbsPDU_ACN_STREAM_SIZE, WIRE.AbsPDU_ACN_Decode);
   procedure Same is new Check (WIRE.SamePDU, WIRE.SamePDU_ACN_STREAM_SIZE, WIRE.SamePDU_ACN_Decode);

   VMy : WIRE.MyPDU;
   VCh : WIRE.ChPDU;
   VPw : WIRE.PwPDU;
   VPwStr : WIRE.PwStrPDU;
   VBool : WIRE.BoolPDU;
   VSq : WIRE.SqPDU;
   VNest2 : WIRE.Nest2PDU;
   VElem : WIRE.ElemPDU;
   VNest : WIRE.NestPDU;
   VAlt : WIRE.AltPDU;
   VCont : WIRE.ContPDU;
   VArg : WIRE.ArgPDU;
   VOptArg : WIRE.OptArgPDU;
   VCol : WIRE.ColPDU;
   VCol2 : WIRE.Col2PDU;
   VAbs : WIRE.AbsPDU;
   VSame : WIRE.SamePDU;
begin
   --  sizes of an OCTET STRING and of an IA5String: header absent, data present
   My ("MyPDU a", (1 => 16#40#), WIRE.ERR_ACN_DECODE_MYPDU_DATA, VMy);
   My ("MyPDU c", (1 => 16#00#), 0, VMy);
   if VMy.Exist.header /= 0 or else VMy.Exist.data /= 0 or else VMy.Exist.str /= 0 then
      Fail ("MyPDU c: value");
   end if;
   --  CHOICE determinant
   Ch ("ChPDU a", (1 => 16#40#), WIRE.ERR_ACN_DECODE_CHPDU_CH, VCh);
   Ch ("ChPDU c", (1 => 16#00#), 0, VCh);
   --  CHOICE present-when an INTEGER field, a string field
   Pw ("PwPDU a", (1 => 16#40#), WIRE.ERR_ACN_DECODE_PWPDU_CP, VPw);
   Pw ("PwPDU c", (1 => 16#00#), 0, VPw);
   PwStr ("PwStrPDU a", (1 => 16#40#), WIRE.ERR_ACN_DECODE_PWSTRPDU_CS, VPwStr);
   PwStr ("PwStrPDU c", (1 => 16#00#), 0, VPwStr);
   --  present-when a BOOLEAN field of an absent component: x absent, tail 16#5A#
   Bool ("BoolPDU c", (16#16#, 16#80#), 0, VBool);
   if VBool.Exist.header /= 0 or else VBool.Exist.x /= 0 or else VBool.Exist.p /= 0 or else VBool.tail /= 16#5A# then
      Fail ("BoolPDU c: value");
   end if;
   --  the same in SEQUENCE OF elements: element 2 has no component, so no x (not element 1's field)
   Sq ("SqPDU", (16#A0#, 16#40#), 0, VSq);
   if VSq.Data (1).Exist.h /= 1 or else VSq.Data (1).Exist.x /= 1 or else VSq.Data (1).x.Length /= 0
     or else VSq.Data (2).Exist.h /= 0 or else VSq.Data (2).Exist.x /= 0 or else VSq.Data (2).tail /= 1
   then
      Fail ("SqPDU: value");
   end if;
   --  nested components present when the same field says so as the dependent
   Nest2 ("Nest2PDU c", (1 => 16#00#), 0, VNest2);
   --  SEQUENCE OF: element 1 sets the size, element 2 has no component but a dependent
   Elem ("ElemPDU a", (16#70#, 16#01#, 16#55#, 16#40#), WIRE.ERR_ACN_DECODE_ELEMPDU_ELEMS_ELM_DD, VElem);
   Elem ("ElemPDU c", (1 => 16#00#), 0, VElem);
   --  nested components: h1 absent; h1 present without h2
   Nest ("NestPDU a1", (1 => 16#40#), WIRE.ERR_ACN_DECODE_NESTPDU_D, VNest);
   Nest ("NestPDU a2", (16#C2#, 16#40#), WIRE.ERR_ACN_DECODE_NESTPDU_D, VNest);
   Nest ("NestPDU c", (1 => 16#00#), 0, VNest);
   --  the dependent is an alternative of a mandatory CHOICE
   Alt ("AltPDU a", (1 => 16#00#), WIRE.ERR_ACN_DECODE_ALTPDU_ALT_O1, VAlt);
   Alt ("AltPDU c", (1 => 16#40#), 0, VAlt);
   --  a CONTAINING sized by the field (legacy: the code of cont; --acn-v2: the region is too short)
   Cont ("ContPDU a", (1 => 16#40#), -1, VCont);
   Cont ("ContPDU c", (1 => 16#00#), 0, VCont);
   --  the field as an ACN argument: legacy fails at pl.d; --acn-v2 passes the reference, value 0, and d takes size 0
   --  (memory-safe; q/issues.md Issue 107: it should fail)
#if ACN_V2 then
   Arg ("ArgPDU a", (1 => 16#40#), 0, VArg);
   if VArg.Exist.pl /= 1 or else VArg.pl.d.Length /= 0 then
      Fail ("ArgPDU a: value");
   end if;
#else
   Arg ("ArgPDU a", (1 => 16#40#), WIRE.ERR_ACN_DECODE_ARGPDU_PL_D, VArg);
#end if;
   Arg ("ArgPDU c", (16#16#, 16#80#), 0, VArg);
   if VArg.Exist.header /= 0 or else VArg.Exist.pl /= 0 or else VArg.tail /= 16#5A# then
      Fail ("ArgPDU c: value");
   end if;
   --  an ACN argument used only by an absent part of the referenced type: a valid stream
   OptArg ("OptArgPDU", (16#40#, 16#A0#, 16#E0#), 0, VOptArg);
   if VOptArg.Exist.header /= 0 or else VOptArg.Exist.pl /= 1 or else VOptArg.pl.Exist.d /= 0
     or else VOptArg.pl.e /= 5 or else VOptArg.tail /= 7
   then
      Fail ("OptArgPDU: value");
   end if;
   --  the same with d present (60 A0 E0): d's size is header.n, not in the stream (--acn-v2 today: size 0, Issue 107)
#if ACN_V2 then
   OptArg ("OptArgPDU d", (16#60#, 16#A0#, 16#E0#), 0, VOptArg);
   if VOptArg.pl.Exist.d /= 1 or else VOptArg.pl.d.Length /= 0 or else VOptArg.pl.e /= 5 or else VOptArg.tail /= 7 then
      Fail ("OptArgPDU d: value");
   end if;
#else
   OptArg ("OptArgPDU d", (16#60#, 16#A0#, 16#E0#), WIRE.ERR_ACN_DECODE_OPTARGPDU_PL_D, VOptArg);
#end if;
   --  two ACN fields with one decode local: not reset, the value decoded for a.b-h.more reaches y
   Col ("ColPDU", (1 => 16#E2#), 0, VCol);
   if VCol.Exist.a /= 1 or else VCol.Exist.y /= 1 or else VCol.y /= 1 or else VCol.tail /= 0 then
      Fail ("ColPDU: value");
   end if;
   --  a shared local as present-when field: header absent, data present (its size is not in the stream)
   Col2 ("Col2PDU", (1 => 16#2C#), WIRE.ERR_ACN_DECODE_COL2PDU_DATA, VCol2);
   --  the component is ALWAYS ABSENT
   Abs_Check ("AbsPDU a", (1 => 16#80#), WIRE.ERR_ACN_DECODE_ABSPDU_DATA, VAbs);
   Abs_Check ("AbsPDU c", (1 => 16#00#), 0, VAbs);
   --  component and dependent present when the same field says so
   Same ("SamePDU c", (1 => 16#00#), 0, VSame);
   Ada.Text_IO.Put_Line ("decode checks done");
end Decode_Test;
