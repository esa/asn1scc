--  A determinant field inside an OPTIONAL component, its dependent outside it (run by v4Tests/scripts/runWireTests.sh,
--  -gnata, legacy and --acn-v2). A value whose component is absent while the dependent is present cannot be encoded:
--  the encoder must fail with a nonzero error code (the dependent's own where it is public), before it writes the
--  dependent (q/issues.md Issue 108, rule R4; the legacy CONTAINING used a length it never set: Constraint_Error, with
--  -gnatp a slice write of that length). Valid values with the component absent still encode. Built with
--  -gnateDACN_V2=True for --acn-v2, False for legacy (GNAT integrated preprocessor). T_Init sets every OPTIONAL
--  component present: each value clears the Exist bits it wants absent.
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl;
with adaasn1rtl.encoding;
with WIRE;
procedure Encode_Test is
   procedure Fail (Msg : String) is
   begin
      Ada.Text_IO.Put_Line (Msg);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Fail;

   --  encode V: failure with Exp_Err (Exp_Err > 0), failure with any nonzero code (Exp_Err = -1), or success (Exp_Err
   --  = 0)
   generic
      type T is private;
      Size : Positive;
      with procedure Encode
        (V : T; S : in out adaasn1rtl.encoding.Bitstream; R : out adaasn1rtl.ASN1_RESULT);
   procedure Check (Name : String; V : T; Exp_Err : Integer);

   procedure Check (Name : String; V : T; Exp_Err : Integer) is
      S : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (Size);
      R : adaasn1rtl.ASN1_RESULT;
      Ok : Boolean;
   begin
      Encode (V, S, R);
      case Exp_Err is
         when 0      => Ok := R.Success;
         when -1     => Ok := not R.Success and then R.ErrorCode /= 0;
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

   --  as Check (Exp_Err /= 0), and the encoder stopped at bit Exp_Bit (nothing of the dependent written)
   generic
      type T is private;
      Size : Positive;
      with procedure Encode
        (V : T; S : in out adaasn1rtl.encoding.Bitstream; R : out adaasn1rtl.ASN1_RESULT);
   procedure Check_At (Name : String; V : T; Exp_Err : Integer; Exp_Bit : Natural);

   procedure Check_At (Name : String; V : T; Exp_Err : Integer; Exp_Bit : Natural) is
      S : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (Size);
      R : adaasn1rtl.ASN1_RESULT;
   begin
      Encode (V, S, R);
      if R.Success or else R.ErrorCode = 0 or else (Exp_Err > 0 and then R.ErrorCode /= Exp_Err)
        or else S.Current_Bit_Pos /= Exp_Bit
      then
         Fail (Name & ": Success=" & Boolean'Image (R.Success) & " ErrorCode=" & Integer'Image (R.ErrorCode) &
               " at bit" & Natural'Image (S.Current_Bit_Pos) & ", expected" & Integer'Image (Exp_Err) & " at bit" &
               Natural'Image (Exp_Bit));
      end if;
   exception
      when others =>
         Fail (Name & ": exception");
   end Check_At;

   procedure My is new Check (WIRE.MyPDU, WIRE.MyPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.MyPDU_ACN_Encode_aux);
   procedure Ch is new Check (WIRE.ChPDU, WIRE.ChPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.ChPDU_ACN_Encode_aux);
   procedure RefCh is new Check (WIRE.RefChPDU, WIRE.RefChPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.RefChPDU_ACN_Encode_aux);
   procedure Pw is new Check (WIRE.PwPDU, WIRE.PwPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.PwPDU_ACN_Encode_aux);
   procedure PwStr is new Check (WIRE.PwStrPDU, WIRE.PwStrPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.PwStrPDU_ACN_Encode_aux);
   procedure Bool is new Check (WIRE.BoolPDU, WIRE.BoolPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.BoolPDU_ACN_Encode_aux);
   procedure Sq is new Check (WIRE.SqPDU, WIRE.SqPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.SqPDU_ACN_Encode_aux);
   procedure Elem is new Check (WIRE.ElemPDU, WIRE.ElemPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.ElemPDU_ACN_Encode_aux);
   procedure Nest is new Check (WIRE.NestPDU, WIRE.NestPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.NestPDU_ACN_Encode_aux);
   procedure Alt is new Check (WIRE.AltPDU, WIRE.AltPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.AltPDU_ACN_Encode_aux);
   procedure Cont is new Check (WIRE.ContPDU, WIRE.ContPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.ContPDU_ACN_Encode_aux);
   procedure Arg is new Check (WIRE.ArgPDU, WIRE.ArgPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.ArgPDU_ACN_Encode_aux);
   procedure OptArg is new Check (WIRE.OptArgPDU, WIRE.OptArgPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.OptArgPDU_ACN_Encode_aux);
   procedure Abs_Check is new Check (WIRE.AbsPDU, WIRE.AbsPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.AbsPDU_ACN_Encode_aux);
   procedure Same is new Check (WIRE.SamePDU, WIRE.SamePDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.SamePDU_ACN_Encode_aux);
   procedure Def is new Check (WIRE.DefPDU, WIRE.DefPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.DefPDU_ACN_Encode_aux);
   procedure Def_At is new Check_At (WIRE.DefPDU, WIRE.DefPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.DefPDU_ACN_Encode_aux);
   procedure Al is new Check (WIRE.AlPDU, WIRE.AlPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.AlPDU_ACN_Encode_aux);
   procedure Al_At is new Check_At (WIRE.AlPDU, WIRE.AlPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.AlPDU_ACN_Encode_aux);
   procedure StrOnly is new Check (WIRE.StrOnlyPDU, WIRE.StrOnlyPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.StrOnlyPDU_ACN_Encode_aux);
   procedure StrOnly_At is new Check_At (WIRE.StrOnlyPDU, WIRE.StrOnlyPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.StrOnlyPDU_ACN_Encode_aux);
   procedure EnCh is new Check (WIRE.EnChPDU, WIRE.EnChPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.EnChPDU_ACN_Encode_aux);
   procedure AlRef is new Check (WIRE.AlRefPDU, WIRE.AlRefPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.AlRefPDU_ACN_Encode_aux);
   procedure AlRef_At is new Check_At (WIRE.AlRefPDU, WIRE.AlRefPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.AlRefPDU_ACN_Encode_aux);
   procedure EnCh_At is new Check_At (WIRE.EnChPDU, WIRE.EnChPDU_REQUIRED_BYTES_FOR_ACN_ENCODING, WIRE.EnChPDU_ACN_Encode_aux);

   VMy : WIRE.MyPDU := WIRE.MyPDU_Init;
   VCh : WIRE.ChPDU := WIRE.ChPDU_Init;
   VRefCh : WIRE.RefChPDU := WIRE.RefChPDU_Init;
   VPw : WIRE.PwPDU := WIRE.PwPDU_Init;
   VPwStr : WIRE.PwStrPDU := WIRE.PwStrPDU_Init;
   VBool : WIRE.BoolPDU := WIRE.BoolPDU_Init;
   VSq : WIRE.SqPDU := WIRE.SqPDU_Init;
   VElem : WIRE.ElemPDU := WIRE.ElemPDU_Init;
   VNest : WIRE.NestPDU := WIRE.NestPDU_Init;
   VAlt : WIRE.AltPDU := WIRE.AltPDU_Init;
   VCont : WIRE.ContPDU := WIRE.ContPDU_Init;
   VArg : WIRE.ArgPDU := WIRE.ArgPDU_Init;
   VOptArg : WIRE.OptArgPDU := WIRE.OptArgPDU_Init;
   VAbs : WIRE.AbsPDU := WIRE.AbsPDU_Init;
   VSame : WIRE.SamePDU := WIRE.SamePDU_Init;
   VDef : WIRE.DefPDU := WIRE.DefPDU_Init;
   VAl : WIRE.AlPDU := WIRE.AlPDU_Init;
   VStrOnly : WIRE.StrOnlyPDU := WIRE.StrOnlyPDU_Init;
   VEnCh : WIRE.EnChPDU := WIRE.EnChPDU_Init;
   VAlRef : WIRE.AlRefPDU := WIRE.AlRefPDU_Init;
begin
   --  sizes of an OCTET STRING and of an IA5String, both present when flg2 says so: data fails first
   VMy.Exist.header := 0;
   VMy.Exist.data := 0;
   VMy.Exist.str := 0;
   My ("MyPDU ok", VMy, 0);
   VMy.Exist.data := 1;
   VMy.data.Length := 2;
   VMy.Exist.str := 1;
   My ("MyPDU r", VMy, WIRE.ERR_ACN_ENCODE_MYPDU_DATA);
   --  CHOICE determinant, inline and of a referenced CHOICE (an ACN argument)
   VCh.Exist.header := 0;
   VCh.ch := (kind => WIRE.c2_PRESENT, c2 => True);
   Ch ("ChPDU r", VCh, WIRE.ERR_ACN_ENCODE_CHPDU_CH);
   VRefCh.Exist.header := 0;
   VRefCh.ch := (kind => WIRE.two_PRESENT, two => True);
   RefCh ("RefChPDU r", VRefCh, -1);
   --  CHOICE present-when an INTEGER field, a string field
   VPw.Exist.header := 0;
   VPw.cp := (kind => WIRE.p1_PRESENT, p1 => 5);
   Pw ("PwPDU r", VPw, WIRE.ERR_ACN_ENCODE_PWPDU_CP);
   VPwStr.Exist.header := 0;
   VPwStr.cs := (kind => WIRE.s2_PRESENT, s2 => True);
   PwStr ("PwStrPDU r", VPwStr, WIRE.ERR_ACN_ENCODE_PWSTRPDU_CS);
   --  present-when a BOOLEAN field of the component: x present without it; the field passed to p, whose y is present
   --  when it says so (--acn-v2: y's code is private to the package body)
   VBool.Exist.header := 0;
   VBool.Exist.x := 0;
   VBool.p.Exist.y := 0;
   Bool ("BoolPDU ok", VBool, 0);
   VBool.Exist.x := 1;
   Bool ("BoolPDU x", VBool, WIRE.ERR_ACN_ENCODE_BOOLPDU_X);
   VBool.Exist.x := 0;
   VBool.p.Exist.y := 1;
#if ACN_V2 then
   Bool ("BoolPDU p.y", VBool, -1);
#else
   Bool ("BoolPDU p.y", VBool, WIRE.ERR_ACN_ENCODE_BOOLPDU_P_Y);
#end if;
   --  SEQUENCE OF elements: element 2 has no component but its dependent
   VSq.Data (1).x.Length := 1;
   VSq.Data (2).Exist.h := 0;
   VSq.Data (2).Exist.x := 0;
   Sq ("SqPDU ok", VSq, 0);
   VSq.Data (2).Exist.x := 1;
   Sq ("SqPDU r", VSq, WIRE.ERR_ACN_ENCODE_SQPDU_ELM_X);
   VElem.elems.Length := 2;
   VElem.elems.Data (1).dd.Length := 1;
   VElem.elems.Data (2).Exist.hh := 0;
   VElem.elems.Data (2).Exist.dd := 0;
   Elem ("ElemPDU ok", VElem, 0);
   VElem.elems.Data (2).Exist.dd := 1;
   Elem ("ElemPDU r", VElem, WIRE.ERR_ACN_ENCODE_ELEMPDU_ELEMS_ELM_DD);
   --  nested components: h1 absent; h1 present without h2
   VNest.d.Length := 1;
   Nest ("NestPDU ok", VNest, 0);
   VNest.h1.Exist.h2 := 0;
   Nest ("NestPDU r2", VNest, WIRE.ERR_ACN_ENCODE_NESTPDU_D);
   VNest.Exist.h1 := 0;
   Nest ("NestPDU r1", VNest, WIRE.ERR_ACN_ENCODE_NESTPDU_D);
   --  the dependent is an alternative of a mandatory CHOICE
   VAlt.Exist.header := 0;
   VAlt.alt := (kind => WIRE.o2_PRESENT, o2 => 0);
   Alt ("AltPDU ok", VAlt, 0);
   VAlt.alt := (kind => WIRE.o1_PRESENT, o1 => (Length => 1, Data => (others => 0)));
   Alt ("AltPDU r", VAlt, WIRE.ERR_ACN_ENCODE_ALTPDU_ALT_O1);
   --  a CONTAINING sized by the field: legacy used a length it never set
   VCont.Exist.header := 0;
   VCont.Exist.cont := 0;
   Cont ("ContPDU ok", VCont, 0);
   VCont.Exist.cont := 1;
   Cont ("ContPDU r", VCont, -1);
   --  the field as an ACN argument (--acn-v2: the callee tests the reference; its code is private to the package body)
   VArg.Exist.header := 0;
   VArg.pl.d.Length := 1;
#if ACN_V2 then
   Arg ("ArgPDU r", VArg, -1);
#else
   Arg ("ArgPDU r", VArg, WIRE.ERR_ACN_ENCODE_ARGPDU_PL_D);
#end if;
   VOptArg.Exist.header := 0;
   VOptArg.pl.Exist.d := 0;
   OptArg ("OptArgPDU ok", VOptArg, 0);
   VOptArg.pl.Exist.d := 1;
   VOptArg.pl.d.Length := 1;
#if ACN_V2 then
   OptArg ("OptArgPDU r", VOptArg, -1);
#else
   OptArg ("OptArgPDU r", VOptArg, WIRE.ERR_ACN_ENCODE_OPTARGPDU_PL_D);
#end if;
   --  the component is ALWAYS ABSENT: a present dependent always fails
   VAbs.Exist.header := 0;
   VAbs.Exist.data := 0;
   Abs_Check ("AbsPDU ok", VAbs, 0);
   VAbs.Exist.data := 1;
   Abs_Check ("AbsPDU r", VAbs, WIRE.ERR_ACN_ENCODE_ABSPDU_DATA);
   --  the dependent present when the same field says so as the component, the field also passed to p: legacy's update
   --  of f rejects before f; --acn-v2 defers f, so d's guard rejects after f's bit, before d
   VDef.Exist.header := 0;
   VDef.Exist.d := 0;
   VDef.p.Exist.y := 0;
   Def ("DefPDU ok", VDef, 0);
   VDef.Exist.d := 1;
   VDef.d.Length := 1;
#if ACN_V2 then
   Def_At ("DefPDU r", VDef, WIRE.ERR_ACN_ENCODE_DEFPDU_D, 1);
#else
   Def_At ("DefPDU r", VDef, WIRE.ERR_ACN_ENCODE_UPDATE_DEFPDU_F, 0);
#end if;
   --  an aligned dependent: rejected after flg and g, before the padding
   VAl.Exist.header := 0;
   VAl.Exist.d := 0;
   Al ("AlPDU ok", VAl, 0);
   VAl.Exist.d := 1;
   VAl.d.Length := 1;
   Al_At ("AlPDU r", VAl, WIRE.ERR_ACN_ENCODE_ALPDU_D, 2);
   --  an IA5String size
   VStrOnly.Exist.header := 0;
   VStrOnly.Exist.str := 0;
   StrOnly ("StrOnlyPDU ok", VStrOnly, 0);
   VStrOnly.Exist.str := 1;
   StrOnly_At ("StrOnlyPDU r", VStrOnly, WIRE.ERR_ACN_ENCODE_STRONLYPDU_STR, 2);
   --  a referenced CHOICE whose ENUMERATED determinant is an ACN argument
   VEnCh.Exist.header := 0;
   VEnCh.Exist.ch := 0;
   EnCh ("EnChPDU ok", VEnCh, 0);
   VEnCh.Exist.ch := 1;
   VEnCh.ch := (kind => WIRE.e2_PRESENT, e2 => True);
#if ACN_V2 then
   EnCh_At ("EnChPDU r", VEnCh, WIRE.ERR_ACN_ENCODE_ENCHPDU_CH_2_2, 2);
#else
   EnCh_At ("EnChPDU r", VEnCh, WIRE.ERR_ACN_ENCODE_ENCHPDU_CH, 2);
#end if;
   --  referenced dependents aligned at the site: rejected after the four flags (bit 4), before the padding, in both
   --  modes (--acn-v2: the site's code is the call wrapper's, with the generated suffix _2_2)
   VAlRef.Exist.header := 0;
   VAlRef.Exist.ch := 0;
   VAlRef.Exist.d := 0;
   VAlRef.Exist.arr := 0;
   AlRef ("AlRefPDU ok", VAlRef, 0);
   VAlRef.Exist.ch := 1;
   VAlRef.ch := (kind => WIRE.e2_PRESENT, e2 => True);
#if ACN_V2 then
   AlRef_At ("AlRefPDU ch", VAlRef, WIRE.ERR_ACN_ENCODE_ALREFPDU_CH_2_2, 4);
#else
   AlRef_At ("AlRefPDU ch", VAlRef, WIRE.ERR_ACN_ENCODE_ALREFPDU_CH, 4);
#end if;
   VAlRef.Exist.ch := 0;
   VAlRef.Exist.d := 1;
   VAlRef.d.Length := 1;
#if ACN_V2 then
   AlRef_At ("AlRefPDU d", VAlRef, WIRE.ERR_ACN_ENCODE_ALREFPDU_D_2_2, 4);
#else
   AlRef_At ("AlRefPDU d", VAlRef, WIRE.ERR_ACN_ENCODE_ALREFPDU_D, 4);
#end if;
   VAlRef.Exist.d := 0;
   VAlRef.Exist.arr := 1;
   VAlRef.arr.Length := 1;
#if ACN_V2 then
   AlRef_At ("AlRefPDU arr", VAlRef, WIRE.ERR_ACN_ENCODE_ALREFPDU_ARR_2_2, 4);
#else
   AlRef_At ("AlRefPDU arr", VAlRef, WIRE.ERR_ACN_ENCODE_ALREFPDU_ARR, 4);
#end if;
   --  component and dependent present when the same field says so: the field's update rejects the disagreement
   VSame.Exist.header := 0;
   Same ("SamePDU r", VSame, -1);
   Ada.Text_IO.Put_Line ("encode checks done");
end Encode_Test;
