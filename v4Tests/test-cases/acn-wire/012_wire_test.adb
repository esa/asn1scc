--  Dependents without bits present when a field of an absent component says so (run by
--  v4Tests/scripts/runWireTests.sh, -gnata, legacy and --acn-v2): the encoder fails with the dependent's error code
--  right after flg (bit 1); it used to succeed (q/issues.md Issue 108). ZeroPDU_Init sets every OPTIONAL component
--  present: the values clear the Exist bits they want absent.
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl;
with adaasn1rtl.encoding;
with ZERO;
procedure Wire_Test is
   procedure Fail (Msg : String) is
   begin
      Ada.Text_IO.Put_Line (Msg);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Fail;

   procedure Check (Name : String; V : ZERO.ZeroPDU; Exp_Err : Integer; Exp_Bit : Natural) is
      S : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (ZERO.ZeroPDU_REQUIRED_BYTES_FOR_ACN_ENCODING);
      R : adaasn1rtl.ASN1_RESULT;
   begin
      ZERO.ZeroPDU_ACN_Encode_aux (V, S, R);
      if (if Exp_Err = 0 then not R.Success or else R.ErrorCode /= 0
          else R.Success or else R.ErrorCode /= Exp_Err or else S.Current_Bit_Pos /= Exp_Bit)
      then
         Fail (Name & ": Success=" & Boolean'Image (R.Success) & " ErrorCode=" & Integer'Image (R.ErrorCode) &
               " at bit" & Natural'Image (S.Current_Bit_Pos));
      end if;
   exception
      when others => Fail (Name & ": exception");
   end Check;

   V : ZERO.ZeroPDU := ZERO.ZeroPDU_Init;
begin
   V.Exist.header := 0;
   V.Exist.z := 0;
   V.Exist.e := 0;
   Check ("all absent", V, 0, 0);
   V.Exist.header := 1;
   V.Exist.z := 1;
   V.Exist.e := 1;
   Check ("all present", V, 0, 0);
   V.Exist.header := 0;
   Check ("z without header", V, ZERO.ERR_ACN_ENCODE_ZEROPDU_Z, 1);
   V.Exist.z := 0;
   Check ("e without header", V, ZERO.ERR_ACN_ENCODE_ZEROPDU_E, 1);
   Ada.Text_IO.Put_Line ("012 checks done");
end Wire_Test;
