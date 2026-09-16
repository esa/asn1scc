with Ada.Assertions;
with Ada.Text_IO;
with adaasn1rtl;
with adaasn1rtl.encoding;
with adaasn1rtl.encoding.acn;

procedure Patch_Contracts is
   use adaasn1rtl;
   use adaasn1rtl.encoding;
   use adaasn1rtl.encoding.acn;
   use type Asn1UInt;

   type Init_Proc is access procedure
     (Bs : in out Bitstream; Det : in out AcnInsertedFieldRef);
   type Patch_Proc is access procedure
     (V : Asn1UInt; Bs : in out Bitstream;
      Det : in out AcnInsertedFieldRef; Result : out ASN1_RESULT);

   procedure Check (Condition : Boolean; Message : String);
   procedure Check (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         raise Program_Error with Message;
      end if;
   end Check;

   procedure Check_Patch
     (Initialize : Init_Proc; Patch : Patch_Proc;
      Value : Asn1UInt; Width : Positive; Expected : OctetBuffer);
   procedure Check_Patch
     (Initialize : Init_Proc; Patch : Patch_Proc;
      Value : Asn1UInt; Width : Positive; Expected : OctetBuffer)
   is
      Bs : Bitstream := BitStream_init (Expected'Length);
      Det : AcnInsertedFieldRef;
      Result : ASN1_RESULT;
      Rejected : Boolean := False;
   begin
      --  The slot lies between sentinel bytes. Initialization/patching of
      --  a partial byte must preserve the other bits in that byte as well.
      Bs.Buffer := (others => 165);
      Bs.Current_Bit_Pos := 8;
      Initialize (Bs, Det);
      Check (Bs.Current_Bit_Pos = 8 + Width, "reserved slot length");
      Bs.Current_Bit_Pos := Bs.Size_In_Bytes * 8;
      Patch (Value, Bs, Det, Result);
      Check (Result.Success and then Det.Is_Set, "patch at end cursor");
      Check (Bs.Buffer = Expected, "patched wire bytes and sentinels");
      Check (Bs.Current_Bit_Pos = Bs.Size_In_Bytes * 8 and then
        Det.Pos.Bit_Pos = 8, "patch must restore end cursor");
      Patch (Value, Bs, Det, Result);
      Check (Result.Success, "same determinant value twice");
      Patch (0, Bs, Det, Result);
      Check (not Result.Success and then
        Result.ErrorCode = ERR_ACN_DET_CONSISTENCY_MISMATCH,
        "different determinant value must be rejected");
      Check (Bs.Buffer = Expected and then
        Bs.Current_Bit_Pos = Bs.Size_In_Bytes * 8, "repeat patch unchanged");

      Det.Is_Set := False;
      Det.Pos.Bit_Pos := Bs.Size_In_Bytes * 8;
      begin
         Patch (Value, Bs, Det, Result);
      exception
         when Ada.Assertions.Assertion_Error => Rejected := True;
      end;
      Check (Rejected and then Bs.Buffer = Expected, "invalid slot bound");
      Rejected := False;
      Det.Pos.Bit_Pos := 8;
      Bs.Current_Bit_Pos := Bs.Size_In_Bytes * 8 + 1;
      begin
         Patch (Value, Bs, Det, Result);
      exception
         when Ada.Assertions.Assertion_Error => Rejected := True;
      end;
      Check (Rejected and then Bs.Buffer = Expected, "invalid saved cursor");
   end Check_Patch;

   procedure Init_Unsigned
     (Bs : in out Bitstream; Det : in out AcnInsertedFieldRef);
   procedure Init_Unsigned
     (Bs : in out Bitstream; Det : in out AcnInsertedFieldRef) is
   begin
      Acn_InitDet_ConstSize (Bs, Det, 5);
   end Init_Unsigned;

   procedure Patch_Unsigned
     (V : Asn1UInt; Bs : in out Bitstream;
      Det : in out AcnInsertedFieldRef; Result : out ASN1_RESULT);
   procedure Patch_Unsigned
     (V : Asn1UInt; Bs : in out Bitstream;
      Det : in out AcnInsertedFieldRef; Result : out ASN1_RESULT) is
   begin
      Acn_PatchDet_ConstSize (V, Bs, Det, 5, Result);
   end Patch_Unsigned;

   procedure Init_Signed
     (Bs : in out Bitstream; Det : in out AcnInsertedFieldRef);
   procedure Init_Signed
     (Bs : in out Bitstream; Det : in out AcnInsertedFieldRef) is
   begin
      Acn_InitDet_TwosComplement_ConstSize (Bs, Det, 5);
   end Init_Signed;

   procedure Patch_Signed
     (V : Asn1UInt; Bs : in out Bitstream;
      Det : in out AcnInsertedFieldRef; Result : out ASN1_RESULT);
   procedure Patch_Signed
     (V : Asn1UInt; Bs : in out Bitstream;
      Det : in out AcnInsertedFieldRef; Result : out ASN1_RESULT) is
   begin
      Acn_PatchDet_TwosComplement_ConstSize (V, Bs, Det, 5, Result);
   end Patch_Signed;

   procedure Init_String
     (Bs : in out Bitstream; Det : in out AcnInsertedFieldRef);
   procedure Init_String
     (Bs : in out Bitstream; Det : in out AcnInsertedFieldRef) is
   begin
      Acn_InitDet_IA5String_FixSize (Bs, Det, 1);
   end Init_String;

   procedure Patch_String
     (V : Asn1UInt; Bs : in out Bitstream;
      Det : in out AcnInsertedFieldRef; Result : out ASN1_RESULT);
   procedure Patch_String
     (V : Asn1UInt; Bs : in out Bitstream;
      Det : in out AcnInsertedFieldRef; Result : out ASN1_RESULT) is
      Text : constant String (1 .. 1) :=
        (1 => Character'Val (Integer (V)));
   begin
      Acn_PatchDet_IA5String_FixSize (Text, Bs, 1, Det, Result);
   end Patch_String;
begin
   Check_Patch (Acn_InitDet_U8'Access, Acn_PatchDet_U8'Access,
                16#12#, 8, (165, 16#12#, 165));
   Check_Patch (Acn_InitDet_U16_BE'Access, Acn_PatchDet_U16_BE'Access,
                16#1234#, 16, (165, 16#12#, 16#34#, 165));
   Check_Patch (Acn_InitDet_U16_LE'Access, Acn_PatchDet_U16_LE'Access,
                16#1234#, 16, (165, 16#34#, 16#12#, 165));
   Check_Patch (Acn_InitDet_U32_BE'Access, Acn_PatchDet_U32_BE'Access,
                16#12345678#, 32,
                (165, 16#12#, 16#34#, 16#56#, 16#78#, 165));
   Check_Patch (Acn_InitDet_U32_LE'Access, Acn_PatchDet_U32_LE'Access,
                16#12345678#, 32,
                (165, 16#78#, 16#56#, 16#34#, 16#12#, 165));
   Check_Patch (Acn_InitDet_U64_BE'Access, Acn_PatchDet_U64_BE'Access,
                16#0123456789ABCDEF#, 64,
                (165, 1, 16#23#, 16#45#, 16#67#,
                 16#89#, 16#AB#, 16#CD#, 16#EF#, 165));
   Check_Patch (Acn_InitDet_U64_LE'Access, Acn_PatchDet_U64_LE'Access,
                16#0123456789ABCDEF#, 64,
                (165, 16#EF#, 16#CD#, 16#AB#, 16#89#,
                 16#67#, 16#45#, 16#23#, 1, 165));
   Check_Patch (Acn_InitDet_I8'Access, Acn_PatchDet_I8'Access,
                Asn1UInt'Last - 1, 8, (165, 254, 165));
   Check_Patch (Acn_InitDet_I16_BE'Access, Acn_PatchDet_I16_BE'Access,
                Asn1UInt'Last - 1, 16, (165, 255, 254, 165));
   Check_Patch (Acn_InitDet_I32_BE'Access, Acn_PatchDet_I32_BE'Access,
                Asn1UInt'Last - 1, 32, (165, 255, 255, 255, 254, 165));
   Check_Patch (Acn_InitDet_I64_BE'Access, Acn_PatchDet_I64_BE'Access,
                Asn1UInt'Last - 1, 64,
                (165, 255, 255, 255, 255, 255, 255, 255, 254, 165));
   Check_Patch (Acn_InitDet_BOOL1'Access, Acn_PatchDet_BOOL1'Access,
                1, 1, (165, 165, 165));
   Check_Patch (Init_Unsigned'Access, Patch_Unsigned'Access,
                21, 5, (165, 16#AD#, 165));
   Check_Patch (Init_Signed'Access, Patch_Signed'Access,
                Asn1UInt'Last - 2, 5, (165, 16#ED#, 165));
   Check_Patch (Init_String'Access, Patch_String'Access,
                65, 7, (165, 16#83#, 165));
   Ada.Text_IO.Put_Line ("15 patch contracts: bytes, cursor, guards OK");
end Patch_Contracts;
