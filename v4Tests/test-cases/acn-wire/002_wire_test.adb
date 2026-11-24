--  ACN fields of a referenced unsigned INTEGER type encoded as ASCII (run by v4Tests/scripts/runWireTests.sh, -gnata):
--  the encoders write the digits without a sign, as a component of the type does, and the decoders read them back.
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl;
with adaasn1rtl.encoding;
with WIRE;
procedure Wire_Test is
   use type adaasn1rtl.Asn1Byte;

   generic
      type T is private;
      with procedure Encode
        (V : T; S : in out adaasn1rtl.encoding.Bitstream; R : out adaasn1rtl.ASN1_RESULT);
      with procedure Decode
        (V : out T; S : in out adaasn1rtl.encoding.Bitstream; R : out adaasn1rtl.ASN1_RESULT);
      with function Equal (A, B : T) return Boolean;
      Init : T;
   procedure Check (Name : String; V : T; Expected : String);

   procedure Check (Name : String; V : T; Expected : String) is
      S : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (64);
      D : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (64);
      W : T := Init;
      R : adaasn1rtl.ASN1_RESULT;
      Ok : Boolean;
   begin
      Encode (V, S, R);
      Ok := R.Success and then S.Current_Bit_Pos = 8 * Expected'Length;
      for I in Expected'Range loop
         Ok := Ok and then S.Buffer (I - Expected'First + 1) = Character'Pos (Expected (I));
      end loop;
      if not Ok then
         Ada.Text_IO.Put_Line (Name & ": unexpected encoding");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
      D.Buffer := S.Buffer;
      Decode (W, D, R);
      if not R.Success or else not Equal (W, V) then
         Ada.Text_IO.Put_Line (Name & ": unexpected decoding");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   exception
      when others =>
         Ada.Text_IO.Put_Line (Name & ": exception");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Check;

   --  the _aux decoders of Fn / Fc take val in out (the wrappers T_ACN_Decode set it first)
   procedure Fn_Dec
     (V : out WIRE.Fn; S : in out adaasn1rtl.encoding.Bitstream; R : out adaasn1rtl.ASN1_RESULT) is
   begin
      V := WIRE.Fn_Init;
      WIRE.Fn_ACN_Decode_aux (V, S, R);
   end Fn_Dec;

   procedure Fc_Dec
     (V : out WIRE.Fc; S : in out adaasn1rtl.encoding.Bitstream; R : out adaasn1rtl.ASN1_RESULT) is
   begin
      V := WIRE.Fc_Init;
      WIRE.Fc_ACN_Decode_aux (V, S, R);
   end Fc_Dec;

   procedure Fn is new Check (WIRE.Fn, WIRE.Fn_ACN_Encode_aux, Fn_Dec, WIRE.Fn_Equal, WIRE.Fn_Init);
   procedure Fc is new Check (WIRE.Fc, WIRE.Fc_ACN_Encode_aux, Fc_Dec, WIRE.Fc_Equal, WIRE.Fc_Init);
   procedure Cm is new Check (WIRE.Cm, WIRE.Cm_ACN_Encode_aux, WIRE.Cm_ACN_Decode_aux, WIRE.Cm_Equal, WIRE.Cm_Init);

   N : constant Character := Character'Val (0);
   VFn : WIRE.Fn := WIRE.Fn_Init;
   VFc : WIRE.Fc := WIRE.Fc_Init;
   VCm : constant WIRE.Cm := (a => 2);
begin
   VFn.s.Length := 2;
   VFn.s.Data (1) := Character'Pos ('A');
   VFn.s.Data (2) := Character'Pos ('B');
   VFc.s.Length := 2;
   VFc.s.Data (1) := Character'Pos ('A');
   VFc.s.Data (2) := Character'Pos ('B');
   Fn ("Fn", VFn, "2" & N & "AB");
   Fc ("Fc", VFc, "02AB");
   Cm ("Cm", VCm, "2" & N);
   VFn.s.Length := 12;
   VFn.s.Data (1 .. 12) := (others => Character'Pos ('x'));
   Fn ("Fn12", VFn, "12" & N & "xxxxxxxxxxxx");
end Wire_Test;
