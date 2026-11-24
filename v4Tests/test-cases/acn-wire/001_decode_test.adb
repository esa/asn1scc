--  ACN INTEGER as ASCII digits with a terminator (run by v4Tests/scripts/runWireTests.sh, x86 and msp430, -gnata: the
--  RTL contracts are checked too): a value outside the range of the type, or a digit string beyond the integer type
--  (it used to wrap around), is an error code, never Constraint_Error.
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with adaasn1rtl;
with adaasn1rtl.encoding;
with WIRE;
procedure Decode_Test is
   use type adaasn1rtl.Asn1UInt;

   function Trim (S : String) return String is
     (Ada.Strings.Fixed.Trim (S, Ada.Strings.Both));

   --  the image a decoded value has: without the '+' of the stream
   function No_Plus (S : String) return String is
     (if S'Length > 0 and then S (S'First) = '+'
      then S (S'First + 1 .. S'Last) else S);

   --  the decimal image of a value ending in 5 or 7, plus K (no carry)
   function Plus (Img : String; K : Natural) return String is
     (Img (Img'First .. Img'Last - 1) &
      Character'Val (Character'Pos (Img (Img'Last)) + K));

   UMax : constant String := Trim (adaasn1rtl.Asn1UInt'Image (adaasn1rtl.Asn1UInt'Last));
   SMax : constant String := Trim (adaasn1rtl.Asn1Int'Image (adaasn1rtl.Asn1Int'Last));

   generic
      type T is (<>);
      with procedure Decode
        (V : out T; S : in out adaasn1rtl.encoding.Bitstream; R : out adaasn1rtl.ASN1_RESULT);
   procedure Check (Name, Digits_Text : String; Expected : Boolean);

   procedure Check (Name, Digits_Text : String; Expected : Boolean) is
      S : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (32);
      V : T;
      R : adaasn1rtl.ASN1_RESULT;
   begin
      for I in Digits_Text'Range loop
         S.Buffer (I - Digits_Text'First + 1) := Character'Pos (Digits_Text (I));
      end loop;
      Decode (V, S, R);
      if R.Success /= Expected or else (not R.Success and then R.ErrorCode = 0)
        or else (R.Success and then Trim (T'Image (V)) /= No_Plus (Digits_Text))
      then
         Ada.Text_IO.Put_Line (Name & ": unexpected result for " & Digits_Text);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   exception
      when others =>
         Ada.Text_IO.Put_Line (Name & ": exception for " & Digits_Text);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Check;

   procedure Un  is new Check (WIRE.Un,  WIRE.Un_ACN_Decode_aux);
   procedure Nr  is new Check (WIRE.Nr,  WIRE.Nr_ACN_Decode_aux);
   procedure Sg  is new Check (WIRE.Sg,  WIRE.Sg_ACN_Decode_aux);
   procedure Big is new Check (WIRE.Big, WIRE.Big_ACN_Decode_aux);
   procedure Any is new Check (WIRE.Any, WIRE.Any_ACN_Decode_aux);
begin
   Un ("Un", "0", True);
   Un ("Un", "255", True);
   Un ("Un", "256", False);
   Un ("Un", "999", False);
   Nr ("Nr", "10", True);
   Nr ("Nr", "20", True);
   Nr ("Nr", "9", False);
   Nr ("Nr", "21", False);
   Sg ("Sg", "+100", True);
   Sg ("Sg", "-100", True);
   Sg ("Sg", "+101", False);
   Sg ("Sg", "-101", False);
   Sg ("Sg", "100", False);
   Big ("Big", UMax, True);
   Big ("Big", Plus (UMax, 1), False);   --  2 ** N: wrapped to 0
   Big ("Big", Plus (UMax, 4), False);   --  wrapped to 3
   Big ("Big", (UMax'Range => '9'), False);
   Any ("Any", "+" & SMax, True);
   Any ("Any", "-" & Plus (SMax, 1), True);
   Any ("Any", "-" & Plus (SMax, 2), False);
   Any ("Any", "+" & Plus (SMax, 1), False);
end Decode_Test;
