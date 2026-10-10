with Interfaces; use Interfaces;

package body adaasn1rtl with
   Spark_Mode
is

   function getStringSize (str : String) return Integer is
      length : Integer := 0;
   begin
      for i in str'Range loop
         pragma Loop_Invariant (length = length'Loop_Entry + (i - str'First));
         exit when str (i) = Standard.Ascii.NUL;
         length := length + 1;
      end loop;

      return length;
   end getStringSize;

   function GetZeroBasedCharIndex
     (CharToSearch : Character; AllowedCharSet : String) return Integer
   is
      ret : Integer;
   begin
      ret := 0;
      for I in Integer range AllowedCharSet'Range loop
         ret := I - AllowedCharSet'First;
         pragma Loop_Invariant
           (AllowedCharSet'Last >= AllowedCharSet'First and
            AllowedCharSet'Last <= Integer'Last - 1 and
            ret = I - AllowedCharSet'First);
         exit when CharToSearch = AllowedCharSet (I);
      end loop;
      return ret;
   end GetZeroBasedCharIndex;

   function CharacterPos (C : Character) return Integer is
      ret : Integer;
   begin
      ret := Character'Pos (C);
      if not (ret >= 0 and ret <= 127) then
         ret := 0;
      end if;
      return ret;
   end CharacterPos;

   procedure ObjectIdentifier_Init (val : out Asn1ObjectIdentifier) is
   begin
      val.Length := 0;
      val.values := ObjectIdentifier_array'(others => 0);
   end ObjectIdentifier_Init;

   function ObjectIdentifier_isValid
     (val : Asn1ObjectIdentifier) return Boolean
   is
   begin
      return val.Length >= 2 and then val.values (1) <= 2
        and then val.values (2) <= 39;
   end ObjectIdentifier_isValid;

   function RelativeOID_isValid (val : Asn1ObjectIdentifier) return Boolean is
   begin
      return val.Length > 0;
   end RelativeOID_isValid;

   function ObjectIdentifier_equal
     (val1 : Asn1ObjectIdentifier; val2 : Asn1ObjectIdentifier) return Boolean
   is
      ret : Boolean;
      i   : Integer;
   begin
      ret := val1.Length = val2.Length;
      i   := 1;
      while ret and i <= val1.Length loop
         pragma Loop_Invariant
           (i >= 1 and i <= val1.Length and val1.Length = val2.Length);
         ret := val1.values (i) = val2.values (i);
         i   := i + 1;
      end loop;

      return ret;
   end ObjectIdentifier_equal;

   function String_Equal (Left, Right : String) return Boolean is
      len1 : constant Integer := getStringSize (Left);
      len2 : constant Integer := getStringSize (Right);
   begin
      return len1 = len2
        and then Left (Left'First .. Left'First + (len1 - 1)) =
          Right (Right'First .. Right'First + (len1 - 1));
   end String_Equal;

   procedure BitString_BitArray_To_Bytes
     (arr : BitArray; len : Integer; bytes : out OctetBuffer)
   is
   begin
      bytes := (others => 0);
      for k in 0 .. len - 1 loop
         if arr (arr'First + k) = 1 then
            bytes (bytes'First + k / 8) :=
              bytes (bytes'First + k / 8) or
              Shift_Left (Asn1Byte (1), Natural (7 - (k mod 8)));
         end if;
      end loop;
   end BitString_BitArray_To_Bytes;

   procedure BitString_Bytes_To_BitArray
     (bytes : OctetBuffer; len : Integer; arr : out BitArray)
   is
   begin
      for k in 0 .. len - 1 loop
         if (bytes (bytes'First + k / 8) and
             Shift_Left (Asn1Byte (1), Natural (7 - (k mod 8)))) /= 0
         then
            arr (arr'First + k) := 1;
         else
            arr (arr'First + k) := 0;
         end if;
      end loop;
   end BitString_Bytes_To_BitArray;

   function BitString_Get_Bit (arr : OctetBuffer; k : Natural) return BIT is
   begin
      if (arr (arr'First + k / 8) and
          Shift_Right (Asn1Byte (16#80#), k mod 8)) /= 0
      then
         return 1;
      end if;
      return 0;
   end BitString_Get_Bit;

   procedure BitString_Set_Bit
     (arr : in out OctetBuffer; k : Natural; v : BIT)
   is
      mask : constant Asn1Byte := Shift_Right (Asn1Byte (16#80#), k mod 8);
   begin
      if v = 1 then
         arr (arr'First + k / 8) := arr (arr'First + k / 8) or mask;
      else
         arr (arr'First + k / 8) := arr (arr'First + k / 8) and not mask;
      end if;
   end BitString_Set_Bit;

   function BitString_Bytes_Equal
     (len1 : Integer; len2 : Integer; arr1 : OctetBuffer; arr2 : OctetBuffer)
      return Boolean
   is
      nBytes : constant Integer := len1 / 8;
      nRest  : constant Integer := len1 mod 8;
   begin
      if len1 /= len2 then
         return False;
      end if;
      for j in 0 .. nBytes - 1 loop
         if arr1 (arr1'First + j) /= arr2 (arr2'First + j) then
            return False;
         end if;
      end loop;
      if nRest > 0 then
         return Shift_Right (arr1 (arr1'First + nBytes), 8 - nRest) =
           Shift_Right (arr2 (arr2'First + nBytes), 8 - nRest);
      end if;
      return True;
   end BitString_Bytes_Equal;

end adaasn1rtl;
