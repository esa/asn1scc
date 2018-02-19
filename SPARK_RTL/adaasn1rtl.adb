with Interfaces;
WITH Ada.Strings;
USE Ada.Strings;

WITH Ada.Strings.Fixed;
USE Ada.Strings.Fixed;

WITH Ada.Strings.Maps;
USE Ada.Strings.Maps;

WITH Ada.Characters.Latin_1;

WITH Ada.Text_IO;
USE  Ada.Text_IO;

WITH Ada.Characters.Handling;
USE Ada.Characters.Handling;

--with Ada.Sequential_IO;
with Ada.Direct_IO;

WITH Ada.Characters.Latin_1;

WITH Ada.Integer_Text_IO;
USE Ada.Characters.Latin_1;

with Ada.Unchecked_Conversion;
use type Interfaces.Integer_32;
use type Interfaces.Integer_16;
use type Interfaces.Unsigned_8;
use type Interfaces.Unsigned_32;
use type Interfaces.Unsigned_16;
use type Interfaces.Integer_8;
use type Interfaces.Unsigned_64;
use type Interfaces.Integer_64;
use Interfaces;

package body adaasn1rtl with
     Spark_Mode is

   MASKS : constant OctetBuffer_8 :=
     OctetBuffer_8'
       (16#80#, 16#40#, 16#20#, 16#10#, 16#08#, 16#04#, 16#02#, 16#01#);
   MSBIT_ONE : constant Asn1UInt := 16#8000000000000000#;

   MSBYTE_FF : constant Asn1UInt := 16#FF00000000000000#;

   MantissaFactor : constant Asn1Real :=
     Asn1Real (Interfaces.Unsigned_64 (2)**Asn1Real'Machine_Mantissa);

   ERR_END_OF_STREAM        : constant Integer := 1001;
   ERR_UNSUPPORTED_ENCODING : constant Integer :=
     1001;  --  Returned when the uPER encoding for REALs is not binary encoding

   function Asn1Real_Equal (Left, Right : in Asn1Real) return Boolean is
      ret : Boolean;
   begin
      if Left = Right then
         ret := True;
      elsif Left = 0.0 then
         ret := Right = 0.0;
      else
         ret := abs ((Left - Right) / Left) < 0.00001;
      end if;
      return ret;
   end Asn1Real_Equal;

   function Asn1Boolean_Equal
     (Left, Right : in Boolean) return Boolean is
     (Left = Right);

   function Asn1Int_Equal
     (Left, Right : in Asn1Int) return Boolean is
     (Left = Right);

   function Asn1NullType_Equal
     (Left, Right : in Asn1NullType) return Boolean
   is
      pragma SPARK_Mode (Off);
   begin
      return True;
   end Asn1NullType_Equal;
   function getStringSize (str : String) return Integer is
      I : Integer := 1;
   begin
      while I <= str'Last and then str (I) /= NUL loop
         pragma Loop_Invariant (I >= 1 and I <= str'Last);
         I := I + 1;
      end loop;
      return I - 1;
   end getStringSize;

   function stringContainsChar (str : String; ch : Character) return Boolean is
      I      : Integer;
      bFound : Boolean := False;
   begin
      I := str'First;
      while I <= str'Last and not bFound loop
         pragma Loop_Invariant (I >= str'First and I <= str'Last);
         bFound := str (I) = ch;
         I      := I + 1;
      end loop;
      return bFound;
   end stringContainsChar;

   function To_Int (IntVal : Asn1UInt) return Asn1Int is
      ret : Asn1Int;
      c   : Asn1UInt;
   begin
      if IntVal > Asn1UInt (Asn1Int'Last) then
         c   := not IntVal;
         ret := -Asn1Int (c) - 1;
      else
         ret := Asn1Int (IntVal);
      end if;
      return ret;
   end To_Int;

   function Zero return Asn1Real is
   begin
      return 0.0;
   end Zero;

   function PLUS_INFINITY return Asn1Real is
      pragma SPARK_Mode (Off);
   begin
      return 1.0 / Zero;
   end PLUS_INFINITY;

   function MINUS_INFINITY return Asn1Real is
      pragma SPARK_Mode (Off);
   begin
      return -1.0 / Zero;
   end MINUS_INFINITY;

   function RequiresReverse (dummy : Boolean) return Boolean is
      pragma SPARK_Mode (Off);
      dword : Integer := 16#00000001#;
      arr   : aliased OctetArray4;
      for arr'Address use dword'Address;
   begin
      return arr (arr'First) = 1;
   end RequiresReverse;

   function To_Int_n (IntVal : Asn1UInt; nBits : Integer) return Asn1Int with
      Pre => nBits >= 0 and nBits <= 64 is
      ret : Asn1Int;
      c   : Asn1UInt;
   begin
      if nBits = 0 then
         ret := 0;
      elsif nBits = 64 then
         ret := To_Int (IntVal);
      else
         pragma Assert
           (nBits >= 1 and
            nBits <= 63 and
            2**(nBits - 1) >= 1 and
            2**(nBits - 1) <= 4611686018427387904 and
            2**nBits - 1 >= 1 and
            2**nBits - 1 <= 9223372036854775807);
      --#  ;
         if IntVal >= Asn1UInt (2)**(nBits - 1) then
            c   := not (Asn1UInt (2)**nBits - 1);
            ret := To_Int (IntVal or c);
         else
            ret := Asn1Int (IntVal);
         end if;
      end if;
      return ret;
   end To_Int_n;

   procedure BitStream_AppendBit
     (S      : in out BitArray;
      I      : in out Natural;
      BitVal : in     BIT)
   is
   begin
      I     := I + 1;
      S (I) := BitVal;
   end BitStream_AppendBit;

   procedure BitStream_ReadBit
     (S      : in     BitArray;
      P      : in out DECODE_PARAMS;
      BitVal :    out BIT;
      result :    out Boolean)
   is
   begin
      P.K    := P.K + 1;
      BitVal := S (P.K);
      result := P.DataLen - P.K >= 0;
   end BitStream_ReadBit;

   ---# pre K +1 >= S'First and K+8 <= S'Last;
   ---# post K = K~ + 8;

   procedure BitStream_AppendByte
     (S         : in out BitArray;
      K         : in out Natural;
      ByteValue : in     Asn1Byte;
      Negate    : in     Boolean)
   is
      ByteVal : Asn1Byte;
   begin
      if Negate then
         ByteVal := not ByteValue;
      else
         ByteVal := ByteValue;
      end if;

      for I in RANGE_1_8 loop
            --# assert K = K~ and K + 1>= S'First and K + 8 <= S'Last and K+I >=S'First  and K+I-1 < S'Last;
         if (MASKS (I) and ByteVal) > 0 then
            S (K + I) := 1;
         else
            S (K + I) := 0;
         end if;
      end loop;
      K := K + 8;

   end BitStream_AppendByte;

   procedure BitStream_DecodeByte
     (S         : in     BitArray;
      P         : in out DECODE_PARAMS;
      ByteValue :    out Asn1Byte;
      success   :    out Boolean)
   is
   begin
      ByteValue := 0;
      for I in Integer range 1 .. 8 loop
    --# assert  P.K + 8 <= S'Last and P.K+1>= S'First and
    --#         P.K=P~.K and  P.K + I - 1< S'Last;

         if S (P.K + I) = 0 then
            ByteValue := 2 * ByteValue;
         else
            ByteValue := 2 * ByteValue + 1;
         end if;

      end loop;

      P.K     := P.K + 8;
      success := P.DataLen - P.K >= 0;
   end BitStream_DecodeByte;

   procedure BitStream_AppendPartialByte
     (S         : in out BitArray;
      K         : in out Natural;
      ByteValue : in     Asn1Byte;
      NBits     : in     Integer;
      Negate    : in     Boolean)
   is
      ByteVal : Asn1Byte;

   begin
      if Negate then
         ByteVal := not ByteValue;
      else
         ByteVal := ByteValue;
      end if;

      for I in Integer range 1 .. NBits loop
--            --# assert  NBits >= MASKS'FIRST and NBits < MASKS'LAST and K + 1 >= S'First and K + NBits <= S'Last and S'First + nBits -1 <= S'Last and
            --# assert  NBits >= MASKS'FIRST and NBits < MASKS'LAST and K + 1 >= S'First and K + NBits <= S'Last and
            --#         K = K~ and  K + I>=S'First  and K + I - 1< S'Last and
            --#         I+(MASKS'LAST - NBITS) >=MASKS'First and I+(MASKS'LAST - NBITS) <=MASKS'Last;
         if (MASKS (I + (MASKS'Last - NBits)) and ByteVal) > 0 then
            S (K + I) := 1;
         else
            S (K + I) := 0;
         end if;
      end loop;

      K := K + NBits;
   end BitStream_AppendPartialByte;

   procedure BitStream_ReadNibble
     (S         : in     BitArray;
      P         : in out DECODE_PARAMS;
      ByteValue :    out Asn1Byte;
      success   :    out Boolean)
   is
   begin
      ByteValue := 0;
      for I in Integer range 1 .. 4 loop
    --# assert  P.K + 4 <= S'Last and P.K+1>= S'First and
    --#         P.K=P~.K and  P.K + I - 1< S'Last;

         if S (P.K + I) = 0 then
            ByteValue := 2 * ByteValue;
         else
            ByteValue := 2 * ByteValue + 1;
         end if;

      end loop;

      P.K     := P.K + 4;
      success := P.DataLen - P.K >= 0;
   end BitStream_ReadNibble;

   procedure BitStream_Encode_Non_Negative_Integer
     (S          : in out BitArray;
      K          : in out Natural;
      IntValue   : in     Asn1UInt;
      nBitsRange : in     Integer)
   is
      IVAL        : Asn1UInt;
      scaleFactor : Asn1UInt;
      nBitsToSkip : Integer;
   begin
      if nBitsRange <= 63 then
         nBitsToSkip := 64 - nBitsRange;
         scaleFactor := 2**nBitsToSkip;
         IVAL        := IntValue * scaleFactor;
      else
         IVAL := IntValue;
      end if;

      for I in Integer range 1 .. nBitsRange loop
    --# assert  nBitsRange >= 1 and nBitsRange <= 64 and
    --#         K + nBitsRange <= S'Last and
    --#         K+1>= S'First and
    --#         K=K~ and  K + I - 1< S'Last;

         if (IVAL and MSBIT_ONE) = 0 then
            S (K + I) := 0;
         else
            S (K + I) := 1;
         end if;

         IVAL := IVAL * 2;
      end loop;

      K := K + nBitsRange;
   end BitStream_Encode_Non_Negative_Integer;

   procedure BitStream_Decode_Non_Negative_Integer
     (S          : in     BitArray;
      P          : in out DECODE_PARAMS;
      IntValue   :    out Asn1UInt;
      nBitsRange : in     Integer;
      result     :    out Boolean)
    --# derives IntValue from S, P, nBitsRange & P from P, nBitsRange & result from P, nBitsRange;
    --# pre     nBitsRange >= 0 and nBitsRange <= 64 and
    --#         P.K+1>= S'First and P.K + nBitsRange <= S'Last;
    --# post P.K = P~.K + nBitsRange;
   is
   begin

      IntValue := 0;
      for I in Integer range 1 .. nBitsRange loop
    --# assert  nBitsRange >= 1 and nBitsRange <= 64 and
    --#         P.K + nBitsRange <= S'Last and
    --#         P.K+1>= S'First and
    --#         P.K=P~.K and  P.K + I - 1< S'Last;

         if S (P.K + I) = 0 then
            IntValue := 2 * IntValue;
         else
            IntValue := 2 * IntValue + 1;
         end if;

      end loop;

      P.K    := P.K + nBitsRange;
      result := P.DataLen - P.K >= 0;
   end BitStream_Decode_Non_Negative_Integer;

   function To_UInt (IntVal : Asn1Int) return Asn1UInt is
      ret : Asn1UInt;
   begin
      if IntVal < 0 then
         ret := Asn1UInt (-(IntVal + 1));
         ret := not ret;
      else
         ret := Asn1UInt (IntVal);
      end if;
      return ret;
   end To_UInt;

   function Sub (A : in Asn1Int; B : in Asn1Int) return Asn1UInt
    --# pre A >= B;
    --# return Asn1Uint(A-B);
       is
--                pragma SPARK_Mode(Off);
      ret : Asn1UInt;
      au  : Asn1UInt;
      bu  : Asn1UInt;
--      diff:Asn1Int;
   begin

      --diff := A-B;
      --  if (diff >= 0) then
      --      ret := Asn1UInt(diff);
      --  else
      --      ret := Asn1UInt(-diff);
      --      ret := NOT ret;
      --      ret := ret + 1;
      --end if;
      au := To_UInt (A);
      bu := To_UInt (B);

      if au >= bu then
         ret := au - bu;
      else
         ret := bu - au;
         ret := not ret;
         ret := ret + 1;
      end if;

      return ret;
   end Sub;

   function GetBytes (V : Asn1UInt) return Integer
      --# return M => M>=1 and M<=8;
       is
      Ret : Integer;
   begin
      if V < 16#100# then
         Ret := 1;
      elsif V < 16#10000# then
         Ret := 2;
      elsif V < 16#1000000# then
         Ret := 3;
      elsif V < 16#100000000# then
         Ret := 4;
      elsif V < 16#10000000000# then
         Ret := 5;
      elsif V < 16#1000000000000# then
         Ret := 6;
      elsif V < 16#100000000000000# then
         Ret := 7;
      else
         Ret := 8;
      end if;
      return Ret;
   end GetBytes;

   procedure Enc_UInt
     (S            : in out BitArray;
      K            : in out Natural;
      EncodedValue : in     Asn1UInt;
      nBytes       : in     Integer)
    --# derives S from S, K, EncodedValue, nBytes & K from K, nBytes;
    --# pre nBytes >= 1  and nBytes<=8 and
    --#     K+1>= S'First and K + nBytes*8 <= S'Last;
    --# post K = K~ + nBytes*8;
   is
      ActualEncodedValue : Asn1UInt;
      byteToEncode       : Interfaces.Unsigned_8;
   begin

      ActualEncodedValue := EncodedValue;

      --Encode number
      for I in Integer range 1 .. (8 - nBytes) loop
            --# assert nBytes >= 1  and nBytes<=8 and K=K~ and I>=1 and I<=7 and
            --#        K+1>= S'First and K + nBytes*8 <= S'Last;
         ActualEncodedValue := ActualEncodedValue * 16#100#;
      end loop;

      for I in Integer range 0 .. nBytes - 1 loop
            --# assert nBytes >= 1  and nBytes<=8 and I>=0 and I<=7 and K = K~ + 8*I and
            --#        K+1>= S'First and K + 8 <= S'Last;
         byteToEncode :=
           Interfaces.Unsigned_8
             ((ActualEncodedValue and MSBYTE_FF) / 16#100000000000000#);
         ActualEncodedValue := ActualEncodedValue * 16#100#;
         BitStream_AppendByte (S, K, byteToEncode, False);
      end loop;
   end Enc_UInt;

   function GetLengthInBytesOfSIntAux (V : Asn1UInt) return Integer
      --# pre V >= 0;
      --# return M => M>=1 and M<=8;
       is
      Ret : Integer;
   begin
      if V < 16#80# then
         Ret := 1;
      elsif V < 16#8000# then
         Ret := 2;
      elsif V < 16#800000# then
         Ret := 3;
      elsif V < 16#80000000# then
         Ret := 4;
      elsif V < 16#8000000000# then
         Ret := 5;
      elsif V < 16#800000000000# then
         Ret := 6;
      elsif V < 16#80000000000000# then
         Ret := 7;
      else
         Ret := 8;
      end if;

      return Ret;
   end GetLengthInBytesOfSIntAux;

   function GetLengthInBytesOfSInt (V : Asn1Int) return Integer
      --# return M => M>=1 and M<=8;
       is
      Ret : Integer;
   begin
      if V >= 0 then
         Ret := GetLengthInBytesOfSIntAux (Asn1UInt (V));
      else
         Ret := GetLengthInBytesOfSIntAux (Asn1UInt (-(V + 1)));
      end if;
      return Ret;
   end GetLengthInBytesOfSInt;

   function Int32_UInt32
     (IntVal : Interfaces.Integer_32) return Interfaces.Unsigned_32
   is
      ret : Interfaces.Unsigned_32;
   begin

      if IntVal < 0 then
         ret := Interfaces.Unsigned_32 (-(IntVal + 1));
         ret := not ret;
      else
         ret := Interfaces.Unsigned_32 (IntVal);
      end if;
      return ret;
   end Int32_UInt32;

   function UInt32_Int32
     (IntVal : Interfaces.Unsigned_32) return Interfaces.Integer_32
   is
      ret : Interfaces.Integer_32;
      c   : Interfaces.Unsigned_32;
   begin
      if IntVal > Interfaces.Unsigned_32 (Interfaces.Integer_32'Last) then
         c   := not IntVal;
         ret := -Interfaces.Integer_32 (c) - 1;
      else
         ret := Interfaces.Integer_32 (IntVal);
      end if;
      return ret;
   end UInt32_Int32;

   function Int16_UInt16
     (IntVal : Interfaces.Integer_16) return Interfaces.Unsigned_16
   is
      ret : Interfaces.Unsigned_16;
   begin

      if IntVal < 0 then
         ret := Interfaces.Unsigned_16 (-(IntVal + 1));
         ret := not ret;
      else
         ret := Interfaces.Unsigned_16 (IntVal);
      end if;
      return ret;
   end Int16_UInt16;

   function UInt16_Int16
     (IntVal : Interfaces.Unsigned_16) return Interfaces.Integer_16
   is
      ret : Interfaces.Integer_16;
      c   : Interfaces.Unsigned_16;
   begin
      if IntVal > Interfaces.Unsigned_16 (Interfaces.Integer_16'Last) then
         c   := not IntVal;
         ret := -Interfaces.Integer_16 (c) - 1;
      else
         ret := Interfaces.Integer_16 (IntVal);
      end if;
      return ret;
   end UInt16_Int16;

   procedure UPER_Enc_ConstraintWholeNumber
     (S           : in out BitArray;
      K           : in out Natural;
      IntVal      : in     Asn1Int;
      MinVal      : in     Asn1Int;
      nSizeInBits : in     Integer)
   is
      encVal : Asn1UInt;
   begin
      encVal := Sub (IntVal, MinVal);
      BitStream_Encode_Non_Negative_Integer (S, K, encVal, nSizeInBits);
   end UPER_Enc_ConstraintWholeNumber;

   procedure UPER_Enc_ConstraintPosWholeNumber
     (S           : in out BitArray;
      K           : in out Natural;
      IntVal      : in     Asn1UInt;
      MinVal      : in     Asn1UInt;
      nSizeInBits : in     Integer)
   is
      encVal : Asn1UInt;
   begin
      encVal := IntVal - MinVal;
      BitStream_Encode_Non_Negative_Integer (S, K, encVal, nSizeInBits);
   end UPER_Enc_ConstraintPosWholeNumber;

   procedure UPER_Dec_ConstraintWholeNumber
     (S           : in     BitArray;
      K           : in out DECODE_PARAMS;
      IntVal      :    out Asn1Int;
      MinVal      : in     Asn1Int;
      MaxVal      : in     Asn1Int;
      nSizeInBits : in     Integer;
      Result      :    out Boolean)
   is
      encVal : Asn1UInt;
   begin
      BitStream_Decode_Non_Negative_Integer
        (S,
         K,
         encVal,
         nSizeInBits,
         Result);
      if Result then
         IntVal := To_Int (encVal + To_UInt (MinVal));

         Result := IntVal >= MinVal and IntVal <= MaxVal;
         if not Result then
            IntVal := MinVal;
         end if;
      else
         IntVal := MinVal;
      end if;

   end UPER_Dec_ConstraintWholeNumber;

   procedure UPER_Dec_ConstraintPosWholeNumber
     (S           : in     BitArray;
      K           : in out DECODE_PARAMS;
      IntVal      :    out Asn1UInt;
      MinVal      : in     Asn1UInt;
      MaxVal      : in     Asn1UInt;
      nSizeInBits : in     Integer;
      Result      :    out Boolean)
   is
      encVal : Asn1UInt;
   begin
      BitStream_Decode_Non_Negative_Integer
        (S,
         K,
         encVal,
         nSizeInBits,
         Result);
      if Result then
         IntVal := encVal + MinVal;

         Result := IntVal >= MinVal and IntVal <= MaxVal;
         if not Result then
            IntVal := MinVal;
         end if;
      else
         IntVal := MinVal;
      end if;

   end UPER_Dec_ConstraintPosWholeNumber;

   procedure UPER_Dec_ConstraintWholeNumberInt
     (S           : in     BitArray;
      K           : in out DECODE_PARAMS;
      IntVal      :    out Integer;
      MinVal      : in     Integer;
      MaxVal      : in     Integer;
      nSizeInBits : in     Integer;
      Result      :    out Boolean)
   is
      Ret : Asn1Int;
   begin
      UPER_Dec_ConstraintWholeNumber
        (S,
         K,
         Ret,
         Asn1Int (MinVal),
         Asn1Int (MaxVal),
         nSizeInBits,
         Result);
      IntVal := Integer (Ret);
   end UPER_Dec_ConstraintWholeNumberInt;

   procedure UPER_Enc_SemiConstraintWholeNumber
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int;
      MinVal : in     Asn1Int)
   is
      nBytes             : Integer;
      ActualEncodedValue : Asn1UInt;
   begin
      ActualEncodedValue := Sub (IntVal, MinVal);

      nBytes := GetBytes (ActualEncodedValue);

      -- encode length
      BitStream_AppendByte (S, K, Interfaces.Unsigned_8 (nBytes), False);
      --Encode number
      Enc_UInt (S, K, ActualEncodedValue, nBytes);
   end UPER_Enc_SemiConstraintWholeNumber;

   procedure UPER_Enc_SemiConstraintPosWholeNumber
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt;
      MinVal : in     Asn1UInt)
   is
      nBytes             : Integer;
      ActualEncodedValue : Asn1UInt;
   begin
      ActualEncodedValue := IntVal - MinVal;

      nBytes := GetBytes (ActualEncodedValue);

      -- encode length
      BitStream_AppendByte (S, K, Interfaces.Unsigned_8 (nBytes), False);
      --Encode number
      Enc_UInt (S, K, ActualEncodedValue, nBytes);
   end UPER_Enc_SemiConstraintPosWholeNumber;

   procedure Dec_UInt
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      nBytes :        Integer;
      Ret    :    out Asn1UInt;
      result :    out Boolean)
    --#derives K    from K, nBytes &
    --#        Ret  from S, K, nBytes &
    --#        result from K, nBytes;
    --#pre     nBytes>=1 AND nBytes<=8 AND
    --#        K.K+1>= S'First and K.K + nBytes*8 <= S'Last;
    --#post    K.K >= K~.K AND K.K<=K~.K+nBytes*8;
   is
      ByteVal : Asn1UInt;
      I       : Integer;
   begin
      I      := 1;
      Ret    := 0;
      result := True;
      while I <= nBytes and result loop
        --# assert I<=nBytes and I>=1 and K~.K+1>= S'First and K~.K + nBytes*8 <= S'Last and K.K=K~.K+8*(I-1);
         BitStream_Decode_Non_Negative_Integer (S, K, ByteVal, 8, result);
         if result then
            Ret := (Ret * 256) or ByteVal;
            I   := I + 1;
         end if;
      end loop;
      result := result and K.DataLen - K.K >= 0;
   end Dec_UInt;

   procedure Dec_Int
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      nBytes :        Integer;
      RetVal :    out Asn1Int;
      result :    out Boolean)
    --#derives K       from K, nBytes &
    --#        RetVal  from S, K, nBytes &
    --#        result  from K, nBytes;
    --#pre     nBytes>=1 AND nBytes<=8 AND
    --#        K.K+1>= S'First and K.K + nBytes*8 <= S'Last;
    --#post    K.K >= K~.K AND K.K<=K~.K+nBytes*8;
   is
      ByteVal : Asn1UInt;
      I       : Integer;
      Ret     : Asn1UInt;
   begin
      I := 1;
      if S (K.K + 1) = 0 then
         Ret := 0;
      else
         Ret := Asn1UInt'Last;
      end if;
      result := True;
      while I <= nBytes and result loop
        --# assert I<=nBytes and I>=1 and K~.K+1>= S'First and K~.K + nBytes*8 <= S'Last and K.K=K~.K+8*(I-1);
         BitStream_Decode_Non_Negative_Integer (S, K, ByteVal, 8, result);
         if result then
            Ret := (Ret * 256) or ByteVal;
            I   := I + 1;
         end if;
      end loop;
      RetVal := To_Int (Ret);
      result := result and K.DataLen - K.K >= 0;
   end Dec_Int;

   procedure UPER_Dec_SemiConstraintWholeNumber
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      MinVal : in     Asn1Int;
      Result :    out Boolean)
   is
      NBytes : Asn1Int;
--      ByteVal : Asn1UInt;
      Ret : Asn1UInt;
--      I_MAX   : Integer;
--      I             : Integer;
   begin

      IntVal := MinVal;
      UPER_Dec_ConstraintWholeNumber (S, K, NBytes, 0, 255, 8, Result);
      if Result and NBytes >= 1 and NBytes <= 8 then
         Dec_UInt (S, K, Integer (NBytes), Ret, Result);
         if Result then
            IntVal := To_Int (Ret + To_UInt (MinVal));
            Result := IntVal >= MinVal;
            if not Result then
               IntVal := MinVal;
            end if;
         end if;
      else
         Result := False;
      end if;

   end UPER_Dec_SemiConstraintWholeNumber;

   procedure UPER_Dec_SemiConstraintPosWholeNumber
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      MinVal : in     Asn1UInt;
      Result :    out Boolean)
   is
      NBytes : Asn1Int;
--      ByteVal : Asn1UInt;
      Ret : Asn1UInt;
--      I_MAX   : Integer;
--      I             : Integer;
   begin

      IntVal := MinVal;
      UPER_Dec_ConstraintWholeNumber (S, K, NBytes, 0, 255, 8, Result);
      if Result and NBytes >= 1 and NBytes <= 8 then
         Dec_UInt (S, K, Integer (NBytes), Ret, Result);
         if Result then
            IntVal := Ret + MinVal;
            Result := IntVal >= MinVal;
            if not Result then
               IntVal := MinVal;
            end if;
         end if;
      else
         Result := False;
      end if;

   end UPER_Dec_SemiConstraintPosWholeNumber;

   procedure UPER_Enc_UnConstraintWholeNumber
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int)
   is
      nBytes : Integer;
   begin

      nBytes := GetLengthInBytesOfSInt (IntVal);

      -- encode length
      BitStream_AppendByte (S, K, Interfaces.Unsigned_8 (nBytes), False);
      Enc_UInt (S, K, To_UInt (IntVal), nBytes);
   end UPER_Enc_UnConstraintWholeNumber;

   procedure UPER_Dec_UnConstraintWholeNumber
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      Result :    out Boolean)
   is
      NBytes : Asn1Int;
   begin
      UPER_Dec_ConstraintWholeNumber (S, K, NBytes, 0, 255, 8, Result);
      if Result and NBytes >= 1 and NBytes <= 8 then
         Dec_Int (S, K, Integer (NBytes), IntVal, Result);
      else
         IntVal := 0;
         Result := False;
      end if;
   end UPER_Dec_UnConstraintWholeNumber;

   procedure UPER_Dec_UnConstraintWholeNumberMax
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      MaxVal : in     Asn1Int;
      Result :    out Boolean)
   ---# derives IntVal,
   ---#         Result from K, MaxVal, S &
   ---#                K      from K, S;
   ---# pre  K.K+1>= S'First and K.K + 72 <= S'Last;
   ---# post K.K >= K~.K +8 and K.K <=K~.K+72 and IntVal<=MaxVal;
   is
   begin
      UPER_Dec_UnConstraintWholeNumber (S, K, IntVal, Result);
      Result := Result and IntVal <= MaxVal;
      if not Result then
         IntVal := MaxVal;
      end if;
   end UPER_Dec_UnConstraintWholeNumberMax;

   procedure UPER_Enc_Boolean
     (S   : in out BitArray;
      I   : in out Natural;
      Val : in     Asn1Boolean)
   is
      b : BIT;
   begin

      if Val then
         b := 1;
      else
         b := 0;
      end if;
      BitStream_AppendBit (S, I, b);
   end UPER_Enc_Boolean;

   procedure UPER_Dec_boolean
     (S      : in     BitArray;
      P      : in out DECODE_PARAMS;
      val    :    out Asn1Boolean;
      result :    out Boolean)
   is
      v : BIT;
   begin
      BitStream_ReadBit (S, P, v, result);
      val := v = 1;
   end UPER_Dec_boolean;

   function GetExponent (V : Asn1Real) return Asn1Int is
      pragma SPARK_Mode (Off);
   --due to the fact that Examiner has not yet implement the Exponent attribute
   begin
      return Asn1Int (Asn1Real'Exponent (V) - Asn1Real'Machine_Mantissa);
   end GetExponent;

   function GetMantissa (V : Asn1Real) return Asn1UInt is
      pragma SPARK_Mode (Off);
   --due to the fact that Examiner has not yet implement the Fraction attribute
   begin
      return Asn1UInt (Asn1Real'Fraction (V) * MantissaFactor);
   end GetMantissa;

   procedure UPER_Enc_Real
     (S       : in out BitArray;
      K       : in out Natural;
      RealVal : in     Asn1Real)
   is
      Header   : Interfaces.Unsigned_8 := 16#80#;
      NExpLen  : Integer;
      NManLen  : Integer;
      Exp      : Asn1Int;
      Mantissa : Asn1UInt;
      V        : Asn1Real;
   begin

      if RealVal >= 0.0 and RealVal <= 0.0 then
         BitStream_AppendByte (S, K, 0, False);
      elsif RealVal = PLUS_INFINITY then
         BitStream_AppendByte (S, K, 1, False);
         BitStream_AppendByte (S, K, 16#40#, False);
      elsif RealVal = MINUS_INFINITY then
         BitStream_AppendByte (S, K, 1, False);
         BitStream_AppendByte (S, K, 16#41#, False);
      else
         V := RealVal;

         if V < 0.0 then
            V      := -V;
            Header := Header or 16#40#;
         end if;

         Exp      := GetExponent (V);
         Mantissa := GetMantissa (V);

         NExpLen := GetLengthInBytesOfSInt (Exp);
         NManLen := GetBytes (Mantissa);

         if NExpLen >= 4 then
            NExpLen := 3;
         end if;

         if NExpLen = 2 then
            Header := Header or 1;
         elsif NExpLen = 3 then
            Header := Header or 2;
         end if;

         --#check NExpLen>=1 AND NExpLen<=3;

         -- encode length
         BitStream_AppendByte
           (S,
            K,
            Interfaces.Unsigned_8 ((1 + NExpLen) + NManLen),
            False); --1

         -- encode header
         BitStream_AppendByte (S, K, Header, False); --1

         -- encode exponent
         Enc_UInt (S, K, To_UInt (Exp), NExpLen); --max 3 octets

         -- encode mantissa
         Enc_UInt (S, K, Mantissa, NManLen); --max 8 octets
      end if;
   end UPER_Enc_Real;

   function CalcReal
     (Factor : Asn1UInt;
      N      : Asn1UInt;
      base   : Integer;
      Exp    : Integer) return Asn1Real with
      Pre => base = 2 or base = 8 or base = 16 is
--          pragma SPARK_Mode(Off);
   begin
      return Asn1Real (Factor * N) * Asn1Real (base)**Exp;
   end CalcReal;

   procedure UPER_Dec_Real_AsBinary_aux
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      ExpLen  : in     Interfaces.Unsigned_8;
      Length  : in     Interfaces.Unsigned_8;
      Factor  : in     Asn1UInt;
      Sign    : in     Integer;
      Base    : in     Integer;
      RealVal :    out Asn1Real;
      Result  :    out ASN1_RESULT)
    --# derives K from K, ExpLen, Length &
    --#         RealVal from S, K, ExpLen, Length, Factor, Sign, Base &
    --#         Result  from  S, K, ExpLen, Length;
    --# pre  K.K+1>= S'First and K.K + 88 <= S'Last AND (base=2 OR base=8 OR base=16) AND ExpLen>=1;
    --# post K.K >= K~.K  and K.K <=K~.K+88;
   is
      Exp : Asn1Int;
      N   : Asn1UInt;
   begin
      RealVal := 0.0;
      Result := ASN1_RESULT'(Success => False, ErrorCode => ERR_END_OF_STREAM);
      if ExpLen < Length and ExpLen <= 3 then
         Dec_Int (S, K, Integer (ExpLen), Exp, Result.Success);

         if Result.Success and Length - ExpLen <= 8 then
            Dec_UInt (S, K, Integer (Length - ExpLen), N, Result.Success);
            if Result.Success and
              Exp > Asn1Int (Integer'First) and
              Exp < Asn1Int (Integer'Last)
            then
               RealVal := CalcReal (Factor, N, Base, Integer (Exp));

               if Sign < 0 then
                  RealVal := -RealVal;
               end if;

               Result := ASN1_RESULT'(Success => True, ErrorCode => 0);
            end if;
         end if;
      end if;
   end UPER_Dec_Real_AsBinary_aux;

   procedure UPER_Dec_Real_AsBinary
     (S         : in     BitArray;
      K         : in out DECODE_PARAMS;
      Header    : in     Interfaces.Unsigned_8;
      EncLength : in     Interfaces.Unsigned_8;
      RealVal   :    out Asn1Real;
      Result    :    out ASN1_RESULT)
    --# derives K from K, Header, EncLength &
    --#         RealVal from S, Header, K, EncLength &
    --#         Result  from  S, Header, K, EncLength;
    --# pre  K.K+1>= S'First and K.K + 88 <= S'Last AND EncLength<=11;
    --# post K.K >= K~.K  and K.K <=K~.K+88;

   is
      Sign   : Integer  := 1;
      Base   : Integer  := 2;
      F      : Interfaces.Unsigned_8;
      Factor : Asn1UInt := 1;
      ExpLen : Interfaces.Unsigned_8;

   begin

      if (Header and 16#40#) > 0 then
         Sign := -1;
      end if;

      if (Header and 16#10#) > 0 then
         Base := 8;
      elsif (Header and 16#20#) > 0 then
         Base := 16;
      end if;

      F      := (Header and 16#0C#) / 4;
      Factor := Factor * (2**Integer (F));

      ExpLen := (Header and 16#03#) + 1;

      UPER_Dec_Real_AsBinary_aux
        (S,
         K,
         ExpLen,
         EncLength,
         Factor,
         Sign,
         Base,
         RealVal,
         Result);

   end UPER_Dec_Real_AsBinary;

   procedure UPER_Dec_Real
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      RealVal :    out Asn1Real;
      Result  :    out ASN1_RESULT)
   is
      Header : Interfaces.Unsigned_8;
      Length : Interfaces.Unsigned_8;
   begin
      RealVal := 0.0;
      Result := ASN1_RESULT'(Success => False, ErrorCode => ERR_END_OF_STREAM);

      BitStream_DecodeByte (S, K, Length, Result.Success);
      if Result.Success and Length <= 12 then
         if Length > 0 then
            BitStream_DecodeByte (S, K, Header, Result.Success);
            if Result.Success then
               if Header = 16#40# then
                  RealVal := PLUS_INFINITY;
                  Result  := ASN1_RESULT'(Success => True, ErrorCode => 0);
               elsif Header = 16#41# then
                  RealVal := MINUS_INFINITY;
                  Result  := ASN1_RESULT'(Success => True, ErrorCode => 0);
               elsif (Header and 16#80#) > 0 then
                  UPER_Dec_Real_AsBinary
                    (S,
                     K,
                     Header,
                     Length - 1,
                     RealVal,
                     Result);
               else
                  Result :=
                    ASN1_RESULT'
                      (Success   => False,
                       ErrorCode => ERR_UNSUPPORTED_ENCODING);
               end if;
            end if;
         else
            Result := ASN1_RESULT'(Success => True, ErrorCode => 0);
         end if;
      end if;
   end UPER_Dec_Real;

   ---# assert I>=AllowedCharSet'FIRST AND I<=AllowedCharSet'LAST AND AllowedCharSet'First=1 AND
   ---# AllowedCharSet'Last>=AllowedCharSet'First AND AllowedCharSet'Last<=INTEGER'LAST-1 AND
   ---# ret=I-AllowedCharSet'First;

   function GetZeroBasedCharIndex
     (CharToSearch   :    Character;
      AllowedCharSet : in String) return Integer
   is
      ret : Integer;
   begin
      ret := 0;
      for I in Integer range AllowedCharSet'Range loop
         ret := I - AllowedCharSet'First;
         pragma Loop_Invariant
           (I >= AllowedCharSet'First and
            I <= AllowedCharSet'Last and
            AllowedCharSet'Last >= AllowedCharSet'First and
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

-- ACN Functions

   procedure Acn_Enc_Int_PositiveInteger_ConstSize
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt;
      Size   : in     Integer)
   is
   begin
      UPER_Enc_ConstraintPosWholeNumber (S, K, IntVal, 0, Size);
   end Acn_Enc_Int_PositiveInteger_ConstSize;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_8
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt)
   is
   begin
      UPER_Enc_ConstraintPosWholeNumber (S, K, IntVal, 0, 8);
   end Acn_Enc_Int_PositiveInteger_ConstSize_8;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt)
   is
   begin
      UPER_Enc_ConstraintPosWholeNumber (S, K, IntVal, 0, 16);
   end Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt)
   is
   begin
      UPER_Enc_ConstraintPosWholeNumber (S, K, IntVal, 0, 32);
   end Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt)
   is
   begin
      UPER_Enc_ConstraintPosWholeNumber (S, K, IntVal, 0, 64);
   end Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt)
   is
      tmp : OctetArray2;
   begin
      tmp := UInt16_to_OctetArray2 (Interfaces.Unsigned_16 (IntVal));
      if not RequiresReverse (K > 0) then
         for I in RANGE_1_2 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(8-I);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      else
         for I in reverse RANGE_1_2 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(I-1);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      end if;
   end Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt)
   is
      tmp : OctetArray4;
   begin
      tmp := UInt32_to_OctetArray4 (Interfaces.Unsigned_32 (IntVal));
      if not RequiresReverse (K > 0) then
         for I in RANGE_1_4 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(8-I);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      else
         for I in reverse RANGE_1_4 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(I-1);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      end if;
   end Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt)
   is
      tmp : OctetArray8;
   begin
      tmp := Asn1UInt_to_OctetArray8 (IntVal);
      if not RequiresReverse (K > 0) then
         for I in RANGE_1_8 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(8-I);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      else
         for I in reverse RANGE_1_8 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(I-1);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      end if;
   end Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64;

   procedure Acn_Enc_Int_PositiveInteger_VarSize_LengthEmbedded
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt)
   is
   begin
      UPER_Enc_SemiConstraintPosWholeNumber (S, K, IntVal, 0);
   end Acn_Enc_Int_PositiveInteger_VarSize_LengthEmbedded;

   procedure Acn_Enc_Int_TwosComplement_ConstSize
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int;
      Size   : in     Integer)
   is
   begin
      BitStream_Encode_Non_Negative_Integer (S, K, To_UInt (IntVal), Size);
   end Acn_Enc_Int_TwosComplement_ConstSize;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_8
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int)
   is
   begin
      Acn_Enc_Int_TwosComplement_ConstSize (S, K, IntVal, 8);
   end Acn_Enc_Int_TwosComplement_ConstSize_8;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_16
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int)
   is
   begin
      Acn_Enc_Int_TwosComplement_ConstSize (S, K, IntVal, 16);
   end Acn_Enc_Int_TwosComplement_ConstSize_big_endian_16;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_32
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int)
   is
   begin
      Acn_Enc_Int_TwosComplement_ConstSize (S, K, IntVal, 32);
   end Acn_Enc_Int_TwosComplement_ConstSize_big_endian_32;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_64
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int)
   is
   begin
      Acn_Enc_Int_TwosComplement_ConstSize (S, K, IntVal, 64);
   end Acn_Enc_Int_TwosComplement_ConstSize_big_endian_64;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_16
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int)
   is
      tmp : OctetArray2;
   begin
      tmp :=
        UInt16_to_OctetArray2 (Int16_UInt16 (Interfaces.Integer_16 (IntVal)));
      if not RequiresReverse (K > 0) then
         for I in RANGE_1_2 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(8-I);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      else
         for I in reverse RANGE_1_2 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(I-1);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      end if;
   end Acn_Enc_Int_TwosComplement_ConstSize_little_endian_16;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_32
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int)
   is
      tmp : OctetArray4;
   begin
      tmp :=
        UInt32_to_OctetArray4 (Int32_UInt32 (Interfaces.Integer_32 (IntVal)));
      if not RequiresReverse (K > 0) then
         for I in RANGE_1_4 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(8-I);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      else
         for I in reverse RANGE_1_4 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(I-1);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      end if;
   end Acn_Enc_Int_TwosComplement_ConstSize_little_endian_32;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_64
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int)
   is
      tmp : OctetArray8;
   begin
      tmp := Asn1UInt_to_OctetArray8 (To_UInt (IntVal));
      if not RequiresReverse (K > 0) then
         for I in RANGE_1_8 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(8-I);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      else
         for I in reverse RANGE_1_8 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(I-1);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      end if;
   end Acn_Enc_Int_TwosComplement_ConstSize_little_endian_64;

   procedure Acn_Enc_Int_TwosComplement_VarSize_LengthEmbedded
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int)
   is
   begin
      UPER_Enc_UnConstraintWholeNumber (S, K, IntVal);
   end Acn_Enc_Int_TwosComplement_VarSize_LengthEmbedded;

   procedure Acn_Enc_Int_BCD_ConstSize
     (S        : in out BitArray;
      K        : in out Natural;
      IntVal   : in     Asn1UInt;
      nNibbles : in     Integer)
   is
      totalNibbles : Integer       := 1;
      tmp          : OctetArray100 := OctetArray100'(others => 0);
      intValCopy   : Asn1UInt;
   begin
      intValCopy := IntVal;
      while intValCopy > 0 and totalNibbles <= nNibbles loop
        --# assert totalNibbles>=1 and totalNibbles>=1 and totalNibbles<=nNibbles and K~ + 1>=S'First and K~ + 4*nNibbles <= S'Last and K~=K;
         tmp (totalNibbles) := Interfaces.Unsigned_8 (intValCopy mod 10);
         totalNibbles       := totalNibbles + 1;
         intValCopy         := intValCopy / 10;
      end loop;

      for I in reverse Integer range 1 .. nNibbles loop
        --# assert K~ + 1>=S'First and K~ + 4*nNibbles <= S'Last and K = K~ + (nNibbles-I)*4;
         BitStream_AppendPartialByte (S, K, tmp (I), 4, False);
      end loop;
   end Acn_Enc_Int_BCD_ConstSize;

   procedure Acn_Enc_Int_BCD_VarSize_LengthEmbedded
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt)
   is
      pragma SPARK_Mode (Off);
   begin
      null;
   end Acn_Enc_Int_BCD_VarSize_LengthEmbedded;

   procedure Acn_Enc_Int_BCD_VarSize_NullTerminated
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt)
   is
      totalNibbles : Integer       := 1;
      tmp          : OctetArray100 := OctetArray100'(others => 0);
      intValCopy   : Asn1UInt;
   begin
      intValCopy := IntVal;
      while intValCopy > 0 and totalNibbles <= 18 loop
        --# assert totalNibbles>=1 and totalNibbles<=18 and K~ + 1>=S'First and K~ + 76 <= S'Last and K~=K;
         tmp (totalNibbles) := Interfaces.Unsigned_8 (intValCopy mod 10);
         totalNibbles       := totalNibbles + 1;
         intValCopy         := intValCopy / 10;
      end loop;

      totalNibbles := totalNibbles - 1;

      for I in reverse Integer range 1 .. totalNibbles loop
        --# assert totalNibbles>=1 and totalNibbles<=18 and K~ + 1>=S'First and K~ + 76 <= S'Last and K = K~ + (totalNibbles-I)*4;
         BitStream_AppendPartialByte (S, K, tmp (I), 4, False);
      end loop;
      BitStream_AppendPartialByte (S, K, 16#F#, 4, False);

   end Acn_Enc_Int_BCD_VarSize_NullTerminated;

   procedure Acn_Enc_Int_ASCII_VarSize_LengthEmbedded
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int)
   is
      pragma SPARK_Mode (Off);
   begin
      null;
   end Acn_Enc_Int_ASCII_VarSize_LengthEmbedded;

   procedure Acn_Enc_Int_ASCII_VarSize_NullTerminated
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int)
   is
      pragma SPARK_Mode (Off);
   begin
      null;
   end Acn_Enc_Int_ASCII_VarSize_NullTerminated;

   procedure Acn_Dec_Int_PositiveInteger_ConstSize
     (S           : in     BitArray;
      K           : in out DECODE_PARAMS;
      IntVal      :    out Asn1UInt;
      minVal      : in     Asn1UInt;
      maxVal      : in     Asn1UInt;
      nSizeInBits : in     Integer;
      Result      :    out ASN1_RESULT)
   is
      encVal : Asn1UInt;
   begin
      Result.ErrorCode := 0;
      BitStream_Decode_Non_Negative_Integer
        (S,
         K,
         encVal,
         nSizeInBits,
         Result.Success);
      if Result.Success then
         IntVal := encVal;

         Result.Success := IntVal >= minVal and IntVal <= maxVal;
         if not Result.Success then
            IntVal           := minVal;
            Result.ErrorCode := ERR_INCORRECT_STREAM;
         end if;
      else
         IntVal           := minVal;
         Result.ErrorCode := ERR_INCORRECT_STREAM;
      end if;
   end Acn_Dec_Int_PositiveInteger_ConstSize;

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_8
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_PositiveInteger_ConstSize
        (S,
         K,
         IntVal,
         minVal,
         maxVal,
         8,
         Result);
   end Acn_Dec_Int_PositiveInteger_ConstSize_8;

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_PositiveInteger_ConstSize
        (S,
         K,
         IntVal,
         minVal,
         maxVal,
         16,
         Result);
   end Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16;

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_PositiveInteger_ConstSize
        (S,
         K,
         IntVal,
         minVal,
         maxVal,
         32,
         Result);
   end Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32;

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_PositiveInteger_ConstSize
        (S,
         K,
         IntVal,
         minVal,
         maxVal,
         64,
         Result);
   end Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64;

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT)
   is
      tmp : OctetArray2 := OctetArray2'(others => 0);
      I   : Integer;
      ret : Asn1UInt;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      ret := minVal;
      if RequiresReverse (K.K > 0) then
         I := RANGE_1_2'Last;
         while Result.Success and I >= RANGE_1_2'First loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(8-I) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I - 1;
         end loop;
      else
         I := RANGE_1_2'First;
         while Result.Success and I <= RANGE_1_2'Last loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(I-1) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I + 1;
         end loop;
      end if;
      if Result.Success then
         ret            := Asn1UInt (OctetArray2_to_UInt16 (tmp));
         Result.Success := ret >= minVal and ret <= maxVal;
      end if;
      if Result.Success then
         IntVal := ret;
      else
         IntVal := minVal;
      end if;
   end Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16;

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT)
   is
      tmp : OctetArray4 := OctetArray4'(others => 0);
      I   : Integer;
      ret : Asn1UInt;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      ret := minVal;
      if RequiresReverse (K.K > 0) then
         I := RANGE_1_4'Last;
         while Result.Success and I >= RANGE_1_4'First loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(8-I) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I - 1;
         end loop;
      else
         I := RANGE_1_4'First;
         while Result.Success and I <= RANGE_1_4'Last loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(I-1) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I + 1;
         end loop;
      end if;
      if Result.Success then
         ret            := Asn1UInt (OctetArray4_to_UInt32 (tmp));
         Result.Success := ret >= minVal and ret <= maxVal;
      end if;
      if Result.Success then
         IntVal := ret;
      else
         IntVal := minVal;
      end if;
   end Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32;

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT)
   is
      tmp : OctetArray8 := OctetArray8'(others => 0);
      I   : Integer;
      ret : Asn1UInt;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      ret := minVal;
      if RequiresReverse (K.K > 0) then
         I := RANGE_1_8'Last;
         while Result.Success and I >= RANGE_1_8'First loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(8-I) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I - 1;
         end loop;
      else
         I := RANGE_1_8'First;
         while Result.Success and I <= RANGE_1_8'Last loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(I-1) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I + 1;
         end loop;
      end if;
      if Result.Success then
         ret            := OctetArray8_to_Asn1UInt (tmp);
         Result.Success := ret >= minVal and ret <= maxVal;
      end if;
      if Result.Success then
         IntVal := ret;
      else
         IntVal := minVal;
      end if;
   end Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64;

   procedure Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      Result :    out ASN1_RESULT)
   is
      NBytes : Asn1Int;
      Ret    : Asn1UInt;
   begin

      IntVal           := minVal;
      Result.ErrorCode := 0;
      UPER_Dec_ConstraintWholeNumber (S, K, NBytes, 0, 255, 8, Result.Success);
      if Result.Success and NBytes >= 1 and NBytes <= 8 then
         Dec_UInt (S, K, Integer (NBytes), Ret, Result.Success);
         if Result.Success then
            IntVal         := Ret;
            Result.Success := IntVal >= minVal;
            if not Result.Success then
               IntVal           := minVal;
               Result.ErrorCode := ERR_INCORRECT_STREAM;
            end if;
         else
            Result.ErrorCode := ERR_INCORRECT_STREAM;
         end if;
      else
         Result.ErrorCode := ERR_INCORRECT_STREAM;
         Result.Success   := False;
      end if;
   end Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded;

   procedure Acn_Dec_Int_TwosComplement_ConstSize
     (S           : in     BitArray;
      K           : in out DECODE_PARAMS;
      IntVal      :    out Asn1Int;
      minVal      : in     Asn1Int;
      maxVal      : in     Asn1Int;
      nSizeInBits : in     Integer;
      Result      :    out ASN1_RESULT)
   is
      encVal : Asn1UInt;
   begin
      Result.ErrorCode := 0;
      BitStream_Decode_Non_Negative_Integer
        (S,
         K,
         encVal,
         nSizeInBits,
         Result.Success);
      if Result.Success then
         IntVal         := To_Int_n (encVal, nSizeInBits);
         Result.Success := IntVal >= minVal and IntVal <= maxVal;
         if not Result.Success then
            IntVal           := minVal;
            Result.ErrorCode := ERR_INCORRECT_STREAM;
         end if;
      else
         IntVal           := minVal;
         Result.ErrorCode := ERR_INCORRECT_STREAM;
      end if;
   end Acn_Dec_Int_TwosComplement_ConstSize;

   procedure Acn_Dec_Int_TwosComplement_ConstSize_8
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_TwosComplement_ConstSize
        (S,
         K,
         IntVal,
         minVal,
         maxVal,
         8,
         Result);
   end Acn_Dec_Int_TwosComplement_ConstSize_8;

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_TwosComplement_ConstSize
        (S,
         K,
         IntVal,
         minVal,
         maxVal,
         16,
         Result);
   end Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16;

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_TwosComplement_ConstSize
        (S,
         K,
         IntVal,
         minVal,
         maxVal,
         32,
         Result);
   end Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32;

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT)
   is
   begin
      Acn_Dec_Int_TwosComplement_ConstSize
        (S,
         K,
         IntVal,
         minVal,
         maxVal,
         64,
         Result);
   end Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64;

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT)
   is
      tmp : OctetArray2 := OctetArray2'(others => 0);
      I   : Integer;
      ret : Asn1Int;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      ret := minVal;
      if RequiresReverse (K.K > 0) then
         I := RANGE_1_2'Last;
         while Result.Success and I >= RANGE_1_2'First loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(8-I) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I - 1;
         end loop;
      else
         I := RANGE_1_2'First;
         while Result.Success and I <= RANGE_1_2'Last loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(I-1) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I + 1;
         end loop;
      end if;
      if Result.Success then
         ret := Asn1Int (UInt16_Int16 (OctetArray2_to_UInt16 (tmp)));
         Result.Success := ret >= minVal and ret <= maxVal;
      end if;
      if Result.Success then
         IntVal := ret;
      else
         IntVal := minVal;
      end if;
   end Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16;

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT)
   is
      tmp : OctetArray4 := OctetArray4'(others => 0);
      I   : Integer;
      ret : Asn1Int;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      ret := minVal;
      if RequiresReverse (K.K > 0) then
         I := RANGE_1_4'Last;
         while Result.Success and I >= RANGE_1_4'First loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(8-I) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I - 1;
         end loop;
      else
         I := RANGE_1_4'First;
         while Result.Success and I <= RANGE_1_4'Last loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(I-1) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I + 1;
         end loop;
      end if;
      if Result.Success then
         ret := Asn1Int (UInt32_Int32 (OctetArray4_to_UInt32 (tmp)));
         Result.Success := ret >= minVal and ret <= maxVal;
      end if;
      if Result.Success then
         IntVal := ret;
      else
         IntVal := minVal;
      end if;
   end Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32;

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT)
   is
      tmp : OctetArray8 := OctetArray8'(others => 0);
      I   : Integer;
      ret : Asn1Int;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      ret := minVal;
      if RequiresReverse (K.K > 0) then
         I := RANGE_1_8'Last;
         while Result.Success and I >= RANGE_1_8'First loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(8-I) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I - 1;
         end loop;
      else
         I := RANGE_1_8'First;
         while Result.Success and I <= RANGE_1_8'Last loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(I-1) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I + 1;
         end loop;
      end if;
      if Result.Success then
         ret            := To_Int (OctetArray8_to_Asn1UInt (tmp));
         Result.Success := ret >= minVal and ret <= maxVal;
      end if;
      if Result.Success then
         IntVal := ret;
      else
         IntVal := minVal;
      end if;
   end Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64;

   function UInt16_to_OctetArray2
     (x : Interfaces.Unsigned_16) return OctetArray2
   is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion
        (Interfaces.Unsigned_16,
         OctetArray2);
   begin
      return do_it (x);
   end UInt16_to_OctetArray2;

   function OctetArray2_to_UInt16
     (x : OctetArray2) return Interfaces.Unsigned_16
   is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion
        (OctetArray2,
         Interfaces.Unsigned_16);
   begin
      return do_it (x);
   end OctetArray2_to_UInt16;

   function UInt32_to_OctetArray4
     (x : Interfaces.Unsigned_32) return OctetArray4
   is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion
        (Interfaces.Unsigned_32,
         OctetArray4);
   begin
      return do_it (x);
   end UInt32_to_OctetArray4;

   function OctetArray4_to_UInt32
     (x : OctetArray4) return Interfaces.Unsigned_32
   is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion
        (OctetArray4,
         Interfaces.Unsigned_32);
   begin
      return do_it (x);
   end OctetArray4_to_UInt32;

   function Asn1UInt_to_OctetArray8 (x : Asn1UInt) return OctetArray8 is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion (Asn1UInt, OctetArray8);
   begin
      return do_it (x);
   end Asn1UInt_to_OctetArray8;

   function OctetArray8_to_Asn1UInt (x : OctetArray8) return Asn1UInt is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion (OctetArray8, Asn1UInt);
   begin
      return do_it (x);
   end OctetArray8_to_Asn1UInt;

   function Int16_to_OctetArray2
     (x : Interfaces.Integer_16) return OctetArray2
   is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion
        (Interfaces.Integer_16,
         OctetArray2);
   begin
      return do_it (x);
   end Int16_to_OctetArray2;

   function OctetArray2_to_Int16
     (x : OctetArray2) return Interfaces.Integer_16
   is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion
        (OctetArray2,
         Interfaces.Integer_16);
   begin
      return do_it (x);
   end OctetArray2_to_Int16;

   function Int32_to_OctetArray4
     (x : Interfaces.Integer_32) return OctetArray4
   is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion
        (Interfaces.Integer_32,
         OctetArray4);
   begin
      return do_it (x);
   end Int32_to_OctetArray4;

   function OctetArray4_to_Int32
     (x : OctetArray4) return Interfaces.Integer_32
   is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion
        (OctetArray4,
         Interfaces.Integer_32);
   begin
      return do_it (x);
   end OctetArray4_to_Int32;

   function Asn1Int_to_OctetArray8 (x : Asn1Int) return OctetArray8 is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion (Asn1Int, OctetArray8);
   begin
      return do_it (x);
   end Asn1Int_to_OctetArray8;

   function OctetArray8_to_Asn1Int (x : OctetArray8) return Asn1Int is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion (OctetArray8, Asn1Int);
   begin
      return do_it (x);
   end OctetArray8_to_Asn1Int;

   function Float_to_OctetArray4 (x : Float) return OctetArray4 is
--        pragma SPARK_Mode(Off);
      function do_it is new Ada.Unchecked_Conversion (Float, OctetArray4);
   begin
      return do_it (x);
   end Float_to_OctetArray4;

   function Long_Float_to_OctetArray8 (x : Asn1Real) return OctetArray8 is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion (Asn1Real, OctetArray8);
   begin
      return do_it (x);
   end Long_Float_to_OctetArray8;

   function OctetArray4_to_Float (x : OctetArray4) return Float is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion (OctetArray4, Float);
   begin
      return do_it (x);
   end OctetArray4_to_Float;

   function OctetArray8_to_Long_Float (x : OctetArray8) return Asn1Real is
      pragma SPARK_Mode (Off);
      function do_it is new Ada.Unchecked_Conversion (OctetArray8, Asn1Real);
   begin
      return do_it (x);
   end OctetArray8_to_Long_Float;

   procedure Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      Result :    out ASN1_RESULT)
   is
   begin
      Result.ErrorCode := ERR_INCORRECT_STREAM;
      UPER_Dec_UnConstraintWholeNumber (S, K, IntVal, Result.Success);
   end Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded;

   procedure Acn_Dec_Int_BCD_ConstSize
     (S        : in     BitArray;
      K        : in out DECODE_PARAMS;
      IntVal   :    out Asn1UInt;
      minVal   : in     Asn1UInt;
      maxVal   : in     Asn1UInt;
      nNibbles : in     Integer;
      Result   :    out ASN1_RESULT)
   is
      digit   : Asn1Byte;
      I       : Integer;
      max_aux : constant Asn1UInt := Asn1UInt'Last / 10 - 9;
   begin
      IntVal := 0;
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      I := nNibbles;
      while I > 0 and Result.Success loop
        --# assert K~.K+1>= S'First and  K~.K + 4*nNibbles <= S'Last and
        --#        I>0 and I<=nNibbles and
        --#        K.K = K~.K + (nNibbles-I)*4;
         BitStream_ReadNibble (S, K, digit, Result.Success);

         Result.Success :=
           Result.Success and digit <= 9 and IntVal >= 0 and IntVal <= max_aux;
         if Result.Success then
            IntVal := IntVal * 10;
            IntVal := IntVal + Asn1UInt (digit);
            I      := I - 1;
         end if;
      end loop;

      Result.Success :=
        Result.Success and then ((IntVal >= minVal) and (IntVal <= maxVal));
      if not Result.Success then
         IntVal := minVal;
      end if;
   end Acn_Dec_Int_BCD_ConstSize;

   procedure Acn_Dec_Int_BCD_VarSize_LengthEmbedded
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      Result :    out ASN1_RESULT)
   is
      pragma SPARK_Mode (Off);
   begin
      null;
   end Acn_Dec_Int_BCD_VarSize_LengthEmbedded;

   procedure Acn_Dec_Int_BCD_VarSize_NullTerminated
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT)
   is
      digit          : Asn1Byte;
      I              : Integer;
      max_aux        : constant Asn1UInt := Asn1UInt'Last / 10 - 9;
      stopDigitFound : Boolean           := False;
   begin
      IntVal := 0;
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      I := 0;
      while Result.Success and (not stopDigitFound) and I < 18 loop
        --# assert K~.K+1>= S'First and  K~.K + 76 <= S'Last and
        --#        I>=0 and I<18 and
        --#        K.K = K~.K + I*4;
         BitStream_ReadNibble (S, K, digit, Result.Success);

         Result.Success :=
           Result.Success and
           (digit <= 9 or digit = 16#F#) and
           IntVal >= 0 and
           IntVal <= max_aux;
         stopDigitFound := digit = 16#F#;
         if Result.Success and (not stopDigitFound) then
            IntVal := IntVal * 10;
            IntVal := IntVal + Asn1UInt (digit);
         end if;
         I := I + 1;
      end loop;

      Result.Success :=
        Result.Success and then ((IntVal >= minVal) and (IntVal <= maxVal));
      if not Result.Success then
         IntVal := minVal;
      end if;
   end Acn_Dec_Int_BCD_VarSize_NullTerminated;

   procedure Acn_Enc_Int_ASCII_ConstSize
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int;
      nChars : in     Integer)
   is
      I          : Integer;
      tmp : OctetArray100 := OctetArray100'(others => Character'Pos ('0'));
      intValCopy : Asn1Int;
   begin
      if IntVal >= 0 then
         tmp (1)    := Character'Pos ('+');
         intValCopy := IntVal;
      else
         tmp (1)    := Character'Pos ('-');
         intValCopy := -IntVal;
      end if;

      I := nChars;
      while intValCopy > 0 and I >= 2 loop
        --# assert I>=2 and I<=nChars and K~ + 1>=S'First and K~ + 8*nChars <= S'Last and K~=K;
         tmp (I) :=
           Interfaces.Unsigned_8 (Character'Pos ('0') + (intValCopy mod 10));
         I          := I - 1;
         intValCopy := intValCopy / 10;
      end loop;

      for J in Integer range 1 .. nChars loop
        --# assert K~ + 1>=S'First and K~ + 8*nChars <= S'Last and K = K~ + (J-1)*8;
         BitStream_AppendByte (S, K, tmp (J), False);
      end loop;
   end Acn_Enc_Int_ASCII_ConstSize;

   procedure Acn_Enc_UInt_ASCII_ConstSize
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt;
      nChars : in     Integer)
   is
      I          : Integer;
      tmp : OctetArray100 := OctetArray100'(others => Character'Pos ('0'));
      intValCopy : Asn1UInt;
   begin
      intValCopy := IntVal;

      I := nChars;
      while intValCopy > 0 and I >= 2 loop
        --# assert I>=2 and I<=nChars and K~ + 1>=S'First and K~ + 8*nChars <= S'Last and K~=K;
         tmp (I) :=
           Interfaces.Unsigned_8 (Character'Pos ('0') + (intValCopy mod 10));
         I          := I - 1;
         intValCopy := intValCopy / 10;
      end loop;

      for J in Integer range 1 .. nChars loop
        --# assert K~ + 1>=S'First and K~ + 8*nChars <= S'Last and K = K~ + (J-1)*8;
         BitStream_AppendByte (S, K, tmp (J), False);
      end loop;
   end Acn_Enc_UInt_ASCII_ConstSize;

   procedure Acn_Dec_Int_ASCII_ConstSize
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      nChars : in     Integer;
      Result :    out ASN1_RESULT)
   is
      digit : Asn1Byte;
      Ch    : Character;

      I        : Integer;
      max_aux  : constant Asn1Int := Asn1Int'Last / 10 - 9;
      negative : Boolean          := False;
   begin
      IntVal := 0;
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      BitStream_DecodeByte (S, K, digit, Result.Success);
      if Result.Success then
         if digit = Character'Pos ('+') then
            negative := False;
         elsif digit = Character'Pos ('-') then
            negative := True;
         else
            Result.Success := False;
         end if;
         I := nChars - 1;
         while I > 0 and Result.Success loop
                --# assert K~.K+1>= S'First and  K~.K + 8*nChars <= S'Last and
                --#        I>0 and I<=nChars-1 and
                --#        K.K = K~.K + (nChars-I)*8;
            BitStream_DecodeByte (S, K, digit, Result.Success);
            Ch    := Character'Val (digit);
            digit := Character'Pos (Ch) - Character'Pos ('0');

            Result.Success :=
              Result.Success and
              digit <= 9 and
              IntVal >= 0 and
              IntVal <= max_aux;
            if Result.Success then
               IntVal := IntVal * 10;
               IntVal := IntVal + Asn1Int (digit);
               I      := I - 1;
            end if;
         end loop;
         if negative and IntVal > Asn1Int'First then
            IntVal := -IntVal;
         end if;

         Result.Success :=
           Result.Success and then ((IntVal >= minVal) and (IntVal <= maxVal));
      end if;
      if not Result.Success then
         IntVal := minVal;
      end if;
   end Acn_Dec_Int_ASCII_ConstSize;

   procedure Acn_Dec_UInt_ASCII_ConstSize
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      nChars : in     Integer;
      Result :    out ASN1_RESULT)
   is
      digit : Asn1Byte;
      Ch    : Character;

      I        : Integer;
      max_aux  : constant Asn1UInt := Asn1UInt'Last / 10 - 9;
      negative : Boolean           := False;
   begin
      IntVal := 0;
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      I := nChars;
      while I > 0 and Result.Success loop
            --# assert K~.K+1>= S'First and  K~.K + 8*nChars <= S'Last and
            --#        I>0 and I<=nChars-1 and
            --#        K.K = K~.K + (nChars-I)*8;
         BitStream_DecodeByte (S, K, digit, Result.Success);
         Ch    := Character'Val (digit);
         digit := Character'Pos (Ch) - Character'Pos ('0');

         Result.Success := Result.Success and digit <= 9 and IntVal <= max_aux;
         if Result.Success then
            IntVal := IntVal * 10;
            IntVal := IntVal + Asn1UInt (digit);
            I      := I - 1;
         end if;
      end loop;

      Result.Success :=
        Result.Success and then ((IntVal >= minVal) and (IntVal <= maxVal));
      if not Result.Success then
         IntVal := minVal;
      end if;
   end Acn_Dec_UInt_ASCII_ConstSize;

   procedure Acn_Dec_Int_ASCII_VarSize_LengthEmbedded
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      Result :    out ASN1_RESULT)
   is
      pragma SPARK_Mode (Off);
   begin
      null;
   end Acn_Dec_Int_ASCII_VarSize_LengthEmbedded;

   procedure Acn_Dec_Int_ASCII_VarSize_NullTerminated
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      Result :    out ASN1_RESULT)
   is
      pragma SPARK_Mode (Off);
   begin
      null;
   end Acn_Dec_Int_ASCII_VarSize_NullTerminated;

   function Long_Float_to_Float (x : Asn1Real) return Float is
--        pragma SPARK_Mode(Off);
   begin
      return Float (x);
   end Long_Float_to_Float;

   procedure Acn_Enc_Real_IEEE754_32_big_endian
     (S       : in out BitArray;
      K       : in out Natural;
      RealVal : in     Asn1Real)
   is
      tmp : OctetArray4;
   begin
      tmp := Float_to_OctetArray4 (Long_Float_to_Float (RealVal));
      if RequiresReverse (K > 0) then
         for I in reverse RANGE_1_4 loop
                --# assert K~+1>= S'First and K~ + 32 <= S'Last and K = K~ + 8*(4-I);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      else
         for I in RANGE_1_4 loop
                --# assert K~+1>= S'First and K~ + 32 <= S'Last and K = K~ + 8*(I-1);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      end if;
   end Acn_Enc_Real_IEEE754_32_big_endian;

   procedure Acn_Dec_Real_IEEE754_32_big_endian
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      RealVal :    out Asn1Real;
      Result  :    out ASN1_RESULT)
   is
      tmp : OctetArray4 := OctetArray4'(others => 0);
      I   : Integer;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      RealVal := 0.0;
      if RequiresReverse (K.K > 0) then
         I := RANGE_1_4'Last;
         while Result.Success and I >= RANGE_1_4'First loop
            --# assert K~.K+1>= S'First and K~.K + 32 <= S'Last and K.K = K~.K + 8*(4-I) AND I>=1 AND I<=4;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I - 1;
         end loop;
      else
         I := RANGE_1_4'First;
         while Result.Success and I <= RANGE_1_4'Last loop
            --# assert K~.K+1>= S'First and K~.K + 32 <= S'Last and K.K = K~.K + 8*(I-1) AND I>=1 AND I<=4;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I + 1;
         end loop;
      end if;
      if Result.Success then
         RealVal := Asn1Real (OctetArray4_to_Float (tmp));
      end if;
   end Acn_Dec_Real_IEEE754_32_big_endian;

   procedure Acn_Enc_Real_IEEE754_64_big_endian
     (S       : in out BitArray;
      K       : in out Natural;
      RealVal : in     Asn1Real)
   is
      tmp : OctetArray8;
   begin
      tmp := Long_Float_to_OctetArray8 (RealVal);
      if RequiresReverse (K > 0) then
         for I in reverse RANGE_1_8 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(8-I);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      else
         for I in RANGE_1_8 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(I-1);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      end if;
   end Acn_Enc_Real_IEEE754_64_big_endian;

   procedure Acn_Dec_Real_IEEE754_64_big_endian
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      RealVal :    out Asn1Real;
      Result  :    out ASN1_RESULT)
   is
      tmp : OctetArray8 := OctetArray8'(others => 0);
      I   : Integer;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      RealVal := 0.0;
      if RequiresReverse (K.K > 0) then
         I := RANGE_1_8'Last;
         while Result.Success and I >= RANGE_1_8'First loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(8-I) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I - 1;
         end loop;
      else
         I := RANGE_1_8'First;
         while Result.Success and I <= RANGE_1_8'Last loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(I-1) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I + 1;
         end loop;
      end if;
      if Result.Success then
         RealVal := OctetArray8_to_Long_Float (tmp);
      end if;
   end Acn_Dec_Real_IEEE754_64_big_endian;

   procedure Acn_Enc_Real_IEEE754_32_little_endian
     (S       : in out BitArray;
      K       : in out Natural;
      RealVal : in     Asn1Real)
   is
      tmp : OctetArray4;
   begin
      tmp := Float_to_OctetArray4 (Long_Float_to_Float (RealVal));
      if not RequiresReverse (K > 0) then
         for I in reverse RANGE_1_4 loop
                --# assert K~+1>= S'First and K~ + 32 <= S'Last and K = K~ + 8*(4-I);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      else
         for I in RANGE_1_4 loop
                --# assert K~+1>= S'First and K~ + 32 <= S'Last and K = K~ + 8*(I-1);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      end if;
   end Acn_Enc_Real_IEEE754_32_little_endian;

   procedure Acn_Dec_Real_IEEE754_32_little_endian
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      RealVal :    out Asn1Real;
      Result  :    out ASN1_RESULT)
   is
      tmp : OctetArray4 := OctetArray4'(others => 0);
      I   : Integer;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      RealVal := 0.0;
      if not RequiresReverse (K.K > 0) then
         I := RANGE_1_4'Last;
         while Result.Success and I >= RANGE_1_4'First loop
            --# assert K~.K+1>= S'First and K~.K + 32 <= S'Last and K.K = K~.K + 8*(4-I) AND I>=1 AND I<=4;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I - 1;
         end loop;
      else
         I := RANGE_1_4'First;
         while Result.Success and I <= RANGE_1_4'Last loop
            --# assert K~.K+1>= S'First and K~.K + 32 <= S'Last and K.K = K~.K + 8*(I-1) AND I>=1 AND I<=4;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I + 1;
         end loop;
      end if;
      if Result.Success then
         RealVal := Asn1Real (OctetArray4_to_Float (tmp));
      end if;
   end Acn_Dec_Real_IEEE754_32_little_endian;

   procedure Acn_Enc_Real_IEEE754_64_little_endian
     (S       : in out BitArray;
      K       : in out Natural;
      RealVal : in     Asn1Real)
   is
      tmp : OctetArray8;
   begin
      tmp := Long_Float_to_OctetArray8 (RealVal);
      if not RequiresReverse (K > 0) then
         for I in reverse RANGE_1_8 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(8-I);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      else
         for I in RANGE_1_8 loop
                --# assert K~+1>= S'First and K~ + 64 <= S'Last and K = K~ + 8*(I-1);
            BitStream_AppendByte (S, K, tmp (I), False);
         end loop;
      end if;
   end Acn_Enc_Real_IEEE754_64_little_endian;

   procedure Acn_Dec_Real_IEEE754_64_little_endian
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      RealVal :    out Asn1Real;
      Result  :    out ASN1_RESULT)
   is
      tmp : OctetArray8 := OctetArray8'(others => 0);
      I   : Integer;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      RealVal := 0.0;
      if not RequiresReverse (K.K > 0) then
         I := RANGE_1_8'Last;
         while Result.Success and I >= RANGE_1_8'First loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(8-I) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I - 1;
         end loop;
      else
         I := RANGE_1_8'First;
         while Result.Success and I <= RANGE_1_8'Last loop
            --# assert K~.K+1>= S'First and K~.K + 64 <= S'Last and K.K = K~.K + 8*(I-1) AND I>=1 AND I<=8;
            BitStream_DecodeByte (S, K, tmp (I), Result.Success);
            I := I + 1;
         end loop;
      end if;
      if Result.Success then
         RealVal := OctetArray8_to_Long_Float (tmp);
      end if;
   end Acn_Dec_Real_IEEE754_64_little_endian;

   procedure Acn_Enc_Boolean_true_pattern
     (S       : in out BitArray;
      K       : in out Natural;
      BoolVal : in     Asn1Boolean;
      pattern : in     BitArray)
   ---# derives S from S, BoolVal, K, pattern &
   ---#         K from  K, pattern;
   ---# pre  K+1>= S'First and K + pattern'Length <= S'Last;
   ---# post K = K~+pattern'Length;
   is
   begin
      if BoolVal then
         for I in Integer range pattern'Range loop
            --# assert K = K~ and K + 1>= S'First and K + pattern'Length <= S'Last and K+I-pattern'First+1 >=S'First  and K+I-pattern'First < S'Last;
            S (K + ((I - pattern'First) + 1)) := pattern (I);
         end loop;
      else
         for I in Integer range pattern'Range loop
            --# assert K = K~ and K + 1>= S'First and K + pattern'Length <= S'Last and K+I-pattern'First+1 >=S'First  and K+I-pattern'First < S'Last;
            S (K + ((I - pattern'First) + 1)) := not pattern (I);
         end loop;
      end if;
      K := K + pattern'Length;
   end Acn_Enc_Boolean_true_pattern;

   procedure Acn_Dec_Boolean_true_pattern
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      BoolVal :    out Asn1Boolean;
      pattern : in     BitArray;
      Result  :    out ASN1_RESULT)
   ---# derives  K      from K, pattern &
   ---#                 BoolVal from S,K, pattern &
   ---#          Result  from K, pattern;
   ---# pre  K.K+1>= S'First and K.K + pattern'Length <= S'Last;
   ---# post K.K = K~.K + pattern'Length;
   is
   begin
      BoolVal := True;
      for I in Integer range pattern'Range loop
        --# assert K.K = K~.K and K.K + 1>= S'First and K.K + pattern'Length <= S'Last and K.K+I-pattern'First+1 >=S'First  and K.K+I-pattern'First < S'Last;
         BoolVal :=
           BoolVal and S (K.K + ((I - pattern'First) + 1)) = pattern (I);
      end loop;
      K.K    := K.K + pattern'Length;
      Result :=
        ASN1_RESULT'
          (Success => K.DataLen - K.K >= 0, ErrorCode => ERR_INCORRECT_STREAM);
   end Acn_Dec_Boolean_true_pattern;

   procedure Acn_Enc_Boolean_false_pattern
     (S       : in out BitArray;
      K       : in out Natural;
      BoolVal : in     Asn1Boolean;
      pattern : in     BitArray)
   ---# derives S from S, BoolVal, K, pattern &
   ---#         K from  K, pattern;
   ---# pre  K+1>= S'First and K + pattern'Length <= S'Last;
   ---# post K = K~+pattern'Length;
   is
   begin
      if not BoolVal then
         for I in Integer range pattern'Range loop
            --# assert K = K~ and K + 1>= S'First and K + pattern'Length <= S'Last and K+I-pattern'First+1 >=S'First  and K+I-pattern'First < S'Last;
            S (K + ((I - pattern'First) + 1)) := pattern (I);
         end loop;
      else
         for I in Integer range pattern'Range loop
            --# assert K = K~ and K + 1>= S'First and K + pattern'Length <= S'Last and K+I-pattern'First+1 >=S'First  and K+I-pattern'First < S'Last;
            S (K + ((I - pattern'First) + 1)) := not pattern (I);
         end loop;
      end if;
      K := K + pattern'Length;
   end Acn_Enc_Boolean_false_pattern;

   procedure Acn_Dec_Boolean_false_pattern
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      BoolVal :    out Asn1Boolean;
      pattern : in     BitArray;
      Result  :    out ASN1_RESULT)
   ---# derives  K      from K, pattern &
   ---#                 BoolVal from S,K, pattern &
   ---#          Result  from K, pattern;
   ---# pre  K.K+1>= S'First and K.K + pattern'Length <= S'Last;
   ---# post K.K = K~.K + pattern'Length;
   is
   begin
      Acn_Dec_Boolean_true_pattern (S, K, BoolVal, pattern, Result);
      BoolVal := not BoolVal;
   end Acn_Dec_Boolean_false_pattern;

   procedure Acn_Enc_NullType_pattern
     (S       : in out BitArray;
      K       : in out Natural;
      encVal  : in     Asn1NullType;
      pattern : in     BitArray)
   ---# derives S from S, K, encVal, pattern &
   ---#         K from  K, pattern;
   ---# pre  K+1>= S'First and K + pattern'Length <= S'Last;
   ---# post K = K~+pattern'Length;
   is
   begin
      if encVal = 0 then
         for I in Integer range pattern'Range loop
            --# assert K = K~ and K + 1>= S'First and K + pattern'Length <= S'Last and K+I-pattern'First+1 >=S'First  and K+I-pattern'First < S'Last;
            S (K + ((I - pattern'First) + 1)) := pattern (I);
         end loop;
      else
         for I in Integer range pattern'Range loop
            --# assert K = K~ and K + 1>= S'First and K + pattern'Length <= S'Last and K+I-pattern'First+1 >=S'First  and K+I-pattern'First < S'Last;
            S (K + ((I - pattern'First) + 1)) := pattern (I);
         end loop;
      end if;
      K := K + pattern'Length;
   end Acn_Enc_NullType_pattern;

   procedure Acn_Dec_NullType_pattern
     (S        : in     BitArray;
      K        : in out DECODE_PARAMS;
      decValue :    out Asn1NullType;
      pattern  : in     BitArray;
      Result   :    out ASN1_RESULT)
      ---# derives  K      from K, pattern &
      ---#          Result  from K, pattern  &
      ---#          decValue from S, K, pattern;
      ---# pre  K.K+1>= S'First and K.K + pattern'Length <= S'Last;
      ---# post K.K = K~.K + pattern'Length;
   is
      BoolVal : Boolean := True;
   begin
      decValue := 0;
      for I in Integer range pattern'Range loop
        --# assert K.K = K~.K and K.K + 1>= S'First and K.K + pattern'Length <= S'Last and K.K+I-pattern'First+1 >=S'First  and K.K+I-pattern'First < S'Last;
         BoolVal :=
           BoolVal and S (K.K + ((I - pattern'First) + 1)) = pattern (I);
      end loop;
      K.K := K.K + pattern'Length;
      if not BoolVal then
         decValue := 1;
      end if;

      Result :=
        ASN1_RESULT'
          (Success   => K.DataLen - K.K >= 0 and BoolVal,
           ErrorCode => ERR_INCORRECT_STREAM);
   end Acn_Dec_NullType_pattern;

   procedure Acn_Enc_NullType_pattern2
     (S       : in out BitArray;
      K       : in out Natural;
      pattern : in     BitArray)
   ---# derives S from S, K, encVal, pattern &
   ---#         K from  K, pattern;
   ---# pre  K+1>= S'First and K + pattern'Length <= S'Last;
   ---# post K = K~+pattern'Length;
   is
   begin
      for I in Integer range pattern'Range loop
            --# assert K = K~ and K + 1>= S'First and K + pattern'Length <= S'Last and K+I-pattern'First+1 >=S'First  and K+I-pattern'First < S'Last;
         S (K + ((I - pattern'First) + 1)) := pattern (I);
      end loop;
      K := K + pattern'Length;
   end Acn_Enc_NullType_pattern2;

   procedure Acn_Dec_NullType_pattern2
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      pattern : in     BitArray;
      Result  :    out ASN1_RESULT)
      ---# derives  K      from K, pattern &
      ---#          Result  from K, pattern  &
      ---#          decValue from S, K, pattern;
      ---# pre  K.K+1>= S'First and K.K + pattern'Length <= S'Last;
      ---# post K.K = K~.K + pattern'Length;
   is
      BoolVal : Boolean := True;
   begin
      for I in Integer range pattern'Range loop
        --# assert K.K = K~.K and K.K + 1>= S'First and K.K + pattern'Length <= S'Last and K.K+I-pattern'First+1 >=S'First  and K.K+I-pattern'First < S'Last;
         BoolVal :=
           BoolVal and S (K.K + ((I - pattern'First) + 1)) = pattern (I);
      end loop;
      K.K := K.K + pattern'Length;

      Result :=
        ASN1_RESULT'
          (Success   => K.DataLen - K.K >= 0 and BoolVal,
           ErrorCode => ERR_INCORRECT_STREAM);
   end Acn_Dec_NullType_pattern2;

   procedure Acn_Enc_NullType
     (S      : in out BitArray;
      K      : in out Natural;
      encVal : in     Asn1NullType)
      ---# derives S from S, K, encVal &
      ---#         K from  K;
      ---# pre  K+1>= S'First and K <= S'Last;
      ---# post K = K~;
   is
      pragma SPARK_Mode (Off);
   begin
      null;
   end Acn_Enc_NullType;

   procedure Acn_Dec_NullType
     (S        : in     BitArray;
      K        : in out DECODE_PARAMS;
      decValue :    out Asn1NullType;
      Result   :    out ASN1_RESULT)
      ---# derives  K      from K &
      ---#          Result  from K  &
      ---#          decValue from S, K;
      ---# pre  K.K+1>= S'First and K.K  <= S'Last;
      ---# post K.K = K~.K ;
   is
      pragma SPARK_Mode (Off);
      pragma Unreferenced (S);
      pragma Unreferenced (K);
   begin
      decValue := 0;
      Result   := ASN1_RESULT'(Success => True, ErrorCode => 0);
   end Acn_Dec_NullType;

   procedure Acn_Enc_String_Ascii_FixSize
     (S      : in out BitArray;
      K      : in out Natural;
      strVal : in     String)
   is
      I : Integer := strVal'First;
   begin
      while I <= strVal'Last - 1 loop
             --# assert I>=1 AND I<=str'Last-1;
         UPER_Enc_ConstraintWholeNumber
           (S,
            K,
            Asn1Int (CharacterPos (strVal (I))),
            0,
            8);

         I := I + 1;
      end loop;

   end Acn_Enc_String_Ascii_FixSize;

   procedure Acn_Dec_String_Ascii_FixSize
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      strVal : in out String;
      Result :    out ASN1_RESULT)
   is
      I         : Integer := strVal'First;
      charIndex : Integer;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      while I <= strVal'Last - 1 and Result.Success loop
             --# assert I>=1 AND I<=str'Last-1;
         UPER_Dec_ConstraintWholeNumberInt
           (S,
            K,
            charIndex,
            0,
            255,
            8,
            Result.Success);
         strVal (I) := Character'Val (charIndex);

         I := I + 1;
      end loop;
      strVal (strVal'Last) := NUL;
   end Acn_Dec_String_Ascii_FixSize;

   procedure Acn_Enc_String_Ascii_Null_Teminated
     (S              : in out BitArray;
      K              : in out Natural;
      null_character : in     Integer;
      strVal         : in     String)
   is
      I : Integer := strVal'First;
   begin
      while I <= strVal'Last - 1 and then strVal (I) /= NUL loop
             --# assert I>=1 AND I<=str'Last-1;
         UPER_Enc_ConstraintWholeNumber
           (S,
            K,
            Asn1Int (CharacterPos (strVal (I))),
            0,
            8);

         I := I + 1;
      end loop;
      UPER_Enc_ConstraintWholeNumber (S, K, Asn1Int (null_character), 0, 8);

   end Acn_Enc_String_Ascii_Null_Teminated;

   procedure Acn_Dec_String_Ascii_Null_Teminated
     (S              : in     BitArray;
      K              : in out DECODE_PARAMS;
      null_character : in     Integer;
      strVal         : in out String;
      Result         :    out ASN1_RESULT)
   is
      I         : Integer := strVal'First;
      charIndex : Integer :=
        65; -- ascii code of 'A'. Let's hope that 'A' will never be null Character
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      while Result.Success
        and then I <= strVal'Last
        and then charIndex /= null_character
      loop
         pragma Loop_Invariant (I >= 1 and I <= strVal'Last);
         UPER_Dec_ConstraintWholeNumberInt
           (S,
            K,
            charIndex,
            0,
            255,
            8,
            Result.Success);
         if charIndex /= null_character then
            strVal (I) := Character'Val (charIndex);
         else
            strVal (I) := NUL;
         end if;

         I := I + 1;
      end loop;
      while I <= strVal'Last loop
         strVal (I) := NUL;
         I          := I + 1;
      end loop;

   end Acn_Dec_String_Ascii_Null_Teminated;

   procedure Acn_Enc_String_Ascii_Internal_Field_Determinant
     (S                            : in out BitArray;
      K                            : in out Natural;
      asn1Min                      :        Asn1Int;
      nLengthDeterminantSizeInBits : in     Integer;
      strVal                       : in     String)
   is
      I : Integer := strVal'First;
   begin
      UPER_Enc_ConstraintWholeNumber
        (S,
         K,
         Asn1Int (getStringSize (strVal)),
         asn1Min,
         nLengthDeterminantSizeInBits);
      while I <= strVal'Last - 1 and then strVal (I) /= NUL loop
         pragma Loop_Invariant (I >= 1 and I <= strVal'Last);
         UPER_Enc_ConstraintWholeNumber
           (S,
            K,
            Asn1Int (CharacterPos (strVal (I))),
            0,
            8);

         I := I + 1;
      end loop;

   end Acn_Enc_String_Ascii_Internal_Field_Determinant;

   procedure Acn_Dec_String_Ascii_Internal_Field_Determinant
     (S                            : in     BitArray;
      K                            : in out DECODE_PARAMS;
      asn1Min                      :        Asn1Int;
      asn1Max                      :        Asn1Int;
      nLengthDeterminantSizeInBits : in     Integer;
      strVal                       : in out String;
      Result                       :    out ASN1_RESULT)
   is
      I         : Integer := strVal'First;
      nSize     : Integer;
      charIndex : Integer;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);

      UPER_Dec_ConstraintWholeNumberInt
        (S,
         K,
         nSize,
         Integer (asn1Min),
         Integer (asn1Max),
         nLengthDeterminantSizeInBits,
         Result.Success);
      while Result.Success and then I <= strVal'Last - 1 and then I <= nSize
      loop
         pragma Loop_Invariant (I >= 1 and I <= strVal'Last);
         UPER_Dec_ConstraintWholeNumberInt
           (S,
            K,
            charIndex,
            0,
            255,
            8,
            Result.Success);
         strVal (I) := Character'Val (charIndex);

         I := I + 1;
      end loop;
      while I <= strVal'Last loop
         strVal (I) := NUL;
         I          := I + 1;
      end loop;

   end Acn_Dec_String_Ascii_Internal_Field_Determinant;

   procedure Acn_Enc_String_Ascii_External_Field_Determinant
     (S      : in out BitArray;
      K      : in out Natural;
      strVal : in     String)
   is
      I : Integer := strVal'First;
   begin
      while I <= strVal'Last - 1 and then strVal (I) /= NUL loop
             --# assert I>=1 AND I<=str'Last-1;
         pragma Loop_Invariant (I >= 1 and I <= strVal'Last - 1);
         UPER_Enc_ConstraintWholeNumber
           (S,
            K,
            Asn1Int (CharacterPos (strVal (I))),
            0,
            8);

         I := I + 1;
      end loop;

   end Acn_Enc_String_Ascii_External_Field_Determinant;

   procedure Acn_Dec_String_Ascii_External_Field_Determinant
     (S                    : in     BitArray;
      K                    : in out DECODE_PARAMS;
      extSizeDeterminatFld : in     Asn1Int;
      strVal               : in out String;
      Result               :    out ASN1_RESULT)
   is
      I         : Integer := strVal'First;
      charIndex : Integer;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);

      while Result.Success
        and then I <= strVal'Last - 1
        and then I <= Integer (extSizeDeterminatFld)
      loop
             --# assert I>=1 AND I<=str'Last-1;
         pragma Loop_Invariant (I >= 1 and I <= strVal'Last - 1);
         UPER_Dec_ConstraintWholeNumberInt
           (S,
            K,
            charIndex,
            0,
            255,
            8,
            Result.Success);
         strVal (I) := Character'Val (charIndex);

         I := I + 1;
      end loop;
      while I <= strVal'Last loop
         strVal (I) := NUL;
         I          := I + 1;
      end loop;

   end Acn_Dec_String_Ascii_External_Field_Determinant;

-- -------------------------------------
   procedure Acn_Enc_String_CharIndex_FixSize
     (S         : in out BitArray;
      K         : in out Natural;
      charSet   :        String;
      nCharSize :        Integer;
      strVal    : in     String)
   is
      I         : Integer := strVal'First;
      charIndex : Integer;
   begin
      while I <= strVal'Last - 1 loop
             --# assert I>=1 AND I<=str'Last-1;
         pragma Loop_Invariant (I >= 1 and I <= strVal'Last - 1);
         charIndex := GetZeroBasedCharIndex (strVal (I), charSet);
         UPER_Enc_ConstraintWholeNumber
           (S,
            K,
            Asn1Int (charIndex),
            0,
            nCharSize);

         I := I + 1;
      end loop;

   end Acn_Enc_String_CharIndex_FixSize;

   procedure Acn_Dec_String_CharIndex_FixSize
     (S         : in     BitArray;
      K         : in out DECODE_PARAMS;
      charSet   :        String;
      nCharSize :        Integer;
      strVal    : in out String;
      Result    :    out ASN1_RESULT)
   is
      I         : Integer          := strVal'First;
      charIndex : Integer;
      asn1Max   : constant Integer := charSet'Last - 1;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);
      while I <= strVal'Last - 1 and Result.Success loop
             --# assert I>=1 AND I<=str'Last-1;
         pragma Loop_Invariant (I >= 1 and I <= strVal'Last - 1);
         UPER_Dec_ConstraintWholeNumberInt
           (S,
            K,
            charIndex,
            0,
            asn1Max,
            nCharSize,
            Result.Success);
         strVal (I) := charSet (charIndex + 1);

         I := I + 1;
      end loop;
      strVal (strVal'Last) := NUL;
   end Acn_Dec_String_CharIndex_FixSize;

   procedure Acn_Enc_String_CharIndex_External_Field_Determinant
     (S         : in out BitArray;
      K         : in out Natural;
      charSet   :        String;
      nCharSize :        Integer;
      strVal    : in     String)
   is
      I         : Integer := strVal'First;
      charIndex : Integer;
   begin
      while I <= strVal'Last - 1 and then strVal (I) /= NUL loop
             --# assert I>=1 AND I<=str'Last-1;
         charIndex := GetZeroBasedCharIndex (strVal (I), charSet);
         UPER_Enc_ConstraintWholeNumber
           (S,
            K,
            Asn1Int (charIndex),
            0,
            nCharSize);

         I := I + 1;
      end loop;

   end Acn_Enc_String_CharIndex_External_Field_Determinant;

   procedure Acn_Dec_String_CharIndex_External_Field_Determinant
     (S                    : in     BitArray;
      K                    : in out DECODE_PARAMS;
      charSet              :        String;
      nCharSize            :        Integer;
      extSizeDeterminatFld : in     Asn1Int;
      strVal               : in out String;
      Result               :    out ASN1_RESULT)
   is
      I         : Integer          := strVal'First;
      charIndex : Integer;
      asn1Max   : constant Integer := charSet'Last - 1;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);

      while Result.Success
        and then I <= strVal'Last - 1
        and then I <= Integer (extSizeDeterminatFld)
      loop
             --# assert I>=1 AND I<=str'Last-1;
         UPER_Dec_ConstraintWholeNumberInt
           (S,
            K,
            charIndex,
            0,
            asn1Max,
            nCharSize,
            Result.Success);
         strVal (I) := charSet (charIndex + 1);

         I := I + 1;
      end loop;
      while I <= strVal'Last loop
         strVal (I) := NUL;
         I          := I + 1;
      end loop;

   end Acn_Dec_String_CharIndex_External_Field_Determinant;

   procedure Acn_Enc_String_CharIndex_Internal_Field_Determinant
     (S                            : in out BitArray;
      K                            : in out Natural;
      charSet                      :        String;
      nCharSize                    :        Integer;
      asn1Min                      :        Asn1Int;
      nLengthDeterminantSizeInBits : in     Integer;
      strVal                       : in     String)
   is
      I         : Integer := strVal'First;
      charIndex : Integer;
   begin
      UPER_Enc_ConstraintWholeNumber
        (S,
         K,
         Asn1Int (getStringSize (strVal)),
         asn1Min,
         nLengthDeterminantSizeInBits);
      while I <= strVal'Last - 1 and then strVal (I) /= NUL loop
             --# assert I>=1 AND I<=str'Last-1;
         charIndex := GetZeroBasedCharIndex (strVal (I), charSet);
         UPER_Enc_ConstraintWholeNumber
           (S,
            K,
            Asn1Int (charIndex),
            0,
            nCharSize);

         I := I + 1;
      end loop;

   end Acn_Enc_String_CharIndex_Internal_Field_Determinant;

   procedure Acn_Dec_String_CharIndex_Internal_Field_Determinant
     (S                            : in     BitArray;
      K                            : in out DECODE_PARAMS;
      charSet                      :        String;
      nCharSize                    :        Integer;
      asn1Min                      :        Asn1Int;
      asn1Max                      :        Asn1Int;
      nLengthDeterminantSizeInBits : in     Integer;
      strVal                       : in out String;
      Result                       :    out ASN1_RESULT)
   is
      I         : Integer := strVal'First;
      nSize     : Integer;
      charIndex : Integer;
   begin
      Result :=
        ASN1_RESULT'(Success => True, ErrorCode => ERR_INSUFFICIENT_DATA);

      UPER_Dec_ConstraintWholeNumberInt
        (S,
         K,
         nSize,
         Integer (asn1Min),
         Integer (asn1Max),
         nLengthDeterminantSizeInBits,
         Result.Success);
      while Result.Success and then I <= strVal'Last - 1 and then I <= nSize
      loop
             --# assert I>=1 AND I<=str'Last-1;
         UPER_Dec_ConstraintWholeNumberInt
           (S,
            K,
            charIndex,
            0,
            charSet'Last - 1,
            nCharSize,
            Result.Success);
         strVal (I) := charSet (charIndex + 1);

         I := I + 1;
      end loop;
      while I <= strVal'Last loop
         strVal (I) := NUL;
         I          := I + 1;
      end loop;

   end Acn_Dec_String_CharIndex_Internal_Field_Determinant;

   function milbus_encode (IntVal : in Asn1Int) return Asn1Int is
      ret : Asn1Int;
   begin
      if IntVal = 32 then
         ret := 0;
      else
         ret := IntVal;
      end if;
      return ret;
   end milbus_encode;

   function milbus_decode (IntVal : in Asn1Int) return Asn1Int is
      ret : Asn1Int;
   begin
      if IntVal = 0 then
         ret := 32;
      else
         ret := IntVal;
      end if;
      return ret;
    end milbus_decode;



    -------------- XER ENCODING ------------------------------------------------

    WORD_ID          : CONSTANT Character := Character'Val(1);

   FUNCTION Strlen (Str : IN     STRING) RETURN INTEGER IS
      ret:INTEGER:=0;
   BEGIN
      ret:= Index(Str, To_SET(Ada.Characters.Latin_1.NUL), Outside, Backward);
      if ret = 0 THEN
         return Str'Length;
      End if;

      RETURN ret;
   END;



   PROCEDURE BS_Append_String(Strm :IN OUT CharStream; str:IN XString; Result :OUT ASN1_RESULT) IS
      len:INTEGER:=strlen(str);
   BEGIN
      Result := (Success => FALSE, ErrorCode=>ERR_INSUFFICIENT_DATA);
      IF (strm.CurrentByte + len - 1 > strm.Data'Last) THEN
          return;
      END IF;

      Strm.Data(strm.CurrentByte .. strm.CurrentByte+len-1):= str(str'First..str'First+len-1);
      Strm.CurrentByte := strm.CurrentByte + len;
      Result := (Success => TRUE, ErrorCode=>0);
   END BS_Append_String;

   PROCEDURE BS_PutSpace(Strm :IN OUT CharStream; level:IN integer; Result :OUT ASN1_RESULT) IS
   BEGIN
      IF Strm.EncodeWhiteSpace THEN
         BS_Append_String(Strm, level*"  ", Result);
      ELSE
         Result := (Success => TRUE, ErrorCode=>0);
      END IF;
   END BS_PutSpace;

   PROCEDURE BS_PutNL(Strm :IN OUT CharStream; Result :OUT ASN1_RESULT) IS
   BEGIN
      IF Strm.EncodeWhiteSpace THEN
         BS_Append_String(Strm, ""&Ada.Characters.Latin_1.LF, Result);
      ELSE
         Result := (Success => TRUE, ErrorCode=>0);
      END IF;
   END BS_PutNL;


   PROCEDURE Xer_EncodePrimitiveElement(Strm:IN OUT CharStream; elementTag:IN STRING; value:IN STRING; Result :OUT ASN1_RESULT; level:Integer) IS
   BEGIN
      BS_PutSpace(Strm, level, Result);
      IF NOT Result.Success THEN RETURN; END IF;

      IF strlen(value)/=0 THEN
        BS_Append_String(Strm,"<"&elementTag&">",Result);
        IF NOT Result.Success THEN RETURN; END IF;

        BS_Append_String(Strm,value,Result);
        IF NOT Result.Success THEN RETURN; END IF;

         BS_Append_String(Strm,"</"&elementTag&">",Result);
         IF NOT Result.Success THEN RETURN; END IF;
         BS_PutNL(Strm, Result);
      ELSE
         BS_Append_String(Strm,"<"&elementTag&"/>",Result);
         IF NOT Result.Success THEN RETURN; END IF;
         BS_PutNL(Strm, Result);
      END IF;

   END Xer_EncodePrimitiveElement;



   PROCEDURE Xer_EncodeXmlHeader(Strm :IN OUT CharStream; XmlHeader:IN XString; Result :OUT ASN1_RESULT) IS
   BEGIN

      if XmlHeader = "" THEN
         BS_Append_String(Strm,"<?xml version=""1.0"" encoding=""UTF-8""?>", result);
         IF NOT Result.Success THEN RETURN; END IF;
         BS_PutNL(Strm, Result);
      else
         BS_Append_String(Strm,XmlHeader, result);
         IF NOT Result.Success THEN RETURN; END IF;
         BS_PutNL(Strm, Result);
      end if;
   END;

   PROCEDURE Xer_EncodeComment(Strm :IN OUT CharStream; comment:IN XString; Result :OUT ASN1_RESULT) IS
   BEGIN
      null;
   END;


   PROCEDURE Xer_EncodeInteger(Strm :IN OUT CharStream; elementTag:IN XString; value: IN Asn1Int; Result :OUT ASN1_RESULT; level: IN Integer) IS
   BEGIN
      Xer_EncodePrimitiveElement(Strm, elementTag, Trim(value'Img, Ada.Strings.Both), Result,level);
   END;

   PROCEDURE Xer_EncodeBoolean(Strm :IN OUT CharStream; elementTag:IN XString; value: IN Asn1Boolean; Result :OUT ASN1_RESULT; level: IN Integer) IS
   BEGIN
      IF elementTag = "" THEN
         IF value THEN
            Xer_EncodePrimitiveElement(Strm, "true", "", Result,level);
         ELSE
            Xer_EncodePrimitiveElement(Strm, "false", "", Result,level);
         END IF;
      ELSE
         IF value THEN
            Xer_EncodePrimitiveElement(Strm, elementTag, "<true/>", Result,level);
         ELSE
            Xer_EncodePrimitiveElement(Strm, elementTag, "<false/>", Result,level);
         END IF;
      END IF;
   END;

   PROCEDURE Xer_EncodeEnumerated(Strm :IN OUT CharStream; elementTag:IN XString; value: IN XString; Result :OUT ASN1_RESULT; level: IN Integer) IS
   BEGIN
      IF elementTag = "" THEN
         Xer_EncodePrimitiveElement(Strm, value, "", Result,level);
      ELSE
         Xer_EncodePrimitiveElement(Strm, elementTag, "<"&value&"/>", Result,level);
      END IF;
   END;

   PROCEDURE Xer_EncodeReal(Strm :IN OUT CharStream; elementTag:IN XString; value: IN Asn1Real; Result :OUT ASN1_RESULT; level: IN Integer) IS
	s:String:= Trim(value'Img, Ada.Strings.Both);
   BEGIN
      FOR I IN s'Range LOOP
         if S(I) = '+' THEN
            S(I) := '0';
         END IF;
      END LOOP;

      Xer_EncodePrimitiveElement(Strm, elementTag, s, Result,level);
   END;

   PROCEDURE Xer_EncodeString(Strm :IN OUT CharStream; elementTag:IN XString; value: IN XString; Result :OUT ASN1_RESULT; level: IN Integer) IS
   BEGIN
      Xer_EncodePrimitiveElement(Strm, elementTag, value, Result,level);
   END;

   PROCEDURE Xer_EncodeOctetString(Strm :IN OUT CharStream; elementTag:IN XString;
                                   value: IN OctetBuffer; len: IN Integer; Result :OUT ASN1_RESULT; level: IN Integer) IS
      str:String:=10*' ';
      str2:String:=2*'0';
      i1,i2:INTEGER;
   BEGIN
      BS_PutSpace(Strm, level, Result);
      IF NOT Result.Success THEN RETURN; END IF;

      BS_Append_String(Strm,"<"&elementTag&">",Result);
      IF NOT Result.Success THEN RETURN; END IF;

      FOR I IN value'First..len LOOP
         Ada.Integer_Text_IO.Put(To => Str, Item => Integer(value(I)), Base => 16);
         i1 := Index(Str, "#", Forward)+1;
         i2 := Index(Str, "#", Backward)-1;

         str2:=2*'0';
         IF i2-i1 = 1 THEN
            str2:= str(i1..i2);
         ELSIF i2=i1 THEN
            str2(2..2):= str(i1..i2);
         ELSE
            RAISE PROGRAM_ERROR;
         END IF;

         BS_Append_String(Strm, str2 ,Result);
         IF NOT Result.Success THEN RETURN; END IF;
      END LOOP;
      BS_Append_String(Strm,"</"&elementTag&">",Result);
      IF NOT Result.Success THEN RETURN; END IF;
      BS_PutNL(Strm, Result);

      IF NOT Result.Success THEN RETURN; END IF;

   END;

   PROCEDURE Xer_EncodeBitString(Strm :IN OUT CharStream; elementTag:IN XString;
                                 value: IN BitArray; len: IN Integer; Result :OUT ASN1_RESULT; level: IN Integer) IS
   BEGIN
      BS_PutSpace(Strm, level, Result);
      IF NOT Result.Success THEN RETURN; END IF;

      BS_Append_String(Strm,"<"&elementTag&">",Result);
      IF NOT Result.Success THEN RETURN; END IF;

      FOR I IN value'First..len LOOP
         if value(I) = 1 THEN
            BS_Append_String(Strm, "1" ,Result);
         ELSE
            BS_Append_String(Strm, "0" ,Result);
         END IF;
         IF NOT Result.Success THEN RETURN; END IF;
      END LOOP;
      BS_Append_String(Strm,"</"&elementTag&">",Result);
      IF NOT Result.Success THEN RETURN; END IF;
      BS_PutNL(Strm, Result);

      IF NOT Result.Success THEN RETURN; END IF;
   END;


   TYPE Token IS
   RECORD
      TokenID : Character  :=Character'Val(0);
      ValueLength:Integer:=0;
      Value       : String  (1 .. 100) := (1..100 => ' ');
   END RECORD;


   FUNCTION IsPartOfID(C: IN Character ) RETURN BOOLEAN IS
   BEGIN
      IF Is_Alphanumeric(C) OR C='.' OR C='+' OR C='-' OR C='_' THEN
         return TRUE;
      END IF;

      return FALSE;
   END IsPartOfID;


   FUNCTION Is_Space(c: IN Character ) RETURN BOOLEAN IS
   BEGIN
      IF c = ' ' OR c = HT OR c = LF OR c = CR THEN
         return TRUE;
      END IF;
      RETURN FALSE;
   END Is_Space;



   PROCEDURE NT(Strm :IN OUT CharStream; tok: OUT Token) IS
      spTokens:String:="<>/=""";
      s1:String:=" ";
   BEGIN
      WHILE Is_Space(Strm.Data(Strm.CurrentByte)) LOOP
         Strm.CurrentByte := Strm.CurrentByte + 1;
      END LOOP;

      IF Strm.CurrentByte+1 <= Strm.Data'Last THEN
      IF Strm.Data(Strm.CurrentByte) = '<' AND  Strm.Data(Strm.CurrentByte+1) = '?' THEN
         Strm.CurrentByte := Strm.CurrentByte + 1;
         WHILE NOT (Strm.Data(Strm.CurrentByte-1) = '?' AND Strm.Data(Strm.CurrentByte) = '>') LOOP
            Strm.CurrentByte := Strm.CurrentByte + 1;
         END LOOP;
         Strm.CurrentByte := Strm.CurrentByte + 1;

	     WHILE Is_Space(Strm.Data(Strm.CurrentByte)) LOOP
           Strm.CurrentByte := Strm.CurrentByte + 1;
	     END LOOP;
      END IF;
      END IF;


      IF Strm.CurrentByte+2 <= Strm.Data'Last THEN
      IF Strm.Data(Strm.CurrentByte) = '<' AND Strm.Data(Strm.CurrentByte+1) = '!' AND Strm.Data(Strm.CurrentByte+2) = '-' AND Strm.Data(Strm.CurrentByte+1) = '-' THEN
         Strm.CurrentByte := Strm.CurrentByte + 2;
         WHILE NOT (Strm.Data(Strm.CurrentByte-2) = '-' AND Strm.Data(Strm.CurrentByte-1) = '-' AND Strm.Data(Strm.CurrentByte) = '>') LOOP
            Strm.CurrentByte := Strm.CurrentByte + 1;
         END LOOP;
         Strm.CurrentByte := Strm.CurrentByte + 1;

	     WHILE Is_Space(Strm.Data(Strm.CurrentByte)) LOOP
           Strm.CurrentByte := Strm.CurrentByte + 1;
	     END LOOP;
      END IF;
      END IF;

      s1(1):=Strm.Data(Strm.CurrentByte);

      IF Index(spTokens, s1, Forward) /= 0 THEN
         tok.TokenID := Strm.Data(Strm.CurrentByte);
         Strm.CurrentByte := Strm.CurrentByte + 1;
         return;
      END IF;

      tok.ValueLength:=1;
      WHILE IsPartOfID(Strm.Data(Strm.CurrentByte)) LOOP
         tok.TokenID := WORD_ID;
         tok.Value(tok.ValueLength):= Strm.Data(Strm.CurrentByte);
         Strm.CurrentByte := Strm.CurrentByte + 1;
         tok.ValueLength := tok.ValueLength + 1;
      END LOOP;

   END NT;


   PROCEDURE LA(Strm :IN OUT CharStream; tok: OUT Token) IS
      I:Integer:= Strm.CurrentByte;
   BEGIN
      NT(Strm, tok);
      Strm.CurrentByte:=I;
   END;

   PROCEDURE Xer_DecodePrimitiveElement(Strm :IN OUT CharStream; elementTag:IN XString; value: OUT XString; valLength: OUT INTEGER; Result :OUT ASN1_RESULT) IS
      tok:Token;
   BEGIN
      Result := (Success => FALSE, ErrorCode=>ERR_INCORRECT_STREAM);
      valLength:=0;

      NT(Strm,tok);
      IF tok.TokenID /='<' THEN RETURN; END IF;

      NT(Strm,tok);
      IF tok.TokenID /= WORD_ID OR tok.ValueLength =0 THEN RETURN; END IF;
      IF Trim(tok.Value, Ada.Strings.Both) /= Trim(elementTag, Ada.Strings.Both) THEN RETURN; END IF;

      NT(Strm,tok);
      IF tok.TokenID = '/' THEN
         NT(Strm,tok);
         IF tok.TokenID = '>' THEN
            Result := (Success => TRUE, ErrorCode=>0);
            return;
         END IF;
      ELSE
         IF tok.TokenID /='>' THEN RETURN; END IF;
      END IF;

      valLength:=0;
      WHILE strm.CurrentByte<=Strm.Data'Last AND strm.Data(strm.CurrentByte) /= '<' LOOP
         value(value'First + valLength):= strm.Data(strm.CurrentByte);
         strm.CurrentByte := strm.CurrentByte + 1;
         valLength := valLength + 1;
      END LOOP;

      NT(Strm,tok);
      IF tok.TokenID /='<' THEN RETURN; END IF;

      NT(Strm,tok);
      IF tok.TokenID /='/' THEN RETURN; END IF;

      NT(Strm,tok);
      IF tok.TokenID /= WORD_ID OR tok.ValueLength =0 THEN RETURN; END IF;
      IF Trim(tok.Value, Ada.Strings.Both) /= Trim(elementTag, Ada.Strings.Both) THEN RETURN; END IF;

      NT(Strm,tok);
      IF tok.TokenID /='>' THEN RETURN; END IF;

      Result := (Success => TRUE, ErrorCode=>0);
   END;



   PROCEDURE Xer_DecodeComplexElementStart(Strm :IN OUT CharStream; elementTag:IN XString; Result :OUT ASN1_RESULT) IS
      tok:Token;
   BEGIN
      Result := (Success => FALSE, ErrorCode=>ERR_INCORRECT_STREAM);

      NT(Strm,tok);
      IF tok.TokenID /='<' THEN RETURN; END IF;

      NT(Strm,tok);
      IF tok.TokenID /=WORD_ID OR tok.ValueLength =0 THEN RETURN; END IF;
      IF Trim(tok.Value, Ada.Strings.Both) /= Trim(elementTag, Ada.Strings.Both) THEN RETURN; END IF;

      NT(Strm,tok);
      IF tok.TokenID = '/' THEN
         NT(Strm,tok);
         IF tok.TokenID = '>' THEN
            Result := (Success => TRUE, ErrorCode=>0);
            return;
         END IF;
      ELSE
         IF tok.TokenID /='>' THEN RETURN; END IF;
      END IF;


      Result := (Success => TRUE, ErrorCode=>0);
   END;

   PROCEDURE Xer_DecodeComplexElementEnd(Strm :IN OUT CharStream; elementTag:IN XString; Result :OUT ASN1_RESULT) IS
      tok:Token;
   BEGIN
      Result := (Success => FALSE, ErrorCode=>ERR_INCORRECT_STREAM);

      NT(Strm,tok);
      IF tok.TokenID /='<' THEN RETURN; END IF;

      NT(Strm,tok);
      IF tok.TokenID /='/' THEN RETURN; END IF;

      NT(Strm,tok);
      IF tok.TokenID /=WORD_ID OR tok.ValueLength =0 THEN RETURN; END IF;
      IF Trim(tok.Value, Ada.Strings.Both) /= Trim(elementTag, Ada.Strings.Both) THEN RETURN; END IF;

      NT(Strm,tok);
      IF tok.TokenID /='>' THEN RETURN; END IF;

      Result := (Success => TRUE, ErrorCode=>0);
   END;



   PROCEDURE Xer_DecodeInteger(Strm :IN OUT CharStream; elementTag:IN XString; value: OUT Asn1Int; Result :OUT ASN1_RESULT) IS
      str:XString(1..100):=(1..100=> ' ');
      len:Integer;
   BEGIN
      value:=0;
      Xer_DecodePrimitiveElement(Strm, elementTag, str,  len, Result);
      IF NOT Result.Success THEN RETURN; END IF;
      value:=Asn1Int'value(str);
      Result := (Success => TRUE, ErrorCode=>0);
   END;

   PROCEDURE Xer_DecodeBoolean(Strm :IN OUT CharStream; elementTag:IN XString; value: OUT Asn1Boolean; Result :OUT ASN1_RESULT) IS
      str:XSTring(1..100):=(1..100=> ' ');
      len:Integer;
   BEGIN
      Result := (Success => FALSE, ErrorCode=>ERR_INCORRECT_STREAM);
      value:= FALSE;
      if elementTag /= "" THEN
         Xer_DecodeComplexElementStart(Strm, elementTag, Result);
         IF NOT Result.Success THEN RETURN; END IF;
      END IF;

      Xer_LA_NextElementTag(Strm, str, Result);
      IF NOT Result.Success THEN RETURN; END IF;

      IF Trim(str, Ada.Strings.Both) = "true" THEN
         value:=TRUE;
         Xer_DecodePrimitiveElement(Strm, "true", str,  len, Result);
         IF NOT Result.Success THEN RETURN; END IF;
      ELSE
         value:=FALSE;
         Xer_DecodePrimitiveElement(Strm, "false", str,  len, Result);
         IF NOT Result.Success THEN RETURN; END IF;
      END IF;


      if elementTag /= "" THEN
         Xer_DecodeComplexElementEnd(Strm, elementTag, Result);
         IF NOT Result.Success THEN RETURN; END IF;
      END IF;

   END;

   PROCEDURE Xer_DecodeEnumerated(Strm :IN OUT CharStream; elementTag:IN XString; value: OUT XString; Result :OUT ASN1_RESULT) IS
      str:XSTring(1..100):=(1..100=> ' ');
      len:Integer;
   BEGIN
      Result := (Success => FALSE, ErrorCode=>ERR_INCORRECT_STREAM);

      if elementTag /= "" THEN
         Xer_DecodeComplexElementStart(Strm, elementTag, Result);
         IF NOT Result.Success THEN RETURN; END IF;
      END IF;

      Xer_LA_NextElementTag(Strm, value, Result);
      IF NOT Result.Success THEN RETURN; END IF;

      Xer_DecodePrimitiveElement(Strm, value, str,  len, Result);
      IF NOT Result.Success THEN RETURN; END IF;

      if elementTag /= "" THEN
         Xer_DecodeComplexElementEnd(Strm, elementTag, Result);
         IF NOT Result.Success THEN RETURN; END IF;
      END IF;
   END;

   PROCEDURE Xer_DecodeReal(Strm :IN OUT CharStream; elementTag:IN XString; value: OUT Asn1Real; Result :OUT ASN1_RESULT) IS
      str:XString(1..100):=(1..100=> ' ');
      len:Integer;
   BEGIN
      value:=0.0;
      Xer_DecodePrimitiveElement(Strm, elementTag, str,  len, Result);
      IF NOT Result.Success THEN RETURN; END IF;
      value:=Asn1Real'value(str);
      Result := (Success => TRUE, ErrorCode=>0);
   END;

   PROCEDURE Xer_DecodeString(Strm :IN OUT CharStream; elementTag:IN XString; value: OUT XString; Result :OUT ASN1_RESULT) IS
      len:Integer;
   BEGIN
      FOR I IN value'RANGE LOOP
         value(i) := Character'Val(0);
      END LOOP;
      Xer_DecodePrimitiveElement(Strm, elementTag, value,  len, Result);
   END;


   FUNCTION CharToNibble(c:Character) RETURN Unsigned_8 IS
   BEGIN
	if c>='0' AND c<='9' THEN
		return Unsigned_8(Character'Pos(c)-Character'Pos('0'));
	elsif c>='A' AND c<='F' THEN
		return Unsigned_8(Character'Pos(c)-Character'Pos('A') + 10);
	elsif c>='a' AND c<='f' THEN
   		return Unsigned_8(Character'Pos(c)-Character'Pos('a') + 10);
	else
           raise program_error;
        END IF;
   END;


   PROCEDURE Xer_DecodeOctetString(Strm :IN OUT CharStream; elementTag:IN XString;
                                   value: OUT OctetBuffer; len: OUT Integer; Result :OUT ASN1_RESULT) IS
      str:XString(1..32768):=(1..32768=> ' ');
      len2:INTEGER;
      J:INTEGER:=1;
      idx:Integer;
   BEGIN
      len:=0;
      Xer_DecodePrimitiveElement(Strm, elementTag, str,  len2, Result);
      IF NOT Result.Success THEN RETURN; END IF;
      FOR I IN 1..len2 LOOP
         IF not Is_Space(str(I)) THEN
            str(J) := str(I);
            J := J + 1;
         END IF;
      END LOOP;

      len2:= J-1;

      idx := value'First;
      FOR I IN 1..len2 LOOP
         IF I MOD 2 = 1 THEN
            value(idx) := Shift_left(CharToNibble(str(I)),4);
         ELSE
            value(idx) := value(idx) OR CharToNibble(str(I));
            idx := idx + 1;
         END IF;
      END LOOP;

      len:= len2/2 + len2 MOD 2;


      Result := (Success => TRUE, ErrorCode=>0);
   END;

   PROCEDURE Xer_DecodeBitString(Strm :IN OUT CharStream; elementTag:IN XString;
                                 value: OUT BitArray; len: OUT Integer; Result :OUT ASN1_RESULT) IS
      str:XString(1..32768):=(1..32768=> ' ');
      J:INTEGER:=1;
   BEGIN
      len:=0;
      Xer_DecodePrimitiveElement(Strm, elementTag, str,  len, Result);
      IF NOT Result.Success THEN RETURN; END IF;

      FOR I IN 1..len LOOP
         IF not Is_Space(str(I))  THEN
            str(J) := str(I);
            J := J + 1;
         END IF;
      END LOOP;

      len := J-1;

      FOR I IN 1..len LOOP
         IF str(I) = '0' THEN
            value(value'First-1+I) := 0;
         ELSE
            value(value'First-1+I) := 1;
         END IF;
      END LOOP;
   END;




   PROCEDURE Xer_EncodeComplexElementStart(Strm :IN OUT CharStream; elementTag:IN XString;  Result :OUT ASN1_RESULT; level: IN Integer) IS
   BEGIN
      BS_PutSpace(Strm, level, Result);
      IF NOT Result.Success THEN RETURN; END IF;

      BS_Append_String(Strm,"<"&elementTag&">",Result);
      IF NOT Result.Success THEN RETURN; END IF;
      BS_PutNL(Strm, Result);

   END;

   PROCEDURE Xer_EncodeComplexElementEnd(Strm :IN OUT CharStream; elementTag:IN XString; Result :OUT ASN1_RESULT; level: IN Integer) IS
   BEGIN
      BS_PutSpace(Strm, level, Result);
      IF NOT Result.Success THEN RETURN; END IF;

      BS_Append_String(Strm,"</"&elementTag&">",Result);
      IF NOT Result.Success THEN RETURN; END IF;
      BS_PutNL(Strm, Result);

   END;



   FUNCTION Xer_NextEndElementIs(Strm :IN CharStream; elementTag:IN XString) return Asn1Boolean IS
      I:INTEGER:=Strm.CurrentByte;
      J:INTEGER:=elementTag'First;
   BEGIN
      WHILE Is_Space(Strm.Data(I)) LOOP   I := I + 1;  END LOOP;

      IF Strm.Data(I) /= '<' THEN RETURN FALSE; END IF;
      IF Strm.Data(I+1) /= '/' THEN RETURN FALSE; END IF;

      I:=I+2;
      WHILE strm.Data(I) /= '>' LOOP
         IF J>elementTag'Last THEN RETURN FALSE; END IF;
         IF I>Strm.Data'Last  THEN RETURN FALSE; END IF;
         IF elementTag(J) /= strm.Data(I) THEN RETURN FALSE; END IF;
         I := I + 1;
         J := J + 1;
      END LOOP;

      return TRUE;
   END;

   FUNCTION Xer_NextStartElementIs(Strm :IN CharStream; elementTag:IN XString) return Asn1Boolean IS
      I:INTEGER:=Strm.CurrentByte;
      J:INTEGER:=elementTag'First;
   BEGIN
      WHILE Is_Space(Strm.Data(I)) LOOP   I := I + 1;  END LOOP;

      IF Strm.Data(I) /= '<' THEN RETURN FALSE; END IF;

      I:=I+1;
      WHILE strm.Data(I) /= '>' LOOP
         IF J>elementTag'Last THEN RETURN FALSE; END IF;
         IF I>Strm.Data'Last  THEN RETURN FALSE; END IF;
         IF elementTag(J) /= strm.Data(I) THEN RETURN FALSE; END IF;
         I := I + 1;
         J := J + 1;
      END LOOP;

      return TRUE;
   END;

   PROCEDURE Xer_LA_NextElementTag(Strm :IN CharStream; elementTag:OUT XString; Result :OUT ASN1_RESULT) IS
      I:INTEGER:=Strm.CurrentByte;
      J:INTEGER:=elementTag'First;
   BEGIN
      Result := (Success => FALSE, ErrorCode=>ERR_INCORRECT_STREAM);

      WHILE Is_Space(Strm.Data(I)) LOOP   I := I + 1;  END LOOP;

      IF Strm.Data(I) /= '<' THEN RETURN; END IF;

      I:=I+1;
      WHILE strm.Data(I) /= '/' AND strm.Data(I) /= '>'LOOP
         IF J>elementTag'Last THEN RETURN; END IF;
         IF I>Strm.Data'Last  THEN RETURN; END IF;
         elementTag(J) := strm.Data(I);
         I := I + 1;
         J := J + 1;
      END LOOP;

      IF strm.Data(I) = '/' THEN
         I := I + 1;
         IF I>Strm.Data'Last  THEN RETURN; END IF;
         IF strm.Data(I) /= '>' THEN  RETURN; END IF;
      END IF;

      Result := (Success => TRUE, ErrorCode=>0);
   END;





-- The following functions are used to load an XML file (by removing white spaces) into an ByteStream

package Ran_IO is new Ada.Direct_IO(Character);


   TYPE STATE_ID is (XmlStart, XmlHeader, XmlStartTag, XmlContent, XmlEndTag, XmlMixedContent, XmlComment);


   PreviousState:STATE_ID:=XmlStart;

   TYPE FunType IS  access PROCEDURE (c: in Character; l1: in Character;
                        Strm : IN OUT CharStream; tmpStrm : IN OUT CharStream; I : IN OUT Ran_IO.Count;
                        state: OUT STATE_ID; success: OUT Boolean);


   PROCEDURE ByteStream_AppendChar(Strm :IN OUT CharStream; c: in Character; success: OUT Boolean) IS
      Result:ASN1_RESULT;
   BEGIN
      BS_Append_String(Strm, ""&c, Result);
      success := Result.Success;
   END ByteStream_AppendChar;


   PROCEDURE OnXmlStart(c: in Character; l1: in Character;
                        Strm : IN OUT CharStream; tmpStrm : IN OUT CharStream; I : IN OUT Ran_IO.Count;
                        state: OUT STATE_ID; success: OUT Boolean) IS
   BEGIN
      success := TRUE;

      IF c/='<' THEN
	 state := XmlStart;
         return;
      END IF;


      IF l1 = '!' THEN
         state := XmlComment;
	 PreviousState := XmlStart;
         return;
      END IF;

      IF l1 = '?' THEN
         state:= XmlHeader;
         return;
      END IF;

      state := XmlStartTag;
      ByteStream_AppendChar(Strm, c, success);

   END;

   PROCEDURE OnXmlHeader(c: in Character; l1: in Character;
                        Strm : IN OUT CharStream; tmpStrm : IN OUT CharStream; I : IN OUT Ran_IO.Count;
                        state: OUT STATE_ID; success: OUT Boolean) IS
      use Ran_IO;
   BEGIN
      success := TRUE;
      IF c='?' AND l1='>' THEN
         I := I + 1;
         state := XmlStart;
      ELSE
         state := XmlHeader;
      END IF;

   END;

   PROCEDURE OnXmlStartTag(c: in Character; l1: in Character;
                        Strm : IN OUT CharStream; tmpStrm : IN OUT CharStream; I : IN OUT Ran_IO.Count;
                        state: OUT STATE_ID; success: OUT Boolean) IS
   BEGIN
      IF c='>' THEN
         state := XmlContent;
      ELSE
         state := XmlStartTag;
      END IF;

      ByteStream_AppendChar(Strm, c, success);
   END;

   PROCEDURE OnXmlContent(c: in Character; l1: in Character;
                        Strm : IN OUT CharStream; tmpStrm : IN OUT CharStream; I : IN OUT Ran_IO.Count;
                        state: OUT STATE_ID; success: OUT Boolean) IS
   BEGIN
      success := TRUE;
      IF c /= '<' THEN
         state := XmlContent;
         ByteStream_AppendChar(tmpStrm, c, success);
      ELSE
         IF l1 = '!' THEN
            state := XmlComment;
	    PreviousState := XmlContent;
	    return;
         END IF;

         IF l1 = '/' THEN
            state := XmlEndTag;
            --copy data from tmp buf to main steam
            FOR J in 1..tmpStrm.CurrentByte-1 LOOP
               ByteStream_AppendChar(Strm, tmpStrm.Data(J), success);
               IF NOT success THEN RETURN; END IF;
            END LOOP;

            --Discard tmp buf
            tmpStrm.CurrentByte :=1;

         ELSE
            state := XmlStartTag;
	    -- Discard tmp buf
	    tmpStrm.CurrentByte := 1;
         END IF;

         ByteStream_AppendChar(Strm, c, success);
      END IF;
   END;

   PROCEDURE OnXmlEndTag(c: in Character; l1: in Character;
                        Strm : IN OUT CharStream; tmpStrm : IN OUT CharStream; I : IN OUT Ran_IO.Count;
                        state: OUT STATE_ID; success: OUT Boolean) IS
   BEGIN
      IF c = '>' THEN
         state := XmlMixedContent;
      ELSE
         state := XmlEndTag;
      END IF;

      ByteStream_AppendChar(Strm, c, success);

   END;

   PROCEDURE OnXmlMixedContent(c: in Character; l1: in Character;
                        Strm : IN OUT CharStream; tmpStrm : IN OUT CharStream; I : IN OUT Ran_IO.Count;
                        state: OUT STATE_ID; success: OUT Boolean) IS
   BEGIN
      success := True;
      IF c /= '<' THEN
         state := XmlMixedContent;
      ELSE
         IF l1 = '!' THEN
            state := XmlComment;
	    PreviousState := XmlMixedContent;
	    return;
         END IF;

         IF l1 = '/' THEN
            state := XmlEndTag;
         ELSE
            state := XmlStartTag;
         END IF;

         ByteStream_AppendChar(Strm, c, success);
      END IF;
   END;

   PROCEDURE OnXmlComment(c: in Character; l1: in Character;
                        Strm : IN OUT CharStream; tmpStrm : IN OUT CharStream; I : IN OUT Ran_IO.Count;
                        state: OUT STATE_ID; success: OUT Boolean) IS
      use Ran_IO;
   BEGIN
      success := True;
      IF c = '-' AND l1 = '>' THEN
         I := I + 1;
         state := PreviousState;
      ELSE
         state := XmlComment;
      END IF;

   END;



   PROCEDURE LoadXmlFile(fileName:IN XString; Strm : OUT CharStream; BytesLoaded : OUT INTEGER; success: OUT Boolean) IS
      use Ran_IO;

      file : Ran_IO.FILE_TYPE;
      c: Character;	-- current character
      l1: Character;	-- next character (look ahead 1)
      I: Ran_IO.Count:=0;
      StateMachine: array(STATE_ID) of FunType :=(OnXmlStart'Access, OnXmlHeader'Access, OnXmlStartTag'Access,
                                                            OnXmlContent'Access, OnXmlEndTag'Access, OnXmlMixedContent'Access,
                                                           OnXmlComment'Access);
      curFunction : FunType;
      state:STATE_ID:=XmlStart;
      tmpStrm : CharStream(16384);

      eofReached : Boolean := False;

   BEGIN
      success:= TRUE;
      BytesLoaded := 0;
      Ran_IO.Open(file, Ran_IO.In_File, fileName);

      WHILE not Ran_IO.End_Of_File(file) LOOP
         Ran_IO.Read(file, c);
         IF Ran_IO.End_Of_File(file) THEN
            l1 := c;
	    eofReached := True;
         ELSE
            I := Ran_IO.Index(file);
            Ran_IO.Read(file, l1); -- read look ahead character
            Ran_IO.Set_Index(file, I);
         END IF;

         curFunction := StateMachine(state);
         curFunction(c,l1, Strm, tmpStrm, I, state, success);
      	 IF NOT success then
      	    Ran_IO.Close(file);
            RETURN;
         ELSE
            IF NOT eofReached THEN
               Ran_IO.Set_Index(file, I);
            END IF;
         END IF;

      END LOOP;

      BytesLoaded := Strm.CurrentByte-1;
      Strm.CurrentByte := 1;
      Ran_IO.Close(file);
   END;




















end adaasn1rtl;
