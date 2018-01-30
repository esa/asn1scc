with Interfaces;
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

end adaasn1rtl;
