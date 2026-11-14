package body adaasn1rtl.encoding.uper with
   Spark_Mode
is

   procedure UPER_Enc_Boolean (bs : in out Bitstream; Val : Asn1Boolean) is
   begin
      BitStream_AppendBit (bs, (if Val then 1 else 0));
   end UPER_Enc_Boolean;

   procedure UPER_Dec_boolean
     (bs : in out Bitstream; val : out Asn1Boolean; result : out Boolean)
   is
      v : BIT;
   begin
      BitStream_ReadBit (bs, v, result);
      val := v = 1;
   end UPER_Dec_boolean;

   procedure UPER_Enc_SemiConstraintWholeNumber
     (bs : in out Bitstream; IntVal : Asn1Int; MinVal : Asn1Int)
   is
   begin
      Enc_SemiConstraintWholeNumber (bs, IntVal, MinVal);
   end UPER_Enc_SemiConstraintWholeNumber;

   procedure UPER_Dec_SemiConstraintWholeNumber
     (bs     : in out Bitstream; IntVal : out Asn1Int; MinVal : Asn1Int;
      Result :    out Boolean)
   is
   begin
      Dec_SemiConstraintWholeNumber (bs, IntVal, MinVal, Result);
   end UPER_Dec_SemiConstraintWholeNumber;

   procedure UPER_Enc_SemiConstraintPosWholeNumber
     (bs : in out Bitstream; IntVal : Asn1UInt; MinVal : Asn1UInt)
   is
   begin
      Enc_SemiConstraintPosWholeNumber (bs, IntVal, MinVal);
   end UPER_Enc_SemiConstraintPosWholeNumber;

   procedure UPER_Dec_SemiConstraintPosWholeNumber
     (bs     : in out Bitstream; IntVal : out Asn1UInt; MinVal : Asn1UInt;
      Result :    out Boolean)
   is
   begin
      Dec_SemiConstraintPosWholeNumber (bs, IntVal, MinVal, Result);

   end UPER_Dec_SemiConstraintPosWholeNumber;

   procedure UPER_Enc_ConstraintWholeNumber
     (bs    : in out Bitstream; IntVal : Asn1Int; MinVal : Asn1Int;
      nBits :        Integer)
   is
   begin
      Enc_ConstraintWholeNumber (bs, IntVal, MinVal, nBits);
   end UPER_Enc_ConstraintWholeNumber;

   procedure UPER_Enc_ConstraintPosWholeNumber
     (bs    : in out Bitstream; IntVal : Asn1UInt; MinVal : Asn1UInt;
      nBits :        Integer)
   is
   begin
      Enc_ConstraintPosWholeNumber (bs, IntVal, MinVal, nBits);
   end UPER_Enc_ConstraintPosWholeNumber;

   procedure UPER_Dec_ConstraintWholeNumber
     (bs     : in out Bitstream; IntVal : out Asn1Int; MinVal : Asn1Int;
      MaxVal :        Asn1Int; nBits : Integer; Result : out Boolean)
   is
   begin
      Dec_ConstraintWholeNumber (bs, IntVal, MinVal, MaxVal, nBits, Result);
   end UPER_Dec_ConstraintWholeNumber;

   procedure UPER_Dec_ConstraintWholeNumberInt8
     (bs     : in out Bitstream; IntVal : out Integer_8; MinVal : Integer_8;
      MaxVal :        Integer_8; nBits : Integer; Result : out Boolean)
   is
      v : Asn1Int;
   begin
      UPER_Dec_ConstraintWholeNumber (bs, v, Asn1Int (MinVal),
             Asn1Int (MaxVal), nBits, Result);

      IntVal := Integer_8 (v);
   end UPER_Dec_ConstraintWholeNumberInt8;

   procedure UPER_Dec_ConstraintWholeNumberInt16
     (bs     : in out Bitstream; IntVal : out Integer_16; MinVal : Integer_16;
      MaxVal :        Integer_16; nBits : Integer; Result : out Boolean)
   is
      v : Asn1Int;
   begin
      UPER_Dec_ConstraintWholeNumber (bs, v, Asn1Int (MinVal),
             Asn1Int (MaxVal), nBits, Result);

      IntVal := Integer_16 (v);
   end UPER_Dec_ConstraintWholeNumberInt16;

   procedure UPER_Dec_ConstraintWholeNumberInt32
     (bs     : in out Bitstream; IntVal : out Integer_32; MinVal : Integer_32;
      MaxVal :        Integer_32; nBits : Integer; Result : out Boolean)
   is
      v : Asn1Int;
   begin
      UPER_Dec_ConstraintWholeNumber (bs, v, Asn1Int (MinVal),
             Asn1Int (MaxVal), nBits, Result);

      IntVal := Integer_32 (v);
   end UPER_Dec_ConstraintWholeNumberInt32;

   procedure UPER_Dec_ConstraintPosWholeNumberUInt8
     (bs     : in out Bitstream; IntVal : out Unsigned_8; MinVal : Unsigned_8;
      MaxVal :        Unsigned_8; nBits : Integer; Result : out Boolean)
   is
      v : Asn1UInt;
   begin
      UPER_Dec_ConstraintPosWholeNumber (bs, v, Asn1UInt (MinVal),
             Asn1UInt (MaxVal), nBits, Result);

      IntVal := Unsigned_8 (v);
   end UPER_Dec_ConstraintPosWholeNumberUInt8;

   procedure UPER_Dec_ConstraintPosWholeNumberUInt16
     (bs : in out Bitstream; IntVal : out Unsigned_16; MinVal : Unsigned_16;
      MaxVal :        Unsigned_16; nBits : Integer; Result : out Boolean)
   is
      v : Asn1UInt;
   begin
      UPER_Dec_ConstraintPosWholeNumber (bs, v, Asn1UInt (MinVal),
             Asn1UInt (MaxVal), nBits, Result);

      IntVal := Unsigned_16 (v);
   end UPER_Dec_ConstraintPosWholeNumberUInt16;

   procedure UPER_Dec_ConstraintPosWholeNumberUInt32
     (bs : in out Bitstream; IntVal : out Unsigned_32; MinVal : Unsigned_32;
      MaxVal :        Unsigned_32; nBits : Integer; Result : out Boolean)
   is
      v : Asn1UInt;
   begin
      UPER_Dec_ConstraintPosWholeNumber (bs, v, Asn1UInt (MinVal),
             Asn1UInt (MaxVal), nBits, Result);

      IntVal := Unsigned_32 (v);
   end UPER_Dec_ConstraintPosWholeNumberUInt32;

   procedure UPER_Dec_ConstraintPosWholeNumber
     (bs : in out Bitstream; IntVal : out Asn1UInt; MinVal : Asn1UInt;
      MaxVal :        Asn1UInt; nBits : Integer; Result : out Boolean)
   is
   begin
      Dec_ConstraintPosWholeNumber (bs, IntVal, MinVal, MaxVal, nBits, Result);

   end UPER_Dec_ConstraintPosWholeNumber;

   procedure UPER_Dec_ConstraintWholeNumberInt
     (bs     : in out Bitstream; IntVal : out Integer; MinVal : Integer;
      MaxVal :        Integer; nBits : Integer; Result : out Boolean)
   is
   begin
      Dec_ConstraintWholeNumberInt (bs, IntVal, MinVal, MaxVal, nBits, Result);
   end UPER_Dec_ConstraintWholeNumberInt;

   procedure UPER_Enc_UnConstraintWholeNumber
     (bs : in out Bitstream; IntVal : Asn1Int)
   is
   begin
      Enc_UnConstraintWholeNumber (bs, IntVal);
   end UPER_Enc_UnConstraintWholeNumber;

   procedure UPER_Dec_UnConstraintWholeNumber
     (bs : in out Bitstream; IntVal : out Asn1Int; Result : out Boolean)
   is
   begin
      Dec_UnConstraintWholeNumber (bs, IntVal, Result);
   end UPER_Dec_UnConstraintWholeNumber;

   procedure UPER_Dec_UnConstraintWholeNumberMax
     (bs     : in out Bitstream; IntVal : out Asn1Int; MaxVal : Asn1Int;
      Result :    out Boolean)
   is
   begin
      UPER_Dec_UnConstraintWholeNumber (bs, IntVal, Result);
      Result := Result and IntVal <= MaxVal;
      if not Result then
         IntVal := MaxVal;
      end if;
   end UPER_Dec_UnConstraintWholeNumberMax;

   procedure UPER_Enc_Real (bs : in out Bitstream; RealVal : Asn1Real) is
      Header   : Interfaces.Unsigned_8 := 16#80#;
      NExpLen  : Asn1Byte;
      NManLen  : Asn1Byte;
      Exp      : Asn1Int;
      Mantissa : Asn1UInt;
      V        : Asn1Real;
   begin

      if isPosZero (RealVal) then
         BitStream_AppendByte (bs, 0, False);
      elsif isNegZero (RealVal) then
         BitStream_AppendByte (bs, 1, False);
         BitStream_AppendByte (bs, 16#43#, False);
      elsif RealVal = PLUS_INFINITY then
         BitStream_AppendByte (bs, 1, False);
         BitStream_AppendByte (bs, 16#40#, False);
      elsif RealVal = MINUS_INFINITY then
         BitStream_AppendByte (bs, 1, False);
         BitStream_AppendByte (bs, 16#41#, False);
      elsif isNaN (RealVal) then
         BitStream_AppendByte (bs, 1, False);
         BitStream_AppendByte (bs, 16#42#, False);
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

         --  encode length
         BitStream_AppendByte (bs, (1 + NExpLen) + NManLen, False); --  1

         --  encode header
         BitStream_AppendByte (bs, Header, False); --  1

         --  encode exponent
         Enc_UInt (bs, To_UInt (Exp), Integer (NExpLen)); --  max 3 octets

         --  encode mantissa
         Enc_UInt (bs, Mantissa, Integer (NManLen)); --  max 8 octets
      end if;
   end UPER_Enc_Real;

   function CalcReal
     (Factor : Asn1UInt; N : Asn1UInt; base : Integer; Exp : Integer)
      return Asn1Real
   is
      pragma SPARK_Mode (Off);
   begin
      return (Asn1Real (Factor * N) * Asn1Real (base)**Exp);
   end CalcReal;

   procedure UPER_Dec_Real_AsBinary_aux
     (bs      : in out Bitstream; ExpLen : Asn1Byte; Length : Asn1Byte;
      Factor  :        Asn1UInt; Sign : Integer; Base : Integer;
      RealVal :    out Asn1Real; Result : out ASN1_RESULT) with
      Pre => (Base = 2 or Base = 8 or Base = 16)
      and then (Factor = 1 or Factor = 2 or Factor = 4 or Factor = 8)
      and then ExpLen <= 4 and then Length >= 0 and then Length <= 11
      and then (Sign = 1 or Sign = -1)
      and then bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 24)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 24),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 24);

   procedure UPER_Dec_Real_AsBinary_aux
     (bs      : in out Bitstream; ExpLen : Asn1Byte; Length : Asn1Byte;
      Factor  :        Asn1UInt; Sign : Integer; Base : Integer;
      RealVal :    out Asn1Real; Result : out ASN1_RESULT)
   is
      Exp : Asn1Int;
      N   : Asn1UInt;
   begin
      RealVal := 0.0;
      Result := ASN1_RESULT'(Success => False, ErrorCode => ERR_END_OF_STREAM);
      if ExpLen < Length and ExpLen <= 3 then
         Dec_Int (bs, Integer (ExpLen), Exp, Result.Success);

         if Result.Success and Length - ExpLen <= Asn1UInt'Size / 8 then
            Dec_UInt (bs, Integer (Length - ExpLen), N, Result.Success);
            if Result.Success and Exp > Asn1Int (Integer'First) and
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
     (bs      : in out Bitstream; Header : Asn1Byte; EncLength : Asn1Byte;
      RealVal :    out Asn1Real; Result : out ASN1_RESULT) with
      Depends => ((bs, RealVal, Result) => (bs, Header, EncLength)),
      Pre     => EncLength <= 11
      and then bs.Current_Bit_Pos < (Natural'Last - (Asn1UInt'Size + 24))
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 24),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 24);

   procedure UPER_Dec_Real_AsBinary
     (bs      : in out Bitstream; Header : Asn1Byte; EncLength : Asn1Byte;
      RealVal :    out Asn1Real; Result : out ASN1_RESULT)
   is
      Sign   : Integer  := 1;
      Base   : Integer  := 2;
      F      : Asn1Byte;
      Factor : Asn1UInt := 1;
      ExpLen : Asn1Byte;
   begin

      if (Header and 16#40#) > 0 then
         Sign := -1;
      end if;

      if (Header and 16#10#) > 0 then
         Base := 8;
      elsif (Header and 16#20#) > 0 then
         Base := 16;
      end if;

      F := (Header and 16#0C#) / 4;
      pragma Assert (F = 0 or F = 1 or F = 2 or F = 3);

      Factor := Factor * (2**Integer (F));
      pragma Assert (Factor = 1 or Factor = 2 or Factor = 4 or Factor = 8);

      ExpLen := (Header and 16#03#) + 1;
      pragma Assert (ExpLen <= 4);

      UPER_Dec_Real_AsBinary_aux
        (bs, ExpLen, EncLength, Factor, Sign, Base, RealVal, Result);

   end UPER_Dec_Real_AsBinary;

   procedure UPER_Dec_Real
     (bs : in out Bitstream; RealVal : out Asn1Real; Result : out ASN1_RESULT)
   is
      Header : Asn1Byte;
      Length : Asn1Byte;
   begin
      RealVal := 0.0;
      Result := ASN1_RESULT'(Success => False, ErrorCode => ERR_END_OF_STREAM);

      BitStream_DecodeByte (bs, Length, Result.Success);
      if Result.Success and Length <= 12 then
         if Length > 0 then
            BitStream_DecodeByte (bs, Header, Result.Success);
            if Result.Success then
               if Header = 16#40# then
                  RealVal := PLUS_INFINITY;
                  Result  := ASN1_RESULT'(Success => True, ErrorCode => 0);
               elsif Header = 16#41# then
                  RealVal := MINUS_INFINITY;
                  Result  := ASN1_RESULT'(Success => True, ErrorCode => 0);
               elsif Header = 16#42# then
                  RealVal := NaN;
                  Result  := ASN1_RESULT'(Success => True, ErrorCode => 0);
               elsif Header = 16#43# then
                  RealVal := -0.0;
                  Result  := ASN1_RESULT'(Success => True, ErrorCode => 0);
               elsif (Header and 16#80#) > 0 then
                  UPER_Dec_Real_AsBinary
                    (bs, Header, Length - 1, RealVal, Result);
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

   procedure UPER_Dec_Real_fp32
     (bs : in out Bitstream; RealVal : out Standard.Float;
          Result : out ASN1_RESULT)
   is
      v : Asn1Real;
   begin
      UPER_Dec_Real (bs, v, Result);

      pragma Warnings
        (Off, "condition can only be False if invalid values present");
      pragma Warnings
        (Off, "condition can only be True if invalid values present");
      pragma Warnings
        (Off, "condition is always True");
      pragma Warnings
        (Off, "condition is always False");

      --  a value outside the Float range (or a NaN) is an incorrect stream
      if Result.Success and then
        not (Asn1Real (Standard.Float'First) <= v and then
             v <= Asn1Real (Standard.Float'Last))
      then
         Result := ASN1_RESULT'(Success => False,
                                ErrorCode => ERR_INCORRECT_STREAM);
      end if;

      pragma Warnings
        (On, "condition can only be False if invalid values present");
      pragma Warnings
        (On, "condition can only be True if invalid values present");
      pragma Warnings
        (On, "condition is always True");
      pragma Warnings
        (On, "condition is always False");

      if Result.Success then
         RealVal := Standard.Float (v);
      else
         RealVal := 0.0;
      end if;

   end UPER_Dec_Real_fp32;

   procedure ObjectIdentifier_uper_decode_length
     (bs     : in out Bitstream; length : out Integer;
      result :    out ASN1_RESULT) with
      Depends => ((bs, length, result) => (bs)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 16
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 16;

   procedure ObjectIdentifier_uper_decode_length
     (bs : in out Bitstream; length : out Integer; result : out ASN1_RESULT)
   is
      len2 : Integer;
   begin
      result := ASN1_RESULT'(ErrorCode => 0, Success => True);
      UPER_Dec_ConstraintWholeNumberInt
        (bs, length, 0, 255, 8, result.Success);
      if result.Success then
         if length > 16#80# then
            length := (length - 16#80#) * 16#100#;
            UPER_Dec_ConstraintWholeNumberInt
              (bs, len2, 0, 255, 8, result.Success);
            if result.Success then
               length := length + len2;
            end if;
         end if;
      end if;
   end ObjectIdentifier_uper_decode_length;

--  Each subidentifier is represented as a series of (one or more) octets.
--  Bit 8 of each octet indicates whether it is the last in the series:
--  bit 8 of the last octet is zero; bit 8 of each preceding octet is one.
--  Bits 7 to 1 of the octets in the series collectively encode the
--  subidentifier. Conceptually, these groups of bits are concatenated to form
--  an unsigned binary number whose most significant bit is bit 7 of the first
--  octet and whose least significant bit is bit 1 of the last octet.
--  The subidentifier shall be encoded in the fewest possible octets, that is,
--  the leading octet of the subidentifier shall not have the value 8016.

   procedure ObjectIdentifier_subidentifiers_uper_encode
     (encodingBuf : in out OctetArray1K; curSize : in out Integer;
      siValue0    :        Asn1UInt) with
      Depends => (curSize => (curSize, siValue0),
       encodingBuf => (encodingBuf, curSize, siValue0)),
      Pre => curSize >= OctetArray1K'First - 1 and
      curSize < OctetArray1K'Last - OctetBuffer_16'Last,
      Post => curSize >= curSize'Old + 1 and
      curSize <= curSize'Old + OctetBuffer_16'Last and
      curSize <= OctetArray1K'Last;

   procedure ObjectIdentifier_subidentifiers_uper_encode
     (encodingBuf : in out OctetArray1K; curSize : in out Integer;
      siValue0    :        Asn1UInt)
   is
      lastOctet : Boolean        := False;
      tmp       : OctetBuffer_16 := OctetBuffer_16'(others => 0);
      nSize     : Integer        := 0;
      curByte   : Asn1Byte;
      siValue   : Asn1UInt       := siValue0;
   begin
      while not lastOctet and nSize < OctetBuffer_16'Last loop
         pragma Loop_Invariant (nSize >= 0);
         curByte                            := Asn1Byte (siValue mod 128);
         siValue                            := siValue / 128;
         lastOctet                          := (siValue = 0);
         tmp (OctetBuffer_16'First + nSize) := curByte;
         nSize                              := nSize + 1;
      end loop;

      pragma Assert (nSize >= 1);
      pragma Assert (nSize <= OctetBuffer_16'Last);

      for i in Integer range 1 .. nSize loop
         pragma Loop_Invariant
           (nSize <= OctetBuffer_16'Last and
            nSize - i + 1 >= OctetBuffer_16'First and
            nSize - i + 1 <= OctetBuffer_16'Last and
            curSize + i >= OctetArray1K'First and
            curSize + i < OctetArray1K'Last);
         curByte :=
           (if i = nSize then tmp (1) else tmp (nSize - i + 1) or 16#80#);
         encodingBuf (curSize + i) := curByte;
      end loop;
      curSize := curSize + nSize;

   end ObjectIdentifier_subidentifiers_uper_encode;

   procedure ObjectIdentifier_subidentifiers_uper_decode
     (bs      : in out Bitstream; remainingOctets : in out Integer;
      siValue :    out Asn1UInt; Result : out ASN1_RESULT) with
      Depends => ((remainingOctets, bs) => (remainingOctets, bs),
       (siValue, Result) => (remainingOctets, bs)),
      Pre => remainingOctets > 0
      and then bs.Current_Bit_Pos < Natural'Last - (8 * OctetBuffer_16'Last)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (8 * OctetBuffer_16'Last),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos + (8 * OctetBuffer_16'Last) and
      remainingOctets <= remainingOctets'Old and
      remainingOctets >= remainingOctets'Old - OctetBuffer_16'Last;

   procedure ObjectIdentifier_subidentifiers_uper_decode
     (bs      : in out Bitstream; remainingOctets : in out Integer;
      siValue :    out Asn1UInt; Result : out ASN1_RESULT)
   is
      curByte       : Asn1Byte;
      bLastOctet    : Boolean := False;
      curOctetValue : Asn1UInt;
      i             : Integer := 1;
   begin
      siValue := 0;
      Result  := ASN1_RESULT'(Success => True, ErrorCode => 0);

      while Result.Success and remainingOctets > 0 and not bLastOctet and
        bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8 and
        i <= OctetBuffer_16'Last
      loop
         pragma Loop_Invariant
           (i >= 1 and
         --  i <= OctetBuffer_16'Last and
         --  remainingOctets > 0 and

            bs.Current_Bit_Pos =
              bs.Current_Bit_Pos'Loop_Entry + (i - 1) * 8 and
            remainingOctets = remainingOctets'Loop_Entry - (i - 1));
         BitStream_DecodeByte (bs, curByte, Result.Success);

         remainingOctets := remainingOctets - 1;
         bLastOctet      := (curByte and 16#80#) = 0;
         curOctetValue   := Asn1UInt (curByte and 16#7F#);
         siValue         := Shift_Left (siValue, 7);

         siValue := siValue or curOctetValue;
         i       := i + 1;

      end loop;

   end ObjectIdentifier_subidentifiers_uper_decode;

   procedure ObjectIdentifier_uper_encode
     (bs : in out Bitstream; val : Asn1ObjectIdentifier)
   is
      tmp       : OctetArray1K := OctetArray1K'(others => 0);
      totalSize : Integer      := 0;
   begin
      ObjectIdentifier_subidentifiers_uper_encode
        (tmp, totalSize, val.values (1) * 40 + val.values (2));
      pragma Assert (totalSize >= 1 and totalSize <= OctetBuffer_16'Last);

      for i in Integer range 3 .. val.Length loop
         pragma Loop_Invariant
           ( --   val.Length <= OBJECT_IDENTIFIER_MAX_LENGTH and
         totalSize >= 1 and
            totalSize <= totalSize'Loop_Entry + (i - 3) * OctetBuffer_16'Last);
         ObjectIdentifier_subidentifiers_uper_encode
           (tmp, totalSize, val.values (i));
      end loop;

      pragma Assert
        (totalSize <= OctetBuffer_16'Last * OBJECT_IDENTIFIER_MAX_LENGTH);

      --  encode length determinant
      if totalSize <= 16#7F# then
         UPER_Enc_ConstraintWholeNumber (bs, Asn1Int (totalSize), 0, 8);
      else
         BitStream_AppendBit (bs, 1);
         UPER_Enc_ConstraintWholeNumber (bs, Asn1Int (totalSize), 0, 15);
      end if;

      --  encode contents
      for i in Integer range 1 .. totalSize loop
         pragma Loop_Invariant
           (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i - 1) * 8);
         BitStream_AppendByte (bs, tmp (i), False);
      end loop;

   end ObjectIdentifier_uper_encode;

   procedure ObjectIdentifier_uper_decode
     (bs     : in out Bitstream; val : out Asn1ObjectIdentifier;
      Result :    out ASN1_RESULT)
   is
      totalSize : Integer;
      si        : Asn1UInt;

   begin
      ObjectIdentifier_Init (val);

      ObjectIdentifier_uper_decode_length (bs, totalSize, Result);

      if Result.Success and totalSize > 0 then
         ObjectIdentifier_subidentifiers_uper_decode
           (bs, totalSize, si, Result);
         if Result.Success then
            val.Length     := 2;
            --  X.690 8.19.4: the first subidentifier is 40 * arc1 + arc2,
            --  with arc1 = 2 for every value from 80 (arc2 may exceed 39)
            val.values (1) := (if si < 80 then si / 40 else 2);
            val.values (2) := si - val.values (1) * 40;

            while Result.Success and totalSize > 0 and
              val.Length < OBJECT_IDENTIFIER_MAX_LENGTH
            loop
               pragma Loop_Invariant
                 (bs.Current_Bit_Pos >= bs.Current_Bit_Pos'Loop_Entry and
                  bs.Current_Bit_Pos <=
                    bs.Current_Bit_Pos'Loop_Entry +
                      (val.Length - val.Length'Loop_Entry) * 8 *
                        OctetBuffer_16'Last);
               ObjectIdentifier_subidentifiers_uper_decode
                 (bs, totalSize, si, Result);
               val.Length              := val.Length + 1;
               val.values (val.Length) := si;
            end loop;
            Result.Success := Result.Success and totalSize = 0;

         end if;
      end if;
   end ObjectIdentifier_uper_decode;

   procedure RelativeOID_uper_encode
     (bs : in out Bitstream; val : Asn1ObjectIdentifier)
   is
      tmp       : OctetArray1K := OctetArray1K'(others => 0);
      totalSize : Integer      := 0;
   begin
      for i in Integer range 1 .. val.Length loop
         pragma Loop_Invariant
           ( --   val.Length <= OBJECT_IDENTIFIER_MAX_LENGTH and
         totalSize >= 0 and
            totalSize <= totalSize'Loop_Entry + (i - 1) * OctetBuffer_16'Last);
         ObjectIdentifier_subidentifiers_uper_encode
           (tmp, totalSize, val.values (i));
      end loop;
      pragma Assert
        (totalSize <= OctetBuffer_16'Last * OBJECT_IDENTIFIER_MAX_LENGTH);

      --  encode length determinant
      if totalSize <= 16#7F# then
         UPER_Enc_ConstraintWholeNumber (bs, Asn1Int (totalSize), 0, 8);
      else
         BitStream_AppendBit (bs, 1);
         UPER_Enc_ConstraintWholeNumber (bs, Asn1Int (totalSize), 0, 15);
      end if;

      --  encode contents
      for i in Integer range 1 .. totalSize loop
         pragma Loop_Invariant
           (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i - 1) * 8);
         BitStream_AppendByte (bs, tmp (i), False);
      end loop;

   end RelativeOID_uper_encode;

   procedure RelativeOID_uper_decode
     (bs     : in out Bitstream; val : out Asn1ObjectIdentifier;
      Result :    out ASN1_RESULT)
   is
      totalSize : Integer;
      si        : Asn1UInt;
   begin
      ObjectIdentifier_Init (val);
      ObjectIdentifier_uper_decode_length (bs, totalSize, Result);
      if Result.Success then
         while Result.Success and totalSize > 0 and
           val.Length < OBJECT_IDENTIFIER_MAX_LENGTH
         loop
            pragma Loop_Invariant
              (bs.Current_Bit_Pos >= bs.Current_Bit_Pos'Loop_Entry and
               bs.Current_Bit_Pos <=
                 bs.Current_Bit_Pos'Loop_Entry +
                   (val.Length - val.Length'Loop_Entry) * 8 *
                     OctetBuffer_16'Last);
            ObjectIdentifier_subidentifiers_uper_decode
              (bs, totalSize, si, Result);
            val.Length              := val.Length + 1;
            val.values (val.Length) := si;
         end loop;
         Result.Success := Result.Success and totalSize = 0;
      end if;
   end RelativeOID_uper_decode;

   procedure BitStream_EncodeOctetString_no_length
     (bs : in out Bitstream; data : OctetBuffer; data_length : Integer)
   is
      Current_Bit : constant BIT_RANGE := bs.Current_Bit_Pos mod 8;
      Current_Byte : constant Integer :=
        bs.Buffer'First + bs.Current_Bit_Pos / 8;
   begin
      if Current_Bit > 0 then
         for i in data'First .. (data'First + data_length - 1) loop
            pragma Loop_Invariant
              (bs.Current_Bit_Pos =
                 bs.Current_Bit_Pos'Loop_Entry + (i - data'First) * 8);
            BitStream_AppendByte (bs, data (i), False);
         end loop;
      else
         bs.Buffer (Current_Byte .. Current_Byte + data_length - 1) :=
           data (data'First .. (data'First + data_length - 1));
         bs.Current_Bit_Pos := bs.Current_Bit_Pos + 8 * data_length;
      end if;
   end BitStream_EncodeOctetString_no_length;

   procedure BitStream_DecodeOctetString_no_length
     (bs : in out Bitstream; data : in out OctetBuffer; data_length : Integer;
      success :    out Boolean)
   is
      Current_Bit : constant BIT_RANGE := bs.Current_Bit_Pos mod 8;
      Current_Byte : constant Integer :=
        bs.Buffer'First + bs.Current_Bit_Pos / 8;
   begin
      success := True;
      if Current_Bit > 0 then
         for i in data'First .. (data'First + data_length - 1) loop
            pragma Loop_Invariant
              (bs.Current_Bit_Pos =
                 bs.Current_Bit_Pos'Loop_Entry + (i - data'First) * 8
               and success);
            BitStream_DecodeByte (bs, data (i), success);
            exit when not success;
         end loop;
      else
         --  The precondition keeps the slice inside the buffer; the test
         --  keeps a decoder that is called outside its contract (checks
         --  suppressed with -gnatp) from copying memory past the stream.
         success := Current_Byte + data_length - 1 <= bs.Buffer'Last;
         if success then
            data (data'First .. (data'First + data_length - 1)) :=
              bs.Buffer (Current_Byte .. Current_Byte + data_length - 1);
         end if;
         bs.Current_Bit_Pos := bs.Current_Bit_Pos + 8 * data_length;
      end if;
   end BitStream_DecodeOctetString_no_length;

   procedure BitStream_EncodeOctetString_fragmentation
     (bs : in out Bitstream; data : OctetBuffer; data_length : Integer);

   procedure BitStream_EncodeOctetString_fragmentation
     (bs : in out Bitstream; data : OctetBuffer; data_length : Integer) with
      SPARK_Mode => Off
   is
      i1                  : Integer;
      nBLJ1               : Integer;
      nRemainingItemsVar1 : Integer;
      nCurBlockSize1      : Integer;
      nCurOffset1         : Integer;

   begin
      nCurOffset1         := 1;
      nRemainingItemsVar1 := data_length;
      while nRemainingItemsVar1 >= 16#4000# loop
         if nRemainingItemsVar1 >= 16#10000# then
            nCurBlockSize1 := 16#10000#;
            UPER_Enc_ConstraintWholeNumber (bs, 16#C4#, 0, 8);
         elsif nRemainingItemsVar1 >= 16#C000# then
            nCurBlockSize1 := 16#C000#;
            UPER_Enc_ConstraintWholeNumber (bs, 16#C3#, 0, 8);
         elsif nRemainingItemsVar1 >= 16#8000# then
            nCurBlockSize1 := 16#8000#;
            UPER_Enc_ConstraintWholeNumber (bs, 16#C2#, 0, 8);
         else
            nCurBlockSize1 := 16#4000#;
            UPER_Enc_ConstraintWholeNumber (bs, 16#C1#, 0, 8);
         end if;

         nBLJ1 := 0;
         while nBLJ1 <= nCurBlockSize1 - 1 loop
            i1 := nCurOffset1 + nBLJ1;
            BitStream_AppendByte (bs, data (i1), False);
            nBLJ1 := nBLJ1 + 1;
         end loop;
         nCurOffset1         := nCurOffset1 + nCurBlockSize1;
         nRemainingItemsVar1 := nRemainingItemsVar1 - nCurBlockSize1;

      end loop;

      if nRemainingItemsVar1 <= 16#7F# then
         UPER_Enc_ConstraintWholeNumber
           (bs, adaasn1rtl.Asn1Int (nRemainingItemsVar1), 0, 8);
      else
         BitStream_AppendBit (bs, 1);
         uper.UPER_Enc_ConstraintWholeNumber
           (bs, adaasn1rtl.Asn1Int (nRemainingItemsVar1), 0, 15);
      end if;

      nBLJ1          := 0;
      nCurBlockSize1 := nRemainingItemsVar1;
      while nBLJ1 <= nCurBlockSize1 - 1 loop
         i1 := nCurOffset1 + nBLJ1;
         --  set3
         BitStream_AppendByte (bs, data (i1), False);
         nBLJ1 := nBLJ1 + 1;
      end loop;

   end BitStream_EncodeOctetString_fragmentation;

   procedure BitStream_EncodeOctetString
     (bs    : in out Bitstream; data : OctetBuffer; data_length : Integer;
      nBits :    Integer; asn1SizeMin : Integer; asn1SizeMax : Integer) with
      SPARK_Mode => Off
   is
   begin
      if asn1SizeMax < 65536 then
         if asn1SizeMax /= asn1SizeMin then
            UPER_Enc_ConstraintWholeNumber
              (bs, Asn1Int (data_length), Asn1Int (asn1SizeMin), nBits);
         end if;
         BitStream_EncodeOctetString_no_length (bs, data, data_length);
      else
         BitStream_EncodeOctetString_fragmentation (bs, data, data_length);
      end if;
   end BitStream_EncodeOctetString;

   --  X.691 10.9.3.6 - 10.9.3.8: the header of the next part of a general
   --  length: a fragment of 16K, 32K, 48K or 64K units (C1 .. C4), or the
   --  last part, of n < 128 units (one octet) or n < 16K units (two
   --  octets, 10xxxxxx). Other values (C0, C5 .. FF) are an error.
   procedure Decode_Length_Part
     (bs     : in out Bitstream; nUnits : out Integer; bLast : out Boolean;
      success :   out Boolean);

   procedure Decode_Length_Part
     (bs     : in out Bitstream; nUnits : out Integer; bLast : out Boolean;
      success :   out Boolean) with
      SPARK_Mode => Off
   is
      nHeader : Integer := 0;
      nLow    : Integer := 0;
   begin
      nUnits  := 0;
      bLast   := True;
      success := bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8;
      if success then
         UPER_Dec_ConstraintWholeNumberInt (bs, nHeader, 0, 255, 8, success);
      end if;
      if success then
         case nHeader is
            when 16#00# .. 16#7F# =>
               nUnits := nHeader;
            when 16#80# .. 16#BF# =>
               success := bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8;
               if success then
                  UPER_Dec_ConstraintWholeNumberInt
                    (bs, nLow, 0, 255, 8, success);
                  nUnits := (nHeader - 16#80#) * 16#100# + nLow;
               end if;
            when 16#C1# =>
               nUnits := 16#4000#;
               bLast  := False;
            when 16#C2# =>
               nUnits := 16#8000#;
               bLast  := False;
            when 16#C3# =>
               nUnits := 16#C000#;
               bLast  := False;
            when 16#C4# =>
               nUnits := 16#10000#;
               bLast  := False;
            when others =>
               success := False;
         end case;
      end if;
   end Decode_Length_Part;

   procedure BitStream_DecodeOctetString_fragmentation
     (bs          : in out Bitstream; data : in out OctetBuffer;
      data_length : out Integer; asn1SizeMin : Integer; asn1SizeMax : Integer;
      success     :    out Boolean);

   --  Every part is checked against asn1SizeMax, the size of data and the
   --  rest of the stream before it is read.
   procedure BitStream_DecodeOctetString_fragmentation
     (bs          : in out Bitstream; data : in out OctetBuffer;
      data_length : out Integer; asn1SizeMin : Integer; asn1SizeMax : Integer;
      success     :    out Boolean) with
      SPARK_Mode => Off
   is
      nTotal : Integer := 0;
      nPart  : Integer;
      bLast  : Boolean := False;
   begin
      data_length := asn1SizeMin;
      success     := True;
      while success and not bLast loop
         Decode_Length_Part (bs, nPart, bLast, success);
         if success then
            success := nPart <= asn1SizeMax - nTotal
              and then nPart <= data'Length - nTotal
              and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8 * nPart;
         end if;
         if success then
            for i in 0 .. nPart - 1 loop
               BitStream_DecodeByte
                 (bs, data (data'First + nTotal + i), success);
               exit when not success;
            end loop;
         end if;
         if success then
            nTotal := nTotal + nPart;
         end if;
      end loop;

      if success and nTotal >= asn1SizeMin and nTotal <= asn1SizeMax then
         data_length := nTotal;
      else
         success := False;
      end if;
   end BitStream_DecodeOctetString_fragmentation;

   procedure BitStream_DecodeOctetString
     (bs          : in out Bitstream; data : in out OctetBuffer;
      data_length :    out Integer; nBits : Integer; asn1SizeMin : Integer;
      asn1SizeMax :        Integer; success : out Boolean) with
      SPARK_Mode => Off
   is
   begin
      success     := True;
      data_length := asn1SizeMin;
      if asn1SizeMax < 65536 then
         if asn1SizeMax /= asn1SizeMin then
            UPER_Dec_ConstraintWholeNumberInt
              (bs, data_length, asn1SizeMin, asn1SizeMax, nBits, success);
         end if;
         if success then
            BitStream_DecodeOctetString_no_length
              (bs, data, data_length, success);
         end if;
      else
         BitStream_DecodeOctetString_fragmentation
           (bs, data, data_length, asn1SizeMin, asn1SizeMax, success);
      end if;

   end BitStream_DecodeOctetString;

   --  ---------------------------------------
   procedure BitStream_EncodeBitString_fragmentation
     (bs : in out Bitstream; data : OctetBuffer; data_length : Integer);

   procedure BitStream_EncodeBitString_fragmentation
     (bs : in out Bitstream; data : OctetBuffer; data_length : Integer) with
      SPARK_Mode => Off
   is
      nRemainingItemsVar1 : Integer;
      nCurBlockSize1      : Integer;
      nCurOffset1         : Integer;

   begin
      nCurOffset1         := 0;
      nRemainingItemsVar1 := data_length;
      while nRemainingItemsVar1 >= 16#4000# loop
         if nRemainingItemsVar1 >= 16#10000# then
            nCurBlockSize1 := 16#10000#;
            UPER_Enc_ConstraintWholeNumber (bs, 16#C4#, 0, 8);
         elsif nRemainingItemsVar1 >= 16#C000# then
            nCurBlockSize1 := 16#C000#;
            UPER_Enc_ConstraintWholeNumber (bs, 16#C3#, 0, 8);
         elsif nRemainingItemsVar1 >= 16#8000# then
            nCurBlockSize1 := 16#8000#;
            UPER_Enc_ConstraintWholeNumber (bs, 16#C2#, 0, 8);
         else
            nCurBlockSize1 := 16#4000#;
            UPER_Enc_ConstraintWholeNumber (bs, 16#C1#, 0, 8);
         end if;

         BitStream_AppendBits
           (bs,
            data
              (data'First + nCurOffset1 / 8 ..
                   data'First + (nCurOffset1 + nCurBlockSize1) / 8 - 1),
            nCurBlockSize1);

         nCurOffset1         := nCurOffset1 + nCurBlockSize1;
         nRemainingItemsVar1 := nRemainingItemsVar1 - nCurBlockSize1;

      end loop;

      if nRemainingItemsVar1 <= 16#7F# then
         UPER_Enc_ConstraintWholeNumber
           (bs, adaasn1rtl.Asn1Int (nRemainingItemsVar1), 0, 8);
      else
         BitStream_AppendBit (bs, 1);
         UPER_Enc_ConstraintWholeNumber
           (bs, adaasn1rtl.Asn1Int (nRemainingItemsVar1), 0, 15);
      end if;

      BitStream_AppendBits
        (bs, data (data'First + nCurOffset1 / 8 .. data'Last),
         nRemainingItemsVar1);

   end BitStream_EncodeBitString_fragmentation;

   procedure BitStream_EncodeBitString
     (bs    : in out Bitstream; data : OctetBuffer; data_length : Integer;
      nBits :    Integer; asn1SizeMin : Integer; asn1SizeMax : Integer) with
      SPARK_Mode => Off
   is
   begin
      if asn1SizeMax < 65536 then
         if asn1SizeMax /= asn1SizeMin then
            UPER_Enc_ConstraintWholeNumber
              (bs, Asn1Int (data_length), Asn1Int (asn1SizeMin), nBits);
         end if;
         BitStream_AppendBits (bs, data, data_length);
      else
         BitStream_EncodeBitString_fragmentation (bs, data, data_length);
      end if;
   end BitStream_EncodeBitString;

   procedure BitStream_DecodeBitString_fragmentation
     (bs          : in out Bitstream; data : in out OctetBuffer;
      data_length : out Integer; asn1SizeMin : Integer; asn1SizeMax : Integer;
      success     :    out Boolean);

   --  Every part is checked against asn1SizeMax, the size of data and the
   --  rest of the stream before it is read. The fragments are multiples of
   --  8 bits, so every part starts at the first bit of a byte of data.
   procedure BitStream_DecodeBitString_fragmentation
     (bs          : in out Bitstream; data : in out OctetBuffer;
      data_length : out Integer; asn1SizeMin : Integer; asn1SizeMax : Integer;
      success     :    out Boolean) with
      SPARK_Mode => Off
   is
      nTotal : Integer := 0;
      nPart  : Integer;
      bLast  : Boolean := False;
   begin
      data_length := asn1SizeMin;
      success     := True;
      while success and not bLast loop
         Decode_Length_Part (bs, nPart, bLast, success);
         if success then
            success := nPart <= asn1SizeMax - nTotal
              and then nPart <= data'Length * 8 - nTotal
              and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nPart;
         end if;
         if success and nPart > 0 then
            BitStream_ReadBits
              (bs,
               data
                 (data'First + nTotal / 8 ..
                      data'First + (nTotal + nPart + 7) / 8 - 1),
               nPart, success);
         end if;
         if success then
            nTotal := nTotal + nPart;
         end if;
      end loop;

      if success and nTotal >= asn1SizeMin and nTotal <= asn1SizeMax then
         data_length := nTotal;
      else
         success := False;
      end if;
   end BitStream_DecodeBitString_fragmentation;

--     procedure BitStream_ReadBits
--       (bs                 : in out Bitstream;
--        bitMaskAsByteArray : in out OctetBuffer;
--        bits_to_read       :        Natural;
--        success            :    out Boolean) with
--        Pre     => bitMaskAsByteArray'First >= 0
--        and then bitMaskAsByteArray'Last < Natural'Last / 8
--        and then bits_to_read >= (bitMaskAsByteArray'Length - 1) * 8
--        and then bits_to_read <= (bitMaskAsByteArray'Length) * 8
--        and then bs.Current_Bit_Pos < Natural'Last - bits_to_read
--        and then bs.Size_In_Bytes < Positive'Last / 8
--        and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - bits_to_read,
--        Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + bits_to_read;

   procedure BitStream_DecodeBitString
     (bs          : in out Bitstream; data : in out OctetBuffer;
      data_length :    out Integer; nBits : Integer; asn1SizeMin : Integer;
      asn1SizeMax :        Integer; success : out Boolean) with
      SPARK_Mode => Off
   is
   begin
      success     := True;
      data_length := asn1SizeMin;
      if asn1SizeMax < 65536 then
         if asn1SizeMax /= asn1SizeMin then
            UPER_Dec_ConstraintWholeNumberInt
              (bs, data_length, asn1SizeMin, asn1SizeMax, nBits, success);
         end if;
         if success then
            BitStream_ReadBits (bs, data, data_length, success);
         end if;
      else
         BitStream_DecodeBitString_fragmentation
           (bs, data, data_length, asn1SizeMin, asn1SizeMax, success);
      end if;

   end BitStream_DecodeBitString;

   --  OCTET / BIT STRING (CONTAINING X), Issue 94. Lengths of 16K units or
   --  more are written / read by the two procedures below, which are not
   --  proved (SPARK_Mode Off): their contract is the callers' budget of bits
   --  (nBudget = most units + Containing_Length_Max_Bits).

   --  Parts of 64K, 48K, 32K or 16K units (X.691 10.9.3.8: the largest
   --  first) while 16K or more remain, then the last part (one octet below
   --  128 units, else two). nUnitBits: 8 (octets) or 1 (bits).
   procedure Containing_Encode_Fragments
     (bs        : in out Bitstream; data : OctetBuffer; nCount : Natural;
      nUnitBits :        Positive; nBudget : Natural) with
      Pre => nCount >= 16#4000# and then data'First = 1
      and then (nUnitBits = 1 or else nUnitBits = 8)
      and then data'Length < Positive'Last / 16
      and then (if nUnitBits = 8 then nCount <= data'Length
                else nCount <= 8 * data'Length)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBudget,
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos
      and then bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + nBudget;

   procedure Containing_Encode_Fragments
     (bs        : in out Bitstream; data : OctetBuffer; nCount : Natural;
      nUnitBits :        Positive; nBudget : Natural) with
      SPARK_Mode => Off
   is
      pragma Unreferenced (nBudget);
      nOffset    : Natural := 0;
      nRemaining : Natural := nCount;
      nBlocks    : Natural;
      nPart      : Natural;
   begin
      while nRemaining >= 16#4000# loop
         nBlocks :=
           (if nRemaining >= 16#10000# then 4 else nRemaining / 16#4000#);
         nPart := nBlocks * 16#4000#;
         UPER_Enc_ConstraintPosWholeNumber
           (bs, 16#C0# + Asn1UInt (nBlocks), 0, 8);
         BitStream_AppendBits
           (bs,
            data (data'First + nOffset * nUnitBits / 8 ..
                    data'First + (nOffset + nPart) * nUnitBits / 8 - 1),
            nPart * nUnitBits);
         nOffset    := nOffset + nPart;
         nRemaining := nRemaining - nPart;
      end loop;
      if nRemaining < 128 then
         UPER_Enc_ConstraintPosWholeNumber (bs, Asn1UInt (nRemaining), 0, 8);
      else
         UPER_Enc_ConstraintPosWholeNumber
           (bs, 16#8000# + Asn1UInt (nRemaining), 0, 16);
      end if;
      BitStream_AppendBits
        (bs,
         data (data'First + nOffset * nUnitBits / 8 ..
                 data'First +
                 ((nOffset + nRemaining) * nUnitBits + 7) / 8 - 1),
         nRemaining * nUnitBits);
   end Containing_Encode_Fragments;

   --  Parts of 16K .. 64K units (headers C1 .. C4), then the last part, of
   --  n < 128 units in one octet or 128 <= n < 16K in two (a shorter length
   --  in two octets is not a valid encoding, X.691 10.9.3.6). Every part is
   --  checked against nMax, the bit budget and the rest of the stream before
   --  it is read; on failure the stream position is restored.
   procedure Containing_Decode_Fragments
     (bs          : in out Bitstream; data : in out OctetBuffer;
      data_length :    out Natural; nMax : Natural; nUnitBits : Positive;
      nBudget     :        Natural; success : out Boolean) with
      Pre => data'First = 1 and then (nUnitBits = 1 or else nUnitBits = 8)
      and then data'Length < Positive'Last / 16
      and then (if nUnitBits = 8 then nMax <= data'Length
                else nMax <= 8 * data'Length)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBudget,
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos
      and then bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + nBudget
      and then (if success then data_length <= nMax);

   procedure Containing_Decode_Fragments
     (bs          : in out Bitstream; data : in out OctetBuffer;
      data_length :    out Natural; nMax : Natural; nUnitBits : Positive;
      nBudget     :        Natural; success : out Boolean) with
      SPARK_Mode => Off
   is
      nStart  : constant Natural := bs.Current_Bit_Pos;
      nTotal  : Natural          := 0;
      nHeader : Integer          := 0;
      nLow    : Integer          := 0;
      nPart   : Integer          := 0;
      bLast   : Boolean          := False;
      function Fits (nBits : Natural) return Boolean is
        (bs.Current_Bit_Pos - nStart + nBits <= nBudget
         and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits);
   begin
      data_length := 0;
      success     := True;
      while success and not bLast loop
         success := Fits (8);
         if success then
            UPER_Dec_ConstraintWholeNumberInt
              (bs, nHeader, 0, 255, 8, success);
         end if;
         if success then
            case nHeader is
               when 16#00# .. 16#7F# =>
                  nPart := nHeader;
                  bLast := True;
               when 16#80# .. 16#BF# =>
                  success := Fits (8);
                  if success then
                     UPER_Dec_ConstraintWholeNumberInt
                       (bs, nLow, 0, 255, 8, success);
                     nPart   := (nHeader - 16#80#) * 16#100# + nLow;
                     success := success and then nPart >= 128;
                  end if;
                  bLast := True;
               when 16#C1# .. 16#C4# =>
                  nPart := (nHeader - 16#C0#) * 16#4000#;
               when others =>
                  success := False;
            end case;
         end if;
         if success then
            success :=
              nPart <= nMax - nTotal and then Fits (nPart * nUnitBits);
         end if;
         if success then
            BitStream_ReadBits
              (bs,
               data (data'First + nTotal * nUnitBits / 8 ..
                       data'First +
                       ((nTotal + nPart) * nUnitBits + 7) / 8 - 1),
               nPart * nUnitBits, success);
            nTotal := nTotal + nPart;
         end if;
      end loop;
      if success then
         data_length := nTotal;
      else
         bs.Current_Bit_Pos := nStart;
      end if;
   end Containing_Decode_Fragments;

   procedure BitStream_EncodeContainingOctets
     (bs         : in out Bitstream; data : OctetBuffer; nBits : Natural;
      nMaxOctets :        Positive)
   is
      --  X.691 10.1.3: padded to whole octets, at least one
      nCount : constant Natural := (if nBits = 0 then 1 else (nBits + 7) / 8);
   begin
      if nCount < 128 then
         UPER_Enc_ConstraintPosWholeNumber (bs, Asn1UInt (nCount), 0, 8);
         BitStream_AppendBits (bs, data (1 .. nCount), 8 * nCount);
      elsif nCount < 16#4000# then
         UPER_Enc_ConstraintPosWholeNumber
           (bs, 16#8000# + Asn1UInt (nCount), 0, 16);
         BitStream_AppendBits (bs, data (1 .. nCount), 8 * nCount);
      else
         Containing_Encode_Fragments
           (bs, data, nCount, 8,
            8 * nMaxOctets + Containing_Length_Max_Bits (nMaxOctets));
      end if;
   end BitStream_EncodeContainingOctets;

   procedure BitStream_DecodeContainingOctets
     (bs          : in out Bitstream; data : in out OctetBuffer;
      data_length :    out Natural; nMaxOctets : Positive;
      success     :    out Boolean)
   is
      nStart  : constant Natural := bs.Current_Bit_Pos;
      nHeader : Integer;
      nLow    : Integer;
      nCount  : Integer := 0;
   begin
      data_length := 0;
      UPER_Dec_ConstraintWholeNumberInt (bs, nHeader, 0, 255, 8, success);
      if success then
         if nHeader < 16#80# then
            nCount  := nHeader;
            success := nCount <= nMaxOctets;
         elsif nHeader < 16#C0# then
            UPER_Dec_ConstraintWholeNumberInt (bs, nLow, 0, 255, 8, success);
            nCount  := (nHeader - 16#80#) * 16#100# + nLow;
            success :=
              success and then nCount >= 128 and then nCount <= nMaxOctets;
         else
            success := False;
         end if;
         if success then
            BitStream_ReadBits (bs, data (1 .. nCount), 8 * nCount, success);
            data_length := nCount;
         elsif nHeader >= 16#C0# and then nMaxOctets >= 16#4000# then
            bs.Current_Bit_Pos := nStart;
            Containing_Decode_Fragments
              (bs, data, data_length, nMaxOctets, 8,
               8 * nMaxOctets + Containing_Length_Max_Bits (nMaxOctets),
               success);
         end if;
      end if;
   end BitStream_DecodeContainingOctets;

   procedure BitStream_EncodeContainingBits
     (bs       : in out Bitstream; data : OctetBuffer; nBits : Natural;
      nMaxBits :        Positive)
   is
      --  X.691 10.1.3: padded to whole octets, at least one; length in bits
      nCount : constant Natural :=
        8 * (if nBits = 0 then 1 else (nBits + 7) / 8);
   begin
      if nCount < 128 then
         UPER_Enc_ConstraintPosWholeNumber (bs, Asn1UInt (nCount), 0, 8);
         BitStream_AppendBits (bs, data (1 .. nCount / 8), nCount);
      elsif nCount < 16#4000# then
         UPER_Enc_ConstraintPosWholeNumber
           (bs, 16#8000# + Asn1UInt (nCount), 0, 16);
         BitStream_AppendBits (bs, data (1 .. nCount / 8), nCount);
      else
         Containing_Encode_Fragments
           (bs, data, nCount, 1,
            nMaxBits + Containing_Length_Max_Bits (nMaxBits));
      end if;
   end BitStream_EncodeContainingBits;

   procedure BitStream_DecodeContainingBits
     (bs          : in out Bitstream; data : in out OctetBuffer;
      data_length :    out Natural; nMaxBits : Positive;
      success     :    out Boolean)
   is
      nStart  : constant Natural := bs.Current_Bit_Pos;
      nHeader : Integer;
      nLow    : Integer;
      nCount  : Integer := 0;
   begin
      data_length := 0;
      UPER_Dec_ConstraintWholeNumberInt (bs, nHeader, 0, 255, 8, success);
      if success then
         if nHeader < 16#80# then
            nCount  := nHeader;
            success := nCount <= nMaxBits;
         elsif nHeader < 16#C0# then
            UPER_Dec_ConstraintWholeNumberInt (bs, nLow, 0, 255, 8, success);
            nCount  := (nHeader - 16#80#) * 16#100# + nLow;
            success :=
              success and then nCount >= 128 and then nCount <= nMaxBits;
         else
            success := False;
         end if;
         if success then
            BitStream_ReadBits
              (bs, data (1 .. (nCount + 7) / 8), nCount, success);
            data_length := nCount;
         elsif nHeader >= 16#C0# and then nMaxBits >= 16#4000# then
            bs.Current_Bit_Pos := nStart;
            Containing_Decode_Fragments
              (bs, data, data_length, nMaxBits, 1,
               nMaxBits + Containing_Length_Max_Bits (nMaxBits), success);
         end if;
      end if;
   end BitStream_DecodeContainingBits;

   procedure UPER_Dec_ConstraintPosWholeNumber_FullRange
     (bs : in out Bitstream; IntVal : out Asn1UInt; MinVal : Asn1UInt;
      nBits : Integer)
   is
      encVal  : Asn1UInt;
      success : Boolean;
   begin
      BitStream_Decode_Non_Negative_Integer (bs, encVal, nBits, success);
      pragma Assert (success);
      --  the mask states the nBits bound for the prover
      IntVal := MinVal + (encVal and (Shift_Left (Asn1UInt (1), nBits) - 1));
   end UPER_Dec_ConstraintPosWholeNumber_FullRange;

   procedure UPER_Dec_ConstraintPosWholeNumber_FullRangeUInt8
     (bs : in out Bitstream; IntVal : out Unsigned_8;
      MinVal : Unsigned_8; nBits : Integer)
   is
      v : Asn1UInt;
   begin
      UPER_Dec_ConstraintPosWholeNumber_FullRange
        (bs, v, Asn1UInt (MinVal), nBits);
      IntVal := Unsigned_8 (v);
   end UPER_Dec_ConstraintPosWholeNumber_FullRangeUInt8;

   procedure UPER_Dec_ConstraintPosWholeNumber_FullRangeUInt16
     (bs : in out Bitstream; IntVal : out Unsigned_16;
      MinVal : Unsigned_16; nBits : Integer)
   is
      v : Asn1UInt;
   begin
      UPER_Dec_ConstraintPosWholeNumber_FullRange
        (bs, v, Asn1UInt (MinVal), nBits);
      IntVal := Unsigned_16 (v);
   end UPER_Dec_ConstraintPosWholeNumber_FullRangeUInt16;

   procedure UPER_Dec_ConstraintPosWholeNumber_FullRangeUInt32
     (bs : in out Bitstream; IntVal : out Unsigned_32;
      MinVal : Unsigned_32; nBits : Integer)
   is
      v : Asn1UInt;
   begin
      UPER_Dec_ConstraintPosWholeNumber_FullRange
        (bs, v, Asn1UInt (MinVal), nBits);
      IntVal := Unsigned_32 (v);
   end UPER_Dec_ConstraintPosWholeNumber_FullRangeUInt32;

   procedure UPER_Dec_ConstraintWholeNumber_FullRange
     (bs : in out Bitstream; IntVal : out Asn1Int; MinVal : Asn1Int;
      nBits : Integer)
   is
      encVal  : Asn1UInt;
      success : Boolean;
   begin
      BitStream_Decode_Non_Negative_Integer (bs, encVal, nBits, success);
      pragma Assert (success);
      --  the mask states the nBits bound for the prover
      IntVal :=
        MinVal + Asn1Int (encVal and (Shift_Left (Asn1UInt (1), nBits) - 1));
   end UPER_Dec_ConstraintWholeNumber_FullRange;

   procedure UPER_Dec_ConstraintWholeNumber_FullRangeInt8
     (bs : in out Bitstream; IntVal : out Integer_8;
      MinVal : Integer_8; nBits : Integer)
   is
      v : Asn1Int;
   begin
      UPER_Dec_ConstraintWholeNumber_FullRange
        (bs, v, Asn1Int (MinVal), nBits);
      IntVal := Integer_8 (v);
   end UPER_Dec_ConstraintWholeNumber_FullRangeInt8;

   procedure UPER_Dec_ConstraintWholeNumber_FullRangeInt16
     (bs : in out Bitstream; IntVal : out Integer_16;
      MinVal : Integer_16; nBits : Integer)
   is
      v : Asn1Int;
   begin
      UPER_Dec_ConstraintWholeNumber_FullRange
        (bs, v, Asn1Int (MinVal), nBits);
      IntVal := Integer_16 (v);
   end UPER_Dec_ConstraintWholeNumber_FullRangeInt16;

   procedure UPER_Dec_ConstraintWholeNumber_FullRangeInt32
     (bs : in out Bitstream; IntVal : out Integer_32;
      MinVal : Integer_32; nBits : Integer)
   is
      v : Asn1Int;
   begin
      UPER_Dec_ConstraintWholeNumber_FullRange
        (bs, v, Asn1Int (MinVal), nBits);
      IntVal := Integer_32 (v);
   end UPER_Dec_ConstraintWholeNumber_FullRangeInt32;

   procedure UPER_Dec_ConstraintWholeNumberInt_FullRange
     (bs : in out Bitstream; IntVal : out Integer; MinVal : Integer;
      nBits : Integer)
   is
      v : Asn1Int;
   begin
      UPER_Dec_ConstraintWholeNumber_FullRange
        (bs, v, Asn1Int (MinVal), nBits);
      IntVal := Integer (v);
   end UPER_Dec_ConstraintWholeNumberInt_FullRange;

end adaasn1rtl.encoding.uper;
