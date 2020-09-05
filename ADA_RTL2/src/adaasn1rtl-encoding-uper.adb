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

   procedure UPER_Dec_ConstraintPosWholeNumber
     (bs     : in out Bitstream; IntVal : out Asn1UInt; MinVal : Asn1UInt;
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

      if RealVal >= 0.0 and RealVal <= 0.0 then
         BitStream_AppendByte (bs, 0, False);
      elsif RealVal = PLUS_INFINITY then
         BitStream_AppendByte (bs, 1, False);
         BitStream_AppendByte (bs, 16#40#, False);
      elsif RealVal = MINUS_INFINITY then
         BitStream_AppendByte (bs, 1, False);
         BitStream_AppendByte (bs, 16#41#, False);
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
            val.values (1) := si / 40;
            val.values (2) := si mod 40;

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
   begin
      for i in data'First .. (data'First + data_length - 1) loop
         pragma Loop_Invariant
           (bs.Current_Bit_Pos =
            bs.Current_Bit_Pos'Loop_Entry + (i - data'First) * 8);
         BitStream_AppendByte (bs, data (i), False);
      end loop;
   end BitStream_EncodeOctetString_no_length;

   procedure BitStream_DecodeOctetString_no_length
     (bs : in out Bitstream; data : in out OctetBuffer; data_length : Integer;
      success :    out Boolean)
   is
   begin
      success := True;
      for i in data'First .. (data'First + data_length - 1) loop
         pragma Loop_Invariant
           (bs.Current_Bit_Pos =
            bs.Current_Bit_Pos'Loop_Entry + (i - data'First) * 8);
         BitStream_DecodeByte (bs, data (i), success);
         exit when not success;
      end loop;
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

   procedure BitStream_DecodeOctetString_fragmentation
     (bs          : in out Bitstream; data : in out OctetBuffer;
      data_length : out Integer; asn1SizeMin : Integer; asn1SizeMax : Integer;
      success     :    out Boolean);

   procedure BitStream_DecodeOctetString_fragmentation
     (bs          : in out Bitstream; data : in out OctetBuffer;
      data_length : out Integer; asn1SizeMin : Integer; asn1SizeMax : Integer;
      success     :    out Boolean) with
      SPARK_Mode => Off
   is
      i1                  : Integer;
      nLengthTmp1         : Integer := 0;
      nRemainingItemsVar1 : Integer;
      nCurBlockSize1      : Integer := 0;
      nCurOffset1         : Integer;
   begin
      --  decode blockSize
      data_length := asn1SizeMin;
      UPER_Dec_ConstraintWholeNumberInt
        (bs, nRemainingItemsVar1, 0, 255, 8, success);
      nCurOffset1 := 1;

      while success and
        (nRemainingItemsVar1 = 16#C4# or nRemainingItemsVar1 = 16#C3# or
         nRemainingItemsVar1 = 16#C2# or nRemainingItemsVar1 = 16#C1#)
      loop
         if nRemainingItemsVar1 = 16#C4# then
            nCurBlockSize1 := 16#10000#;
         elsif nRemainingItemsVar1 = 16#C3# then
            nCurBlockSize1 := 16#C000#;
         elsif nRemainingItemsVar1 = 16#C2# then
            nCurBlockSize1 := 16#8000#;
         else
            nCurBlockSize1 := 16#4000#;
         end if;

         nLengthTmp1 := nLengthTmp1 + nCurBlockSize1;
         success     := nLengthTmp1 <= asn1SizeMax;
         i1          := nCurOffset1;
         while i1 <= nCurOffset1 + nCurBlockSize1 - 1 and success loop
            BitStream_DecodeByte (bs, data (i1), success);
            i1 := i1 + 1;
         end loop;
         nCurOffset1 := nCurOffset1 + nCurBlockSize1;
         UPER_Dec_ConstraintWholeNumberInt
           (bs, nRemainingItemsVar1, 0, 255, 8, success);
      end loop;

      if nRemainingItemsVar1 >= 16#80# then
         declare
            len2 : Integer;
         begin
            nRemainingItemsVar1 := (nRemainingItemsVar1 - 16#80#) * 16#100#;
            UPER_Dec_ConstraintWholeNumberInt (bs, len2, 0, 255, 8, success);
            if success then
               nRemainingItemsVar1 := nRemainingItemsVar1 + len2;
            end if;
         end;
      end if;

      if nCurOffset1 + nRemainingItemsVar1 - 1 <= asn1SizeMax then
         i1 := nCurOffset1;
         while i1 <= nCurOffset1 + nRemainingItemsVar1 - 1 loop
            BitStream_DecodeByte (bs, data (i1), success);
            i1 := i1 + 1;
         end loop;
         nLengthTmp1 := nLengthTmp1 + nRemainingItemsVar1;
      end if;

      if nLengthTmp1 >= asn1SizeMin and nLengthTmp1 <= asn1SizeMax then
         data_length := nLengthTmp1;

      else
         success := False;   --  COVERAGE_IGNORE
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

   procedure BitStream_DecodeBitString_fragmentation
     (bs          : in out Bitstream; data : in out OctetBuffer;
      data_length : out Integer; asn1SizeMin : Integer; asn1SizeMax : Integer;
      success     :    out Boolean) with
      SPARK_Mode => Off
   is
      nLengthTmp1         : Integer := 0;
      nRemainingItemsVar1 : Integer;
      nCurBlockSize1      : Integer := 0;
      nCurOffset1         : Integer;
   begin

      UPER_Dec_ConstraintWholeNumberInt
        (bs, nRemainingItemsVar1, 0, 255, 8, success);
      nCurOffset1 := 1;

      while success and
        (nRemainingItemsVar1 = 16#C4# or nRemainingItemsVar1 = 16#C3# or
         nRemainingItemsVar1 = 16#C2# or nRemainingItemsVar1 = 16#C1#)
      loop
         if nRemainingItemsVar1 = 16#C4# then
            nCurBlockSize1 := 16#10000#;
         elsif nRemainingItemsVar1 = 16#C3# then
            nCurBlockSize1 := 16#C000#;
         elsif nRemainingItemsVar1 = 16#C2# then
            nCurBlockSize1 := 16#8000#;
         else
            nCurBlockSize1 := 16#4000#;
         end if;

         BitStream_ReadBits
           (bs,
            data
              (data'First + nCurOffset1 / 8 ..
                   data'First + (nCurOffset1 + nCurBlockSize1) / 8 - 1),
            nCurBlockSize1, success);

         if success then
            nLengthTmp1 := nLengthTmp1 + nCurBlockSize1;
            nCurOffset1 := nCurOffset1 + nCurBlockSize1;
            UPER_Dec_ConstraintWholeNumberInt
              (bs, nRemainingItemsVar1, 0, 255, 8, success);
         end if;
      end loop;

      if success then

         if nRemainingItemsVar1 >= 16#80# then
            declare
               len2 : Integer;
            begin
               nRemainingItemsVar1 := (nRemainingItemsVar1 - 16#80#) * 16#100#;
               UPER_Dec_ConstraintWholeNumberInt
                 (bs, len2, 0, 255, 8, success);
               if success then
                  nRemainingItemsVar1 := nRemainingItemsVar1 + len2;
               end if;
            end;
         end if;

         if success and nCurOffset1 + nRemainingItemsVar1 - 1 <= asn1SizeMax
         then

            BitStream_ReadBits
              (bs,
               data
                 (data'First + nCurOffset1 / 8 ..
                      data'First + (nCurOffset1 + nRemainingItemsVar1) / 8 -
                      1),
               nRemainingItemsVar1, success);

            nLengthTmp1 := nLengthTmp1 + nRemainingItemsVar1;
         end if;

         if success and nLengthTmp1 >= 1 and nLengthTmp1 <= asn1SizeMax then
            data_length := nLengthTmp1;

         else
            data_length := asn1SizeMin;
            success     := False;       --  COVERAGE_IGNORE
         end if;

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

end adaasn1rtl.encoding.uper;
