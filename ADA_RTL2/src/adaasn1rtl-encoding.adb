--  with user_code;

package body adaasn1rtl.encoding with
     Spark_Mode is

   MASKS : constant OctetBuffer_0_7 :=
     OctetBuffer_0_7'
       (0 => 16#80#,
        1 => 16#40#,
        2 => 16#20#,
        3 => 16#10#,
        4 => 16#08#,
        5 => 16#04#,
        6 => 16#02#,
        7 => 16#01#);
   MASKSB : constant OctetBuffer_0_7 :=
     OctetBuffer_0_7'
       (0 => 16#00#,
        1 => 16#01#,
        2 => 16#03#,
        3 => 16#07#,
        4 => 16#0F#,
        5 => 16#1F#,
        6 => 16#3F#,
        7 => 16#7F#);

   MantissaFactor : constant Asn1Real :=
     Asn1Real (Interfaces.Unsigned_64 (2)**Asn1Real'Machine_Mantissa);

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

   function Sub (A : Asn1Int; B : Asn1Int) return Asn1UInt is
      ret : Asn1UInt;
      au  : Asn1UInt;
      bu  : Asn1UInt;
   begin

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

   function GetBytes (V : Asn1UInt) return Asn1Byte is
      Ret : Asn1Byte;
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

   function GetLengthInBytesOfSIntAux (V : Asn1UInt) return Asn1Byte;

   function GetLengthInBytesOfSIntAux (V : Asn1UInt) return Asn1Byte is
      Ret : Asn1Byte;
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

   function GetLengthInBytesOfSInt (V : Asn1Int) return Asn1Byte is
      Ret : Asn1Byte;
   begin
      if V >= 0 then
         Ret := GetLengthInBytesOfSIntAux (Asn1UInt (V));
      else
         Ret := GetLengthInBytesOfSIntAux (Asn1UInt (-(V + 1)));
      end if;
      return Ret;
   end GetLengthInBytesOfSInt;

   function GetExponent (V : Asn1Real) return Asn1Int is
      pragma SPARK_Mode (Off);
      --  due to the fact that Examiner has not yet implement the Exponent
      --  attribute
   begin
      return Asn1Int (Asn1Real'Exponent (V) - Asn1Real'Machine_Mantissa);
   end GetExponent;

   function GetMantissa (V : Asn1Real) return Asn1UInt is
      pragma SPARK_Mode (Off);
      --  due to the fact that Examiner has not yet implement the Fraction
      --  attribute
   begin
      return Asn1UInt (Asn1Real'Fraction (V) * MantissaFactor);
   end GetMantissa;

   function Zero return Asn1Real;

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

   function BitStream_init
     (Bitstream_Size_In_Bytes : Positive) return Bitstream is
     (Bitstream'
        (Size_In_Bytes   => Bitstream_Size_In_Bytes,
         Current_Bit_Pos => 0,
         pushDataPrm     => 0,
         fetchDataPrm    => 0,
         Buffer          => (others => 0)));

   procedure BitStream_AppendBit (bs : in out Bitstream; Bit_Value : BIT) is
      Current_Byte : constant Integer :=
        bs.Buffer'First + bs.Current_Bit_Pos / 8;
      Current_Bit : constant BIT_RANGE := bs.Current_Bit_Pos mod 8;
   begin

      bs.Buffer (Current_Byte) :=
        (if Bit_Value = 1 then bs.Buffer (Current_Byte) or MASKS (Current_Bit)
         else bs.Buffer (Current_Byte) and (not MASKS (Current_Bit)));

      bs.Current_Bit_Pos := bs.Current_Bit_Pos + 1;
      bitstrean_push_data_if_required (bs);

   end BitStream_AppendBit;

   procedure BitStream_ReadBit
     (bs        : in out Bitstream;
      Bit_Value :    out BIT;
      result    :    out Boolean)
   is
      Current_Byte : constant Integer :=
        bs.Buffer'First + bs.Current_Bit_Pos / 8;
      Current_Bit : constant BIT_RANGE := bs.Current_Bit_Pos mod 8;
   begin
      result :=
        bs.Current_Bit_Pos < Natural'Last and
        bs.Current_Bit_Pos < bs.Size_In_Bytes * 8;

      if (bs.Buffer (Current_Byte) and MASKS (Current_Bit)) > 0 then
         Bit_Value := 1;
      else
         Bit_Value := 0;
      end if;

      bs.Current_Bit_Pos := bs.Current_Bit_Pos + 1;
      bitstrean_fetch_data_if_required (bs);
   end BitStream_ReadBit;

   procedure BitStream_AppendByte
     (bs         : in out Bitstream;
      Byte_Value :      Asn1Byte;
      Negate     :      Boolean)
   is
      Current_Byte : constant Integer :=
        bs.Buffer'First + bs.Current_Bit_Pos / 8;
      Current_Bit : constant BIT_RANGE := bs.Current_Bit_Pos mod 8;
      byteVal     : Asn1Byte;
      mask        : Asn1Byte;
      ncb         : BIT_RANGE;
   begin
      if Negate then
         byteVal := not Byte_Value;
      else
         byteVal := Byte_Value;
      end if;

      if Current_Bit > 0 then
         ncb                      := 8 - Current_Bit;
         mask                     := not MASKSB (ncb);
         bs.Buffer (Current_Byte) := bs.Buffer (Current_Byte) and mask;
         bs.Buffer (Current_Byte) :=
           bs.Buffer (Current_Byte) or Shift_Right (byteVal, Current_Bit);
         mask                         := not mask;
         bs.Buffer (Current_Byte + 1) := bs.Buffer (Current_Byte + 1) and mask;
         bs.Buffer (Current_Byte + 1) :=
           bs.Buffer (Current_Byte + 1) or Shift_Left (byteVal, ncb);
      else
         bs.Buffer (Current_Byte) := byteVal;
      end if;
      bs.Current_Bit_Pos := bs.Current_Bit_Pos + 8;
      bitstrean_push_data_if_required (bs);
   end BitStream_AppendByte;

   procedure BitStream_DecodeByte
     (bs         : in out Bitstream;
      Byte_Value :    out Asn1Byte;
      success    :    out Boolean)
   is
   begin
      success :=
        bs.Current_Bit_Pos < Natural'Last - 8
        and then bs.Size_In_Bytes < Positive'Last / 8
        and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8;

      Byte_Value         := BitStream_PeekByte (bs, 0);
      bs.Current_Bit_Pos := bs.Current_Bit_Pos + 8;
      bitstrean_fetch_data_if_required (bs);

   end BitStream_DecodeByte;

   function BitStream_PeekByte
     (bs     :  Bitstream;
      offset :    Natural) return Asn1Byte
   is
      Current_Byte : constant Integer :=
        bs.Buffer'First + (bs.Current_Bit_Pos + offset) / 8;
      Current_Bit : constant BIT_RANGE := (bs.Current_Bit_Pos + offset) mod 8;
      ncb         : BIT_RANGE;
      ret_byte    : Asn1Byte;
   begin
      if Current_Bit > 0 then
         ncb      := 8 - Current_Bit;
         ret_byte := Shift_Left (bs.Buffer (Current_Byte), Current_Bit);
         ret_byte :=
           ret_byte or Shift_Right (bs.Buffer (Current_Byte + 1), ncb);
      else
         ret_byte := bs.Buffer (Current_Byte);
      end if;

      return ret_byte;
   end BitStream_PeekByte;

   procedure BitStream_ReadNibble
     (bs         : in out Bitstream;
      Byte_Value :    out Asn1Byte;
      success    :    out Boolean)
   is
      Current_Byte : constant Integer :=
        bs.Buffer'First + bs.Current_Bit_Pos / 8;
      Current_Bit          : constant BIT_RANGE := bs.Current_Bit_Pos mod 8;
      totalBitsForNextByte : BIT_RANGE;
   begin
      success :=
        bs.Current_Bit_Pos < Natural'Last - 4
        and then bs.Size_In_Bytes < Positive'Last / 8
        and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 4;

      if Current_Bit < 4 then
         Byte_Value :=
           Shift_Right (bs.Buffer (Current_Byte), 4 - Current_Bit) and 16#0F#;
      else
         totalBitsForNextByte := Current_Bit - 4;
         Byte_Value           :=
           Shift_Left (bs.Buffer (Current_Byte), totalBitsForNextByte);
         if totalBitsForNextByte > 0 then
            Byte_Value :=
              Byte_Value or
              (Shift_Right
                 (bs.Buffer (Current_Byte + 1),
                  8 - totalBitsForNextByte));
         end if;

         Byte_Value := Byte_Value and 16#0F#;
      end if;
      bs.Current_Bit_Pos := bs.Current_Bit_Pos + 4;
      bitstrean_fetch_data_if_required (bs);

   end BitStream_ReadNibble;

   procedure BitStream_AppendPartialByte
     (bs         : in out Bitstream;
      Byte_Value :     Asn1Byte;
      nBits      :     BIT_RANGE;
      negate     :     Boolean)
   is
      Current_Byte : Integer := bs.Buffer'First + bs.Current_Bit_Pos / 8;
      cb                   : constant BIT_RANGE := bs.Current_Bit_Pos mod 8;
      totalBits            : BIT_RANGE;
      totalBitsForNextByte : BIT_RANGE;
      byteValue            : Asn1Byte;
      mask1                : Asn1Byte;
      mask2                : Asn1Byte;
      mask                 : Asn1Byte;
   begin
      if nBits > 0 then
         byteValue :=
           (if negate then (MASKSB (nBits) and not Byte_Value)
            else Byte_Value);
         mask1 := (if cb > 0 then not MASKSB (8 - cb) else 0);

         if cb < 8 - nBits then
            totalBits := cb + nBits;

            mask2                    := MASKSB (8 - totalBits);
            mask                     := mask1 or mask2;
            bs.Buffer (Current_Byte) := bs.Buffer (Current_Byte) and mask;
            bs.Buffer (Current_Byte) :=
              bs.Buffer (Current_Byte) or
              Shift_Left (byteValue, 8 - totalBits);
         else
            totalBitsForNextByte     := cb + nBits - 8;
            bs.Buffer (Current_Byte) := bs.Buffer (Current_Byte) and mask1;
            bs.Buffer (Current_Byte) :=
              bs.Buffer (Current_Byte) or
              Shift_Right (byteValue, totalBitsForNextByte);

            if totalBitsForNextByte > 0 then
               Current_Byte := Current_Byte + 1;

               mask := not MASKSB (8 - totalBitsForNextByte);

               bs.Buffer (Current_Byte) := bs.Buffer (Current_Byte) and mask;
               bs.Buffer (Current_Byte) :=
                 bs.Buffer (Current_Byte) or
                 Shift_Left (byteValue, 8 - totalBitsForNextByte);
            end if;

         end if;
         bs.Current_Bit_Pos := bs.Current_Bit_Pos + nBits;
         bitstrean_push_data_if_required (bs);

      end if;
   end BitStream_AppendPartialByte;

   procedure BitStream_AppendBits
     (bs                 : in out Bitstream;
      bitMaskAsByteArray :      OctetBuffer;
      bits_to_write      :      Natural)
   is
      total_bytes : constant Integer   := bits_to_write / 8;
      rest_bits   : constant BIT_RANGE := bits_to_write mod 8;
      lastByte    : Asn1Byte;
   begin
      for i in
        bitMaskAsByteArray'First ..
            (bitMaskAsByteArray'First + total_bytes - 1)
      loop
         pragma Loop_Invariant
           (bs.Current_Bit_Pos =
            bs.Current_Bit_Pos'Loop_Entry +
              (i - bitMaskAsByteArray'First) * 8);
         BitStream_AppendByte (bs, bitMaskAsByteArray (i), False);
      end loop;
      if rest_bits > 0 then
         lastByte :=
           Shift_Right
             (bitMaskAsByteArray (bitMaskAsByteArray'First + total_bytes),
              8 - rest_bits);
         BitStream_AppendPartialByte (bs, lastByte, rest_bits, False);
      end if;
   end BitStream_AppendBits;

   procedure BitStream_ReadBits
     (bs                 : in out Bitstream;
      bitMaskAsByteArray : in out OctetBuffer;
      bits_to_read       :     Natural;
      success            : out Boolean)
   is
      total_bytes : constant Integer   := bits_to_read / 8;
      rest_bits   : constant BIT_RANGE := bits_to_read mod 8;
   begin
      success := True;
      for i in
        bitMaskAsByteArray'First ..
            (bitMaskAsByteArray'First + total_bytes - 1)
      loop
         pragma Loop_Invariant
           (bs.Current_Bit_Pos =
            bs.Current_Bit_Pos'Loop_Entry +
              (i - bitMaskAsByteArray'First) * 8);
         BitStream_DecodeByte (bs, bitMaskAsByteArray (i), success);
         if not success then
            return;
         end if;
      end loop;
      if rest_bits > 0 then
         BitStream_ReadPartialByte
           (bs,
            bitMaskAsByteArray (bitMaskAsByteArray'First + total_bytes),
            rest_bits);
         bitMaskAsByteArray (bitMaskAsByteArray'First + total_bytes) :=
           Shift_Left
             (bitMaskAsByteArray (bitMaskAsByteArray'First + total_bytes),
              8 - rest_bits);
      end if;
   end BitStream_ReadBits;

   procedure BitStream_SkipBits
     (bs           : in out Bitstream;
      bits_to_skip :     Natural)
   is
   begin
      bs.Current_Bit_Pos := bs.Current_Bit_Pos + bits_to_skip;
   end BitStream_SkipBits;

   procedure BitStream_ReadPartialByte
     (bs         : in out Bitstream;
      Byte_Value :    out Asn1Byte;
      nBits      :    BIT_RANGE)
   is
   begin
      Byte_Value         := BitStream_PeekPartialByte (bs, 0, nBits);
      bs.Current_Bit_Pos := bs.Current_Bit_Pos + nBits;
      bitstrean_fetch_data_if_required (bs);
   end BitStream_ReadPartialByte;

   function BitStream_PeekPartialByte
     (bs     : Bitstream;
      offset : Natural;
      nBits  : BIT_RANGE) return Asn1Byte
   is
      Current_Byte : Integer :=
        bs.Buffer'First + (bs.Current_Bit_Pos + offset) / 8;
      cb : constant BIT_RANGE := (bs.Current_Bit_Pos + offset) mod 8;
      totalBits            : BIT_RANGE;
      totalBitsForNextByte : BIT_RANGE;
      Byte_Value           : Asn1Byte;
   begin
      if cb < 8 - nBits then
         totalBits  := cb + nBits;
         Byte_Value :=
           Shift_Right (bs.Buffer (Current_Byte), 8 - totalBits) and
           MASKSB (nBits);
      else
         totalBitsForNextByte := cb + nBits - 8;
         Byte_Value           :=
           Shift_Left (bs.Buffer (Current_Byte), totalBitsForNextByte);
         if totalBitsForNextByte > 0 then
            Current_Byte := Current_Byte + 1;
            Byte_Value   :=
              Byte_Value or
              Shift_Right (bs.Buffer (Current_Byte), 8 - totalBitsForNextByte);
         end if;
         Byte_Value := Byte_Value and MASKSB (nBits);
      end if;
      return Byte_Value;
   end BitStream_PeekPartialByte;

   procedure BitStream_Encode_Non_Negative_Integer
     (bs       : in out Bitstream;
      intValue : Asn1UInt;
      nBits    : Integer)
   is
      byteValue     : Asn1Byte;
      tmp           : Asn1UInt;
      total_bytes   : constant Integer   := nBits / 8;
      cc            : constant BIT_RANGE := nBits mod 8;
      bits_to_shift : Integer;
   begin
      for i in 1 .. total_bytes loop
         bits_to_shift := (total_bytes - i) * 8 + cc;
         tmp           := 16#FF# and Shift_Right (intValue, bits_to_shift);
         byteValue     := Asn1Byte (tmp);

         pragma Loop_Invariant
           (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i - 1) * 8);

         BitStream_AppendByte (bs, byteValue, False);
      end loop;

      if cc > 0 then
         byteValue := MASKSB (cc) and Asn1Byte (16#FF# and intValue);
         BitStream_AppendPartialByte (bs, byteValue, cc, False);
      end if;

   end BitStream_Encode_Non_Negative_Integer;

   procedure BitStream_Decode_Non_Negative_Integer
     (bs       : in out Bitstream;
      IntValue :    out Asn1UInt;
      nBits    :        Integer;
      result   :    out Boolean)
   is
      byteValue   : Asn1Byte;
      total_bytes : constant Integer   := nBits / 8;
      cc          : constant BIT_RANGE := nBits mod 8;
   begin
      result :=
        nBits >= 0
        and then nBits <= Asn1UInt'Size
        and then bs.Current_Bit_Pos < Natural'Last - nBits
        and then bs.Size_In_Bytes < Positive'Last / 8
        and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits;
      IntValue := 0;

      for i in 1 .. total_bytes loop
         pragma Loop_Invariant
           (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i - 1) * 8);

         BitStream_DecodeByte (bs, byteValue, result);
         IntValue := (IntValue * 256) or Asn1UInt (byteValue);
      end loop;

      if cc > 0 then
         BitStream_ReadPartialByte (bs, byteValue, cc);

         IntValue := Shift_Left (IntValue, cc) or Asn1UInt (byteValue);
      end if;

   end BitStream_Decode_Non_Negative_Integer;

   procedure Enc_UInt
     (bs          : in out Bitstream;
      intValue    :        Asn1UInt;
      total_bytes :        Integer)
   is
      byteValue     : Asn1Byte;
      tmp           : Asn1UInt;
      bits_to_shift : Integer;
   begin

      for i in 1 .. total_bytes loop
         bits_to_shift := (total_bytes - i) * 8;
         tmp           := 16#FF# and Shift_Right (intValue, bits_to_shift);
         byteValue     := Asn1Byte (tmp);

         pragma Loop_Invariant
           (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i - 1) * 8);

         BitStream_AppendByte (bs, byteValue, False);
      end loop;

   end Enc_UInt;

   procedure Dec_UInt
     (bs          : in out Bitstream;
      total_bytes :        Integer;
      Ret         :    out Asn1UInt;
      Result      :    out Boolean)
   is
      ByteVal : Asn1Byte;
   begin
      Ret    := 0;
      Result :=
        total_bytes >= 0
        and then total_bytes <= Asn1UInt'Size / 8
        and then bs.Current_Bit_Pos < Natural'Last - total_bytes * 8
        and then bs.Size_In_Bytes < Positive'Last / 8
        and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - total_bytes * 8;

      for i in 1 .. total_bytes loop
         pragma Loop_Invariant
           (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i - 1) * 8);

         BitStream_DecodeByte (bs, ByteVal, Result);
         Ret := Ret * 256; --  shift left one byte
         Ret := Ret + Asn1UInt (ByteVal);

      end loop;
      if total_bytes < 8 then
         pragma Assume (Ret < 256**total_bytes);
      end if;

   end Dec_UInt;

   procedure Dec_Int
     (bs          : in out Bitstream;
      total_bytes :        Integer;
      int_value   :    out Asn1Int;
      Result      :    out Boolean)
   is
      Current_Byte : constant Integer :=
        bs.Buffer'First + bs.Current_Bit_Pos / 8;
      Current_Bit : constant BIT_RANGE := bs.Current_Bit_Pos mod 8;
      ByteVal     : Asn1Byte;
      Ret         : Asn1UInt;
   begin
      Result :=
        total_bytes >= 0
        and then total_bytes <= Asn1UInt'Size / 8
        and then bs.Current_Bit_Pos < Natural'Last - total_bytes * 8
        and then bs.Size_In_Bytes < Positive'Last / 8
        and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - total_bytes * 8;
      if total_bytes > 0 then
         if (bs.Buffer (Current_Byte) and MASKS (Current_Bit)) = 0 then
            Ret := 0;
         else
            Ret := Asn1UInt'Last;
         end if;

         for i in 1 .. total_bytes loop
            pragma Loop_Invariant
              (bs.Current_Bit_Pos =
               bs.Current_Bit_Pos'Loop_Entry + (i - 1) * 8);

            BitStream_DecodeByte (bs, ByteVal, Result);
            Ret := (Ret * 256) or Asn1UInt (ByteVal);

         end loop;
         int_value := To_Int (Ret);
      else
         int_value := 0;
      end if;

   end Dec_Int;

   function RequiresReverse return Boolean is
      pragma SPARK_Mode (Off);
      dword : Integer := 16#00000001#;
      arr   : aliased OctetArray4;
      for arr'Address use dword'Address;
   begin
      return arr (arr'First) = 1;
   end RequiresReverse;

   function Long_Float_to_Float (x : Asn1Real) return Float is
      pragma SPARK_Mode (Off);
   begin
      return Float (x);
   end Long_Float_to_Float;

   procedure Enc_SemiConstraintWholeNumber
     (bs     : in out Bitstream;
      IntVal :        Asn1Int;
      MinVal :        Asn1Int)
   is
      nBytes             : Asn1Byte;
      ActualEncodedValue : Asn1UInt;
   begin
      ActualEncodedValue := Sub (IntVal, MinVal);

      nBytes := GetBytes (ActualEncodedValue);

      --  encode length
      BitStream_AppendByte (bs, nBytes, False);
      --  Encode number
      Enc_UInt (bs, ActualEncodedValue, Integer (nBytes));
   end Enc_SemiConstraintWholeNumber;

   procedure Dec_SemiConstraintWholeNumber
     (bs     : in out Bitstream;
      IntVal :    out Asn1Int;
      MinVal :        Asn1Int;
      Result :    out Boolean)
   is
      NBytes : Asn1Byte;
      Ret    : Asn1UInt;
   begin

      IntVal := MinVal;
      BitStream_DecodeByte (bs, NBytes, Result);
      if Result and NBytes >= 1 and NBytes <= 8 then
         Dec_UInt (bs, Integer (NBytes), Ret, Result);
         IntVal := To_Int (Ret + To_UInt (MinVal));
         Result := IntVal >= MinVal;
         if not Result then
            IntVal := MinVal;
         end if;
      else
         Result := False;
      end if;

   end Dec_SemiConstraintWholeNumber;

   procedure Enc_SemiConstraintPosWholeNumber
     (bs     : in out Bitstream;
      IntVal :        Asn1UInt;
      MinVal :        Asn1UInt)
   is
      nBytes             : Asn1Byte;
      ActualEncodedValue : Asn1UInt;
   begin
      ActualEncodedValue := IntVal - MinVal;

      nBytes := GetBytes (ActualEncodedValue);

      --  encode length
      BitStream_AppendByte (bs, nBytes, False);
      --  Encode number
      Enc_UInt (bs, ActualEncodedValue, Integer (nBytes));
   end Enc_SemiConstraintPosWholeNumber;

   procedure Dec_SemiConstraintPosWholeNumber
     (bs     : in out Bitstream;
      IntVal :    out Asn1UInt;
      MinVal :        Asn1UInt;
      Result :    out Boolean)
   is
      NBytes : Asn1Byte;
      Ret    : Asn1UInt := 0;
   begin

      IntVal := MinVal;
      pragma Assert (IntVal >= MinVal);
      BitStream_DecodeByte (bs, NBytes, Result);
      Result := Result and NBytes >= 1 and NBytes <= 8;
      if Result then
         Dec_UInt (bs, Integer (NBytes), Ret, Result);
         IntVal := Ret + MinVal;
         Result := IntVal >= MinVal;
         if not Result then
            IntVal := MinVal;
         end if;

      end if;
      pragma Assert (IntVal >= MinVal);

   end Dec_SemiConstraintPosWholeNumber;

   procedure Enc_UnConstraintWholeNumber
     (bs     : in out Bitstream;
      IntVal :        Asn1Int)
   is
      nBytes : Asn1Byte;
   begin

      nBytes := GetLengthInBytesOfSInt (IntVal);

      --  encode length
      BitStream_AppendByte (bs, nBytes, False);
      Enc_UInt (bs, To_UInt (IntVal), Integer (nBytes));
   end Enc_UnConstraintWholeNumber;

   procedure Dec_UnConstraintWholeNumber
     (bs     : in out Bitstream;
      IntVal :    out Asn1Int;
      Result :    out Boolean)
   is
      NBytes : Asn1Byte;
   begin
      BitStream_DecodeByte (bs, NBytes, Result);
      Result := Result and NBytes >= 1 and NBytes <= 8;
      if Result then
         Dec_Int (bs, Integer (NBytes), IntVal, Result);
      else
         IntVal := 0;
         Result := False;
      end if;
   end Dec_UnConstraintWholeNumber;

   procedure Enc_ConstraintWholeNumber
     (bs     : in out Bitstream;
      IntVal :      Asn1Int;
      MinVal :      Asn1Int;
      nBits  :      Integer)
   is
      encVal : Asn1UInt;
   begin
      encVal := Sub (IntVal, MinVal);
      BitStream_Encode_Non_Negative_Integer (bs, encVal, nBits);
   end Enc_ConstraintWholeNumber;

   procedure Enc_ConstraintPosWholeNumber
     (bs     : in out Bitstream;
      IntVal :      Asn1UInt;
      MinVal :      Asn1UInt;
      nBits  :      Integer)
   is
      encVal : Asn1UInt;
   begin
      encVal := IntVal - MinVal;
      BitStream_Encode_Non_Negative_Integer (bs, encVal, nBits);
   end Enc_ConstraintPosWholeNumber;

   procedure Dec_ConstraintWholeNumber
     (bs     : in out Bitstream;
      IntVal :    out Asn1Int;
      MinVal :      Asn1Int;
      MaxVal :      Asn1Int;
      nBits  :      Integer;
      Result :    out Boolean)
   is
      encVal : Asn1UInt;
   begin
      BitStream_Decode_Non_Negative_Integer (bs, encVal, nBits, Result);
      if Result then
         IntVal := To_Int (encVal + To_UInt (MinVal));

         Result := IntVal >= MinVal and IntVal <= MaxVal;
         if not Result then
            IntVal := MinVal;
         end if;
      else
         IntVal := MinVal;
      end if;

   end Dec_ConstraintWholeNumber;

   procedure Dec_ConstraintPosWholeNumber
     (bs     : in out Bitstream;
      IntVal :    out Asn1UInt;
      MinVal :      Asn1UInt;
      MaxVal :      Asn1UInt;
      nBits  :      Integer;
      Result :    out Boolean)
   is
      encVal : Asn1UInt;
   begin
      BitStream_Decode_Non_Negative_Integer (bs, encVal, nBits, Result);
      if Result then
         IntVal := encVal + MinVal;

         Result := IntVal >= MinVal and IntVal <= MaxVal;
         if not Result then
            IntVal := MinVal;
         end if;
      else
         IntVal := MinVal;
      end if;

   end Dec_ConstraintPosWholeNumber;

   procedure Dec_ConstraintWholeNumberInt
     (bs     : in out Bitstream;
      IntVal :    out Integer;
      MinVal :      Integer;
      MaxVal :      Integer;
      nBits  :      Integer;
      Result :    out Boolean)
   is
      Ret : Asn1Int;
   begin
      Dec_ConstraintWholeNumber
        (bs,
         Ret,
         Asn1Int (MinVal),
         Asn1Int (MaxVal),
         nBits,
         Result);
      IntVal := Integer (Ret);
   end Dec_ConstraintWholeNumberInt;

   function BitStream_bitPatternMatches
     (bs                                  :  Bitstream;
      bit_terminated_pattern              :  OctetBuffer;
      bit_terminated_pattern_size_in_bits :    Natural) return Boolean
   is
      total_bytes : constant Natural :=
        bit_terminated_pattern_size_in_bits / 8;
      rest_bits : constant BIT_RANGE :=
        bit_terminated_pattern_size_in_bits mod 8;
      offset  : Natural := 0;
      tmpByte : Asn1Byte;
   begin
      for i in
        bit_terminated_pattern'First ..
            (bit_terminated_pattern'First + total_bytes - 1)
      loop
         pragma Loop_Invariant
           (offset =
            offset'Loop_Entry + (i - bit_terminated_pattern'First) * 8);
         tmpByte := BitStream_PeekByte (bs, offset);
         offset  := offset + 8;
         if tmpByte /= bit_terminated_pattern (i) then
            return False;
         end if;
      end loop;
      if rest_bits > 0 then
         tmpByte := BitStream_PeekPartialByte (bs, offset, rest_bits);
         tmpByte := Shift_Left (tmpByte, 8 - rest_bits);
         if bit_terminated_pattern
             (bit_terminated_pattern'First + total_bytes) /=
           tmpByte
         then
            return False;
         end if;
      end if;
      return True;
   end BitStream_bitPatternMatches;

   pragma Warnings (Off, "formal parameter ""bs"" is not modified");

   procedure bitstrean_fetch_data_if_required (bs : in out Bitstream) is
      Current_Byte : constant Integer :=
        bs.Buffer'First + bs.Current_Bit_Pos / 8;
      Current_Bit : constant BIT_RANGE := bs.Current_Bit_Pos mod 8;
   begin
      if Current_Bit = 0 and
        Current_Byte = bs.Size_In_Bytes + 1 and
        bs.fetchDataPrm > 0
      then
         --  user_code.fetch_data(bs, bs.fetchDataPrm);
         bs.Current_Bit_Pos := 0;
      end if;
   end bitstrean_fetch_data_if_required;

   procedure bitstrean_push_data_if_required (bs : in out Bitstream) is
      Current_Byte : constant Integer :=
        bs.Buffer'First + bs.Current_Bit_Pos / 8;
      Current_Bit : constant BIT_RANGE := bs.Current_Bit_Pos mod 8;
   begin
      if Current_Bit = 0 and
        Current_Byte = bs.Size_In_Bytes + 1 and
        bs.fetchDataPrm > 0
      then
         --  user_code.push_data(bs, bs.pushDataPrm);
         bs.Current_Bit_Pos := 0;
      end if;
   end bitstrean_push_data_if_required;

   pragma Warnings (On, "formal parameter ""bs"" is not modified");

end adaasn1rtl.encoding;
