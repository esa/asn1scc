with Board_Config; use Board_Config;

package adaasn1rtl.encoding.acn with
   Spark_Mode
is

   procedure Acn_Enc_Int_PositiveInteger_ConstSize
     (bs : in out Bitstream; IntVal : Asn1UInt; sizeInBits : Integer) with
      Depends => (bs => (bs, IntVal, sizeInBits)),
      Pre     => sizeInBits >= 0 and then sizeInBits < Asn1UInt'Size
      and then IntVal <= 2**sizeInBits - 1
      and then bs.Current_Bit_Pos < Natural'Last - sizeInBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - sizeInBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + sizeInBits;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_8
     (bs : in out Bitstream; IntVal : Asn1UInt) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 8
      and then IntVal <= Asn1UInt (Asn1Byte'Last)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16
     (bs : in out Bitstream; IntVal : Asn1UInt) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 16
      and then IntVal <= Asn1UInt (Interfaces.Unsigned_16'Last)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32
     (bs : in out Bitstream; IntVal : Asn1UInt) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 32
      and then IntVal <= Asn1UInt (Interfaces.Unsigned_32'Last)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64
     (bs : in out Bitstream; IntVal : Asn1UInt) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16
     (bs : in out Bitstream; IntVal : Asn1UInt) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 16
      and then IntVal <= Asn1UInt (Interfaces.Unsigned_16'Last)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32
     (bs : in out Bitstream; IntVal : Asn1UInt) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 32
      and then IntVal <= Asn1UInt (Interfaces.Unsigned_32'Last)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64
     (bs : in out Bitstream; IntVal : Asn1UInt) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64;

   procedure Acn_Enc_Int_PositiveInteger_VarSize_LengthEmbedded
     (bs : in out Bitstream; IntVal : Asn1UInt) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);

   type Asn1Int_ARRAY_2_64 is array (2 .. 64) of Asn1Int;

   function PV (i : Integer) return Asn1Int is
     (Asn1Int (Shift_Left (Asn1UInt (1), i - 1) - 1)) with
      Pre => i >= 2 and i <= Asn1UInt'Size;

   function NV (i : Integer) return Asn1Int is (-PV (i) - 1) with
      Pre => i >= 2 and i <= Asn1UInt'Size;

--     PV : constant Asn1Int_ARRAY_2_64 :=
--       Asn1Int_ARRAY_2_64'
--         (2  => Asn1Int (1),
--          3  => Asn1Int (3),
--          4  => Asn1Int (7),
--          5  => Asn1Int (15),
--          6  => Asn1Int (31),
--          7  => Asn1Int (63),
--          8  => Asn1Int (127),
--          9  => Asn1Int (255),
--          10 => Asn1Int (511),
--          11 => Asn1Int (1023),
--          12 => Asn1Int (2047),
--          13 => Asn1Int (4095),
--          14 => Asn1Int (8191),
--          15 => Asn1Int (16383),
--          16 => Asn1Int (32767),
--          17 => Asn1Int (65535),
--          18 => Asn1Int (131071),
--          19 => Asn1Int (262143),
--          20 => Asn1Int (524287),
--          21 => Asn1Int (1048575),
--          22 => Asn1Int (2097151),
--          23 => Asn1Int (4194303),
--          24 => Asn1Int (8388607),
--          25 => Asn1Int (16777215),
--          26 => Asn1Int (33554431),
--          27 => Asn1Int (67108863),
--          28 => Asn1Int (134217727),
--          29 => Asn1Int (268435455),
--          30 => Asn1Int (536870911),
--          31 => Asn1Int (1073741823),
--          32 => Asn1Int (2147483647),
--          33 => Asn1Int (4294967295),
--          34 => Asn1Int (8589934591),
--          35 => Asn1Int (17179869183),
--          36 => Asn1Int (34359738367),
--          37 => Asn1Int (68719476735),
--          38 => Asn1Int (137438953471),
--          39 => Asn1Int (274877906943),
--          40 => Asn1Int (549755813887),
--          41 => Asn1Int (1099511627775),
--          42 => Asn1Int (2199023255551),
--          43 => Asn1Int (4398046511103),
--          44 => Asn1Int (8796093022207),
--          45 => Asn1Int (17592186044415),
--          46 => Asn1Int (35184372088831),
--          47 => Asn1Int (70368744177663),
--          48 => Asn1Int (140737488355327),
--          49 => Asn1Int (281474976710655),
--          50 => Asn1Int (562949953421311),
--          51 => Asn1Int (1125899906842623),
--          52 => Asn1Int (2251799813685247),
--          53 => Asn1Int (4503599627370495),
--          54 => Asn1Int (9007199254740991),
--          55 => Asn1Int (18014398509481983),
--          56 => Asn1Int (36028797018963967),
--          57 => Asn1Int (72057594037927935),
--          58 => Asn1Int (144115188075855871),
--          59 => Asn1Int (288230376151711743),
--          60 => Asn1Int (576460752303423487),
--          61 => Asn1Int (1152921504606846975),
--          62 => Asn1Int (2305843009213693951),
--          63 => Asn1Int (4611686018427387903),
--          64 => Asn1Int (9223372036854775807));
--     NV : constant Asn1Int_ARRAY_2_64 :=
--       Asn1Int_ARRAY_2_64'
--         (2  => Asn1Int (-2),
--          3  => Asn1Int (-4),
--          4  => Asn1Int (-8),
--          5  => Asn1Int (-16),
--          6  => Asn1Int (-32),
--          7  => Asn1Int (-64),
--          8  => Asn1Int (-128),
--          9  => Asn1Int (-256),
--          10 => Asn1Int (-512),
--          11 => Asn1Int (-1024),
--          12 => Asn1Int (-2048),
--          13 => Asn1Int (-4096),
--          14 => Asn1Int (-8192),
--          15 => Asn1Int (-16384),
--          16 => Asn1Int (-32768),
--          17 => Asn1Int (-65536),
--          18 => Asn1Int (-131072),
--          19 => Asn1Int (-262144),
--          20 => Asn1Int (-524288),
--          21 => Asn1Int (-1048576),
--          22 => Asn1Int (-2097152),
--          23 => Asn1Int (-4194304),
--          24 => Asn1Int (-8388608),
--          25 => Asn1Int (-16777216),
--          26 => Asn1Int (-33554432),
--          27 => Asn1Int (-67108864),
--          28 => Asn1Int (-134217728),
--          29 => Asn1Int (-268435456),
--          30 => Asn1Int (-536870912),
--          31 => Asn1Int (-1073741824),
--          32 => Asn1Int (-2147483648),
--          33 => Asn1Int (-4294967296),
--          34 => Asn1Int (-8589934592),
--          35 => Asn1Int (-17179869184),
--          36 => Asn1Int (-34359738368),
--          37 => Asn1Int (-68719476736),
--          38 => Asn1Int (-137438953472),
--          39 => Asn1Int (-274877906944),
--          40 => Asn1Int (-549755813888),
--          41 => Asn1Int (-1099511627776),
--          42 => Asn1Int (-2199023255552),
--          43 => Asn1Int (-4398046511104),
--          44 => Asn1Int (-8796093022208),
--          45 => Asn1Int (-17592186044416),
--          46 => Asn1Int (-35184372088832),
--          47 => Asn1Int (-70368744177664),
--          48 => Asn1Int (-140737488355328),
--          49 => Asn1Int (-281474976710656),
--          50 => Asn1Int (-562949953421312),
--          51 => Asn1Int (-1125899906842624),
--          52 => Asn1Int (-2251799813685248),
--          53 => Asn1Int (-4503599627370496),
--          54 => Asn1Int (-9007199254740992),
--          55 => Asn1Int (-18014398509481984),
--          56 => Asn1Int (-36028797018963968),
--          57 => Asn1Int (-72057594037927936),
--          58 => Asn1Int (-144115188075855872),
--          59 => Asn1Int (-288230376151711744),
--          60 => Asn1Int (-576460752303423488),
--          61 => Asn1Int (-1152921504606846976),
--          62 => Asn1Int (-2305843009213693952),
--          63 => Asn1Int (-4611686018427387904),
--          64 => Asn1Int (-9223372036854775808));

   procedure Acn_Enc_Int_TwosComplement_ConstSize
     (bs : in out Bitstream; IntVal : Asn1Int; sizeInBits : Natural) with
      Depends => (bs => (bs, IntVal, sizeInBits)),
      Pre     => sizeInBits >= 2 and then sizeInBits < Asn1UInt'Size
      and then IntVal >= NV (sizeInBits) and then IntVal <= PV (sizeInBits)
      and then bs.Current_Bit_Pos < Natural'Last - sizeInBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - sizeInBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + sizeInBits;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_8
     (bs : in out Bitstream; IntVal : Asn1Int) with
      Depends => (bs => (bs, IntVal)),
      Pre     => IntVal >= NV (8) and then IntVal <= PV (8)
      and then bs.Current_Bit_Pos < Natural'Last - 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_16
     (bs : in out Bitstream; IntVal : Asn1Int) with
      Depends => (bs => (bs, IntVal)),
      Pre     => IntVal >= NV (16) and then IntVal <= PV (16)
      and then bs.Current_Bit_Pos < Natural'Last - 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_32
     (bs : in out Bitstream; IntVal : Asn1Int) with
      Depends => (bs => (bs, IntVal)),
      Pre     => IntVal >= NV (32) and then IntVal <= PV (32)
      and then bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_64
     (bs : in out Bitstream; IntVal : Asn1Int) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_16
     (bs : in out Bitstream; IntVal : Asn1Int) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 16
      and then IntVal <= Asn1Int (Interfaces.Unsigned_16'Last)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_32
     (bs : in out Bitstream; IntVal : Asn1Int) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_64
     (bs : in out Bitstream; IntVal : Asn1Int) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64;

   procedure Acn_Enc_Int_TwosComplement_VarSize_LengthEmbedded
     (bs : in out Bitstream; IntVal : Asn1Int) with
      Depends => (bs => (bs, IntVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1Int'Size + 8)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1Int'Size + 8),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1Int'Size + 8);

   procedure Acn_Enc_Int_BCD_ConstSize
     (bs : in out Bitstream; IntVal : Asn1UInt; nNibbles : Integer) with
      Depends => (bs => (bs, IntVal, nNibbles)),
      Pre     => nNibbles >= 1 and then nNibbles < Max_Int_Digits
      and then IntVal < Powers_of_10 (nNibbles)
      and then bs.Current_Bit_Pos < Natural'Last - 4 * nNibbles
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 4 * nNibbles,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 4 * nNibbles;

   procedure Acn_Enc_Int_BCD_VarSize_NullTerminated
     (bs : in out Bitstream; IntVal : Asn1UInt) with
      Depends => (bs => (bs, IntVal)),
      Pre     => IntVal < Powers_of_10 (Max_Int_Digits - 1)
      and then bs.Current_Bit_Pos < Natural'Last - 4 * (Max_Int_Digits + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 4 * (Max_Int_Digits + 1),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 4 * (Max_Int_Digits + 1);

   procedure Acn_Enc_Int_ASCII_VarSize_LengthEmbedded
     (bs : in out Bitstream; IntVal : Asn1Int) with
      Depends => (bs => (bs, IntVal)),
      Pre     => IntVal > -Asn1Int (Powers_of_10 (Max_Int_Digits - 2))
      and then Asn1UInt (abs IntVal) < Powers_of_10 (Max_Int_Digits - 2)
      and then bs.Current_Bit_Pos < Natural'Last - 8 * (Max_Int_Digits)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 8 * (Max_Int_Digits),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8 * (Max_Int_Digits);

   procedure Acn_Enc_UInt_ASCII_VarSize_LengthEmbedded
     (bs : in out Bitstream; IntVal : Asn1UInt) with
      Depends => (bs => (bs, IntVal)),
      Pre     => IntVal < Powers_of_10 (Max_Int_Digits - 1)
      and then bs.Current_Bit_Pos < Natural'Last - 8 * (Max_Int_Digits + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 8 * (Max_Int_Digits + 1),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8 * (Max_Int_Digits + 1);

   procedure Acn_Enc_Int_ASCII_VarSize_NullTerminated
     (bs : in out Bitstream; IntVal : Asn1Int; nullChars : OctetBuffer) with
      Depends => (bs => (bs, IntVal, nullChars)),
      Pre     => nullChars'Length <= 100
      and then bs.Current_Bit_Pos <
        Natural'Last -
          8 *
            (Get_number_of_digits (abs_value (IntVal)) + nullChars'Length + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 -
          8 *
            (Get_number_of_digits (abs_value (IntVal)) + nullChars'Length + 1),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos +
          8 *
            (Get_number_of_digits (abs_value (IntVal)) + nullChars'Length + 1);

   procedure Acn_Enc_UInt_ASCII_VarSize_NullTerminated
     (bs : in out Bitstream; IntVal : Asn1UInt; nullChars : OctetBuffer) with
      Depends => (bs => (bs, IntVal, nullChars)),
      Pre     => nullChars'Length <= 100
      and then bs.Current_Bit_Pos <
        Natural'Last - 8 * (Get_number_of_digits (IntVal) + nullChars'Length)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 -
          8 * (Get_number_of_digits (IntVal) + nullChars'Length),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos +
          8 * (Get_number_of_digits (IntVal) + nullChars'Length);

   procedure Acn_Dec_Int_PositiveInteger_ConstSize
     (bs     : in out Bitstream; IntVal : out Asn1UInt; minVal : Asn1UInt;
      maxVal : Asn1UInt; nSizeInBits : Integer; Result : out ASN1_RESULT) with
      Pre => nSizeInBits >= 0 and then nSizeInBits <= Asn1UInt'Size
      and then bs.Current_Bit_Pos < Natural'Last - nSizeInBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nSizeInBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nSizeInBits and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_8
     (bs     : in out Bitstream; IntVal : out Asn1UInt; minVal : Asn1UInt;
      maxVal :        Asn1UInt; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16
     (bs     : in out Bitstream; IntVal : out Asn1UInt; minVal : Asn1UInt;
      maxVal :        Asn1UInt; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 16
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32
     (bs     : in out Bitstream; IntVal : out Asn1UInt; minVal : Asn1UInt;
      maxVal :        Asn1UInt; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64
     (bs     : in out Bitstream; IntVal : out Asn1UInt; minVal : Asn1UInt;
      maxVal :        Asn1UInt; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16
     (bs     : in out Bitstream; IntVal : out Asn1UInt; minVal : Asn1UInt;
      maxVal :        Asn1UInt; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 16
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32
     (bs     : in out Bitstream; IntVal : out Asn1UInt; minVal : Asn1UInt;
      maxVal :        Asn1UInt; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64
     (bs     : in out Bitstream; IntVal : out Asn1UInt; minVal : Asn1UInt;
      maxVal :        Asn1UInt; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));
   procedure Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded
     (bs     : in out Bitstream; IntVal : out Asn1UInt; minVal : Asn1UInt;
      Result :    out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - (8 * 8 + 8)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (8 * 8 + 8),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (8 * 8 + 8) and
      ((Result.Success and IntVal >= minVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize
     (bs     : in out Bitstream; IntVal : out Asn1Int; minVal : Asn1Int;
      maxVal : Asn1Int; nSizeInBits : Integer; Result : out ASN1_RESULT) with
      Pre => nSizeInBits >= 0 and then nSizeInBits <= Asn1UInt'Size
      and then bs.Current_Bit_Pos < Natural'Last - nSizeInBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nSizeInBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nSizeInBits and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_8
     (bs     : in out Bitstream; IntVal : out Asn1Int; minVal : Asn1Int;
      maxVal :        Asn1Int; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16
     (bs     : in out Bitstream; IntVal : out Asn1Int; minVal : Asn1Int;
      maxVal :        Asn1Int; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 16
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32
     (bs     : in out Bitstream; IntVal : out Asn1Int; minVal : Asn1Int;
      maxVal :        Asn1Int; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64
     (bs     : in out Bitstream; IntVal : out Asn1Int; minVal : Asn1Int;
      maxVal :        Asn1Int; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16
     (bs     : in out Bitstream; IntVal : out Asn1Int; minVal : Asn1Int;
      maxVal :        Asn1Int; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 16
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32
     (bs     : in out Bitstream; IntVal : out Asn1Int; minVal : Asn1Int;
      maxVal :        Asn1Int; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64
     (bs     : in out Bitstream; IntVal : out Asn1Int; minVal : Asn1Int;
      maxVal :        Asn1Int; Result : out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded
     (bs     : in out Bitstream; IntVal : out Asn1Int;
      Result :    out ASN1_RESULT) with
      Pre => bs.Current_Bit_Pos < Natural'Last - (Asn1Int'Size + 8)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1Int'Size + 8),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1Int'Size + 8);

   procedure Acn_Dec_Int_BCD_ConstSize
     (bs     : in out Bitstream; IntVal : out Asn1UInt; minVal : Asn1UInt;
      maxVal :    Asn1UInt; nNibbles : Integer; Result : out ASN1_RESULT) with
      Pre => nNibbles >= 1 and then nNibbles <= Max_Int_Digits - 1
      and then minVal <= maxVal and then maxVal <= Powers_of_10 (nNibbles) - 1
      and then bs.Current_Bit_Pos < Natural'Last - 4 * nNibbles
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 4 * nNibbles,
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 4 * nNibbles and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_BCD_VarSize_NullTerminated
     (bs     : in out Bitstream; IntVal : out Asn1UInt; minVal : Asn1UInt;
      maxVal :        Asn1UInt; Result : out ASN1_RESULT) with
      Pre => minVal <= maxVal
      and then maxVal <= Powers_of_10 (Max_Int_Digits - 1) - 1
      and then bs.Current_Bit_Pos < Natural'Last - 4 * (Max_Int_Digits)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 4 * (Max_Int_Digits),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 4 * (Max_Int_Digits) and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Enc_Int_ASCII_ConstSize
     (bs : in out Bitstream; IntVal : Asn1Int; nChars : Integer) with
      Depends => (bs => (bs, IntVal, nChars)),
      Pre     => nChars >= Get_number_of_digits (abs_value (IntVal)) + 1
      and then nChars <= 50
      and then bs.Current_Bit_Pos < Natural'Last - 8 * (nChars)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8 * (nChars),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8 * (nChars);

   procedure Acn_Dec_Int_ASCII_ConstSize
     (bs     : in out Bitstream; IntVal : out Asn1Int; minVal : Asn1Int;
      maxVal :        Asn1Int; nChars : Integer; Result : out ASN1_RESULT) with
      Pre => nChars >= 2 and then nChars <= Max_Int_Digits
      and then minVal <= maxVal
      and then bs.Current_Bit_Pos < Natural'Last - 8 * (nChars)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8 * (nChars),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8 * (nChars) and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Enc_UInt_ASCII_ConstSize
     (bs : in out Bitstream; IntVal : Asn1UInt; nChars : Integer) with
      Depends => (bs => (bs, IntVal, nChars)),
      Pre     => nChars >= 1 and then nChars <= Max_Int_Digits - 1
      and then IntVal < Powers_of_10 (nChars)
      and then bs.Current_Bit_Pos < Natural'Last - 8 * (nChars)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8 * (nChars),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8 * (nChars);

   procedure Acn_Dec_UInt_ASCII_ConstSize
     (bs     : in out Bitstream; IntVal : out Asn1UInt; minVal : Asn1UInt;
      maxVal :    Asn1UInt; nChars : Integer; Result : out ASN1_RESULT) with
      Pre => nChars >= 2 and then nChars <= 20 and then minVal <= maxVal
      and then maxVal < Powers_of_10 (Max_Int_Digits - 1)
      and then bs.Current_Bit_Pos < Natural'Last - 8 * (nChars)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8 * (nChars),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8 * (nChars) and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_UInt_ASCII_VarSize_NullTerminated
     (bs : in out Bitstream; IntVal : out Asn1UInt; nullChars : OctetBuffer;
      Result :    out ASN1_RESULT) with
      Pre => nullChars'Length >= 1 and then nullChars'Length <= 10
      and then nullChars'First = 1
      and then bs.Current_Bit_Pos <
        Natural'Last - 8 * (Max_Int_Digits + nullChars'Length)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 8 * (Max_Int_Digits + nullChars'Length),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos + 8 * (Max_Int_Digits + nullChars'Length);

   procedure Acn_Dec_Int_ASCII_VarSize_NullTerminated
     (bs     : in out Bitstream; IntVal : out Asn1Int; nullChars : OctetBuffer;
      Result :    out ASN1_RESULT) with
      Pre => nullChars'Length >= 1 and then nullChars'Length < 10
      and then nullChars'First = 1
      and then bs.Current_Bit_Pos <
        Natural'Last - 8 * (Max_Int_Digits + nullChars'Length + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 8 * (Max_Int_Digits + nullChars'Length + 1),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos + 8 * (Max_Int_Digits + nullChars'Length + 1);

   procedure Acn_Enc_Real_IEEE754_32_big_endian
     (bs : in out Bitstream; RealVal : Asn1Real) with
      Depends => (bs => (bs, RealVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Dec_Real_IEEE754_32_big_endian
     (bs     : in out Bitstream; RealVal : out Asn1Real;
      Result :    out ASN1_RESULT) with
      Depends => ((Result, bs, RealVal) => bs),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => Result.Success and
      bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Dec_Real_IEEE754_32_big_endian_fp32
     (bs     : in out Bitstream; RealVal : out Standard.Float;
      Result :    out ASN1_RESULT) with
      Depends => ((Result, bs, RealVal) => bs),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Enc_Real_IEEE754_64_big_endian
     (bs : in out Bitstream; RealVal : Asn1Real) with
      Depends => (bs => (bs, RealVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64;

   procedure Acn_Dec_Real_IEEE754_64_big_endian
     (bs     : in out Bitstream; RealVal : out Asn1Real;
      Result :    out ASN1_RESULT) with
      Depends => ((Result, bs, RealVal) => bs),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => Result.Success and
      bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64;

   procedure Acn_Enc_Real_IEEE754_32_little_endian
     (bs : in out Bitstream; RealVal : Asn1Real) with
      Depends => (bs => (bs, RealVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Dec_Real_IEEE754_32_little_endian
     (bs     : in out Bitstream; RealVal : out Asn1Real;
      Result :    out ASN1_RESULT) with
      Depends => ((Result, bs, RealVal) => bs),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => Result.Success and
      bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Dec_Real_IEEE754_32_little_endian_fp32
     (bs     : in out Bitstream; RealVal : out Standard.Float;
      Result :    out ASN1_RESULT) with
      Depends => ((Result, bs, RealVal) => bs),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Enc_Real_IEEE754_64_little_endian
     (bs : in out Bitstream; RealVal : Asn1Real) with
      Depends => (bs => (bs, RealVal)),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64;

   procedure Acn_Dec_Real_IEEE754_64_little_endian
     (bs     : in out Bitstream; RealVal : out Asn1Real;
      Result :    out ASN1_RESULT) with
      Depends => ((Result, bs, RealVal) => bs),
      Pre     => bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => Result.Success and
      bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64;

   procedure Acn_Enc_Boolean_true_pattern
     (bs : in out Bitstream; BoolVal : Asn1Boolean; pattern : BitArray) with
      Depends => (bs => (bs, BoolVal, pattern)),
      Pre     => pattern'Last >= pattern'First
      and then pattern'Last - pattern'First < Natural'Last
      and then bs.Current_Bit_Pos <
        Natural'Last - (pattern'Last - pattern'First + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (pattern'Last - pattern'First + 1),
      Post => bs.Current_Bit_Pos =
      bs'Old.Current_Bit_Pos + (pattern'Last - pattern'First + 1);

   procedure Acn_Dec_Boolean_true_pattern
     (bs     : in out Bitstream; BoolVal : out Asn1Boolean; pattern : BitArray;
      Result :    out ASN1_RESULT) with
      Pre => pattern'Last >= pattern'First
      and then pattern'Last - pattern'First < Natural'Last
      and then bs.Current_Bit_Pos <
        Natural'Last - (pattern'Last - pattern'First + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (pattern'Last - pattern'First + 1),
      Post => Result.Success and
      bs.Current_Bit_Pos =
        bs'Old.Current_Bit_Pos + (pattern'Last - pattern'First + 1);

   procedure Acn_Enc_Boolean_false_pattern
     (bs : in out Bitstream; BoolVal : Asn1Boolean; pattern : BitArray) with
      Depends => (bs => (bs, BoolVal, pattern)),
      Pre     => pattern'Last >= pattern'First
      and then pattern'Last - pattern'First < Natural'Last
      and then bs.Current_Bit_Pos <
        Natural'Last - (pattern'Last - pattern'First + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (pattern'Last - pattern'First + 1),
      Post => bs.Current_Bit_Pos =
      bs'Old.Current_Bit_Pos + (pattern'Last - pattern'First + 1);

   procedure Acn_Dec_Boolean_false_pattern
     (bs     : in out Bitstream; BoolVal : out Asn1Boolean; pattern : BitArray;
      Result :    out ASN1_RESULT) with
      Depends => ((bs, BoolVal, Result) => (bs, pattern)),
      Pre     => pattern'Last >= pattern'First
      and then pattern'Last - pattern'First < Natural'Last
      and then bs.Current_Bit_Pos <
        Natural'Last - (pattern'Last - pattern'First + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (pattern'Last - pattern'First + 1),
      Post => Result.Success and
      bs.Current_Bit_Pos =
        bs'Old.Current_Bit_Pos + (pattern'Last - pattern'First + 1);

   procedure Acn_Enc_NullType_pattern
     (bs : in out Bitstream; encVal : Asn1NullType; pattern : BitArray) with
      Pre => pattern'Last >= pattern'First
      and then pattern'Last - pattern'First < Natural'Last
      and then bs.Current_Bit_Pos <
        Natural'Last - (pattern'Last - pattern'First + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (pattern'Last - pattern'First + 1),
      Post => bs.Current_Bit_Pos =
      bs'Old.Current_Bit_Pos + (pattern'Last - pattern'First + 1);

   procedure Acn_Dec_NullType_pattern
     (bs : in out Bitstream; decValue : out Asn1NullType; pattern : BitArray;
      Result :    out ASN1_RESULT) with
      Depends => ((bs, decValue, Result) => (bs, pattern)),
      Pre     => pattern'Last >= pattern'First
      and then pattern'Last - pattern'First < Natural'Last
      and then bs.Current_Bit_Pos <
        Natural'Last - (pattern'Last - pattern'First + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (pattern'Last - pattern'First + 1),
      Post => bs.Current_Bit_Pos =
      bs'Old.Current_Bit_Pos + (pattern'Last - pattern'First + 1);

   procedure Acn_Enc_NullType_pattern2
     (bs : in out Bitstream; pattern : BitArray) with
      Depends => (bs => (bs, pattern)),
      Pre     => pattern'Last >= pattern'First
      and then pattern'Last - pattern'First < Natural'Last
      and then bs.Current_Bit_Pos <
        Natural'Last - (pattern'Last - pattern'First + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (pattern'Last - pattern'First + 1),
      Post => bs.Current_Bit_Pos =
      bs'Old.Current_Bit_Pos + (pattern'Last - pattern'First + 1);

   procedure Acn_Dec_NullType_pattern2
     (bs : in out Bitstream; pattern : BitArray; Result : out ASN1_RESULT) with
      Depends => ((bs, Result) => (bs, pattern)),
      Pre     => pattern'Last >= pattern'First
      and then pattern'Last - pattern'First < Natural'Last
      and then bs.Current_Bit_Pos <
        Natural'Last - (pattern'Last - pattern'First + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (pattern'Last - pattern'First + 1),
      Post => bs.Current_Bit_Pos =
      bs'Old.Current_Bit_Pos + (pattern'Last - pattern'First + 1);

   procedure Acn_Enc_NullType
     (bs : in out Bitstream; encVal : Asn1NullType) with
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos;

   procedure Acn_Dec_NullType
     (bs     : in out Bitstream; decValue : out Asn1NullType;
      Result :    out ASN1_RESULT) with
      Post => Result.Success and decValue = 0 and
      bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos;

   procedure Acn_Enc_String_Ascii_FixSize
     (bs : in out Bitstream; strVal : String) with
      Depends => (bs => (bs, strVal)),
      Pre     => strVal'Last >= strVal'First
      and then strVal'Last - strVal'First < Natural'Last / 8 - 8
      and then bs.Current_Bit_Pos <
        Natural'Last - (strVal'Last - strVal'First) * 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (strVal'Last - strVal'First) * 8,
      Post => bs.Current_Bit_Pos =
      bs'Old.Current_Bit_Pos + (strVal'Last - strVal'First) * 8;

   procedure Acn_Dec_String_Ascii_FixSize
     (bs     : in out Bitstream; strVal : in out String;
      Result :    out ASN1_RESULT) with
      Pre => strVal'Last >= strVal'First
      and then strVal'Last - strVal'First < Natural'Last / 8 - 8
      and then bs.Current_Bit_Pos <
        Natural'Last - (strVal'Last - strVal'First) * 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - (strVal'Last - strVal'First) * 8,
      Post => Result.Success and
      bs.Current_Bit_Pos =
        bs'Old.Current_Bit_Pos + (strVal'Last - strVal'First) * 8;

   procedure Acn_Enc_String_Ascii_Null_Teminated
     (bs     : in out Bitstream; null_characters : OctetBuffer;
      strVal :        String) with
      Depends => (bs => (bs, strVal, null_characters)),
      Pre => null_characters'Length >= 1 and then null_characters'Length <= 10
      and then null_characters'First = 1 and then strVal'Last >= strVal'First
      and then strVal'Last - strVal'First <
        Natural'Last / 8 - null_characters'Length
      and then bs.Current_Bit_Pos <
        Natural'Last - (strVal'Length - 1 + null_characters'Length) * 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 -
          (strVal'Length - 1 + null_characters'Length) * 8,
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos +
          (strVal'Length - 1 + null_characters'Length) * 8;

   procedure Acn_Dec_String_Ascii_Null_Teminated
     (bs     : in out Bitstream; null_characters : OctetBuffer;
      strVal : in out String; Result : out ASN1_RESULT) with
      Pre => null_characters'Length >= 1 and then null_characters'Length <= 10
      and then null_characters'First = 1 and then strVal'Last < Natural'Last
      and then strVal'Last >= strVal'First
      and then strVal'Length < Natural'Last / 8 - null_characters'Length
      and then bs.Current_Bit_Pos <
        Natural'Last - (strVal'Length - 1 + null_characters'Length) * 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 -
          (strVal'Length - 1 + null_characters'Length) * 8,
      Post => Result.Success and
      bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos +
          (strVal'Length - 1 + null_characters'Length) * 8;

   procedure Acn_Enc_String_Ascii_Internal_Field_Determinant
     (bs                           : in out Bitstream; asn1Min : Asn1Int;
      nLengthDeterminantSizeInBits :        Integer; strVal : String) with
      Depends => (bs => (bs, asn1Min, strVal, nLengthDeterminantSizeInBits)),
      Pre     => nLengthDeterminantSizeInBits >= 0
      and then nLengthDeterminantSizeInBits < Asn1UInt'Size
      and then asn1Min >= 0 and then strVal'Last < Natural'Last
      and then strVal'Last >= strVal'First
      and then strVal'Last - strVal'First < Natural'Last / 8 - 8
      and then asn1Min <= Asn1Int (getStringSize (strVal))
      and then bs.Current_Bit_Pos <
        Natural'Last -
          ((strVal'Last - strVal'First) * 8 + nLengthDeterminantSizeInBits)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 -
          ((strVal'Last - strVal'First) * 8 + nLengthDeterminantSizeInBits),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos +
          ((strVal'Last - strVal'First) * 8 + nLengthDeterminantSizeInBits);

   procedure Acn_Dec_String_Ascii_Internal_Field_Determinant
     (bs : in out Bitstream; asn1Min : Asn1Int; asn1Max : Asn1Int;
      nLengthDeterminantSizeInBits :        Integer; strVal : in out String;
      Result                       :    out ASN1_RESULT) with
      Pre => asn1Min >= 0 and then asn1Max <= Asn1Int (Integer'Last)
      and then asn1Min <= asn1Max and then nLengthDeterminantSizeInBits >= 0
      and then nLengthDeterminantSizeInBits < Asn1UInt'Size
      and then strVal'Last < Natural'Last and then strVal'Last >= strVal'First
      and then strVal'Last - strVal'First < Natural'Last / 8 - 8
      and then bs.Current_Bit_Pos <
        Natural'Last -
          ((strVal'Last - strVal'First) * 8 + nLengthDeterminantSizeInBits)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 -
          ((strVal'Last - strVal'First) * 8 + nLengthDeterminantSizeInBits),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos +
          ((strVal'Last - strVal'First) * 8 + nLengthDeterminantSizeInBits);

   procedure Acn_Enc_String_Ascii_External_Field_Determinant
     (bs : in out Bitstream; strVal : String) with
      Depends => (bs => (bs, strVal)),
      Pre => strVal'Last < Natural'Last and then strVal'Last >= strVal'First
      and then strVal'Last - strVal'First < Natural'Last / 8 - 8
      and then bs.Current_Bit_Pos <
        Natural'Last - ((strVal'Last - strVal'First) * 8)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - ((strVal'Last - strVal'First) * 8),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos + ((strVal'Last - strVal'First) * 8);

   procedure Acn_Dec_String_Ascii_External_Field_Determinant
     (bs     : in out Bitstream; extSizeDeterminatFld : Asn1Int;
      strVal : in out String; Result : out ASN1_RESULT) with
      Pre => extSizeDeterminatFld >= 0
      and then extSizeDeterminatFld <= Asn1Int (Integer'Last)
      and then strVal'Last < Natural'Last and then strVal'Last >= strVal'First
      and then strVal'Last - strVal'First < Natural'Last / 8 - 8
      and then bs.Current_Bit_Pos <
        Natural'Last - ((strVal'Last - strVal'First) * 8)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - ((strVal'Last - strVal'First) * 8),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos + ((strVal'Last - strVal'First) * 8);

   procedure Acn_Enc_String_CharIndex_External_Field_Determinant
     (bs     : in out Bitstream; charSet : String; nCharSize : Integer;
      strVal :        String) with
      Depends => (bs => (bs, strVal, nCharSize, charSet)),
      Pre     => nCharSize >= 1 and then nCharSize <= 8
      and then strVal'Last < Natural'Last and then strVal'Last >= strVal'First
      and then charSet'Last < Natural'Last
      and then charSet'Last >= charSet'First
      and then charSet'Last - charSet'First <= 255
      and then strVal'Last - strVal'First < Natural'Last / 8 - nCharSize
      and then bs.Current_Bit_Pos <
        Natural'Last - ((strVal'Last - strVal'First) * nCharSize)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - ((strVal'Last - strVal'First) * nCharSize),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos + ((strVal'Last - strVal'First) * nCharSize);

   procedure Acn_Dec_String_CharIndex_External_Field_Determinant
     (bs : in out Bitstream; charSet : String; nCharSize : Integer;
      extSizeDeterminatFld :        Asn1Int; strVal : out String;
      Result               :    out ASN1_RESULT) with
      Pre => extSizeDeterminatFld >= 0
      and then extSizeDeterminatFld <= Asn1Int (Integer'Last)
      and then nCharSize >= 1 and then nCharSize <= 8
      and then strVal'Last < Natural'Last and then strVal'Last >= strVal'First
      and then charSet'Last < Natural'Last
      and then charSet'Last >= charSet'First
      and then charSet'Last - charSet'First <= 255
      and then strVal'Last - strVal'First < Natural'Last / 8 - nCharSize
      and then bs.Current_Bit_Pos <
        Natural'Last - ((strVal'Last - strVal'First) * nCharSize)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - ((strVal'Last - strVal'First) * nCharSize),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos + ((strVal'Last - strVal'First) * nCharSize);

   procedure Acn_Enc_String_CharIndex_Internal_Field_Determinant
     (bs      : in out Bitstream; charSet : String; nCharSize : Integer;
      asn1Min :        Asn1Int; nLengthDeterminantSizeInBits : Integer;
      strVal  :        String) with
      Pre => nLengthDeterminantSizeInBits >= 0
      and then nLengthDeterminantSizeInBits < Asn1UInt'Size
      and then asn1Min >= 0 and then nCharSize >= 1 and then nCharSize <= 8
      and then strVal'Last < Natural'Last and then strVal'Last >= strVal'First
      and then charSet'Last < Natural'Last
      and then charSet'Last >= charSet'First
      and then charSet'Last - charSet'First <= 255
      and then strVal'Last - strVal'First < Natural'Last / 8 - nCharSize
      and then asn1Min <= Asn1Int (getStringSize (strVal))
      and then bs.Current_Bit_Pos <
        Natural'Last -
          ((strVal'Last - strVal'First) * nCharSize +
           nLengthDeterminantSizeInBits)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 -
          ((strVal'Last - strVal'First) * nCharSize +
           nLengthDeterminantSizeInBits),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos +
          ((strVal'Last - strVal'First) * nCharSize +
           nLengthDeterminantSizeInBits);

   procedure Acn_Dec_String_CharIndex_Internal_Field_Determinant
     (bs : in out Bitstream; charSet : String; nCharSize : Integer;
      asn1Min                      :        Asn1Int; asn1Max : Asn1Int;
      nLengthDeterminantSizeInBits :        Integer; strVal : in out String;
      Result                       :    out ASN1_RESULT) with
      Pre => asn1Min >= 0 and then asn1Max <= Asn1Int (Integer'Last)
      and then asn1Min <= asn1Max and then nLengthDeterminantSizeInBits >= 0
      and then nLengthDeterminantSizeInBits < Asn1UInt'Size
      and then nCharSize >= 1 and then nCharSize <= 8
      and then strVal'Last < Natural'Last and then strVal'Last >= strVal'First
      and then charSet'Last < Natural'Last
      and then charSet'Last >= charSet'First
      and then charSet'Last - charSet'First <= 255
      and then asn1Max < Asn1Int (Integer'Last) - Asn1Int (charSet'First)
      and then strVal'Last - strVal'First < Natural'Last / 8 - nCharSize
      and then bs.Current_Bit_Pos <
        Natural'Last -
          ((strVal'Last - strVal'First) * nCharSize +
           nLengthDeterminantSizeInBits)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 -
          ((strVal'Last - strVal'First) * nCharSize +
           nLengthDeterminantSizeInBits),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos +
          ((strVal'Last - strVal'First) * nCharSize +
           nLengthDeterminantSizeInBits);

   procedure Acn_Dec_Int_PositiveInteger_ConstSizeUInt8
     (bs : in out Bitstream; IntVal : out Unsigned_8; MinVal : Unsigned_8;
      MaxVal : Unsigned_8; nBits : Integer; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal, nBits)),
      Pre     => MinVal <= MaxVal and then nBits >= 0
      and then nBits <= Asn1UInt'Size
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSizeUInt16
     (bs : in out Bitstream; IntVal : out Unsigned_16; MinVal : Unsigned_16;
      MaxVal : Unsigned_16; nBits : Integer; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal, nBits)),
      Pre     => MinVal <= MaxVal and then nBits >= 0
      and then nBits <= Asn1UInt'Size
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSizeUInt32
     (bs : in out Bitstream; IntVal : out Unsigned_32; MinVal : Unsigned_32;
      MaxVal : Unsigned_32; nBits : Integer; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal, nBits)),
      Pre     => MinVal <= MaxVal and then nBits >= 0
      and then nBits <= Asn1UInt'Size
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSizeInt8
     (bs : in out Bitstream; IntVal : out Integer_8; MinVal : Integer_8;
      MaxVal : Integer_8; nBits : Integer; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal, nBits)),
      Pre     => MinVal <= MaxVal and then nBits >= 0
      and then nBits <= Asn1Int'Size
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSizeInt16
     (bs : in out Bitstream; IntVal : out Integer_16; MinVal : Integer_16;
      MaxVal : Integer_16; nBits : Integer; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal, nBits)),
      Pre     => MinVal <= MaxVal and then nBits >= 0
      and then nBits <= Asn1Int'Size
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSizeInt32
     (bs : in out Bitstream; IntVal : out Integer_32; MinVal : Integer_32;
      MaxVal : Integer_32; nBits : Integer; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal, nBits)),
      Pre     => MinVal <= MaxVal and then nBits >= 0
      and then nBits <= Asn1Int'Size
      and then bs.Current_Bit_Pos < Natural'Last - nBits
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_8UInt8
     (bs : in out Bitstream; IntVal : out Unsigned_8; MinVal : Unsigned_8;
      MaxVal : Unsigned_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16UInt16
     (bs : in out Bitstream; IntVal : out Unsigned_16; MinVal : Unsigned_16;
      MaxVal : Unsigned_16; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 16
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16UInt8
     (bs : in out Bitstream; IntVal : out Unsigned_8; MinVal : Unsigned_8;
      MaxVal : Unsigned_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 16
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt32
     (bs : in out Bitstream; IntVal : out Unsigned_32; MinVal : Unsigned_32;
      MaxVal : Unsigned_32; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt16
     (bs : in out Bitstream; IntVal : out Unsigned_16; MinVal : Unsigned_16;
      MaxVal : Unsigned_16; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt8
     (bs : in out Bitstream; IntVal : out Unsigned_8; MinVal : Unsigned_8;
      MaxVal : Unsigned_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt32
     (bs : in out Bitstream; IntVal : out Unsigned_32; MinVal : Unsigned_32;
      MaxVal : Unsigned_32; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt16
     (bs : in out Bitstream; IntVal : out Unsigned_16; MinVal : Unsigned_16;
      MaxVal : Unsigned_16; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt8
     (bs : in out Bitstream; IntVal : out Unsigned_8; MinVal : Unsigned_8;
      MaxVal : Unsigned_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16UInt16
     (bs : in out Bitstream; IntVal : out Unsigned_16; MinVal : Unsigned_16;
      MaxVal : Unsigned_16; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 16
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16UInt8
     (bs : in out Bitstream; IntVal : out Unsigned_8; MinVal : Unsigned_8;
      MaxVal : Unsigned_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 16
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt32
     (bs : in out Bitstream; IntVal : out Unsigned_32; MinVal : Unsigned_32;
      MaxVal : Unsigned_32; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt16
     (bs : in out Bitstream; IntVal : out Unsigned_16; MinVal : Unsigned_16;
      MaxVal : Unsigned_16; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt8
     (bs : in out Bitstream; IntVal : out Unsigned_8; MinVal : Unsigned_8;
      MaxVal : Unsigned_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt32
     (bs : in out Bitstream; IntVal : out Unsigned_32; MinVal : Unsigned_32;
      MaxVal : Unsigned_32; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt16
     (bs : in out Bitstream; IntVal : out Unsigned_16; MinVal : Unsigned_16;
      MaxVal : Unsigned_16; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt8
     (bs : in out Bitstream; IntVal : out Unsigned_8; MinVal : Unsigned_8;
      MaxVal : Unsigned_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_8Int8
     (bs : in out Bitstream; IntVal : out Integer_8; MinVal : Integer_8;
      MaxVal : Integer_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 8
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16Int16
     (bs : in out Bitstream; IntVal : out Integer_16; MinVal : Integer_16;
      MaxVal : Integer_16; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 16
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16Int8
     (bs : in out Bitstream; IntVal : out Integer_8; MinVal : Integer_8;
      MaxVal : Integer_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 16
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int32
     (bs : in out Bitstream; IntVal : out Integer_32; MinVal : Integer_32;
      MaxVal : Integer_32; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int16
     (bs : in out Bitstream; IntVal : out Integer_16; MinVal : Integer_16;
      MaxVal : Integer_16; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int8
     (bs : in out Bitstream; IntVal : out Integer_8; MinVal : Integer_8;
      MaxVal : Integer_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int32
     (bs : in out Bitstream; IntVal : out Integer_32; MinVal : Integer_32;
      MaxVal : Integer_32; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int16
     (bs : in out Bitstream; IntVal : out Integer_16; MinVal : Integer_16;
      MaxVal : Integer_16; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int8
     (bs : in out Bitstream; IntVal : out Integer_8; MinVal : Integer_8;
      MaxVal : Integer_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16Int16
     (bs : in out Bitstream; IntVal : out Integer_16; MinVal : Integer_16;
      MaxVal : Integer_16; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 16
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16Int8
     (bs : in out Bitstream; IntVal : out Integer_8; MinVal : Integer_8;
      MaxVal : Integer_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 16
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int32
     (bs : in out Bitstream; IntVal : out Integer_32; MinVal : Integer_32;
      MaxVal : Integer_32; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int16
     (bs : in out Bitstream; IntVal : out Integer_16; MinVal : Integer_16;
      MaxVal : Integer_16; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int8
     (bs : in out Bitstream; IntVal : out Integer_8; MinVal : Integer_8;
      MaxVal : Integer_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 32
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 32,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int32
     (bs : in out Bitstream; IntVal : out Integer_32; MinVal : Integer_32;
      MaxVal : Integer_32; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int16
     (bs : in out Bitstream; IntVal : out Integer_16; MinVal : Integer_16;
      MaxVal : Integer_16; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
       (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int8
     (bs : in out Bitstream; IntVal : out Integer_8; MinVal : Integer_8;
      MaxVal : Integer_8; Result : out ASN1_RESULT) with
      Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal)),
      Pre     => MinVal <= MaxVal
      and then bs.Current_Bit_Pos < Natural'Last - 64
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 64,
      Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64 and
      ((Result.Success and (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
         (not Result.Success and (IntVal = MinVal)));

   procedure Acn_Dec_Int_BCD_ConstSizeUInt8
     (bs : in out Bitstream; IntVal : out Unsigned_8; minVal : Unsigned_8;
      maxVal : Unsigned_8; nNibbles : Integer; Result : out ASN1_RESULT) with
      Pre => nNibbles >= 1 and then nNibbles <= Max_Int_Digits - 1
      and then minVal <= maxVal
      and then Asn1UInt (maxVal) <= Powers_of_10 (nNibbles) - 1
      and then bs.Current_Bit_Pos < Natural'Last - 4 * nNibbles
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 4 * nNibbles,
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 4 * nNibbles and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_BCD_ConstSizeUInt16
     (bs : in out Bitstream; IntVal : out Unsigned_16; minVal : Unsigned_16;
      maxVal : Unsigned_16; nNibbles : Integer; Result : out ASN1_RESULT) with
      Pre => nNibbles >= 1 and then nNibbles <= Max_Int_Digits - 1
      and then minVal <= maxVal
      and then Asn1UInt (maxVal) <= Powers_of_10 (nNibbles) - 1
      and then bs.Current_Bit_Pos < Natural'Last - 4 * nNibbles
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 4 * nNibbles,
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 4 * nNibbles and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_BCD_ConstSizeUInt32
     (bs : in out Bitstream; IntVal : out Unsigned_32; minVal : Unsigned_32;
      maxVal : Unsigned_32; nNibbles : Integer; Result : out ASN1_RESULT) with
      Pre => nNibbles >= 1 and then nNibbles <= Max_Int_Digits - 1
      and then minVal <= maxVal
      and then Asn1UInt (maxVal) <= Powers_of_10 (nNibbles) - 1
      and then bs.Current_Bit_Pos < Natural'Last - 4 * nNibbles
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 4 * nNibbles,
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 4 * nNibbles and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt8
     (bs : in out Bitstream; IntVal : out Unsigned_8; minVal : Unsigned_8;
      maxVal : Unsigned_8; Result : out ASN1_RESULT) with
      Pre => minVal <= maxVal
      and then Asn1UInt (maxVal) <= Powers_of_10 (Max_Int_Digits - 1) - 1
      and then bs.Current_Bit_Pos < Natural'Last - 4 * (Max_Int_Digits)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 4 * (Max_Int_Digits),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 4 * (Max_Int_Digits) and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt16
     (bs : in out Bitstream; IntVal : out Unsigned_16; minVal : Unsigned_16;
      maxVal : Unsigned_16; Result : out ASN1_RESULT) with
      Pre => minVal <= maxVal
      and then Asn1UInt (maxVal) <= Powers_of_10 (Max_Int_Digits - 1) - 1
      and then bs.Current_Bit_Pos < Natural'Last - 4 * (Max_Int_Digits)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 4 * (Max_Int_Digits),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 4 * (Max_Int_Digits) and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt32
     (bs : in out Bitstream; IntVal : out Unsigned_32; minVal : Unsigned_32;
      maxVal : Unsigned_32; Result : out ASN1_RESULT) with
      Pre => minVal <= maxVal
      and then Asn1UInt (maxVal) <= Powers_of_10 (Max_Int_Digits - 1) - 1
      and then bs.Current_Bit_Pos < Natural'Last - 4 * (Max_Int_Digits)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 4 * (Max_Int_Digits),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 4 * (Max_Int_Digits) and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
           (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_ASCII_ConstSizeInt8
     (bs : in out Bitstream; IntVal : out Integer_8; minVal : Integer_8;
      maxVal : Integer_8; nChars : Integer; Result : out ASN1_RESULT) with
      Pre => nChars >= 2 and then nChars <= Max_Int_Digits
      and then minVal <= maxVal
      and then bs.Current_Bit_Pos < Natural'Last - 8 * (nChars)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8 * (nChars),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8 * (nChars) and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_ASCII_ConstSizeInt16
     (bs : in out Bitstream; IntVal : out Integer_16; minVal : Integer_16;
      maxVal : Integer_16; nChars : Integer; Result : out ASN1_RESULT) with
      Pre => nChars >= 2 and then nChars <= Max_Int_Digits
      and then minVal <= maxVal
      and then bs.Current_Bit_Pos < Natural'Last - 8 * (nChars)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8 * (nChars),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8 * (nChars) and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_ASCII_ConstSizeInt32
     (bs : in out Bitstream; IntVal : out Integer_32; minVal : Integer_32;
      maxVal : Integer_32; nChars : Integer; Result : out ASN1_RESULT) with
      Pre => nChars >= 2 and then nChars <= Max_Int_Digits
      and then minVal <= maxVal
      and then bs.Current_Bit_Pos < Natural'Last - 8 * (nChars)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8 * (nChars),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8 * (nChars) and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
           (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_UInt_ASCII_ConstSizeUInt8
     (bs : in out Bitstream; IntVal : out Unsigned_8; minVal : Unsigned_8;
      maxVal : Unsigned_8; nChars : Integer; Result : out ASN1_RESULT) with
      Pre => nChars >= 2 and then nChars <= 20 and then minVal <= maxVal
      and then Asn1UInt (maxVal) < Powers_of_10 (Max_Int_Digits - 1)
      and then bs.Current_Bit_Pos < Natural'Last - 8 * (nChars)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8 * (nChars),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8 * (nChars) and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_UInt_ASCII_ConstSizeUInt16
     (bs : in out Bitstream; IntVal : out Unsigned_16; minVal : Unsigned_16;
      maxVal : Unsigned_16; nChars : Integer; Result : out ASN1_RESULT) with
      Pre => nChars >= 2 and then nChars <= 20 and then minVal <= maxVal
      and then Asn1UInt (maxVal) < Powers_of_10 (Max_Int_Digits - 1)
      and then bs.Current_Bit_Pos < Natural'Last - 8 * (nChars)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8 * (nChars),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8 * (nChars) and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_UInt_ASCII_ConstSizeUInt32
     (bs : in out Bitstream; IntVal : out Unsigned_32; minVal : Unsigned_32;
      maxVal : Unsigned_32; nChars : Integer; Result : out ASN1_RESULT) with
      Pre => nChars >= 2 and then nChars <= 20 and then minVal <= maxVal
      and then Asn1UInt (maxVal) < Powers_of_10 (Max_Int_Digits - 1)
      and then bs.Current_Bit_Pos < Natural'Last - 8 * (nChars)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8 * (nChars),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8 * (nChars) and
      ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal));

   procedure Acn_Dec_Int_ASCII_VarSize_NullTerminatedInt8
     (bs : in out Bitstream; IntVal : out Integer_8;
      nullChars : OctetBuffer; Result :    out ASN1_RESULT) with
      Pre => nullChars'Length >= 1 and then nullChars'Length < 10
      and then nullChars'First = 1
      and then bs.Current_Bit_Pos <
        Natural'Last - 8 * (Max_Int_Digits + nullChars'Length + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 8 * (Max_Int_Digits + nullChars'Length + 1),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos + 8 * (Max_Int_Digits + nullChars'Length + 1);

   procedure Acn_Dec_Int_ASCII_VarSize_NullTerminatedInt16
     (bs : in out Bitstream; IntVal : out Integer_16;
      nullChars : OctetBuffer; Result :    out ASN1_RESULT) with
      Pre => nullChars'Length >= 1 and then nullChars'Length < 10
      and then nullChars'First = 1
      and then bs.Current_Bit_Pos <
        Natural'Last - 8 * (Max_Int_Digits + nullChars'Length + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 8 * (Max_Int_Digits + nullChars'Length + 1),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos + 8 * (Max_Int_Digits + nullChars'Length + 1);

   procedure Acn_Dec_Int_ASCII_VarSize_NullTerminatedInt32
     (bs : in out Bitstream; IntVal : out Integer_32;
      nullChars : OctetBuffer; Result :    out ASN1_RESULT) with
      Pre => nullChars'Length >= 1 and then nullChars'Length < 10
      and then nullChars'First = 1
      and then bs.Current_Bit_Pos <
        Natural'Last - 8 * (Max_Int_Digits + nullChars'Length + 1)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 8 * (Max_Int_Digits + nullChars'Length + 1),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos + 8 * (Max_Int_Digits + nullChars'Length + 1);

   procedure Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt8
     (bs : in out Bitstream; IntVal : out Unsigned_8;
     nullChars : OctetBuffer; Result :    out ASN1_RESULT) with
      Pre => nullChars'Length >= 1 and then nullChars'Length <= 10
      and then nullChars'First = 1
      and then bs.Current_Bit_Pos <
        Natural'Last - 8 * (Max_Int_Digits + nullChars'Length)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 8 * (Max_Int_Digits + nullChars'Length),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos + 8 * (Max_Int_Digits + nullChars'Length);

   procedure Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt16
     (bs : in out Bitstream; IntVal : out Unsigned_16;
     nullChars : OctetBuffer; Result :    out ASN1_RESULT) with
      Pre => nullChars'Length >= 1 and then nullChars'Length <= 10
      and then nullChars'First = 1
      and then bs.Current_Bit_Pos <
        Natural'Last - 8 * (Max_Int_Digits + nullChars'Length)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 8 * (Max_Int_Digits + nullChars'Length),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos + 8 * (Max_Int_Digits + nullChars'Length);

   procedure Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt32
     (bs : in out Bitstream; IntVal : out Unsigned_32;
     nullChars : OctetBuffer; Result :    out ASN1_RESULT) with
      Pre => nullChars'Length >= 1 and then nullChars'Length <= 10
      and then nullChars'First = 1
      and then bs.Current_Bit_Pos <
        Natural'Last - 8 * (Max_Int_Digits + nullChars'Length)
      and then bs.Size_In_Bytes < Positive'Last / 8
      and then bs.Current_Bit_Pos <=
        bs.Size_In_Bytes * 8 - 8 * (Max_Int_Digits + nullChars'Length),
      Post => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and
      bs.Current_Bit_Pos <=
        bs'Old.Current_Bit_Pos + 8 * (Max_Int_Digits + nullChars'Length);

end adaasn1rtl.encoding.acn;
