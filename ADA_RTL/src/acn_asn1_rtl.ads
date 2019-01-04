with Interfaces; 
use Interfaces;
with adaasn1rtl;
use adaasn1rtl;


package acn_asn1_rtl with Spark_Mode is

   procedure Acn_Enc_Int_PositiveInteger_ConstSize(bs : in out BitStream;  IntVal : in     Asn1UInt;   sizeInBits   : in     Integer) with
     Depends => (bs => (bs, IntVal, sizeInBits)),
     Pre     => sizeInBits >= 0 and then 
                sizeInBits < Asn1UInt'Size and then 
                IntVal <= 2**sizeInBits - 1 and then
                bs.Current_Bit_Pos < Natural'Last - sizeInBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - sizeInBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + sizeInBits     ;
   

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_8(bs : in out BitStream; IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 8 and then  
                IntVal <= Asn1UInt(Asn1Byte'Last) and then
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16 (bs : in out BitStream; IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 16 and then  
                IntVal <= Asn1UInt(Interfaces.Unsigned_16'Last) and then
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 16,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32 (bs : in out BitStream;  IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 32 and then  
                IntVal <= Asn1UInt(Interfaces.Unsigned_32'Last) and then
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 32,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64 (bs : in out BitStream;   IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 64 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 64,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64;
   
   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16 (bs : in out BitStream; IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 16 and then  
                IntVal <= Asn1UInt(Interfaces.Unsigned_16'Last) and then
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 16,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16;
   
   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32 (bs : in out BitStream;  IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 32 and then  
                IntVal <= Asn1UInt(Interfaces.Unsigned_32'Last) and then
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 32,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64 (bs : in out BitStream;   IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 64 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 64,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64;
   
   procedure Acn_Enc_Int_PositiveInteger_VarSize_LengthEmbedded  (bs : in out BitStream;  IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);  
     
   
   type Asn1Int_ARRAY_2_64 is array (2 .. 64) of Asn1Int;
   
   PV  : constant Asn1Int_ARRAY_2_64 := Asn1Int_ARRAY_2_64'(2 => Asn1Int(1), 3 => Asn1Int(3), 4 => Asn1Int(7), 5 => Asn1Int(15), 6 => Asn1Int(31), 7 => Asn1Int(63), 8 => Asn1Int(127), 9 => Asn1Int(255), 10 => Asn1Int(511), 11 => Asn1Int(1023), 12 => Asn1Int(2047), 13 => Asn1Int(4095), 14 => Asn1Int(8191), 15 => Asn1Int(16383), 16 => Asn1Int(32767), 17 => Asn1Int(65535), 18 => Asn1Int(131071), 19 => Asn1Int(262143), 20 => Asn1Int(524287), 21 => Asn1Int(1048575), 22 => Asn1Int(2097151), 23 => Asn1Int(4194303), 24 => Asn1Int(8388607), 25 => Asn1Int(16777215), 26 => Asn1Int(33554431), 27 => Asn1Int(67108863), 28 => Asn1Int(134217727), 29 => Asn1Int(268435455), 30 => Asn1Int(536870911), 31 => Asn1Int(1073741823), 32 => Asn1Int(2147483647), 33 => Asn1Int(4294967295), 34 => Asn1Int(8589934591), 35 => Asn1Int(17179869183), 36 => Asn1Int(34359738367), 37 => Asn1Int(68719476735), 38 => Asn1Int(137438953471), 39 => Asn1Int(274877906943), 40 => Asn1Int(549755813887), 41 => Asn1Int(1099511627775), 42 => Asn1Int(2199023255551), 43 => Asn1Int(4398046511103), 44 => Asn1Int(8796093022207), 45 => Asn1Int(17592186044415), 46 => Asn1Int(35184372088831), 47 => Asn1Int(70368744177663), 48 => Asn1Int(140737488355327), 49 => Asn1Int(281474976710655), 50 => Asn1Int(562949953421311), 51 => Asn1Int(1125899906842623), 52 => Asn1Int(2251799813685247), 53 => Asn1Int(4503599627370495), 54 => Asn1Int(9007199254740991), 55 => Asn1Int(18014398509481983), 56 => Asn1Int(36028797018963967), 57 => Asn1Int(72057594037927935), 58 => Asn1Int(144115188075855871), 59 => Asn1Int(288230376151711743), 60 => Asn1Int(576460752303423487), 61 => Asn1Int(1152921504606846975), 62 => Asn1Int(2305843009213693951), 63 => Asn1Int(4611686018427387903), 64 => Asn1Int(9223372036854775807));
   NV  : constant Asn1Int_ARRAY_2_64 := Asn1Int_ARRAY_2_64'(2 => Asn1Int(-2), 3 => Asn1Int(-4), 4 => Asn1Int(-8), 5 => Asn1Int(-16), 6 => Asn1Int(-32), 7 => Asn1Int(-64), 8 => Asn1Int(-128), 9 => Asn1Int(-256), 10 => Asn1Int(-512), 11 => Asn1Int(-1024), 12 => Asn1Int(-2048), 13 => Asn1Int(-4096), 14 => Asn1Int(-8192), 15 => Asn1Int(-16384), 16 => Asn1Int(-32768), 17 => Asn1Int(-65536), 18 => Asn1Int(-131072), 19 => Asn1Int(-262144), 20 => Asn1Int(-524288), 21 => Asn1Int(-1048576), 22 => Asn1Int(-2097152), 23 => Asn1Int(-4194304), 24 => Asn1Int(-8388608), 25 => Asn1Int(-16777216), 26 => Asn1Int(-33554432), 27 => Asn1Int(-67108864), 28 => Asn1Int(-134217728), 29 => Asn1Int(-268435456), 30 => Asn1Int(-536870912), 31 => Asn1Int(-1073741824), 32 => Asn1Int(-2147483648), 33 => Asn1Int(-4294967296), 34 => Asn1Int(-8589934592), 35 => Asn1Int(-17179869184), 36 => Asn1Int(-34359738368), 37 => Asn1Int(-68719476736), 38 => Asn1Int(-137438953472), 39 => Asn1Int(-274877906944), 40 => Asn1Int(-549755813888), 41 => Asn1Int(-1099511627776), 42 => Asn1Int(-2199023255552), 43 => Asn1Int(-4398046511104), 44 => Asn1Int(-8796093022208), 45 => Asn1Int(-17592186044416), 46 => Asn1Int(-35184372088832), 47 => Asn1Int(-70368744177664), 48 => Asn1Int(-140737488355328), 49 => Asn1Int(-281474976710656), 50 => Asn1Int(-562949953421312), 51 => Asn1Int(-1125899906842624), 52 => Asn1Int(-2251799813685248), 53 => Asn1Int(-4503599627370496), 54 => Asn1Int(-9007199254740992), 55 => Asn1Int(-18014398509481984), 56 => Asn1Int(-36028797018963968), 57 => Asn1Int(-72057594037927936), 58 => Asn1Int(-144115188075855872), 59 => Asn1Int(-288230376151711744), 60 => Asn1Int(-576460752303423488), 61 => Asn1Int(-1152921504606846976), 62 => Asn1Int(-2305843009213693952), 63 => Asn1Int(-4611686018427387904), 64 => Asn1Int(-9223372036854775808));
   
   procedure Acn_Enc_Int_TwosComplement_ConstSize(bs : in out BitStream;  IntVal : in Asn1Int;   sizeInBits   : in     Natural) with
     Depends => (bs => (bs, IntVal, sizeInBits)),
     Pre     => sizeInBits >= 2 and then 
                sizeInBits < Asn1UInt'Size and then 
                IntVal >= NV(sizeInBits) and then
                IntVal <=  PV(sizeInBits) and then
                bs.Current_Bit_Pos < Natural'Last - sizeInBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - sizeInBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + sizeInBits     ;
   
   procedure Acn_Enc_Int_TwosComplement_ConstSize_8(bs : in out BitStream; IntVal : in     Asn1Int) with
     Depends => (bs => (bs, IntVal)),
     Pre     => IntVal >= NV(8) and then
                IntVal <=  PV(8) and then
                bs.Current_Bit_Pos < Natural'Last - 8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8     ;
   
   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_16(bs : in out BitStream; IntVal : in     Asn1Int) with
     Depends => (bs => (bs, IntVal)),
     Pre     => IntVal >= NV(16) and then
                IntVal <=  PV(16) and then
                bs.Current_Bit_Pos < Natural'Last - 8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 16,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16     ;


   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_32(bs : in out BitStream; IntVal : in     Asn1Int) with
     Depends => (bs => (bs, IntVal)),
     Pre     => IntVal >= NV(32) and then
                IntVal <=  PV(32) and then
                bs.Current_Bit_Pos < Natural'Last - 32 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 32,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32     ;
   
   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_64(bs : in out BitStream; IntVal : in     Asn1Int) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 64 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 64,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64     ;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_16 (bs : in out BitStream; IntVal : in     Asn1Int) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 16 and then  
                IntVal <= Asn1Int(Interfaces.Unsigned_16'Last) and then
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 16,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16;
   
   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_32 (bs : in out BitStream;  IntVal : in     Asn1Int) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 32 and then  
                IntVal <= Asn1Int(Interfaces.Unsigned_32'Last) and then
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 32,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_64 (bs : in out BitStream;   IntVal : in     Asn1Int) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 64 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 64,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64;
   
   procedure Acn_Enc_Int_TwosComplement_VarSize_LengthEmbedded  (bs : in out BitStream;  IntVal : in     Asn1Int) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1Int'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - (Asn1Int'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1Int'Size + 8);  


   
   procedure Acn_Enc_Int_BCD_ConstSize (bs : in out BitStream; IntVal : in Asn1UInt; nNibbles : in Integer) with
     Depends => (bs => (bs, IntVal, nNibbles)),
     Pre     => nNibbles >= 1
     and then
                nNibbles <= 19 and then
                IntVal < Powers_of_10(nNibbles) and then
                bs.Current_Bit_Pos < Natural'Last - 4*nNibbles and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 4*nNibbles,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 4*nNibbles  ;


   procedure Acn_Enc_Int_BCD_VarSize_NullTerminated (bs : in out BitStream; IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => IntVal < Powers_of_10(19) and then
                bs.Current_Bit_Pos < Natural'Last - 4*(19+1) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 4*(19+1),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 4*(19+1)  ;





   procedure Acn_Enc_Int_ASCII_VarSize_LengthEmbedded (bs : in out BitStream; IntVal : in     Asn1Int) with
     Depends => (bs => (bs, IntVal)),
     Pre     => IntVal > -Asn1Int(Powers_of_10(18)) and then
                Asn1Uint(abs IntVal) < Powers_of_10(18) and then
                bs.Current_Bit_Pos < Natural'Last - 8*(18+1+1) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 8*(18+1+1),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8*(18+1+1)  ;



   procedure Acn_Enc_UInt_ASCII_VarSize_LengthEmbedded (bs : in out BitStream;  IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => IntVal < Powers_of_10(19) and then
                bs.Current_Bit_Pos < Natural'Last - 8*(19+1) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 8*(19+1),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8*(19+1)  ;


   procedure Acn_Enc_Int_ASCII_VarSize_NullTerminated (bs : in out BitStream; IntVal : in Asn1Int; nullChar : in Asn1Byte) with
     Depends => (bs => (bs, IntVal, nullChar)),
     Pre     => IntVal > -Asn1Int(Powers_of_10(18)) and then
                Asn1Uint(abs IntVal) < Powers_of_10(18) and then
                bs.Current_Bit_Pos < Natural'Last - 8*(18+1+1) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 8*(18+1+1),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8*(18+1+1)  ;

   procedure Acn_Enc_UInt_ASCII_VarSize_NullTerminated (bs : in out BitStream; IntVal : in Asn1UInt; nullChar : in Asn1Byte)  with
     Depends => (bs => (bs, IntVal, nullChar)),
     Pre     => IntVal < Powers_of_10(19) and then
                bs.Current_Bit_Pos < Natural'Last - 8*(19+1) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 8*(19+1),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 8*(19+1)  ;

   procedure Acn_Dec_Int_PositiveInteger_ConstSize (bs : in out BitStream; IntVal :out Asn1UInt; minVal : in Asn1UInt; maxVal : in Asn1UInt; nSizeInBits : in Integer; Result: out ASN1_RESULT) with
     Pre     => nSizeInBits >= 0 and then 
                nSizeInBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nSizeInBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - nSizeInBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nSizeInBits  and 
     ( (Result.Success and IntVal >= minVal and IntVal <= maxVal) or
       (not Result.Success and IntVal = minVal))
   ;


end acn_asn1_rtl;
