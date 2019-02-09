package body uper_asn1_rtl with Spark_Mode  is

   -- UPER FUNCTIONS
   procedure UPER_Enc_Boolean (bs : in out BitStream; Val : in  Asn1Boolean)
   is
   begin
      BitStream_AppendBit (bs, (if val then 1 else 0));
   end UPER_Enc_Boolean;

   procedure UPER_Dec_boolean (bs : in out BitStream; val: out Asn1Boolean; result : out Boolean)
   is
      v : BIT;
   begin
      BitStream_ReadBit (bs, v, result);
      val := v = 1;
   end UPER_Dec_boolean;
   
   
   
   procedure UPER_Enc_SemiConstraintWholeNumber(bs : in out BitStream; IntVal : in Asn1Int; MinVal : in     Asn1Int)
   is
      nBytes             : Asn1Byte;
      ActualEncodedValue : Asn1UInt;
   begin
      ActualEncodedValue := Sub (IntVal, MinVal);

      nBytes := GetBytes (ActualEncodedValue);

      -- encode length
      BitStream_AppendByte (bs, nBytes, False);
      --Encode number
      Enc_UInt (bs, ActualEncodedValue, Integer(nBytes));
   end UPER_Enc_SemiConstraintWholeNumber;   
   
   
   
   procedure UPER_Dec_SemiConstraintWholeNumber (bs : in out BitStream; IntVal : out Asn1Int; MinVal : in Asn1Int; Result :    out Boolean)
   is
      NBytes : Asn1Byte;
      Ret : Asn1UInt;
   begin

      IntVal := MinVal;
      BitStream_DecodeByte(bs, NBytes, Result) ;
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

   end UPER_Dec_SemiConstraintWholeNumber;
   
   
   procedure UPER_Enc_SemiConstraintPosWholeNumber (bs : in out BitStream; IntVal : in Asn1UInt; MinVal : in     Asn1UInt)   
   is
      nBytes             : Asn1Byte;
      ActualEncodedValue : Asn1UInt;
   begin
      ActualEncodedValue := IntVal - MinVal;

      nBytes := GetBytes (ActualEncodedValue);

      -- encode length
      BitStream_AppendByte (bs, nBytes, False);
      --Encode number
      Enc_UInt (bs, ActualEncodedValue, Integer(nBytes));
   end UPER_Enc_SemiConstraintPosWholeNumber;

   procedure UPER_Dec_SemiConstraintPosWholeNumber (bs : in out BitStream; IntVal : out Asn1UInt; MinVal : in     Asn1UInt; Result :    out Boolean) 
   is
      NBytes : Asn1Byte;
      Ret : Asn1UInt:=0;
   begin

      IntVal := MinVal;
      pragma Assert(IntVal >= MinVal);
      BitStream_DecodeByte(bs, NBytes, Result) ;
      Result := Result and NBytes >= 1 and NBytes <= 8;
      if Result  then
         Dec_UInt (bs, Integer (NBytes), Ret, Result);
         IntVal := Ret + MinVal;
         Result := IntVal >= MinVal;
         if not Result then
            IntVal := MinVal;
         end if;
         
      end if;
      pragma Assert(IntVal >= MinVal);

   end UPER_Dec_SemiConstraintPosWholeNumber;
   

   
   procedure UPER_Enc_ConstraintWholeNumber (bs : in out BitStream; IntVal : in Asn1Int; MinVal : in Asn1Int; nBits : in Integer)
   is
      encVal : Asn1UInt;
   begin
      encVal := Sub (IntVal, MinVal);
      BitStream_Encode_Non_Negative_Integer (bs, encVal, nBits);
   end UPER_Enc_ConstraintWholeNumber;

   procedure UPER_Enc_ConstraintPosWholeNumber (bs : in out BitStream; IntVal: in Asn1UInt; MinVal : in Asn1UInt; nBits : in Integer)
   is
      encVal : Asn1UInt;
   begin
      encVal := IntVal - MinVal;
      BitStream_Encode_Non_Negative_Integer (bs, encVal, nBits);
   end UPER_Enc_ConstraintPosWholeNumber;
   
   procedure UPER_Dec_ConstraintWholeNumber (bs : in out BitStream; IntVal : out Asn1Int; MinVal : in Asn1Int; MaxVal : in Asn1Int; nBits : in Integer; Result : out Boolean)
   is
      encVal : Asn1UInt;
   begin
      BitStream_Decode_Non_Negative_Integer (bs, encVal, nBits,  Result);
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
   
   procedure UPER_Dec_ConstraintPosWholeNumber (bs : in out BitStream; IntVal : out Asn1UInt; MinVal : in Asn1UInt; MaxVal : in Asn1UInt; nBits : in Integer; Result : out Boolean)
   is
      encVal : Asn1UInt;
   begin
      BitStream_Decode_Non_Negative_Integer (bs, encVal, nBits,  Result);
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
     (bs : in out BitStream;
      IntVal      :    out Integer;
      MinVal      : in     Integer;
      MaxVal      : in     Integer;
      nBits : in     Integer;
      Result      :    out Boolean)
   is
      Ret : Asn1Int;
   begin
      UPER_Dec_ConstraintWholeNumber (bs, Ret,  Asn1Int (MinVal),  Asn1Int (MaxVal),   nBits,       Result);
      IntVal := Integer (Ret);
   end UPER_Dec_ConstraintWholeNumberInt;
   
   
   procedure UPER_Enc_UnConstraintWholeNumber (bs : in out BitStream; IntVal : in Asn1Int)
   is
      nBytes : Asn1Byte;
   begin

      nBytes := GetLengthInBytesOfSInt (IntVal);

      -- encode length
      BitStream_AppendByte (bs, nBytes, False);
      Enc_UInt (bs, To_UInt (IntVal), Integer(nBytes));
   end UPER_Enc_UnConstraintWholeNumber;
   
   procedure UPER_Dec_UnConstraintWholeNumber (bs : in out BitStream; IntVal : out Asn1Int; Result :    out Boolean)
   is
      NBytes : Asn1Byte;
   begin
      BitStream_DecodeByte(bs, NBytes, Result) ;
      Result := Result and NBytes >= 1 and NBytes <= 8;
      if Result then
         Dec_Int (bs, Integer (NBytes), IntVal, Result);
      else
         IntVal := 0;
         Result := False;
      end if;
   end UPER_Dec_UnConstraintWholeNumber;
   
   procedure UPER_Dec_UnConstraintWholeNumberMax (bs : in out BitStream; IntVal : out Asn1Int;  MaxVal : in Asn1Int; Result : out Boolean)
   is
   begin
      UPER_Dec_UnConstraintWholeNumber (bs, IntVal, Result);
      Result := Result and IntVal <= MaxVal;
      if not Result then
         IntVal := MaxVal;
      end if;
   end UPER_Dec_UnConstraintWholeNumberMax;
   

   procedure UPER_Enc_Real (bs : in out BitStream;  RealVal : in     Asn1Real)
   is
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

         --#check NExpLen>=1 AND NExpLen<=3;

         -- encode length
         BitStream_AppendByte(bs, (1 + NExpLen) + NManLen, False); --1

         -- encode header
         BitStream_AppendByte (bs, Header, False); --1

         -- encode exponent
         Enc_UInt (bs, To_UInt (Exp), Integer(NExpLen)); --max 3 octets

         -- encode mantissa
         Enc_UInt (bs, Mantissa, Integer(NManLen)); --max 8 octets
      end if;
   end UPER_Enc_Real;
   

   function CalcReal (Factor : Asn1UInt; N : Asn1UInt; base : Integer;Exp : Integer) return Asn1Real 
   is
     pragma SPARK_Mode(Off);
   begin
     return (Asn1Real (Factor * N) * Asn1Real (base)**Exp);
   end;


   procedure UPER_Dec_Real_AsBinary_aux  
   (bs : in out BitStream;
      ExpLen  : in     Asn1Byte;
      Length  : in     Asn1Byte;
      Factor  : in     Asn1UInt;
      Sign    : in     Integer;
      Base    : in     Integer;
      RealVal :    out Asn1Real;
      Result  :    out ASN1_RESULT)
   with
     Depends => ((bs, RealVal, Result) => (bs, ExpLen, Length, Factor, Sign, Base)),
       Pre     => 
                (base=2 or base=8 or base=16) and then
                (Factor = 1 or Factor=2 or Factor=4 or Factor=8) and then
                ExpLen <= 4 and then
                Length >=0 and then Length <=11 and then
                (Sign = 1 or Sign = -1) and then
                bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 24) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 24),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 24)  
       
   is
      Exp : Asn1Int;
      N   : Asn1UInt;
   begin
      RealVal := 0.0;
      Result := ASN1_RESULT'(Success => False, ErrorCode => ERR_END_OF_STREAM);
      if ExpLen < Length and ExpLen <= 3 then
         Dec_Int (bs, Integer (ExpLen), Exp, Result.Success);

         if Result.Success and Length - ExpLen <= 8 then
            Dec_UInt (bs, Integer (Length - ExpLen), N, Result.Success);
            if Result.Success and Exp > Asn1Int (Integer'First) and Exp < Asn1Int (Integer'Last)    then
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
     (bs : in out BitStream;
      Header    : in     Asn1Byte;
      EncLength : in     Asn1Byte;
      RealVal   :    out Asn1Real;
      Result    :    out ASN1_RESULT)
   with
     Depends => ((bs, RealVal, Result) => (bs, Header, EncLength)),
       Pre     => 
                EncLength >=0 and then EncLength <=11 and then
                bs.Current_Bit_Pos < (Natural'Last - (Asn1UInt'Size + 24)) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 24),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 24)  

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

      F      := (Header and 16#0C#) / 4;
      pragma Assert(F = 0 or F=1 or F=2 or F=3);
      
      Factor := Factor * (2**Integer (F));
      pragma Assert(Factor = 1 or Factor=2 or Factor=4 or Factor=8);
      

      ExpLen := (Header and 16#03#) + 1;
      pragma Assert(ExpLen <= 4);

      UPER_Dec_Real_AsBinary_aux(bs, ExpLen, EncLength, Factor, Sign, Base, RealVal, Result);

   end UPER_Dec_Real_AsBinary;

   procedure UPER_Dec_Real (bs : in out BitStream; RealVal : out Asn1Real; Result  : out ASN1_RESULT)
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
                  UPER_Dec_Real_AsBinary (bs, Header, Length - 1, RealVal, Result);
               else
                  Result := ASN1_RESULT' (Success   => False, ErrorCode => ERR_UNSUPPORTED_ENCODING);
               end if;
            end if;
         else
            Result := ASN1_RESULT'(Success => True, ErrorCode => 0);
         end if;
      end if;
   end UPER_Dec_Real;
   

   
   



   procedure ObjectIdentifier_uper_decode_length(bs : in out BitStream; length : out integer; result  :    out ASN1_RESULT)
   with
     Depends => ((bs, length, result) => (bs)),
       Pre     => 
                bs.Current_Bit_Pos < Natural'Last - 16 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 16,
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + 16    
    is
        len2:Integer;
    begin
        result := ASN1_RESULT'(ErrorCode => 0,Success => true);
        UPER_Dec_ConstraintWholeNumberInt(bs, length, 0, 255, 8, result.Success);
        if result.Success then
            if length > 16#80# then
                length := (length - 16#80#) * 16#100#;
                UPER_Dec_ConstraintWholeNumberInt(bs, len2, 0, 255, 8, result.Success);
                if result.Success THEN
                    length := length + len2;
                end if;
            end if;
        end if;
    end ObjectIdentifier_uper_decode_length;


   
--  Each subidentifier is represented as a series of (one or more) octets. 
--      Bit 8 of each octet indicates whether it is the last in the series:  bit 8 of the last octet is zero; bit 8 of each preceding octet is one. 
--      Bits 7 to 1 of the octets in the series collectively encode the subidentifier. Conceptually, these groups of bits are concatenated to form 
--      an unsigned binary number whose most significant bit is bit 7 of the first octet and whose least significant bit is bit 1 of the last octet. 
--      The subidentifier shall be encoded in the fewest possible octets, that is, the leading octet of the subidentifier shall not have the value 8016.

    procedure ObjectIdentifier_subidentifiers_uper_encode(encodingBuf:in out OctetArray1K; curSize : in out integer; siValue0 : in Asn1UInt) with
     Depends => (curSize => (curSize, siValue0), encodingBuf => (encodingBuf, curSize, siValue0)),
     Pre     => curSize>=OctetArray1K'First - 1 and curSize < OctetArray1K'Last - OctetBuffer_16'Last,
     Post    => curSize >= curSize'Old + 1 and curSize<= curSize'Old + OctetBuffer_16'Last and curSize<= OctetArray1K'Last
    is
        lastOctet : boolean := FALSE;
        tmp : OctetBuffer_16 := OctetBuffer_16'(others => 0);
        nSize : integer:= 0;
        curByte : Asn1Byte;
        siValue : Asn1UInt := siValue0;
    begin
        while not lastOctet and nSize < OctetBuffer_16'Last loop
            pragma Loop_Invariant(nSize >= 0 and nSize < OctetBuffer_16'Last);
            curByte := Asn1Byte(siValue mod 128);
            siValue := siValue / 128;
            lastOctet := (siValue = 0);
            tmp(OctetBuffer_16'First + nSize) := curByte;
            nSize := nSize + 1;
        end loop;

        pragma Assert(nSize>=1);
        pragma Assert(nSize <= OctetBuffer_16'Last);

        for i in Integer range 1 .. nSize loop
            pragma Loop_Invariant (
                                nSize <= OctetBuffer_16'Last and 
                                nSize - i + 1 >= OctetBuffer_16'First and 
                                nSize - i + 1 <= OctetBuffer_16'Last and
                                curSize + i >=OctetArray1K'First and
                                curSize + i  < OctetArray1K'Last 
                                );        
            curByte := (if i = nSize then tmp(1) else tmp(nSize - i + 1) or 16#80#);
            --curSize := curSize + 1;
            encodingBuf(curSize + i) := curByte;
        end loop;
        curSize := curSize + nSize;


    end ObjectIdentifier_subidentifiers_uper_encode;
   
   
    procedure ObjectIdentifier_subidentifiers_uper_decode (bs : in out BitStream; remainingOctets : in out Integer; siValue : out Asn1UInt; Result  :    out ASN1_RESULT) with
     Depends => ((remainingOctets,bs) => (remainingOctets, bs), (siValue, Result) => (remainingOctets, bs)),
     Pre     => 
                remainingOctets > 0 and then
                bs.Current_Bit_Pos < Natural'Last - (8*OctetBuffer_16'Last) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (8*OctetBuffer_16'Last),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (8*OctetBuffer_16'Last) 
                and remainingOctets <= remainingOctets'Old and remainingOctets >= remainingOctets'Old - OctetBuffer_16'Last  
    is
    	curByte : Asn1Byte;
	bLastOctet : boolean  := false;
      curOctetValue : Asn1UInt := 0;
      i : Integer := 1;
    begin
        siValue := 0;
        Result := ASN1_RESULT'(Success => true,ErrorCode => 0);

        while Result.Success and remainingOctets > 0 and not bLastOctet and bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8  and  i <= OctetBuffer_16'Last loop
            curByte := 0;
            pragma Loop_Invariant (i>=1 and i <= OctetBuffer_16'Last and remainingOctets > 0 
                                and bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8 and 
                                  remainingOctets = remainingOctets'Loop_Entry - (i-1));
            BitStream_DecodeByte (bs, curByte, Result.Success);
            
            remainingOctets := remainingOctets - 1;
            bLastOctet := (curByte and 16#80#) = 0;
            curOctetValue := Asn1UInt(curByte and 16#7F#);
            siValue := Shift_Left(siValue, 7);

	    siValue := siValue or curOctetValue;
            i:=i+1; 

        end loop;


    end ObjectIdentifier_subidentifiers_uper_decode;


    procedure ObjectIdentifier_uper_encode(bs : in out BitStream; val : Asn1ObjectIdentifier)
    is
        tmp : OctetArray1K := OctetArray1K'(others => 0);
        totalSize : integer := 0;
    begin
        ObjectIdentifier_subidentifiers_uper_encode(tmp, totalSize, val.values(1)*40 + val.values(2));
        pragma Assert(totalSize >= 1 and totalSize <= OctetBuffer_16'Last);
      
        for i in integer range 3 .. val.Length loop
         pragma Loop_Invariant(
                               val.Length <= OBJECT_IDENTIFIER_MAX_LENGTH and
                               totalSize >= 1 and totalSize <=  totalSize'Loop_Entry + (i-3)*OctetBuffer_16'Last);
            ObjectIdentifier_subidentifiers_uper_encode(tmp, totalSize, val.values(i));
        end loop;

      pragma Assert(totalSize <= OctetBuffer_16'Last*OBJECT_IDENTIFIER_MAX_LENGTH);
      

        --encode length determinant
        if totalSize <= 16#7F# then
            UPER_Enc_ConstraintWholeNumber(bs, Asn1Int(totalSize), 0, 8);
        else
            BitStream_AppendBit(bs, 1);
            UPER_Enc_ConstraintWholeNumber(bs, Asn1Int(totalSize), 0, 15);
        end if;
      


        --encode contents
        for i in integer range 1 .. totalSize loop
            pragma Loop_Invariant(bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
            BitStream_AppendByte(bs , tmp(i), false);
        end loop;

    end ObjectIdentifier_uper_encode;

   
   
    procedure ObjectIdentifier_uper_decode
     (bs : in out BitStream;
      val :    out Asn1ObjectIdentifier;
      Result  :    out ASN1_RESULT)
    is
      totalSize : Integer;
      si : Asn1UInt;
      
    begin
      ObjectIdentifier_Init(val);
      
         ObjectIdentifier_uper_decode_length (bs, totalSize, Result);
      
        if Result.Success and totalSize > 0 then
            ObjectIdentifier_subidentifiers_uper_decode(bs, totalSize, si, Result);
            if result.Success then
                val.Length := 2;
                val.values(1) := si/40;
                val.values(2) := si mod 40;

                while Result.Success and totalSize > 0  and val.Length < OBJECT_IDENTIFIER_MAX_LENGTH loop
                    pragma Loop_Invariant (bs.Current_Bit_Pos >= bs.Current_Bit_Pos'Loop_Entry and bs.Current_Bit_Pos <= bs.Current_Bit_Pos'Loop_Entry + (val.Length - val.Length'Loop_Entry)*8*OctetBuffer_16'Last ); 
                    ObjectIdentifier_subidentifiers_uper_decode(bs, totalSize, si, Result);
                    val.Length := val.Length + 1;
                    val.values(val.Length) := si;
                end loop;
                result.Success := result.Success and totalSize = 0;

            end if;
        end if;
    end ObjectIdentifier_uper_decode;

    procedure RelativeOID_uper_encode(bs : in out BitStream; val : Asn1ObjectIdentifier)
    is
        tmp : OctetArray1K := OctetArray1K'(others => 0);
        totalSize : integer := 0;
    begin
        for i in integer range 1 .. val.Length loop
         pragma Loop_Invariant(
                               val.Length <= OBJECT_IDENTIFIER_MAX_LENGTH and
                               totalSize >= 0 and totalSize <=  totalSize'Loop_Entry + (i-1)*OctetBuffer_16'Last);
            ObjectIdentifier_subidentifiers_uper_encode(tmp, totalSize, val.values(i));
        end loop;
      pragma Assert(totalSize <= OctetBuffer_16'Last*OBJECT_IDENTIFIER_MAX_LENGTH);

        --encode length determinant
        if totalSize <= 16#7F# then
            UPER_Enc_ConstraintWholeNumber(bs, Asn1Int(totalSize), 0, 8);
        else
            BitStream_AppendBit(bs, 1);
            UPER_Enc_ConstraintWholeNumber(bs, Asn1Int(totalSize), 0, 15);
        end if;

        --encode contents
        for i in integer range 1 .. totalSize loop
            pragma Loop_Invariant(bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
            BitStream_AppendByte(bs , tmp(i), false);
        end loop;

    end RelativeOID_uper_encode;

    procedure RelativeOID_uper_decode
     (bs : in out BitStream;
      val :    out Asn1ObjectIdentifier;
      Result  :    out ASN1_RESULT)
    is
        totalSize : Integer;
        si : Asn1UInt;
    begin
        ObjectIdentifier_Init(val);
        ObjectIdentifier_uper_decode_length (bs, totalSize, Result);
        if Result.Success then
            while Result.Success and totalSize > 0  and val.Length < OBJECT_IDENTIFIER_MAX_LENGTH loop
                pragma Loop_Invariant (bs.Current_Bit_Pos >= bs.Current_Bit_Pos'Loop_Entry and bs.Current_Bit_Pos <= bs.Current_Bit_Pos'Loop_Entry + (val.Length - val.Length'Loop_Entry)*8*OctetBuffer_16'Last ); 
                ObjectIdentifier_subidentifiers_uper_decode(bs, totalSize, si, Result);
                val.Length := val.Length + 1;
                val.values(val.Length) := si;
            end loop;
            result.Success := result.Success and totalSize = 0;
      end if;
    end RelativeOID_uper_decode;
   
   
end uper_asn1_rtl;
