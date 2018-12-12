
package body adaasn1rtl with Spark_Mode is

   MASKS  : constant OctetBuffer_0_7 := OctetBuffer_0_7'(0 => 16#80#, 1=> 16#40#, 2=>16#20#, 3=>16#10#, 4=>16#08#, 5=>16#04#, 6=>16#02#, 7=>16#01#);
   MASKSB : constant OctetBuffer_0_7 := OctetBuffer_0_7'(0 => 16#00#, 1=> 16#01#, 2=>16#03#, 3=>16#07#, 4=>16#1F#, 5=>16#3F#, 6=>16#7F#, 7=>16#FF#);
   
   function BitStream_init(bitStreamSizeInBytes : Positive) return BitStream
   is
      ret : BitStream(bitStreamSizeInBytes);
   begin
      ret.currentBytePos := 1;
      ret.buffer := (others => 0);
      ret.currentBitPos := 0;
      return ret;
   end;
   
   
   
   procedure BitStream_AppendBit(bs : in out BitStream; bitValue : in BIT) 
   is
   begin 
      if bitValue = 1 then
         bs.buffer(bs.currentBytePos) := bs.buffer(bs.currentBytePos) or MASKS(bs.currentBitPos);
      end if;
      
      if bs.currentBitPos = 7 then
         bs.currentBitPos := 0;
         bs.currentBytePos := bs.currentBytePos + 1;
      else
         bs.currentBitPos := bs.currentBitPos + 1;
      end if;
   end;
   
   procedure BitStream_ReadBit(bs : in out BitStream; bitValue : out BIT; result :    out Boolean)
   is
   begin
      if (bs.buffer(bs.currentBytePos) and MASKS(bs.currentBitPos)) > 0 then
         bitValue := 1;
      else
         bitValue := 1;
      end if;
      
      if bs.currentBitPos = 7 then
         result := true;
         bs.currentBitPos := 0;
         bs.currentBytePos := bs.currentBytePos + 1;
      else
         result := true;
         bs.currentBitPos := bs.currentBitPos + 1;
      end if;
   end;
   
   
   procedure BitStream_AppendByte(bs : in out BitStream; ByteValue : in Asn1Byte; negate : in Boolean)
   is
      byteVal : Asn1Byte;
      cb : BIT_RANGE;
      ncb :BIT_RANGE;
   begin
      cb := bs.currentBitPos;
      if Negate then
         byteVal := not byteValue;
      else
         byteVal := byteValue;
      end if;
      
      if cb > 0 then
         ncb := 8 - cb;
         bs.buffer(bs.currentBytePos) := bs.buffer(bs.currentBytePos) or Shift_right(ByteVal, cb);
         bs.currentBytePos := bs.currentBytePos + 1;
         bs.buffer(bs.currentBytePos) := Shift_left(ByteVal, ncb);
      else
         bs.buffer(bs.currentBytePos) := ByteVal;
         bs.currentBytePos := bs.currentBytePos + 1;
      end if;
   end;
   
   
   procedure BitStream_DecodeByte(bs : in out BitStream; byteValue : out Asn1Byte; success   :    out Boolean) 
   is
      cb : BIT_RANGE;
      ncb :BIT_RANGE;
      ret :Asn1Byte;
   begin
      cb := bs.currentBitPos;
      if cb > 0 then
         ncb := 8 - cb;
         ret := Shift_left(bs.buffer(bs.currentBytePos), cb);
         bs.currentBytePos := bs.currentBytePos + 1;
         ret := ret or Shift_right(bs.buffer(bs.currentBytePos), ncb);
         success := true;
      else
         ret := bs.buffer(bs.currentBytePos);
         bs.currentBytePos := bs.currentBytePos + 1;
         success := true;
      end if;
      byteValue := ret;
      
   end;
   
   procedure BitStream_ReadNibble(bs : in out BitStream; byteValue : out Asn1Byte; success   :    out Boolean)
   is
      cb: BIT_RANGE;
      totalBitsForNextByte : BIT_RANGE;
   begin
      cb := bs.currentBitPos;
      if cb < 4 then
         byteValue := Shift_right(bs.buffer(bs.currentBytePos), 4 - cb) and 16#0F#;
         bs.currentBitPos := bs.currentBitPos + 4;
         success := true;
      else
         totalBitsForNextByte := cb - 4;
         byteValue := Shift_left(bs.buffer(bs.currentBytePos), totalBitsForNextByte);
         bs.currentBytePos := bs.currentBytePos + 1;
         byteValue := byteValue or (Shift_right(bs.buffer(bs.currentBytePos), 8 - totalBitsForNextByte));
         byteValue := byteValue and 16#0F#;
         bs.currentBitPos := bs.currentBitPos - 4;
         success := true;
      end if;
   end;
   
   
   procedure BitStream_AppendPartialByte(bs : in out BitStream; byteValueIn : in Asn1Byte; nBits : in BIT_RANGE; negate : in Boolean)
   is
      cb: BIT_RANGE;
      totalBits : BIT_RANGE;
      totalBitsForNextByte : BIT_RANGE;
      byteValue : Asn1Byte;
   begin
      cb := bs.currentBitPos;
      
      byteValue := (if negate then (masksb(nBits) and  not byteValueIn) else byteValueIn);
         
      if cb < 8 - nbits then
         totalBits := cb + nBits;
         bs.buffer(bs.currentBytePos) := bs.buffer(bs.currentBytePos) or Shift_left(byteValue, 8 -totalBits);
         bs.currentBitPos := bs.currentBitPos + nBits;
      else
         totalBitsForNextByte := cb+nbits - 8;
         bs.buffer(bs.currentBytePos) := bs.buffer(bs.currentBytePos) or Shift_right(byteValue, totalBitsForNextByte);
         bs.currentBytePos := bs.currentBytePos + 1;
         
         bs.buffer(bs.currentBytePos) := bs.buffer(bs.currentBytePos) or Shift_left(byteValue, 8 - totalBitsForNextByte);
           
         bs.currentBitPos := totalBitsForNextByte;
      end if;
      
   end;
   
--     procedure BitStream_Encode_Non_Negative_Integer(bs : in out BitStream; intValue   : in Asn1UInt; nBitsRange : in Integer) 
--     is
--        cc    : Integer range 0 .. Asn1UInt'Size-1;
--        byteValue : Asn1Byte;
--     begin
--        cc := nBitsRange;
--        while cc > 8 loop
--           pragma Loop_Invariant (cc - 8 > 0 and bs.currentBytePos  < );
--           
--           byteValue := Asn1Byte (Shift_left(intValue, cc-8));
--           BitStream_AppendByte(bs, byteValue, false);
--           cc := cc - 8;
--        end loop;
--        if cc > 0 then
--           byteValue := MASKSB(cc) and Asn1Byte(intValue);
--           BitStream_AppendPartialByte(bs, byteValue, cc, False);
--        end if;
--  
--     end;
--     
   

end adaasn1rtl;
