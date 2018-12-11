
package body adaasn1rtl with Spark_Mode is

   MASKS : constant OctetBuffer_0_7 := OctetBuffer_0_7'(0 => 16#80#, 1=> 16#40#, 2=>16#20#, 3=>16#10#, 4=>16#08#, 5=>16#04#, 6=>16#02#, 7=>16#01#);
   
   
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
         bs.currentBitPos := 0;
         bs.currentBytePos := bs.currentBytePos + 1;
      else
         bs.currentBitPos := bs.currentBitPos + 1;
      end if;
      result := true;
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
      else
         ret := bs.buffer(bs.currentBytePos);
         bs.currentBytePos := bs.currentBytePos + 1;
      end if;
      byteValue := ret;
      success := true;
      
   end;
   
   

end adaasn1rtl;
