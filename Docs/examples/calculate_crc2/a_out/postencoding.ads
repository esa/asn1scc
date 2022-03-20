with adaasn1rtl;
with System;
use adaasn1rtl;
with adaasn1rtl.encoding;
with adaasn1rtl.encoding.acn;
with MYMODULE;

use type adaasn1rtl.OctetBuffer;
use type adaasn1rtl.BitArray;
use type adaasn1rtl.Asn1UInt;
use type adaasn1rtl.Asn1Int;
use type adaasn1rtl.BIT;
use MYMODULE;
--# inherit ;

package postencoding with
   SPARK_Mode
is

   procedure my_encoding_patcher
     (val                       : in     Packet;
      bitStreamPositions_start1 :        adaasn1rtl.encoding.BitstreamPtr;
      bitStreamPositions_1      :        Packet_extension_function_positions;
      bs                        : in out adaasn1rtl.encoding.Bitstream);

   function crc_validator
     (val                       : in     Packet;
      bitStreamPositions_start1 :        adaasn1rtl.encoding.BitstreamPtr;
      bitStreamPositions_1      :        Packet_extension_function_positions;
      bs : in out adaasn1rtl.encoding.Bitstream) return adaasn1rtl.ASN1_RESULT;

end postencoding;
