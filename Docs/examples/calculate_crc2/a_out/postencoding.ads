with adaasn1rtl;
use adaasn1rtl;
with adaasn1rtl.encoding;
with MYMODULE;

use MYMODULE;

package postencoding with
   SPARK_Mode
is

   procedure my_encoding_patcher
     (unused_val                       : Packet;
      unused_bitStreamPositions_start1 : adaasn1rtl.encoding.BitstreamPtr;
      bitStreamPositions_1      :        Packet_extension_function_positions;
      bs                        : in out adaasn1rtl.encoding.Bitstream);

   function crc_validator
     (unused_val                       :        Packet;
      unused_bitStreamPositions_start1 :  adaasn1rtl.encoding.BitstreamPtr;
      bitStreamPositions_1      :        Packet_extension_function_positions;
      bs : in out adaasn1rtl.encoding.Bitstream) return adaasn1rtl.ASN1_RESULT;

end postencoding;
