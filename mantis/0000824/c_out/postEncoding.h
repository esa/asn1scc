#ifndef POST_ENCODING_H
#define POST_ENCODING_H
#include "asn1crt.h"
#include "a.h"

 

void my_encoding_patcher(const T_Packet * pPacket, BitStream* pStartBitStrm, T_Packet_extension_function_positions* pNullPos, BitStream* pEndBitStrm);

flag crc_validator(const T_Packet * pPacket, BitStream* pStartBitStrm, T_Packet_extension_function_positions* pNullPos, BitStream* pEndBitStrm, int* pErrCode);
#endif
