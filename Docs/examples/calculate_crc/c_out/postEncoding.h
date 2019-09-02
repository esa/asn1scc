#ifndef POST_ENCODING_H
#define POST_ENCODING_H
#include "asn1crt.h"
#include "a.h"

 

void my_encoding_patcher(const Packet * pPacket, BitStream* pStartBitStrm, Packet_extension_function_positions* pNullPos, BitStream* pEndBitStrm);

flag crc_validator(const Packet * pPacket, BitStream* pStartBitStrm, Packet_extension_function_positions* pNullPos, BitStream* pEndBitStrm, int* pErrCode);
#endif
