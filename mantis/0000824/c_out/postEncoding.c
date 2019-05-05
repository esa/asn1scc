#include "asn1crt.h"
#include "a.h"
#include "assert.h"


 

void my_encoding_patcher(const T_Packet * pPacket, BitStream* pStartBitStrm, T_Packet_extention_function_potisions* pNullPos, BitStream* pEndBitStrm) {
    long endPosIBits =
        pNullPos->T_Packet_crc32.currentByte * 8 + pNullPos->T_Packet_crc32.currentBit;
    long startPosInBits =
        pNullPos->T_Packet_length.currentByte * 8 + pNullPos->T_Packet_length.currentBit; 
    long lengthInBits = endPosIBits - startPosInBits - 12;
    assert(lengthInBits > 0);
    assert(lengthInBits < 4096);
    assert(lengthInBits == 16 || lengthInBits == 64);
    Acn_Enc_Int_PositiveInteger_ConstSize(&pNullPos->T_Packet_length, lengthInBits, 12);

}    


flag crc_validator(const T_Packet * pPacket, BitStream* pStartBitStrm, T_Packet_extention_function_potisions* pNullPos, BitStream* pEndBitStrm, int* pErrCode) {
    *pErrCode = 0;
    return TRUE;
}    
