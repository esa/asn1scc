#include <milbus2.h>

asn1SccUint milbus2_encode(asn1SccUint val) { 
    return val == 32 ? 0 : val; 
} 

asn1SccUint milbus2_decode(asn1SccUint val) { 
    return val == 0 ? 32 : val; 
}