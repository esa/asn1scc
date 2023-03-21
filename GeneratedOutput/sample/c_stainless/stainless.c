/* --------------------------- GenC requirements ----- */

#include <limits.h>
#if (__STDC_VERSION__ < 199901L) || (CHAR_BIT != 8)
#error "Your compiler does not meet the minimum requirements of GenC. Please see"
#error "https://epfl-lara.github.io/stainless/genc.html#requirements for more details."
#endif

/* ---------------------------- include header ------- */

#include "stainless.h"

/* ----------------------------------- includes ----- */

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>








/* ----------------------- function definitions ----- */

void Message_Initialize(Message* thiss_4) {
    thiss_4->msgId = 0;
    thiss_4->myflag = 0;
    Message_szDescription_Initialize(&thiss_4->szDescription);
    thiss_4->isReady = false;
}

bool Message_IsConstraintValid(Message* thiss_11, int32_t* pErrCode_1) {
    bool ret_1 = true;
    bool norm_1 = Message_szDescription_IsConstraintValid(&thiss_11->szDescription, pErrCode_1);
    ret_1 = norm_1;
    return ret_1;
}

void Message_szDescription_Initialize(array_int32* thiss_3) {
    int32_t stainless_buffer_0[1000] = { 0 };
    array_int32 norm_0 = (array_int32) { .data = stainless_buffer_0, .length = 1000 };
    *thiss_3 = norm_0;
}

bool Message_szDescription_IsConstraintValid(array_int32* thiss_1, int32_t* pErrCode_0) {
    bool ret_0 = true;
    *pErrCode_0 = 0;
    return ret_0;
}
