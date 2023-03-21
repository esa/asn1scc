#ifndef __STAINLESS_H__
#define __STAINLESS_H__

/* --------------------------- preprocessor macros ----- */

#define STAINLESS_FUNC_PURE
#if defined(__cplusplus)
#undef STAINLESS_FUNC_PURE
#define STAINLESS_FUNC_PURE _Pragma("FUNC_IS_PURE;")
#elif __GNUC__>=3
#undef STAINLESS_FUNC_PURE
#define STAINLESS_FUNC_PURE __attribute__((__pure__))
#elif defined(__has_attribute)
#if __has_attribute(pure)
#undef STAINLESS_FUNC_PURE
#define STAINLESS_FUNC_PURE __attribute__((__pure__))
#endif
#endif


/* ----------------------------------- includes ----- */

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>




/* ---------------------- data type definitions ----- */

typedef struct {
  int32_t* data;
  int32_t length;
} array_int32;

typedef struct {
  int32_t msgId;
  int32_t myflag;
  array_int32 szDescription;
  bool isReady;
} Message;



/* ---------------------- function declarations ----- */

void Message_Initialize(Message* thiss_4);
bool Message_IsConstraintValid(Message* thiss_11, int32_t* pErrCode_1);
void Message_szDescription_Initialize(array_int32* thiss_3);
bool Message_szDescription_IsConstraintValid(array_int32* thiss_1, int32_t* pErrCode_0);

#endif
