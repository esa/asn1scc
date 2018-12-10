#include "out/dataview-uniq.h"
#include <stdio.h>

/* ---------------------------------------------------------------------- */
/* Use this to enable/disable the wrong behaviour */
/* ---------------------------------------------------------------------- */
#define ALLOW_BUG_SHOW_UP   1
/* ---------------------------------------------------------------------- */

#if ALLOW_BUG_SHOW_UP
/* currently (wrongly) set to 49 */
# define BUF_SIZE T_Boolean_Seq_REQUIRED_BYTES_FOR_XER_ENCODING
#else
/* to hold "<T-Boolean-Seq><T-Boolean><false/></T-Boolean></T-Boolean-Seq>" 
   62 bytes are needed */
# define BUF_SIZE 62
#endif

int main()
{
  T_Boolean_Seq vec;
  ByteStream bs;
  byte buf[BUF_SIZE];
  
  int res;
  int err;
  
  T_Boolean_Seq_Initialize(&vec);
  
  ByteStream_Init(&bs, buf, BUF_SIZE);

  res = T_Boolean_Seq_XER_Encode(&vec, &bs, &err, true);

  if (!res) {
    printf("Error: %d\n", err);
    res = 1;
  }
  else {
    printf("Buffer is:\n");
    printf((char*) buf);
    printf("\n");
    res = 0;
  }

  return res;  
}
