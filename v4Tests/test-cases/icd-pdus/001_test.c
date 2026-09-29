/* Links only if every function the PDU's functions call was generated. */
#include "001.h"

int main(void)
{
    Rec value;
    int error = 0;
    Rec_Initialize(&value);
    return Rec_IsConstraintValid(&value, &error) ? 0 : 1;
}
