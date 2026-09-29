#include <assert.h>

#include "020.h"

int main(void)
{
    byte invalid_wire[] = {1, 1};
    byte valid_wire[] = {1, 0};
    BitStream stream;
    W value;
    int error = 0;

    W_Initialize(&value);
    value.body.field = 1;
    assert(!W_IsConstraintValid(&value, &error));

    W_Initialize(&value);
    BitStream_AttachBuffer(&stream, invalid_wire, sizeof invalid_wire);
    error = 0;
    assert(!W_ACN_Decode(&value, &stream, &error));

    W_Initialize(&value);
    BitStream_AttachBuffer(&stream, valid_wire, sizeof valid_wire);
    error = 0;
    assert(W_ACN_Decode(&value, &stream, &error));

    return 0;
}
