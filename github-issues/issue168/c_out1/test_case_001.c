#include "test_case_001.h"

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <limits.h>
#include "asn1crt.h"

#include "a_auto_tcs.h"

void test_case_UPER_000001(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;
    int i1;

    output->report_case_begin("test_case_UPER_000001");
    {
        // dummy statement used for calling init functions
        static OOO tmp0;
        OOO_Initialize(&tmp0);
    }

    static OOO tc_data; 
    i1 = 0;
    while (i1< 24/8) {
        tc_data.arr[i1] = 0x55; /* --> 0101 0101 as in Ada*/
        i1 = i1 + 1;
    }


    result = OOO_enc_dec(&tc_data, &errCode);
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_UPER_000001 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_UPER_000001' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_UPER_000001' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_UPER_000001' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_UPER_000001'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}
