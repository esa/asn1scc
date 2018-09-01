#include "testsuite.h"

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <limits.h>

#include "asn1crt.h"

#include "test_case_001.h"

int asn1scc_run_generated_testsuite(TestOutput* output)
{
    int totalErrors = 0;


    output->report_suite_begin();

    test_case_UPER_000001(output, &totalErrors);

    output->report_suite_end();

    if (totalErrors > 0 ) {
        output->report_tests_failed(1, totalErrors);
        return 1;
    } else {
        output->report_all_tests_passed(1);
        return 0;
    }
}
