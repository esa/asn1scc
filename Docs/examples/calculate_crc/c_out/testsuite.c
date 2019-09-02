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

    test_case_ACN_000001(output, &totalErrors);

    test_case_ACN_000002(output, &totalErrors);

    test_case_ACN_000003(output, &totalErrors);

    test_case_ACN_000004(output, &totalErrors);

    test_case_ACN_000005(output, &totalErrors);

    test_case_ACN_000006(output, &totalErrors);

    test_case_ACN_000007(output, &totalErrors);

    test_case_ACN_000008(output, &totalErrors);

    test_case_ACN_000009(output, &totalErrors);

    test_case_ACN_000010(output, &totalErrors);

    test_case_ACN_000011(output, &totalErrors);

    test_case_ACN_000012(output, &totalErrors);

    test_case_ACN_000013(output, &totalErrors);

    test_case_ACN_000014(output, &totalErrors);

    test_case_ACN_000015(output, &totalErrors);

    test_case_ACN_000016(output, &totalErrors);

    output->report_suite_end();

    if (totalErrors > 0 ) {
        output->report_tests_failed(16, totalErrors);
        return 1;
    } else {
        output->report_all_tests_passed(16);
        return 0;
    }
}
