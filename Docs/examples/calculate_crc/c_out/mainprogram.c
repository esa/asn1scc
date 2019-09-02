#include <stdio.h>

#include "testsuite.h"

static void printf_tests_failed(int testCount, int failedCount)
{
    printf("%d out of %d failed.\n", failedCount, testCount);
}

static void printf_tests_passed(int testCount)
{
    printf("All test cases (%d) run successfully.\n", testCount);
}

static void printf_null()
{
}

static void printf_null_char(const char* s)
{
    (void)s;
}

static void printf_message(const char* message)
{
    printf("%s\n", message);
}

int main(int argc, char* argv[])
{
    (void)argc;
    (void)argv;

    TestOutput output = {
               .report_tests_failed = printf_tests_failed,
               .report_all_tests_passed = printf_tests_passed,
               .report_suite_begin = printf_null,
               .report_suite_end = printf_null,
               .report_case_begin = printf_null_char,
               .report_case_end = printf_null,
               .report_failure_begin = printf_null,
               .report_failure_end = printf_null,
               .report_failure_message = printf_message
    };

    return asn1scc_run_generated_testsuite(&output);
}
