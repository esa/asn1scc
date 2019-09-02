#ifndef GENERATED_ASN1SCC_TESTSUITE_H
#define GENERATED_ASN1SCC_TESTSUITE_H

#ifdef  __cplusplus
extern "C" {
#endif

typedef struct {
    void (*report_tests_failed)(int testsCount, int failedCount);
    void (*report_all_tests_passed)(int testsCount);

    void (*report_suite_begin)();
    void (*report_suite_end)();

    void (*report_case_begin)(const char* caseName);
    void (*report_case_end)();

    void (*report_failure_begin)();
    void (*report_failure_end)();
    void (*report_failure_message)(const char* message);
} TestOutput;

int asn1scc_run_generated_testsuite(TestOutput* output);

#ifdef  __cplusplus
}
#endif


#endif // GENERATED_ASN1SCC_TESTSUITE_H
