#include "test_case_001.h"

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <limits.h>
#include "asn1crt.h"

#include "a_auto_tcs.h"

void test_case_ACN_000001(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;
    int i1;

    output->report_case_begin("test_case_ACN_000001");
    {
        // dummy statement used for calling init functions
        static PacketHeader tmp0;
        PacketHeader_version_Initialize((&(tmp0.version)));
        PacketHeader_release_Initialize((&(tmp0.release)));
        PacketHeader_varSizeArray_elem_Initialize((&(tmp0.varSizeArray.arr[0])));
        PacketHeader_varSizeArray_Initialize((&(tmp0.varSizeArray)));
        PacketHeader_Initialize((&(tmp0)));
    }

    static PacketHeader tc_data; 
    /*set version */
    tc_data.version = 1;
    /*set release */
    tc_data.release = 1;
    /*set varSizeArray */
    i1 = 0;
    while (i1< 20) {
        if (i1 % 2 == 0)  {
            tc_data.varSizeArray.arr[i1] = 1;
        }
        else {
            tc_data.varSizeArray.arr[i1] = 20;
        }
        i1 = i1 + 1;
    }
    tc_data.varSizeArray.nCount = 20;

    result = PacketHeader_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000001");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000001 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000001' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000001' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000001' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000001'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000002(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;
    int i1;

    output->report_case_begin("test_case_ACN_000002");
    {
        // dummy statement used for calling init functions
        static PacketHeader tmp0;
        PacketHeader_version_Initialize((&(tmp0.version)));
        PacketHeader_release_Initialize((&(tmp0.release)));
        PacketHeader_varSizeArray_elem_Initialize((&(tmp0.varSizeArray.arr[0])));
        PacketHeader_varSizeArray_Initialize((&(tmp0.varSizeArray)));
        PacketHeader_Initialize((&(tmp0)));
    }

    static PacketHeader tc_data; 
    /*set version */
    tc_data.version = 100;
    /*set release */
    tc_data.release = 100;
    /*set varSizeArray */
    i1 = 0;
    while (i1< 1) {
        if (i1 % 2 == 0)  {
            tc_data.varSizeArray.arr[i1] = 1;
        }
        else {
            tc_data.varSizeArray.arr[i1] = 20;
        }
        i1 = i1 + 1;
    }
    tc_data.varSizeArray.nCount = 1;

    result = PacketHeader_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000002");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000002 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000002' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000002' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000002' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000002'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000003(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;

    output->report_case_begin("test_case_ACN_000003");
    {
        // dummy statement used for calling init functions
        static PacketBody tmp0;
        PacketBody_anInteger_Initialize((&(tmp0.u.anInteger)));
        PacketBody_anotherSizeArray_elem_Initialize((&(tmp0.u.anotherSizeArray.arr[0])));
        PacketBody_anotherSizeArray_Initialize((&(tmp0.u.anotherSizeArray)));
        PacketBody_Initialize((&(tmp0)));
    }

    static PacketBody tc_data; 
    /*set aReal*/
    tc_data.kind = aReal_PRESENT;
    tc_data.u.aReal = -1.79769313486231570000E+308;

    result = PacketBody_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000003");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000003 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000003' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000003' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000003' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000003'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000004(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;

    output->report_case_begin("test_case_ACN_000004");
    {
        // dummy statement used for calling init functions
        static PacketBody tmp0;
        PacketBody_anInteger_Initialize((&(tmp0.u.anInteger)));
        PacketBody_anotherSizeArray_elem_Initialize((&(tmp0.u.anotherSizeArray.arr[0])));
        PacketBody_anotherSizeArray_Initialize((&(tmp0.u.anotherSizeArray)));
        PacketBody_Initialize((&(tmp0)));
    }

    static PacketBody tc_data; 
    /*set aReal*/
    tc_data.kind = aReal_PRESENT;
    tc_data.u.aReal = 0.00000000000000000000E+000;

    result = PacketBody_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000004");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000004 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000004' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000004' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000004' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000004'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000005(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;

    output->report_case_begin("test_case_ACN_000005");
    {
        // dummy statement used for calling init functions
        static PacketBody tmp0;
        PacketBody_anInteger_Initialize((&(tmp0.u.anInteger)));
        PacketBody_anotherSizeArray_elem_Initialize((&(tmp0.u.anotherSizeArray.arr[0])));
        PacketBody_anotherSizeArray_Initialize((&(tmp0.u.anotherSizeArray)));
        PacketBody_Initialize((&(tmp0)));
    }

    static PacketBody tc_data; 
    /*set aReal*/
    tc_data.kind = aReal_PRESENT;
    tc_data.u.aReal = 1.79769313486231570000E+308;

    result = PacketBody_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000005");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000005 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000005' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000005' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000005' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000005'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000006(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;

    output->report_case_begin("test_case_ACN_000006");
    {
        // dummy statement used for calling init functions
        static PacketBody tmp0;
        PacketBody_anInteger_Initialize((&(tmp0.u.anInteger)));
        PacketBody_anotherSizeArray_elem_Initialize((&(tmp0.u.anotherSizeArray.arr[0])));
        PacketBody_anotherSizeArray_Initialize((&(tmp0.u.anotherSizeArray)));
        PacketBody_Initialize((&(tmp0)));
    }

    static PacketBody tc_data; 
    /*set anInteger*/
    tc_data.kind = anInteger_PRESENT;
    tc_data.u.anInteger = 0;

    result = PacketBody_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000006");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000006 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000006' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000006' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000006' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000006'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000007(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;

    output->report_case_begin("test_case_ACN_000007");
    {
        // dummy statement used for calling init functions
        static PacketBody tmp0;
        PacketBody_anInteger_Initialize((&(tmp0.u.anInteger)));
        PacketBody_anotherSizeArray_elem_Initialize((&(tmp0.u.anotherSizeArray.arr[0])));
        PacketBody_anotherSizeArray_Initialize((&(tmp0.u.anotherSizeArray)));
        PacketBody_Initialize((&(tmp0)));
    }

    static PacketBody tc_data; 
    /*set anInteger*/
    tc_data.kind = anInteger_PRESENT;
    tc_data.u.anInteger = 65535;

    result = PacketBody_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000007");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000007 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000007' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000007' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000007' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000007'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000008(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;
    int i1;

    output->report_case_begin("test_case_ACN_000008");
    {
        // dummy statement used for calling init functions
        static PacketBody tmp0;
        PacketBody_anInteger_Initialize((&(tmp0.u.anInteger)));
        PacketBody_anotherSizeArray_elem_Initialize((&(tmp0.u.anotherSizeArray.arr[0])));
        PacketBody_anotherSizeArray_Initialize((&(tmp0.u.anotherSizeArray)));
        PacketBody_Initialize((&(tmp0)));
    }

    static PacketBody tc_data; 
    /*set anotherSizeArray*/
    tc_data.kind = anotherSizeArray_PRESENT;
    i1 = 0;
    while (i1< 100) {
        if (i1 % 2 == 0)  {
            tc_data.u.anotherSizeArray.arr[i1] = 1;
        }
        else {
            tc_data.u.anotherSizeArray.arr[i1] = 200;
        }
        i1 = i1 + 1;
    }
    tc_data.u.anotherSizeArray.nCount = 100;

    result = PacketBody_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000008");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000008 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000008' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000008' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000008' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000008'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000009(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;
    int i1;

    output->report_case_begin("test_case_ACN_000009");
    {
        // dummy statement used for calling init functions
        static PacketBody tmp0;
        PacketBody_anInteger_Initialize((&(tmp0.u.anInteger)));
        PacketBody_anotherSizeArray_elem_Initialize((&(tmp0.u.anotherSizeArray.arr[0])));
        PacketBody_anotherSizeArray_Initialize((&(tmp0.u.anotherSizeArray)));
        PacketBody_Initialize((&(tmp0)));
    }

    static PacketBody tc_data; 
    /*set anotherSizeArray*/
    tc_data.kind = anotherSizeArray_PRESENT;
    i1 = 0;
    while (i1< 1) {
        if (i1 % 2 == 0)  {
            tc_data.u.anotherSizeArray.arr[i1] = 1;
        }
        else {
            tc_data.u.anotherSizeArray.arr[i1] = 200;
        }
        i1 = i1 + 1;
    }
    tc_data.u.anotherSizeArray.nCount = 1;

    result = PacketBody_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000009");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000009 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000009' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000009' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000009' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000009'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000010(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;
    int i1;

    output->report_case_begin("test_case_ACN_000010");
    {
        // dummy statement used for calling init functions
        static Packet tmp0;
        Packet_Initialize((&(tmp0)));
    }

    static Packet tc_data; 
    /*set p_header */
    /*set version */
    tc_data.p_header.version = 1;
    /*set release */
    tc_data.p_header.release = 1;
    /*set varSizeArray */
    i1 = 0;
    while (i1< 20) {
        if (i1 % 2 == 0)  {
            tc_data.p_header.varSizeArray.arr[i1] = 1;
        }
        else {
            tc_data.p_header.varSizeArray.arr[i1] = 20;
        }
        i1 = i1 + 1;
    }
    tc_data.p_header.varSizeArray.nCount = 20;
    /*set p_body */
    /*set aReal*/
    tc_data.p_body.kind = aReal_PRESENT;
    tc_data.p_body.u.aReal = -1.79769313486231570000E+308;

    result = Packet_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000010");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000010 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000010' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000010' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000010' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000010'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000011(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;
    int i1;

    output->report_case_begin("test_case_ACN_000011");
    {
        // dummy statement used for calling init functions
        static Packet tmp0;
        Packet_Initialize((&(tmp0)));
    }

    static Packet tc_data; 
    /*set p_header */
    /*set version */
    tc_data.p_header.version = 100;
    /*set release */
    tc_data.p_header.release = 100;
    /*set varSizeArray */
    i1 = 0;
    while (i1< 1) {
        if (i1 % 2 == 0)  {
            tc_data.p_header.varSizeArray.arr[i1] = 1;
        }
        else {
            tc_data.p_header.varSizeArray.arr[i1] = 20;
        }
        i1 = i1 + 1;
    }
    tc_data.p_header.varSizeArray.nCount = 1;
    /*set p_body */
    /*set aReal*/
    tc_data.p_body.kind = aReal_PRESENT;
    tc_data.p_body.u.aReal = 0.00000000000000000000E+000;

    result = Packet_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000011");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000011 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000011' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000011' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000011' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000011'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000012(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;
    int i1;

    output->report_case_begin("test_case_ACN_000012");
    {
        // dummy statement used for calling init functions
        static Packet tmp0;
        Packet_Initialize((&(tmp0)));
    }

    static Packet tc_data; 
    /*set p_header */
    /*set version */
    tc_data.p_header.version = 1;
    /*set release */
    tc_data.p_header.release = 1;
    /*set varSizeArray */
    i1 = 0;
    while (i1< 20) {
        if (i1 % 2 == 0)  {
            tc_data.p_header.varSizeArray.arr[i1] = 1;
        }
        else {
            tc_data.p_header.varSizeArray.arr[i1] = 20;
        }
        i1 = i1 + 1;
    }
    tc_data.p_header.varSizeArray.nCount = 20;
    /*set p_body */
    /*set aReal*/
    tc_data.p_body.kind = aReal_PRESENT;
    tc_data.p_body.u.aReal = 1.79769313486231570000E+308;

    result = Packet_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000012");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000012 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000012' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000012' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000012' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000012'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000013(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;
    int i1;

    output->report_case_begin("test_case_ACN_000013");
    {
        // dummy statement used for calling init functions
        static Packet tmp0;
        Packet_Initialize((&(tmp0)));
    }

    static Packet tc_data; 
    /*set p_header */
    /*set version */
    tc_data.p_header.version = 100;
    /*set release */
    tc_data.p_header.release = 100;
    /*set varSizeArray */
    i1 = 0;
    while (i1< 1) {
        if (i1 % 2 == 0)  {
            tc_data.p_header.varSizeArray.arr[i1] = 1;
        }
        else {
            tc_data.p_header.varSizeArray.arr[i1] = 20;
        }
        i1 = i1 + 1;
    }
    tc_data.p_header.varSizeArray.nCount = 1;
    /*set p_body */
    /*set anInteger*/
    tc_data.p_body.kind = anInteger_PRESENT;
    tc_data.p_body.u.anInteger = 0;

    result = Packet_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000013");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000013 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000013' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000013' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000013' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000013'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000014(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;
    int i1;

    output->report_case_begin("test_case_ACN_000014");
    {
        // dummy statement used for calling init functions
        static Packet tmp0;
        Packet_Initialize((&(tmp0)));
    }

    static Packet tc_data; 
    /*set p_header */
    /*set version */
    tc_data.p_header.version = 1;
    /*set release */
    tc_data.p_header.release = 1;
    /*set varSizeArray */
    i1 = 0;
    while (i1< 20) {
        if (i1 % 2 == 0)  {
            tc_data.p_header.varSizeArray.arr[i1] = 1;
        }
        else {
            tc_data.p_header.varSizeArray.arr[i1] = 20;
        }
        i1 = i1 + 1;
    }
    tc_data.p_header.varSizeArray.nCount = 20;
    /*set p_body */
    /*set anInteger*/
    tc_data.p_body.kind = anInteger_PRESENT;
    tc_data.p_body.u.anInteger = 65535;

    result = Packet_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000014");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000014 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000014' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000014' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000014' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000014'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000015(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;
    int i1;

    output->report_case_begin("test_case_ACN_000015");
    {
        // dummy statement used for calling init functions
        static Packet tmp0;
        Packet_Initialize((&(tmp0)));
    }

    static Packet tc_data; 
    /*set p_header */
    /*set version */
    tc_data.p_header.version = 100;
    /*set release */
    tc_data.p_header.release = 100;
    /*set varSizeArray */
    i1 = 0;
    while (i1< 1) {
        if (i1 % 2 == 0)  {
            tc_data.p_header.varSizeArray.arr[i1] = 1;
        }
        else {
            tc_data.p_header.varSizeArray.arr[i1] = 20;
        }
        i1 = i1 + 1;
    }
    tc_data.p_header.varSizeArray.nCount = 1;
    /*set p_body */
    /*set anotherSizeArray*/
    tc_data.p_body.kind = anotherSizeArray_PRESENT;
    i1 = 0;
    while (i1< 100) {
        if (i1 % 2 == 0)  {
            tc_data.p_body.u.anotherSizeArray.arr[i1] = 1;
        }
        else {
            tc_data.p_body.u.anotherSizeArray.arr[i1] = 200;
        }
        i1 = i1 + 1;
    }
    tc_data.p_body.u.anotherSizeArray.nCount = 100;

    result = Packet_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000015");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000015 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000015' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000015' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000015' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000015'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}

void test_case_ACN_000016(const TestOutput* output, int *totalErrors)
{
    flag result;
    int errCode;
    int i1;

    output->report_case_begin("test_case_ACN_000016");
    {
        // dummy statement used for calling init functions
        static Packet tmp0;
        Packet_Initialize((&(tmp0)));
    }

    static Packet tc_data; 
    /*set p_header */
    /*set version */
    tc_data.p_header.version = 1;
    /*set release */
    tc_data.p_header.release = 1;
    /*set varSizeArray */
    i1 = 0;
    while (i1< 20) {
        if (i1 % 2 == 0)  {
            tc_data.p_header.varSizeArray.arr[i1] = 1;
        }
        else {
            tc_data.p_header.varSizeArray.arr[i1] = 20;
        }
        i1 = i1 + 1;
    }
    tc_data.p_header.varSizeArray.nCount = 20;
    /*set p_body */
    /*set anotherSizeArray*/
    tc_data.p_body.kind = anotherSizeArray_PRESENT;
    i1 = 0;
    while (i1< 1) {
        if (i1 % 2 == 0)  {
            tc_data.p_body.u.anotherSizeArray.arr[i1] = 1;
        }
        else {
            tc_data.p_body.u.anotherSizeArray.arr[i1] = 200;
        }
        i1 = i1 + 1;
    }
    tc_data.p_body.u.anotherSizeArray.nCount = 1;

    result = Packet_ACN_enc_dec(&tc_data, &errCode, "test_case_ACN_000016");
    if (!result) {
        output->report_failure_begin();

        switch(errCode)
        {
        case 1:
            output->report_failure_message("Test case test_case_ACN_000016 failed in encoding.");
            break;
        case 2:
            output->report_failure_message("Test case 'test_case_ACN_000016' failed in decoding.");
            break;
        case 3:
            output->report_failure_message("Test case 'test_case_ACN_000016' failed in the validation of the decoded message.");
            break;
        case 4:
            output->report_failure_message("Test case 'test_case_ACN_000016' failed. Encoded and decoded messages are different.");
            break;
        default:
            output->report_failure_message("Unexpected error code in test case 'test_case_ACN_000016'.");
        }
        output->report_failure_message("========================================");
        *totalErrors = (*totalErrors) + 1;

        output->report_failure_end();
    }

    output->report_case_end();
}
