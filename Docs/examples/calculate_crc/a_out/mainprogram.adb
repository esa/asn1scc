WITH Ada.Text_IO;
WITH adaasn1rtl; use type adaasn1rtl.Asn1UInt; use type adaasn1rtl.Asn1Int;use type adaasn1rtl.BIT;
with test_case_001;
use test_case_001;
--# inherit adaasn1rtl, test_case_001;
--# main_program;


FUNCTION MainProgram RETURN INTEGER
IS --# hide MainProgram
    USE Ada.Text_IO;
    totalErrors  : INTEGER:=0;
    
BEGIN

    test_case_ACN_000001(totalErrors);

    test_case_ACN_000002(totalErrors);

    test_case_ACN_000003(totalErrors);

    test_case_ACN_000004(totalErrors);

    test_case_ACN_000005(totalErrors);

    test_case_ACN_000006(totalErrors);

    test_case_ACN_000007(totalErrors);

    test_case_ACN_000008(totalErrors);

    test_case_ACN_000009(totalErrors);

    test_case_ACN_000010(totalErrors);

    test_case_ACN_000011(totalErrors);

    test_case_ACN_000012(totalErrors);

    test_case_ACN_000013(totalErrors);

    test_case_ACN_000014(totalErrors);

    test_case_ACN_000015(totalErrors);

    test_case_ACN_000016(totalErrors);

    -- used to increase statement coverage


    if totalErrors > 0 THEN
        Put_Line (Integer'Image(totalErrors) & " out of 16 failed."); 
        RETURN 1;
    ELSE
        Put_Line ("All test cases (16) run successfully."); 
        RETURN 0;
    end if;
end MainProgram;