namespace PUS_C_Scala_Test
{
    /// <summary>
    /// XER interop tests between C and Python backends.
    ///
    /// Design:
    /// - Both C and Python are generated with -XER -atc for the same PUS grammar.
    /// - C writes .xml files into the output folder root; Python writes .dat files into output/.
    /// - Comparison is normalized-XML + numeric-aware leaf equality (see CompareXerTestCases):
    ///   element local names, child order, attributes, and leaf text are compared;
    ///   leaf text is compared numerically if both sides parse as a number (relative tol ~1e-9)
    ///   to handle C's NR3 scientific notation (e.g. 3.14E0) vs Python repr (e.g. 3.14).
    /// - Python emits a min-size subset of test cases; we assert Python ⊆ C and compare
    ///   the intersection — do NOT assert equal file counts.
    ///
    /// Known skip reasons:
    ///   STRING_SIZEOF_BUG — Pre-existing C XER bug: String_decode template uses sizeof(pVal)
    ///     where pVal is a char[] function parameter (decays to char*), so sizeof returns
    ///     pointer size (8) instead of array size. Affects all services that include BasicTypes
    ///     (which declares PUSC-CHARSTR-VARIABLE-LEN as IA5String). Template: StgC/xer_c.stg
    ///     String_decode, line ~169. All other services pass once BasicTypes is absent.
    ///   S12_UNSUPPORTED — S12 is also unsupported in ACN interop; complex FMON structure.
    ///   NULLTERMINATED — readBits_nullterminated is broken in XER context.
    ///   ACN_ATTRIBUTES — No applicable XER-only grammar variant.
    ///   ADVANCED — Parameterized/import types require further XER investigation.
    ///   PRIMITIVES — Not yet validated for XER interop.
    ///   STRUCTURED — Not yet validated for XER interop.
    /// </summary>
    [TestClass]
    public class XerInteropPython
    {
        private static readonly ServiceVariation XerInteropFlags =
            ServiceVariation.CREATE_C |
            ServiceVariation.CREATE_PYTHON |
            ServiceVariation.XER |
            ServiceVariation.CREATE_TESTS |
            ServiceVariation.COMPARE_ENCODINGS;

        private void XerInteropPythonC(PUS_C_Service s, string folderSuffix) =>
            new TestBasics().Run_TestService(s, folderSuffix, XerInteropFlags);

        // S1 does not include BasicTypes; passes XER C compile and interop check.
        [TestMethod]
        public void TestService_01_C() => XerInteropPythonC(PUS_C_Service.S1, "S1");

        // S2–S6, S8, S9, S11, S13–S15, S18, S19 all include BasicTypes which has the
        // STRING_SIZEOF_BUG — sizeof(char[] param) returns pointer size; C compile fails.
        [TestMethod, Ignore] // STRING_SIZEOF_BUG in BasicTypes IA5String XER decode
        public void TestService_02_C() => XerInteropPythonC(PUS_C_Service.S2, "S2");

        [TestMethod, Ignore] // STRING_SIZEOF_BUG in BasicTypes
        public void TestService_03_C() => XerInteropPythonC(PUS_C_Service.S3, "S3");

        [TestMethod, Ignore] // STRING_SIZEOF_BUG in BasicTypes
        public void TestService_04_C() => XerInteropPythonC(PUS_C_Service.S4, "S4");

        [TestMethod, Ignore] // STRING_SIZEOF_BUG in BasicTypes
        public void TestService_05_C() => XerInteropPythonC(PUS_C_Service.S5, "S5");

        [TestMethod, Ignore] // STRING_SIZEOF_BUG in BasicTypes
        public void TestService_06_C() => XerInteropPythonC(PUS_C_Service.S6, "S6");

        [TestMethod, Ignore] // STRING_SIZEOF_BUG in BasicTypes
        public void TestService_08_C() => XerInteropPythonC(PUS_C_Service.S8, "S8");

        [TestMethod, Ignore] // STRING_SIZEOF_BUG in BasicTypes
        public void TestService_09_C() => XerInteropPythonC(PUS_C_Service.S9, "S9");

        [TestMethod, Ignore] // STRING_SIZEOF_BUG in BasicTypes
        public void TestService_11_C() => XerInteropPythonC(PUS_C_Service.S11, "S11");

        // S12 is also ignored in ACN interop; complex FMON structure not supported
        [TestMethod, Ignore] // S12_UNSUPPORTED + STRING_SIZEOF_BUG
        public void TestService_12_C() => XerInteropPythonC(PUS_C_Service.S12, "S12");

        [TestMethod, Ignore] // STRING_SIZEOF_BUG in BasicTypes
        public void TestService_13_C() => XerInteropPythonC(PUS_C_Service.S13, "S13");

        [TestMethod, Ignore] // STRING_SIZEOF_BUG in BasicTypes
        public void TestService_14_C() => XerInteropPythonC(PUS_C_Service.S14, "S14");

        [TestMethod, Ignore] // STRING_SIZEOF_BUG in BasicTypes
        public void TestService_15_C() => XerInteropPythonC(PUS_C_Service.S15, "S15");

        // S17 does not include BasicTypes; passes XER C compile and interop check.
        [TestMethod]
        public void TestService_17_C() => XerInteropPythonC(PUS_C_Service.S17, "S17");

        [TestMethod, Ignore] // STRING_SIZEOF_BUG in BasicTypes
        public void TestService_18_C() => XerInteropPythonC(PUS_C_Service.S18, "S18");

        [TestMethod, Ignore] // STRING_SIZEOF_BUG in BasicTypes
        public void TestService_19_C() => XerInteropPythonC(PUS_C_Service.S19, "S19");

        [TestMethod, Ignore] // ACN_ATTRIBUTES — no XER grammar variant applicable
        public void TestService_ACN_ATTRIBUTES_C() => XerInteropPythonC(PUS_C_Service.ACN_ATTRIBUTES, "ACN_Attributes");

        // Additional includes determinant_color (REAL type). The C XER REAL encoder/decoder
        // fails to round-trip extreme double values (DBL_MIN, DBL_MAX) — test cases 21 and 23
        // report errCode=4 "Encoded and decoded messages are different." Pre-existing C XER bug.
        [TestMethod, Ignore] // C XER REAL extreme-value round-trip bug (test cases 21, 23)
        public void TestService_Additional_C() => XerInteropPythonC(PUS_C_Service.ADDITIONAL, "Additional");

        [TestMethod, Ignore] // ADVANCED — parameterized/import types need further XER investigation
        public void TestService_Advanced_C() => XerInteropPythonC(PUS_C_Service.ADVANCED, "Advanced");

        [TestMethod, Ignore] // PRIMITIVES — not yet validated for XER interop
        public void TestService_Primitives_C() => XerInteropPythonC(PUS_C_Service.PRIMITIVES, "Primitives");

        [TestMethod, Ignore] // STRUCTURED — not yet validated for XER interop
        public void TestService_Structured_C() => XerInteropPythonC(PUS_C_Service.STRUCTURED, "Structured");

        // NULLTERMINATED — readBits_nullterminated broken in XER context too
        [TestMethod, Ignore]
        public void AdditionalTestCases_C() => XerInteropPythonC(PUS_C_Service.ADDITIONAL_TEST_CASES, "AdditionalTestCases");
    }
}
