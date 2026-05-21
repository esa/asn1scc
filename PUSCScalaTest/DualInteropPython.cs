namespace PUS_C_Scala_Test
{
    [TestClass]
    public class DualInteropPython
    {
        private void DualInteropEncPythonC(PUS_C_Service s, string folderSuffix) =>
            new TestBasics().Run_TestService(s, folderSuffix, ServiceVariation.CREATE_PYTHON |
                ServiceVariation.CREATE_C | ServiceVariation.ACN | ServiceVariation.UPER | ServiceVariation.CREATE_TESTS | ServiceVariation.COMPARE_ENCODINGS);

        private void DualInteropEncPythonScala(PUS_C_Service s, string folderSuffix) =>
            new TestBasics().Run_TestService(s, folderSuffix, ServiceVariation.CREATE_PYTHON |
                ServiceVariation.CREATE_SCALA | ServiceVariation.ACN | ServiceVariation.UPER  | ServiceVariation.CREATE_TESTS | ServiceVariation.COMPARE_ENCODINGS);
        
        [TestMethod]
        public void TestService_01_C() => DualInteropEncPythonC(PUS_C_Service.S1, "S1");

        [TestMethod]
        public void TestService_02_C() => DualInteropEncPythonC(PUS_C_Service.S2, "S2");

        [TestMethod]
        public void TestService_03_C() => DualInteropEncPythonC(PUS_C_Service.S3, "S3");

        [TestMethod]
        public void TestService_04_C() => DualInteropEncPythonC(PUS_C_Service.S4, "S4");

        [TestMethod]
        public void TestService_05_C() => DualInteropEncPythonC(PUS_C_Service.S5, "S5");

        [TestMethod]
        public void TestService_06_C() => DualInteropEncPythonC(PUS_C_Service.S6, "S6");

        [TestMethod]
        public void TestService_08_C() => DualInteropEncPythonC(PUS_C_Service.S8, "S8");

        [TestMethod]
        public void TestService_09_C() => DualInteropEncPythonC(PUS_C_Service.S9, "S9");

        [TestMethod]
        public void TestService_11_C() => DualInteropEncPythonC(PUS_C_Service.S11, "S11");

        // TODO: not working for C and Scala
        [TestMethod, Ignore]
        public void TestService_12_C() => DualInteropEncPythonC(PUS_C_Service.S12, "S12");

        [TestMethod]
        public void TestService_13_C() => DualInteropEncPythonC(PUS_C_Service.S13, "S13");

        [TestMethod]
        public void TestService_14_C() => DualInteropEncPythonC(PUS_C_Service.S14, "S14");

        [TestMethod]
        public void TestService_15_C() => DualInteropEncPythonC(PUS_C_Service.S15, "S15");

        [TestMethod]
        public void TestService_17_C() => DualInteropEncPythonC(PUS_C_Service.S17, "S17");

        [TestMethod]
        public void TestService_18_C() => DualInteropEncPythonC(PUS_C_Service.S18, "S18");

        [TestMethod]
        public void TestService_19_C() => DualInteropEncPythonC(PUS_C_Service.S19, "S19");
        
        [TestMethod, Ignore]
        public void TestService_ACN_ATTRIBUTES_C() => DualInteropEncPythonC(PUS_C_Service.ACN_ATTRIBUTES, "ACN_Attributes");
        
        [TestMethod]
        public void TestService_Additional_C() => DualInteropEncPythonC(PUS_C_Service.ADDITIONAL, "Additional");
        
        [TestMethod, Ignore]
        public void TestService_Advanced_C() => DualInteropEncPythonC(PUS_C_Service.ADVANCED, "Advanced");
        
        [TestMethod, Ignore]
        public void TestService_Primitives_C() => DualInteropEncPythonC(PUS_C_Service.PRIMITIVES, "Primitives");
        
        [TestMethod, Ignore]
        public void TestService_Structured_C() => DualInteropEncPythonC(PUS_C_Service.STRUCTURED, "Structured");

        // TODO: uses readBits_nullterminated which is broken
        [TestMethod, Ignore]
        public void AdditionalTestCases_C() => DualInteropEncPythonC(PUS_C_Service.ADDITIONAL_TEST_CASES, "AdditionalTestCases");
        
        [TestMethod]
        public void TestService_01_Scala() => DualInteropEncPythonScala(PUS_C_Service.S1, "S1");
        
        [TestMethod]
        public void TestService_02_Scala() => DualInteropEncPythonScala(PUS_C_Service.S2, "S2");

        [TestMethod]
        public void TestService_03_Scala() => DualInteropEncPythonScala(PUS_C_Service.S3, "S3");

        [TestMethod]
        public void TestService_04_Scala() => DualInteropEncPythonScala(PUS_C_Service.S4, "S4");

        [TestMethod]
        public void TestService_05_Scala() => DualInteropEncPythonScala(PUS_C_Service.S5, "S5");

        [TestMethod]
        public void TestService_06_Scala() => DualInteropEncPythonScala(PUS_C_Service.S6, "S6");

        [TestMethod]
        public void TestService_08_Scala() => DualInteropEncPythonScala(PUS_C_Service.S8, "S8");

        [TestMethod]
        public void TestService_09_Scala() => DualInteropEncPythonScala(PUS_C_Service.S9, "S9");

        [TestMethod]
        public void TestService_11_Scala() => DualInteropEncPythonScala(PUS_C_Service.S11, "S11");
        
        [TestMethod, Ignore]
        public void TestService_12_Scala() => DualInteropEncPythonScala(PUS_C_Service.S12, "S12");
        
        [TestMethod]
        public void TestService_13_Scala() => DualInteropEncPythonScala(PUS_C_Service.S13, "S13");

        [TestMethod]
        public void TestService_14_Scala() => DualInteropEncPythonScala(PUS_C_Service.S14, "S14");

        [TestMethod]
        public void TestService_15_Scala() => DualInteropEncPythonScala(PUS_C_Service.S15, "S15");

        [TestMethod]
        public void TestService_17_Scala() => DualInteropEncPythonScala(PUS_C_Service.S17, "S17");

        [TestMethod]
        public void TestService_18_Scala() => DualInteropEncPythonScala(PUS_C_Service.S18, "S18");

        [TestMethod]
        public void TestService_19_Scala() => DualInteropEncPythonScala(PUS_C_Service.S19, "S19");
        
        [TestMethod, Ignore]
        public void TestService_ACN_ATTRIBUTES_Scala() => DualInteropEncPythonScala(PUS_C_Service.ACN_ATTRIBUTES, "ACN_Attributes");
        
        [TestMethod, Ignore]
        public void TestService_Additional_Scala() => DualInteropEncPythonScala(PUS_C_Service.ADDITIONAL, "Additional");
        
        [TestMethod, Ignore]
        public void TestService_Advanced_Scala() => DualInteropEncPythonScala(PUS_C_Service.ADVANCED, "Advanced");
        
        [TestMethod, Ignore]
        public void TestService_Primitives_Scala() => DualInteropEncPythonScala(PUS_C_Service.PRIMITIVES, "Primitives");
        
        [TestMethod, Ignore]
        public void TestService_Structured_Scala() => DualInteropEncPythonScala(PUS_C_Service.STRUCTURED, "Structured");
        
        [TestMethod, Ignore]
        public void AdditionalTestCases_Scala() => DualInteropEncPythonScala(PUS_C_Service.ADDITIONAL_TEST_CASES, "AdditionalTestCases");

    }
}
