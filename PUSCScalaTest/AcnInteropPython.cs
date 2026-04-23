namespace PUS_C_Scala_Test
{
    [TestClass]
    public class AcnInteropPython
    {
        private void ACNInteropEncPythonC(PUS_C_Service s, string folderSuffix) =>
            new TestBasics().Run_TestService(s, folderSuffix, ServiceVariation.CREATE_PYTHON |
                ServiceVariation.CREATE_C | ServiceVariation.ACN | ServiceVariation.CREATE_TESTS | ServiceVariation.COMPARE_ENCODINGS);

        private void ACNInteropEncPythonScala(PUS_C_Service s, string folderSuffix) =>
            new TestBasics().Run_TestService(s, folderSuffix, ServiceVariation.CREATE_PYTHON |
                                                              ServiceVariation.CREATE_SCALA | ServiceVariation.ACN | ServiceVariation.CREATE_TESTS | ServiceVariation.COMPARE_ENCODINGS);
        
        [TestMethod]
        public void TestService_01_C() => ACNInteropEncPythonC(PUS_C_Service.S1, "S1");

        [TestMethod]
        public void TestService_02_C() => ACNInteropEncPythonC(PUS_C_Service.S2, "S2");

        [TestMethod]
        public void TestService_03_C() => ACNInteropEncPythonC(PUS_C_Service.S3, "S3");

        [TestMethod]
        public void TestService_04_C() => ACNInteropEncPythonC(PUS_C_Service.S4, "S4");

        [TestMethod]
        public void TestService_05_C() => ACNInteropEncPythonC(PUS_C_Service.S5, "S5");

        [TestMethod]
        public void TestService_06_C() => ACNInteropEncPythonC(PUS_C_Service.S6, "S6");

        [TestMethod]
        public void TestService_08_C() => ACNInteropEncPythonC(PUS_C_Service.S8, "S8");

        [TestMethod]
        public void TestService_09_C() => ACNInteropEncPythonC(PUS_C_Service.S9, "S9");

        [TestMethod]
        public void TestService_11_C() => ACNInteropEncPythonC(PUS_C_Service.S11, "S11");

        // TODO: not working for C and Scala
        //[TestMethod]
        //public void TestService_12() => ACNInteropEncScalaDecC(PUS_C_Service.S12, "S12");

        [TestMethod]
        public void TestService_13_C() => ACNInteropEncPythonC(PUS_C_Service.S13, "S13");

        [TestMethod]
        public void TestService_14_C() => ACNInteropEncPythonC(PUS_C_Service.S14, "S14");

        [TestMethod]
        public void TestService_15_C() => ACNInteropEncPythonC(PUS_C_Service.S15, "S15");

        [TestMethod]
        public void TestService_17_C() => ACNInteropEncPythonC(PUS_C_Service.S17, "S17");

        [TestMethod]
        public void TestService_18_C() => ACNInteropEncPythonC(PUS_C_Service.S18, "S18");

        [TestMethod]
        public void TestService_19_C() => ACNInteropEncPythonC(PUS_C_Service.S19, "S19");

        // TODO: uses readBits_nullterminated which is broken
        // [TestMethod]
        // public void AdditionalTestCases() => ACNInteropEncScalaDecC(PUS_C_Service.ADDITIONAL_TEST_CASES, "AdditionalTestCases");
        
        [TestMethod]
        public void TestService_01_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S1, "S1");
        
        [TestMethod]
        public void TestService_02_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S2, "S2");

        [TestMethod]
        public void TestService_03_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S3, "S3");

        [TestMethod]
        public void TestService_04_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S4, "S4");

        [TestMethod]
        public void TestService_05_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S5, "S5");

        [TestMethod]
        public void TestService_06_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S6, "S6");

        [TestMethod]
        public void TestService_08_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S8, "S8");

        [TestMethod]
        public void TestService_09_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S9, "S9");

        [TestMethod]
        public void TestService_11_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S11, "S11");
        
        [TestMethod]
        public void TestService_13_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S13, "S13");

        [TestMethod]
        public void TestService_14_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S14, "S14");

        [TestMethod]
        public void TestService_15_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S15, "S15");

        [TestMethod]
        public void TestService_17_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S17, "S17");

        [TestMethod]
        public void TestService_18_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S18, "S18");

        [TestMethod]
        public void TestService_19_Scala() => ACNInteropEncPythonScala(PUS_C_Service.S19, "S19");

    }
}
