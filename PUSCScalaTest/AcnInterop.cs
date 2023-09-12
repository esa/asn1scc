namespace PUS_C_Scala_Test
{
    [TestClass]
    public class AcnInterop
    {
        private void UPERInteropEncScalaDecC(PUS_C_Service s, string folderSuffix) =>
            new TestBasics().Run_TestService(s, folderSuffix, ServiceVariation.CREATE_SCALA | 
                ServiceVariation.CREATE_C | ServiceVariation.ACN | ServiceVariation.CREATE_TESTS | ServiceVariation.COMPARE_ENCODINGS);

//        [TestMethod]
//        public void TestService_01() => UPERInteropEncScalaDecC(PUS_C_Service.S1, "S1");
//
//        [TestMethod]
//        public void TestService_02() => UPERInteropEncScalaDecC(PUS_C_Service.S2, "S2");
//
//        [TestMethod]
//        public void TestService_03() => UPERInteropEncScalaDecC(PUS_C_Service.S3, "S3");
//
//        [TestMethod]
//        public void TestService_04() => UPERInteropEncScalaDecC(PUS_C_Service.S4, "S4");
//
//        [TestMethod]
//        public void TestService_05() => UPERInteropEncScalaDecC(PUS_C_Service.S5, "S5");
//
//        [TestMethod]
//        public void TestService_06() => UPERInteropEncScalaDecC(PUS_C_Service.S6, "S6");
//
//        [TestMethod]
//        public void TestService_08() => UPERInteropEncScalaDecC(PUS_C_Service.S8, "S8");
//
//        [TestMethod]
//        public void TestService_09() => UPERInteropEncScalaDecC(PUS_C_Service.S9, "S9");
//
//        [TestMethod]
//        public void TestService_11() => UPERInteropEncScalaDecC(PUS_C_Service.S11, "S11");
//
//        //[TestMethod]
//        //public void TestService_12() => UPERInteropEncScalaDecC(PUS_C_Service.S12, "S12");
//
//        [TestMethod]
//        public void TestService_13() => UPERInteropEncScalaDecC(PUS_C_Service.S13, "S13");
//
//        //[TestMethod]
//        //public void TestService_14() => UPERInteropEncScalaDecC(PUS_C_Service.S14, "S14");
//
//        [TestMethod]
//        public void TestService_15() => UPERInteropEncScalaDecC(PUS_C_Service.S15, "S15");
//
//        [TestMethod]
//        public void TestService_17() => UPERInteropEncScalaDecC(PUS_C_Service.S17, "S17");
//
//        [TestMethod]
//        public void TestService_18() => UPERInteropEncScalaDecC(PUS_C_Service.S18, "S18");
//
//        [TestMethod]
//        public void TestService_19() => UPERInteropEncScalaDecC(PUS_C_Service.S19, "S19");
    }
}
