namespace PUS_C_Scala_Test
{
    [TestClass]
    public class UperAtcScalaTests
    {
        private void UPERWithTestGenTestCall(PUS_C_Service s, string folderSuffix) => 
            new TestBasics().Run_TestService(s, folderSuffix, 
                ServiceVariation.CREATE_SCALA | ServiceVariation.UPER | ServiceVariation.CREATE_TESTS);

        [TestMethod]
        public void TestService_01() => UPERWithTestGenTestCall(PUS_C_Service.S1, "S1");

        [TestMethod]
        public void TestService_02() => UPERWithTestGenTestCall(PUS_C_Service.S2, "S2");

        [TestMethod]
        public void TestService_03() => UPERWithTestGenTestCall(PUS_C_Service.S3, "S3");

        [TestMethod]
        public void TestService_04() => UPERWithTestGenTestCall(PUS_C_Service.S4, "S4");

        [TestMethod]
        public void TestService_05() => UPERWithTestGenTestCall(PUS_C_Service.S5, "S5");

        [TestMethod]
        public void TestService_06() => UPERWithTestGenTestCall(PUS_C_Service.S6, "S6");

        [TestMethod]
        public void TestService_08() => UPERWithTestGenTestCall(PUS_C_Service.S8, "S8");

        [TestMethod]
        public void TestService_09() => UPERWithTestGenTestCall(PUS_C_Service.S9, "S9");


        [TestMethod]
        public void TestService_11() => UPERWithTestGenTestCall(PUS_C_Service.S11, "S11");


        [TestMethod]
        public void TestService_12() => UPERWithTestGenTestCall(PUS_C_Service.S12, "S12");


        [TestMethod]
        public void TestService_13() => UPERWithTestGenTestCall(PUS_C_Service.S13, "S13");


        [TestMethod]
        public void TestService_14() => UPERWithTestGenTestCall(PUS_C_Service.S14, "S14");


        [TestMethod]
        public void TestService_15() => UPERWithTestGenTestCall(PUS_C_Service.S15, "S15");


        [TestMethod]
        public void TestService_17() => UPERWithTestGenTestCall(PUS_C_Service.S17, "S17");


        [TestMethod]
        public void TestService_18() => UPERWithTestGenTestCall(PUS_C_Service.S18, "S18");


        [TestMethod]
        public void TestService_19() => UPERWithTestGenTestCall(PUS_C_Service.S19, "S19");
    }
}
