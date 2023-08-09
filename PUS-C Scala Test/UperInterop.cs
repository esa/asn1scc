namespace PUS_C_Scala_Test
{
    [TestClass]
    public class UperInterop
    {
        private void UPERInteropEncScalaDecC(PUS_C_Service s, string folderSuffix) =>
            new TestBasics().Run_TestService(s, folderSuffix, /*ServiceVariation.CREATE_SCALA | */
                ServiceVariation.CREATE_C | ServiceVariation.UPER | ServiceVariation.CREATE_TESTS | ServiceVariation.COMPARE_ENCODINGS);

        [TestMethod]
        public void TestService_01() => UPERInteropEncScalaDecC(PUS_C_Service.S1, "S1");
    }
}
