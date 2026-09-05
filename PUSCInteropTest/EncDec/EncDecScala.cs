namespace PUS_C_Interop_Test;

[TestClass]
public class EncDecScala
{
    private static void Check(PUS_C_Service s, Enc e) =>
        Assert.IsTrue(BuildCache.EnsureBuilt(new BuildKey(s, Lang.Scala, e)).TestsPassed,
            $"Scala {e} {s} enc/dec failed");

    [TestMethod] public void S1_UPER() => Check(PUS_C_Service.S1, Enc.UPER);
    [TestMethod] public void S2_UPER() => Check(PUS_C_Service.S2, Enc.UPER);
    [TestMethod] public void S3_UPER() => Check(PUS_C_Service.S3, Enc.UPER);
    [TestMethod] public void S4_UPER() => Check(PUS_C_Service.S4, Enc.UPER);
    [TestMethod] public void S5_UPER() => Check(PUS_C_Service.S5, Enc.UPER);
    [TestMethod] public void S6_UPER() => Check(PUS_C_Service.S6, Enc.UPER);
    [TestMethod] public void S8_UPER() => Check(PUS_C_Service.S8, Enc.UPER);
    [TestMethod] public void S9_UPER() => Check(PUS_C_Service.S9, Enc.UPER);
    [TestMethod] public void S11_UPER() => Check(PUS_C_Service.S11, Enc.UPER);
    [TestMethod, Ignore] public void S12_UPER() => Check(PUS_C_Service.S12, Enc.UPER); // S12 TODO: not working for C/Scala
    [TestMethod] public void S13_UPER() => Check(PUS_C_Service.S13, Enc.UPER);
    [TestMethod] public void S14_UPER() => Check(PUS_C_Service.S14, Enc.UPER);
    [TestMethod] public void S15_UPER() => Check(PUS_C_Service.S15, Enc.UPER);
    [TestMethod] public void S17_UPER() => Check(PUS_C_Service.S17, Enc.UPER);
    [TestMethod] public void S18_UPER() => Check(PUS_C_Service.S18, Enc.UPER);
    [TestMethod] public void S19_UPER() => Check(PUS_C_Service.S19, Enc.UPER);

    [TestMethod] public void S1_ACN() => Check(PUS_C_Service.S1, Enc.ACN);
    [TestMethod] public void S2_ACN() => Check(PUS_C_Service.S2, Enc.ACN);
    [TestMethod] public void S3_ACN() => Check(PUS_C_Service.S3, Enc.ACN);
    [TestMethod] public void S4_ACN() => Check(PUS_C_Service.S4, Enc.ACN);
    [TestMethod] public void S5_ACN() => Check(PUS_C_Service.S5, Enc.ACN);
    [TestMethod] public void S6_ACN() => Check(PUS_C_Service.S6, Enc.ACN);
    [TestMethod] public void S8_ACN() => Check(PUS_C_Service.S8, Enc.ACN);
    [TestMethod] public void S9_ACN() => Check(PUS_C_Service.S9, Enc.ACN);
    [TestMethod] public void S11_ACN() => Check(PUS_C_Service.S11, Enc.ACN);
    [TestMethod, Ignore] public void S12_ACN() => Check(PUS_C_Service.S12, Enc.ACN); // S12 TODO: not working for C/Scala
    [TestMethod] public void S13_ACN() => Check(PUS_C_Service.S13, Enc.ACN);
    [TestMethod] public void S14_ACN() => Check(PUS_C_Service.S14, Enc.ACN);
    [TestMethod] public void S15_ACN() => Check(PUS_C_Service.S15, Enc.ACN);
    [TestMethod] public void S17_ACN() => Check(PUS_C_Service.S17, Enc.ACN);
    [TestMethod] public void S18_ACN() => Check(PUS_C_Service.S18, Enc.ACN);
    [TestMethod] public void S19_ACN() => Check(PUS_C_Service.S19, Enc.ACN);
}
