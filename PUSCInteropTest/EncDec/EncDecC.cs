namespace PUS_C_Interop_Test;

[TestClass]
public class EncDecC
{
    private static void Check(PUS_C_Service s, Enc e) =>
        Assert.IsTrue(BuildCache.EnsureBuilt(new BuildKey(s, Lang.C, e)).TestsPassed,
            $"C {e} {s} enc/dec failed");

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

    // XER: only S1 and S17 lack BasicTypes; all other services hit the C XER
    // STRING_SIZEOF_BUG (sizeof(char[] param) returns pointer size) at compile.
    [TestMethod] public void S1_XER() => Check(PUS_C_Service.S1, Enc.XER);
    [TestMethod] public void S17_XER() => Check(PUS_C_Service.S17, Enc.XER);
    [TestMethod, Ignore] public void S2_XER() => Check(PUS_C_Service.S2, Enc.XER);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S3_XER() => Check(PUS_C_Service.S3, Enc.XER);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S4_XER() => Check(PUS_C_Service.S4, Enc.XER);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S5_XER() => Check(PUS_C_Service.S5, Enc.XER);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S6_XER() => Check(PUS_C_Service.S6, Enc.XER);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S8_XER() => Check(PUS_C_Service.S8, Enc.XER);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S9_XER() => Check(PUS_C_Service.S9, Enc.XER);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S11_XER() => Check(PUS_C_Service.S11, Enc.XER); // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S12_XER() => Check(PUS_C_Service.S12, Enc.XER); // S12 TODO + STRING_SIZEOF_BUG
    [TestMethod, Ignore] public void S13_XER() => Check(PUS_C_Service.S13, Enc.XER); // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S14_XER() => Check(PUS_C_Service.S14, Enc.XER); // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S15_XER() => Check(PUS_C_Service.S15, Enc.XER); // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S18_XER() => Check(PUS_C_Service.S18, Enc.XER); // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S19_XER() => Check(PUS_C_Service.S19, Enc.XER); // STRING_SIZEOF_BUG in BasicTypes
}
