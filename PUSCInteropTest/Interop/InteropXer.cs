namespace PUS_C_Interop_Test;

[TestClass]
public class InteropXer
{
    private static void Interop(PUS_C_Service s)
    {
        var c = BuildCache.EnsureBuilt(new BuildKey(s, Lang.C, Enc.XER));
        Assert.IsTrue(c.TestsPassed, $"C reference XER {s} failed");
        var py = BuildCache.EnsureBuilt(new BuildKey(s, Lang.Python, Enc.XER));
        Assert.IsTrue(py.TestsPassed, $"Python XER {s} failed");
        BuildCache.CompareXer(c, py, BuildCache.Exclusions(Lang.Python, s));
    }

    // Only S1/S17 lack BasicTypes and clear the C XER STRING_SIZEOF_BUG.
    [TestMethod] public void S1() => Interop(PUS_C_Service.S1);
    [TestMethod] public void S17() => Interop(PUS_C_Service.S17);
    [TestMethod, Ignore] public void S2() => Interop(PUS_C_Service.S2);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S3() => Interop(PUS_C_Service.S3);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S4() => Interop(PUS_C_Service.S4);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S5() => Interop(PUS_C_Service.S5);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S6() => Interop(PUS_C_Service.S6);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S8() => Interop(PUS_C_Service.S8);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S9() => Interop(PUS_C_Service.S9);   // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S11() => Interop(PUS_C_Service.S11); // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S12() => Interop(PUS_C_Service.S12); // S12 TODO + STRING_SIZEOF_BUG
    [TestMethod, Ignore] public void S13() => Interop(PUS_C_Service.S13); // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S14() => Interop(PUS_C_Service.S14); // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S15() => Interop(PUS_C_Service.S15); // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S18() => Interop(PUS_C_Service.S18); // STRING_SIZEOF_BUG in BasicTypes
    [TestMethod, Ignore] public void S19() => Interop(PUS_C_Service.S19); // STRING_SIZEOF_BUG in BasicTypes
}
