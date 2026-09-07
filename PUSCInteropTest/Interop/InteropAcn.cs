namespace PUS_C_Interop_Test;

[TestClass]
public class InteropAcn
{
    private static void Interop(PUS_C_Service s)
    {
        var c = BuildCache.EnsureBuilt(new BuildKey(s, Lang.C, Enc.ACN));
        Assert.IsTrue(c.TestsPassed, $"C reference ACN {s} failed");
        foreach (var lang in new[] { Lang.Scala, Lang.Python })
        {
            var other = BuildCache.EnsureBuilt(new BuildKey(s, lang, Enc.ACN));
            Assert.IsTrue(other.TestsPassed, $"{lang} ACN {s} failed");
            BuildCache.CompareDat(c, other, BuildCache.Exclusions(lang, s));
        }
    }

    [TestMethod] public void S1() => Interop(PUS_C_Service.S1);
    [TestMethod] public void S2() => Interop(PUS_C_Service.S2);
    [TestMethod] public void S3() => Interop(PUS_C_Service.S3);
    [TestMethod] public void S4() => Interop(PUS_C_Service.S4);
    [TestMethod] public void S5() => Interop(PUS_C_Service.S5);
    [TestMethod] public void S6() => Interop(PUS_C_Service.S6);
    [TestMethod] public void S8() => Interop(PUS_C_Service.S8);
    [TestMethod] public void S9() => Interop(PUS_C_Service.S9);
    [TestMethod] public void S11() => Interop(PUS_C_Service.S11);
    [TestMethod, Ignore] public void S12() => Interop(PUS_C_Service.S12); // S12 TODO: not working for C/Scala
    [TestMethod] public void S13() => Interop(PUS_C_Service.S13);
    [TestMethod] public void S14() => Interop(PUS_C_Service.S14);
    [TestMethod] public void S15() => Interop(PUS_C_Service.S15);
    [TestMethod] public void S17() => Interop(PUS_C_Service.S17);
    [TestMethod] public void S18() => Interop(PUS_C_Service.S18);
    [TestMethod] public void S19() => Interop(PUS_C_Service.S19);
}
