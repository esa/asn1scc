using System.Text.RegularExpressions;
using System.Xml.Linq;

namespace PUS_C_Interop_Test;

public static partial class BuildCache
{
    // Known cases that legitimately cannot match the C reference.
    // Scala: the JVM has no unsigned integers, so case 80 (an unsigned-value case) diverges.
    public static ISet<int> Exclusions(Lang lang, PUS_C_Service service) =>
        lang == Lang.Scala ? new HashSet<int> { 80 } : new HashSet<int>();

    private static string DatPrefix(Enc enc) => enc switch
    {
        Enc.UPER => "test_case_UPER_",
        Enc.ACN => "test_case_ACN_",
        _ => throw new InvalidOperationException($"CompareDat does not support {enc}")
    };

    private static int CaseNumber(string fileName)
    {
        var m = Regex.Match(fileName, @"(\d+)");
        return m.Success ? int.Parse(m.Groups[1].Value) : -1;
    }

    /// <summary>
    /// Assert every reference .dat (for its encoding, minus excluded cases) exists in `other`
    /// and is byte-identical. `reference` is C; `other` is Scala or Python.
    /// </summary>
    public static void CompareDat(BuildResult reference, BuildResult other, ISet<int> excludedCases)
    {
        var prefix = DatPrefix(reference.Enc);
        var refFiles = Directory.GetFiles(reference.DatDir, prefix + "*.dat").Order().ToArray();
        Assert.IsTrue(refFiles.Length > 0, $"No reference .dat files in {reference.DatDir}");

        List<string> failures = [];
        var compared = 0;
        foreach (var rf in refFiles)
        {
            var name = Path.GetFileName(rf);
            if (excludedCases.Contains(CaseNumber(name)))
            {
                Console.WriteLine($"[CompareDat] skip excluded case: {name}");
                continue;
            }

            var of = Path.Combine(other.DatDir, name);
            if (!File.Exists(of))
            {
                failures.Add($"{name}: missing for {other.Lang}");
                continue;
            }

            if (!File.ReadAllBytes(rf).SequenceEqual(File.ReadAllBytes(of)))
            {
                failures.Add($"{name}: bytes differ");
                continue;
            }
            compared++;
        }

        Assert.IsTrue(failures.Count == 0,
            $"{other.Lang} vs {reference.Lang} ({reference.Enc}): {failures.Count} mismatch, {compared} ok. [{string.Join(", ", failures)}]");
    }

    /// <summary>
    /// XER: C emits .xml, Python emits .dat. Assert Python ⊆ C and every shared file is
    /// semantically XML-equal (numeric-aware leaf compare).
    /// </summary>
    public static void CompareXer(BuildResult reference, BuildResult other, ISet<int> excludedCases)
    {
        var cFiles = Directory.GetFiles(reference.DatDir, "*.xml")
            .ToDictionary(p => Path.GetFileNameWithoutExtension(p)!, p => p);
        var pyFiles = Directory.GetFiles(other.DatDir, "*.dat")
            .ToDictionary(p => Path.GetFileNameWithoutExtension(p)!, p => p);

        var pyOnly = pyFiles.Keys.Except(cFiles.Keys).ToList();
        Assert.IsTrue(pyOnly.Count == 0,
            $"Python produced XER files not present in C: [{string.Join(", ", pyOnly)}]");

        var shared = cFiles.Keys.Intersect(pyFiles.Keys).OrderBy(x => x).ToList();
        Console.WriteLine($"XER interop: C={cFiles.Count}, Python={pyFiles.Count}, shared={shared.Count}");
        Assert.IsTrue(shared.Count > 0, "No shared XER test case files to compare");

        List<string> failures = [];
        foreach (var name in shared)
        {
            if (excludedCases.Contains(CaseNumber(name)))
            {
                Console.WriteLine($"[CompareXer] skip excluded case: {name}");
                continue;
            }

            var cContent = File.ReadAllText(cFiles[name]).Trim();
            var pyContent = File.ReadAllText(pyFiles[name]).Trim();
            try
            {
                if (!XmlSemanticEquals(XDocument.Parse(cContent).Root!, XDocument.Parse(pyContent).Root!))
                {
                    Console.WriteLine($"XER mismatch for {name}:\n  C:  {cContent}\n  Py: {pyContent}");
                    failures.Add(name);
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"XML parse error for {name}: {ex.Message}\n  C:  {cContent}\n  Py: {pyContent}");
                failures.Add(name);
            }
        }

        Assert.IsTrue(failures.Count == 0,
            $"XER semantic mismatches {failures.Count}/{shared.Count}: [{string.Join(", ", failures)}]");
    }

    private static bool XmlSemanticEquals(XElement a, XElement b)
    {
        if (a.Name.LocalName != b.Name.LocalName)
            return false;

        var aAttrs = a.Attributes().OrderBy(x => x.Name.LocalName).ToList();
        var bAttrs = b.Attributes().OrderBy(x => x.Name.LocalName).ToList();
        if (aAttrs.Count != bAttrs.Count) return false;
        for (int i = 0; i < aAttrs.Count; i++)
        {
            if (aAttrs[i].Name.LocalName != bAttrs[i].Name.LocalName) return false;
            if (!LeafTextEqual(aAttrs[i].Value, bAttrs[i].Value)) return false;
        }

        var aChildren = a.Elements().ToList();
        var bChildren = b.Elements().ToList();
        if (aChildren.Count != bChildren.Count) return false;
        if (aChildren.Count == 0) return LeafTextEqual(a.Value, b.Value);

        for (int i = 0; i < aChildren.Count; i++)
            if (!XmlSemanticEquals(aChildren[i], bChildren[i]))
                return false;
        return true;
    }

    private static bool LeafTextEqual(string a, string b)
    {
        var at = a.Trim();
        var bt = b.Trim();
        if (double.TryParse(at, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out var da) &&
            double.TryParse(bt, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out var db))
        {
            if (da == 0.0 && db == 0.0) return true;
            var denom = Math.Max(Math.Abs(da), Math.Abs(db));
            return Math.Abs(da - db) / denom < 1e-9;
        }
        return at == bt;
    }
}
