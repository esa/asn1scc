using System.Collections.Concurrent;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;

namespace PUS_C_Interop_Test;

public enum Lang { C, Scala, Python }
public enum Enc { UPER, ACN, XER }

public readonly record struct BuildKey(PUS_C_Service Service, Lang Lang, Enc Enc);

public sealed record BuildResult(Lang Lang, Enc Enc, bool TestsPassed, string OutDir, string DatDir);

public static partial class BuildCache
{
    private const string InputPrefix = "../../../../PUSCInteropTest/asn1-pusc-lib-asn1CompilerTestInput/";
    private const string OutPrefix = "../../../../PUSCInteropTest/GenTests/Test/";
    private const string CConfig = "release";
    private const string CProject = "VsProject";

    private static readonly ConcurrentDictionary<BuildKey, Lazy<BuildResult>> Cache = new();

    public static BuildResult EnsureBuilt(BuildKey key) =>
        Cache.GetOrAdd(key, k => new Lazy<BuildResult>(() => Build(k))).Value;

    private static BuildResult Build(BuildKey key)
    {
        var outDir = OutDir(key);
        var datDir = key.Lang == Lang.Python ? Path.Combine(outDir, "output") : outDir;

        var args = Args(key, outDir);
        Console.WriteLine($"[BuildCache] {key} -> {outDir}");
        Assert.AreEqual(0, Program.main(args), $"asn1scc generation failed for {key}");

        bool passed;
        switch (key.Lang)
        {
            case Lang.C:
                CompileC(outDir);
                passed = RunCTests(outDir);
                break;
            case Lang.Scala:
                CompileScala(outDir);
                passed = RunScalaTests(outDir);
                break;
            case Lang.Python:
                passed = RunPythonTests(outDir);
                break;
            default:
                throw new InvalidOperationException($"unknown language {key.Lang}");
        }

        return new BuildResult(key.Lang, key.Enc, passed, outDir, datDir);
    }

    private static string OutDir(BuildKey key)
    {
        var enc = key.Enc switch
        {
            Enc.UPER => "UPER/PUSC_",
            Enc.ACN => "ACN/PUSC_",
            Enc.XER => "XER/PUSC_",
            _ => throw new InvalidOperationException()
        };
        var lang = key.Lang switch
        {
            Lang.C => "/C",
            Lang.Scala => "/Scala",
            Lang.Python => "/Python",
            _ => throw new InvalidOperationException()
        };
        return OutPrefix + enc + key.Service + lang;
    }

    private static string[] Args(BuildKey key, string outDir)
    {
        var langFlag = key.Lang switch
        {
            Lang.C => "-c",
            Lang.Scala => "-Scala",
            Lang.Python => "-python",
            _ => throw new InvalidOperationException()
        };
        var encFlag = key.Enc switch
        {
            Enc.UPER => "--uper-enc",
            Enc.ACN => "-ACN",
            Enc.XER => "-XER",
            _ => throw new InvalidOperationException()
        };

        var args = new List<string> { langFlag, encFlag, "-atc", "-fp", "AUTO", "-typePrefix", "T", "-o", outDir };

        var files = ServiceFiles.Files(key.Service);

        // Every entry in the service table must have a .asn1 file; a missing one means a
        // typo/stale entry that would silently shrink the tested set, so warn loudly.
        var asn1 = files.Select(f => InputPrefix + f + ".asn1").ToArray();
        var missingAsn1 = asn1.Where(f => !File.Exists(f)).ToArray();
        if (missingAsn1.Length > 0)
            Console.WriteLine($"WARNING: ASN1 files not found for {key}: {string.Join(", ", missingAsn1)}");
        args.AddRange(asn1.Where(File.Exists));

        // .acn files are optional: a module with no ACN encoding directives has none, so a
        // missing .acn is normal and not warned about (unlike the required .asn1 above).
        if (key.Enc == Enc.ACN)
            args.AddRange(files.Select(f => InputPrefix + f + ".acn").Where(File.Exists));

        return args.ToArray();
    }

    // ---- process helpers ---------------------------------------------------

    private static (int Exit, string Out, string Err) RunProcess(string fileName, string arguments, string workingDir)
    {
        using var proc = new Process
        {
            StartInfo = new ProcessStartInfo
            {
                FileName = fileName,
                Arguments = arguments,
                WorkingDirectory = workingDir,
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                CreateNoWindow = true,
            }
        };
        proc.Start();
        var outp = proc.StandardOutput.ReadToEnd();
        var err = proc.StandardError.ReadToEnd();
        proc.WaitForExit();
        return (proc.ExitCode, outp, err);
    }

    private static void CompileC(string outDir)
    {
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            RunMSBuild(outDir);
        }
        else
        {
            var (_, outp, _) = RunProcess("bash", "-c \"make all\"", outDir);
            Console.WriteLine(outp);
        }
    }

    private static bool RunCTests(string outDir)
    {
        var (_, stdout, stderr) = RunProcess(
            RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? "cmd.exe" : "bash",
            RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? $"/C {CConfig}\\{CProject}.exe" : "-c ./mainprogram",
            outDir);
        var worked = stdout.Contains("All test cases (") && stdout.Contains(") run successfully.");
        if (!worked)
        {
            Console.WriteLine("C test cases failed. Stdout:");
            Console.WriteLine(stdout);
            Console.WriteLine("Stderr:");
            Console.WriteLine(stderr);
        }
        return worked;
    }

    private static void CompileScala(string outDir) =>
        Assert.IsTrue(StartSBT(outDir, "sbt compile", "[success]"), "Scala compile failed");

    private static bool RunScalaTests(string outDir) =>
        StartSBT(outDir, "sbt run", "[test success]");

    private static bool StartSBT(string outDir, string arg, string check)
    {
        var (_, outp, err) = RunProcess(
            RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? "cmd.exe" : "bash",
            RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? $"/C {arg}" : $"-c \"{arg}\"",
            outDir);
        var lines = outp.Split("\n").ToList();
        var worked = lines.FindLastIndex(x => x.Contains(check)) > lines.Count - 5;
        if (!worked)
        {
            Console.WriteLine($"Scala step '{arg}' failed. Stdout:");
            Console.WriteLine(outp);
            Console.WriteLine("Stderr:");
            Console.WriteLine(err);
        }
        return worked;
    }

    private static bool RunPythonTests(string outDir)
    {
        var (exit, stdout, stderr) = RunProcess(
            RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? "cmd.exe" : "bash",
            RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? "/C uvx --python 3.11 pytest" : "--login -c \"uvx --python 3.11 pytest\"",
            outDir);

        var passed = Regex.Match(stdout, @"(\d+)\s+passed");
        var failed = Regex.Match(stdout, @"(\d+)\s+failed");
        if (passed.Success || failed.Success)
            Console.WriteLine($"Python Tests: {(passed.Success ? passed.Groups[1].Value : "0")} passed, {(failed.Success ? failed.Groups[1].Value : "0")} failed");

        var worked = exit == 0;
        if (!worked)
        {
            Console.WriteLine(stdout);
            if (!string.IsNullOrEmpty(stderr))
                Console.WriteLine("STDERR: " + stderr);
        }
        return worked;
    }

    private static void RunMSBuild(string outDir)
    {
        string msBuildPath;
        using (var proc = new Process
        {
            StartInfo = new ProcessStartInfo
            {
                FileName = @$"{Environment.GetEnvironmentVariable("ProgramFiles(x86)")}\Microsoft Visual Studio\Installer\vswhere.exe",
                Arguments = @"-latest -requires Microsoft.Component.MSBuild -find MSBuild\**\Bin\msbuild.exe",
                UseShellExecute = false,
                RedirectStandardOutput = true,
                CreateNoWindow = true,
            }
        })
        {
            proc.Start();
            msBuildPath = proc.StandardOutput.ReadToEnd().Trim();
            proc.WaitForExit();
            Assert.AreNotEqual("", msBuildPath, "Couldn't find the location of msbuild.exe");
        }

        var (exit, outp, _) = RunProcess(msBuildPath, $"{CProject}.vcxproj /p:configuration={CConfig}", outDir);
        if (exit != 0)
            Console.WriteLine(outp);
        Assert.AreEqual(0, exit, "error while compiling C project");
    }
}
