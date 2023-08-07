global using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace PUS_C_Scala_Test
{
    public enum PUS_C_Service
    {
        S1, S2, S3, S4,
        S5, S6, S8, S9,
        S11, S12, S13,
        S14, S15, S17,
        S18, S19
    }

    [Flags]
    public enum ServiceVariation
    {
        UPER = 0b0000_0001,
        ACN = 0b0000_0010,
        CREATE_TESTS = 0b0000_0100
    }

    class TestBasics
    {
        readonly string lang = "-Scala";
        readonly string uperEnc = "--uper-enc";
        readonly string acnEnc = "--acn-enc";
        readonly string genTests= "-atc";
        readonly List<string> stdArgs = new List<string> { "--field-prefix", "AUTO", "--type-prefix", "T", "-o" };
        readonly string outFolderPrefix = "../../../../../GenScala/";
        readonly string outFolderTestFix = "Test/";
        readonly string outFolderSuffixUPER = "UPER/PUSC_";
        readonly string outFolderSuffixACN = "ACN/PUSC_";
        readonly string inputFilePrefix = "../../../../../asn1-pusc-lib/";
        readonly string asn1FileEnding = ".asn1";
        readonly string acnFileEnding = ".acn";

        public string[] CombineArgs(string outputFolder, string[] asn1Files, ServiceVariation sv)
        {
            var parList = new List<string>();
            parList.Add(lang);
            
            if ((sv & ServiceVariation.UPER) == ServiceVariation.UPER)
                parList.Add(uperEnc);
            
            if ((sv & ServiceVariation.ACN) == ServiceVariation.ACN)
                parList.Add(acnEnc);

            if ((sv & ServiceVariation.CREATE_TESTS) == ServiceVariation.CREATE_TESTS)
                parList.Add(genTests);

            parList.AddRange(stdArgs);
            parList.Add(outputFolder);

            // add asn1 input
            parList.AddRange(asn1Files.Select(s => inputFilePrefix + s + asn1FileEnding));
            
            // add acn file input
            if ((sv & ServiceVariation.ACN) == ServiceVariation.ACN)
                parList.AddRange(asn1Files.Select(s => inputFilePrefix + s + acnFileEnding));
            
            return parList.ToArray();
        }

        public string GetOutputFolder(string serviceName, ServiceVariation sv)
        {
            var ret = outFolderPrefix;

            if ((sv & ServiceVariation.CREATE_TESTS) == ServiceVariation.CREATE_TESTS)
                ret += outFolderTestFix;

            if ((sv & ServiceVariation.UPER) == ServiceVariation.UPER)
                ret += outFolderSuffixUPER;                

            if ((sv & ServiceVariation.ACN) == ServiceVariation.ACN)
                ret += outFolderSuffixACN;

            ret += serviceName;

            return ret;
        }

        public void Run_TestService(PUS_C_Service service, string folderSuffix, ServiceVariation sv)
        {
            if (sv == 0 || (sv & ServiceVariation.UPER) != 0 && (sv & ServiceVariation.ACN) != 0)
                throw new InvalidOperationException("can't do nothing or both UPER and ACN");

            string outDir = GetOutputFolder(folderSuffix, sv);
            if (Directory.Exists(outDir))
                Directory.Delete(outDir, true);

            var serviceFiles = GetServiceFiles(service);
            var args = CombineArgs(outDir, serviceFiles(), sv);

            var runTests = (sv & ServiceVariation.CREATE_TESTS) == ServiceVariation.CREATE_TESTS;

            Console.WriteLine("Called Compiler with args:");
            foreach(var a in args)
                Console.WriteLine(a);

            CompileASN(args);
            CompileScala(outDir, !runTests);

            if (runTests)
                RunScalaTests(outDir, runTests);
        }

        Func<string[]> GetServiceFiles(PUS_C_Service service) =>
            service switch
            {
                PUS_C_Service.S1 => GetService01FileNames,
                PUS_C_Service.S2 => GetService02FileNames,
                PUS_C_Service.S3 => GetService03FileNames,
                PUS_C_Service.S4 => GetService04FileNames,
                PUS_C_Service.S5 => GetService05FileNames,
                PUS_C_Service.S6 => GetService06FileNames,
                PUS_C_Service.S8 => GetService08FileNames,
                PUS_C_Service.S9 => GetService09FileNames,
                PUS_C_Service.S11 => GetService11FileNames,
                PUS_C_Service.S12 => GetService12FileNames,
                PUS_C_Service.S13 => GetService13FileNames,
                PUS_C_Service.S14 => GetService14FileNames,
                PUS_C_Service.S15 => GetService15FileNames,
                PUS_C_Service.S17 => GetService17FileNames,
                PUS_C_Service.S18 => GetService18FileNames,
                PUS_C_Service.S19 => GetService19FileNames,
                _ => throw new InvalidOperationException("what?")

            };

        private void CompileASN(string[] args)
        {
            Assert.AreEqual(Program.main(args), 0);
        }

        private void CompileScala(string outDir, bool printOutput)
        {
            StartSBTWithArg(outDir, "sbt compile", "[success]", printOutput);
        }
        private void RunScalaTests(string outDir, bool printOutput)
        {
            StartSBTWithArg(outDir, "sbt run", "[test success]", printOutput);
        }

        private void StartSBTWithArg(string outDir, string arg, string check, bool printOutput)
        {
            using (var proc = new Process
            {
                StartInfo = new ProcessStartInfo
                {
                    FileName = RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? "cmd.exe" : "bash",
                    WorkingDirectory = outDir,
                    UseShellExecute = false,
                    RedirectStandardOutput = true,
                    RedirectStandardInput = true,
                    CreateNoWindow = false,
                }
            })
            {
                proc.Start();
                proc.StandardInput.WriteLine(arg);
                System.Threading.Thread.Sleep(500); // give some time for command to execute
                proc.StandardInput.Flush();
                proc.StandardInput.Close();
                //proc.WaitForExit(0);

                // parse sbt output
                var outp = proc.StandardOutput.ReadToEnd();
                var outputList = outp.Split("\n").ToList();
                var worked = outputList.FindLastIndex(x => x.Contains(check)) > outputList.Count - 5;

                // print sbt output
                if(printOutput)
                    Console.WriteLine(outp);

                Assert.IsTrue(worked);
            }
        }

        private string[] GetService01FileNames() => 
             new string[]{
                "common/ApplicationProcess",
                "common/ApplicationProcessUser",
                "common/ExecutionStep",
                "common/MessageType",
                "ccsds/PacketTypes",
                "ccsds/TC-Packet",
                "ccsds/TC-Payload",
                "service-01/ErrorCodes",
                "service-01/PUS-1-1",
                "service-01/PUS-1-10",
                "service-01/PUS-1-2",
                "service-01/PUS-1-3",
                "service-01/PUS-1-4",
                "service-01/PUS-1-5",
                "service-01/PUS-1-6",
                "service-01/PUS-1-7",
                "service-01/PUS-1-8",
                "service-01/VerificationRequest"
            };

        private string[] GetService02FileNames() => 
            new string[]{
                "common/BasicTypes",
                "common/Dummy",
                "common/Parameter",
                "service-02/LogicalDevice",
                "service-02/PhysicalDevice",
                "service-02/PUS-2-1",
                "service-02/PUS-2-10",
                "service-02/PUS-2-11",
                "service-02/PUS-2-12",
                "service-02/PUS-2-2",
                "service-02/PUS-2-4",
                "service-02/PUS-2-5",
                "service-02/PUS-2-6",
                "service-02/PUS-2-7",
                "service-02/PUS-2-8",
                "service-02/PUS-2-9",
                "service-02/Registers",
                "service-02/Transaction"
            };

        private string[] GetService03FileNames() =>
            new string[]{
                "common/ApplicationProcess",
                "common/ApplicationProcessUser",
                "common/BasicTypes",
                "common/DiagnosticParameterReportStructure",
                "common/HousekeepingParameterReportStructure",
                "common/Parameter",
                "service-03/CollectionInterval",
                "service-03/ParameterFunctionalReportingDefinition",
                "service-03/ParameterReportingEntries",
                "service-03/ParameterReportStructureType",
                "service-03/PeriodicGenerationActionStatus",
                "service-03/PeriodicGenerationProperties",
                "service-03/PUS-3-1",
                "service-03/PUS-3-2",
                "service-03/PUS-3-3",
                "service-03/PUS-3-4",
                "service-03/PUS-3-5",
                "service-03/PUS-3-6",
                "service-03/PUS-3-7",
                "service-03/PUS-3-8",
                "service-03/PUS-3-9",
                "service-03/PUS-3-10",
                "service-03/PUS-3-11",
                "service-03/PUS-3-12",
                "service-03/PUS-3-25",
                "service-03/PUS-3-26",
                "service-03/PUS-3-27",
                "service-03/PUS-3-28",
                "service-03/PUS-3-29",
                "service-03/PUS-3-30",
                "service-03/PUS-3-31",
                "service-03/PUS-3-32",
                "service-03/PUS-3-33",
                "service-03/PUS-3-34",
                "service-03/PUS-3-35",
                "service-03/PUS-3-36",
                "service-03/PUS-3-37",
                "service-03/PUS-3-38",
                "service-03/PUS-3-39",
                "service-03/PUS-3-40",
                "service-03/PUS-3-41",
                "service-03/PUS-3-42",
                "service-03/PUS-3-43",
                "service-03/PUS-3-44"
            };

        private string[] GetService04FileNames() =>
            new string[]{
                "common/BasicTypes",
                "common/Parameter",
                "service-04/Intervals",
                "service-04/ParameterStatisticsDefinitions",
                "service-04/PUS-4-1",
                "service-04/PUS-4-2",
                "service-04/PUS-4-3",
                "service-04/PUS-4-4",
                "service-04/PUS-4-5",
                "service-04/PUS-4-6",
                "service-04/PUS-4-7",
                "service-04/PUS-4-8",
                "service-04/PUS-4-9"
            };


        private string[] GetService05FileNames() =>
            new string[]{
                "common/ApplicationProcess",
                "common/ApplicationProcessUser",
                "common/BasicTypes",
                "common/Dummy",
                "common/ExecutionStep",
                "common/EventDefinition",
                "common/MessageType",
                "ccsds/PacketTypes",
                "ccsds/TC-Packet",
                "ccsds/TC-Payload",
                "service-05/PUS-5-1",
                "service-05/PUS-5-2",
                "service-05/PUS-5-3",
                "service-05/PUS-5-4",
                "service-05/PUS-5-5",
                "service-05/PUS-5-6",
                "service-05/PUS-5-7",
                "service-05/PUS-5-8"
            };

        private string[] GetService06FileNames() =>
            new string[]{
                "common/BasicTypes",
                "common/FilePath",
                "service-06/Data",
                "service-06/Memory",
                "service-06/PUS-6-1",
                "service-06/PUS-6-2",
                "service-06/PUS-6-3",
                "service-06/PUS-6-4",
                "service-06/PUS-6-5",
                "service-06/PUS-6-6",
                "service-06/PUS-6-7",
                "service-06/PUS-6-8",
                "service-06/PUS-6-9",
                "service-06/PUS-6-10",
                "service-06/PUS-6-11",
                "service-06/PUS-6-12",
                "service-06/PUS-6-13",
                "service-06/PUS-6-14",
                "service-06/PUS-6-15",
                "service-06/PUS-6-16",
                "service-06/PUS-6-17",
                "service-06/PUS-6-18",
                "service-06/PUS-6-19",
                "service-06/PUS-6-20",
                "service-06/PUS-6-21",
                "service-06/PUS-6-22",
                "service-06/RawMemory",
                "service-06/StructuredMemory"
            };

        private string[] GetService08FileNames() =>
             new string[]{
                "common/BasicTypes",
                "service-08/PUS-8-1"
            };

        private string[] GetService09FileNames() => 
            new string[]{
                "common/BasicTypes",
                "common/SpacecraftTimeReferenceStatus",
                "service-09/PUS-9-1",
                "service-09/PUS-9-2",
                "service-09/PUS-9-3",
                "service-09/RateExponentialValue"
            };

        private string[] GetService11FileNames() =>
            new string[]{
                "common/ApplicationProcess",
                "common/ApplicationProcessUser",
                "common/BasicTypes",
                "common/Dummy",
                "common/Request",
                "common/TimeWindow",
                "service-11/Group",
                "service-11/PUS-11-1",
                "service-11/PUS-11-2",
                "service-11/PUS-11-3",
                "service-11/PUS-11-4",
                "service-11/PUS-11-5",
                "service-11/PUS-11-6",
                "service-11/PUS-11-7",
                "service-11/PUS-11-8",
                "service-11/PUS-11-9",
                "service-11/PUS-11-10",
                "service-11/PUS-11-11",
                "service-11/PUS-11-12",
                "service-11/PUS-11-13",
                "service-11/PUS-11-14",
                "service-11/PUS-11-15",
                "service-11/PUS-11-16",
                "service-11/PUS-11-17",
                "service-11/PUS-11-18",
                "service-11/PUS-11-19",
                "service-11/PUS-11-20",
                "service-11/PUS-11-21",
                "service-11/PUS-11-22",
                "service-11/PUS-11-23",
                "service-11/PUS-11-24",
                "service-11/PUS-11-25",
                "service-11/PUS-11-26",
                "service-11/PUS-11-27",
                "service-11/SubSchedule"
            };

        private string[] GetService12FileNames() =>
            new string[]{
                "common/ApplicationProcess",
                "common/ApplicationProcessUser",
                "common/BasicTypes",
                "common/Dummy",
                "common/EventDefinition",
                "common/Parameter",
                "service-12/CheckValidityCondition",
                "service-12/FMON",
                "service-12/ParameterMonitoringDefinition",
                "service-12/PMON-FailingNumber",
                "service-12/PMON",
                "service-12/PUS-12-1",
                "service-12/PUS-12-2",
                "service-12/PUS-12-3",
                "service-12/PUS-12-4",
                "service-12/PUS-12-5",
                "service-12/PUS-12-6",
                "service-12/PUS-12-7",
                "service-12/PUS-12-8",
                "service-12/PUS-12-9",
                "service-12/PUS-12-10",
                "service-12/PUS-12-11",
                "service-12/PUS-12-12",
                "service-12/PUS-12-13",
                "service-12/PUS-12-14",
                "service-12/PUS-12-15",
                "service-12/PUS-12-16",
                "service-12/PUS-12-17",
                "service-12/PUS-12-18",
                "service-12/PUS-12-19",
                "service-12/PUS-12-20",
                "service-12/PUS-12-21",
                "service-12/PUS-12-22",
                "service-12/PUS-12-23",
                "service-12/PUS-12-24",
                "service-12/PUS-12-25",
                "service-12/PUS-12-26",
                "service-12/PUS-12-27",
                "service-12/PUS-12-28",
                "service-12/TransitionNotification"
            };

        private string[] GetService13FileNames() =>
            new string[]{
                "common/BasicTypes",
                "service-13/LargePacketTransferMessageParts",
                "service-13/PUS-13-1",
                "service-13/PUS-13-10",
                "service-13/PUS-13-11",
                "service-13/PUS-13-16",
                "service-13/PUS-13-2",
                "service-13/PUS-13-3",
                "service-13/PUS-13-9"
            };

        private string[] GetService14FileNames() =>
            new string[]{
                "common/ApplicationProcess",
                "common/ApplicationProcessUser",
                "common/BasicTypes",
                "common/DiagnosticParameterReportStructure",
                "common/Dummy",
                "common/EventDefinition",
                "common/HousekeepingParameterReportStructure",
                "common/MessageType",
                "service-14/ApplicationProcessForwardControl",
                "service-14/DiagnosticParameterReportForwardControl",
                "service-14/EventDefinitionForwardControl",
                "service-14/HousekeepingParameterReportForwardControl",
                "service-14/PUS-14-1",
                "service-14/PUS-14-10",
                "service-14/PUS-14-11",
                "service-14/PUS-14-12",
                "service-14/PUS-14-13",
                "service-14/PUS-14-14",
                "service-14/PUS-14-15",
                "service-14/PUS-14-16",
                "service-14/PUS-14-2",
                "service-14/PUS-14-3",
                "service-14/PUS-14-4",
                "service-14/PUS-14-5",
                "service-14/PUS-14-6",
                "service-14/PUS-14-7",
                "service-14/PUS-14-8",
                "service-14/PUS-14-9",
                "service-14/SubsamplingRate"
            };

        private string[] GetService15FileNames() =>
            new string[]{
                "common/ApplicationProcess",
                "common/ApplicationProcessUser",
                "common/BasicTypes",
                "common/DiagnosticParameterReportStructure",
                "common/Dummy",
                "common/EventDefinition",
                "common/HousekeepingParameterReportStructure",
                "common/MessageType",
                "common/TimeWindow",
                "service-15/PacketStore",
                "service-15/PacketStoreConfiguration",
                "service-15/PacketStoreEnumerations",
                "service-15/PUS-15-1",
                "service-15/PUS-15-2",
                "service-15/PUS-15-3",
                "service-15/PUS-15-4",
                "service-15/PUS-15-5",
                "service-15/PUS-15-6",
                "service-15/PUS-15-9",
                "service-15/PUS-15-11",
                "service-15/PUS-15-12",
                "service-15/PUS-15-13",
                "service-15/PUS-15-14",
                "service-15/PUS-15-15",
                "service-15/PUS-15-16",
                "service-15/PUS-15-17",
                "service-15/PUS-15-18",
                "service-15/PUS-15-19",
                "service-15/PUS-15-20",
                "service-15/PUS-15-21",
                "service-15/PUS-15-22",
                "service-15/PUS-15-23",
                "service-15/PUS-15-24",
                "service-15/PUS-15-25",
                "service-15/PUS-15-26",
                "service-15/PUS-15-27",
                "service-15/PUS-15-28",
                "service-15/PUS-15-29",
                "service-15/PUS-15-30",
                "service-15/PUS-15-31",
                "service-15/PUS-15-32",
                "service-15/PUS-15-33",
                "service-15/PUS-15-34",
                "service-15/PUS-15-35",
                "service-15/PUS-15-36",
                "service-15/PUS-15-37",
                "service-15/PUS-15-38",
                "service-15/PUS-15-39",
                "service-15/PUS-15-40",
                "service-15/Storage-ControlConfiguration",
                "service-15/Storage-ControlDiagnosticParameterReport",
                "service-15/Storage-ControlEventReportBlocking",
                "service-15/Storage-ControlHousekeepingParameterReport",
                "service-15/Storage-ControlReportType"
            };

        private string[] GetService17FileNames() =>
            new string[]{
                "common/ApplicationProcess",
                "common/ApplicationProcessUser",
                "service-17/PUS-17-1",
                "service-17/PUS-17-3",
                "service-17/PUS-17-2",
                "service-17/PUS-17-4"
            };

        private string[] GetService18FileNames() =>
            new string[]{
                "common/BasicTypes",
                "common/FilePath",
                "service-18/OBCP",
                "service-18/OBCPActivation",
                "service-18/OBCPArgumentValues",
                "service-18/OBCPManagement",
                "service-18/OBCPWithold",
                "service-18/PUS-18-1",
                "service-18/PUS-18-2",
                "service-18/PUS-18-3",
                "service-18/PUS-18-4",
                "service-18/PUS-18-5",
                "service-18/PUS-18-6",
                "service-18/PUS-18-7",
                "service-18/PUS-18-8",
                "service-18/PUS-18-9",
                "service-18/PUS-18-12",
                "service-18/PUS-18-13",
                "service-18/PUS-18-14",
                "service-18/PUS-18-15",
                "service-18/PUS-18-16",
                "service-18/PUS-18-17",
                "service-18/PUS-18-18",
                "service-18/PUS-18-19",
                "service-18/PUS-18-20",
                "service-18/PUS-18-21",
                "service-18/PUS-18-22"
            };

        private string[] GetService19FileNames() =>
            new string[]{
                "common/ApplicationProcess",
                "common/ApplicationProcessUser",
                "common/BasicTypes",
                "common/Dummy",
                "common/EventDefinition",
                "service-19/EventActionStatus",
                "service-19/EventDefinitionSystem",
                "service-19/PUS-19-1",
                "service-19/PUS-19-10",
                "service-19/PUS-19-11",
                "service-19/PUS-19-2",
                "service-19/PUS-19-3",
                "service-19/PUS-19-4",
                "service-19/PUS-19-5",
                "service-19/PUS-19-6",
                "service-19/PUS-19-7",
                "service-19/PUS-19-8",
                "service-19/PUS-19-9"
            };
    }
}
