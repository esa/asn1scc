using Antlr;
using System.Diagnostics;
using System.Net.Sockets;
using System.Runtime.InteropServices;

[assembly: Parallelize(Workers = 4, Scope = ExecutionScope.MethodLevel)]

namespace PUS_C_Scala_Test
{
    [TestClass]
    public class ACNScalaTest
    {
        readonly string[] stdArgs = new string[] { "-Scala", "--acn-enc", "--field-prefix", "AUTO", "--type-prefix", "T", "-o" };
        readonly string outFolderPrefix = "../../../../../GenScala/ACN/PUSC_";
        readonly string inputFilePrefix = "../../../../../asn1-pusc-lib/";

        public string[] combineArgs(string outputFolder, string[] files)
        {
            var ret = new string[stdArgs.Length + files.Length + 1];
            int i = 0;
            for(; i < stdArgs.Length; ++i)
            {
                ret[i] = stdArgs[i];
            }

            ret[i++] = outputFolder;

            for (int j = 0; j < files.Length; j++)
            {
                ret[j+i] = inputFilePrefix+files[j];
            }
            return ret;
        }

        public string getOutputFolder(string name)
        {
            return outFolderPrefix + name;
        }

        [TestMethod]
        public void TestService_1()
        {
            string outDir = getOutputFolder("S1");

            var args = combineArgs(outDir, new string[]{
                "common/ApplicationProcess.asn1",
                "common/ApplicationProcessUser.asn1",
                "common/ExecutionStep.asn1",
                "common/MessageType.asn1",
                "ccsds/PacketTypes.asn1",
                "ccsds/TC-Packet.asn1",
                "ccsds/TC-Payload.asn1",
                "service-01/ErrorCodes.asn1",
                "service-01/PUS-1-1.asn1",
                "service-01/PUS-1-10.asn1",
                "service-01/PUS-1-2.asn1",
                "service-01/PUS-1-3.asn1",
                "service-01/PUS-1-4.asn1",
                "service-01/PUS-1-5.asn1",
                "service-01/PUS-1-6.asn1",
                "service-01/PUS-1-7.asn1",
                "service-01/PUS-1-8.asn1",
                "service-01/VerificationRequest.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_2()
        {
            string outDir = getOutputFolder("S2");

            var args = combineArgs(outDir, new string[]{
                "common/BasicTypes.asn1",
                "common/Dummy.asn1",
                "common/Parameter.asn1",
                "service-02/LogicalDevice.asn1",
                "service-02/PhysicalDevice.asn1",
                "service-02/PUS-2-1.asn1",
                "service-02/PUS-2-10.asn1",
                "service-02/PUS-2-11.asn1",
                "service-02/PUS-2-12.asn1",
                "service-02/PUS-2-2.asn1",
                "service-02/PUS-2-4.asn1",
                "service-02/PUS-2-5.asn1",
                "service-02/PUS-2-6.asn1",
                "service-02/PUS-2-7.asn1",
                "service-02/PUS-2-8.asn1",
                "service-02/PUS-2-9.asn1",
                "service-02/Registers.asn1",
                "service-02/Transaction.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_3()
        {
            string outDir = getOutputFolder("S3");

            var args = combineArgs(outDir, new string[]{
                "common/ApplicationProcess.asn1",
                "common/ApplicationProcessUser.asn1",
                "common/BasicTypes.asn1",
                "common/DiagnosticParameterReportStructure.asn1",
                "common/HousekeepingParameterReportStructure.asn1",
                "common/Parameter.asn1",
                "service-03/CollectionInterval.asn1",
                "service-03/ParameterFunctionalReportingDefinition.asn1",
                "service-03/ParameterReportingEntries.asn1",
                "service-03/ParameterReportStructureType.asn1",
                "service-03/PeriodicGenerationActionStatus.asn1",
                "service-03/PeriodicGenerationProperties.asn1",
                "service-03/PUS-3-1.asn1",
                "service-03/PUS-3-2.asn1",
                "service-03/PUS-3-3.asn1",
                "service-03/PUS-3-4.asn1",
                "service-03/PUS-3-5.asn1",
                "service-03/PUS-3-6.asn1",
                "service-03/PUS-3-7.asn1",
                "service-03/PUS-3-8.asn1",
                "service-03/PUS-3-9.asn1",
                "service-03/PUS-3-10.asn1",
                "service-03/PUS-3-11.asn1",
                "service-03/PUS-3-12.asn1",
                "service-03/PUS-3-25.asn1",
                "service-03/PUS-3-26.asn1",
                "service-03/PUS-3-27.asn1",
                "service-03/PUS-3-28.asn1",
                "service-03/PUS-3-29.asn1",
                "service-03/PUS-3-30.asn1",
                "service-03/PUS-3-31.asn1",
                "service-03/PUS-3-32.asn1",
                "service-03/PUS-3-33.asn1",
                "service-03/PUS-3-34.asn1",
                "service-03/PUS-3-35.asn1",
                "service-03/PUS-3-36.asn1",
                "service-03/PUS-3-37.asn1",
                "service-03/PUS-3-38.asn1",
                "service-03/PUS-3-39.asn1",
                "service-03/PUS-3-40.asn1",
                "service-03/PUS-3-41.asn1",
                "service-03/PUS-3-42.asn1",
                "service-03/PUS-3-43.asn1",
                "service-03/PUS-3-44.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_4()
        {
            string outDir = getOutputFolder("S4");

            var args = combineArgs(outDir, new string[]{
                "common/BasicTypes.asn1",
                "common/Parameter.asn1",
                "service-04/Intervals.asn1",
                "service-04/ParameterStatisticsDefinitions.asn1",
                "service-04/PUS-4-1.asn1",
                "service-04/PUS-4-2.asn1",
                "service-04/PUS-4-3.asn1",
                "service-04/PUS-4-4.asn1",
                "service-04/PUS-4-5.asn1",
                "service-04/PUS-4-6.asn1",
                "service-04/PUS-4-7.asn1",
                "service-04/PUS-4-8.asn1",
                "service-04/PUS-4-9.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_5()
        {
            string outDir = getOutputFolder("S5");

            var args = combineArgs(outDir, new string[]{
                "common/ApplicationProcess.asn1",
                "common/ApplicationProcessUser.asn1",
                "common/BasicTypes.asn1",
                "common/Dummy.asn1",
                "common/ExecutionStep.asn1",
                "common/EventDefinition.asn1",
                "common/MessageType.asn1",
                "ccsds/PacketTypes.asn1",
                "ccsds/TC-Packet.asn1",
                "ccsds/TC-Payload.asn1",
                "service-05/PUS-5-1.asn1",
                "service-05/PUS-5-2.asn1",
                "service-05/PUS-5-3.asn1",
                "service-05/PUS-5-4.asn1",
                "service-05/PUS-5-5.asn1",
                "service-05/PUS-5-6.asn1",
                "service-05/PUS-5-7.asn1",
                "service-05/PUS-5-8.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_6()
        {
            string outDir = getOutputFolder("S6");

            var args = combineArgs(outDir, new string[]{
                "common/BasicTypes.asn1",
                "common/FilePath.asn1",
                "service-06/Data.asn1",
                "service-06/Memory.asn1",
                "service-06/PUS-6-1.asn1",
                "service-06/PUS-6-2.asn1",
                "service-06/PUS-6-3.asn1",
                "service-06/PUS-6-4.asn1",
                "service-06/PUS-6-5.asn1",
                "service-06/PUS-6-6.asn1",
                "service-06/PUS-6-7.asn1",
                "service-06/PUS-6-8.asn1",
                "service-06/PUS-6-9.asn1",
                "service-06/PUS-6-10.asn1",
                "service-06/PUS-6-11.asn1",
                "service-06/PUS-6-12.asn1",
                "service-06/PUS-6-13.asn1",
                "service-06/PUS-6-14.asn1",
                "service-06/PUS-6-15.asn1",
                "service-06/PUS-6-16.asn1",
                "service-06/PUS-6-17.asn1",
                "service-06/PUS-6-18.asn1",
                "service-06/PUS-6-19.asn1",
                "service-06/PUS-6-20.asn1",
                "service-06/PUS-6-21.asn1",
                "service-06/PUS-6-22.asn1",
                "service-06/RawMemory.asn1",
                "service-06/StructuredMemory.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_8()
        {
            string outDir = getOutputFolder("S8");

            var args = combineArgs(outDir, new string[]{
                "common/BasicTypes.asn1",
                "service-08/PUS-8-1.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_9()
        {
            string outDir = getOutputFolder("S9");

            var args = combineArgs(outDir, new string[]{
                "common/BasicTypes.asn1",
                "common/SpacecraftTimeReferenceStatus.asn1",
                "service-09/PUS-9-1.asn1",
                "service-09/PUS-9-2.asn1",
                "service-09/PUS-9-3.asn1",
                "service-09/RateExponentialValue.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_11()
        {
            string outDir = getOutputFolder("S11");

            var args = combineArgs(outDir, new string[]{
                "common/ApplicationProcess.asn1",
                "common/ApplicationProcessUser.asn1",
                "common/BasicTypes.asn1",
                "common/Dummy.asn1",
                "common/Request.asn1",
                "common/TimeWindow.asn1",
                "service-11/Group.asn1",
                "service-11/PUS-11-1.asn1",
                "service-11/PUS-11-2.asn1",
                "service-11/PUS-11-3.asn1",
                "service-11/PUS-11-4.asn1",
                "service-11/PUS-11-5.asn1",
                "service-11/PUS-11-6.asn1",
                "service-11/PUS-11-7.asn1",
                "service-11/PUS-11-8.asn1",
                "service-11/PUS-11-9.asn1",
                "service-11/PUS-11-10.asn1",
                "service-11/PUS-11-11.asn1",
                "service-11/PUS-11-12.asn1",
                "service-11/PUS-11-13.asn1",
                "service-11/PUS-11-14.asn1",
                "service-11/PUS-11-15.asn1",
                "service-11/PUS-11-16.asn1",
                "service-11/PUS-11-17.asn1",
                "service-11/PUS-11-18.asn1",
                "service-11/PUS-11-19.asn1",
                "service-11/PUS-11-20.asn1",
                "service-11/PUS-11-21.asn1",
                "service-11/PUS-11-22.asn1",
                "service-11/PUS-11-23.asn1",
                "service-11/PUS-11-24.asn1",
                "service-11/PUS-11-25.asn1",
                "service-11/PUS-11-26.asn1",
                "service-11/PUS-11-27.asn1",
                "service-11/SubSchedule.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_12()
        {
            string outDir = getOutputFolder("S12");

            var args = combineArgs(outDir, new string[]{
                "common/ApplicationProcess.asn1",
                "common/ApplicationProcessUser.asn1",
                "common/BasicTypes.asn1",
                "common/Dummy.asn1",
                "common/EventDefinition.asn1",
                "common/Parameter.asn1",
                "service-12/CheckValidityCondition.asn1",
                "service-12/FMON.asn1",
                "service-12/ParameterMonitoringDefinition.asn1",
                "service-12/PMON-FailingNumber.asn1",
                "service-12/PMON.asn1",
                "service-12/PUS-12-1.asn1",
                "service-12/PUS-12-2.asn1",
                "service-12/PUS-12-3.asn1",
                "service-12/PUS-12-4.asn1",
                "service-12/PUS-12-5.asn1",
                "service-12/PUS-12-6.asn1",
                "service-12/PUS-12-7.asn1",
                "service-12/PUS-12-8.asn1",
                "service-12/PUS-12-9.asn1",
                "service-12/PUS-12-10.asn1",
                "service-12/PUS-12-11.asn1",
                "service-12/PUS-12-12.asn1",
                "service-12/PUS-12-13.asn1",
                "service-12/PUS-12-14.asn1",
                "service-12/PUS-12-15.asn1",
                "service-12/PUS-12-16.asn1",
                "service-12/PUS-12-17.asn1",
                "service-12/PUS-12-18.asn1",
                "service-12/PUS-12-19.asn1",
                "service-12/PUS-12-20.asn1",
                "service-12/PUS-12-21.asn1",
                "service-12/PUS-12-22.asn1",
                "service-12/PUS-12-23.asn1",
                "service-12/PUS-12-24.asn1",
                "service-12/PUS-12-25.asn1",
                "service-12/PUS-12-26.asn1",
                "service-12/PUS-12-27.asn1",
                "service-12/PUS-12-28.asn1",
                "service-12/TransitionNotification.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_13()
        {
            string outDir = getOutputFolder("S13");

            var args = combineArgs(outDir, new string[]{
                "common/BasicTypes.asn1",
                "service-13/LargePacketTransferMessageParts.asn1",
                "service-13/PUS-13-1.asn1",
                "service-13/PUS-13-10.asn1",
                "service-13/PUS-13-11.asn1",
                "service-13/PUS-13-16.asn1",
                "service-13/PUS-13-2.asn1",
                "service-13/PUS-13-3.asn1",
                "service-13/PUS-13-9.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_14()
        {
            string outDir = getOutputFolder("S14");

            var args = combineArgs(outDir, new string[]{
                "common/ApplicationProcess.asn1",
                "common/ApplicationProcessUser.asn1",
                "common/BasicTypes.asn1",
                "common/DiagnosticParameterReportStructure.asn1",
                "common/Dummy.asn1",
                "common/EventDefinition.asn1",
                "common/HousekeepingParameterReportStructure.asn1",
                "common/MessageType.asn1",
                "service-14/ApplicationProcessForwardControl.asn1",
                "service-14/DiagnosticParameterReportForwardControl.asn1",
                "service-14/EventDefinitionForwardControl.asn1",
                "service-14/HousekeepingParameterReportForwardControl.asn1",
                "service-14/PUS-14-1.asn1",
                "service-14/PUS-14-10.asn1",
                "service-14/PUS-14-11.asn1",
                "service-14/PUS-14-12.asn1",
                "service-14/PUS-14-13.asn1",
                "service-14/PUS-14-14.asn1",
                "service-14/PUS-14-15.asn1",
                "service-14/PUS-14-16.asn1",
                "service-14/PUS-14-2.asn1",
                "service-14/PUS-14-3.asn1",
                "service-14/PUS-14-4.asn1",
                "service-14/PUS-14-5.asn1",
                "service-14/PUS-14-6.asn1",
                "service-14/PUS-14-7.asn1",
                "service-14/PUS-14-8.asn1",
                "service-14/PUS-14-9.asn1",
                "service-14/SubsamplingRate.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_15()
        {
            string outDir = getOutputFolder("S15");

            var args = combineArgs(outDir, new string[]{
                "common/ApplicationProcess.asn1",
                "common/ApplicationProcessUser.asn1",
                "common/BasicTypes.asn1",
                "common/DiagnosticParameterReportStructure.asn1",
                "common/Dummy.asn1",
                "common/EventDefinition.asn1",
                "common/HousekeepingParameterReportStructure.asn1",
                "common/MessageType.asn1",
                "common/TimeWindow.asn1",
                "service-15/PacketStore.asn1",
                "service-15/PacketStoreConfiguration.asn1",
                "service-15/PacketStoreEnumerations.asn1",
                "service-15/PUS-15-1.asn1",
                "service-15/PUS-15-2.asn1",
                "service-15/PUS-15-3.asn1",
                "service-15/PUS-15-4.asn1",
                "service-15/PUS-15-5.asn1",
                "service-15/PUS-15-6.asn1",
                "service-15/PUS-15-9.asn1",
                "service-15/PUS-15-11.asn1",
                "service-15/PUS-15-12.asn1",
                "service-15/PUS-15-13.asn1",
                "service-15/PUS-15-14.asn1",
                "service-15/PUS-15-15.asn1",
                "service-15/PUS-15-16.asn1",
                "service-15/PUS-15-17.asn1",
                "service-15/PUS-15-18.asn1",
                "service-15/PUS-15-19.asn1",
                "service-15/PUS-15-20.asn1",
                "service-15/PUS-15-21.asn1",
                "service-15/PUS-15-22.asn1",
                "service-15/PUS-15-23.asn1",
                "service-15/PUS-15-24.asn1",
                "service-15/PUS-15-25.asn1",
                "service-15/PUS-15-26.asn1",
                "service-15/PUS-15-27.asn1",
                "service-15/PUS-15-28.asn1",
                "service-15/PUS-15-29.asn1",
                "service-15/PUS-15-30.asn1",
                "service-15/PUS-15-31.asn1",
                "service-15/PUS-15-32.asn1",
                "service-15/PUS-15-33.asn1",
                "service-15/PUS-15-34.asn1",
                "service-15/PUS-15-35.asn1",
                "service-15/PUS-15-36.asn1",
                "service-15/PUS-15-37.asn1",
                "service-15/PUS-15-38.asn1",
                "service-15/PUS-15-39.asn1",
                "service-15/PUS-15-40.asn1",
                "service-15/Storage-ControlConfiguration.asn1",
                "service-15/Storage-ControlDiagnosticParameterReport.asn1",
                "service-15/Storage-ControlEventReportBlocking.asn1",
                "service-15/Storage-ControlHousekeepingParameterReport.asn1",
                "service-15/Storage-ControlReportType.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_17()
        {
            string outDir = getOutputFolder("S17");

            var args = combineArgs(outDir, new string[]{
                "common/ApplicationProcess.asn1",
                "common/ApplicationProcessUser.asn1",
                "service-17/PUS-17-1.asn1",
                "service-17/PUS-17-3.asn1",
                "service-17/PUS-17-2.asn1",
                "service-17/PUS-17-4.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_18()
        {
            string outDir = getOutputFolder("S18");

            var args = combineArgs(outDir, new string[]{
                "common/BasicTypes.asn1",
                "common/FilePath.asn1",
                "service-18/OBCP.asn1",
                "service-18/OBCPActivation.asn1",
                "service-18/OBCPArgumentValues.asn1",
                "service-18/OBCPManagement.asn1",
                "service-18/OBCPWithold.asn1",
                "service-18/PUS-18-1.asn1",
                "service-18/PUS-18-2.asn1",
                "service-18/PUS-18-3.asn1",
                "service-18/PUS-18-4.asn1",
                "service-18/PUS-18-5.asn1",
                "service-18/PUS-18-6.asn1",
                "service-18/PUS-18-7.asn1",
                "service-18/PUS-18-8.asn1",
                "service-18/PUS-18-9.asn1",
                "service-18/PUS-18-12.asn1",
                "service-18/PUS-18-13.asn1",
                "service-18/PUS-18-14.asn1",
                "service-18/PUS-18-15.asn1",
                "service-18/PUS-18-16.asn1",
                "service-18/PUS-18-17.asn1",
                "service-18/PUS-18-18.asn1",
                "service-18/PUS-18-19.asn1",
                "service-18/PUS-18-20.asn1",
                "service-18/PUS-18-21.asn1",
                "service-18/PUS-18-22.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        [TestMethod]
        public void TestService_19()
        {
            string outDir = getOutputFolder("S19");

            var args = combineArgs(outDir, new string[]{
                "common/ApplicationProcess.asn1",
                "common/ApplicationProcessUser.asn1",
                "common/BasicTypes.asn1",
                "common/Dummy.asn1",
                "common/EventDefinition.asn1",
                "service-19/EventActionStatus.asn1",
                "service-19/EventDefinitionSystem.asn1",
                "service-19/PUS-19-1.asn1",
                "service-19/PUS-19-10.asn1",
                "service-19/PUS-19-11.asn1",
                "service-19/PUS-19-2.asn1",
                "service-19/PUS-19-3.asn1",
                "service-19/PUS-19-4.asn1",
                "service-19/PUS-19-5.asn1",
                "service-19/PUS-19-6.asn1",
                "service-19/PUS-19-7.asn1",
                "service-19/PUS-19-8.asn1",
                "service-19/PUS-19-9.asn1"
            });

            CompileASN(args);
            CompileScala(outDir);
        }

        private void CompileASN(string[] args)
        {
            Assert.AreEqual(Program.main(args), 0);
        }

        private void CompileScala(string outDir){
            
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
                proc.StandardInput.WriteLine("sbt compile");
                System.Threading.Thread.Sleep(500); // give some time for command to execute
                proc.StandardInput.Flush();
                proc.StandardInput.Close();
                //proc.WaitForExit(0);

                // parse sbt output
                var outp = proc.StandardOutput.ReadToEnd();
                var outputList = outp.Split("\n").ToList();
                var worked = outputList.FindLastIndex(x => x.Contains("[success]")) > outputList.Count - 5;

                // print sbt output
                Console.WriteLine(outp);

                Assert.IsTrue(worked);
            }
        }
    }
}
