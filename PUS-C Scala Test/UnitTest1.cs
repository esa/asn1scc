using System.Diagnostics;
using System.Net.Sockets;

namespace PUS_C_Scala_Test
{
    [TestClass]
    public class UnitTest1
    {
        readonly string[] stdArgs = new string[] { "-Scala", "--uper-enc", "-o", };
        readonly string outFolderPrefix = "../../../../GenScala/PUSC_";
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

        private void CompileASN(string[] args)
        {
            Assert.AreEqual(Program.main(args), 0);
        }

        private void CompileScala(string outDir){
            
            using (var proc = new Process
            {
                StartInfo = new ProcessStartInfo
                {
                    FileName = "cmd.exe",
                    Arguments = $"/K cd {outDir}",
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
                var outputList = outp.Split("\n");
                var worked = outputList[outputList.Length - 3].Contains("[success]");

                // print sbt output
                Console.WriteLine(outp);

                Assert.IsTrue(worked);
            }
        }
    }
}
