using System.Diagnostics;

namespace PUS_C_Scala_Test
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            // Execute wsl command:
            using (var proc = new Process
            {
                StartInfo = new ProcessStartInfo
                {
                    FileName = @"cmd.exe",
                    Arguments = "/K cd D:\\Filip\\Dokumente\\Repos\\asn1scc\\GenScala\\PUSC_1_1",
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
                proc.WaitForExit(5000); // wait up to 5 seconds for command to execute
                var outputList = proc.StandardOutput.ReadToEnd().Split("\n");
                var successFail = outputList[outputList.Length - 3].Contains("[success]");
                Console.WriteLine(successFail);
            }
        }
    }
}
