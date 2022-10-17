module ShellProcess
open System
open System.IO

type ProcessResult = { 
    ExitCode : int; 
    StdOut : string; 
    StdErr : string 
}
type TestCaseResult =
    | Success of string
    | Failed  of string


let markError fmt = Printf.kprintf (fun str ->  Failed str) fmt

let markSuccess fmt = Printf.kprintf (fun str ->   Success str) fmt

let printError fmt = 
    Printf.kprintf (fun str ->   
        let currentColor = Console.ForegroundColor
        Console.ForegroundColor <- ConsoleColor.Red
        eprintf "%s" str
        Console.ForegroundColor <- currentColor) fmt

let printSuccess  fmt = 
    Printf.kprintf (fun str ->   
        let currentColor = Console.ForegroundColor
        Console.ForegroundColor <- ConsoleColor.Green
        printf "%s" str
        Console.ForegroundColor <- currentColor) fmt

let printInfo fmt = 
    Printf.kprintf (fun str ->   
        let currentColor = Console.ForegroundColor
        Console.ForegroundColor <- ConsoleColor.Blue
        printf "%s" str
        Console.ForegroundColor <- currentColor) fmt


let executeProcess_priv  (workingDirectory: string) (processName: string) (processArgs: string) =
    let psi = new Diagnostics.ProcessStartInfo(processName, processArgs) 
    psi.UseShellExecute <- false
    psi.WorkingDirectory <- workingDirectory
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.CreateNoWindow <- true        
    let proc = Diagnostics.Process.Start(psi) 
    let output = new Text.StringBuilder()
    let error = new Text.StringBuilder()
    proc.OutputDataReceived.Add(fun args -> output.Append(args.Data) |> ignore)
    proc.ErrorDataReceived.Add(fun args -> error.Append(args.Data) |> ignore)
    proc.BeginErrorReadLine()
    proc.BeginOutputReadLine()
    proc.WaitForExit()
    { ExitCode = proc.ExitCode; StdOut = output.ToString(); StdErr = error.ToString() }

let executeProcess (workingDirectory: string) (processName: string) (processArgs: string) =
    executeProcess_priv  workingDirectory processName processArgs

let executeBashScript (workingDirectory: string) (cmd: string) =
    let fname = Path.GetRandomFileName() + ".sh"
    let absName = Path.Combine(workingDirectory, fname)
    File.WriteAllText(absName,(sprintf "#!/bin/bash\n%s\n" cmd))
    let ret = executeProcess_priv  workingDirectory "bash" fname
    File.Delete absName
    ret
