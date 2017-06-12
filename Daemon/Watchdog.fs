namespace Daemon

open System.Timers
open System
open System.Net

type Watchdog(ms, server:HttpServer) =

    let timer = new Timer(ms)

    let OnTimerEvent args = 
        Console.WriteLine "Watchdog is stopping the server"
        server.Stop

    do
        timer.AutoReset <- false
        timer.Elapsed.Add(OnTimerEvent)
        timer.Start()

    member this.StayAlive (context:HttpListenerContext) = 
        timer.Stop()
        timer.Start()
        Http.SendPlainText context.Response "OK"
