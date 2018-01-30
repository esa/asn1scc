module Daemon.Program

open System
open Service.Implementation
open CommandLine

let buildWatchdogHandler (opts:Options) server =
    if opts.IsWatchdogEnabled then 
        let watchdog = new Watchdog((float opts.WatchdogMs), server)
        watchdog.StayAlive
    else
        server.SendNotFound
        

let runServer (opts:Options) =
    let server = new HttpServer(opts.Uri)

    let watchdogHandler = buildWatchdogHandler opts server

    let service = new Asn1ServiceBindings(new Asn1Service())

    let handlers path = 
        match path with
        | "stayAlive" -> watchdogHandler
        | "version" -> service.GetVersion
        | "ast" -> service.BuildAst
        | _ -> server.SendNotFound

    server.Serve handlers

    Console.Write "Server stopped"

[<EntryPoint>]
let main argv = 
    let result = Parser.Default.ParseArguments<Options>(argv)
    match result with
    | :? Parsed<Options> as parsed -> runServer parsed.Value; 0
    | :? NotParsed<Options> as notParsed -> 
            notParsed.Errors |> Seq.map (fun e -> Console.WriteLine e) |> ignore; 1
    | _ -> -1
