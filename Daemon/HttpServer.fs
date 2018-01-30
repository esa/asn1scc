namespace Daemon

open System.Net
open System

type HttpServer(uri:string) =

    let listener = new HttpListener()
    let pathPrefix = (new Uri(uri.Replace("+", "localhost"))).AbsolutePath
    do  
        listener.Prefixes.Add(uri)
    let mutable stopRequested = false

    let ExtractCommandPath (uriPath:string) =
        if not (uriPath.StartsWith pathPrefix) then ""
        else uriPath.Substring(pathPrefix.Length)

    let handleRequest (context:HttpListenerContext) handlers = async {
        try
            let handler = handlers (ExtractCommandPath context.Request.Url.AbsolutePath)
            handler context
            context.Response.Close()
        with
            | _ -> Http.SendError context.Response HttpStatusCode.InternalServerError
     }

    member this.Serve handlers = 
        listener.Start()

        stopRequested <- false

        let asynctask = Async.FromBeginEnd(listener.BeginGetContext,listener.EndGetContext)
        async {
            while not stopRequested do
                try
                    let! context = asynctask
                    Async.Start (handleRequest context handlers)
                with
                    | :? HttpListenerException -> ()
        } |> Async.RunSynchronously 

    member this.Stop = 
        stopRequested <- true
        listener.Stop()

    member this.SendNotFound (context:HttpListenerContext) =
        Http.SendError context.Response HttpStatusCode.NotFound
    
