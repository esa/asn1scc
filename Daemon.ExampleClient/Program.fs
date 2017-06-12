open System
open System.Net.Http

[<EntryPoint>]
let main argv = 
    let uri = if argv.Length > 0 then argv.[0] else "http://localhost:9749/"

    let downloadVersion = async {
        Console.WriteLine "Asn1Scc Daemon Example Client"

        use client = new HttpClient()
        client.BaseAddress <- new Uri(uri)

        let! versionResponse = Async.AwaitTask (client.GetAsync "version")
        let! version = Async.AwaitTask (versionResponse.Content.ReadAsStringAsync())
        Console.WriteLine("Connected to service version: " + version);
    }

    Async.RunSynchronously downloadVersion

    0
