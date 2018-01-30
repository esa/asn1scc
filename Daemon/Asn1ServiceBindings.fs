namespace Daemon

open Service
open System.Net

type Asn1ServiceBindings(service:IAsn1Service) =

    member this.GetVersion (context:HttpListenerContext) = Http.SendPlainText context.Response service.Version

    member this.BuildAst (context:HttpListenerContext) = 
        let data = Http.ReadJson<Dto.InputFiles> context.Request
        Http.SendJson context.Response (service.BuildAst data)

