module Daemon.Http

open System.Net
open System.Text
open Newtonsoft.Json
open System.IO

let private serializer = 
    let settings = new JsonSerializerSettings()
    settings.Converters.Add(new Converters.StringEnumConverter())
    settings.Converters.Add(new Converters.DiscriminatedUnionConverter())
    JsonSerializer.Create(settings)

let ReadJson<'T> (request:HttpListenerRequest) =
    use reader = new StreamReader(request.InputStream)
    serializer.Deserialize(reader, typeof<'T>) :?> 'T

let SendJson<'T> (response:HttpListenerResponse) (obj:'T) =
    response.StatusCode <- (int HttpStatusCode.OK)
    response.SendChunked <- true
    response.ContentType <- "application/json"
    response.ContentEncoding <- Encoding.UTF8

    use writer = new StreamWriter(response.OutputStream)
    serializer.Serialize(writer, obj) 

let private WriteText (response:HttpListenerResponse) (text:string) =
    let bytes = Encoding.UTF8.GetBytes text

    response.ContentType <- "text/plain"
    response.ContentEncoding <- Encoding.UTF8
    response.ContentLength64 <- (int64 bytes.Length)

    response.OutputStream.Write(bytes, 0, bytes.Length)

let SendPlainText (response:HttpListenerResponse) text =
    response.StatusCode <- (int HttpStatusCode.OK)
    WriteText response text

let SendError (response:HttpListenerResponse) (code:HttpStatusCode) =
    response.StatusCode <- (int code)
    WriteText response ("Error: " + code.ToString() + " " + (int code).ToString())
    