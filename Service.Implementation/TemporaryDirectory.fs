namespace Service.Implementation.Utils

open System.IO
open System
open Service

type TemporaryDirectory() = 

    let path = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
    let dirInfo = Directory.CreateDirectory(path)

    member this.Path = path

    interface IDisposable with 
        member this.Dispose() = Directory.Delete(path, recursive=true)

    member this.FullPath fileName = Path.Combine(path, fileName)

    member this.Store (file:Dto.FileData) = 
        let filePath = this.FullPath file.Name
        File.WriteAllText(filePath, file.Contents)
        filePath

