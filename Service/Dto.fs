namespace Service.Dto

open System.Collections.Generic

type FileData = {
    Name: string
    Contents: string
}

type InputFiles = {
    AsnFiles: IEnumerable<FileData>
    AcnFiles: IEnumerable<FileData>
}

type Output = {
    ErrorCode: int
    Files: IEnumerable<FileData>
    Messages: IEnumerable<string>
}
