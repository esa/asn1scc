namespace Service.Dto

open System.Runtime.Serialization
open System.Collections.Generic

[<NoEquality;NoComparison;DataContract>]
type FileData = {
    [<field : DataMember(Name="Name")>]
    Name: string

    [<field : DataMember(Name="Contents")>]
    Contents: string
}

[<NoEquality;NoComparison;DataContract>]
type InputFiles = {
    [<field : DataMember(Name="AsnFiles")>]
    AsnFiles: IEnumerable<FileData>

    [<field : DataMember(Name="AcnFiles")>]
    AcnFiles: IEnumerable<FileData>
}

[<NoEquality;NoComparison;DataContract>]
type Output = {
    [<field : DataMember(Name="ErrorCode")>]
    ErrorCode: int

    [<field : DataMember(Name="Files")>]
    Files: IEnumerable<FileData>

    [<field : DataMember(Name="Messages")>]
    Messages: IEnumerable<string>
}
