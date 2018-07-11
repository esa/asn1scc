namespace Service.Dto

open System.Collections.Generic

type GenerationOptions = {
    Encoding: CommonTypes.Asn1Encoding
    TargetLanguage: CommonTypes.ProgrammingLanguage
    IntegerSizeInBytes: int
    FloatingPointSizeInBytes: int
    TypePrefix: string
    FieldPrefix: CommonTypes.FieldPrefix
    RenamePolicy: CommonTypes.EnumRenamePolicy
}

type FileData = {
    Name: string
    Contents: string
}

type Input = {
    AsnFiles: IEnumerable<FileData>
    AcnFiles: IEnumerable<FileData>
    Options: GenerationOptions
}

type Output = {
    ErrorCode: int
    Files: IEnumerable<FileData>
    Messages: IEnumerable<string>
}
