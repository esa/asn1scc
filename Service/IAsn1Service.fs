namespace Service

type IAsn1Service =
    abstract member Version : string
    abstract member BuildAst : Dto.InputFiles -> Dto.Output