namespace Service

type IAsn1Service =
    abstract member Version : string
    abstract member BuildAst : Dto.Input -> Dto.Output