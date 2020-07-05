module OutDirectories
open System.IO
open CommonTypes

let asn1rtlDirName    l = match l with C -> "" | Ada -> "asn1rtl"
let srcDirName        l = match l with C -> "" | Ada -> "src"
let boardsDirName     l = match l with C -> "" | Ada -> "boards"

let getTopLevelDirs (l:ProgrammingLanguage) =
    match l with
    | C     -> []
    | Ada   -> [asn1rtlDirName l; srcDirName l; boardsDirName l]

let getBoardNames (l:ProgrammingLanguage) =
    match l with
    | C     -> []
    | Ada   -> ["x86";"stm32";"msp430"] 

let getBoardDirs (l:ProgrammingLanguage) =
    getBoardNames l |> List.map(fun s -> Path.Combine(boardsDirName l, s))

