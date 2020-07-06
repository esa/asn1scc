module OutDirectories
open System.IO
open CommonTypes

let asn1rtlDirName    l = match l with C -> "" | Ada -> "asn1rtl"
let srcDirName        l = match l with C -> "" | Ada -> "src"
let boardsDirName     l = match l with C -> "" | Ada -> "boards"

type DirInfo = {
    rootDir     : string   
    srcDir      : string
    asn1rtlDir  : string
    boardsDir   : string
}

let getDirInfo l rootDir =
    match l with
    | C     -> {rootDir = rootDir; srcDir=rootDir;asn1rtlDir=rootDir;boardsDir=rootDir}
    | Ada   -> 
        {
            rootDir = rootDir; 
            srcDir=Path.Combine(rootDir, srcDirName l);
            asn1rtlDir=Path.Combine(rootDir, asn1rtlDirName l);
            boardsDir=Path.Combine(rootDir, boardsDirName l)
        }

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

