module OutDirectories
open System.IO
open CommonTypes
open AbstractMacros

// TODO: Scala
let asn1rtlDirName    l target = match l with C -> "" | Scala -> Path.Combine("src", "main", "scala", "asn1scala") | Ada when target=None -> "" | Ada -> "asn1rtl"
let srcDirName        l target = match l with C -> "" | Scala -> Path.Combine("src", "main", "scala", "asn1src") | Ada when target=None -> "" | Ada -> "src"
let boardsDirName     l target = match l with C -> "" | Scala -> "" | Ada when target=None -> "" | Ada -> "boards"

type DirInfo = {
    rootDir     : string   
    srcDir      : string
    asn1rtlDir  : string
    boardsDir   : string
}


let getDirInfo l target rootDir =
    match l with
    | C     -> {rootDir = rootDir; srcDir=rootDir;asn1rtlDir=rootDir;boardsDir=rootDir}
    | Scala -> {
            rootDir = rootDir;
            srcDir=Path.Combine(rootDir, srcDirName l target);
            asn1rtlDir=Path.Combine(rootDir, asn1rtlDirName l target);
            boardsDir=rootDir
        } // TODO: Scala
    | Ada   when target = None -> {rootDir = rootDir; srcDir=rootDir;asn1rtlDir=rootDir;boardsDir=rootDir}
    | Ada   -> 
        {
            rootDir = rootDir; 
            srcDir=Path.Combine(rootDir, srcDirName l target);
            asn1rtlDir=Path.Combine(rootDir, asn1rtlDirName l target);
            boardsDir=Path.Combine(rootDir, boardsDirName l target)
        }

let getTopLevelDirs (l:ProgrammingLanguage) target =
    match l with
    | C     -> []
    | Scala     -> [asn1rtlDirName l target; srcDirName l target; "lib"] // TODO: Scala
    | Ada when target = None     -> []
    | Ada   -> [asn1rtlDirName l target; srcDirName l target; boardsDirName l target]

let getBoardNames (l:ProgrammingLanguage) target =
    match l with
    | C     -> []
    | Scala -> [] // TODO: Scala
    | Ada   -> 
        match target with
        | None              -> ["x86"]  //default board
        | Some X86          -> ["x86"] 
        | Some Stm32        -> ["stm32"] 
        | Some Msp430       -> ["msp430"] 
        | Some AllBoards    -> ["x86";"stm32";"msp430"] 

let getBoardDirs (l:ProgrammingLanguage) target =
    getBoardNames l target |> List.map(fun s -> Path.Combine(boardsDirName l target , s))


    
    
