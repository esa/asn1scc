module OutDirectories
open System.IO
open CommonTypes
open AbstractMacros

#if false
// TODO: Scala
let asn1rtlDirName    l target = match l with C -> "" | Scala -> Path.Combine("src", "main", "scala", "asn1scala") | Ada when target=None -> "" | Ada -> "asn1rtl"
let srcDirName        l target = match l with C -> "" | Scala -> Path.Combine("src", "main", "scala", "asn1src") | Ada when target=None -> "" | Ada -> "src"
let boardsDirName     l target = match l with C -> "" | Scala -> "" | Ada when target=None -> "" | Ada -> "boards"



let getDirInfo l (target:Targets option) rootDir =
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
            boardsDir=Path.Combine(rootDir, asn1rtlDirName l target)
        }

let getTopLevelDirs (l:ProgrammingLanguage) target =
    match l with
    | C     -> []
    | Scala -> [asn1rtlDirName l target; srcDirName l target; "lib"] // TODO: Scala
    | Ada when target = None     -> []
    | Ada   -> [asn1rtlDirName l target; srcDirName l target; boardsDirName l target]



    
#endif