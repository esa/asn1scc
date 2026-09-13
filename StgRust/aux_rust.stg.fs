module aux_rust
open System
open System.Numerics
open CommonTypes

let rtlModuleName () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "aux_rust" "rtlModuleName" []

let PrintMakeFile (arrsFilesNoExt:seq<string>) (bWordSize4:bool) (bFpWordSize4:bool) (bAsn1sccStreaming:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "aux_rust" "PrintMakeFile" [("arrsFilesNoExt",(arrsFilesNoExt|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bWordSize4",bWordSize4 :>Object);("bFpWordSize4",bFpWordSize4 :>Object);("bAsn1sccStreaming",bAsn1sccStreaming :>Object)]

let emitVisualStudioSolution () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "aux_rust" "emitVisualStudioSolution" []

let PrintCargoToml (sCrateName:string) (arrsFilesNoExt:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "aux_rust" "PrintCargoToml" [("sCrateName",(if sCrateName = null then null else ST.StrHelper sCrateName:>Object) );("arrsFilesNoExt",(arrsFilesNoExt|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

