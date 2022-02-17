module aux_c
open System
open System.Numerics
open CommonTypes

let PrintMakeFile (arrsFilesNoExt:seq<string>) (bWordSize4:bool) (bFpWordSize4:bool) (bAsn1sccStreaming:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.C; ST.double2StringPlain <- false
    ST.call "aux_c" "PrintMakeFile" [("arrsFilesNoExt",(arrsFilesNoExt|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bWordSize4",bWordSize4 :>Object);("bFpWordSize4",bFpWordSize4 :>Object);("bAsn1sccStreaming",bAsn1sccStreaming :>Object)]

let emitVisualStudioSolution () =
    ST.lang <- CommonTypes.ProgrammingLanguage.C; ST.double2StringPlain <- false
    ST.call "aux_c" "emitVisualStudioSolution" []

