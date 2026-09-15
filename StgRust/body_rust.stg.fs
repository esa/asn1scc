module body_rust
open System
open System.Numerics
open CommonTypes

let rtlModuleName () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "body_rust" "rtlModuleName" []

let printSourceFile (sFileNameWithoutExtension:string) (arrsIncludedFiles:seq<string>) (arrsAdaUseTypes:seq<string>) (arrsUserDefinedFunctions:seq<string>) (arrsValueAndTypeAssignments:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "body_rust" "printSourceFile" [("sFileNameWithoutExtension",(if sFileNameWithoutExtension = null then null else ST.StrHelper sFileNameWithoutExtension:>Object) );("arrsIncludedFiles",(arrsIncludedFiles|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsAdaUseTypes",(arrsAdaUseTypes|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsUserDefinedFunctions",(arrsUserDefinedFunctions|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsValueAndTypeAssignments",(arrsValueAndTypeAssignments|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let printTass (arrsAllProcs:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "body_rust" "printTass" [("arrsAllProcs",(arrsAllProcs|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

