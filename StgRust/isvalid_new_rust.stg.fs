module isvalid_new_rust
open System
open System.Numerics
open CommonTypes

let makeExpressionToStatement (sIsValidExp:string) (sErrCode:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_new_rust" "makeExpressionToStatement" [("sIsValidExp",(if sIsValidExp = null then null else ST.StrHelper sIsValidExp:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

