(*
* Copyright (c) 2008-2012 Semantix and (c) 2012-2015 Neuropublic
*
* This file is part of the ASN1SCC tool.
*
* Licensed under the terms of GNU General Public Licence as published by
* the Free Software Foundation.
*
*  For more informations see License.txt file
*)

module ST


open FsUtils
open System
open System.Collections
open System.Numerics
open System.Globalization
open Antlr.StringTemplate
open System.IO

type dummy = 
    | Dummy1 

let mutable lang:CommonTypes.ProgrammingLanguage = CommonTypes.ProgrammingLanguage.Ada
let mutable double2StringPlain :bool = false 

type StrHelper =
    StrHelper of string
    with 
    override this.ToString () = 
        match this with  StrHelper(actStr) -> actStr
    member this.U1 =
        match this with  StrHelper(actStr) -> actStr.U1
    member this.L1 =
        match this with  StrHelper(actStr) -> actStr.L1
    member this.RDA =
        match this with  StrHelper(actStr) -> actStr.RDA
    member this.JSEsc =
        match this with  StrHelper(actStr) -> actStr.JSEsc
    member this.ISQ =
        match this with  StrHelper(actStr) -> actStr.ISQ
    member this.HtmlEsc =
        match this with  StrHelper(actStr) -> actStr.HtmlEsc
    member this.XmlEsc =
        match this with  StrHelper(actStr) -> actStr.HtmlEsc
    member this.EDQ =
        match this with  StrHelper(actStr) -> actStr.EDQ
        

type BasicFormatRenderer() =
    interface IAttributeRenderer with
        member this.ToString o = o.ToString()
        member this.ToString(o,format) =
            let frmStr = "{0:" + format + "}"
            String.Format(frmStr, o)


type BasicFormatRenderer2<'T when 'T :equality>() =
    interface IAttributeRenderer with
        member this.ToString o = (o :?> PrimitiveWithLocation<'T>).Value.ToString()
        member this.ToString(o,format) =    
            let frmStr = "{0:" + format + "}"
            String.Format(frmStr, (o :?> PrimitiveWithLocation<'T>).Value)


type StringFormatRenderer() =
    static member TS o format =
        let frmStr = "{0," + format + "}";
        String.Format(frmStr, o.ToString())
    interface IAttributeRenderer with
        member this.ToString o = o.ToString()
        member this.ToString(o,format) = StringFormatRenderer.TS o format

type StringFormatRenderer2() =
    interface IAttributeRenderer with
        member this.ToString o = (o :?> StringLoc).Value.ToString()
        member this.ToString(o,format) = StringFormatRenderer.TS (o :?> StringLoc).Value format


type RealFormatRenderer() =
    interface IAttributeRenderer with
        member this.ToString (o:Object) = 
            let v = o :?> double
            let result = 
                match double2StringPlain with
                | true      -> v.ToString()
                | false     -> v.ToString(FsUtils.doubleParseString, NumberFormatInfo.InvariantInfo)
//            String.Format(NumberFormatInfo.InvariantInfo, "{0}", o)
//            if ((v%1.0) = 0.0) && not(result.Contains("E")) then
//                result + ".0"
//            else
//                result
            result
        member this.ToString(o,format) = 
            String.Format(NumberFormatInfo.InvariantInfo, "{0:" + format + "}", o)

type RealFormatRenderer2() =
    interface IAttributeRenderer with
        member this.ToString o =
            String.Format(NumberFormatInfo.InvariantInfo, "{0}", (o :?> DoubleLoc).Value)
        member this.ToString(o,format) = 
            String.Format(NumberFormatInfo.InvariantInfo, "{0:" + format + "}", (o :?> DoubleLoc).Value)

            
type BigIntegerFormatRenderer() =
    static member TS1 (obj:BigInteger) =
        if (obj > BigInteger Int32.MinValue && obj < BigInteger Int32.MaxValue) then
            obj.ToString();
        else
            match lang with
            | CommonTypes.ProgrammingLanguage.C -> 
                if obj = (BigInteger Int64.MinValue) then
                    "LLONG_MIN"
                else
                    if (obj > BigInteger Int64.MaxValue) then
                        obj.ToString() + "ULL"
                    else
                        obj.ToString() + "LL"
            | CommonTypes.ProgrammingLanguage.Scala ->
                if obj = (BigInteger Int64.MinValue) then
                    "Long.MinValue"
                else
                    if (obj > BigInteger Int64.MaxValue) then
                        let uInt: uint64 = uint64 obj
                        (int64 uInt).ToString() + "L"
                    else
                        obj.ToString() + "L"
            | _ -> obj.ToString()
    static member TS2(o:Object, format) =
        let frmStr = "{0:" + format + "}";
        String.Format(frmStr, o);
    interface IAttributeRenderer with
        member this.ToString o = BigIntegerFormatRenderer.TS1 (o :?> BigInteger)
        member this.ToString(o,format) = BigIntegerFormatRenderer.TS2(o, format)


type BasicFormatRendererBigInt() =
    interface IAttributeRenderer with
        member this.ToString o = BigIntegerFormatRenderer.TS1 (o :?> IntLoc).Value
        member this.ToString(o,format) = BigIntegerFormatRenderer.TS2((o :?> IntLoc).Value, format)



let cache = System.Collections.Generic.Dictionary<string, StringTemplateGroup>()

let macrosWithErrorsDiscoveredInRunTime = System.Collections.Generic.Dictionary<string, int>()

let runsUnderMono() = 
    match System.Type.GetType("Mono.Runtime") with
    | null  -> false
    | _     -> true

let DollarDelimeterFiles = ["xml"; "icd_uper"; "xml_outputs"]

let get_group  fileName =
    if cache.ContainsKey fileName then
        cache.[fileName]
    else
        let applicationFolder = Path.GetDirectoryName(System.Reflection.Assembly.GetAssembly(typedefof<dummy>).Location);
        let devFolders = [@"C:\prj\GitHub\asn1scc\StgC"; @"C:\prj\GitHub\asn1scc\StgAda"; @"C:\prj\GitHub\asn1scc\StgScala"]
        let custFolder =
            match Path.GetDirectoryName fileName with
            | ""    -> []
            | _     -> [Path.GetFullPath(Path.Combine(System.IO.Directory.GetCurrentDirectory(), Path.GetDirectoryName fileName))] 
        let stgFoldres = match devFolders |> Seq.forall(fun devFolder -> Directory.Exists devFolder) with
                         | true  -> devFolders @ custFolder @ [ (System.IO.Directory.GetCurrentDirectory ());applicationFolder ]   |> Seq.toArray
                         | false -> custFolder @ [ (System.IO.Directory.GetCurrentDirectory ()); applicationFolder ]  |> Seq.toArray
        let grpLoader = CommonGroupLoader(StringTemplateGroup.DEFAULT_ERROR_LISTENER, stgFoldres)
                        
        StringTemplateGroup.RegisterGroupLoader(grpLoader);
        let fileNameNotExt = Path.GetFileNameWithoutExtension(fileName)
        let HasDollarDelimeter () =
            let ContainsMagicLine (folder:string) =
                let absPath = System.IO.Path.Combine(folder, fileNameNotExt+".stg")
                match System.IO.File.Exists absPath with
                | false -> false
                | true  ->
                    let lines = System.IO.File.ReadAllLines absPath
                    lines |> Array.exists (fun (l:string) -> l.Contains("delimiters \"$\", \"$\""))
            let a1 = stgFoldres |> Array.exists  ContainsMagicLine
            let a2 = DollarDelimeterFiles |> Seq.exists ((=) fileNameNotExt)
            a1 || a2
        let group = match HasDollarDelimeter () with
                    |true   -> grpLoader.LoadGroup(fileNameNotExt, null, typedefof<Antlr.StringTemplate.Language.DefaultTemplateLexer>);
                    |false  -> grpLoader.LoadGroup(fileNameNotExt);

        group.RegisterAttributeRenderer(typedefof<BigInteger>, new BigIntegerFormatRenderer());
        group.RegisterAttributeRenderer(typedefof<UInt64>, new BasicFormatRenderer());
        group.RegisterAttributeRenderer(typedefof<Int64>, new BasicFormatRenderer());
        group.RegisterAttributeRenderer(typedefof<int>, new BasicFormatRenderer());
        group.RegisterAttributeRenderer(typedefof<byte>, new BasicFormatRenderer());
        group.RegisterAttributeRenderer(typedefof<double>, new RealFormatRenderer());
        group.RegisterAttributeRenderer(typedefof<float>, new RealFormatRenderer());
        group.RegisterAttributeRenderer(typedefof<string>, new StringFormatRenderer());
        group.RegisterAttributeRenderer(typedefof<StrHelper>, new StringFormatRenderer());
        group.RegisterAttributeRenderer(typedefof<PrimitiveWithLocation<Double>>, new RealFormatRenderer2());
        group.RegisterAttributeRenderer(typedefof<PrimitiveWithLocation<string>>, new StringFormatRenderer2());
        group.RegisterAttributeRenderer(typedefof<PrimitiveWithLocation<BigInteger>>, new BasicFormatRendererBigInt());
        group.RegisterAttributeRenderer(typedefof<PrimitiveWithLocation<bool>>, new BasicFormatRenderer2<bool>());
        group.RegisterAttributeRenderer(typedefof<PrimitiveWithLocation<byte>>, new BasicFormatRenderer2<byte>());
        cache.[fileName] <- group
        group

let call fileName macroName (attrs:seq<string*#Object>)=
    let group = get_group fileName

    let template = group.GetInstanceOf(macroName);
    attrs |> Seq.iter(fun (attrName, obj) -> template.SetAttribute(attrName,obj))
    let ret = template.ToString 80
    //printfn "%s\n" ret
    ret

let call_generic fileName macroName (attrs:seq<string*#Object>)=
    let group = get_group fileName

    let template = group.GetInstanceOf(macroName);

    let argsNotPresentInLocalMacro = 
        attrs |> Seq.filter(fun (attrName, obj) -> (not (template.FormalArguments.Contains attrName))) |> Seq.map fst |> Seq.toList

    match argsNotPresentInLocalMacro with
    | []    -> ()
    | _     ->
        let macroKey = fileName + "#" + macroName
        match macrosWithErrorsDiscoveredInRunTime.ContainsKey macroKey with
        | true  -> () //we have already print a warning for this macro
        | false ->
            macrosWithErrorsDiscoveredInRunTime.Add(macroKey, 1)
            let newArgs = attrs |> Seq.map fst |> Seq.StrJoin ", "
            let newSignature = sprintf "%s(%s)" macroName newArgs
            let orldArgs = template.FormalArguments.Values |> Seq.cast |> Seq.StrJoin ", "
            let oldSignature = sprintf "%s(%s)" macroName orldArgs

            let errMsg = sprintf "Warning: Signature of macro '%s' of custom stg file '%s' has changed from\n  %s\nto\n  %s\nConsider updating your custom stg file '%s' to comply to the new signature.\n" macroName fileName oldSignature newSignature fileName
            Console.Error.WriteLine(errMsg)


    attrs |> 
    Seq.iter(fun (attrName, obj) -> 
        match template.FormalArguments.Contains attrName with
        | true  -> template.SetAttribute(attrName,obj)
        | false -> ()
        )
    let ret = template.ToString 80
    //printfn "%s\n" ret
    ret


let call2 (fileName, macroName, [<ParamArray>] attrs:array<string*#Object>)=
    let group = get_group fileName

    let template = group.GetInstanceOf(macroName);
    attrs |> Seq.iter(fun (attrName, obj) -> template.SetAttribute(attrName,obj))
    let ret = template.ToString 80
    //printfn "%s\n" ret
    ret

