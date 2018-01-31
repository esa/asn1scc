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

module ParameterizedAsn1Ast

open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime
open CommonTypes

open FsUtils

type AstRoot = {
    Files: list<Asn1File>
    Encodings:list<Asn1Encoding>
    GenerateEqualFunctions:bool
    TypePrefix:string
    AstXmlAbsFileName:string
    IcdUperHtmlFileName:string
    IcdAcnHtmlFileName:string
    CheckWithOss:bool
    mappingFunctionsModule : string option
    integerSizeInBytes : int            //currently only the value of 8 bytes (64 bits) is supported
}

and Asn1File = {
    FileName:string;
    Tokens: IToken array
    Modules : list<Asn1Module>
}

and Asn1Module = {
    Name:StringLoc
    TypeAssignments : list<TypeAssignment>
    ValueAssignments : list<ValueAssignment>
    Imports : list<ImportedModule>
    Exports : Exports
    Comments : string array
}

and Exports =
    | All
    | OnlySome of list<string>

and  ImportedModule = {
    Name:StringLoc
    Types:list<StringLoc>
    Values:list<StringLoc>
}

and TypeAssignment = {
    Name:StringLoc
    Type:Asn1Type
    Parameters : TemplateParameter list
    Comments: string array
}

and TemplateParameter =
    | TypeParameter of StringLoc
    | ValueParameter of Asn1Type*StringLoc

and ValueAssignment = {
    Name:StringLoc
    c_name:string
    ada_name:string
    Type:Asn1Type
    Value:Asn1Value
    Scope : ValueScope
}

and ValueScope =
    | GlobalScope
    | TypeScope  of StringLoc*StringLoc     

and Asn1Type = {
    Kind:Asn1TypeKind;
    Constraints:list<Asn1Constraint>;
    Location: SrcLoc   //Line no, Char pos
}

and Asn1TypeKind =
    | Integer 
    | Real    
    | IA5String 
    | NumericString
    | OctetString 
    | NullType
    | BitString         
    | Boolean 
    | Enumerated        of list<NamedItem>
    | SequenceOf        of Asn1Type    
    | Sequence          of list<SequenceChild>
    | Choice            of list<ChildInfo>
    | ReferenceType     of StringLoc*StringLoc*list<TemplateArgument>

and TemplateArgument =
    | ArgType of Asn1Type
    | ArgValue of Asn1Value
    | TemplateParameter of StringLoc    //name of parameter

and NamedItem = {
    Name:StringLoc
    _value:Asn1Value option
    Comments: string array
}

and SequenceChild =
    | ChildInfo of ChildInfo
    | ComponentsOf of StringLoc*StringLoc

and ChildInfo = {
        Name:StringLoc;
        Type:Asn1Type;
        Optionality:Asn1Optionality option
        AcnInsertedField:bool
        Comments: string array
    }


and Asn1Optionality = 
    | AlwaysAbsent
    | AlwaysPresent
    | Optional  
    | Default   of Asn1Value


and Asn1Value = {
    Kind:Asn1ValueKind
    Location: SrcLoc
}


and Asn1ValueKind =
    |   IntegerValue        of IntLoc
    |   RealValue           of DoubleLoc
    |   StringValue         of StringLoc
    |   BooleanValue        of BoolLoc
    |   BitStringValue      of StringLoc
    |   OctetStringValue    of list<ByteLoc>
    |   RefValue            of StringLoc*StringLoc
    |   SeqOfValue          of list<Asn1Value>
    |   SeqValue            of list<StringLoc*Asn1Value>
    |   ChValue             of StringLoc*Asn1Value
    |   NullValue
    |   EmptyList


and Asn1Constraint = 
    | SingleValueContraint              of Asn1Value             
    | RangeContraint                    of Asn1Value*Asn1Value*bool*bool    //min, max, InclusiveMin(=true), InclusiveMax(=true)         
    | RangeContraint_val_MAX            of Asn1Value*bool         //min, InclusiveMin(=true)
    | RangeContraint_MIN_val            of Asn1Value*bool         //max, InclusiveMax(=true)
    | TypeInclusionConstraint           of StringLoc*StringLoc     
    | SizeContraint                     of Asn1Constraint               
    | AlphabetContraint                 of Asn1Constraint           
    | UnionConstraint                   of Asn1Constraint*Asn1Constraint*bool //left,righ, virtual constraint
    | IntersectionConstraint            of Asn1Constraint*Asn1Constraint
    | AllExceptConstraint               of Asn1Constraint
    | ExceptConstraint                  of Asn1Constraint*Asn1Constraint
    | RootConstraint                    of Asn1Constraint
    | RootConstraint2                   of Asn1Constraint*Asn1Constraint
    | WithComponentConstraint           of Asn1Constraint
    | WithComponentsConstraint          of list<NamedConstraint>

and NamedConstraint = {
    Name:StringLoc;
    Contraint:Asn1Constraint option
    Mark:NamedConstraintMark
}

and NamedConstraintMark =
    | NoMark
    | MarkPresent
    | MarkAbsent
    | MarkOptional



let getModuleByName (r:AstRoot) (mdName:StringLoc) =
    match r.Files|> Seq.collect(fun x -> x.Modules) |> Seq.tryFind(fun x -> x.Name = mdName) with
    | None  -> raise (SemanticError(mdName.Location, sprintf "Unknown module '%s'" mdName.Value))
    | Some (x) -> x


let getTasByName  (tsName:StringLoc) (m:Asn1Module) =
    match m.TypeAssignments |> Seq.tryFind(fun x -> x.Name = tsName) with
    | Some(x)   -> x
    | None      -> raise(SemanticError(tsName.Location, sprintf "No type assignment with name '%s' found in the module '%s'" tsName.Value m.Name.Value))


let getTypeAssignment r m t = m |> getModuleByName r |> getTasByName t

let rec TryGetActualType (t:Asn1Type) (r:AstRoot) =
    match t.Kind with
    | ReferenceType(mn,tasname, _) ->
        let mods = r.Files |> List.collect (fun x -> x.Modules) 
        match  mods |> Seq.tryFind(fun m -> m.Name = mn) with
        | Some newmod ->
            match newmod.TypeAssignments |> Seq.tryFind(fun tas -> tas.Name.Value = tasname.Value) with
            | Some tas  -> TryGetActualType tas.Type r
            | None      -> None
        | None              -> None
    | _                         -> Some t


let rec GetActualType (t:Asn1Type) (r:AstRoot) =
    match t.Kind with
    | ReferenceType(mn,tasname, _) ->
        match TryGetActualType t r with
        | Some t    -> t
        | None      -> raise(SemanticError(tasname.Location, sprintf "Reference type: %s.%s can not be resolved" mn.Value tasname.Value ))
    
    | _                         -> t
