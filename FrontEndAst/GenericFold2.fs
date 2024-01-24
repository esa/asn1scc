module GenericFold2
open System
open System.Numerics

open Asn1Ast
open FsUtils


let enumIntegerType =
    {
        Asn1Type.Kind = Integer
        Constraints = []
        Location = FsUtils.emptyLocation
        acnProperties = None
    }

let stringType =
    {
        Asn1Type.Kind = IA5String
        Constraints = []
        Location = FsUtils.emptyLocation
        acnProperties = None
    }

let sizeIntegerType =
    {
        Asn1Type.Kind = Integer
        Constraints = [RangeConstraint_val_MAX ({Asn1Value.Kind = IntegerValue(FsUtils.IntLoc.ByValue 0I); Location=FsUtils.emptyLocation},true)]
        Location = FsUtils.emptyLocation
        acnProperties = None
    }



let rec foldMap func state lst =
    match lst with
    | []        -> [],state
    | h::tail   ->
        let procItem, newState = func state h
        let restList, finalState = tail |> foldMap func newState
        procItem::restList, finalState



type ScopeNode =
    | MD of string      //MODULE
    | TA of string      //TYPE ASSIGNMENT
    | VA of string      //VALUE ASSIGNMENT
    | SEQ_CHILD of string
    | CH_CHILD of string      //SEQUENCE OF CHOICE CHILD
    | PRM of string     // ACN parameter
    | TMP of string     // ACN temp type
    | SQF               //SEQUENCE OF CHILD
    with
        member this.StrValue =
            match this with
            | MD strVal
            | TA strVal
            | VA strVal
            | PRM strVal
            | SEQ_CHILD strVal
            | CH_CHILD strVal
            | TMP strVal     -> strVal
            | SQF           -> "#"
        override this.ToString() = this.StrValue

type UserDefinedTypeScope = ScopeNode list

let visitModule (md:Asn1Module) : UserDefinedTypeScope=
    [MD md.Name.Value]

let visitTas (s:UserDefinedTypeScope) (ts:TypeAssignment) : UserDefinedTypeScope=
    s@[TA ts.Name.Value]

let visitTypeVas (s:UserDefinedTypeScope) (vas:ValueAssignment) : UserDefinedTypeScope=
    s@[VA vas.Name.Value]

let visitRefType (md:string) (ts:string) : UserDefinedTypeScope=
    [MD md; TA ts]

//let visitRevValue (md:string) (vs:string) =
//    {UserDefinedTypeScope.typeID=[MD md; VA vs]; asn1TypeName=None; asn1VarName=Some vs;varID=[]}

let visitSeqChild (s:UserDefinedTypeScope) (ch:ChildInfo) : UserDefinedTypeScope=
    s@[SEQ_CHILD ch.Name.Value]

let visitChoiceChild (s:UserDefinedTypeScope) (ch:ChildInfo) : UserDefinedTypeScope=
    s@[CH_CHILD ch.Name.Value]

let visitSeqOfChild (s:UserDefinedTypeScope) : UserDefinedTypeScope =
    s@[SQF]

//let visitWithComponentChild (s:UserDefinedTypeScope) =
//    s@[WITH_COMP]


type VarScopNode =
    | VA2 of string      //VALUE ASSIGNMENT
    | DV        //DEFAULT VALUE
    | NI  of string      //NAMED ITEM VALUE (enum)
    | CON of int         // constraint index
    | SQOV of int             //SEQUENCE OF VALUE (value index)
    | SQCHILD   of string   //child value (SEQUENCE, CHOICE)
    | VL of int         //value index
    | IMG of int        //non ASN.1 value. Required when constructing values for types in backends
    with
        member this.StrValue =
            match this with
            | VA2 strVal -> strVal
            | DV        -> "DV"
            | NI    ni  -> ni
            | VL   idx  -> "v" + idx.ToString()
            | IMG  idx  -> "img" + idx.ToString()
            | CON idx   -> "c" + idx.ToString()
            | SQOV i     -> sprintf"[%d]" i
            | SQCHILD  s-> s

        override this.ToString() = this.StrValue

(*
{
    asn1TypeName : string option
    asn1VarName : string option
    typeID : ScopeNode list
    varID : VarScopNode list
}*)

type Scope =
    | UserDefinedTypeScope of UserDefinedTypeScope
    | GlobanIntScope



type UserDefinedVarScope = VarScopNode list

let visitValueVas  (vs:ValueAssignment) : UserDefinedVarScope=
    [VA2 vs.Name.Value]



let visitDefaultValue () : UserDefinedVarScope =
    [DV]

let visitNamedItemValue  (nm:NamedItem) : UserDefinedVarScope=
    [NI nm.Name.Value]

let visitConstraint (s:UserDefinedVarScope) : UserDefinedVarScope=
    s@[CON 0]

let visitSiblingConstraint (s:UserDefinedVarScope) : UserDefinedVarScope =
    let idx, xs =
        match s |> List.rev with
        | (CON idx)::xs  -> idx, xs
        | _              -> raise(BugErrorException "invalid call to visitSiblingConstraint")
    xs@[CON (idx+1)]


let visitValue (s:UserDefinedVarScope) :UserDefinedVarScope =
    s @[VL 0]

let visitSiblingValue (s:UserDefinedVarScope) :UserDefinedVarScope =
    let idx, xs =
        match s |> List.rev with
        | (VL idx)::xs  -> idx, xs
        | _              -> raise(BugErrorException "invalid call to visitSiblingConstraint")
    xs@[VL (idx+1)]

let visitSeqOfValue (s:UserDefinedVarScope) idx :UserDefinedVarScope =
    s @[SQOV idx]

let visitSeqChildValue (s:UserDefinedVarScope) childName :UserDefinedVarScope =
    s @[SQCHILD childName ]

let error (loc: SrcLoc) format =
    let doAfter s =
        raise (SemanticError (loc, s))
    Printf.ksprintf doAfter format

type CheckContext =
    | CheckContent
    | CheckSize
    | CheckCharacter


let foldAstRoot
    rootFunc
    fileFunc
    modFunc
    tasFunc
    vasFunc
    typeFunc
//    refTypeFunc
    integerFunc
    realFunc
    ia5StringFunc
    numericStringFunc
    octetStringFunc
    nullTypeFunc
    bitStringFunc
    booleanFunc
    enumeratedFunc
    enmItemFunc
    seqOfTypeFunc
    seqTypeFunc
    chTypeFunc
    sequenceChildFunc
    alwaysAbsentFunc
    alwaysPresentFunc
    optionalFunc
    defaultFunc
    choiceChildFunc
    refValueFunc
    enumValueFunc
    intValFunc
    realValFunc
    ia5StringValFunc
    numStringValFunc
    boolValFunc
    octetStringValueFunc
    bitStringValueFunc
    nullValueFunc
    seqOfValueFunc
    seqValueFunc
    chValueFunc
    singleValueConstraintFunc
    rangeConstraintFunc
    rangeConstraint_val_MAXFunc
    rangeConstraint_MIN_valFunc
    rangeConstraint_MIN_MAXFunc
    typeInclConstraintFunc
    unionConstraintFunc
    intersectionConstraintFunc
    allExceptConstraintFunc
    exceptConstraintFunc
    rootConstraintFunc
    rootConstraint2Func
    sizeConstraint
    alphabetConstraint
    withComponentConstraint
    withComponentConstraints
    globalIntType
    getSequenceOfTypeChild
    getChoiceTypeChild
    getSequenceTypeChild
    getTypeKind
    getTypeFromStateFunc
    (r:AstRoot)
    (us:'UserState)
    =

    let rec loopAstRoot (r:AstRoot) (us:'UserState) =
        let files, nus = r.Files |> foldMap (loopFile r) us
        rootFunc nus r files
    and loopFile (r:AstRoot) us (f:Asn1File) =
        let modules, nus = f.Modules |> foldMap (loopModule r f) us
        fileFunc nus r f modules
    and loopModule (r:AstRoot) (f:Asn1File) us (m:Asn1Module) =
        let s = visitModule m
        let tases, nus1 = m.TypeAssignments |> foldMap (loopTypeAssignment s r f m) us
        let vases, nus2 = m.ValueAssignments |> foldMap  (loopValueAssignments s r f m) nus1
        modFunc nus2 r f m tases vases
    and loopTypeAssignment (s:UserDefinedTypeScope) (r:AstRoot) (f:Asn1File) (m:Asn1Module) (us:'UserState) (tas:TypeAssignment) =
        let s = visitTas s tas
        let asn1Type, newUs = loopType us (s,tas.Type) []
        tasFunc newUs r f m tas asn1Type
    and loopValueAssignments (s:UserDefinedTypeScope) (r:AstRoot) (f:Asn1File) (m:Asn1Module) (us:'UserState) (vas:ValueAssignment) =
        match vas.Type.Kind with
        | ReferenceType rf  ->
            let varScope = visitValueVas vas
            let typeScope = visitRefType rf.modName.Value (rf.tasName.Value (*+ "_" + vas.Name.Value*))   //visitTypeVas s vas
            let actType = GetActualType vas.Type  r
            // We temporarily create the new type (state is ignored)
            let newAsn1Type, _ = loopType us (typeScope, vas.Type) []
            let asn1Value, nus2 = loopAsn1Value us (typeScope, (getTypeKind newAsn1Type), actType) (Some (m.Name, vas.Name)) (varScope,vas.Value, false)


            vasFunc nus2 r f m vas newAsn1Type asn1Value
        | _                     -> raise (SemanticError (vas.Name.Location, "Unnamed types are not currently supported."))
    and loopType (us:'UserState) (s:UserDefinedTypeScope, t:Asn1Type) (witchCompsCons:Asn1Constraint list) =
        match getTypeFromStateFunc us s with
        | None  ->
            let conScope = visitConstraint []
            let withCompCons = (t.Constraints@witchCompsCons) |> List.choose(fun c -> match c with WithComponentConstraint _ -> Some c| WithComponentsConstraint _ -> Some c | _ -> None)
            (*
            match t.Kind with
            | ReferenceType (mdName,tasName, tabularized) when not (withCompCons.IsEmpty) ->
                let oldBaseType = Ast.GetBaseType t r
                let oldBaseType = {oldBaseType with Constraints = oldBaseType.Constraints@t.Constraints@witchCompsCons}
                loopType us (s, oldBaseType) []
            | _     ->*)
            let newTypeKind, nus = loopTypeKind us s t.Kind  withCompCons None
            let newCons, (retScope, nus1) = t.Constraints |> foldMap  (fun (ss, uds) c ->
                                                                                    let lc, uds2 = loopConstraint uds (s, newTypeKind,t) CheckContent (ss,c)
                                                                                    lc, (visitSiblingConstraint ss,uds2)) (conScope, nus)
            let fromWithComps, (_, nus2) = witchCompsCons |> foldMap  (fun (ss, uds) c ->
                                                                            let lc, uds2 = loopConstraint uds (s, newTypeKind,t) CheckContent (ss,c)
                                                                            lc, (visitSiblingConstraint ss, uds2)) (retScope, nus1)
            typeFunc nus2 s t newTypeKind (newCons,fromWithComps)
        | Some res ->
            res

    and loopTypeKind (us:'UserState) (s:UserDefinedTypeScope) (asn1TypeKind:Asn1TypeKind)  (witchCompsCons:Asn1Constraint list) (newBaseType) =
        match asn1TypeKind with
        | Integer       -> integerFunc newBaseType us
        | Real          -> realFunc    newBaseType us
        | IA5String     -> ia5StringFunc newBaseType us
        | NumericString -> numericStringFunc newBaseType us
        | OctetString   -> octetStringFunc newBaseType us
        | NullType      -> nullTypeFunc newBaseType us
        | BitString     -> bitStringFunc newBaseType us
        | Boolean       -> booleanFunc newBaseType us
        | Enumerated (enmItems) ->
            enumeratedFunc us newBaseType enmItems
        | SequenceOf (innerType)  ->
            let childScope = visitSeqOfChild s
            let withCompCons = witchCompsCons |> List.choose(fun c -> match c with WithComponentConstraint wc -> Some wc | _ -> None)
            let newInnerType, nus = loopType us (childScope, innerType) withCompCons
            seqOfTypeFunc nus newInnerType newBaseType
        | Sequence (children)     ->
            let withCompCons = witchCompsCons |> List.choose(fun c -> match c with WithComponentsConstraint wc -> Some wc | _ -> None) |> List.collect id
            let newChildren, fus =
                children |> foldMap  (fun cs chInfo ->
                    let childScope = visitSeqChild s chInfo
                    let childWithComps = withCompCons |> List.filter(fun x -> x.Name.Value = chInfo.Name.Value)
                    loopSequenceChild cs childScope chInfo childWithComps) us
            seqTypeFunc fus newChildren newBaseType
        | Choice (children)       ->
            let withCompCons = witchCompsCons |> List.choose(fun c -> match c with WithComponentsConstraint wc -> Some wc | _ -> None) |> List.collect id
            let newChildren, fus =
                children |> foldMap  (fun cs chInfo ->
                    let childScope = visitChoiceChild s chInfo
                    let childWithComps = withCompCons |> List.filter(fun x -> x.Name.Value = chInfo.Name.Value)
                    loopChoiceChild cs childScope chInfo childWithComps) us
            chTypeFunc fus newChildren newBaseType
        | ReferenceType rf ->
            let oldBaseType = GetBaseTypeByName rf.modName rf.tasName r
            let refTypeScope = [MD rf.modName.Value; TA rf.tasName.Value]
            let newBaseType, nus = loopType us (refTypeScope, oldBaseType) []
            let extraWithCompCons = (oldBaseType.Constraints) |> List.choose(fun c -> match c with WithComponentConstraint _ -> Some c| WithComponentsConstraint _ -> Some c | _ -> None)
            loopTypeKind nus s oldBaseType.Kind  (extraWithCompCons@witchCompsCons) (Some newBaseType)

        (*
            match witchCompsCons with
            | []    ->
                let oldBaseType = GetBaseTypeByName mdName tasName r
                let refTypeScope = [MD mdName.Value; TA tasName.Value]
                let newBaseType, nus = loopType us (refTypeScope, oldBaseType) witchCompsCons
                refTypeFunc us mdName tasName tabularized newBaseType
            | _     ->
                let oldBaseType = GetBaseTypeByName mdName tasName r
                let extraWithCompCons = (oldBaseType.Constraints) |> List.choose(fun c -> match c with WithComponentConstraint _ -> Some c| WithComponentsConstraint _ -> Some c | _ -> None)
                loopTypeKind us s oldBaseType.Kind  (extraWithCompCons@witchCompsCons)
                //loopType us (s, oldBaseType)
        *)
    and loopNamedItem (us:'UserState) (s:UserDefinedTypeScope) (ni:NamedItem) =
            match ni._value with
            | Some v    ->
                let globalIntScope, gloabIntKind = globalIntType()
                let newValue, fs = loopAsn1Value us (globalIntScope, gloabIntKind, enumIntegerType) None ((visitNamedItemValue ni),v, false)
                enmItemFunc  fs ni (Some newValue)
            | None      ->
                enmItemFunc us ni  None
    and loopSequenceChild (us:'UserState) (ts:UserDefinedTypeScope)  (chInfo:ChildInfo) (nc:NamedConstraint list) =
            let withCompCons = nc |> List.choose (fun x -> x.Constraint)
            let newType, us1 = loopType us (ts,chInfo.Type) withCompCons
            let presMark =
                nc |> Seq.tryFind (fun x -> x.Mark <> NoMark) |> Option.map (fun nc -> nc.Mark)
            let newOptionality, us2 =
                    match presMark, chInfo.Optionality with
                    | Some MarkPresent, _                   -> Some (alwaysPresentFunc ts), us1
                    | Some MarkAbsent,  _                   -> Some (alwaysAbsentFunc ts), us1
                    | _                                     ->
                        match chInfo.Optionality with
                        | Some Asn1Optionality.AlwaysAbsent     -> Some (alwaysAbsentFunc ts), us1
                        | Some Asn1Optionality.AlwaysPresent    -> Some (alwaysPresentFunc ts), us1
                        | Some Asn1Optionality.Optional         -> Some (optionalFunc ts), us1
                        | None                                  -> None, us1
                        | Some  (Asn1Optionality.Default   vl)  ->
                            let eqType = GetActualType chInfo.Type r
                            let newValue, us2 = loopAsn1Value  us1 (ts, getTypeKind newType, eqType) None ((visitDefaultValue ()), vl, false)
                            Some (defaultFunc ts newValue), us2
            sequenceChildFunc us2 ts chInfo newType newOptionality

    and loopChoiceChild (us:'UserState) (ts:UserDefinedTypeScope)  (chInfo:ChildInfo) (nc:NamedConstraint list) =
            let withCompCons = nc |> List.choose (fun x -> x.Constraint)
            let newType, nus = loopType us (ts, chInfo.Type) withCompCons
            choiceChildFunc nus ts   chInfo newType
    and loopAsn1Value (us:'UserState) (ts:UserDefinedTypeScope, newTypeKind, t:Asn1Type) (asn1ValName:(StringLoc*StringLoc) option) (vs:UserDefinedVarScope, v:Asn1Value, childValue:bool) : 'ASN1VALUE*'UserState =
        match v.Kind, t.Kind with
        | RefValue (md,vas), Enumerated (enmItems)   ->
                match enmItems |> Seq.tryFind (fun x -> x.Name.Value = vas.Value) with
                | Some enmItem    ->
                        enumValueFunc us asn1ValName ts vs v vas childValue
                | None          ->
                        let baseVal = GetBaseValue md vas r
                        loopAsn1Value us (ts, newTypeKind, t) (Some (md,vas)) (vs, baseVal, childValue)
        | RefValue (md,vas), _   ->
                        let baseVal = GetBaseValue md vas r
                        loopAsn1Value us (ts, newTypeKind, t) (Some (md,vas)) (vs, baseVal, childValue)
        | IntegerValue bi, Integer          ->
            intValFunc us asn1ValName ts vs v bi childValue
        | IntegerValue bi, Real  _           ->
            let dc:double = (double)bi.Value
            realValFunc us asn1ValName ts vs v {Value=dc;Location=v.Location} childValue
        | RealValue dc , Real   _                  ->
            realValFunc us asn1ValName ts vs v dc childValue
        | StringValue str, IA5String _   ->
            ia5StringValFunc us asn1ValName ts vs v str childValue
        | StringValue str , NumericString _   ->
            numStringValFunc us asn1ValName ts vs v str childValue
        | BooleanValue b, Boolean _       ->
            boolValFunc us asn1ValName ts vs v b childValue
        | OctetStringValue b, OctetString _         ->
            octetStringValueFunc us asn1ValName ts vs v b childValue
        | OctetStringValue b, BitString _        ->
            octetStringValueFunc us asn1ValName ts vs v b childValue
        | BitStringValue b, BitString _           ->
            bitStringValueFunc us asn1ValName ts vs v b childValue
        | BitStringValue b, OctetString _           ->
            bitStringValueFunc us asn1ValName ts vs v b childValue
        | NullValue , NullType _   ->
            nullValueFunc us asn1ValName ts vs v childValue
        | SeqOfValue  vals, SequenceOf chType    ->
            let eqType = GetActualType chType r
            let newChScope, newChT = getSequenceOfTypeChild us newTypeKind
            let newVals, (fs,_) = vals |> foldMap  (fun (ust,idx) chVal ->
                                                    let newV,newS = loopAsn1Value ust (newChScope, newChT, eqType) None ((visitSeqOfValue vs idx), chVal, true)
                                                    newV,(newS, idx+1)) (us,0)
            seqOfValueFunc fs asn1ValName ts vs v  newVals childValue
        | SeqValue    namedValues, Sequence children ->
            let newValues, fs =  namedValues |> foldMap (fun ust nc -> loopSeqValueChild ust newTypeKind children vs nc ) us
            seqValueFunc fs asn1ValName ts vs v newValues childValue
        | ChValue (name,vl), Choice children         ->
            match children |> Seq.tryFind(fun ch -> ch.Name.Value = name.Value) with
            | Some chType   ->
                    let newChScope, newChT = getChoiceTypeChild us newTypeKind name
                    let eqType = GetActualType chType.Type r
                    let newValue, fs = loopAsn1Value us (newChScope, newChT,eqType) None ((visitSeqChildValue vs name.Value),vl, true)
                    chValueFunc fs asn1ValName ts vs v name newValue childValue
            | None  ->
                error name.Location "Invalid alternative name '%s'" name.Value
        | _         ->
            error v.Location "Invalid combination ASN.1 type and ASN.1 value"


    and loopSeqValueChild (us:'UserState) (newSeqT)  (children: list<ChildInfo>)  (vs:UserDefinedVarScope) (nm:StringLoc, chv:Asn1Value) =
            let child =
                match children |> Seq.tryFind (fun ch -> ch.Name.Value = nm.Value) with
                | Some ch -> ch
                | None    -> error nm.Location "Invalid child name '%s'" nm.Value
            let newChScope, newChT = getSequenceTypeChild us newSeqT nm
            let eqType = GetActualType child.Type r
            let newValue, fs = loopAsn1Value us (newChScope, newChT, eqType) None ((visitSeqChildValue vs nm.Value),chv, true)
            (nm,newValue), fs

    and loopConstraint (us:'UserState) (s:UserDefinedTypeScope, newT, t:Asn1Type) (checContent:CheckContext) (cs:UserDefinedVarScope, c:Asn1Constraint) =
        let vtype =
            match checContent with
            | CheckSize         -> sizeIntegerType
            | CheckCharacter    -> stringType
            | CheckContent      -> GetActualType t r

        match c with
        | SingleValueConstraint v        ->
                let newValue, fs = loopAsn1Value us (s, newT ,vtype) None (visitValue cs,v, false)
                singleValueConstraintFunc fs newT t checContent newValue
        | RangeConstraint(v1,v2,b1,b2)   ->
                let vs1 = visitValue cs
                let vs2 = visitSiblingValue vs1
                let newValue1, fs1 = loopAsn1Value us (s, newT,vtype) None (vs1,v1, false)
                let newValue2, fs2 = loopAsn1Value fs1 (s, newT,vtype) None (vs2,v2, false)
                rangeConstraintFunc fs2 newT t checContent newValue1 newValue2 b1 b2
        | RangeConstraint_val_MAX (v,b)  ->
                let newValue, fs = loopAsn1Value us (s, newT,vtype) None (visitValue cs,v, false)
                rangeConstraint_val_MAXFunc fs newT t checContent newValue b
        | RangeConstraint_MIN_val (v,b)  ->
                let newValue, fs = loopAsn1Value us (s, newT,vtype) None (visitValue cs,v, false)
                rangeConstraint_MIN_valFunc fs newT t checContent newValue  b
        | RangeConstraint_MIN_MAX             ->
                rangeConstraint_MIN_MAXFunc us newT t checContent
        | TypeInclusionConstraint (md,tas)  ->
                let actTypeAllCons = GetActualTypeByNameAllConsIncluded md tas r
                let agrCon =
                    match actTypeAllCons.Constraints with
                    | []    -> None
                    | x::[] -> Some x
                    | x::xs ->
                        let aa = xs |> List.fold(fun s cc -> IntersectionConstraint (s,cc)) x
                        Some aa
                match agrCon with
                | Some c1 ->
                    let nc, fs = loopConstraint us (s, newT,t) checContent ((visitConstraint cs),c1)
                    typeInclConstraintFunc fs newT t (Some nc)
                | None -> typeInclConstraintFunc us newT t None
        | UnionConstraint (c1,c2, virtualCon)      ->
                let cs1 = visitConstraint cs
                let cs2 = visitSiblingConstraint cs1
                let nc1, fs1 = loopConstraint us ( s, newT,t) checContent (cs1,c1)
                let nc2, fs2 = loopConstraint fs1 (s, newT,t) checContent (cs2,c2)
                unionConstraintFunc fs2 newT t nc1 nc2 virtualCon
        | IntersectionConstraint(c1,c2)         ->
                let cs1 = visitConstraint cs
                let cs2 = visitSiblingConstraint cs1
                let nc1, fs1 = loopConstraint us  (s, newT,t) checContent (cs1,c1)
                let nc2, fs2 = loopConstraint fs1 (s, newT,t) checContent (cs2,c2)
                intersectionConstraintFunc fs2 newT t nc1 nc2
        | AllExceptConstraint c1 ->
                let nc, fs = loopConstraint us (s, newT,t) checContent ((visitConstraint cs),c1)
                allExceptConstraintFunc fs newT t nc
        | ExceptConstraint (c1,c2)  ->
                let cs1 = visitConstraint cs
                let cs2 = visitSiblingConstraint cs1
                let nc1, fs1 = loopConstraint us  (s, newT,t) checContent (cs1,c1)
                let nc2, fs2 = loopConstraint fs1 (s, newT,t) checContent (cs2,c2)
                exceptConstraintFunc fs2 newT t nc1 nc2
        | RootConstraint c1  ->
                let nc, fs = loopConstraint us (s, newT,t) checContent ((visitConstraint cs),c1)
                rootConstraintFunc fs newT t nc
        | RootConstraint2 (c1,c2)  ->
                let cs1 = visitConstraint cs
                let cs2 = visitSiblingConstraint cs1
                let nc1, fs1  = loopConstraint us (s, newT,t) checContent (cs1,c1)
                let nc2, fs2  = loopConstraint fs1 (s, newT,t) checContent (cs2,c2)
                rootConstraint2Func fs2 newT t nc1 nc2
        | SizeConstraint(sc)         ->
                let nc, fs = loopConstraint us (s, newT,t) CheckSize (visitConstraint cs,sc)
                sizeConstraint fs newT t nc
        | AlphabetConstraint c       ->
                let nc, fs = loopConstraint us (s, newT, t) CheckCharacter (visitConstraint cs,c)
                alphabetConstraint fs newT t nc
        | WithComponentConstraint c          -> withComponentConstraint us newT t
        | WithComponentsConstraint _         -> withComponentConstraints us newT t
    loopAstRoot r us


