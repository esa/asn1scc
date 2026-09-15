module header_rust
open System
open System.Numerics
open CommonTypes

let rtlModuleName () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "rtlModuleName" []

let indentation (sStatement:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "indentation" [("sStatement",(if sStatement = null then null else ST.StrHelper sStatement:>Object) )]

let PrintSpecificationFile (sFileNameWithNoExtUpperCase:string) (sPackageName:string) (arrsIncludedModules:seq<string>) (arrsTypeAssignments:seq<string>) (arrsValueAssignments:seq<string>) (arrsPrototypes:seq<string>) (arrsUtilityDefines:seq<string>) (bHasEncodings:bool) (bXer:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "PrintSpecificationFile" [("sFileNameWithNoExtUpperCase",(if sFileNameWithNoExtUpperCase = null then null else ST.StrHelper sFileNameWithNoExtUpperCase:>Object) );("sPackageName",(if sPackageName = null then null else ST.StrHelper sPackageName:>Object) );("arrsIncludedModules",(arrsIncludedModules|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsTypeAssignments",(arrsTypeAssignments|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsValueAssignments",(arrsValueAssignments|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsPrototypes",(arrsPrototypes|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsUtilityDefines",(arrsUtilityDefines|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bHasEncodings",bHasEncodings :>Object);("bXer",bXer :>Object)]

let Define_TAS (sTypeDefinition:string) (arrsProcs:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_TAS" [("sTypeDefinition",(if sTypeDefinition = null then null else ST.StrHelper sTypeDefinition:>Object) );("arrsProcs",(arrsProcs|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let PrintValueAssignment (sName:string) (sTypeDecl:string) (sValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "PrintValueAssignment" [("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("sTypeDecl",(if sTypeDecl = null then null else ST.StrHelper sTypeDecl:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) )]

let Declare_Integer () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Integer" []

let Declare_PosInteger () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_PosInteger" []

let Declare_IntegerNoRTL () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_IntegerNoRTL" []

let Declare_PosIntegerNoRTL () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_PosIntegerNoRTL" []

let Declare_Boolean () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Boolean" []

let Declare_Real () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Real" []

let Declare_Int8 () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Int8" []

let Declare_UInt8 () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_UInt8" []

let Declare_Int16 () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Int16" []

let Declare_UInt16 () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_UInt16" []

let Declare_Int32 () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Int32" []

let Declare_UInt32 () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_UInt32" []

let Declare_Int64 () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Int64" []

let Declare_UInt64 () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_UInt64" []

let Declare_Real32 () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Real32" []

let Declare_Real64 () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Real64" []

let Declare_BooleanNoRTL () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_BooleanNoRTL" []

let Declare_RealNoRTL () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_RealNoRTL" []

let Declare_Null () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Null" []

let Declare_NullNoRTL () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_NullNoRTL" []

let Declare_ObjectIdentifier () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_ObjectIdentifier" []

let Declare_ObjectIdentifierNoRTL () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_ObjectIdentifierNoRTL" []

let Declare_Asn1LocalTime () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1LocalTime" []

let Declare_Asn1UtcTime () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1UtcTime" []

let Declare_Asn1LocalTimeWithTimeZone () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1LocalTimeWithTimeZone" []

let Declare_Asn1Date () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1Date" []

let Declare_Asn1Date_LocalTime () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1Date_LocalTime" []

let Declare_Asn1Date_UtcTime () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1Date_UtcTime" []

let Declare_Asn1Date_LocalTimeWithTimeZone () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1Date_LocalTimeWithTimeZone" []

let Declare_Asn1LocalTimeNoRTL () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1LocalTimeNoRTL" []

let Declare_Asn1UtcTimeNoRTL () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1UtcTimeNoRTL" []

let Declare_Asn1LocalTimeWithTimeZoneNoRTL () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1LocalTimeWithTimeZoneNoRTL" []

let Declare_Asn1DateNoRTL () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1DateNoRTL" []

let Declare_Asn1Date_LocalTimeNoRTL () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1Date_LocalTimeNoRTL" []

let Declare_Asn1Date_UtcTimeNoRTL () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1Date_UtcTimeNoRTL" []

let Declare_Asn1Date_LocalTimeWithTimeZoneNoRTL () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Declare_Asn1Date_LocalTimeWithTimeZoneNoRTL" []

let Define_SubType (sTypeDefinitionName:string) (soParentTypePackage:string option) (sParentType:string) (soNewRange:string option) (soExtraDefs:string option) (arrsAnnots:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_SubType" [("sTypeDefinitionName",(if sTypeDefinitionName = null then null else ST.StrHelper sTypeDefinitionName:>Object) );("soParentTypePackage",(if soParentTypePackage.IsNone then null else ST.StrHelper soParentTypePackage.Value:>Object) );("sParentType",(if sParentType = null then null else ST.StrHelper sParentType:>Object) );("soNewRange",(if soNewRange.IsNone then null else ST.StrHelper soNewRange.Value:>Object) );("soExtraDefs",(if soExtraDefs.IsNone then null else ST.StrHelper soExtraDefs.Value:>Object) );("arrsAnnots",(arrsAnnots|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_new_enumerated_item (td:FE_EnumeratedTypeDefinition) (sName:string) (nValue:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_enumerated_item" [("td",td :>Object);("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("nValue",nValue :>Object)]

let Define_new_enumerated_item_macro (td:FE_EnumeratedTypeDefinition) (sAsn1Name:string) (sCName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_enumerated_item_macro" [("td",td :>Object);("sAsn1Name",(if sAsn1Name = null then null else ST.StrHelper sAsn1Name:>Object) );("sCName",(if sCName = null then null else ST.StrHelper sCName:>Object) )]

let Define_new_enumerated (td:FE_EnumeratedTypeDefinition) (arrsEnumNames:seq<string>) (arrsEnumNamesAndValues:seq<string>) (nIndexMax:BigInteger) (arrsResolvingMacros:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_enumerated" [("td",td :>Object);("arrsEnumNames",(arrsEnumNames|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsEnumNamesAndValues",(arrsEnumNamesAndValues|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("nIndexMax",nIndexMax :>Object);("arrsResolvingMacros",(arrsResolvingMacros|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_subType_enumerated (td:FE_EnumeratedTypeDefinition) (prTd:FE_EnumeratedTypeDefinition) (soParentTypePackage:string option) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_subType_enumerated" [("td",td :>Object);("prTd",prTd :>Object);("soParentTypePackage",(if soParentTypePackage.IsNone then null else ST.StrHelper soParentTypePackage.Value:>Object) );("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_new_enumerated_private (td:FE_EnumeratedTypeDefinition) (arrsValidEnumNames:seq<string>) (arrsEnumNames:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_enumerated_private" [("td",td :>Object);("arrsValidEnumNames",(arrsValidEnumNames|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsEnumNames",(arrsEnumNames|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let Define_subType_enumerated_private (td:FE_EnumeratedTypeDefinition) (prTd:FE_EnumeratedTypeDefinition) (arrsValidEnumNames:seq<string>) (arrsEnumNames:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_subType_enumerated_private" [("td",td :>Object);("prTd",prTd :>Object);("arrsValidEnumNames",(arrsValidEnumNames|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsEnumNames",(arrsEnumNames|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let Define_new_ia5string (td:FE_StringTypeDefinition) (nMin:BigInteger) (nMax:BigInteger) (nCMax:BigInteger) (arrnAlphaChars:seq<BigInteger>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_ia5string" [("td",td :>Object);("nMin",nMin :>Object);("nMax",nMax :>Object);("nCMax",nCMax :>Object);("arrnAlphaChars",arrnAlphaChars|>Seq.toArray :>Object);("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_subType_ia5string (td:FE_StringTypeDefinition) (prTd:FE_StringTypeDefinition) (soParentTypePackage:string option) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_subType_ia5string" [("td",td :>Object);("prTd",prTd :>Object);("soParentTypePackage",(if soParentTypePackage.IsNone then null else ST.StrHelper soParentTypePackage.Value:>Object) );("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_new_octet_string (td:FE_SizeableTypeDefinition) (nMin:BigInteger) (nMax:BigInteger) (bFixedSize:bool) (arrsInvariants:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_octet_string" [("td",td :>Object);("nMin",nMin :>Object);("nMax",nMax :>Object);("bFixedSize",bFixedSize :>Object);("arrsInvariants",(arrsInvariants|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_subType_octet_string (td:FE_SizeableTypeDefinition) (prTd:FE_SizeableTypeDefinition) (soParentTypePackage:string option) (bFixedSize:bool) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_subType_octet_string" [("td",td :>Object);("prTd",prTd :>Object);("soParentTypePackage",(if soParentTypePackage.IsNone then null else ST.StrHelper soParentTypePackage.Value:>Object) );("bFixedSize",bFixedSize :>Object);("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_new_bit_string_named_bit (td:FE_SizeableTypeDefinition) (sTargetLangBitName:string) (sHexValue:string) (sComment:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_bit_string_named_bit" [("td",td :>Object);("sTargetLangBitName",(if sTargetLangBitName = null then null else ST.StrHelper sTargetLangBitName:>Object) );("sHexValue",(if sHexValue = null then null else ST.StrHelper sHexValue:>Object) );("sComment",(if sComment = null then null else ST.StrHelper sComment:>Object) )]

let Define_new_bit_string (td:FE_SizeableTypeDefinition) (nMin:BigInteger) (nMax:BigInteger) (bFixedSize:bool) (nMaxOctets:BigInteger) (arrsNamedBits:seq<string>) (arrsInvariants:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_bit_string" [("td",td :>Object);("nMin",nMin :>Object);("nMax",nMax :>Object);("bFixedSize",bFixedSize :>Object);("nMaxOctets",nMaxOctets :>Object);("arrsNamedBits",(arrsNamedBits|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsInvariants",(arrsInvariants|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_subType_bit_string (td:FE_SizeableTypeDefinition) (prTd:FE_SizeableTypeDefinition) (soParentTypePackage:string option) (nMin:BigInteger) (nMax:BigInteger) (bFixedSize:bool) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_subType_bit_string" [("td",td :>Object);("prTd",prTd :>Object);("soParentTypePackage",(if soParentTypePackage.IsNone then null else ST.StrHelper soParentTypePackage.Value:>Object) );("nMin",nMin :>Object);("nMax",nMax :>Object);("bFixedSize",bFixedSize :>Object);("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_new_sequence_of (td:FE_SizeableTypeDefinition) (nMin:BigInteger) (nMax:BigInteger) (bFixedSize:bool) (sChildType:string) (soChildDefinition:string option) (arrsSizeClassDefinition:seq<string>) (arrsSizeObjDefinition:seq<string>) (arrsInvariants:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_sequence_of" [("td",td :>Object);("nMin",nMin :>Object);("nMax",nMax :>Object);("bFixedSize",bFixedSize :>Object);("sChildType",(if sChildType = null then null else ST.StrHelper sChildType:>Object) );("soChildDefinition",(if soChildDefinition.IsNone then null else ST.StrHelper soChildDefinition.Value:>Object) );("arrsSizeClassDefinition",(arrsSizeClassDefinition|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsSizeObjDefinition",(arrsSizeObjDefinition|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsInvariants",(arrsInvariants|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_subType_sequence_of (td:FE_SizeableTypeDefinition) (prTd:FE_SizeableTypeDefinition) (soParentTypePackage:string option) (bFixedSize:bool) (soChildDefinition:string option) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_subType_sequence_of" [("td",td :>Object);("prTd",prTd :>Object);("soParentTypePackage",(if soParentTypePackage.IsNone then null else ST.StrHelper soParentTypePackage.Value:>Object) );("bFixedSize",bFixedSize :>Object);("soChildDefinition",(if soChildDefinition.IsNone then null else ST.StrHelper soChildDefinition.Value:>Object) );("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_new_sequence_child_bit (sName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_sequence_child_bit" [("sName",(if sName = null then null else ST.StrHelper sName:>Object) )]

let Define_new_sequence_child (sName:string) (sType:string) (bIsOptional:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_sequence_child" [("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) );("bIsOptional",bIsOptional :>Object)]

let Define_new_sequence_save_pos_child (td:FE_SequenceTypeDefinition) (sName:string) (nMaxBytesInACN:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_sequence_save_pos_child" [("td",td :>Object);("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("nMaxBytesInACN",nMaxBytesInACN :>Object)]

let Define_new_sequence (td:FE_SequenceTypeDefinition) (arrsChildren:seq<string>) (arrsOptionalChildren:seq<string>) (arrsChildrenDefinitions:seq<string>) (arrsNullFieldsSavePos:seq<string>) (arrsSizeDefinition:seq<string>) (arrsInvariants:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_sequence" [("td",td :>Object);("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsOptionalChildren",(arrsOptionalChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsChildrenDefinitions",(arrsChildrenDefinitions|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsNullFieldsSavePos",(arrsNullFieldsSavePos|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsSizeDefinition",(arrsSizeDefinition|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsInvariants",(arrsInvariants|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_subType_sequence (td:FE_SequenceTypeDefinition) (prTd:FE_SequenceTypeDefinition) (soParentTypePackage:string option) (arrsOptionalChildren:seq<string>) (arrsExtraDefs:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_subType_sequence" [("td",td :>Object);("prTd",prTd :>Object);("soParentTypePackage",(if soParentTypePackage.IsNone then null else ST.StrHelper soParentTypePackage.Value:>Object) );("arrsOptionalChildren",(arrsOptionalChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsExtraDefs",(arrsExtraDefs|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_new_choice_child (sName:string) (sType:string) (sPresent:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_choice_child" [("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) );("sPresent",(if sPresent = null then null else ST.StrHelper sPresent:>Object) )]

let Define_new_choice (td:FE_ChoiceTypeDefinition) (sChoiceIDForNone:string) (sFirstChildNamePresent:string) (arrsChildren:seq<string>) (arrsPresent:seq<string>) (arrsCombined:seq<string>) (nIndexMax:BigInteger) (arrsChildrenDefinitions:seq<string>) (arrsSizeDefinition:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_new_choice" [("td",td :>Object);("sChoiceIDForNone",(if sChoiceIDForNone = null then null else ST.StrHelper sChoiceIDForNone:>Object) );("sFirstChildNamePresent",(if sFirstChildNamePresent = null then null else ST.StrHelper sFirstChildNamePresent:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsPresent",(arrsPresent|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsCombined",(arrsCombined|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("nIndexMax",nIndexMax :>Object);("arrsChildrenDefinitions",(arrsChildrenDefinitions|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsSizeDefinition",(arrsSizeDefinition|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_subType_choice (td:FE_ChoiceTypeDefinition) (prTd:FE_ChoiceTypeDefinition) (soParentTypePackage:string option) (arr_Asn1Encoding:seq<Asn1Encoding>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_subType_choice" [("td",td :>Object);("prTd",prTd :>Object);("soParentTypePackage",(if soParentTypePackage.IsNone then null else ST.StrHelper soParentTypePackage.Value:>Object) );("arr_Asn1Encoding",arr_Asn1Encoding|>Seq.toArray :>Object)]

let Define_SubType_int_range (soParentTypePackage:string option) (sParentType:string) (noMin:BigInteger option) (noMax:BigInteger option) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "header_rust" "Define_SubType_int_range" [("soParentTypePackage",(if soParentTypePackage.IsNone then null else ST.StrHelper soParentTypePackage.Value:>Object) );("sParentType",(if sParentType = null then null else ST.StrHelper sParentType:>Object) );("noMin",(if noMin.IsNone then null else noMin.Value:>Object) );("noMax",(if noMax.IsNone then null else noMax.Value:>Object) )]

