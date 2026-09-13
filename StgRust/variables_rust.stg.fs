module variables_rust
open System
open System.Numerics
open CommonTypes

let PrintIntValue (nValue:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintIntValue" [("nValue",nValue :>Object)]

let PrintRealValue (dValue:double) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintRealValue" [("dValue",dValue :>Object)]

let PrintEnumValue (sValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintEnumValue" [("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) )]

let PrintRefValue1 (sValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintRefValue1" [("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) )]

let PrintRefValue2 (sModName:string) (sValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintRefValue2" [("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) )]

let PrintStringValue (arrsVals:seq<string>) (arrsNullChars:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintStringValue" [("arrsVals",(arrsVals|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsNullChars",(arrsNullChars|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let PrintSingleStringValue (sValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintSingleStringValue" [("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) )]

let PrintLF () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintLF" []

let PrintCR () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintCR" []

let PrintHT () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintHT" []

let PrintStringValueNull () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintStringValueNull" []

let PrintCharValue (cValue:char) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintCharValue" [("cValue",cValue :>Object)]

let PrintStringChar (cValue:char) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintStringChar" [("cValue",cValue :>Object)]

let PrintBooleanValue (bValue:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintBooleanValue" [("bValue",bValue :>Object)]

let PrintNullValue () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintNullValue" []

let PrintOctetStringValue (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (arruBytes:seq<byte>) (nCount:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintOctetStringValue" [("td",td :>Object);("bIsFixedSize",bIsFixedSize :>Object);("arruBytes",arruBytes|>Seq.toArray :>Object);("nCount",nCount :>Object)]

let PrintBitStringValue (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (arrsBits:seq<string>) (nCount:BigInteger) (arruBytes:seq<byte>) (nBytesCount:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintBitStringValue" [("td",td :>Object);("bIsFixedSize",bIsFixedSize :>Object);("arrsBits",(arrsBits|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("nCount",nCount :>Object);("arruBytes",arruBytes|>Seq.toArray :>Object);("nBytesCount",nBytesCount :>Object)]

let PrintBitOrOctetStringValueAsCompoundLiteral (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (arruBytes:seq<byte>) (nCount:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintBitOrOctetStringValueAsCompoundLiteral" [("td",td :>Object);("bIsFixedSize",bIsFixedSize :>Object);("arruBytes",arruBytes|>Seq.toArray :>Object);("nCount",nCount :>Object)]

let PrintOctetArrayAsCompoundLiteral (arruBytes:seq<byte>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintOctetArrayAsCompoundLiteral" [("arruBytes",arruBytes|>Seq.toArray :>Object)]

let PrintBitArrayAsCompoundLiteral (arruBits:seq<byte>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintBitArrayAsCompoundLiteral" [("arruBits",arruBits|>Seq.toArray :>Object)]

let PrintObjectIdentifierValue (td:FE_PrimitiveTypeDefinition) (arrnValues:seq<BigInteger>) (nCount:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintObjectIdentifierValue" [("td",td :>Object);("arrnValues",arrnValues|>Seq.toArray :>Object);("nCount",nCount :>Object)]

let PrintObjectIdentifierValueAsCompoundLiteral (arrnValues:seq<BigInteger>) (nCount:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintObjectIdentifierValueAsCompoundLiteral" [("arrnValues",arrnValues|>Seq.toArray :>Object);("nCount",nCount :>Object)]

let PrintTimeValueAsCompoundLiteral_Asn1LocalTime (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValueAsCompoundLiteral_Asn1LocalTime" [("td",td :>Object);("tv",tv :>Object)]

let PrintTimeValueAsCompoundLiteral_Asn1UtcTime (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValueAsCompoundLiteral_Asn1UtcTime" [("td",td :>Object);("tv",tv :>Object)]

let PrintTimeValueAsCompoundLiteral_Asn1LocalTimeWithTimeZone (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValueAsCompoundLiteral_Asn1LocalTimeWithTimeZone" [("td",td :>Object);("tv",tv :>Object);("tz",tz :>Object)]

let PrintTimeValueAsCompoundLiteral_Asn1Date (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValueAsCompoundLiteral_Asn1Date" [("td",td :>Object);("dt",dt :>Object)]

let PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTime (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTime" [("td",td :>Object);("dt",dt :>Object);("tv",tv :>Object)]

let PrintTimeValueAsCompoundLiteral_Asn1Date_UtcTime (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValueAsCompoundLiteral_Asn1Date_UtcTime" [("td",td :>Object);("dt",dt :>Object);("tv",tv :>Object)]

let PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTimeWithTimeZone (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTimeWithTimeZone" [("td",td :>Object);("dt",dt :>Object);("tv",tv :>Object);("tz",tz :>Object)]

let PrintTimeValue_Asn1LocalTime (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValue_Asn1LocalTime" [("td",td :>Object);("tv",tv :>Object)]

let PrintTimeValue_Asn1UtcTime (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValue_Asn1UtcTime" [("td",td :>Object);("tv",tv :>Object)]

let PrintTimeValue_Asn1LocalTimeWithTimeZone (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValue_Asn1LocalTimeWithTimeZone" [("td",td :>Object);("tv",tv :>Object);("tz",tz :>Object)]

let PrintTimeValue_Asn1Date (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValue_Asn1Date" [("td",td :>Object);("dt",dt :>Object)]

let PrintTimeValue_Asn1Date_LocalTime (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValue_Asn1Date_LocalTime" [("td",td :>Object);("dt",dt :>Object);("tv",tv :>Object)]

let PrintTimeValue_Asn1Date_UtcTime (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValue_Asn1Date_UtcTime" [("td",td :>Object);("dt",dt :>Object);("tv",tv :>Object)]

let PrintTimeValue_Asn1Date_LocalTimeWithTimeZone (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintTimeValue_Asn1Date_LocalTimeWithTimeZone" [("td",td :>Object);("dt",dt :>Object);("tv",tv :>Object);("tz",tz :>Object)]

let PrintSequenceValueChild (sName:string) (sInnerValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintSequenceValueChild" [("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("sInnerValue",(if sInnerValue = null then null else ST.StrHelper sInnerValue:>Object) )]

let PrintSequenceValueOptionalChild (sName:string) (sInnerValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintSequenceValueOptionalChild" [("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("sInnerValue",(if sInnerValue = null then null else ST.StrHelper sInnerValue:>Object) )]

let PrintSequenceValue_child_exists (sName:string) (sExistsBit:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintSequenceValue_child_exists" [("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("sExistsBit",(if sExistsBit = null then null else ST.StrHelper sExistsBit:>Object) )]

let PrintSequenceValue (td:FE_SequenceTypeDefinition) (sTasName:string) (arrsChildren:seq<string>) (arrsOptionalPresentFields:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintSequenceValue" [("td",td :>Object);("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsOptionalPresentFields",(arrsOptionalPresentFields|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let PrintChoiceValue (sTasName:string) (sChildName:string) (sChildVal:string) (sChildNamePresent:string) (bUseUncheckedUnions:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintChoiceValue" [("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildVal",(if sChildVal = null then null else ST.StrHelper sChildVal:>Object) );("sChildNamePresent",(if sChildNamePresent = null then null else ST.StrHelper sChildNamePresent:>Object) );("bUseUncheckedUnions",bUseUncheckedUnions :>Object)]

let PrintValueAssignment (sName:string) (sTypeDecl:string) (sValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintValueAssignment" [("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("sTypeDecl",(if sTypeDecl = null then null else ST.StrHelper sTypeDecl:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) )]

let PrintSequenceOfValue (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (nLength:BigInteger) (arrsInnerValues:seq<string>) (sDefValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "variables_rust" "PrintSequenceOfValue" [("td",td :>Object);("bIsFixedSize",bIsFixedSize :>Object);("nLength",nLength :>Object);("arrsInnerValues",(arrsInnerValues|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sDefValue",(if sDefValue = null then null else ST.StrHelper sDefValue:>Object) )]

