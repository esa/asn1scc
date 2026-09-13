module ITypeDefinition_rust
open System
open System.Numerics
open CommonTypes

type ITypeDefinition_rust() =
    inherit AbstractMacros.ITypeDefinition()
        override this.rtlModuleName  () =
            header_rust.rtlModuleName  () 
        override this.indentation  (sStatement:string) =
            header_rust.indentation  sStatement 
        override this.PrintSpecificationFile  (sFileNameWithNoExtUpperCase:string) (sPackageName:string) (arrsIncludedModules:seq<string>) (arrsTypeAssignments:seq<string>) (arrsValueAssignments:seq<string>) (arrsPrototypes:seq<string>) (arrsUtilityDefines:seq<string>) (bHasEncodings:bool) (bXer:bool) =
            header_rust.PrintSpecificationFile  sFileNameWithNoExtUpperCase sPackageName arrsIncludedModules arrsTypeAssignments arrsValueAssignments arrsPrototypes arrsUtilityDefines bHasEncodings bXer 
        override this.Define_TAS  (sTypeDefinition:string) (arrsProcs:seq<string>) =
            header_rust.Define_TAS  sTypeDefinition arrsProcs 
        override this.PrintValueAssignment  (sName:string) (sTypeDecl:string) (sValue:string) =
            header_rust.PrintValueAssignment  sName sTypeDecl sValue 
        override this.Declare_Integer  () =
            header_rust.Declare_Integer  () 
        override this.Declare_PosInteger  () =
            header_rust.Declare_PosInteger  () 
        override this.Declare_IntegerNoRTL  () =
            header_rust.Declare_IntegerNoRTL  () 
        override this.Declare_PosIntegerNoRTL  () =
            header_rust.Declare_PosIntegerNoRTL  () 
        override this.Declare_Boolean  () =
            header_rust.Declare_Boolean  () 
        override this.Declare_Real  () =
            header_rust.Declare_Real  () 
        override this.Declare_Int8  () =
            header_rust.Declare_Int8  () 
        override this.Declare_UInt8  () =
            header_rust.Declare_UInt8  () 
        override this.Declare_Int16  () =
            header_rust.Declare_Int16  () 
        override this.Declare_UInt16  () =
            header_rust.Declare_UInt16  () 
        override this.Declare_Int32  () =
            header_rust.Declare_Int32  () 
        override this.Declare_UInt32  () =
            header_rust.Declare_UInt32  () 
        override this.Declare_Int64  () =
            header_rust.Declare_Int64  () 
        override this.Declare_UInt64  () =
            header_rust.Declare_UInt64  () 
        override this.Declare_Real32  () =
            header_rust.Declare_Real32  () 
        override this.Declare_Real64  () =
            header_rust.Declare_Real64  () 
        override this.Declare_BooleanNoRTL  () =
            header_rust.Declare_BooleanNoRTL  () 
        override this.Declare_RealNoRTL  () =
            header_rust.Declare_RealNoRTL  () 
        override this.Declare_Null  () =
            header_rust.Declare_Null  () 
        override this.Declare_NullNoRTL  () =
            header_rust.Declare_NullNoRTL  () 
        override this.Declare_ObjectIdentifier  () =
            header_rust.Declare_ObjectIdentifier  () 
        override this.Declare_ObjectIdentifierNoRTL  () =
            header_rust.Declare_ObjectIdentifierNoRTL  () 
        override this.Declare_Asn1LocalTime  () =
            header_rust.Declare_Asn1LocalTime  () 
        override this.Declare_Asn1UtcTime  () =
            header_rust.Declare_Asn1UtcTime  () 
        override this.Declare_Asn1LocalTimeWithTimeZone  () =
            header_rust.Declare_Asn1LocalTimeWithTimeZone  () 
        override this.Declare_Asn1Date  () =
            header_rust.Declare_Asn1Date  () 
        override this.Declare_Asn1Date_LocalTime  () =
            header_rust.Declare_Asn1Date_LocalTime  () 
        override this.Declare_Asn1Date_UtcTime  () =
            header_rust.Declare_Asn1Date_UtcTime  () 
        override this.Declare_Asn1Date_LocalTimeWithTimeZone  () =
            header_rust.Declare_Asn1Date_LocalTimeWithTimeZone  () 
        override this.Declare_Asn1LocalTimeNoRTL  () =
            header_rust.Declare_Asn1LocalTimeNoRTL  () 
        override this.Declare_Asn1UtcTimeNoRTL  () =
            header_rust.Declare_Asn1UtcTimeNoRTL  () 
        override this.Declare_Asn1LocalTimeWithTimeZoneNoRTL  () =
            header_rust.Declare_Asn1LocalTimeWithTimeZoneNoRTL  () 
        override this.Declare_Asn1DateNoRTL  () =
            header_rust.Declare_Asn1DateNoRTL  () 
        override this.Declare_Asn1Date_LocalTimeNoRTL  () =
            header_rust.Declare_Asn1Date_LocalTimeNoRTL  () 
        override this.Declare_Asn1Date_UtcTimeNoRTL  () =
            header_rust.Declare_Asn1Date_UtcTimeNoRTL  () 
        override this.Declare_Asn1Date_LocalTimeWithTimeZoneNoRTL  () =
            header_rust.Declare_Asn1Date_LocalTimeWithTimeZoneNoRTL  () 
        override this.Define_SubType  (sTypeDefinitionName:string) (soParentTypePackage:string option) (sParentType:string) (soNewRange:string option) (soExtraDefs:string option) (arrsAnnots:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_SubType  sTypeDefinitionName soParentTypePackage sParentType soNewRange soExtraDefs arrsAnnots arr_Asn1Encoding 
        override this.Define_new_enumerated_item  (td:FE_EnumeratedTypeDefinition) (sName:string) (nValue:BigInteger) =
            header_rust.Define_new_enumerated_item  td sName nValue 
        override this.Define_new_enumerated_item_macro  (td:FE_EnumeratedTypeDefinition) (sAsn1Name:string) (sCName:string) =
            header_rust.Define_new_enumerated_item_macro  td sAsn1Name sCName 
        override this.Define_new_enumerated  (td:FE_EnumeratedTypeDefinition) (arrsEnumNames:seq<string>) (arrsEnumNamesAndValues:seq<string>) (nIndexMax:BigInteger) (arrsResolvingMacros:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_new_enumerated  td arrsEnumNames arrsEnumNamesAndValues nIndexMax arrsResolvingMacros arr_Asn1Encoding 
        override this.Define_subType_enumerated  (td:FE_EnumeratedTypeDefinition) (prTd:FE_EnumeratedTypeDefinition) (soParentTypePackage:string option) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_subType_enumerated  td prTd soParentTypePackage arr_Asn1Encoding 
        override this.Define_new_enumerated_private  (td:FE_EnumeratedTypeDefinition) (arrsValidEnumNames:seq<string>) (arrsEnumNames:seq<string>) =
            header_rust.Define_new_enumerated_private  td arrsValidEnumNames arrsEnumNames 
        override this.Define_subType_enumerated_private  (td:FE_EnumeratedTypeDefinition) (prTd:FE_EnumeratedTypeDefinition) (arrsValidEnumNames:seq<string>) (arrsEnumNames:seq<string>) =
            header_rust.Define_subType_enumerated_private  td prTd arrsValidEnumNames arrsEnumNames 
        override this.Define_new_ia5string  (td:FE_StringTypeDefinition) (nMin:BigInteger) (nMax:BigInteger) (nCMax:BigInteger) (arrnAlphaChars:seq<BigInteger>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_new_ia5string  td nMin nMax nCMax arrnAlphaChars arr_Asn1Encoding 
        override this.Define_subType_ia5string  (td:FE_StringTypeDefinition) (prTd:FE_StringTypeDefinition) (soParentTypePackage:string option) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_subType_ia5string  td prTd soParentTypePackage arr_Asn1Encoding 
        override this.Define_new_octet_string  (td:FE_SizeableTypeDefinition) (nMin:BigInteger) (nMax:BigInteger) (bFixedSize:bool) (arrsInvariants:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_new_octet_string  td nMin nMax bFixedSize arrsInvariants arr_Asn1Encoding 
        override this.Define_subType_octet_string  (td:FE_SizeableTypeDefinition) (prTd:FE_SizeableTypeDefinition) (soParentTypePackage:string option) (bFixedSize:bool) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_subType_octet_string  td prTd soParentTypePackage bFixedSize arr_Asn1Encoding 
        override this.Define_new_bit_string_named_bit  (td:FE_SizeableTypeDefinition) (sTargetLangBitName:string) (sHexValue:string) (sComment:string) =
            header_rust.Define_new_bit_string_named_bit  td sTargetLangBitName sHexValue sComment 
        override this.Define_new_bit_string  (td:FE_SizeableTypeDefinition) (nMin:BigInteger) (nMax:BigInteger) (bFixedSize:bool) (nMaxOctets:BigInteger) (arrsNamedBits:seq<string>) (arrsInvariants:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_new_bit_string  td nMin nMax bFixedSize nMaxOctets arrsNamedBits arrsInvariants arr_Asn1Encoding 
        override this.Define_subType_bit_string  (td:FE_SizeableTypeDefinition) (prTd:FE_SizeableTypeDefinition) (soParentTypePackage:string option) (nMin:BigInteger) (nMax:BigInteger) (bFixedSize:bool) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_subType_bit_string  td prTd soParentTypePackage nMin nMax bFixedSize arr_Asn1Encoding 
        override this.Define_new_sequence_of  (td:FE_SizeableTypeDefinition) (nMin:BigInteger) (nMax:BigInteger) (bFixedSize:bool) (sChildType:string) (soChildDefinition:string option) (arrsSizeClassDefinition:seq<string>) (arrsSizeObjDefinition:seq<string>) (arrsInvariants:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_new_sequence_of  td nMin nMax bFixedSize sChildType soChildDefinition arrsSizeClassDefinition arrsSizeObjDefinition arrsInvariants arr_Asn1Encoding 
        override this.Define_subType_sequence_of  (td:FE_SizeableTypeDefinition) (prTd:FE_SizeableTypeDefinition) (soParentTypePackage:string option) (bFixedSize:bool) (soChildDefinition:string option) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_subType_sequence_of  td prTd soParentTypePackage bFixedSize soChildDefinition arr_Asn1Encoding 
        override this.Define_new_sequence_child_bit  (sName:string) =
            header_rust.Define_new_sequence_child_bit  sName 
        override this.Define_new_sequence_child  (sName:string) (sType:string) (bIsOptional:bool) =
            header_rust.Define_new_sequence_child  sName sType bIsOptional 
        override this.Define_new_sequence_save_pos_child  (td:FE_SequenceTypeDefinition) (sName:string) (nMaxBytesInACN:BigInteger) =
            header_rust.Define_new_sequence_save_pos_child  td sName nMaxBytesInACN 
        override this.Define_new_sequence  (td:FE_SequenceTypeDefinition) (arrsChildren:seq<string>) (arrsOptionalChildren:seq<string>) (arrsChildrenDefinitions:seq<string>) (arrsNullFieldsSavePos:seq<string>) (arrsSizeDefinition:seq<string>) (arrsInvariants:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_new_sequence  td arrsChildren arrsOptionalChildren arrsChildrenDefinitions arrsNullFieldsSavePos arrsSizeDefinition arrsInvariants arr_Asn1Encoding 
        override this.Define_subType_sequence  (td:FE_SequenceTypeDefinition) (prTd:FE_SequenceTypeDefinition) (soParentTypePackage:string option) (arrsOptionalChildren:seq<string>) (arrsExtraDefs:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_subType_sequence  td prTd soParentTypePackage arrsOptionalChildren arrsExtraDefs arr_Asn1Encoding 
        override this.Define_new_choice_child  (sName:string) (sType:string) (sPresent:string) =
            header_rust.Define_new_choice_child  sName sType sPresent 
        override this.Define_new_choice  (td:FE_ChoiceTypeDefinition) (sChoiceIDForNone:string) (sFirstChildNamePresent:string) (arrsChildren:seq<string>) (arrsPresent:seq<string>) (arrsCombined:seq<string>) (nIndexMax:BigInteger) (arrsChildrenDefinitions:seq<string>) (arrsSizeDefinition:seq<string>) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_new_choice  td sChoiceIDForNone sFirstChildNamePresent arrsChildren arrsPresent arrsCombined nIndexMax arrsChildrenDefinitions arrsSizeDefinition arr_Asn1Encoding 
        override this.Define_subType_choice  (td:FE_ChoiceTypeDefinition) (prTd:FE_ChoiceTypeDefinition) (soParentTypePackage:string option) (arr_Asn1Encoding:seq<Asn1Encoding>) =
            header_rust.Define_subType_choice  td prTd soParentTypePackage arr_Asn1Encoding 
        override this.Define_SubType_int_range  (soParentTypePackage:string option) (sParentType:string) (noMin:BigInteger option) (noMax:BigInteger option) =
            header_rust.Define_SubType_int_range  soParentTypePackage sParentType noMin noMax 
