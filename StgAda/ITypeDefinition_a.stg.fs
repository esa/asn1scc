module ITypeDefinition_a
open System
open System.Numerics
open CommonTypes

type ITypeDefinition_a() =
    inherit AbstractMacros.ITypeDefinition()
        override this.rtlModuleName  () =
            header_a.rtlModuleName  () 
        override this.indentation  (sStatement:string) =
            header_a.indentation  sStatement 
        override this.PrintSpecificationFile  (sFileNameWithNoExtUpperCase:string) (sPackageName:string) (arrsIncludedModules:seq<string>) (arrsTypeAssignments:seq<string>) (arrsValueAssignments:seq<string>) (arrsPrototypes:seq<string>) (arrsUtilityDefines:seq<string>) (bHasEncodings:bool) (bXer:bool) =
            header_a.PrintSpecificationFile  sFileNameWithNoExtUpperCase sPackageName arrsIncludedModules arrsTypeAssignments arrsValueAssignments arrsPrototypes arrsUtilityDefines bHasEncodings bXer 
        override this.Define_TAS  (sTypeDefinition:string) (arrsProcs:seq<string>) =
            header_a.Define_TAS  sTypeDefinition arrsProcs 
        override this.PrintValueAssignment  (sVasName:string) (sTypeDecl:string) (sValue:string) =
            header_a.PrintValueAssignment  sVasName sTypeDecl sValue 
        override this.Declare_IntegerNoRTL  () =
            header_a.Declare_IntegerNoRTL  () 
        override this.Declare_PosIntegerNoRTL  () =
            header_a.Declare_PosIntegerNoRTL  () 
        override this.Declare_Integer  () =
            header_a.Declare_Integer  () 
        override this.Declare_PosInteger  () =
            header_a.Declare_PosInteger  () 
        override this.Declare_Int8  () =
            header_a.Declare_Int8  () 
        override this.Declare_UInt8  () =
            header_a.Declare_UInt8  () 
        override this.Declare_Int16  () =
            header_a.Declare_Int16  () 
        override this.Declare_UInt16  () =
            header_a.Declare_UInt16  () 
        override this.Declare_Int32  () =
            header_a.Declare_Int32  () 
        override this.Declare_UInt32  () =
            header_a.Declare_UInt32  () 
        override this.Declare_Int64  () =
            header_a.Declare_Int64  () 
        override this.Declare_UInt64  () =
            header_a.Declare_UInt64  () 
        override this.Declare_Real32  () =
            header_a.Declare_Real32  () 
        override this.Declare_Real64  () =
            header_a.Declare_Real64  () 
        override this.Declare_Boolean  () =
            header_a.Declare_Boolean  () 
        override this.Declare_Real  () =
            header_a.Declare_Real  () 
        override this.Declare_Null  () =
            header_a.Declare_Null  () 
        override this.Declare_BooleanNoRTL  () =
            header_a.Declare_BooleanNoRTL  () 
        override this.Declare_RealNoRTL  () =
            header_a.Declare_RealNoRTL  () 
        override this.Declare_ObjectIdentifierNoRTL  () =
            header_a.Declare_ObjectIdentifierNoRTL  () 
        override this.Declare_ObjectIdentifier  () =
            header_a.Declare_ObjectIdentifier  () 
        override this.Declare_NullNoRTL  () =
            header_a.Declare_NullNoRTL  () 
        override this.Declare_Asn1LocalTime  () =
            header_a.Declare_Asn1LocalTime  () 
        override this.Declare_Asn1UtcTime  () =
            header_a.Declare_Asn1UtcTime  () 
        override this.Declare_Asn1LocalTimeWithTimeZone  () =
            header_a.Declare_Asn1LocalTimeWithTimeZone  () 
        override this.Declare_Asn1Date  () =
            header_a.Declare_Asn1Date  () 
        override this.Declare_Asn1Date_LocalTime  () =
            header_a.Declare_Asn1Date_LocalTime  () 
        override this.Declare_Asn1Date_UtcTime  () =
            header_a.Declare_Asn1Date_UtcTime  () 
        override this.Declare_Asn1Date_LocalTimeWithTimeZone  () =
            header_a.Declare_Asn1Date_LocalTimeWithTimeZone  () 
        override this.Declare_Asn1LocalTimeNoRTL  () =
            header_a.Declare_Asn1LocalTimeNoRTL  () 
        override this.Declare_Asn1UtcTimeNoRTL  () =
            header_a.Declare_Asn1UtcTimeNoRTL  () 
        override this.Declare_Asn1LocalTimeWithTimeZoneNoRTL  () =
            header_a.Declare_Asn1LocalTimeWithTimeZoneNoRTL  () 
        override this.Declare_Asn1DateNoRTL  () =
            header_a.Declare_Asn1DateNoRTL  () 
        override this.Declare_Asn1Date_LocalTimeNoRTL  () =
            header_a.Declare_Asn1Date_LocalTimeNoRTL  () 
        override this.Declare_Asn1Date_UtcTimeNoRTL  () =
            header_a.Declare_Asn1Date_UtcTimeNoRTL  () 
        override this.Declare_Asn1Date_LocalTimeWithTimeZoneNoRTL  () =
            header_a.Declare_Asn1Date_LocalTimeWithTimeZoneNoRTL  () 
        override this.Define_SubType_int_range  (soParentTypePackage:string option) (sParentType:string) (noMin:BigInteger option) (noMax:BigInteger option) =
            header_a.Define_SubType_int_range  soParentTypePackage sParentType noMin noMax 
        override this.Define_SubType  (sTypeDefinitionName:string) (soParentTypePackage:string option) (sParentType:string) (soNewRange:string option) (soExtraDefs:string option) =
            header_a.Define_SubType  sTypeDefinitionName soParentTypePackage sParentType soNewRange soExtraDefs 
        override this.Define_new_enumerated_item  (sName:string) (nValue:BigInteger) =
            header_a.Define_new_enumerated_item  sName nValue 
        override this.Define_new_enumerated_item_macro  (td:FE_EnumeratedTypeDefinition) (sAsn1Name:string) (sCName:string) =
            header_a.Define_new_enumerated_item_macro  td sAsn1Name sCName 
        override this.Define_new_enumerated  (td:FE_EnumeratedTypeDefinition) (arrsEnumNames:seq<string>) (arrsEnumNamesAndValues:seq<string>) (nIndexMax:BigInteger) (arrsResolvingMacros:seq<string>) =
            header_a.Define_new_enumerated  td arrsEnumNames arrsEnumNamesAndValues nIndexMax arrsResolvingMacros 
        override this.Define_subType_enumerated  (td:FE_EnumeratedTypeDefinition) (prTd:FE_EnumeratedTypeDefinition) (soParentTypePackage:string option) =
            header_a.Define_subType_enumerated  td prTd soParentTypePackage 
        override this.Define_new_ia5string  (td:FE_StringTypeDefinition) (nMin:BigInteger) (nMax:BigInteger) (nCMax:BigInteger) (arrnAlphaChars:seq<BigInteger>) =
            header_a.Define_new_ia5string  td nMin nMax nCMax arrnAlphaChars 
        override this.Define_subType_ia5string  (td:FE_StringTypeDefinition) (prTd:FE_StringTypeDefinition) (soParentTypePackage:string option) =
            header_a.Define_subType_ia5string  td prTd soParentTypePackage 
        override this.Define_new_octet_string  (td:FE_SizeableTypeDefinition) (nMin:BigInteger) (nMax:BigInteger) (bFixedSize:bool) =
            header_a.Define_new_octet_string  td nMin nMax bFixedSize 
        override this.Define_subType_octet_string  (td:FE_SizeableTypeDefinition) (prTd:FE_SizeableTypeDefinition) (soParentTypePackage:string option) (bFixedSize:bool) =
            header_a.Define_subType_octet_string  td prTd soParentTypePackage bFixedSize 
        override this.Define_new_bit_string_named_bit  (td:FE_SizeableTypeDefinition) (sTargetLangBitName:string) (sHexValue:string) (sComment:string) =
            header_a.Define_new_bit_string_named_bit  td sTargetLangBitName sHexValue sComment 
        override this.Define_new_bit_string  (td:FE_SizeableTypeDefinition) (nMin:BigInteger) (nMax:BigInteger) (bFixedSize:bool) (nMaxOctets:BigInteger) (arrsNamedBits:seq<string>) =
            header_a.Define_new_bit_string  td nMin nMax bFixedSize nMaxOctets arrsNamedBits 
        override this.Define_subType_bit_string  (td:FE_SizeableTypeDefinition) (prTd:FE_SizeableTypeDefinition) (soParentTypePackage:string option) (bFixedSize:bool) =
            header_a.Define_subType_bit_string  td prTd soParentTypePackage bFixedSize 
        override this.Define_new_sequence_of  (td:FE_SizeableTypeDefinition) (nMin:BigInteger) (nMax:BigInteger) (bFixedSize:bool) (sChildType:string) (soChildDefintion:string option) =
            header_a.Define_new_sequence_of  td nMin nMax bFixedSize sChildType soChildDefintion 
        override this.Define_subType_sequence_of  (td:FE_SizeableTypeDefinition) (prTd:FE_SizeableTypeDefinition) (soParentTypePackage:string option) (bFixedSize:bool) (soChildDefintion:string option) =
            header_a.Define_subType_sequence_of  td prTd soParentTypePackage bFixedSize soChildDefintion 
        override this.Define_new_sequence_child_bit  (sName:string) =
            header_a.Define_new_sequence_child_bit  sName 
        override this.Define_new_sequence_child  (sName:string) (sType:string) =
            header_a.Define_new_sequence_child  sName sType 
        override this.Define_new_sequence_save_pos_child  (td:FE_SequenceTypeDefinition) (sName:string) (nMaxBytesInACN:BigInteger) =
            header_a.Define_new_sequence_save_pos_child  td sName nMaxBytesInACN 
        override this.Define_new_sequence  (td:FE_SequenceTypeDefinition) (arrsChildren:seq<string>) (arrsOptionalChildren:seq<string>) (arrsChildldrenDefintions:seq<string>) (arrsNullFieldsSavePos:seq<string>) =
            header_a.Define_new_sequence  td arrsChildren arrsOptionalChildren arrsChildldrenDefintions arrsNullFieldsSavePos 
        override this.Define_subType_sequence  (td:FE_SequenceTypeDefinition) (prTd:FE_SequenceTypeDefinition) (soParentTypePackage:string option) (arrsOptionalChildren:seq<string>) =
            header_a.Define_subType_sequence  td prTd soParentTypePackage arrsOptionalChildren 
        override this.Define_new_choice_child  (sName:string) (sType:string) (sPresent:string) =
            header_a.Define_new_choice_child  sName sType sPresent 
        override this.Define_new_choice  (td:FE_ChoiceTypeDefinition) (sChoiceIDForNone:string) (sFirstChildNamePresent:string) (arrsChildren:seq<string>) (arrsPresent:seq<string>) (nIndexMax:BigInteger) (arrsChildldrenDefintions:seq<string>) =
            header_a.Define_new_choice  td sChoiceIDForNone sFirstChildNamePresent arrsChildren arrsPresent nIndexMax arrsChildldrenDefintions 
        override this.Define_subType_choice  (td:FE_ChoiceTypeDefinition) (prTd:FE_ChoiceTypeDefinition) (soParentTypePackage:string option) =
            header_a.Define_subType_choice  td prTd soParentTypePackage 
