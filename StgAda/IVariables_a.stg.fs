module IVariables_a
open System
open System.Numerics
open CommonTypes

type IVariables_a() =
    inherit AbstractMacros.IVariables()
        override this.PrintIntValue  (nValue:BigInteger) =
            variables_a.PrintIntValue  nValue 
        override this.PrintRealValue  (dValue:double) =
            variables_a.PrintRealValue  dValue 
        override this.PrintStringValue  (sValue:string) (arrsNullChars:seq<string>) =
            variables_a.PrintStringValue  sValue arrsNullChars 
        override this.PrintStringValueNull  () =
            variables_a.PrintStringValueNull  () 
        override this.PrintRefValue1  (sValue:string) =
            variables_a.PrintRefValue1  sValue 
        override this.PrintRefValue2  (sModName:string) (sValue:string) =
            variables_a.PrintRefValue2  sModName sValue 
        override this.PrintEnumValue  (sValue:string) =
            variables_a.PrintEnumValue  sValue 
        override this.PrintCharValue  (cValue:char) =
            variables_a.PrintCharValue  cValue 
        override this.PrintBooleanValue  (bValue:bool) =
            variables_a.PrintBooleanValue  bValue 
        override this.PrintNullValue  () =
            variables_a.PrintNullValue  () 
        override this.PrintOctetStringValue  (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (arruBytes:seq<byte>) (nCount:BigInteger) =
            variables_a.PrintOctetStringValue  td bIsFixedSize arruBytes nCount 
        override this.PrintBitOrOctetStringValueAsCompoundLitteral  (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (arruBytes:seq<byte>) (nCount:BigInteger) =
            variables_a.PrintBitOrOctetStringValueAsCompoundLitteral  td bIsFixedSize arruBytes nCount 
        override this.PrintOctetArrayAsCompoundLitteral  (arruBytes:seq<byte>) =
            variables_a.PrintOctetArrayAsCompoundLitteral  arruBytes 
        override this.PrintBitArrayAsCompoundLitteral  (arruBits:seq<byte>) =
            variables_a.PrintBitArrayAsCompoundLitteral  arruBits 
        override this.PrintBitStringValue  (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (arrsBits:seq<string>) (nCount:BigInteger) (arruBytes:seq<byte>) (nBytesCount:BigInteger) =
            variables_a.PrintBitStringValue  td bIsFixedSize arrsBits nCount arruBytes nBytesCount 
        override this.PrintObjectIdentifierValue  (td:FE_PrimitiveTypeDefinition) (arrnValues:seq<BigInteger>) (nCount:BigInteger) =
            variables_a.PrintObjectIdentifierValue  td arrnValues nCount 
        override this.PrintObjectIdentifierValueAsCompoundLiteral  (arrnValues:seq<BigInteger>) (nCount:BigInteger) =
            variables_a.PrintObjectIdentifierValueAsCompoundLiteral  arrnValues nCount 
        override this.PrintTimeValueAsCompoundLiteral_Asn1LocalTime  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
            variables_a.PrintTimeValueAsCompoundLiteral_Asn1LocalTime  td tv 
        override this.PrintTimeValueAsCompoundLiteral_Asn1UtcTime  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
            variables_a.PrintTimeValueAsCompoundLiteral_Asn1UtcTime  td tv 
        override this.PrintTimeValueAsCompoundLiteral_Asn1LocalTimeWithTimeZone  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            variables_a.PrintTimeValueAsCompoundLiteral_Asn1LocalTimeWithTimeZone  td tv tz 
        override this.PrintTimeValueAsCompoundLiteral_Asn1Date  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) =
            variables_a.PrintTimeValueAsCompoundLiteral_Asn1Date  td dt 
        override this.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTime  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            variables_a.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTime  td dt tv 
        override this.PrintTimeValueAsCompoundLiteral_Asn1Date_UtcTime  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            variables_a.PrintTimeValueAsCompoundLiteral_Asn1Date_UtcTime  td dt tv 
        override this.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTimeWithTimeZone  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            variables_a.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTimeWithTimeZone  td dt tv tz 
        override this.PrintTimeValue_Asn1LocalTime  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
            variables_a.PrintTimeValue_Asn1LocalTime  td tv 
        override this.PrintTimeValue_Asn1UtcTime  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
            variables_a.PrintTimeValue_Asn1UtcTime  td tv 
        override this.PrintTimeValue_Asn1LocalTimeWithTimeZone  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            variables_a.PrintTimeValue_Asn1LocalTimeWithTimeZone  td tv tz 
        override this.PrintTimeValue_Asn1Date  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) =
            variables_a.PrintTimeValue_Asn1Date  td dt 
        override this.PrintTimeValue_Asn1Date_LocalTime  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            variables_a.PrintTimeValue_Asn1Date_LocalTime  td dt tv 
        override this.PrintTimeValue_Asn1Date_UtcTime  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            variables_a.PrintTimeValue_Asn1Date_UtcTime  td dt tv 
        override this.PrintTimeValue_Asn1Date_LocalTimeWithTimeZone  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            variables_a.PrintTimeValue_Asn1Date_LocalTimeWithTimeZone  td dt tv tz 
        override this.PrintSequenceValueChild  (sName:string) (sInnerValue:string) =
            variables_a.PrintSequenceValueChild  sName sInnerValue 
        override this.PrintSequenceValue_child_exists  (sName:string) (sExistsBit:string) =
            variables_a.PrintSequenceValue_child_exists  sName sExistsBit 
        override this.PrintSequenceValue  (td:FE_SequenceTypeDefinition) (sTasName:string) (arrsChildren:seq<string>) (arrsOptionalPresentFields:seq<string>) =
            variables_a.PrintSequenceValue  td sTasName arrsChildren arrsOptionalPresentFields 
        override this.PrintChoiceValue  (sTasName:string) (sChildName:string) (sChildVal:string) (sChildNamePresent:string) =
            variables_a.PrintChoiceValue  sTasName sChildName sChildVal sChildNamePresent 
        override this.PrintSequenceOfValue  (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (nLength:BigInteger) (arrsInnerValues:seq<string>) (sDefValue:string) =
            variables_a.PrintSequenceOfValue  td bIsFixedSize nLength arrsInnerValues sDefValue 
        override this.PrintValueAssignment  (sVasName:string) (sTypeDecl:string) (sValue:string) =
            variables_a.PrintValueAssignment  sVasName sTypeDecl sValue 
