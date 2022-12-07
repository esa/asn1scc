module IVariables_c
open System
open System.Numerics
open CommonTypes

type IVariables_c() =
    inherit AbstractMacros.IVariables()
        override this.PrintIntValue  (nValue:BigInteger) =
            variables_c.PrintIntValue  nValue 
        override this.PrintRealValue  (dValue:double) =
            variables_c.PrintRealValue  dValue 
        override this.PrintEnumValue  (sValue:string) =
            variables_c.PrintEnumValue  sValue 
        override this.PrintRefValue1  (sValue:string) =
            variables_c.PrintRefValue1  sValue 
        override this.PrintRefValue2  (sModName:string) (sValue:string) =
            variables_c.PrintRefValue2  sModName sValue 
        override this.PrintStringValue  (sValue:string) (arrsNullChars:seq<string>) =
            variables_c.PrintStringValue  sValue arrsNullChars 
        override this.PrintStringValueNull  () =
            variables_c.PrintStringValueNull  () 
        override this.PrintCharValue  (cValue:char) =
            variables_c.PrintCharValue  cValue 
        override this.PrintBooleanValue  (bValue:bool) =
            variables_c.PrintBooleanValue  bValue 
        override this.PrintNullValue  () =
            variables_c.PrintNullValue  () 
        override this.PrintOctetStringValue  (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (arruBytes:seq<byte>) (nCount:BigInteger) =
            variables_c.PrintOctetStringValue  td bIsFixedSize arruBytes nCount 
        override this.PrintBitStringValue  (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (arrsBits:seq<string>) (nCount:BigInteger) (arruBytes:seq<byte>) (nBytesCount:BigInteger) =
            variables_c.PrintBitStringValue  td bIsFixedSize arrsBits nCount arruBytes nBytesCount 
        override this.PrintBitOrOctetStringValueAsCompoundLitteral  (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (arruBytes:seq<byte>) (nCount:BigInteger) =
            variables_c.PrintBitOrOctetStringValueAsCompoundLitteral  td bIsFixedSize arruBytes nCount 
        override this.PrintOctetArrayAsCompoundLitteral  (arruBytes:seq<byte>) =
            variables_c.PrintOctetArrayAsCompoundLitteral  arruBytes 
        override this.PrintBitArrayAsCompoundLitteral  (arruBits:seq<byte>) =
            variables_c.PrintBitArrayAsCompoundLitteral  arruBits 
        override this.PrintObjectIdentifierValue  (td:FE_PrimitiveTypeDefinition) (arrnValues:seq<BigInteger>) (nCount:BigInteger) =
            variables_c.PrintObjectIdentifierValue  td arrnValues nCount 
        override this.PrintObjectIdentifierValueAsCompoundLiteral  (arrnValues:seq<BigInteger>) (nCount:BigInteger) =
            variables_c.PrintObjectIdentifierValueAsCompoundLiteral  arrnValues nCount 
        override this.PrintTimeValueAsCompoundLiteral_Asn1LocalTime  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
            variables_c.PrintTimeValueAsCompoundLiteral_Asn1LocalTime  td tv 
        override this.PrintTimeValueAsCompoundLiteral_Asn1UtcTime  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
            variables_c.PrintTimeValueAsCompoundLiteral_Asn1UtcTime  td tv 
        override this.PrintTimeValueAsCompoundLiteral_Asn1LocalTimeWithTimeZone  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            variables_c.PrintTimeValueAsCompoundLiteral_Asn1LocalTimeWithTimeZone  td tv tz 
        override this.PrintTimeValueAsCompoundLiteral_Asn1Date  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) =
            variables_c.PrintTimeValueAsCompoundLiteral_Asn1Date  td dt 
        override this.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTime  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            variables_c.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTime  td dt tv 
        override this.PrintTimeValueAsCompoundLiteral_Asn1Date_UtcTime  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            variables_c.PrintTimeValueAsCompoundLiteral_Asn1Date_UtcTime  td dt tv 
        override this.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTimeWithTimeZone  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            variables_c.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTimeWithTimeZone  td dt tv tz 
        override this.PrintTimeValue_Asn1LocalTime  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
            variables_c.PrintTimeValue_Asn1LocalTime  td tv 
        override this.PrintTimeValue_Asn1UtcTime  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
            variables_c.PrintTimeValue_Asn1UtcTime  td tv 
        override this.PrintTimeValue_Asn1LocalTimeWithTimeZone  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            variables_c.PrintTimeValue_Asn1LocalTimeWithTimeZone  td tv tz 
        override this.PrintTimeValue_Asn1Date  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) =
            variables_c.PrintTimeValue_Asn1Date  td dt 
        override this.PrintTimeValue_Asn1Date_LocalTime  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            variables_c.PrintTimeValue_Asn1Date_LocalTime  td dt tv 
        override this.PrintTimeValue_Asn1Date_UtcTime  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            variables_c.PrintTimeValue_Asn1Date_UtcTime  td dt tv 
        override this.PrintTimeValue_Asn1Date_LocalTimeWithTimeZone  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            variables_c.PrintTimeValue_Asn1Date_LocalTimeWithTimeZone  td dt tv tz 
        override this.PrintSequenceValueChild  (sName:string) (sInnerValue:string) =
            variables_c.PrintSequenceValueChild  sName sInnerValue 
        override this.PrintSequenceValue_child_exists  (sName:string) (sExistsBit:string) =
            variables_c.PrintSequenceValue_child_exists  sName sExistsBit 
        override this.PrintSequenceValue  (td:FE_SequenceTypeDefinition) (sTasName:string) (arrsChildren:seq<string>) (arrsOptionalPresentFields:seq<string>) =
            variables_c.PrintSequenceValue  td sTasName arrsChildren arrsOptionalPresentFields 
        override this.PrintChoiceValue  (sTasName:string) (sChildName:string) (sChildVal:string) (sChildNamePresent:string) (bUseUncheckedUnions:bool) =
            variables_c.PrintChoiceValue  sTasName sChildName sChildVal sChildNamePresent bUseUncheckedUnions 
        override this.PrintValueAssignment  (sName:string) (sTypeDecl:string) (sValue:string) =
            variables_c.PrintValueAssignment  sName sTypeDecl sValue 
        override this.PrintSequenceOfValue  (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (nLength:BigInteger) (arrsInnerValues:seq<string>) (sDefValue:string) =
            variables_c.PrintSequenceOfValue  td bIsFixedSize nLength arrsInnerValues sDefValue 
