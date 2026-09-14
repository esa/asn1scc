module IVariables_rust
open System
open System.Numerics
open CommonTypes

type IVariables_rust() =
    inherit AbstractMacros.IVariables()
        override this.PrintIntValue  (nValue:BigInteger) =
            variables_rust.PrintIntValue  nValue 
        override this.PrintRealValue  (dValue:double) =
            variables_rust.PrintRealValue  dValue 
        override this.PrintEnumValue  (sValue:string) =
            variables_rust.PrintEnumValue  sValue 
        override this.PrintRefValue1  (sValue:string) =
            variables_rust.PrintRefValue1  sValue 
        override this.PrintRefValue2  (sModName:string) (sValue:string) =
            variables_rust.PrintRefValue2  sModName sValue 
        override this.PrintStringValue  (arrsVals:seq<string>) (arrsNullChars:seq<string>) =
            variables_rust.PrintStringValue  arrsVals arrsNullChars 
        override this.PrintSingleStringValue  (sValue:string) =
            variables_rust.PrintSingleStringValue  sValue 
        override this.PrintLF  () =
            variables_rust.PrintLF  () 
        override this.PrintCR  () =
            variables_rust.PrintCR  () 
        override this.PrintHT  () =
            variables_rust.PrintHT  () 
        override this.PrintStringValueNull  () =
            variables_rust.PrintStringValueNull  () 
        override this.PrintCharValue  (cValue:char) =
            variables_rust.PrintCharValue  cValue 
        override this.PrintStringChar  (sValue:string) =
            variables_rust.PrintStringChar  sValue 
        override this.PrintBooleanValue  (bValue:bool) =
            variables_rust.PrintBooleanValue  bValue 
        override this.PrintNullValue  () =
            variables_rust.PrintNullValue  () 
        override this.PrintOctetStringValue  (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (arruBytes:seq<byte>) (nCount:BigInteger) =
            variables_rust.PrintOctetStringValue  td bIsFixedSize arruBytes nCount 
        override this.PrintBitStringValue  (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (arrsBits:seq<string>) (nCount:BigInteger) (arruBytes:seq<byte>) (nBytesCount:BigInteger) =
            variables_rust.PrintBitStringValue  td bIsFixedSize arrsBits nCount arruBytes nBytesCount 
        override this.PrintBitOrOctetStringValueAsCompoundLiteral  (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (arruBytes:seq<byte>) (nCount:BigInteger) =
            variables_rust.PrintBitOrOctetStringValueAsCompoundLiteral  td bIsFixedSize arruBytes nCount 
        override this.PrintOctetArrayAsCompoundLiteral  (arruBytes:seq<byte>) =
            variables_rust.PrintOctetArrayAsCompoundLiteral  arruBytes 
        override this.PrintBitArrayAsCompoundLiteral  (arruBits:seq<byte>) =
            variables_rust.PrintBitArrayAsCompoundLiteral  arruBits 
        override this.PrintObjectIdentifierValue  (td:FE_PrimitiveTypeDefinition) (arrnValues:seq<BigInteger>) (nCount:BigInteger) =
            variables_rust.PrintObjectIdentifierValue  td arrnValues nCount 
        override this.PrintObjectIdentifierValueAsCompoundLiteral  (arrnValues:seq<BigInteger>) (nCount:BigInteger) =
            variables_rust.PrintObjectIdentifierValueAsCompoundLiteral  arrnValues nCount 
        override this.PrintTimeValueAsCompoundLiteral_Asn1LocalTime  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
            variables_rust.PrintTimeValueAsCompoundLiteral_Asn1LocalTime  td tv 
        override this.PrintTimeValueAsCompoundLiteral_Asn1UtcTime  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
            variables_rust.PrintTimeValueAsCompoundLiteral_Asn1UtcTime  td tv 
        override this.PrintTimeValueAsCompoundLiteral_Asn1LocalTimeWithTimeZone  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            variables_rust.PrintTimeValueAsCompoundLiteral_Asn1LocalTimeWithTimeZone  td tv tz 
        override this.PrintTimeValueAsCompoundLiteral_Asn1Date  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) =
            variables_rust.PrintTimeValueAsCompoundLiteral_Asn1Date  td dt 
        override this.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTime  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            variables_rust.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTime  td dt tv 
        override this.PrintTimeValueAsCompoundLiteral_Asn1Date_UtcTime  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            variables_rust.PrintTimeValueAsCompoundLiteral_Asn1Date_UtcTime  td dt tv 
        override this.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTimeWithTimeZone  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            variables_rust.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTimeWithTimeZone  td dt tv tz 
        override this.PrintTimeValue_Asn1LocalTime  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
            variables_rust.PrintTimeValue_Asn1LocalTime  td tv 
        override this.PrintTimeValue_Asn1UtcTime  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) =
            variables_rust.PrintTimeValue_Asn1UtcTime  td tv 
        override this.PrintTimeValue_Asn1LocalTimeWithTimeZone  (td:FE_PrimitiveTypeDefinition) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            variables_rust.PrintTimeValue_Asn1LocalTimeWithTimeZone  td tv tz 
        override this.PrintTimeValue_Asn1Date  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) =
            variables_rust.PrintTimeValue_Asn1Date  td dt 
        override this.PrintTimeValue_Asn1Date_LocalTime  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            variables_rust.PrintTimeValue_Asn1Date_LocalTime  td dt tv 
        override this.PrintTimeValue_Asn1Date_UtcTime  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            variables_rust.PrintTimeValue_Asn1Date_UtcTime  td dt tv 
        override this.PrintTimeValue_Asn1Date_LocalTimeWithTimeZone  (td:FE_PrimitiveTypeDefinition) (dt:Asn1DateValue) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            variables_rust.PrintTimeValue_Asn1Date_LocalTimeWithTimeZone  td dt tv tz 
        override this.PrintSequenceValueChild  (sName:string) (sInnerValue:string) =
            variables_rust.PrintSequenceValueChild  sName sInnerValue 
        override this.PrintSequenceValueOptionalChild  (sName:string) (sInnerValue:string) =
            variables_rust.PrintSequenceValueOptionalChild  sName sInnerValue 
        override this.PrintSequenceValue_child_exists  (sName:string) (sExistsBit:string) =
            variables_rust.PrintSequenceValue_child_exists  sName sExistsBit 
        override this.PrintSequenceValue  (td:FE_SequenceTypeDefinition) (sTasName:string) (arrsChildren:seq<string>) (arrsOptionalPresentFields:seq<string>) =
            variables_rust.PrintSequenceValue  td sTasName arrsChildren arrsOptionalPresentFields 
        override this.PrintChoiceValue  (sTasName:string) (sChildName:string) (sChildVal:string) (sChildNamePresent:string) (bUseUncheckedUnions:bool) =
            variables_rust.PrintChoiceValue  sTasName sChildName sChildVal sChildNamePresent bUseUncheckedUnions 
        override this.PrintValueAssignment  (sName:string) (sTypeDecl:string) (sValue:string) =
            variables_rust.PrintValueAssignment  sName sTypeDecl sValue 
        override this.PrintSequenceOfValue  (td:FE_SizeableTypeDefinition) (bIsFixedSize:bool) (nLength:BigInteger) (arrsInnerValues:seq<string>) (sDefValue:string) =
            variables_rust.PrintSequenceOfValue  td bIsFixedSize nLength arrsInnerValues sDefValue 
