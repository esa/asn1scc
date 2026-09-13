module IXer_rust
open System
open System.Numerics
open CommonTypes

type IXer_rust() =
    inherit AbstractMacros.IXer()
        override this.rtlModuleName  () =
            xer_rust.rtlModuleName  () 
        override this.EmitTypeAssignment_def_err_code  (sErrCode:string) (nErrValue:BigInteger) (sFieldPath:string) =
            xer_rust.EmitTypeAssignment_def_err_code  sErrCode nErrValue sFieldPath 
        override this.EmitTypeAssignment_def  (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (arrsErrcodes:seq<string>) (bEmptyEncodingSpace:bool) (nMaxBytesInXER:BigInteger) (soSparkAnnotations:string option) (codec:Codec) =
            xer_rust.EmitTypeAssignment_def  sVarName sStar sFuncName sTypeDefName arrsErrcodes bEmptyEncodingSpace nMaxBytesInXER soSparkAnnotations codec
        override this.EmitTypeAssignment  (sTasName:string) (sVarName:string) (sStar:string) (sFuncName:string) (soIValidFuncName:string option) (sTypeDefName:string) (arrsLocalVariables:seq<string>) (sContent:string) (soSparkAnnotations:string option) (sInitialExp:string) (codec:Codec) =
            xer_rust.EmitTypeAssignment  sTasName sVarName sStar sFuncName soIValidFuncName sTypeDefName arrsLocalVariables sContent soSparkAnnotations sInitialExp codec
        override this.Integer  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_rust.Integer  p sTag nLevel soCheckExp sErrCode codec
        override this.IntegerPos  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_rust.IntegerPos  p sTag nLevel soCheckExp sErrCode codec
        override this.Boolean  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_rust.Boolean  p sTag nLevel soCheckExp sErrCode codec
        override this.Real  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_rust.Real  p sTag nLevel soCheckExp sErrCode codec
        override this.ObjectIdentifier  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_rust.ObjectIdentifier  p sTag nLevel soCheckExp sErrCode codec
        override this.TimeType  (p:string) (sTimeSubType:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_rust.TimeType  p sTimeSubType sTag nLevel soCheckExp sErrCode codec
        override this.Null  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_rust.Null  p sTag nLevel soCheckExp sErrCode codec
        override this.String  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (nMaxLength:BigInteger) (codec:Codec) =
            xer_rust.String  p sTag nLevel soCheckExp sErrCode nMaxLength codec
        override this.Enumerated_item  (p:string) (sTag:string) (nLevel:BigInteger) (sItemID:string) (sXerValue:string) (sErrCode:string) (bFirst:bool) (codec:Codec) =
            xer_rust.Enumerated_item  p sTag nLevel sItemID sXerValue sErrCode bFirst codec
        override this.Enumerated  (p:string) (sTag:string) (nLevel:BigInteger) (arrsItems:seq<string>) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_rust.Enumerated  p sTag nLevel arrsItems soCheckExp sErrCode codec
        override this.OctetString  (p:string) (sAcc:string) (sTag:string) (nLevel:BigInteger) (nSizeMax:BigInteger) (bIsFixedSize:bool) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_rust.OctetString  p sAcc sTag nLevel nSizeMax bIsFixedSize soCheckExp sErrCode codec
        override this.BitString  (p:string) (sAcc:string) (sTag:string) (nLevel:BigInteger) (nSizeMax:BigInteger) (bIsFixedSize:bool) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_rust.BitString  p sAcc sTag nLevel nSizeMax bIsFixedSize soCheckExp sErrCode codec
        override this.SequenceOf  (p:string) (sAcc:string) (sTag:string) (nLevel:BigInteger) (sI:string) (nSizeMax:BigInteger) (sChildBody:string) (bFixedSize:bool) (soCheckExp:string option) (sErrCode:string) (sElemTypeDef:string) (codec:Codec) =
            xer_rust.SequenceOf  p sAcc sTag nLevel sI nSizeMax sChildBody bFixedSize soCheckExp sErrCode sElemTypeDef codec
        override this.Sequence_mandatory_child  (sChName:string) (sChildContent:string) (sChildTag:string) (sChildTypeDef:string) (codec:Codec) =
            xer_rust.Sequence_mandatory_child  sChName sChildContent sChildTag sChildTypeDef codec
        override this.Sequence_optional_child  (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (sChildTag:string) (sChildTypeDef:string) (codec:Codec) =
            xer_rust.Sequence_optional_child  p sAcc sChName sChildContent sChildTag sChildTypeDef codec
        override this.Sequence_default_child  (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (sChildTag:string) (sInitWithDefaultValue:string) (sChildTypeDef:string) (codec:Codec) =
            xer_rust.Sequence_default_child  p sAcc sChName sChildContent sChildTag sInitWithDefaultValue sChildTypeDef codec
        override this.SEQUENCE_start  (p:string) (sTag:string) (nLevel:BigInteger) (sErrCode:string) (bEmptySequence:bool) (codec:Codec) =
            xer_rust.SEQUENCE_start  p sTag nLevel sErrCode bEmptySequence codec
        override this.SEQUENCE_end  (sTag:string) (nLevel:BigInteger) (sErrCode:string) (codec:Codec) =
            xer_rust.SEQUENCE_end  sTag nLevel sErrCode codec
        override this.SEQUENCE_xer  (sChildren:string) (codec:Codec) =
            xer_rust.SEQUENCE_xer  sChildren codec
        override this.CHOICE_child  (p:string) (sAcc:string) (sChID:string) (sChildBody:string) (bFirst:bool) (sChildTag:string) (sChildName:string) (sChildTypeDef:string) (sChoiceTypeName:string) (codec:Codec) =
            xer_rust.CHOICE_child  p sAcc sChID sChildBody bFirst sChildTag sChildName sChildTypeDef sChoiceTypeName codec
        override this.CHOICE_no_tag  (p:string) (sAcc:string) (arrsChildren:seq<string>) (sErrCode:string) (codec:Codec) =
            xer_rust.CHOICE_no_tag  p sAcc arrsChildren sErrCode codec
        override this.CHOICE  (p:string) (sAcc:string) (sTag:string) (nLevel:BigInteger) (sMainBody:string) (sErrCode:string) (codec:Codec) =
            xer_rust.CHOICE  p sAcc sTag nLevel sMainBody sErrCode codec
        override this.call_base_type_func  (p:string) (soXmlTag:string option) (sFuncName:string) (sBaseTypeName:string) (codec:Codec) =
            xer_rust.call_base_type_func  p soXmlTag sFuncName sBaseTypeName codec
