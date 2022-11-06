module IXer_a
open System
open System.Numerics
open CommonTypes

type IXer_a() =
    inherit AbstractMacros.IXer()
        override this.rtlModuleName  () =
            xer_a.rtlModuleName  () 
        override this.EmitTypeAssignment_def_err_code  (sErrCode:string) (nErrValue:BigInteger) =
            xer_a.EmitTypeAssignment_def_err_code  sErrCode nErrValue 
        override this.EmitTypeAssignment_def  (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (arrsErrcodes:seq<string>) (bEmptyEncodingSpace:bool) (nMaxBytesInXER:BigInteger) (soSparkAnnotations:string option) (codec:Codec) =
            xer_a.EmitTypeAssignment_def  sVarName sStar sFuncName sTypeDefName arrsErrcodes bEmptyEncodingSpace nMaxBytesInXER soSparkAnnotations codec
        override this.EmitTypeAssignment  (sTasName:string) (sVarName:string) (sStar:string) (sFuncName:string) (soIValidFuncName:string option) (sTypeDefName:string) (arrsLocalVariables:seq<string>) (sContent:string) (soSparkAnnotations:string option) (sInitilialExp:string) (codec:Codec) =
            xer_a.EmitTypeAssignment  sTasName sVarName sStar sFuncName soIValidFuncName sTypeDefName arrsLocalVariables sContent soSparkAnnotations sInitilialExp codec
        override this.Integer  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_a.Integer  p sTag nLevel soCheckExp sErrCode codec
        override this.IntegerPos  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_a.IntegerPos  p sTag nLevel soCheckExp sErrCode codec
        override this.Boolean  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_a.Boolean  p sTag nLevel soCheckExp sErrCode codec
        override this.Real  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_a.Real  p sTag nLevel soCheckExp sErrCode codec
        override this.ObjectIdentifier  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_a.ObjectIdentifier  p sTag nLevel soCheckExp sErrCode codec
        override this.TimeType  (p:string) (sTimeSubType:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_a.TimeType  p sTimeSubType sTag nLevel soCheckExp sErrCode codec
        override this.String  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_a.String  p sTag nLevel soCheckExp sErrCode codec
        override this.Enumerated_item  (p:string) (sTag:string) (nLevel:BigInteger) (sItemID:string) (sXerValue:string) (sErrCode:string) (bFirst:bool) (codec:Codec) =
            xer_a.Enumerated_item  p sTag nLevel sItemID sXerValue sErrCode bFirst codec
        override this.Enumerated  (p:string) (sTag:string) (nLevel:BigInteger) (arrsItems:seq<string>) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_a.Enumerated  p sTag nLevel arrsItems soCheckExp sErrCode codec
        override this.OctetString  (p:string) (sAcc:string) (sTag:string) (nLevel:BigInteger) (nSizeMax:BigInteger) (bIsFixedSize:bool) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_a.OctetString  p sAcc sTag nLevel nSizeMax bIsFixedSize soCheckExp sErrCode codec
        override this.BitString  (p:string) (sAcc:string) (sTag:string) (nLevel:BigInteger) (nSizeMax:BigInteger) (bIsFixedSize:bool) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_a.BitString  p sAcc sTag nLevel nSizeMax bIsFixedSize soCheckExp sErrCode codec
        override this.Null  (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_a.Null  p sTag nLevel soCheckExp sErrCode codec
        override this.SequenceOf  (p:string) (sAcc:string) (sTag:string) (nLevel:BigInteger) (sI:string) (nSizeMax:BigInteger) (sChildBody:string) (bFixedSize:bool) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            xer_a.SequenceOf  p sAcc sTag nLevel sI nSizeMax sChildBody bFixedSize soCheckExp sErrCode codec
        override this.Sequence_mandatory_child  (sChName:string) (sChildContent:string) (sChildTag:string) (codec:Codec) =
            xer_a.Sequence_mandatory_child  sChName sChildContent sChildTag codec
        override this.Sequence_optional_child  (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (sChildTag:string) (codec:Codec) =
            xer_a.Sequence_optional_child  p sAcc sChName sChildContent sChildTag codec
        override this.Sequence_default_child  (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (sChildTag:string) (sInitWithDefaultValue:string) (codec:Codec) =
            xer_a.Sequence_default_child  p sAcc sChName sChildContent sChildTag sInitWithDefaultValue codec
        override this.SEQUENCE_start  (p:string) (sTag:string) (nLevel:BigInteger) (sErrCode:string) (bEmptySequence:bool) (codec:Codec) =
            xer_a.SEQUENCE_start  p sTag nLevel sErrCode bEmptySequence codec
        override this.SEQUENCE_end  (sTag:string) (nLevel:BigInteger) (sErrCode:string) (codec:Codec) =
            xer_a.SEQUENCE_end  sTag nLevel sErrCode codec
        override this.SEQUENCE_xer  (sChildren:string) (codec:Codec) =
            xer_a.SEQUENCE_xer  sChildren codec
        override this.CHOICE_child  (p:string) (sAcc:string) (sChID:string) (sChildBody:string) (bFirst:bool) (sChildTag:string) (sChildName:string) (sChildTypeDef:string) (sChoiceTypeName:string) (codec:Codec) =
            xer_a.CHOICE_child  p sAcc sChID sChildBody bFirst sChildTag sChildName sChildTypeDef sChoiceTypeName codec
        override this.CHOICE_no_tag  (p:string) (sAcc:string) (arrsChildren:seq<string>) (sErrCode:string) (codec:Codec) =
            xer_a.CHOICE_no_tag  p sAcc arrsChildren sErrCode codec
        override this.CHOICE  (p:string) (sAcc:string) (sTag:string) (nLevel:BigInteger) (sMainBody:string) (sErrCode:string) (codec:Codec) =
            xer_a.CHOICE  p sAcc sTag nLevel sMainBody sErrCode codec
        override this.call_base_type_func  (p:string) (sXmlTag:string) (sFuncName:string) (codec:Codec) =
            xer_a.call_base_type_func  p sXmlTag sFuncName codec
