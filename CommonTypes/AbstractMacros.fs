module AbstractMacros
open System
open System.Numerics
open CommonTypes

    module ACN =
        type IntegerMacros_positiveInteger_ConstSize_8 = {
            p:string 
            soMF:string option 
            soMFM:string option 
            nUperMin:BigInteger
            nUperMax:BigInteger
            codec:Codec 
        }

        [<AbstractClass>]
        type IntegerMacros () =
            abstract member positiveInteger_ConstSize_8_x : inp:IntegerMacros_positiveInteger_ConstSize_8 -> string;
            member this.positiveInteger_ConstSize_8 (p:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (codec:Codec) : string =
                this.positiveInteger_ConstSize_8_x ({IntegerMacros_positiveInteger_ConstSize_8.p=p; soMF=soMF; soMFM=soMFM; nUperMin=nUperMin; nUperMax=nUperMax; codec=codec})
            abstract member positiveInteger_ConstSize_big_endian_16 : p:string -> soMF:string option -> soMFM:string option -> nUperMin:BigInteger -> nUperMax:BigInteger -> codec:Codec -> string;

        type RealMacros =
            abstract member positiveInteger_ConstSize_8 : p:string -> soMF:string option -> soMFM:string option -> nUperMin:BigInteger-> nUperMax:BigInteger-> codec:Codec -> string;
            abstract member positiveInteger_ConstSize_big_endian_16 : p:string -> soMF:string option -> soMFM:string option -> nUperMin:BigInteger -> nUperMax:BigInteger -> codec:Codec -> string;

        type Macros = {
            integerMacros   : IntegerMacros
            realMacros      : RealMacros
        }

    module uPER =
        type IntegerMacros =
            abstract member positiveInteger_ConstSize_8 : p:string -> soMF:string option -> soMFM:string option -> nUperMin:BigInteger-> nUperMax:BigInteger-> codec:Codec -> string;
            abstract member positiveInteger_ConstSize_big_endian_16 : p:string -> soMF:string option -> soMFM:string option -> nUperMin:BigInteger -> nUperMax:BigInteger -> codec:Codec -> string;

        type RealMacros =
            abstract member positiveInteger_ConstSize_8 : p:string -> soMF:string option -> soMFM:string option -> nUperMin:BigInteger-> nUperMax:BigInteger-> codec:Codec -> string;
            abstract member positiveInteger_ConstSize_big_endian_16 : p:string -> soMF:string option -> soMFM:string option -> nUperMin:BigInteger -> nUperMax:BigInteger -> codec:Codec -> string;

        type Macros = {
            integerMacros   : IntegerMacros
            realMacros      : RealMacros
        }

type LangMacros = {
    acn     : ACN.Macros
    uper    : uPER.Macros
}