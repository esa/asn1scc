module AbstractMacrosImp
open System.Numerics
open CommonTypes

open AbstractMacros

let abstractMacros_ACN_IntegerMacros =
    {new ACN.IntegerMacros() with
            member this.positiveInteger_ConstSize_8_x (x:ACN.IntegerMacros_positiveInteger_ConstSize_8) =
                acn_c.PositiveInteger_ConstSize_8 x.p x.soMF x.soMFM x.nUperMin x.nUperMax x.codec 
            member this.positiveInteger_ConstSize_big_endian_16 (p:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) codec =
                acn_c.PositiveInteger_ConstSize_big_endian_16 (p:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) codec 
            }


let real_ACN_IntegerMacros =
    {new ACN.RealMacros with
            member this.positiveInteger_ConstSize_8 (p:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (codec:Codec) : string =
                acn_c.PositiveInteger_ConstSize_8 p soMF soMFM nUperMin nUperMax codec 
            member this.positiveInteger_ConstSize_big_endian_16 (p:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) codec =
                acn_c.PositiveInteger_ConstSize_big_endian_16 (p:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) codec 
            }
    


let abstractMacros_uPER_IntegerMacros =
    {new uPER.IntegerMacros with
            member this.positiveInteger_ConstSize_8 (p:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (codec:Codec) : string =
                acn_c.PositiveInteger_ConstSize_8 p soMF soMFM nUperMin nUperMax codec 
            member this.positiveInteger_ConstSize_big_endian_16 (p:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) codec =
                acn_c.PositiveInteger_ConstSize_big_endian_16 (p:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) codec 
            }


let real_uPER_IntegerMacros =
    {new uPER.RealMacros with
            member this.positiveInteger_ConstSize_8 (p:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (codec:Codec) : string =
                acn_c.PositiveInteger_ConstSize_8 p soMF soMFM nUperMin nUperMax codec 
            member this.positiveInteger_ConstSize_big_endian_16 (p:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) codec =
                acn_c.PositiveInteger_ConstSize_big_endian_16 (p:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) codec 
            }



let acnMacros = {ACN.Macros.integerMacros = abstractMacros_ACN_IntegerMacros; ACN.Macros.realMacros = real_ACN_IntegerMacros}

let uPERMacros = {uPER.Macros.integerMacros = abstractMacros_uPER_IntegerMacros; uPER.Macros.realMacros = real_uPER_IntegerMacros}

let c_macros = {LangMacros.acn = acnMacros; uper = uPERMacros}