module ISrcBody_rust
open System
open System.Numerics
open CommonTypes

type ISrcBody_rust() =
    inherit AbstractMacros.ISrcBody()
        override this.rtlModuleName  () =
            body_rust.rtlModuleName  () 
        override this.printSourceFile  (sFileNameWithoutExtension:string) (arrsIncludedFiles:seq<string>) (arrsAdaUseTypes:seq<string>) (arrsUserDefinedFunctions:seq<string>) (arrsValueAndTypeAssignments:seq<string>) =
            body_rust.printSourceFile  sFileNameWithoutExtension arrsIncludedFiles arrsAdaUseTypes arrsUserDefinedFunctions arrsValueAndTypeAssignments 
        override this.printTass  (arrsAllProcs:seq<string>) =
            body_rust.printTass  arrsAllProcs 
