module ISrcBody_c
open System
open System.Numerics
open CommonTypes

type ISrcBody_c() =
    inherit AbstractMacros.ISrcBody()
        override this.rtlModuleName  () =
            body_c.rtlModuleName  () 
        override this.printSourceFile  (sFileNameWithoutExtension:string) (arrsIncludedFiles:seq<string>) (arrsAdaUseTypes:seq<string>) (arrsValueAndTypeAssignments:seq<string>) =
            body_c.printSourceFile  sFileNameWithoutExtension arrsIncludedFiles arrsAdaUseTypes arrsValueAndTypeAssignments 
        override this.printTass  (arrsAllProcs:seq<string>) =
            body_c.printTass  arrsAllProcs 
