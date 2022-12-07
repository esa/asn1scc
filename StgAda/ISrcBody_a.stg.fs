module ISrcBody_a
open System
open System.Numerics
open CommonTypes

type ISrcBody_a() =
    inherit AbstractMacros.ISrcBody()
        override this.rtlModuleName  () =
            body_a.rtlModuleName  () 
        override this.printSourceFile  (sPackageName:string) (arrsIncludedModules:seq<string>) (arrsAdaUseTypes:seq<string>) (arrsValueAndTypeAssignments:seq<string>) =
            body_a.printSourceFile  sPackageName arrsIncludedModules arrsAdaUseTypes arrsValueAndTypeAssignments 
        override this.printTass  (arrsAllProcs:seq<string>) =
            body_a.printTass  arrsAllProcs 
