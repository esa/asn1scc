module DAstTypeDefinition2

open System
open System.Numerics
open FsUtils
open Asn1AcnAstUtilFunctions
open Asn1AcnAst
open CommonTypes
open DAst
open DAstUtilFunctions

(*
type TypeDefintionOrReference =
    /// indicates that no extra type definition is required (e.g. INTEGER without constraints or type reference type without new constraints)
    | ReferenceToExistingDefinition    of ReferenceToExistingDefinition                
    /// indicates that a new type is defined
    | TypeDefinition                of TypeDefinition       


	examples
	  A ::= INTEGER				-> {TypeDefinition.typedefName = "A"; typedefBody="typedef A Asn1SInt"}
	  
	  C ::= SEQUENCE  {			-> {TypeDefinition.typedefName = "C"; typedefBody="typedef struct{ ... }"}
			a1   INTEGER		-> {ReferenceToExistingDefinition.programUnit="adaasn1rtl";  typedefName="Asn1Int"} 
			a2	 A				-> {ReferenceToExistingDefinition.programUnit=None;  typedefName="A"} //program unit is none since the type being referenced is in the same module as the referenced type
			a3	 A(1..20)		-> {TypeDefinition.typedefName = "C_a3"; typedefBody = "SUBTYPE C_a3 is A range 0..15"}
		}
		
		

                            Type Assignment ?
                            /               \
                          No                Yes --> New type definition with typedefname = Type Assignement Name
                           |                        
                 (=> Composite Type Child)
                           |
                           |
                                 (Is Primitive types with base definition in RTL  ?)
                               /                                                     \
                              Yes (Int,Real,Bool, Null)                               No (Octet, bit, IA5String, Enumerated, Sequence, Sequence Of, choice)
                               |                                                      |
                        (has extra range?)                                          (Is reference Type with no extra constraint)
                  /                             \                                    /                  \ 
                 /                               \                                  /                    \
                Yes                              No                                 Yes                   No
                 |                                |                                  |                     |
             New Subtype                      Reference to                          Reference to             New Type
            Definition with                   Existing                               Existing              Definition with 
           typedef name =                     Definition                             Definition                name = 
parent type typedefName + child Name         (The existing definition                                       parent type typedefName + child Name
and with base type the referneced type       may be the base
or  base type in RTL                         definition in RTL or
                                             the base type if this is
                                             a referenced type)  

*)



//let getPotentialTypedefName (r:AstRoot) (t:Asn1Type)  (potentialTypedefName:string)   =
//    t.newTypeDefName        
    


       
    

/// Called before visiting a choice or sequence or sequence of children

