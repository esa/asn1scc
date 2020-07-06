with Interfaces;
package Board_Config is
   subtype Asn1Int is Interfaces.Integer_32;
   subtype Asn1UInt is Interfaces.Unsigned_32;
   subtype Asn1Real is Standard.Float;
   Enumerated_Size : constant := 16;
end Board_Config;