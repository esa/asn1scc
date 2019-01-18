 -- ok
 Asn1f2 -uPER -c -o out1 D1.asn D2.asn
 
 -- the following fails as it should
Asn1f2 -uPER -c -o out2 D1_SE.asn D2.asn
File:D2.asn, line:5, No type assignment with name T-UInt16 exists (or exported) in module MOD1
