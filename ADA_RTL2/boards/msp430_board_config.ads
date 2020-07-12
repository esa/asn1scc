with Interfaces; use Interfaces;
package Board_Config is
   subtype Asn1Int is Interfaces.Integer_32;
   subtype Asn1UInt is Interfaces.Unsigned_32;
   subtype Asn1Real is Standard.Float;
   Enumerated_Size : constant := 16;

   type Asn1Int_ARRAY_0_9 is array (0 .. 9) of Asn1UInt;
   Powers_of_10 : constant Asn1Int_ARRAY_0_9 :=
     Asn1Int_ARRAY_0_9'
       (0 => 1, 1 => 10, 2 => 100, 3 => 1000, 4 => 10000, 5 => 100000,
        6 => 1000000, 7 => 10000000, 8 => 100000000, 9 => 1000000000);

   Max_Int_Digits : constant Integer := 10;

   function Get_number_of_digits (Int_value : Asn1UInt) return Integer is
     (if Int_value < Powers_of_10 (1) then 1
      elsif Int_value >= Powers_of_10 (1) and Int_value < Powers_of_10 (2) then
        2
      elsif Int_value >= Powers_of_10 (2) and Int_value < Powers_of_10 (3) then
        3
      elsif Int_value >= Powers_of_10 (3) and Int_value < Powers_of_10 (4) then
        4
      elsif Int_value >= Powers_of_10 (4) and Int_value < Powers_of_10 (5) then
        5
      elsif Int_value >= Powers_of_10 (5) and Int_value < Powers_of_10 (6) then
        6
      elsif Int_value >= Powers_of_10 (6) and Int_value < Powers_of_10 (7) then
        7
      elsif Int_value >= Powers_of_10 (7) and Int_value < Powers_of_10 (8) then
        8
      elsif Int_value >= Powers_of_10 (8) and Int_value < Powers_of_10 (9) then
        9
      else 10);

end Board_Config;
