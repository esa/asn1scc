with Interfaces; use Interfaces;
package Board_Config is
   subtype Asn1Int is Interfaces.Integer_64;
   subtype Asn1UInt is Interfaces.Unsigned_64;
   subtype Asn1Real is Standard.Float;
   Enumerated_Size : constant := 8;

   type Asn1Int_ARRAY_0_19 is array (0 .. 19) of Asn1UInt;
   Powers_of_10 : constant Asn1Int_ARRAY_0_19 :=
     Asn1Int_ARRAY_0_19'
       (0  => 1, 1 => 10, 2 => 100, 3 => 1000, 4 => 10000, 5 => 100000,
        6  => 1000000, 7 => 10000000, 8 => 100000000, 9 => 1000000000,
        10 => 10000000000, 11 => 100000000000, 12 => 1000000000000,
        13 => 10000000000000, 14 => 100000000000000, 15 => 1000000000000000,
        16 => 10000000000000000, 17 => 100000000000000000,
        18 => 1000000000000000000, 19 => 10000000000000000000);

   Max_Int_Digits : constant Integer := 20;

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
      elsif Int_value >= Powers_of_10 (9) and Int_value < Powers_of_10 (10)
      then 10
      elsif Int_value >= Powers_of_10 (10) and Int_value < Powers_of_10 (11)
      then 11
      elsif Int_value >= Powers_of_10 (11) and Int_value < Powers_of_10 (12)
      then 12
      elsif Int_value >= Powers_of_10 (12) and Int_value < Powers_of_10 (13)
      then 13
      elsif Int_value >= Powers_of_10 (13) and Int_value < Powers_of_10 (14)
      then 14
      elsif Int_value >= Powers_of_10 (14) and Int_value < Powers_of_10 (15)
      then 15
      elsif Int_value >= Powers_of_10 (15) and Int_value < Powers_of_10 (16)
      then 16
      elsif Int_value >= Powers_of_10 (16) and Int_value < Powers_of_10 (17)
      then 17
      elsif Int_value >= Powers_of_10 (17) and Int_value < Powers_of_10 (18)
      then 18
      elsif Int_value >= Powers_of_10 (18) and Int_value < Powers_of_10 (19)
      then 19
      else 20);

end Board_Config;
