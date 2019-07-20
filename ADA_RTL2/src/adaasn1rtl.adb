package body  adaasn1rtl with Spark_Mode is

   function getStringSize (str : String) return Integer is
      length : Integer :=0;
   begin
      for i in str'Range loop
         pragma Loop_Invariant (length = length'Loop_Entry + (i - str'First));
         exit when str (I) = Standard.ASCII.NUL;
         length := length + 1;
      end loop;
      
      return length;
   end getStringSize;
   
   
   
   
   function GetZeroBasedCharIndex (CharToSearch   :    Character;  AllowedCharSet : in String) return Integer
   is
      ret : Integer;
   begin
      ret := 0;
      for I in Integer range AllowedCharSet'Range loop
         ret := I - AllowedCharSet'First;
         pragma Loop_Invariant  ( 
            AllowedCharSet'Last >= AllowedCharSet'First and
            AllowedCharSet'Last <= Integer'Last - 1 and
            ret = I - AllowedCharSet'First);
         exit when CharToSearch = AllowedCharSet (I);
      end loop;
      return ret;
   end GetZeroBasedCharIndex;

   function CharacterPos (C : Character) return Integer is
      ret : Integer;
   begin
      ret := Character'Pos (C);
      if not (ret >= 0 and ret <= 127) then
         ret := 0;
      end if;
      return ret;
    end CharacterPos;
   
   
   
end adaasn1rtl;
