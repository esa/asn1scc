
with Ada.Sequential_IO;

package body test_cases_aux is

   package Seq_IO is new Ada.Sequential_IO(Asn1Byte);
   
   procedure Write_BitStream_To_File (bs : in BitStream; Filename : in String) is
      Out_File : Seq_IO.File_Type;
      Current_Bit  : constant BIT_RANGE := bs.Current_Bit_Pos mod 8;
      End_Byte : constant Integer   := bs.Buffer'First + bs.Current_Bit_Pos / 8 + (if Current_Bit > 0 then 1 else 0);
   begin
      Seq_IO.Create(Out_File, Seq_IO.Out_File, Filename);
      for i in  bs.Buffer'First .. End_Byte loop
         Seq_IO.Write(Out_File, bs.Buffer(i));
      end loop;
      

      Seq_IO.Close(Out_File);
   end;
   

end test_cases_aux;
