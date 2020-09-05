with Ada.Sequential_IO;

package body adaasn1rtl.encoding.test_cases_aux is

   package Seq_IO is new Ada.Sequential_IO (Asn1Byte);

   procedure Write_BitStream_To_File (bs : Bitstream; Filename : String) is
      Out_File : Seq_IO.File_Type;
   begin
      Seq_IO.Create (Out_File, Seq_IO.Out_File, Filename);
      for i in bs.Buffer'Range loop
         Seq_IO.Write (Out_File, bs.Buffer (i));
      end loop;

      Seq_IO.Close (Out_File);
   end Write_BitStream_To_File;

end adaasn1rtl.encoding.test_cases_aux;
