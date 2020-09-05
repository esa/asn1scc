with Interfaces;

with Ada.Text_IO;    use Ada.Text_IO;
with adaasn1rtl;
with TASTE_Dataview; use TASTE_Dataview;
use adaasn1rtl;

with Ada.Sequential_IO;

package body user_code is

   package Seq_IO is new Ada.Sequential_IO (Asn1Byte);

   procedure Write_BitStream_To_File (bs : Bitstream; Filename : String);

   procedure Write_BitStream_To_File (bs : Bitstream; Filename : String)
   is
      Out_File : Seq_IO.File_Type;
   begin
      Seq_IO.Create (Out_File, Seq_IO.Out_File, Filename);
      for i in bs.Buffer'Range loop
         Seq_IO.Write (Out_File, bs.Buffer (i));
      end loop;

      Seq_IO.Close (Out_File);
   end Write_BitStream_To_File;

   procedure initialize (tc_data : in out A1);

   procedure initialize (tc_data : in out A1) is
      i1 : Integer;
   begin
      i1 := 1;
      while i1 <= 1000 loop
         pragma Loop_Invariant (i1 >= 1 and i1 <= 1000);

         tc_data.Data (i1).a11 := Interfaces.Unsigned_64 (i1 * 50);
         --  set a12
         tc_data.Data (i1).a12 := Interfaces.Unsigned_64 (i1 * 50);

         i1 := i1 + 1;
      end loop;

   end initialize;

   procedure encode_no_streaming_mode (val : A1;
                                  Filename : String;
                                   success : out Boolean);

   procedure encode_no_streaming_mode (val : A1;
                                  Filename : String;
                                  success  : out Boolean)
   is
      result : adaasn1rtl.ASN1_RESULT;
      stream : adaasn1rtl.encoding.Bitstream :=
        adaasn1rtl.encoding.BitStream_init
          (TASTE_Dataview.A1_REQUIRED_BYTES_FOR_ENCODING);
   begin
      TASTE_Dataview.A1_Encode (val, stream, result);
      if result.Success then
         Write_BitStream_To_File (stream, Filename);
         success := True;
      else
         Put_Line ("Error: Encoding failed !!!");
         success := False;
      end if;

   end encode_no_streaming_mode;

   procedure read_from_file (bs : in out adaasn1rtl.encoding.Bitstream;
                             ft : in out Seq_IO.File_Type);

   procedure read_from_file (bs : in out adaasn1rtl.encoding.Bitstream;
                             ft : in out Seq_IO.File_Type)
   is
   begin
      for i in bs.Buffer'Range loop
         exit when Seq_IO.End_Of_File (ft);
         Seq_IO.Read (ft, bs.Buffer (i));
      end loop;

   end read_from_file;

   ft : Seq_IO.File_Type;

   pragma Warnings (Off, "formal parameter ""prm"" is not referenced");

   procedure fetch_data (bs : in out Bitstream; prm : Integer) is
   begin
      read_from_file (bs, ft);
   end fetch_data;
   pragma Warnings (On, "formal parameter ""prm"" is not referenced");

   procedure push_data (bs : in out Bitstream; prm : Integer) is
   begin
      null;
   end push_data;

   procedure decode_with_streaming_mode (decodedPDU :    out A1;
      Filename : String; result : out adaasn1rtl.ASN1_RESULT);

   procedure decode_with_streaming_mode (decodedPDU :    out A1;
      Filename : String; result : out adaasn1rtl.ASN1_RESULT)
   is

      stream : adaasn1rtl.encoding.Bitstream :=
        adaasn1rtl.encoding.BitStream_init (1024);

   begin
      stream.fetchDataPrm := 1;
      Seq_IO.Open (ft, Seq_IO.In_File, Filename);
      read_from_file (stream, ft);
--    TASTE_Dataview.A1_Decode(decodedPDU, stream, result);
      TASTE_Dataview.A1_Decode_aux (decodedPDU, stream, result);

      if result.Success then
         Put_Line ("Suscess: Decoding OK !!!");
      else
         Put_Line ("Error: Decoding failed !!!");
      end if;

   end decode_with_streaming_mode;

   procedure streaming_mode_test is
      encodedPDU : A1;
      decodedPDU : A1;
      success    : Boolean;
      result     : adaasn1rtl.ASN1_RESULT;
   begin
      initialize (encodedPDU);
      encode_no_streaming_mode (encodedPDU, "foo3.bin", success);
      if success then

         decode_with_streaming_mode (decodedPDU, "foo3.bin", result);
         if result.Success then
            result := TASTE_Dataview.A1_IsConstraintValid (decodedPDU);
            if result.Success then
               result.Success :=
                 TASTE_Dataview.A1_Equal (encodedPDU, decodedPDU);
               if result.Success then
                  Put_Line ("Suscess !!!");
               else
                  Put_Line ("Error encodedPDU <> decodedPDU");
               end if;
            end if;

         end if;
      end if;

   end streaming_mode_test;

end user_code;
