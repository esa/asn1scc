with Interfaces;

WITH Ada.Text_IO;
use Ada.Text_IO;
WITH adaasn1rtl;
with TASTE_Dataview;
use TASTE_Dataview;
use adaasn1rtl;

with Ada.Sequential_IO;

package body user_code is

   package Seq_IO is new Ada.Sequential_IO(Asn1Byte);

   procedure Write_BitStream_To_File (bs : in BitStream; Filename : in String) is
      Out_File : Seq_IO.File_Type;
      --Current_Bit  : constant BIT_RANGE := bs.Current_Bit_Pos mod 8;
      --End_Byte : constant Integer   := bs.Buffer'First + bs.Current_Bit_Pos / 8 + (if Current_Bit > 0 then 1 else 0);
   begin
      Seq_IO.Create(Out_File, Seq_IO.Out_File, Filename);
      for i in  bs.Buffer'Range loop
         Seq_IO.Write(Out_File, bs.Buffer(i));
      end loop;


      Seq_IO.Close(Out_File);
   end;






procedure initialize(tc_data : in out A1) is
i1 : integer;
begin
    i1 := 1;
    while i1<= 1000 loop
        pragma Loop_Invariant (i1 >=1 and i1<=1000);

        tc_data.Data(i1).a11 := Interfaces.Unsigned_64(i1*50);
            --set a12
        tc_data.Data(i1).a12 := Interfaces.Unsigned_64(i1*50);

        i1 := i1 + 1;
    end loop;

end;


procedure encode_no_streaming_mode(val : in  A1; Filename : in String; success: out Boolean) is
    result      : adaasn1rtl.ASN1_RESULT;
    stream      : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init(TASTE_Dataview.A1_REQUIRED_BYTES_FOR_ENCODING);
begin
    TASTE_Dataview.A1_Encode(val, stream, result);
    if result.Success then
      Write_BitStream_To_File(stream, Filename);
      success := true;
    else
       Put_Line ("Error: Encoding failed !!!");
      success := false;
    end if;

end;


procedure read_from_file(bs : in out adaasn1rtl.encoding.Bitstream; ft : in out Seq_IO.File_Type) is
begin
      for i in  bs.Buffer'Range loop
         exit when Seq_IO.End_Of_File(ft);
         Seq_IO.Read(ft, bs.Buffer(i));

      end loop;

end;

ft : Seq_IO.File_Type;

pragma Warnings (Off, "formal parameter ""prm"" is not referenced");

procedure fetch_data(bs : in out BitStream; prm : Integer)
is
begin
    read_from_file(bs,ft);
end;
pragma Warnings (On, "formal parameter ""prm"" is not referenced");


procedure push_data(bs : in out BitStream; prm : Integer)
is
begin
    null;
end;


procedure decode_with_streaming_mode(decodedPDU : out  A1; Filename : in String; result      : out adaasn1rtl.ASN1_RESULT) is

    stream      : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init(1024);

begin
    Seq_IO.Open(ft,Seq_IO.In_File,Filename);
    read_from_file(stream,ft);
--    TASTE_Dataview.A1_Decode(decodedPDU, stream, result);
    TASTE_Dataview.A1_Decode_aux(decodedPDU, Stream, result);

    if result.Success then
      Put_Line ("Suscess: Decoding OK !!!");
    else
      Put_Line ("Error: Decoding failed !!!");
    end if;

end;


procedure streaming_mode_test
is
 encodedPDU :  A1;
 decodedPDU :  A1;
 success : boolean;
 result      : adaasn1rtl.ASN1_RESULT;
begin
   initialize(encodedPDU);
   encode_no_streaming_mode(encodedPDU, "foo3.bin", success);
   if success then

       decode_with_streaming_mode(decodedPDU, "foo3.bin", result);
       if result.Success then
            result := TASTE_Dataview.A1_IsConstraintValid(decodedPDU);
            if result.Success then
                   result.Success := TASTE_Dataview.A1_Equal(encodedPDU, decodedPDU);
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
