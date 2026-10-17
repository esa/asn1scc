with Ada.Sequential_IO;
with Ada.Text_IO;
with Ada.Exceptions;

package body adaasn1rtl.encoding.test_cases_aux is

   package Seq_IO is new Ada.Sequential_IO (Asn1Byte);

   procedure Write_BitStream_To_File (bs : Bitstream; Filename : String) is
      Out_File : Seq_IO.File_Type;
      nFileLen : constant Integer :=
        bs.Current_Bit_Pos / 8 +
          (if bs.Current_Bit_Pos mod 8 > 0 then 1 else 0)
      ;

   begin
      Seq_IO.Create (Out_File, Seq_IO.Out_File, Filename);
      for i in bs.Buffer'First .. nFileLen loop
         Seq_IO.Write (Out_File, bs.Buffer (i));
      end loop;

      Seq_IO.Close (Out_File);
   end Write_BitStream_To_File;

   procedure Decode_Corruption_Sweep
     (bs     : in out Bitstream;
      Name   : String;
      Budget : Long_Long_Integer;
      Decode : not null access procedure
        (bs    : in out Bitstream;
         Res   : out ASN1_RESULT;
         Valid : out Boolean);
      Result : out ASN1_RESULT)
   is
      nBytes    : constant Natural := (bs.Current_Bit_Pos + 7) / 8;
      Saved_Pos : constant Natural := bs.Current_Bit_Pos;
      type Pattern_Array is array (1 .. 2) of Asn1Byte;
      Patterns  : constant Pattern_Array := (16#00#, 16#FF#);
      Last_Head : Natural := 0;

      procedure Report (i : Positive; Pattern : Asn1Byte; What : String);
      procedure Try (i : Positive);

      procedure Report (i : Positive; Pattern : Asn1Byte; What : String) is
      begin
         Ada.Text_IO.Put_Line
           (Name & ": decoding with byte" & Integer'Image (i) & " of"
            & Integer'Image (nBytes) & " set to"
            & Asn1Byte'Image (Pattern) & " " & What);
         Result := (Success => False, ErrorCode => 7);
      end Report;

      procedure Try (i : Positive) is
         Saved : constant Asn1Byte := bs.Buffer (i);
         Res   : ASN1_RESULT;
         Valid : Boolean;
      begin
         for Pattern of Patterns loop
            exit when not Result.Success;
            if Saved /= Pattern then
               bs.Buffer (i) := Pattern;
               begin
                  Decode (bs, Res, Valid);
                  if Res.Success and then not Valid then
                     Report (i, Pattern, "returned an invalid value");
                  elsif not Res.Success and then Res.ErrorCode = 0 then
                     Report (i, Pattern, "failed without an error code");
                  end if;
               exception
                  when E : others =>
                     Report (i, Pattern, "raised "
                             & Ada.Exceptions.Exception_Name (E));
               end;
               bs.Buffer (i) := Saved;
            end if;
         end loop;
      end Try;

   begin
      Result := (Success => True, ErrorCode => 0);
      for Side in 1 .. 2 loop
         declare
            Count : Natural := 0;
            Spent : Long_Long_Integer := 0;
            i     : Natural := (if Side = 1 then 1 else nBytes);
         begin
            while Result.Success
              and then (if Side = 1 then i <= nBytes else i > Last_Head)
              and then Count < Sweep_Positions
              and then (Count < 2 or else Spent < Budget / 2)
            loop
               Try (i);
               Count := Count + 1;
               Spent := Spent + Long_Long_Integer (nBytes);
               if Side = 1 then
                  Last_Head := i;
                  i := i + 1;
               else
                  i := i - 1;
               end if;
            end loop;
         end;
      end loop;
      bs.Current_Bit_Pos := Saved_Pos;
   end Decode_Corruption_Sweep;

end adaasn1rtl.encoding.test_cases_aux;
