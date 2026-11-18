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
         Saved   : constant Asn1Byte := bs.Buffer (i);
         Res     : ASN1_RESULT;
         Valid   : Boolean;
         Pattern : Asn1Byte;
      begin
         --  16#00#, 16#FF#, then the byte with each of its 8 bits flipped
         for k in 1 .. 10 loop
            exit when not Result.Success;
            Pattern :=
              (case k is
                  when 1      => 16#00#,
                  when 2      => 16#FF#,
                  when others => Saved xor Interfaces.Shift_Left (1, k - 3));
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

   procedure Decode_View_End_Sweep
     (bs     : in out Bitstream;
      Name   : String;
      Decode : not null access procedure
        (bs    : in out Bitstream;
         Res   : out ASN1_RESULT;
         Valid : out Boolean);
      Result : out ASN1_RESULT)
   is
      Saved_End : constant Natural := bs.End_Bit_Pos;
      Saved_Pos : constant Natural := bs.Current_Bit_Pos;
      Res       : ASN1_RESULT;
      Valid     : Boolean;
   begin
      Result := (Success => True, ErrorCode => 0);
      for k in 1 .. Natural'Min (8, Saved_End) loop
         exit when not Result.Success;
         bs.End_Bit_Pos := Saved_End - k;
         begin
            Decode (bs, Res, Valid);
            if Res.Success and then not Valid then
               Ada.Text_IO.Put_Line
                 (Name & ": decoding with the view" & Integer'Image (k)
                  & " bits shorter returned an invalid value");
               Result := (Success => False, ErrorCode => 7);
            elsif not Res.Success and then Res.ErrorCode = 0 then
               Ada.Text_IO.Put_Line
                 (Name & ": decoding with the view" & Integer'Image (k)
                  & " bits shorter failed without an error code");
               Result := (Success => False, ErrorCode => 7);
            end if;
         exception
            when E : others =>
               Ada.Text_IO.Put_Line
                 (Name & ": decoding with the view" & Integer'Image (k)
                  & " bits shorter raised "
                  & Ada.Exceptions.Exception_Name (E));
               Result := (Success => False, ErrorCode => 7);
         end;
      end loop;
      bs.End_Bit_Pos := Saved_End;
      bs.Current_Bit_Pos := Saved_Pos;
   end Decode_View_End_Sweep;

   procedure Offset_Round_Trips
     (Name          : String;
      Size_In_Bytes : Positive;
      Last_Offset   : Positive;
      Deduced       : Boolean;
      Encode : not null access procedure
        (bs  : in out Bitstream;
         Res : out ASN1_RESULT);
      Decode : not null access procedure
        (bs  : in out Bitstream;
         Res : out ASN1_RESULT);
      Valid  : not null access function return Boolean;
      Same   : not null access function return Boolean;
      Result : out TEST_CASE_RESULT)
   is
      type Phase is (Encoding, Decoding, Validating, Comparing);
      Now     : Phase;
      Res     : ASN1_RESULT;
      Enc_End : Natural;

      procedure Fail
        (s : Positive; Step : TEST_CASE_STEP; Code : Integer; What : String);

      procedure Fail
        (s : Positive; Step : TEST_CASE_STEP; Code : Integer; What : String)
      is
      begin
         Ada.Text_IO.Put_Line
           (Name & ": round trip from bit offset" & Integer'Image (s)
            & ": " & What);
         Result := (Step => Step, Success => False, ErrorCode => Code);
      end Fail;

   begin
      Result := (Step => TC_DECODE_CORRUPTED, Success => True, ErrorCode => 0);
      --  BitStream_init and the _aux preconditions need
      --  Size_In_Bytes < Positive'Last / 8 (else Size_In_Bytes * 8
      --  overflows): a larger stream cannot be made, the value is not swept
      --  (a type that large exists in no test; this is not a failure)
      if Size_In_Bytes >= Positive'Last / 8 then
         return;
      end if;
      for s in 1 .. Last_Offset loop
         exit when not Result.Success;
         --  the handler also covers the allocation of the stream
         Now := Encoding;
         begin
            declare
               bs : Bitstream := BitStream_init (Size_In_Bytes);
            begin
               bs.Current_Bit_Pos := s;
               Encode (bs, Res);
               if not Res.Success then
                  Fail (s, TC_ENCODE, Res.ErrorCode,
                        "encoding failed with error code"
                        & Integer'Image (Res.ErrorCode));
               else
                  Enc_End := bs.Current_Bit_Pos;
                  if Deduced then
                     bs.End_Bit_Pos := ((Enc_End + 7) / 8) * 8;
                  end if;
                  bs.Current_Bit_Pos := s;
                  Now := Decoding;
                  Decode (bs, Res);
                  if not Res.Success then
                     Fail (s, TC_DECODE, Res.ErrorCode,
                           "decoding failed with error code"
                           & Integer'Image (Res.ErrorCode));
                  elsif bs.Current_Bit_Pos /= Enc_End then
                     Fail (s, TC_DECODE, 8,
                           "the decoder stopped at bit"
                           & Integer'Image (bs.Current_Bit_Pos)
                           & ", the encoder at bit"
                           & Integer'Image (Enc_End));
                  else
                     Now := Validating;
                     if not Valid.all then
                        Fail (s, TC_VALIDATE_DECODED, 8,
                              "the decoded value is invalid");
                     else
                        Now := Comparing;
                        if not Same.all then
                           Fail (s, TC_EQUAL, 8,
                                 "the decoded value differs from the "
                                 & "encoded");
                        end if;
                     end if;
                  end if;
               end if;
            end;
         exception
            when E : others =>
               Fail (s,
                     (case Now is
                         when Encoding   => TC_ENCODE,
                         when Decoding   => TC_DECODE,
                         when Validating => TC_VALIDATE_DECODED,
                         when Comparing  => TC_EQUAL),
                     8,
                     (case Now is
                         when Encoding   => "stream allocation or encoding",
                         when Decoding   => "decoding",
                         when Validating => "validating",
                         when Comparing  => "comparing")
                     & " raised " & Ada.Exceptions.Exception_Name (E));
         end;
      end loop;
   end Offset_Round_Trips;

end adaasn1rtl.encoding.test_cases_aux;
