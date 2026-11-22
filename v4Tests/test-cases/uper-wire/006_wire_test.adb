--  uPER and ACN checks of 006.asn1 in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, without and
--  with -gnata; Issues 155 / 156): the rows of 006_wire_test.c, whose C results are the reference. Each row gives the
--  stream as hex tokens ("<hex>*n" = the token n times) and, for a valid stream, the arcs ("A9" = 2^62, "A10" = 2^63,
--  "M" = 2^64 - 1, "*n" = n times) and the tail of S. Every row is decoded by the uPER and by the ACN decoder from a
--  buffer of max (stream, REQUIRED_BYTES) octets (zero padded): a rejected stream must fail with the type's error
--  code (S: the one of its component o) and stop at the given bit position; a valid one must decode to the arcs, stop
--  at the end of the stream, and the arcs must encode to the same bytes (byte-exact).
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with uper_wire_oid; use uper_wire_oid;
procedure Wire_Test is
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_64;
   Failed : Boolean := False;

   type Outcome is (Rejected, Accepted);

   procedure Fail (Name, Codec, What : String; Got, Expected : Integer) is
   begin
      Ada.Text_IO.Put_Line
        ("FAILED " & Name & " (" & Codec & "): " & What & " (got" & Got'Image & ", expected" & Expected'Image & ")");
      Failed := True;
   end Fail;

   function Hv (C : Character) return Natural is
     (if C in '0' .. '9' then Character'Pos (C) - 48
      elsif C in 'a' .. 'f' then Character'Pos (C) - 87 else Character'Pos (C) - 55);

   --  the next token of Spec from Pos on: its text (without "*n") and n
   procedure Next_Token (Spec : String; Pos : in out Natural; First, Last : out Natural; Rep : out Natural) is
      Star : Natural := 0;
   begin
      while Pos <= Spec'Last and then Spec (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;
      First := Pos;
      while Pos <= Spec'Last and then Spec (Pos) /= ' ' loop
         if Spec (Pos) = '*' then
            Star := Pos;
         end if;
         Pos := Pos + 1;
      end loop;
      if Star = 0 then
         Last := Pos - 1;
         Rep := 1;
      else
         Last := Star - 1;
         Rep := Natural'Value (Spec (Star + 1 .. Pos - 1));
      end if;
   end Next_Token;

   procedure Row (Name : String; Typ : Character; Expect : Outcome; Pos : Natural; Hex : String; Arcs : String;
                  Tail : Integer) is
      Stream : OctetBuffer (1 .. 400) := (others => 0);
      N      : Natural := 0;
      X      : T := T_Init;
      XS     : S := S_Init;
      P      : Natural := Hex'First;
      First, Last, Rep : Natural;
   begin
      while P <= Hex'Last loop
         Next_Token (Hex, P, First, Last, Rep);
         for R in 1 .. Rep loop
            for H in 0 .. (Last - First + 1) / 2 - 1 loop
               N := N + 1;
               Stream (N) := Asn1Byte (Hv (Hex (First + 2 * H)) * 16 + Hv (Hex (First + 2 * H + 1)));
            end loop;
         end loop;
      end loop;
      P := Arcs'First;
      while P <= Arcs'Last loop
         Next_Token (Arcs, P, First, Last, Rep);
         for R in 1 .. Rep loop
            X.Length := X.Length + 1;
            X.values (X.Length) :=
              (if Arcs (First .. Last) = "A9" then 2 ** 62
               elsif Arcs (First .. Last) = "A10" then 2 ** 63
               elsif Arcs (First .. Last) = "M" then Asn1UInt'Last
               else Asn1UInt'Value (Arcs (First .. Last)));
         end loop;
      end loop;
      XS.o := X;
      if Tail >= 0 then
         XS.tail := Asn1UInt (Tail);
      end if;
      if Expect = Accepted and then N * 8 /= Pos then
         Fail (Name, "row", "stream bits", N * 8, Pos);
      end if;
      for Acn in Boolean loop
         declare
            Codec : constant String := (if Acn then "ACN" else "uPER");
            Req   : constant Positive :=
              (case Typ is
                  when 'S' => (if Acn then S_REQUIRED_BYTES_FOR_ACN_ENCODING else S_REQUIRED_BYTES_FOR_ENCODING),
                  when 'R' => (if Acn then R_REQUIRED_BYTES_FOR_ACN_ENCODING else R_REQUIRED_BYTES_FOR_ENCODING),
                  when others =>
                    (if Acn then T_REQUIRED_BYTES_FOR_ACN_ENCODING else T_REQUIRED_BYTES_FOR_ENCODING));
            Cap    : constant Positive := Natural'Max (N, Req);
            Bs     : adaasn1rtl.encoding.Bitstream (Cap);
            D      : T := T_Init;
            DS     : S := S_Init;
            Res    : ASN1_RESULT;
            Exp_Err : Integer;
         begin
            Bs.Buffer := (others => 0);
            Bs.Buffer (1 .. N) := Stream (1 .. N);
            Bs.Current_Bit_Pos := 0;
            case Typ is
               when 'T' =>
                  if Acn then
                     T_ACN_Decode_aux (D, Bs, Res);
                  else
                     T_Decode_aux (D, Bs, Res);
                  end if;
                  Exp_Err := (if Acn then ERR_ACN_DECODE_T else ERR_UPER_DECODE_T);
               when 'R' =>
                  if Acn then
                     R_ACN_Decode_aux (D, Bs, Res);
                  else
                     R_Decode_aux (D, Bs, Res);
                  end if;
                  Exp_Err := (if Acn then ERR_ACN_DECODE_R else ERR_UPER_DECODE_R);
               when others =>
                  if Acn then
                     S_ACN_Decode_aux (DS, Bs, Res);
                  else
                     S_Decode_aux (DS, Bs, Res);
                  end if;
                  Exp_Err := (if Acn then ERR_ACN_DECODE_S_O else ERR_UPER_DECODE_S_O);
                  D := DS.o;
            end case;
            if Expect = Rejected then
               if Res.Success then
                  Fail (Name, Codec, "accepted", 1, 0);
               end if;
               if Res.ErrorCode /= Exp_Err then
                  Fail (Name, Codec, "error code", Res.ErrorCode, Exp_Err);
               end if;
               if Bs.Current_Bit_Pos /= Pos then
                  Fail (Name, Codec, "bit position after the decoder", Bs.Current_Bit_Pos, Pos);
               end if;
            else
               if not Res.Success then
                  Fail (Name, Codec, "rejected, error code", Res.ErrorCode, 0);
               end if;
               if Bs.Current_Bit_Pos /= Pos then
                  Fail (Name, Codec, "bit position after the decoder", Bs.Current_Bit_Pos, Pos);
               end if;
               if Res.Success and then not T_Equal (X, D) then
                  Fail (Name, Codec, "decoded arcs differ (count)", D.Length, X.Length);
               end if;
               if Res.Success and then Typ = 'S' and then DS.tail /= XS.tail then
                  Fail (Name, Codec, "decoded tail", Integer (DS.tail), Integer (XS.tail));
               end if;
               Bs.Buffer := (others => 0);
               Bs.Current_Bit_Pos := 0;
               case Typ is
                  when 'T' =>
                     if Acn then
                        T_ACN_Encode_aux (X, Bs, Res);
                     else
                        T_Encode_aux (X, Bs, Res);
                     end if;
                  when 'R' =>
                     if Acn then
                        R_ACN_Encode_aux (X, Bs, Res);
                     else
                        R_Encode_aux (X, Bs, Res);
                     end if;
                  when others =>
                     if Acn then
                        S_ACN_Encode_aux (XS, Bs, Res);
                     else
                        S_Encode_aux (XS, Bs, Res);
                     end if;
               end case;
               if not Res.Success then
                  Fail (Name, Codec, "encode failed, error code", Res.ErrorCode, 0);
               end if;
               if Bs.Current_Bit_Pos /= Pos then
                  Fail (Name, Codec, "encoded bits", Bs.Current_Bit_Pos, Pos);
               elsif Bs.Buffer (1 .. N) /= Stream (1 .. N) then
                  Fail (Name, Codec, "encoded bytes differ from the stream", 0, 1);
               end if;
            end if;
         exception
            when Ex : others =>
               Ada.Text_IO.Put_Line ("FAILED " & Name & " (" & Codec & "): "
                                     & Ada.Exceptions.Exception_Information (Ex));
               Failed := True;
         end;
      end loop;
   end Row;
begin
   --  valid values: one- and two-octet contents lengths up to the largest values (190 / 200 octets), largest arcs
   Row ("A1 T 127 octets: length 7F", 'T', Accepted, 1024, "7f 2a c08080808080808000*14", "1 2 A9*14", -1);
   Row ("A2 T 128 octets: length 80 80", 'T', Accepted, 1040, "8080 2a c08080808080808000*14 05", "1 2 A9*14 5", -1);
   Row ("A3 T 129 octets", 'T', Accepted, 1048, "8081 2a c08080808080808000*14 05 06", "1 2 A9*14 5 6", -1);
   Row ("A4 T 163 octets, 20 arcs", 'T', Accepted, 1320, "80a3 2a c08080808080808000*18", "1 2 A9*18", -1);
   Row ("A5 T 189 octets", 'T', Accepted, 1528, "80bd 81808080808080808000*18 c08080808080808000", "2 9223372036854775728 A10*17 A9", -1);
   Row ("A6 T 190 octets (largest OID contents)", 'T', Accepted, 1536, "80be 81808080808080808050 81808080808080808000*18", "2 A10*19", -1);
   Row ("A7 T 190 octets, largest arcs", 'T', Accepted, 1536, "80be 81ffffffffffffffff7f*19", "2 18446744073709551535 M*18", -1);
   Row ("A8 R 127 octets", 'R', Accepted, 1024, "7f c08080808080808000*14 07", "A9*14 7", -1);
   Row ("A9 R 128 octets: length 80 80", 'R', Accepted, 1040, "8080 c08080808080808000*14 07 08", "A9*14 7 8", -1);
   Row ("A10 R 191 octets", 'R', Accepted, 1544, "80bf 81808080808080808000*19 03", "A10*19 3", -1);
   Row ("A11 R 199 octets", 'R', Accepted, 1608, "80c7 81808080808080808000*19 c08080808080808000", "A10*19 A9", -1);
   Row ("A12 R 200 octets (largest RELATIVE-OID contents)", 'R', Accepted, 1616, "80c8 81808080808080808000*20", "A10*20", -1);
   Row ("A13 R 200 octets, largest arcs", 'R', Accepted, 1616, "80c8 81ffffffffffffffff7f*20", "M*20", -1);
   Row ("A14 S o of 128 octets, tail A5", 'S', Accepted, 1048, "8080 2a c08080808080808000*14 05 a5", "1 2 A9*14 5", 165);
   --  small values, 20 arcs, arcs of 2^64 - 1, 0x80 octets inside a subidentifier
   Row ("A15 T {1 2}", 'T', Accepted, 16, "01 2a", "1 2", -1);
   Row ("A16 T 20 arcs of one octet", 'T', Accepted, 160, "13 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13", "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19", -1);
   Row ("A17 R 20 arcs of one octet", 'R', Accepted, 168, "14 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13 14", "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20", -1);
   Row ("A18 R arc 2^64 - 1", 'R', Accepted, 88, "0a 81ffffffffffffffff7f", "M", -1);
   Row ("A19 T first subidentifier 2^64 - 1", 'T', Accepted, 88, "0a 81ffffffffffffffff7f", "2 18446744073709551535", -1);
   Row ("A20 R arc 2^32", 'R', Accepted, 48, "05 9080808000", "4294967296", -1);
   Row ("A21 R 0x80 inside a subidentifier", 'R', Accepted, 32, "03 818000", "16384", -1);
   Row ("A22 T 0x80 inside the first subidentifier", 'T', Accepted, 32, "03 818000", "2 16304", -1);
   Row ("A23 T 0x80 inside a subidentifier", 'T', Accepted, 48, "05 2a 81808000", "1 2 2097152", -1);
   --  a two-octet length below 128 (X.691 10.9.3.6, Issue 156): rejected after the second octet
   Row ("N1 T 80 03", 'T', Rejected, 16, "80032a0304", "", -1);
   Row ("N2 T 80 7F + 127 octets", 'T', Rejected, 16, "807f 2a c08080808080808000*14", "", -1);
   Row ("N3 T 80 00", 'T', Rejected, 16, "8000", "", -1);
   Row ("N4 T 80 01", 'T', Rejected, 16, "80012a", "", -1);
   Row ("N5 R 80 01", 'R', Rejected, 16, "800105", "", -1);
   Row ("N6 R 80 7F + 127 octets", 'R', Rejected, 16, "807f 01*127", "", -1);
   Row ("N7 S 80 03", 'S', Rejected, 16, "80032a0304a5", "", -1);
   --  a fragment header (11xxxxxx): no value of at most 20 arcs has 16K contents octets; rejected after it
   Row ("F1 T C0", 'T', Rejected, 8, "c0002a", "", -1);
   Row ("F2 T C1", 'T', Rejected, 8, "c1 00*16", "", -1);
   Row ("F3 T C4", 'T', Rejected, 8, "c42a", "", -1);
   Row ("F4 T E0", 'T', Rejected, 8, "e02a", "", -1);
   Row ("F5 R FE", 'R', Rejected, 8, "fe01", "", -1);
   Row ("F6 R FF FF", 'R', Rejected, 8, "ffff", "", -1);
   --  lengths no value reaches, more than 20 arcs: rejected after the 20th arc
   Row ("L1 R 81 00 + 256 octets", 'R', Rejected, 176, "8100 01*256", "", -1);
   Row ("L2 T 81 00 + 256 octets", 'T', Rejected, 168, "8100 01*256", "", -1);
   Row ("L3 T BF FF + 300 octets", 'T', Rejected, 168, "bfff 01*300", "", -1);
   Row ("L4 R 21 arcs", 'R', Rejected, 168, "15 01*21", "", -1);
   Row ("L5 T 21 arcs", 'T', Rejected, 160, "14 01*20", "", -1);
   Row ("L6 T 80 BE + 128 octets (zero arcs follow)", 'T', Rejected, 1064, "80be 2a c08080808080808000*14 05", "", -1);
   --  a length of 0 (no subidentifier)
   Row ("E1 T 00", 'T', Rejected, 8, "00", "", -1);
   Row ("E2 R 00", 'R', Rejected, 8, "00", "", -1);
   --  the contents length ends inside a subidentifier
   Row ("E3 T 02 2A 85", 'T', Rejected, 24, "022a85", "", -1);
   Row ("E4 R 01 85", 'R', Rejected, 16, "0185", "", -1);
   Row ("E5 T 80 80 + 190 octets (length ends inside an arc)", 'T', Rejected, 1040, "8080 81808080808080808050 81808080808080808000*18", "", -1);
   --  an arc over 64 bits: rejected at the octet that overflows
   Row ("E6 R arc 2^64", 'R', Rejected, 88, "0a 82808080808080808000", "", -1);
   Row ("E7 T first subidentifier 2^64", 'T', Rejected, 88, "0a 82808080808080808000", "", -1);
   Row ("E8 R arc 2^70", 'R', Rejected, 96, "0b 8180808080808080808000", "", -1);
   Row ("E9 R 80 80 + 128 continuation octets", 'R', Rejected, 104, "8080 81*128", "", -1);

   if Failed then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Ada.Text_IO.Put_Line ("uper-wire 006 (Ada): all checks passed");
   end if;
end Wire_Test;
