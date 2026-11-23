--  uPER checks of 005.asn1 in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, without and with
--  -gnata; Issue 154): the rows of 005_wire_test.c, whose C results are the reference. Each row encodes the contents
--  value (the region), then lays the region out in the stream behind the given length fields: H (x) one header /
--  length octet, L2 (n) a two-octet length, P (k) the next k units of the region (octets; bits for B32K; zeros past
--  the end of the region). The decoder must reject the stream with the given error code and stop at the given bit
--  position, or accept it, decode the contents value and encode it to the same bytes. The positions differ from C on
--  purpose where a fragmented length is rejected: the Ada decoder restores the position to the start of the
--  CONTAINING (bit 0) there, C stops after the offending header.
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Interfaces;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with uper_wire_cont; use uper_wire_cont;
procedure Wire_Test is
   use type Interfaces.Unsigned_8;
   Failed : Boolean := False;

   procedure Fail (Name, What : String; Got, Expected : Integer) is
   begin
      Ada.Text_IO.Put_Line
        ("FAILED " & Name & ": " & What & " (got" & Got'Image & ", expected" & Expected'Image & ")");
      Failed := True;
   end Fail;

   type Outcome is (Rejected, Accepted);
   type Kind is (Hd, Len2, Part);
   type Seg is record
      K : Kind;
      V : Natural;
   end record;
   type Seg_Array is array (Positive range <>) of Seg;
   function H (X : Natural) return Seg is ((Hd, X));
   function L2 (N : Natural) return Seg is ((Len2, N));
   function P (N : Natural) return Seg is ((Part, N));

   Cap : constant := 40000;
   type Stream_Ptr is access adaasn1rtl.encoding.Bitstream;
   procedure Free is new Ada.Unchecked_Deallocation (adaasn1rtl.encoding.Bitstream, Stream_Ptr);

   generic
      type VT is private;
      with procedure Enc_Contents
        (val : VT; bs : in out adaasn1rtl.encoding.Bitstream; result : out ASN1_RESULT);
      with procedure Dec (val : in out VT; bs : in out adaasn1rtl.encoding.Bitstream; result : out ASN1_RESULT);
      with procedure Enc (val : VT; bs : in out adaasn1rtl.encoding.Bitstream; result : out ASN1_RESULT);
      with function Equal (val1, val2 : VT) return Boolean;
      with function Init return VT;
      --  the contents value: octets (i * 7 + 1) mod 256 (index i from 0), N of them where the size varies
      with function Make (N : Natural) return VT;
      Unit_Bits : Positive;
   procedure Check (Name : String; N : Natural; Expect : Outcome; Err : Integer; Pos : Natural; Segs : Seg_Array);

   procedure Check (Name : String; N : Natural; Expect : Outcome; Err : Integer; Pos : Natural; Segs : Seg_Array)
   is
      type VP is access VT;
      procedure Free is new Ada.Unchecked_Deallocation (VT, VP);
      X, D        : VP;
      Rg, S, E    : Stream_Ptr;
      R           : ASN1_RESULT;
      At_Bit      : Natural := 0;
      R_Bit       : Natural := 0;
      Region_Bits : constant Natural := Cap * 8;
      N_Bytes     : Natural;

      procedure Put_Bit (B : Natural) is
      begin
         if B = 1 then
            S.Buffer (At_Bit / 8 + 1) := S.Buffer (At_Bit / 8 + 1) or Asn1Byte (2 ** (7 - At_Bit mod 8));
         end if;
         At_Bit := At_Bit + 1;
      end Put_Bit;
   begin
      X := new VT'(Make (N));
      Rg := new adaasn1rtl.encoding.Bitstream'(adaasn1rtl.encoding.BitStream_init (Cap));
      Rg.Buffer := (others => 0);
      Enc_Contents (X.all, Rg.all, R);
      if not R.Success then
         Fail (Name, "contents encode failed, error code", R.ErrorCode, 0);
         return;
      end if;
      S := new adaasn1rtl.encoding.Bitstream'(adaasn1rtl.encoding.BitStream_init (Cap));
      S.Buffer := (others => 0);
      for G of Segs loop
         case G.K is
            when Hd =>
               for B in reverse 0 .. 7 loop
                  Put_Bit ((G.V / 2 ** B) mod 2);
               end loop;
            when Len2 =>
               for B in reverse 0 .. 15 loop
                  Put_Bit (((16#8000# + G.V) / 2 ** B) mod 2);
               end loop;
            when Part =>
               for J in 1 .. G.V * Unit_Bits loop
                  if R_Bit < Region_Bits then
                     Put_Bit (Natural (Rg.Buffer (R_Bit / 8 + 1) / 2 ** (7 - R_Bit mod 8)) mod 2);
                  else
                     Put_Bit (0);
                  end if;
                  R_Bit := R_Bit + 1;
               end loop;
         end case;
      end loop;
      N_Bytes := (At_Bit + 7) / 8;

      D := new VT'(Init);
      Dec (D.all, S.all, R);
      if Expect = Rejected then
         if R.Success then
            Fail (Name, "accepted", 1, 0);
         end if;
         if R.ErrorCode /= Err then
            Fail (Name, "error code", R.ErrorCode, Err);
         end if;
         if S.Current_Bit_Pos /= Pos then
            Fail (Name, "bit position after the decoder", S.Current_Bit_Pos, Pos);
         end if;
      else
         if At_Bit /= Pos then
            Fail (Name, "row: stream bits", At_Bit, Pos);
         end if;
         if not R.Success then
            Fail (Name, "rejected, error code", R.ErrorCode, 0);
         end if;
         if S.Current_Bit_Pos /= At_Bit then
            Fail (Name, "bit position after the decoder", S.Current_Bit_Pos, At_Bit);
         end if;
         if R.Success and then not Equal (X.all, D.all) then
            Fail (Name, "decoded value differs from the expected one", 0, 1);
         end if;
         E := new adaasn1rtl.encoding.Bitstream'(adaasn1rtl.encoding.BitStream_init (Cap));
         E.Buffer := (others => 0);
         Enc (X.all, E.all, R);
         if not R.Success then
            Fail (Name, "encode failed, error code", R.ErrorCode, 0);
         end if;
         if E.Current_Bit_Pos /= At_Bit then
            Fail (Name, "encoded bits", E.Current_Bit_Pos, At_Bit);
         elsif E.Buffer (1 .. N_Bytes) /= S.Buffer (1 .. N_Bytes) then
            Fail (Name, "encoded bytes differ from the stream", 0, 1);
         end if;
         Free (E);
      end if;
      Free (X);
      Free (D);
      Free (Rg);
      Free (S);
   exception
      when Ex : others =>
         Ada.Text_IO.Put_Line ("FAILED " & Name & ": " & Ada.Exceptions.Exception_Information (Ex));
         Failed := True;
   end Check;

   function Octet (I : Natural) return Asn1Byte is (Asn1Byte ((I * 7 + 1) mod 256));

   function Make_C32K (N : Natural) return C32K is
      pragma Unreferenced (N);
      V : C32K;
   begin
      for I in V.Data'Range loop
         V.Data (I) := Octet (I - 1);
      end loop;
      return V;
   end Make_C32K;
   function Make_B32K (N : Natural) return B32K is
      pragma Unreferenced (N);
      V : B32K;
   begin
      for I in V.Data'Range loop
         V.Data (I) := Octet (I - 1);
      end loop;
      return V;
   end Make_B32K;
   function Make_B80K (N : Natural) return B80K is
      pragma Unreferenced (N);
      V : B80K;
   begin
      for I in V.Data'Range loop
         V.Data (I) := Octet (I - 1);
      end loop;
      return V;
   end Make_B80K;
   function Make_CV (N : Natural) return CV is
      V : CV := CV_Init;
   begin
      V.Length := N;
      for I in V.Data'Range loop
         V.Data (I) := (if I <= N then Octet (I - 1) else 0);
      end loop;
      return V;
   end Make_CV;

   procedure Check_C32K is new Check (C32K, O32768_Encode_aux, C32K_Decode_aux, C32K_Encode_aux, C32K_Equal,
                                      C32K_Init, Make_C32K, 8);
   procedure Check_B32K is new Check (B32K, O4096_Encode_aux, B32K_Decode_aux, B32K_Encode_aux, B32K_Equal,
                                      B32K_Init, Make_B32K, 1);
   procedure Check_CV is new Check (CV, OV_Encode_aux, CV_Decode_aux, CV_Encode_aux, CV_Equal, CV_Init, Make_CV, 8);
   procedure Check_B80K is new Check (B80K, O10240_Encode_aux, B80K_Decode_aux, B80K_Encode_aux, B80K_Equal,
                                      B80K_Init, Make_B80K, 1);
begin
   --  the canonical layouts (C32K / B32K: 32768 units = C2 + final length 00; CV: a final length after a C1, in one
   --  and in two octets)
   Check_C32K ("V1 C32K C2 00", 0, Accepted, 0, 262160, (H (16#C2#), P (32768), H (16#00#)));
   Check_B32K ("V2 B32K C2 00 (bits)", 0, Accepted, 0, 32784, (H (16#C2#), P (32768), H (16#00#)));
   Check_CV ("V3 CV 32766 octets: region C2 00", 32766, Accepted, 0, 262160, (H (16#C2#), P (32768), H (16#00#)));
   Check_CV ("V4 CV 16384 octets: region C1 + 02", 16384, Accepted, 0, 131104,
             (H (16#C1#), P (16384), H (16#02#), P (2)));
   Check_CV ("V5 CV 16510 octets: region C1 + 80 80", 16510, Accepted, 0, 132120,
             (H (16#C1#), P (16384), L2 (128), P (128)));
   --  (1) a fragment header after a fragment of 16K .. 48K units: the same region, cut into two 16K fragments
   --  (C: rejected after the second header; Ada: the position is restored to 0)
   Check_C32K ("J1 C32K C1 C1 00", 0, Rejected, ERR_UPER_DECODE_C32K_2, 0,
               (H (16#C1#), P (16384), H (16#C1#), P (16384), H (16#00#)));
   Check_B32K ("J2 B32K C1 C1 00 (bits)", 0, Rejected, ERR_UPER_DECODE_B32K_2, 0,
               (H (16#C1#), P (16384), H (16#C1#), P (16384), H (16#00#)));
   Check_CV ("J3 CV 32766 octets: region C1 C1 00", 32766, Rejected, ERR_UPER_DECODE_CV_2, 0,
             (H (16#C1#), P (16384), H (16#C1#), P (16384), H (16#00#)));
   --  (2) a two-octet length below 128: after a C1 (fragmented: restored to 0), and alone (16, as in C)
   Check_CV ("J4 CV 16384 octets: region C1 + 80 02", 16384, Rejected, ERR_UPER_DECODE_CV_2, 0,
             (H (16#C1#), P (16384), L2 (2), P (2)));
   Check_CV ("J5 CV two-octet 5", 16384, Rejected, ERR_UPER_DECODE_CV_2, 16, (L2 (5), P (5)));
   --  a fragment header after a 64K fragment (C4): a C1 and a final length 0 fill B80K's 81920 bits exactly; one bit
   --  more is over the maximum (fragmented: restored to 0)
   Check_B80K ("V6 B80K C4 C1 00 (bits)", 0, Accepted, 0, 81944,
               (H (16#C4#), P (65536), H (16#C1#), P (16384), H (16#00#)));
   Check_B80K ("J6 B80K C4 C1 01 (over the maximum)", 0, Rejected, ERR_UPER_DECODE_B80K_2, 0,
               (H (16#C4#), P (65536), H (16#C1#), P (16384), H (16#01#), P (1)));

   if Failed then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Ada.Text_IO.Put_Line ("uper-wire 005 (Ada): all checks passed");
   end if;
end Wire_Test;
