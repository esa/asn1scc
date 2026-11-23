--  uPER and ACN checks of 003.asn1 in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, with and
--  without -gnata; Issue 134): the bytes of 003_wire_test.c (written by hand from X.691 and the ACN default encoding),
--  the streams it rejects and the streams of 4 octets that the decoders used to write into a temporary buffer of 3
--  (valid encodings).
with Ada.Text_IO;
with Ada.Command_Line;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with uper_wire_nested; use uper_wire_nested;
procedure Wire_Test is
   Failed : Boolean := False;

   procedure Fail (What : String) is
   begin
      Ada.Text_IO.Put_Line ("FAILED: " & What);
      Failed := True;
   end Fail;

   procedure Check_Bytes
     (What : String; S : adaasn1rtl.encoding.Bitstream; Expected : OctetBuffer; Bits : Natural) is
   begin
      if S.Current_Bit_Pos /= Bits or else S.Buffer (1 .. Expected'Length) /= Expected then
         Fail (What & ": encoded bytes");
      end if;
   end Check_Bytes;

   --  the given bytes at the start of the stream, zeros after them, position 0
   procedure Load (S : in out adaasn1rtl.encoding.Bitstream; Bytes : OctetBuffer) is
   begin
      S.Buffer := (others => 0);
      S.Buffer (1 .. Bytes'Length) := Bytes;
      S.Current_Bit_Pos := 0;
   end Load;

   function P10_Of (a : T_a; Has_B : Boolean; b : Boolean; Has_C : Boolean; c : T_c) return P10 is
     ((f => (a => a, b => b, c => c,
             Exist => (b => (if Has_B then 1 else 0), c => (if Has_C then 1 else 0))),
       z => True));

   R : ASN1_RESULT;
   V1 : constant P10 := P10_Of (5, True, True, False, 0);
   V2 : constant P10 := P10_Of (5, False, False, False, 0);
   V3 : constant P10 := P10_Of (16#AB#, True, False, True, 16#CD#);
   Zero : P10 := P10_Of (0, False, False, False, 0);
   W : constant P70 := (f => (a => 255, b => 255), g => (a => 255, b => 255), z => False);
   D : P10;
   DW : P70;
begin
   --  uPER
   declare
      S0, S : P10_uPER_Stream;

      procedure Round_Trip (What : String; V : P10; Expected : OctetBuffer; Bits : Natural) is
      begin
         P10_Encode (V, S0, R);
         if not R.Success then
            Fail (What & ": encode");
         end if;
         Check_Bytes (What, S0, Expected, Bits);
         S := S0;
         S.Current_Bit_Pos := 0;
         P10_Decode (D, S, R);
         if not R.Success or else D /= V then
            Fail (What & ": decode");
         end if;
      end Round_Trip;
   begin
      Round_Trip ("uPER {a 5, b TRUE}", V1, (16#03#, 16#02#, 16#81#, 16#60#, 16#80#), 33);
      Round_Trip ("uPER {a 5}", V2, (16#03#, 16#02#, 16#01#, 16#40#, 16#80#), 33);
      Round_Trip ("uPER {a AB, b FALSE, c CD}", V3, (16#04#, 16#03#, 16#EA#, 16#D9#, 16#A0#, 16#80#), 41);

      --  Box of 0 octets: T's reads do not fail in Ada (the stream is zero-filled), Box's decoder rejects contents
      --  that end past its region (C: T's decoder fails at the end of the region)
      S := S0;
      Load (S, (16#04#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#));
      P10_Decode (D, S, R);
      if R.Success or else R.ErrorCode /= ERR_UPER_DECODE_BOX_2 then
         Fail ("uPER: a Box of 0 octets was accepted");
      end if;

      S := S0;
      Load (S, (16#05#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#));
      P10_Decode (D, S, R);
      if R.Success or else R.ErrorCode /= ERR_UPER_DECODE_P10_F_2_2 then
         Fail ("uPER: f of 5 octets was accepted");
      end if;

      S := S0;
      Load (S, (16#04#, 16#03#, 16#00#, 16#00#, 16#00#, 16#80#));
      P10_Decode (D, S, R);
      if not R.Success or else D /= Zero or else S.Current_Bit_Pos /= 41 then
         Fail ("uPER: f of 4 octets holding {a 0} was rejected");
      end if;
   end;

   --  ACN (no ACN specification)
   declare
      S0, S : P10_ACN_Stream;

      procedure Round_Trip (What : String; V : P10; Expected : OctetBuffer; Bits : Natural) is
      begin
         P10_ACN_Encode (V, S0, R);
         if not R.Success then
            Fail (What & ": encode");
         end if;
         Check_Bytes (What, S0, Expected, Bits);
         S := S0;
         S.Current_Bit_Pos := 0;
         P10_ACN_Decode (D, S, R);
         if not R.Success or else D /= V then
            Fail (What & ": decode");
         end if;
      end Round_Trip;
   begin
      Round_Trip ("ACN {a 5, b TRUE}", V1, (16#20#, 16#58#, 16#00#, 16#40#), 26);
      Round_Trip ("ACN {a 5}", V2, (16#00#, 16#50#, 16#00#, 16#40#), 26);
      Round_Trip ("ACN {a AB, b FALSE, c CD}", V3, (16#FA#, 16#B6#, 16#68#, 16#00#, 16#40#), 34);

      Zero.z := False;
      S := S0;
      Load (S, (16#80#, 16#00#, 16#00#, 16#00#, 16#00#));
      P10_ACN_Decode (D, S, R);
      if not R.Success or else D /= Zero or else S.Current_Bit_Pos /= 34 then
         Fail ("ACN: f of 4 octets holding {a 0} was rejected");
      end if;
   end;

   --  fixed-size contents
   declare
      S : P70_uPER_Stream;
      SA : P70_ACN_Stream;
   begin
      P70_Encode (W, S, R);
      if not R.Success then
         Fail ("uPER P70: encode");
      end if;
      Check_Bytes ("uPER P70", S,
                   (16#03#, 16#02#, 16#FF#, 16#FF#, 16#18#, 16#10#, 16#FF#, 16#FF#, 16#00#), 65);
      S.Current_Bit_Pos := 0;
      P70_Decode (DW, S, R);
      if not R.Success or else DW /= W then
         Fail ("uPER P70: decode");
      end if;

      P70_ACN_Encode (W, SA, R);
      if not R.Success then
         Fail ("ACN P70: encode");
      end if;
      Check_Bytes ("ACN P70", SA, (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#00#), 33);
      SA.Current_Bit_Pos := 0;
      P70_ACN_Decode (DW, SA, R);
      if not R.Success or else DW /= W then
         Fail ("ACN P70: decode");
      end if;
   end;

   if Failed then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Ada.Text_IO.Put_Line ("uper-wire 003 (Ada): all checks passed");
   end if;
end Wire_Test;
