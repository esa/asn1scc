--  uPER and ACN checks of 002.asn1 in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, with and
--  without -gnata; Issue 133): the bytes of one value, the two streams of 002_wire_test.c that must be rejected, and for
--  every value of the first two octets of the encoded stream: when Box's decoder fails on the stream, PDU's decoder
--  (which starts with f, a Box) must fail too. Before the fix, PDU's decoders asserted that the call of Box's decoder
--  cannot fail (pragma Assert, not checked without -gnata) and decoded on: z overwrote the result and the stream was
--  accepted (uPER 65152, ACN 16384 of the 65536 streams).
with Ada.Text_IO;
with Ada.Command_Line;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with uper_wire_containing; use uper_wire_containing;
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

   R, R1 : ASN1_RESULT;
   V : constant PDU := (f => (a => 5, b => True, c => 7, Exist => (b => 1, c => 1)), z => True);
   D : PDU;
   B : Box;
   Accepted : Natural;
begin
   --  uPER
   declare
      S0, S, S2 : PDU_uPER_Stream;
   begin
      PDU_Encode (V, S0, R);
      if not R.Success then
         Fail ("uPER encode");
      end if;
      Check_Bytes ("uPER", S0, (16#03#, 16#C1#, 16#60#, 16#E0#, 16#80#), 33);
      S := S0;
      S.Current_Bit_Pos := 0;
      PDU_Decode (D, S, R);
      if not R.Success or else D /= V then
         Fail ("uPER decode");
      end if;

      S := S0;
      S.Buffer := (16#00#, 16#80#, 16#00#, 16#00#, 16#00#);
      S.Current_Bit_Pos := 0;
      PDU_Decode (D, S, R);
      if R.Success or else R.ErrorCode = 0 then
         Fail ("uPER: a CONTAINING region of 0 octets was accepted");
      end if;

      Accepted := 0;
      for B0 in Asn1Byte loop
         for B1 in Asn1Byte loop
            S := S0;
            S.Buffer (1) := B0;
            S.Buffer (2) := B1;
            S.Current_Bit_Pos := 0;
            S2 := S;
            B := Box_Init;
            Box_Decode_aux (B, S2, R1);
            PDU_Decode (D, S, R);
            if R.Success and not R1.Success then
               Accepted := Accepted + 1;
            end if;
         end loop;
      end loop;
      if Accepted /= 0 then
         Fail ("uPER: f failed but PDU accepted" & Accepted'Image & " streams");
      end if;
   end;

   --  ACN (no ACN specification)
   declare
      S0, S, S2 : PDU_ACN_Stream;
   begin
      PDU_ACN_Encode (V, S0, R);
      if not R.Success then
         Fail ("ACN encode");
      end if;
      Check_Bytes ("ACN", S0, (16#E0#, 16#B0#, 16#70#, 16#40#), 26);
      S := S0;
      S.Current_Bit_Pos := 0;
      PDU_ACN_Decode (D, S, R);
      if not R.Success or else D /= V then
         Fail ("ACN decode");
      end if;

      S := S0;
      S.Buffer := (16#20#, 16#00#, 16#00#, 16#00#);
      S.Current_Bit_Pos := 0;
      PDU_ACN_Decode (D, S, R);
      if R.Success or else R.ErrorCode = 0 then
         Fail ("ACN: a CONTAINING region too short for its contents was accepted");
      end if;

      Accepted := 0;
      for B0 in Asn1Byte loop
         for B1 in Asn1Byte loop
            S := S0;
            S.Buffer (1) := B0;
            S.Buffer (2) := B1;
            S.Current_Bit_Pos := 0;
            S2 := S;
            B := Box_Init;
            Box_ACN_Decode_aux (B, S2, R1);
            PDU_ACN_Decode (D, S, R);
            if R.Success and not R1.Success then
               Accepted := Accepted + 1;
            end if;
         end loop;
      end loop;
      if Accepted /= 0 then
         Fail ("ACN: f failed but PDU accepted" & Accepted'Image & " streams");
      end if;
   end;

   if Failed then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Ada.Text_IO.Put_Line ("uper-wire 002 (Ada): all checks passed");
   end if;
end Wire_Test;
