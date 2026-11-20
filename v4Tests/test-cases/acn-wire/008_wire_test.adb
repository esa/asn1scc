--  ACN checks of 008.asn1 / 008.acn in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, with and
--  without -gnata; Issue 134): the bytes, accepted and rejected streams of 008_wire_test.c.
with Ada.Text_IO;
with Ada.Command_Line;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with acn_wire_aligned; use acn_wire_aligned;
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

   R : ASN1_RESULT;
   QA : constant Q6 := (y => True, f => (a => 5, b => True, c => 0, Exist => (b => 1, c => 0)), z => True);
   QB : constant Q6 := (y => True, f => (a => 16#AB#, b => False, c => 16#CD#, Exist => (b => 1, c => 1)),
                        z => True);
   QZ : constant Q6 := (y => False, f => (a => 0, b => False, c => 0, Exist => (b => 0, c => 0)), z => False);
   W : constant QF := (y => True, f => (a => 1, b => 2), g => (a => 3, b => 4), z => True);
   M : MyMsg (b_PRESENT);
   D : Q6;
   DW : QF;
   DM : MyMsg;
begin
   declare
      S0, S : Q6_ACN_Stream;

      procedure Round_Trip (What : String; V : Q6; Expected : OctetBuffer; Bits : Natural) is
      begin
         Q6_ACN_Encode (V, S0, R);
         if not R.Success then
            Fail (What & ": encode");
         end if;
         Check_Bytes (What, S0, Expected, Bits);
         S := S0;
         S.Current_Bit_Pos := 0;
         Q6_ACN_Decode (D, S, R);
         if not R.Success or else D /= V then
            Fail (What & ": decode");
         end if;
      end Round_Trip;
   begin
      Round_Trip ("Q6 {a 5, b TRUE}", QA, (16#80#, 16#40#, 16#B0#, 16#40#), 26);
      Round_Trip ("Q6 {a AB, b FALSE, c CD}", QB, (16#80#, 16#F5#, 16#6C#, 16#D0#, 16#40#), 34);

      --  the stream of 008_wire_test.c without its last octet (Q6 needs at most 5)
      S := S0;
      Load (S, (16#00#, 16#80#, 16#00#, 16#00#, 16#00#));
      Q6_ACN_Decode (D, S, R);
      if not R.Success or else D /= QZ or else S.Current_Bit_Pos /= 34 then
         Fail ("Q6: a region of 3 octets holding {a 0} was rejected");
      end if;

      S := S0;
      Load (S, (16#00#, 16#20#, 16#00#, 16#00#, 16#00#));
      Q6_ACN_Decode (D, S, R);
      if R.Success or else R.ErrorCode = 0 then
         Fail ("Q6: a region of 2 octets holding 18 bits was accepted");
      end if;
   end;

   declare
      S : QF_ACN_Stream;
   begin
      QF_ACN_Encode (W, S, R);
      if not R.Success then
         Fail ("QF: encode");
      end if;
      Check_Bytes ("QF", S, (16#80#, 16#01#, 16#02#, 16#03#, 16#04#, 16#80#), 41);
      S.Current_Bit_Pos := 0;
      QF_ACN_Decode (DW, S, R);
      if not R.Success or else DW /= W then
         Fail ("QF: decode");
      end if;
   end;

   declare
      S0, S : MyMsg_ACN_Stream;
   begin
      M.b := (Length => 1, Data => (1 => 4000, others => 0));
      MyMsg_ACN_Encode (M, S0, R);
      if not R.Success then
         Fail ("MyMsg: encode");
      end if;
      Check_Bytes ("MyMsg", S0, (16#80#, 16#00#, 16#FA#, 16#00#), 32);
      S := S0;
      S.Current_Bit_Pos := 0;
      MyMsg_ACN_Decode (DM, S, R);
      if not R.Success or else DM /= MyMsg (M) then
         Fail ("MyMsg: decode");
      end if;

      S := S0;
      Load (S, (16#80#, 16#95#));
      MyMsg_ACN_Decode (DM, S, R);
      if R.Success or else R.ErrorCode = 0 then
         Fail ("MyMsg: a region of 151 octets was accepted");
      end if;
   end;

   if Failed then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Ada.Text_IO.Put_Line ("acn-wire 008 (Ada): all checks passed");
   end if;
end Wire_Test;
