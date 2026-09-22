--  Direct tests of the Ada XER runtime decoders (adaasn1rtl.encoding.xer):
--  oversized or malformed input must return a decoding error, never raise
--  Constraint_Error / Program_Error, and well-formed input must still decode.
--  One test group per process, selected by the first argument (see
--  reproduce_ada.sh).  Exit status 1 on any failed expectation.

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Command_Line;        use Ada.Command_Line;
with Interfaces;              use Interfaces;
with adaasn1rtl;              use adaasn1rtl;
with adaasn1rtl.encoding.xer; use adaasn1rtl.encoding.xer;

procedure Ada_Runtime_Tests is
   Failed : Boolean := False;

   procedure Check (Cond : Boolean; What : String) is
   begin
      if not Cond then
         Put_Line ("  FAIL: " & What);
         Failed := True;
      end if;
   end Check;

   --  Decode <T>Payload</T> into a BitArray of Cap bits.
   procedure Bits
     (Payload : String; Cap : Positive; Expect_Ok : Boolean;
      Expect_Len : Integer := 0)
   is
      Input : constant String := "<T>" & Payload & "</T>";
      Strm  : CharStream (Input'Length);
      Val   : BitArray (1 .. Cap) := (others => 0);
      Len   : Integer := -1;
      Res   : ASN1_RESULT;
   begin
      Strm.Data := Input;
      Xer_DecodeBitString (Strm, "T", Val, Len, Res);
      Check (Res.Success = Expect_Ok, "bits '" & Payload (Payload'First ..
             Integer'Min (Payload'Last, Payload'First + 15)) &
             "' success=" & Boolean'Image (Res.Success));
      if Expect_Ok then
         Check (Len = Expect_Len, "bits len=" & Integer'Image (Len));
      else
         Check (Res.ErrorCode = ERR_INCORRECT_STREAM, "bits error code");
      end if;
   end Bits;

   --  Decode <T>Payload</T> into an OctetBuffer of Cap bytes.
   procedure Octets
     (Payload : String; Cap : Positive; Expect_Ok : Boolean;
      Expect_Len : Integer := 0; Expect_First : Unsigned_8 := 0)
   is
      Input : constant String := "<T>" & Payload & "</T>";
      Strm  : CharStream (Input'Length);
      Val   : OctetBuffer (1 .. Cap) := (others => 0);
      Len   : Integer := -1;
      Res   : ASN1_RESULT;
   begin
      Strm.Data := Input;
      Xer_DecodeOctetString (Strm, "T", Val, Len, Res);
      Check (Res.Success = Expect_Ok, "octets '" & Payload &
             "' success=" & Boolean'Image (Res.Success));
      if Expect_Ok then
         Check (Len = Expect_Len, "octets len=" & Integer'Image (Len));
         Check (Val (1) = Expect_First, "octets first byte");
      else
         Check (Res.ErrorCode = ERR_INCORRECT_STREAM, "octets error code");
      end if;
   end Octets;

   T : constant String := Argument (1);
begin
   if T = "xer-bits" then
      --  ESACERT #74626 input: 2047 bits into a 255-bit array
      Bits (String'(1 .. 2047 => '1'), 255, Expect_Ok => False);
      --  more text than the 32768-byte scratch buffer of the runtime
      Bits (String'(1 .. 40000 => '1'), 255, Expect_Ok => False);
      --  characters other than 0/1
      Bits ("1021", 255, Expect_Ok => False);
      --  positive controls: boundary and ordinary value
      Bits (String'(1 .. 255 => '1'), 255, Expect_Ok => True,
            Expect_Len => 255);
      Bits ("10101010", 255, Expect_Ok => True, Expect_Len => 8);
   elsif T = "xer-octets" then
      Octets ("AABB", 1, Expect_Ok => False);     -- 2 bytes into 1
      Octets ("A", 1, Expect_Ok => False);        -- odd digit count
      Octets ("ZZ", 1, Expect_Ok => False);       -- not hexadecimal
      Octets ("AA", 1, Expect_Ok => True, Expect_Len => 1,
              Expect_First => 16#AA#);
   else
      Put_Line ("Unknown test: " & T);
      Set_Exit_Status (2);
      return;
   end if;

   if Failed then
      Set_Exit_Status (1);
   else
      Put_Line ("PASS " & T);
   end if;
end Ada_Runtime_Tests;
