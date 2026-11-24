--  --acn-v2: determinant fields exposed by an OPTIONAL referenced component to its siblings (run by
--  v4Tests/scripts/runWireTests.sh, -gnata). A stream with hdr2 absent and data2 present does not hold the size of data2:
--  the decoder must fail with the error code of data2 (it decoded data2 with size 0); x, present when the exposed BOOLEAN
--  field says so, is absent; with pl2 present the decoder of pl2.d fails (the size it receives is not in the stream).
--  The round trip is in 004_decode_test.c.
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl;
with adaasn1rtl.encoding;
with WIRE;
procedure Decode_Test is
   use type adaasn1rtl.BIT;
   use type adaasn1rtl.Asn1UInt;

   procedure Decode (Name : String; First, Second : adaasn1rtl.Asn1Byte; Exp_Err : Integer; V : out WIRE.MyPDU) is
      S : WIRE.MyPDU_ACN_Stream := adaasn1rtl.encoding.BitStream_init (WIRE.MyPDU_ACN_STREAM_SIZE);
      R : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer := (others => 16#AB#);
      S.Buffer (S.Buffer'First) := First;
      S.Buffer (S.Buffer'First + 1) := Second;
      WIRE.MyPDU_ACN_Decode (V, S, R);
      if (Exp_Err = 0 and then not R.Success)
        or else (Exp_Err > 0 and then (R.Success or else R.ErrorCode /= Exp_Err))
        or else (Exp_Err = -1 and then (R.Success or else R.ErrorCode = 0))
      then
         Ada.Text_IO.Put_Line (Name & ": Success=" & Boolean'Image (R.Success) & " ErrorCode=" &
                               Integer'Image (R.ErrorCode) & ", expected" & Integer'Image (Exp_Err));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   exception
      when others =>
         Ada.Text_IO.Put_Line (Name & ": exception");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Decode;

   V : WIRE.MyPDU;
begin
   --  hdr2 absent, data2 present
   Decode ("data2", 16#40#, 16#AB#, WIRE.ERR_ACN_DECODE_MYPDU_DATA2, V);
   --  hdr2 absent: x absent, tail 16#5A#
   Decode ("x", 16#0B#, 16#40#, 0, V);
   if V.Exist.hdr2 /= 0 or else V.Exist.x /= 0 or else V.tail /= 16#5A# then
      Ada.Text_IO.Put_Line ("x: value");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
   --  hdr2 absent, pl2 present: pl2.d checks that the reference it receives holds a field of the stream
   --  (q/issues.md Issue 107; ERR_ACN_DECODE_MYPDU_PL2_D is private to the package body: any nonzero code)
   Decode ("pl2", 16#20#, 16#AB#, -1, V);
   Ada.Text_IO.Put_Line ("decode checks done");
end Decode_Test;
