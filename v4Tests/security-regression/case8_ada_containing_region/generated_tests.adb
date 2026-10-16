--  Decodes OCTET STRING CONTAINING regions whose length (ACN size field or uPER length) is shorter
--  than the contents need. Every malformed stream must fail with a non-zero error code and without
--  an exception; the valid streams must decode.
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Exceptions;
with adaasn1rtl;
with TEST_CASE; use TEST_CASE;

procedure Generated_Tests is
   Failures : Natural := 0;
   Fill     : constant adaasn1rtl.Asn1Byte := 16#AB#;

   procedure Report (Name : String; Expect_Ok : Boolean; Res : adaasn1rtl.ASN1_RESULT; Value_Ok : Boolean := True) is
      Ok : constant Boolean :=
        (if Expect_Ok then Res.Success and then Value_Ok
         else not Res.Success and then Res.ErrorCode /= 0);
   begin
      Ada.Text_IO.Put_Line ((if Ok then "PASS " else "FAIL ") & Name & ": Success=" & Boolean'Image (Res.Success)
                            & " ErrorCode=" & Integer'Image (Res.ErrorCode));
      if not Ok then
         Failures := Failures + 1;
      end if;
   end Report;

   procedure Report_Exception (Name : String; E : Ada.Exceptions.Exception_Occurrence) is
   begin
      Ada.Text_IO.Put_Line ("FAIL " & Name & ": exception " & Ada.Exceptions.Exception_Name (E) & " "
                            & Ada.Exceptions.Exception_Message (E));
      Failures := Failures + 1;
   end Report_Exception;

   --  OctExt: 16-bit length field, then the region = Inner (8-bit n, n data bytes)
   procedure Oct_Ext (Name : String; Bytes : adaasn1rtl.OctetBuffer; Expect_Ok : Boolean; Expect_Len : Natural := 0) is
      S   : OctExt_ACN_Stream := (Size_In_Bytes => OctExt_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : OctExt;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      OctExt_ACN_Decode (V, S, Res);
      Report (Name, Expect_Ok, Res, Res.Success and then V.payload.data.Length = Expect_Len);
   exception
      when E : others => Report_Exception (Name, E);
   end Oct_Ext;

   --  OctEmb: 8-bit uPER length (value - 1), then the region
   procedure Oct_Emb (Name : String; Bytes : adaasn1rtl.OctetBuffer; Expect_Ok : Boolean; Expect_Len : Natural := 0) is
      S   : OctEmb_ACN_Stream := (Size_In_Bytes => OctEmb_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : OctEmb;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      OctEmb_ACN_Decode (V, S, Res);
      Report (Name, Expect_Ok, Res, Res.Success and then V.payload.data.Length = Expect_Len);
   exception
      when E : others => Report_Exception (Name, E);
   end Oct_Emb;

   --  OctDed: 16-bit length field, then the region = DedInner (16-bit hdr, deduced list of 12-bit items)
   procedure Oct_Ded (Name : String; Bytes : adaasn1rtl.OctetBuffer; Expect_Ok : Boolean; Expect_Len : Natural := 0) is
      S   : OctDed_ACN_Stream := (Size_In_Bytes => OctDed_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : OctDed;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      OctDed_ACN_Decode (V, S, Res);
      Report (Name, Expect_Ok, Res, Res.Success and then V.payload.elems.Length = Expect_Len);
   exception
      when E : others => Report_Exception (Name, E);
   end Oct_Ded;

   --  OctDedEmb: 3-bit field + 5-bit uPER length (region byte-aligned, q/issues.md Issue 23 for v2);
   --  only a round trip, since the length minimum (2 bytes) already covers the header
   procedure Oct_Ded_Emb_Round_Trip (Name : String) is
      S   : OctDedEmb_ACN_Stream := (Size_In_Bytes => OctDedEmb_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                     pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => 0));
      V   : OctDedEmb;
      D   : OctDedEmb;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      V.pad := 5;
      V.payload.hdr := 16#1234#;
      V.payload.elems.Length := 3;
      V.payload.elems.Data := (others => 7);
      OctDedEmb_ACN_Encode (V, S, Res);
      if Res.Success then
         OctDedEmb_ACN_Decode (D, S, Res);
      end if;
      Report (Name, True, Res, Res.Success and then Integer (D.payload.hdr) = 16#1234# and then D.payload.elems.Length = 3);
   exception
      when E : others => Report_Exception (Name, E);
   end Oct_Ded_Emb_Round_Trip;

begin
   --  valid streams
   Oct_Ext ("OctExt valid (len 3, n 2)", (16#00#, 16#03#, 16#02#, 16#11#, 16#22#), True, 2);
   Oct_Emb ("OctEmb valid (len 3, n 2)", (16#02#, 16#02#, 16#11#, 16#22#), True, 2);
   Oct_Ded ("OctDed valid (len 5, 2 items)", (16#00#, 16#05#, 16#12#, 16#34#, 16#00#, 16#10#, 16#02#), True, 2);
   Oct_Ded_Emb_Round_Trip ("OctDedEmb round trip (3 items)");

   --  the contents claim 200 data bytes, the region holds 1 byte: without the region bound the
   --  inner decoder copies 200 bytes from beyond its region
   Oct_Ext ("OctExt region 1 byte, n 200", (16#00#, 16#01#, 16#C8#), False);
   Oct_Ext ("OctExt region 2 bytes, n 200", (16#00#, 16#02#, 16#C8#, 16#00#), False);
   Oct_Ext ("OctExt region 0 bytes", (16#00#, 16#00#), False);
   Oct_Emb ("OctEmb region 1 byte, n 200", (16#00#, 16#C8#), False);
   Oct_Emb ("OctEmb region 2 bytes, n 200", (16#01#, 16#C8#, 16#00#), False);

   --  'size deduced' contents: the region is shorter than the 16-bit header
   Oct_Ded ("OctDed region 1 byte", (16#00#, 16#01#, 16#12#), False);
   Oct_Ded ("OctDed region 0 bytes", (16#00#, 16#00#), False);

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("ALL PASSED");
   else
      Ada.Text_IO.Put_Line ("FAILURES:" & Natural'Image (Failures));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Generated_Tests;
