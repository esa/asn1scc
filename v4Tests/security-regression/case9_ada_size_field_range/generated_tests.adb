--  q/issues.md Issue 64: decodes streams whose size field (ACN `[size len]`) holds a value outside
--  0 .. Integer'Last: a negative twos-complement value or a pos-int value above Integer'Last. Every such
--  stream must fail with a non-zero error code and without an exception (with run-time checks and with
--  -gnatp); the encoding of each type's initial value must decode.
with Ada.Text_IO;
with Ada.Exceptions;
with adaasn1rtl;
with TEST_CASE; use TEST_CASE;

procedure Generated_Tests is
   Failures : Natural := 0;
   Fill     : constant adaasn1rtl.Asn1Byte := 16#AB#;

   procedure Report (Name : String; Expect_Ok : Boolean; Res : adaasn1rtl.ASN1_RESULT) is
      Ok : constant Boolean :=
        (if Expect_Ok then Res.Success else not Res.Success and then Res.ErrorCode /= 0);
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


   --  COctT16: the size field, then the field it sizes
   procedure Decode_COctT16 (Name : String; Bytes : adaasn1rtl.OctetBuffer) is
      S   : COctT16_ACN_Stream := (Size_In_Bytes => COctT16_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : COctT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      COctT16_ACN_Decode (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_COctT16;

   procedure Round_Trip_COctT16 is
      S   : COctT16_ACN_Stream;
      V   : COctT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      COctT16_ACN_Encode (COctT16_Init, S, Res);
      if Res.Success then
         S.Current_Bit_Pos := 0;
         COctT16_ACN_Decode (V, S, Res);
      end if;
      Report ("COctT16 initial value round trip", True, Res);
   exception
      when E : others => Report_Exception ("COctT16 initial value round trip", E);
   end Round_Trip_COctT16;


   --  COctP32: the size field, then the field it sizes
   procedure Decode_COctP32 (Name : String; Bytes : adaasn1rtl.OctetBuffer) is
      S   : COctP32_ACN_Stream := (Size_In_Bytes => COctP32_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : COctP32;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      COctP32_ACN_Decode (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_COctP32;

   procedure Round_Trip_COctP32 is
      S   : COctP32_ACN_Stream;
      V   : COctP32;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      COctP32_ACN_Encode (COctP32_Init, S, Res);
      if Res.Success then
         S.Current_Bit_Pos := 0;
         COctP32_ACN_Decode (V, S, Res);
      end if;
      Report ("COctP32 initial value round trip", True, Res);
   exception
      when E : others => Report_Exception ("COctP32 initial value round trip", E);
   end Round_Trip_COctP32;


   --  CBitT16: the size field, then the field it sizes
   procedure Decode_CBitT16 (Name : String; Bytes : adaasn1rtl.OctetBuffer) is
      S   : CBitT16_ACN_Stream := (Size_In_Bytes => CBitT16_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : CBitT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      CBitT16_ACN_Decode (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_CBitT16;

   procedure Round_Trip_CBitT16 is
      S   : CBitT16_ACN_Stream;
      V   : CBitT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      CBitT16_ACN_Encode (CBitT16_Init, S, Res);
      if Res.Success then
         S.Current_Bit_Pos := 0;
         CBitT16_ACN_Decode (V, S, Res);
      end if;
      Report ("CBitT16 initial value round trip", True, Res);
   exception
      when E : others => Report_Exception ("CBitT16 initial value round trip", E);
   end Round_Trip_CBitT16;


   --  CBitP32: the size field, then the field it sizes
   procedure Decode_CBitP32 (Name : String; Bytes : adaasn1rtl.OctetBuffer) is
      S   : CBitP32_ACN_Stream := (Size_In_Bytes => CBitP32_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : CBitP32;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      CBitP32_ACN_Decode (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_CBitP32;

   procedure Round_Trip_CBitP32 is
      S   : CBitP32_ACN_Stream;
      V   : CBitP32;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      CBitP32_ACN_Encode (CBitP32_Init, S, Res);
      if Res.Success then
         S.Current_Bit_Pos := 0;
         CBitP32_ACN_Decode (V, S, Res);
      end if;
      Report ("CBitP32 initial value round trip", True, Res);
   exception
      when E : others => Report_Exception ("CBitP32 initial value round trip", E);
   end Round_Trip_CBitP32;


   --  DedT16: the size field, then the field it sizes
   procedure Decode_DedT16 (Name : String; Bytes : adaasn1rtl.OctetBuffer) is
      S   : DedT16_ACN_Stream := (Size_In_Bytes => DedT16_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : DedT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      DedT16_ACN_Decode (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_DedT16;

   procedure Round_Trip_DedT16 is
      S   : DedT16_ACN_Stream;
      V   : DedT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      DedT16_ACN_Encode (DedT16_Init, S, Res);
      if Res.Success then
         S.Current_Bit_Pos := 0;
         DedT16_ACN_Decode (V, S, Res);
      end if;
      Report ("DedT16 initial value round trip", True, Res);
   exception
      when E : others => Report_Exception ("DedT16 initial value round trip", E);
   end Round_Trip_DedT16;


   --  DedP32: the size field, then the field it sizes
   procedure Decode_DedP32 (Name : String; Bytes : adaasn1rtl.OctetBuffer) is
      S   : DedP32_ACN_Stream := (Size_In_Bytes => DedP32_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : DedP32;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      DedP32_ACN_Decode (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_DedP32;

   procedure Round_Trip_DedP32 is
      S   : DedP32_ACN_Stream;
      V   : DedP32;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      DedP32_ACN_Encode (DedP32_Init, S, Res);
      if Res.Success then
         S.Current_Bit_Pos := 0;
         DedP32_ACN_Decode (V, S, Res);
      end if;
      Report ("DedP32 initial value round trip", True, Res);
   exception
      when E : others => Report_Exception ("DedP32 initial value round trip", E);
   end Round_Trip_DedP32;


   --  OctT16: the size field, then the field it sizes
   procedure Decode_OctT16 (Name : String; Bytes : adaasn1rtl.OctetBuffer) is
      S   : OctT16_ACN_Stream := (Size_In_Bytes => OctT16_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : OctT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      OctT16_ACN_Decode (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_OctT16;

   procedure Round_Trip_OctT16 is
      S   : OctT16_ACN_Stream;
      V   : OctT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      OctT16_ACN_Encode (OctT16_Init, S, Res);
      if Res.Success then
         S.Current_Bit_Pos := 0;
         OctT16_ACN_Decode (V, S, Res);
      end if;
      Report ("OctT16 initial value round trip", True, Res);
   exception
      when E : others => Report_Exception ("OctT16 initial value round trip", E);
   end Round_Trip_OctT16;


   --  BitT16: the size field, then the field it sizes
   procedure Decode_BitT16 (Name : String; Bytes : adaasn1rtl.OctetBuffer) is
      S   : BitT16_ACN_Stream := (Size_In_Bytes => BitT16_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : BitT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      BitT16_ACN_Decode (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_BitT16;

   procedure Round_Trip_BitT16 is
      S   : BitT16_ACN_Stream;
      V   : BitT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      BitT16_ACN_Encode (BitT16_Init, S, Res);
      if Res.Success then
         S.Current_Bit_Pos := 0;
         BitT16_ACN_Decode (V, S, Res);
      end if;
      Report ("BitT16 initial value round trip", True, Res);
   exception
      when E : others => Report_Exception ("BitT16 initial value round trip", E);
   end Round_Trip_BitT16;


   --  SqOfT16: the size field, then the field it sizes
   procedure Decode_SqOfT16 (Name : String; Bytes : adaasn1rtl.OctetBuffer) is
      S   : SqOfT16_ACN_Stream := (Size_In_Bytes => SqOfT16_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : SqOfT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      SqOfT16_ACN_Decode (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_SqOfT16;

   procedure Round_Trip_SqOfT16 is
      S   : SqOfT16_ACN_Stream;
      V   : SqOfT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      SqOfT16_ACN_Encode (SqOfT16_Init, S, Res);
      if Res.Success then
         S.Current_Bit_Pos := 0;
         SqOfT16_ACN_Decode (V, S, Res);
      end if;
      Report ("SqOfT16 initial value round trip", True, Res);
   exception
      when E : others => Report_Exception ("SqOfT16 initial value round trip", E);
   end Round_Trip_SqOfT16;


   --  StrT16: the size field, then the field it sizes
   procedure Decode_StrT16 (Name : String; Bytes : adaasn1rtl.OctetBuffer) is
      S   : StrT16_ACN_Stream := (Size_In_Bytes => StrT16_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : StrT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      StrT16_ACN_Decode (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_StrT16;

   procedure Round_Trip_StrT16 is
      S   : StrT16_ACN_Stream;
      V   : StrT16;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      StrT16_ACN_Encode (StrT16_Init, S, Res);
      if Res.Success then
         S.Current_Bit_Pos := 0;
         StrT16_ACN_Decode (V, S, Res);
      end if;
      Report ("StrT16 initial value round trip", True, Res);
   exception
      when E : others => Report_Exception ("StrT16 initial value round trip", E);
   end Round_Trip_StrT16;


   --  StrP64: the size field, then the field it sizes
   procedure Decode_StrP64 (Name : String; Bytes : adaasn1rtl.OctetBuffer) is
      S   : StrP64_ACN_Stream := (Size_In_Bytes => StrP64_ACN_STREAM_SIZE, Current_Bit_Pos => 0,
                                  pushDataPrm => 0, fetchDataPrm => 0, Buffer => (others => Fill));
      V   : StrP64;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      S.Buffer (1 .. Bytes'Length) := Bytes;
      StrP64_ACN_Decode (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_StrP64;

   procedure Round_Trip_StrP64 is
      S   : StrP64_ACN_Stream;
      V   : StrP64;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      StrP64_ACN_Encode (StrP64_Init, S, Res);
      if Res.Success then
         S.Current_Bit_Pos := 0;
         StrP64_ACN_Decode (V, S, Res);
      end if;
      Report ("StrP64 initial value round trip", True, Res);
   exception
      when E : others => Report_Exception ("StrP64 initial value round trip", E);
   end Round_Trip_StrP64;

begin
   Round_Trip_COctT16;
   Decode_COctT16 ("COctT16 size field = -16", (16#FF#, 16#F0#));
   Decode_COctT16 ("COctT16 size field = -32768", (16#80#, 16#00#));
   Round_Trip_COctP32;
   Decode_COctP32 ("COctP32 size field = 16#FFFF_FFF0#", (16#FF#, 16#FF#, 16#FF#, 16#F0#));
   Decode_COctP32 ("COctP32 size field = 2**31", (16#80#, 16#00#, 16#00#, 16#00#));
   Round_Trip_CBitT16;
   Decode_CBitT16 ("CBitT16 size field = -16", (16#FF#, 16#F0#));
   Decode_CBitT16 ("CBitT16 size field = -32768", (16#80#, 16#00#));
   Round_Trip_CBitP32;
   Decode_CBitP32 ("CBitP32 size field = 16#FFFF_FFF0#", (16#FF#, 16#FF#, 16#FF#, 16#F0#));
   Decode_CBitP32 ("CBitP32 size field = 2**31", (16#80#, 16#00#, 16#00#, 16#00#));
   Round_Trip_DedT16;
   Decode_DedT16 ("DedT16 size field = -16", (16#FF#, 16#F0#));
   Decode_DedT16 ("DedT16 size field = -32768", (16#80#, 16#00#));
   Round_Trip_DedP32;
   Decode_DedP32 ("DedP32 size field = 16#FFFF_FFF0#", (16#FF#, 16#FF#, 16#FF#, 16#F0#));
   Decode_DedP32 ("DedP32 size field = 2**31", (16#80#, 16#00#, 16#00#, 16#00#));
   Round_Trip_OctT16;
   Decode_OctT16 ("OctT16 size field = -16", (16#FF#, 16#F0#));
   Decode_OctT16 ("OctT16 size field = -32768", (16#80#, 16#00#));
   Round_Trip_BitT16;
   Decode_BitT16 ("BitT16 size field = -16", (16#FF#, 16#F0#));
   Decode_BitT16 ("BitT16 size field = -32768", (16#80#, 16#00#));
   Round_Trip_SqOfT16;
   Decode_SqOfT16 ("SqOfT16 size field = -16", (16#FF#, 16#F0#));
   Decode_SqOfT16 ("SqOfT16 size field = -32768", (16#80#, 16#00#));
   Round_Trip_StrT16;
   Decode_StrT16 ("StrT16 size field = -16", (16#FF#, 16#F0#));
   Decode_StrT16 ("StrT16 size field = -32768", (16#80#, 16#00#));
   Round_Trip_StrP64;
   Decode_StrP64 ("StrP64 size field = 16#FFFF_FFFF_FFFF_FFF0#", (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#F0#));
   Decode_StrP64 ("StrP64 size field = 2**63", (16#80#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#));
   Decode_StrP64 ("StrP64 size field = 2**31", (16#00#, 16#00#, 16#00#, 16#00#, 16#80#, 16#00#, 16#00#, 16#00#));
   if Failures > 0 then
      Ada.Text_IO.Put_Line (Natural'Image (Failures) & " FAILED");
      raise Program_Error with "case 9 failed";
   end if;
   Ada.Text_IO.Put_Line ("all passed");
end Generated_Tests;
