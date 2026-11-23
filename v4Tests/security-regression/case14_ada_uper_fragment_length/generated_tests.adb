--  Ada uPER: the RTL decoders of a fragmented length (X.691 10.9.3.8), used by OCTET / BIT STRING (CONTAINING X) when
--  the contents may exceed 64K units. Every decode must either succeed with a valid value or fail with a non-zero
--  error code, without an exception (with run-time checks and with -gnatp); valid values must round-trip.
with Ada.Text_IO;
with Ada.Exceptions;
with adaasn1rtl;
with adaasn1rtl.encoding;
with TEST_CASE; use TEST_CASE;

procedure Generated_Tests is
   Failures : Natural := 0;

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

   --  Valid values: BIT STRING contents of 72014 bits = one 64K-bit fragment + 6478 bits (not a multiple of 8);
   --  OCTET STRING contents of 70003 octets = one 64K fragment + 4467 octets.
   procedure Round_Trip_BitBox is
      Name : constant String := "BitBox round trip (72014 bits)";
      S    : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (BitBox_REQUIRED_BYTES_FOR_ENCODING);
      V    : BitBox := BitBox_Init;
      W    : BitBox := BitBox_Init;
      Res  : adaasn1rtl.ASN1_RESULT;
   begin
      V.b.d.Length := 9000;
      for i in V.b.d.Data'Range loop
         V.b.d.Data (i) := adaasn1rtl.Asn1Byte (i mod 251);
      end loop;
      BitBox_Encode_aux (V, S, Res);
      if Res.Success then
         S.Current_Bit_Pos := 0;
         BitBox_Decode_aux (W, S, Res);
         if Res.Success and then W /= V then
            Res := (Success => False, ErrorCode => 0);
         end if;
      end if;
      Report (Name, True, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Round_Trip_BitBox;

   procedure Round_Trip_OctBox is
      Name : constant String := "OctBox round trip (70003 octets)";
      S    : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (OctBox_REQUIRED_BYTES_FOR_ENCODING);
      V    : OctBox := OctBox_Init;
      W    : OctBox := OctBox_Init;
      Res  : adaasn1rtl.ASN1_RESULT;
   begin
      V.o.d.Length := 70000;
      for i in V.o.d.Data'Range loop
         V.o.d.Data (i) := adaasn1rtl.Asn1Byte (i mod 251);
      end loop;
      OctBox_Encode_aux (V, S, Res);
      if Res.Success then
         S.Current_Bit_Pos := 0;
         OctBox_Decode_aux (W, S, Res);
         if Res.Success and then W /= V then
            Res := (Success => False, ErrorCode => 0);
         end if;
      end if;
      Report (Name, True, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Round_Trip_OctBox;

   --  Malformed lengths. Stream bytes: Headers (i) at the start of fragment i, Block (i) bytes of content after it,
   --  then Tail (one or two length octets).
   type Header_List is array (Positive range <>) of adaasn1rtl.Asn1Byte;
   type Size_List is array (Positive range <>) of Natural;

   procedure Fill (S : in out adaasn1rtl.encoding.Bitstream; Headers : Header_List; Blocks : Size_List;
                   Tail : Header_List) is
      Pos : Positive := S.Buffer'First;
   begin
      for k in Headers'Range loop
         S.Buffer (Pos) := Headers (k);
         Pos := Pos + 1 + Blocks (k);
      end loop;
      for k in Tail'Range loop
         S.Buffer (Pos) := Tail (k);
         Pos := Pos + 1;
      end loop;
   end Fill;

   procedure Decode_BitBox (Name : String; Headers : Header_List; Blocks : Size_List; Tail : Header_List) is
      S   : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (40_000);
      V   : BitBox := BitBox_Init;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      Fill (S, Headers, Blocks, Tail);
      BitBox_Decode_aux (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_BitBox;

   procedure Decode_OctBox (Name : String; Headers : Header_List; Blocks : Size_List; Tail : Header_List) is
      S   : adaasn1rtl.encoding.Bitstream := adaasn1rtl.encoding.BitStream_init (200_000);
      V   : OctBox := OctBox_Init;
      Res : adaasn1rtl.ASN1_RESULT;
   begin
      Fill (S, Headers, Blocks, Tail);
      OctBox_Decode_aux (V, S, Res);
      Report (Name, False, Res);
   exception
      when E : others => Report_Exception (Name, E);
   end Decode_OctBox;

begin
   Round_Trip_BitBox;
   Round_Trip_OctBox;
   --  BitBox: the contents hold at most 72014 bits (9002 octets)
   Decode_BitBox ("BitBox, two 64K-bit fragments (above the maximum)", (16#C4#, 16#C4#), (8192, 8192), (1 => 0));
   Decode_BitBox ("BitBox, 64K-bit fragment + 16383 bits (above the maximum)", (1 => 16#C4#), (1 => 8192),
                  (16#BF#, 16#FF#));
   Decode_BitBox ("BitBox, header C5", (1 .. 0 => 0), (1 .. 0 => 0), (1 => 16#C5#));
   Decode_BitBox ("BitBox, header C0", (1 .. 0 => 0), (1 .. 0 => 0), (1 => 16#C0#));
   --  OctBox: the contents hold at most 70003 octets
   Decode_OctBox ("OctBox, 64K + 16383 octets (above the maximum)", (1 => 16#C4#), (1 => 65536), (16#BF#, 16#FF#));
   Decode_OctBox ("OctBox, two 64K fragments (above the maximum)", (16#C4#, 16#C4#), (65536, 65536), (1 => 0));
   Decode_OctBox ("OctBox, header C5", (1 .. 0 => 0), (1 .. 0 => 0), (1 => 16#C5#));

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("ALL PASS");
   else
      Ada.Text_IO.Put_Line ("FAILURES:" & Natural'Image (Failures));
      raise Program_Error;
   end if;
end Generated_Tests;
