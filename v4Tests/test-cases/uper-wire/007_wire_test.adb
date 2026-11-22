--  uPER checks of 007.asn1 in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, without and with
--  -gnata; Issue 151): the rows of 007_wire_test.c, whose C results are the reference, except the E rows (the C
--  decoder reads from a buffer of exactly the stream's length; the Ada reads take the zero padding of a larger
--  buffer). Each row builds one stream from runs of fields; the decoder must reject it with the given error code and
--  stop at the given bit position, or accept it, decode to the value the runs describe (Equal) and that value must
--  encode to the same bytes. When the last fragment of a fixed-size fragmented length holds a multiple of 16K items, a
--  final length octet 0 follows (X.691 10.9.3.8.3 NOTE). Before the fix the encoders wrote no final octet and the
--  decoders did not read it, and a header after a block of elements with their own decoder (QE, QN, QR) was rejected
--  with error code 0 (the element decoder had set it). With the first argument --exact-size every stream is decoded
--  from a buffer of exactly REQUIRED_BYTES (a longer stream is cut there; an optional argument after it, or the first
--  argument without --exact-size, runs only the rows whose name starts with it).
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Interfaces;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with uper_wire_fixed; use uper_wire_fixed;
procedure Wire_Test is
   use type Interfaces.Unsigned_8;
   Failed : Boolean := False;
   Rows   : Natural := 0;

   procedure Fail (Name, What : String; Got, Expected : Integer) is
   begin
      Ada.Text_IO.Put_Line
        ("FAILED " & Name & ": " & What & " (got" & Got'Image & ", expected" & Expected'Image & ")");
      Failed := True;
   end Fail;

   type Outcome is (Rejected, Accepted);

   --  N times the value V in N_Bits bits; Comp = 0: a length / header field, Comp = K > 0: N items of the K-th
   --  component (S80: the index in "A".."Z", QE / QR: the bits a (3) and b (1) of an Elm, QN: the index of the En
   --  value; W / WI: Comp 1 = the octets of a, Comp 2 = b)
   type Seg is record
      Comp   : Natural;
      V      : Natural;
      N_Bits : Positive;
      N      : Positive;
   end record;
   type Seg_Array is array (Positive range <>) of Seg;

   function H (X : Natural) return Seg is ((0, X, 8, 1));
   function L2 (N : Natural) return Seg is ((0, 16#8000# + N, 16, 1));
   function It (V : Natural; W, N : Positive) return Seg is ((1, V, W, N));
   function C (K, V : Natural; W, N : Positive) return Seg is ((K, V, W, N));

   type Counts is array (1 .. 2) of Natural;

   type Stream_Ptr is access adaasn1rtl.encoding.Bitstream;
   procedure Free is new Ada.Unchecked_Deallocation (adaasn1rtl.encoding.Bitstream, Stream_Ptr);

   generic
      type VT is private;
      with procedure Dec (val : in out VT; bs : in out adaasn1rtl.encoding.Bitstream; result : out ASN1_RESULT);
      with procedure Enc (val : VT; bs : in out adaasn1rtl.encoding.Bitstream; result : out ASN1_RESULT);
      with function Equal (val1, val2 : VT) return Boolean;
      --  the value the decoder starts from, the value the runs are added to
      with function Init return VT;
      with function Blank return VT;
      with procedure Add (V : in out VT; Comp : Positive; Idx : Positive; X : Natural);
      Req : Positive;
   procedure Check
     (Name : String; Expect : Outcome; Err : Integer; Pos : Natural; Segs : Seg_Array);

   Exact_Size : constant Boolean :=
     Ada.Command_Line.Argument_Count >= 1 and then Ada.Command_Line.Argument (1) = "--exact-size";
   Prefix_Arg : constant Natural := (if Exact_Size then 2 else 1);

   function Selected (Name : String) return Boolean is
     (Ada.Command_Line.Argument_Count < Prefix_Arg
      or else (Name'Length >= Ada.Command_Line.Argument (Prefix_Arg)'Length
               and then Name (Name'First .. Name'First + Ada.Command_Line.Argument (Prefix_Arg)'Length - 1)
                        = Ada.Command_Line.Argument (Prefix_Arg)));

   procedure Check
     (Name : String; Expect : Outcome; Err : Integer; Pos : Natural; Segs : Seg_Array)
   is
      type VP is access VT;
      procedure Free is new Ada.Unchecked_Deallocation (VT, VP);
      N_Bits  : Natural := 0;
      N_Bytes : Natural;
      Cap     : Positive;
      S, E    : Stream_Ptr;
      V, X    : VP;
      R       : ASN1_RESULT;
      At_Bit  : Natural := 0;
      Cnt     : Counts := (others => 0);
   begin
      if not Selected (Name) then
         return;
      end if;
      Rows := Rows + 1;
      for G of Segs loop
         N_Bits := N_Bits + G.N_Bits * G.N;
      end loop;
      N_Bytes := (N_Bits + 7) / 8;
      Cap := (if Exact_Size then Req else Positive'Max (N_Bytes, Req));
      S := new adaasn1rtl.encoding.Bitstream'(adaasn1rtl.encoding.BitStream_init (Cap));
      S.Buffer := (others => 0);
      for G of Segs loop
         for J in 1 .. G.N loop
            for B in reverse 0 .. G.N_Bits - 1 loop
               if At_Bit / 8 + 1 <= Cap and then (G.V / 2 ** B) mod 2 = 1 then
                  S.Buffer (At_Bit / 8 + 1) :=
                    S.Buffer (At_Bit / 8 + 1) or Asn1Byte (2 ** (7 - At_Bit mod 8));
               end if;
               At_Bit := At_Bit + 1;
            end loop;
         end loop;
      end loop;

      V := new VT'(Init);
      Dec (V.all, S.all, R);
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
         if N_Bits /= Pos then
            Fail (Name, "row: stream bits", N_Bits, Pos);
         end if;
         if not R.Success then
            Fail (Name, "rejected, error code", R.ErrorCode, 0);
         end if;
         if S.Current_Bit_Pos /= N_Bits then
            Fail (Name, "bit position after the decoder", S.Current_Bit_Pos, N_Bits);
         end if;
         --  the value the runs describe: Equal to the decoded one, and encoded byte for byte to the stream
         X := new VT'(Blank);
         for G of Segs loop
            if G.Comp > 0 then
               for J in 1 .. G.N loop
                  Cnt (G.Comp) := Cnt (G.Comp) + 1;
                  Add (X.all, G.Comp, Cnt (G.Comp), G.V);
               end loop;
            end if;
         end loop;
         if R.Success and then not Equal (X.all, V.all) then
            Fail (Name, "decoded value differs from the expected one", 0, 1);
         end if;
         E := new adaasn1rtl.encoding.Bitstream'(adaasn1rtl.encoding.BitStream_init (Req + 8));
         Enc (X.all, E.all, R);
         if not R.Success then
            Fail (Name, "encode failed, error code", R.ErrorCode, 0);
         end if;
         if E.Current_Bit_Pos /= N_Bits then
            Fail (Name, "encoded bits", E.Current_Bit_Pos, N_Bits);
         elsif E.Buffer (1 .. N_Bytes) /= S.Buffer (1 .. N_Bytes) then
            Fail (Name, "encoded bytes differ from the stream", 0, 1);
         end if;
         Free (X);
         Free (E);
      end if;
      Free (V);
      Free (S);
   exception
      when Ex : others =>
         Ada.Text_IO.Put_Line ("FAILED " & Name & ": " & Ada.Exceptions.Exception_Information (Ex));
         Failed := True;
   end Check;

   function Ch (X : Natural) return Character is (Character'Val (Character'Pos ('A') + X));

   procedure Add (V : in out F64; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := Asn1Byte (X);
   end Add;
   procedure Add (V : in out F80; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := Asn1Byte (X);
   end Add;
   procedure Add (V : in out F112; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := Asn1Byte (X);
   end Add;
   procedure Add (V : in out F128; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := Asn1Byte (X);
   end Add;
   procedure Add (V : in out F70; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := Asn1Byte (X);
   end Add;
   procedure Add (V : in out B64; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := BIT (X);
   end Add;
   procedure Add (V : in out B80; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := BIT (X);
   end Add;
   procedure Add (V : in out S80; Comp, Idx : Positive; X : Natural) is
   begin
      V (Idx) := Ch (X);
   end Add;
   procedure Add (V : in out Q80; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := Q80_elem (X);
   end Add;
   procedure Add (V : in out QE; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := (a => Elm_a (X / 2), b => X mod 2 = 1);
   end Add;
   procedure Add (V : in out QN; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := En'Val (X);
   end Add;
   procedure Add (V : in out QR; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := (a => Elm_a (X / 2), b => X mod 2 = 1);
   end Add;
   procedure Add (V : in out W; Comp, Idx : Positive; X : Natural) is
   begin
      if Comp = 1 then
         V.a.Data (Idx) := Asn1Byte (X);
      else
         V.b := W_b (X);
      end if;
   end Add;
   procedure Add (V : in out WI; Comp, Idx : Positive; X : Natural) is
   begin
      if Comp = 1 then
         V.a.Data (Idx) := Asn1Byte (X);
      else
         V.b := WI_b (X);
      end if;
   end Add;

   function S80_Blank return S80 is (others => NUL);

   procedure Check_F64 is new Check (F64, F64_Decode_aux, F64_Encode_aux, F64_Equal, F64_Init, F64_Init, Add,
                                     F64_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_F80 is new Check (F80, F80_Decode_aux, F80_Encode_aux, F80_Equal, F80_Init, F80_Init, Add,
                                     F80_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_F112 is new Check (F112, F112_Decode_aux, F112_Encode_aux, F112_Equal, F112_Init, F112_Init, Add,
                                      F112_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_F128 is new Check (F128, F128_Decode_aux, F128_Encode_aux, F128_Equal, F128_Init, F128_Init, Add,
                                      F128_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_F70 is new Check (F70, F70_Decode_aux, F70_Encode_aux, F70_Equal, F70_Init, F70_Init, Add,
                                     F70_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_B64 is new Check (B64, B64_Decode_aux, B64_Encode_aux, B64_Equal, B64_Init, B64_Init, Add,
                                     B64_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_B80 is new Check (B80, B80_Decode_aux, B80_Encode_aux, B80_Equal, B80_Init, B80_Init, Add,
                                     B80_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_S80 is new Check (S80, S80_Decode_aux, S80_Encode_aux, S80_Equal, S80_Init, S80_Blank, Add,
                                     S80_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_S80_ACN is new Check (S80, S80_ACN_Decode_aux, S80_ACN_Encode_aux, S80_Equal, S80_Init,
                                         S80_Blank, Add, S80_REQUIRED_BYTES_FOR_ACN_ENCODING);
   procedure Check_Q80 is new Check (Q80, Q80_Decode_aux, Q80_Encode_aux, Q80_Equal, Q80_Init, Q80_Init, Add,
                                     Q80_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_QE is new Check (QE, QE_Decode_aux, QE_Encode_aux, QE_Equal, QE_Init, QE_Init, Add,
                                    QE_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_QN is new Check (QN, QN_Decode_aux, QN_Encode_aux, QN_Equal, QN_Init, QN_Init, Add,
                                    QN_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_QR is new Check (QR, QR_Decode_aux, QR_Encode_aux, QR_Equal, QR_Init, QR_Init, Add,
                                    QR_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_W is new Check (W, W_Decode_aux, W_Encode_aux, W_Equal, W_Init, W_Init, Add,
                                   W_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_WI is new Check (WI, WI_Decode_aux, WI_Encode_aux, WI_Equal, WI_Init, WI_Init, Add,
                                    WI_REQUIRED_BYTES_FOR_ENCODING);

   --  the S80 rows that run through the uPER and the ACN decoder (ACN without a specification: the uPER layout)
   generic
      with procedure Chk (Name : String; Expect : Outcome; Err : Integer; Pos : Natural; Segs : Seg_Array);
   procedure S80_Rows (Err : Integer);
   procedure S80_Rows (Err : Integer) is
      Blocks : constant Seg_Array := (H (16#C4#), It (3, 5, 65536), H (16#C1#), It (25, 5, 16384));
   begin
      Chk ("V8 S80 C4 C1 00", Accepted, 0, 409624,
           (H (16#C4#), It (3, 5, 65536), H (16#C1#), It (25, 5, 16383), It (0, 5, 1), H (16#00#)));
      Chk ("Z8 S80 final octet 01", Rejected, Err, 409624, Blocks & H (16#01#));
      Chk ("Z8 S80 final octet 7F", Rejected, Err, 409624, Blocks & H (16#7F#));
      Chk ("Z8 S80 final octet 80", Rejected, Err, 409624, Blocks & H (16#80#));
      Chk ("Z8 S80 final octet C1", Rejected, Err, 409624, Blocks & H (16#C1#));
      Chk ("Z8 S80 final octet FF", Rejected, Err, 409624, Blocks & H (16#FF#));
   end S80_Rows;

   procedure S80_Uper_Rows is new S80_Rows (Check_S80);
   procedure S80_ACN_Rows is new S80_Rows (Check_S80_ACN);

   --  the final octet 01, 7F, 80, C1, FF after the blocks of a row (the 00 of a valid stream replaced)
   generic
      with procedure Chk (Name : String; Expect : Outcome; Err : Integer; Pos : Natural; Segs : Seg_Array);
   procedure Final_Rows (Nm : String; Err : Integer; Pos : Natural; Blocks : Seg_Array);
   procedure Final_Rows (Nm : String; Err : Integer; Pos : Natural; Blocks : Seg_Array) is
   begin
      Chk ("Z " & Nm & " final octet 01", Rejected, Err, Pos, Blocks & H (16#01#));
      Chk ("Z " & Nm & " final octet 7F", Rejected, Err, Pos, Blocks & H (16#7F#));
      Chk ("Z " & Nm & " final octet 80", Rejected, Err, Pos, Blocks & H (16#80#));
      Chk ("Z " & Nm & " final octet C1", Rejected, Err, Pos, Blocks & H (16#C1#));
      Chk ("Z " & Nm & " final octet FF", Rejected, Err, Pos, Blocks & H (16#FF#));
   end Final_Rows;

   procedure Final_F64 is new Final_Rows (Check_F64);
   procedure Final_F80 is new Final_Rows (Check_F80);
   procedure Final_F112 is new Final_Rows (Check_F112);
   procedure Final_F128 is new Final_Rows (Check_F128);
   procedure Final_B64 is new Final_Rows (Check_B64);
   procedure Final_B80 is new Final_Rows (Check_B80);
   procedure Final_Q80 is new Final_Rows (Check_Q80);
   procedure Final_QE is new Final_Rows (Check_QE);
   procedure Final_QN is new Final_Rows (Check_QN);
   procedure Final_W is new Final_Rows (Check_W);
   procedure Final_WI is new Final_Rows (Check_WI);
begin
   --  valid: every fixed size that is a multiple of 16K ends with a final 00 (F70 / QR: the length of a rest)
   Check_F64 ("V1 F64 C4 00", Accepted, 0, 524304,
              (H (16#C4#), It (16#5A#, 8, 65535), It (16#01#, 8, 1), H (16#00#)));
   Check_F80 ("V2 F80 C4 C1 00", Accepted, 0, 655384,
              (H (16#C4#), It (16#5A#, 8, 65536), H (16#C1#), It (16#02#, 8, 16384), H (16#00#)));
   Check_F112 ("V3 F112 C4 C3 00", Accepted, 0, 917528,
               (H (16#C4#), It (16#5A#, 8, 65536), H (16#C3#), It (16#3C#, 8, 49151), It (16#FF#, 8, 1),
                H (16#00#)));
   Check_F128 ("V4 F128 C4 C4 00", Accepted, 0, 1048600,
               (H (16#C4#), It (16#5A#, 8, 65536), H (16#C4#), It (16#A5#, 8, 65536), H (16#00#)));
   Check_F70 ("V5 F70 C4 + 4464 (no final octet)", Accepted, 0, 560024,
              (H (16#C4#), It (16#5A#, 8, 65536), L2 (4464), It (16#A5#, 8, 4464)));
   Check_B64 ("V6 B64 C4 00", Accepted, 0, 65552,
              (H (16#C4#), It (1, 1, 3), It (0, 1, 65532), It (1, 1, 1), H (16#00#)));
   Check_B80 ("V7 B80 C4 C1 00", Accepted, 0, 81944,
              (H (16#C4#), It (1, 1, 65536), H (16#C1#), It (0, 1, 16383), It (1, 1, 1), H (16#00#)));
   Check_Q80 ("V9 Q80 C4 C1 00", Accepted, 0, 245784,
              (H (16#C4#), It (5, 3, 65536), H (16#C1#), It (2, 3, 16384), H (16#00#)));
   Check_QE ("V10 QE C4 C4 C1 00", Accepted, 0, 589856,
             (H (16#C4#), It (9, 4, 65536), H (16#C4#), It (4, 4, 65536), H (16#C1#), It (11, 4, 16383),
              It (1, 4, 1), H (16#00#)));
   Check_QN ("V11 QN C4 C1 00", Accepted, 0, 163864,
             (H (16#C4#), It (2, 2, 65536), H (16#C1#), It (1, 2, 16384), H (16#00#)));
   Check_QR ("V12 QR C4 + 4464 (no final octet)", Accepted, 0, 280024,
             (H (16#C4#), It (7, 4, 65536), L2 (4464), It (2, 4, 4464)));
   Check_W ("V13 W a = C4 C1 00, b = 5A", Accepted, 0, 655392,
            (H (16#C4#), C (1, 16#5A#, 8, 65536), H (16#C1#), C (1, 16#02#, 8, 16384), H (16#00#),
             C (2, 16#5A#, 8, 1)));
   Check_WI ("V14 WI a = C4 C1 00, b = 5A", Accepted, 0, 655392,
             (H (16#C4#), C (1, 16#5A#, 8, 65536), H (16#C1#), C (1, 16#02#, 8, 16384), H (16#00#),
              C (2, 16#5A#, 8, 1)));
   S80_Uper_Rows (ERR_UPER_DECODE_S80);
   S80_ACN_Rows (ERR_ACN_DECODE_S80);

   --  a final octet other than 00: rejected with the type's error code, after the octet
   Final_F64 ("F64", ERR_UPER_DECODE_F64, 524304, (H (16#C4#), It (16#5A#, 8, 65536)));
   Final_F80 ("F80", ERR_UPER_DECODE_F80, 655384,
              (H (16#C4#), It (16#5A#, 8, 65536), H (16#C1#), It (16#02#, 8, 16384)));
   Final_F112 ("F112", ERR_UPER_DECODE_F112, 917528,
               (H (16#C4#), It (16#5A#, 8, 65536), H (16#C3#), It (16#3C#, 8, 49152)));
   Final_F128 ("F128", ERR_UPER_DECODE_F128, 1048600,
               (H (16#C4#), It (16#5A#, 8, 65536), H (16#C4#), It (16#A5#, 8, 65536)));
   Final_B64 ("B64", ERR_UPER_DECODE_B64, 65552, (H (16#C4#), It (1, 1, 65536)));
   Final_B80 ("B80", ERR_UPER_DECODE_B80, 81944, (H (16#C4#), It (1, 1, 65536), H (16#C1#), It (0, 1, 16384)));
   Final_Q80 ("Q80", ERR_UPER_DECODE_Q80, 245784, (H (16#C4#), It (5, 3, 65536), H (16#C1#), It (2, 3, 16384)));
   --  elements with their own decoder (the element decoder sets the error code to 0 when it succeeds)
   Final_QE ("QE", ERR_UPER_DECODE_QE, 589856,
             (H (16#C4#), It (9, 4, 65536), H (16#C4#), It (4, 4, 65536), H (16#C1#), It (11, 4, 16384)));
   Final_QN ("QN", ERR_UPER_DECODE_QN, 163864, (H (16#C4#), It (2, 2, 65536), H (16#C1#), It (1, 2, 16384)));
   --  a component: the error code of the referenced type (W) / of the inline component (WI)
   Final_W ("W", ERR_UPER_DECODE_F80, 655384,
            (H (16#C4#), C (1, 16#5A#, 8, 65536), H (16#C1#), C (1, 16#02#, 8, 16384)));
   Final_WI ("WI", ERR_UPER_DECODE_WI_A, 655384,
             (H (16#C4#), C (1, 16#5A#, 8, 65536), H (16#C1#), C (1, 16#02#, 8, 16384)));

   --  the layout without the final octet (asn1scc up to 4.9.161.0): the next field's octet is taken as the final
   --  length (before: accepted with b = 0, position 8 bits early)
   Check_W ("W1 W old layout, b = 5A", Rejected, ERR_UPER_DECODE_F80, 655384,
            (H (16#C4#), C (1, 16#5A#, 8, 65536), H (16#C1#), C (1, 16#02#, 8, 16384), C (2, 16#5A#, 8, 1)));
   Check_WI ("W2 WI old layout, b = 5A", Rejected, ERR_UPER_DECODE_WI_A, 655384,
             (H (16#C4#), C (1, 16#5A#, 8, 65536), H (16#C1#), C (1, 16#02#, 8, 16384), C (2, 16#5A#, 8, 1)));

   --  other headers (unchanged behaviour, pins the codes and positions): a wrong 64K header, a wrong small block
   --  header, a wrong length of the rest; QE / QN / QR after a block of elements with their own decoder (before the
   --  fix: error code 0)
   Check_F80 ("H1 F80 first header C3", Rejected, ERR_UPER_DECODE_F80, 8, (H (16#C3#), It (16#5A#, 8, 65536)));
   Check_F80 ("H2 F80 small block header C2", Rejected, ERR_UPER_DECODE_F80, 524304,
              (H (16#C4#), It (16#5A#, 8, 65536), H (16#C2#), It (16#02#, 8, 16384), H (16#00#)));
   Check_F80 ("H3 F80 small block header 00", Rejected, ERR_UPER_DECODE_F80, 524304,
              (H (16#C4#), It (16#5A#, 8, 65536), H (16#00#)));
   Check_F70 ("H4 F70 rest length 4463", Rejected, ERR_UPER_DECODE_F70, 524312,
              (H (16#C4#), It (16#5A#, 8, 65536), L2 (4463), It (16#A5#, 8, 4463)));
   Check_F70 ("H5 F70 one-octet rest length", Rejected, ERR_UPER_DECODE_F70, 524312,
              (H (16#C4#), It (16#5A#, 8, 65536), H (16#7F#), It (16#A5#, 8, 4464)));
   Check_QE ("H6 QE second 64K header C1", Rejected, ERR_UPER_DECODE_QE, 262160,
             (H (16#C4#), It (9, 4, 65536), H (16#C1#), It (4, 4, 65536)));
   Check_QE ("H7 QE small block header C2", Rejected, ERR_UPER_DECODE_QE, 524312,
             (H (16#C4#), It (9, 4, 65536), H (16#C4#), It (4, 4, 65536), H (16#C2#), It (11, 4, 16384),
              H (16#00#)));
   Check_QN ("H8 QN small block header C2", Rejected, ERR_UPER_DECODE_QN, 131088,
             (H (16#C4#), It (2, 2, 65536), H (16#C2#), It (1, 2, 16384), H (16#00#)));
   Check_QR ("H9 QR rest length 4465", Rejected, ERR_UPER_DECODE_QR, 262168,
             (H (16#C4#), It (7, 4, 65536), L2 (4465), It (2, 4, 4465)));
   --  an element that fails (Elm.a = 6, En = 3): the element's error code, before the final octet
   Check_QE ("H10 QE last element a = 6", Rejected, ERR_UPER_DECODE_ELM_A, 589847,
             (H (16#C4#), It (9, 4, 65536), H (16#C4#), It (4, 4, 65536), H (16#C1#), It (11, 4, 16383),
              It (12, 4, 1), H (16#00#)));
   Check_QN ("H11 QN last element 3", Rejected, ERR_UPER_DECODE_EN, 163856,
             (H (16#C4#), It (2, 2, 65536), H (16#C1#), It (1, 2, 16383), It (3, 2, 1), H (16#00#)));

   if Failed then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Ada.Text_IO.Put_Line ("uper-wire 007 (Ada):" & Rows'Image & " rows OK");
   end if;
end Wire_Test;
