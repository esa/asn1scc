--  uPER checks of 004.asn1 in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, without and with
--  -gnata, and with -gnatp; Issue 152): the rows of 004_wire_test.c, whose C results are the reference. Each row builds
--  one stream from runs of fields; the decoder must reject it with the given error code and stop at the given bit
--  position, or accept it, decode to the value the runs describe (Equal) and that value must encode to the same bytes.
--  Rows G1 / P1 also check that a rejected value leaves the octets after the value and the next component untouched:
--  under -gnatp an index check cannot stop a write past the maximum size, and such a write may stay inside the
--  allocation, where a sanitizer does not see it. Before the fix the decoders read every block without checking it
--  against the maximum (Constraint_Error, or a write past the value under -gnatp), took any header from C0 on as a
--  length, skipped a final part over the maximum and returned Success, and lost the failure of an item.
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Interfaces;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with uper_wire_frag; use uper_wire_frag;
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

   --  N times the value V in N_Bits bits; Comp = 0: a length / header field, Comp = K > 0: N items of the K-th
   --  variable-size component (the value: VStr / PInl.s the index in "A".."Z", VSeqR the bits a (3) and b (1))
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

   type Counts is array (1 .. 4) of Natural;

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
      --  what a rejected decode must leave as it was (guards, the next component)
      with function Kept (V : VT) return Boolean;
      Req : Positive;
   procedure Check
     (Name : String; Expect : Outcome; Err : Integer; Pos : Natural; Segs : Seg_Array);

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
      for G of Segs loop
         N_Bits := N_Bits + G.N_Bits * G.N;
      end loop;
      N_Bytes := (N_Bits + 7) / 8;
      Cap := Positive'Max (N_Bytes, Req);
      S := new adaasn1rtl.encoding.Bitstream'(adaasn1rtl.encoding.BitStream_init (Cap));
      S.Buffer := (others => 0);
      for G of Segs loop
         for J in 1 .. G.N loop
            for B in reverse 0 .. G.N_Bits - 1 loop
               if (G.V / 2 ** B) mod 2 = 1 then
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
         if not Kept (V.all) then
            Fail (Name, "octets after the value / next component changed", 1, 0);
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

   function Always (V : VOct) return Boolean is (True);
   function Always (V : VStr) return Boolean is (True);
   function Always (V : VBit) return Boolean is (True);
   function Always (V : VSeq) return Boolean is (True);
   function Always (V : VOctM) return Boolean is (True);
   function Always (V : VBit3) return Boolean is (True);
   function Always (V : VSeqR) return Boolean is (True);
   function Always (V : PInl) return Boolean is (True);

   function Ch (X : Natural) return Character is (Character'Val (Character'Pos ('A') + X));

   procedure Add (V : in out VOct; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := Asn1Byte (X);
      V.Length := Idx;
   end Add;
   procedure Add (V : in out VStr; Comp, Idx : Positive; X : Natural) is
   begin
      V (Idx) := Ch (X);
   end Add;
   procedure Add (V : in out VBit; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := BIT (X);
      V.Length := Idx;
   end Add;
   procedure Add (V : in out VSeq; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := VSeq_elem (X);
      V.Length := Idx;
   end Add;
   procedure Add (V : in out VOctM; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := Asn1Byte (X);
      V.Length := Idx;
   end Add;
   procedure Add (V : in out VBit3; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := BIT (X);
      if Idx >= 3 then  --  SIZE min 3
         V.Length := Idx;
      end if;
   end Add;
   procedure Add (V : in out VSeqR; Comp, Idx : Positive; X : Natural) is
   begin
      V.Data (Idx) := (a => Elm_a (X / 2), b => X mod 2 = 1);
      V.Length := Idx;
   end Add;
   procedure Add (V : in out POct; Comp, Idx : Positive; X : Natural) is
   begin
      if Comp = 1 then
         V.o.Data (Idx) := Asn1Byte (X);
         V.o.Length := Idx;
      else
         V.tail.Data (Idx) := Asn1Byte (X);
      end if;
   end Add;
   procedure Add (V : in out PInl; Comp, Idx : Positive; X : Natural) is
   begin
      case Comp is
         when 1 =>
            V.o.Data (Idx) := Asn1Byte (X);
            V.o.Length := Idx;
         when 2 =>
            V.s (Idx) := Ch (X);
         when 3 =>
            V.q.Data (Idx) := PInl_q_elem (X);
            V.q.Length := Idx;
         when others =>
            V.tail := PInl_tail (X);
      end case;
   end Add;

   function VStr_Blank return VStr is (others => NUL);
   function PInl_Blank return PInl is
     ((o => (Length => 0, Data => (others => 0)), s => (others => NUL), q => PInl_q_Init, tail => 0));

   --  POct: the tail starts filled with Tail_Fill and a rejected decode must leave it so
   Tail_Fill : constant Asn1Byte := 16#C3#;
   function POct_Filled return POct is ((o => (Length => 0, Data => (others => 0)), tail => (Data => (others => Tail_Fill))));
   function Tail_Kept (V : POct) return Boolean is (for all B of V.tail.Data => B = Tail_Fill);

   --  VOct between two guard regions in one allocation
   Guard_Fill : constant Asn1Byte := 16#A5#;
   type Guard_Region is array (1 .. 4096) of Asn1Byte;
   type Guarded_VOct is record
      G1 : Guard_Region;
      V  : VOct;
      G2 : Guard_Region;
   end record;
   function Guarded_Init return Guarded_VOct is
     ((G1 => (others => Guard_Fill), V => VOct_Init, G2 => (others => Guard_Fill)));
   function Guards_Kept (X : Guarded_VOct) return Boolean is
     ((for all B of X.G1 => B = Guard_Fill) and (for all B of X.G2 => B = Guard_Fill));
   procedure Guarded_Dec (val : in out Guarded_VOct; bs : in out adaasn1rtl.encoding.Bitstream;
                          result : out ASN1_RESULT) is
   begin
      VOct_Decode_aux (val.V, bs, result);
   end Guarded_Dec;
   procedure Guarded_Enc (val : Guarded_VOct; bs : in out adaasn1rtl.encoding.Bitstream;
                          result : out ASN1_RESULT) is
   begin
      VOct_Encode_aux (val.V, bs, result);
   end Guarded_Enc;
   function Guarded_Equal (val1, val2 : Guarded_VOct) return Boolean is (VOct_Equal (val1.V, val2.V));
   procedure Guarded_Add (V : in out Guarded_VOct; Comp, Idx : Positive; X : Natural) is
   begin
      Add (V.V, Comp, Idx, X);
   end Guarded_Add;

   procedure Check_VOct is new Check (VOct, VOct_Decode_aux, VOct_Encode_aux, VOct_Equal, VOct_Init, VOct_Init, Add,
                                      Always, VOct_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_VStr is new Check (VStr, VStr_Decode_aux, VStr_Encode_aux, VStr_Equal, VStr_Init, VStr_Blank, Add,
                                      Always, VStr_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_VStr_ACN is new Check (VStr, VStr_ACN_Decode_aux, VStr_ACN_Encode_aux, VStr_Equal, VStr_Init,
                                          VStr_Blank, Add, Always, VStr_REQUIRED_BYTES_FOR_ACN_ENCODING);
   procedure Check_VBit is new Check (VBit, VBit_Decode_aux, VBit_Encode_aux, VBit_Equal, VBit_Init, VBit_Init, Add,
                                      Always, VBit_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_VSeq is new Check (VSeq, VSeq_Decode_aux, VSeq_Encode_aux, VSeq_Equal, VSeq_Init, VSeq_Init, Add,
                                      Always, VSeq_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_VOctM is new Check (VOctM, VOctM_Decode_aux, VOctM_Encode_aux, VOctM_Equal, VOctM_Init,
                                       VOctM_Init, Add, Always, VOctM_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_VBit3 is new Check (VBit3, VBit3_Decode_aux, VBit3_Encode_aux, VBit3_Equal, VBit3_Init,
                                       VBit3_Init, Add, Always, VBit3_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_VSeqR is new Check (VSeqR, VSeqR_Decode_aux, VSeqR_Encode_aux, VSeqR_Equal, VSeqR_Init,
                                       VSeqR_Init, Add, Always, VSeqR_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_POct is new Check (POct, POct_Decode_aux, POct_Encode_aux, POct_Equal, POct_Filled, POct_Init,
                                      Add, Tail_Kept, POct_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_PInl is new Check (PInl, PInl_Decode_aux, PInl_Encode_aux, PInl_Equal, PInl_Init, PInl_Blank,
                                      Add, Always, PInl_REQUIRED_BYTES_FOR_ENCODING);
   procedure Check_Guarded is new Check (Guarded_VOct, Guarded_Dec, Guarded_Enc, Guarded_Equal, Guarded_Init,
                                         Guarded_Init, Guarded_Add, Guards_Kept, VOct_REQUIRED_BYTES_FOR_ENCODING);

   --  the VStr rows that run through the uPER and the ACN decoder (ACN without a specification: the uPER layout)
   generic
      with procedure Chk (Name : String; Expect : Outcome; Err : Integer; Pos : Natural; Segs : Seg_Array);
   procedure VStr_Rows (Err : Integer);
   procedure VStr_Rows (Err : Integer) is
   begin
      --  an item that fails: the first item of a final part, of a block, the last item of a final part after a
      --  block, the last item of a block, an item inside a final part after a block (indexes 26 .. 31 are not in
      --  "A".."Z")
      Chk ("R1 VStr length 2, first item fails", Rejected, Err, 13, (H (16#02#), It (31, 5, 1), It (0, 5, 1)));
      Chk ("R2 VStr C1, first item fails", Rejected, Err, 13,
           (H (16#C1#), It (31, 5, 1), It (0, 5, 16383), H (16#00#)));
      Chk ("R3 VStr C1, last item of the final part fails", Rejected, Err, 81941,
           (H (16#C1#), It (0, 5, 16384), H (16#01#), It (31, 5, 1)));
      Chk ("R3b VStr C4, last item of the block fails", Rejected, Err, 327688,
           (H (16#C4#), It (0, 5, 65535), It (26, 5, 1), H (16#00#)));
      Chk ("R3c VStr C4, item 100 of the final part fails", Rejected, Err, 328204,
           (H (16#C4#), It (0, 5, 65536), L2 (4464), It (0, 5, 99), It (27, 5, 1), It (0, 5, 4364)));
      --  a block whose items pass the maximum 70000, a final part that passes it
      Chk ("R4 VStr C4 C1: 81920 items", Rejected, Err, 327696,
           (H (16#C4#), It (0, 5, 65536), H (16#C1#), It (0, 5, 16384), H (16#00#)));
      Chk ("R5 VStr C4 + 8191: 73727 items", Rejected, Err, 327704,
           (H (16#C4#), It (0, 5, 65536), L2 (8191), It (0, 5, 8191)));
      --  headers that are no length and no C1 .. C4
      Chk ("R6 VStr header C0", Rejected, Err, 8, (H (16#C0#), It (0, 8, 8)));
      Chk ("R6 VStr header C5", Rejected, Err, 8, (H (16#C5#), It (0, 8, 8)));
      Chk ("R6 VStr header FF", Rejected, Err, 8, (H (16#FF#), It (0, 8, 8)));
      Chk ("R7 VStr C4 then header C0", Rejected, Err, 327696, (H (16#C4#), It (0, 5, 65536), H (16#C0#)));
      Chk ("R8 VStr length 0 (SIZE min 1)", Rejected, Err, 8, (1 => H (16#00#)));
      --  valid: a block and a two-octet final length, a block and a one-octet final length, a block and an empty
      --  final part (SIZE min 1)
      Chk ("A1 VStr 65536 B + 4464 C", Accepted, 0, 350024,
           (H (16#C4#), It (1, 5, 65536), L2 (4464), It (2, 5, 4464)));
      Chk ("A2 VStr 16384 Z + 3 A", Accepted, 0, 81951, (H (16#C1#), It (25, 5, 16384), H (16#03#), It (0, 5, 3)));
      Chk ("A2b VStr 65536 D, empty final part", Accepted, 0, 327696, (H (16#C4#), It (3, 5, 65536), H (16#00#)));
   end VStr_Rows;

   procedure VStr_Uper_Rows is new VStr_Rows (Check_VStr);
   procedure VStr_ACN_Rows is new VStr_Rows (Check_VStr_ACN);
begin
   VStr_Uper_Rows (ERR_UPER_DECODE_VSTR);
   VStr_ACN_Rows (ERR_ACN_DECODE_VSTR);

   --  OCTET STRING (SIZE(0..70000))
   Check_VOct ("R9 VOct C4 C2: 98304 items", Rejected, ERR_UPER_DECODE_VOCT, 524304,
               (H (16#C4#), It (16#5A#, 8, 65536), H (16#C2#), It (16#5A#, 8, 32768), H (16#00#)));
   Check_VOct ("R10 VOct C4 + 4465: 70001 items", Rejected, ERR_UPER_DECODE_VOCT, 524312,
               (H (16#C4#), It (16#5A#, 8, 65536), L2 (4465), It (16#5A#, 8, 4465)));
   Check_VOct ("R11 VOct C4 then header C5", Rejected, ERR_UPER_DECODE_VOCT, 524304,
               (H (16#C4#), It (16#5A#, 8, 65536), H (16#C5#)));
   Check_VOct ("A3 VOct 70000", Accepted, 0, 560024,
               (H (16#C4#), It (16#5A#, 8, 65536), L2 (4464), It (16#A5#, 8, 4464)));
   Check_VOct ("A4 VOct 16384", Accepted, 0, 131088, (H (16#C1#), It (16#11#, 8, 16384), H (16#00#)));
   Check_VOct ("A4b VOct empty", Accepted, 0, 8, (1 => H (16#00#)));
   --  the R9 stream into a VOct between two guard regions
   Check_Guarded ("G1 VOct C4 C2 between guards", Rejected, ERR_UPER_DECODE_VOCT, 524304,
                  (H (16#C4#), It (16#5A#, 8, 65536), H (16#C2#), It (16#5A#, 8, 32768), H (16#00#)));
   Check_Guarded ("G2 VOct 70000 between guards", Accepted, 0, 560024,
                  (H (16#C4#), It (16#5A#, 8, 65536), L2 (4464), It (16#A5#, 8, 4464)));

   --  OCTET STRING (SIZE(0..81920)): blocks that end exactly at the maximum
   Check_VOctM ("M1 VOctM C4 C1 00: 81920", Accepted, 0, 655384,
                (H (16#C4#), It (16#5A#, 8, 65536), H (16#C1#), It (16#02#, 8, 16384), H (16#00#)));
   Check_VOctM ("M2 VOctM C4 C1 + 1: 81921", Rejected, ERR_UPER_DECODE_VOCTM, 655384,
                (H (16#C4#), It (16#5A#, 8, 65536), H (16#C1#), It (16#02#, 8, 16384), H (16#01#),
                 It (16#09#, 8, 1)));
   Check_VOctM ("M3 VOctM C4 C2: 98304", Rejected, ERR_UPER_DECODE_VOCTM, 524304,
                (H (16#C4#), It (16#5A#, 8, 65536), H (16#C2#), It (16#02#, 8, 32768)));
   Check_VOctM ("M4 VOctM C4 + 16383", Accepted, 0, 655376,
                (H (16#C4#), It (16#5A#, 8, 65536), L2 (16383), It (16#07#, 8, 16383)));

   --  BIT STRING (SIZE(0..70000))
   Check_VBit ("R12 VBit C4 C1: 81920 bits", Rejected, ERR_UPER_DECODE_VBIT, 65552,
               (H (16#C4#), It (1, 1, 65536), H (16#C1#), It (1, 1, 16384), H (16#00#)));
   Check_VBit ("R13 VBit C4 + 4465: 70001 bits", Rejected, ERR_UPER_DECODE_VBIT, 65560,
               (H (16#C4#), It (1, 1, 65536), L2 (4465), It (1, 1, 4465)));
   Check_VBit ("R14 VBit header C0", Rejected, ERR_UPER_DECODE_VBIT, 8, (1 => H (16#C0#)));
   Check_VBit ("A5 VBit 70000", Accepted, 0, 70024,
               (H (16#C4#), It (1, 1, 65536), L2 (4464), It (0, 1, 4463), It (1, 1, 1)));
   Check_VBit ("A5b VBit 16387 (not a multiple of 8)", Accepted, 0, 16403,
               (H (16#C1#), It (1, 1, 16384), H (16#03#), It (1, 1, 1), It (0, 1, 1), It (1, 1, 1)));

   --  BIT STRING (SIZE(3..200000)): three blocks
   Check_VBit3 ("B1 VBit3 C4 C4 C4 + 3392: 200000", Accepted, 0, 200040,
                (H (16#C4#), It (1, 1, 65536), H (16#C4#), It (0, 1, 65536), H (16#C4#), It (1, 1, 65536),
                 L2 (3392), It (1, 1, 3392)));
   Check_VBit3 ("B2 VBit3 C4 C4 C4 + 3393: 200001", Rejected, ERR_UPER_DECODE_VBIT3, 196648,
                (H (16#C4#), It (1, 1, 65536), H (16#C4#), It (0, 1, 65536), H (16#C4#), It (1, 1, 65536),
                 L2 (3393), It (1, 1, 3393)));
   Check_VBit3 ("B3 VBit3 C4 C4 C4 C1: 212992", Rejected, ERR_UPER_DECODE_VBIT3, 196640,
                (H (16#C4#), It (1, 1, 65536), H (16#C4#), It (0, 1, 65536), H (16#C4#), It (1, 1, 65536),
                 H (16#C1#), It (1, 1, 16384)));
   Check_VBit3 ("B4 VBit3 C4 + 4467: 70003 (not a multiple of 8)", Accepted, 0, 70027,
                (H (16#C4#), It (1, 1, 65536), L2 (4467), It (0, 1, 4466), It (1, 1, 1)));
   Check_VBit3 ("B5 VBit3 length 2 (SIZE min 3)", Rejected, ERR_UPER_DECODE_VBIT3, 10, (H (16#02#), It (1, 1, 2)));

   --  SEQUENCE (SIZE(0..70000)) OF INTEGER (0..5): 3-bit items, 6 and 7 are invalid
   Check_VSeq ("R15 VSeq length 2, first element fails", Rejected, ERR_UPER_DECODE_VSEQ_ELM, 11,
               (H (16#02#), It (7, 3, 1), It (0, 3, 1)));
   Check_VSeq ("R16 VSeq C1, first element fails", Rejected, ERR_UPER_DECODE_VSEQ_ELM, 11,
               (H (16#C1#), It (6, 3, 1), It (0, 3, 16383), H (16#00#)));
   Check_VSeq ("R17 VSeq C4 C1: 81920", Rejected, ERR_UPER_DECODE_VSEQ, 196624,
               (H (16#C4#), It (5, 3, 65536), H (16#C1#), It (5, 3, 16384), H (16#00#)));
   Check_VSeq ("R18 VSeq C4 + 8191", Rejected, ERR_UPER_DECODE_VSEQ, 196632,
               (H (16#C4#), It (5, 3, 65536), L2 (8191)));
   Check_VSeq ("A6 VSeq 70000", Accepted, 0, 210024, (H (16#C4#), It (4, 3, 65536), L2 (4464), It (3, 3, 4464)));

   --  SEQUENCE (SIZE(0..150000)) OF Elm: 4-bit items (a = 0..5 in 3 bits, b); 13 = a 6, 15 = a 7
   Check_VSeqR ("Q1 VSeqR element 101 of the second block fails", Rejected, ERR_UPER_DECODE_ELM_A, 262563,
                (H (16#C4#), It (5, 4, 65536), H (16#C4#), It (5, 4, 100), It (13, 4, 1), It (5, 4, 65435),
                 H (16#00#)));
   Check_VSeqR ("Q2 VSeqR last element of the first block fails", Rejected, ERR_UPER_DECODE_ELM_A, 262151,
                (H (16#C4#), It (5, 4, 65535), It (13, 4, 1), H (16#00#)));
   Check_VSeqR ("Q3 VSeqR element 2 of the final part after two blocks fails", Rejected, ERR_UPER_DECODE_ELM_A,
                524319,
                (H (16#C4#), It (5, 4, 65536), H (16#C4#), It (5, 4, 65536), H (16#03#), It (5, 4, 1),
                 It (15, 4, 1), It (5, 4, 1)));
   Check_VSeqR ("Q4 VSeqR C4 C4 C1 + 2544: 150000", Accepted, 0, 600040,
                (H (16#C4#), It (5, 4, 65536), H (16#C4#), It (4, 4, 65536), H (16#C1#), It (1, 4, 16384),
                 L2 (2544), It (3, 4, 2544)));
   Check_VSeqR ("Q5 VSeqR C4 C4 C1 + 2545: 150001", Rejected, ERR_UPER_DECODE_VSEQR, 589864,
                (H (16#C4#), It (5, 4, 65536), H (16#C4#), It (4, 4, 65536), H (16#C1#), It (1, 4, 16384),
                 L2 (2545), It (3, 4, 2545)));
   Check_VSeqR ("Q6 VSeqR C4 C4 C2: 163840", Rejected, ERR_UPER_DECODE_VSEQR, 524312,
                (H (16#C4#), It (5, 4, 65536), H (16#C4#), It (5, 4, 65536), H (16#C2#), It (5, 4, 32768)));

   --  a component of type VOct, then a fixed-size tail: a block over the maximum leaves the tail untouched
   Check_POct ("P1 POct o = C4 C2, tail kept", Rejected, ERR_UPER_DECODE_VOCT, 524304,
               (H (16#C4#), C (1, 16#AA#, 8, 65536), H (16#C2#), C (1, 16#BB#, 8, 32768), H (16#00#),
                C (2, 16#BB#, 8, 4096)));
   Check_POct ("P2 POct o = 3 octets", Accepted, 0, 32800,
               (H (16#03#), C (1, 16#AA#, 8, 3), C (2, 16#CC#, 8, 4096)));

   --  three inline fragmented components (o OCTET, s IA5String, q SEQUENCE OF) and an INTEGER
   Check_PInl ("N1 PInl a block in o, s and q", Accepted, 0, 262203,
               (H (16#C1#), C (1, 7, 8, 16384), H (16#00#), H (16#C1#), C (2, 2, 5, 16384), H (16#00#),
                H (16#C1#), C (3, 4, 3, 16384), H (16#01#), C (3, 5, 3, 1), C (4, 16#77#, 8, 1)));
   Check_PInl ("N2 PInl s: item 101 of the block fails", Rejected, ERR_UPER_DECODE_PINL_S, 529,
               (H (16#01#), C (1, 7, 8, 1), H (16#C1#), C (2, 2, 5, 100), C (2, 30, 5, 1), C (2, 2, 5, 16283),
                H (16#00#), H (16#00#), C (4, 16#77#, 8, 1)));
   Check_PInl ("N3 PInl q = C4 C1: 81920", Rejected, ERR_UPER_DECODE_PINL_Q, 196640,
               (H (16#00#), H (16#00#), H (16#C4#), C (3, 4, 3, 65536), H (16#C1#), C (3, 4, 3, 16384), H (16#00#),
                C (4, 16#77#, 8, 1)));
   Check_PInl ("N4 PInl q: first element fails", Rejected, ERR_UPER_DECODE_PINL_Q_ELM, 45,
               (H (16#01#), C (1, 7, 8, 1), H (16#02#), C (2, 1, 5, 1), C (2, 0, 5, 1), H (16#02#), C (3, 7, 3, 1),
                C (3, 1, 3, 1), C (4, 16#77#, 8, 1)));

   if Failed then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Ada.Text_IO.Put_Line ("uper-wire 004 (Ada): all checks passed");
   end if;
end Wire_Test;
