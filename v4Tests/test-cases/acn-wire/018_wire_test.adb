--  Checks of 018.asn1 in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, plain, with -gnata, and
--  with -gnatp + ASan; Issue 174): the rows and bytes of 018_wire_test.c (generated from the same table).
--  INVALID rows: IsConstraintValid and the uPER and ACN encoders return the constraint's error code; each encoder writes
--  into a stream of exactly the type's REQUIRED_BYTES that lies between two guard regions of one heap allocation, and
--  the guards must stay untouched (before the fix the validator of a WITH COMPONENT on a SEQUENCE OF checked nothing:
--  the encoders raised Constraint_Error, failed a loop invariant under -gnata, or under -gnatp wrote past the stream,
--  into the guard, where no sanitizer sees it). VALID rows: the bytes of the C test, decoded back (Equal). DECODE rows:
--  the decoders end with the validator, so they reject the decoded value with the constraint's error code.
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with acn_wire_with_component; use acn_wire_with_component;
procedure Wire_Test is
   use type Interfaces.Unsigned_8;

   procedure Fail (Row, Msg : String) is
   begin
      Ada.Text_IO.Put_Line ("FAILED: " & Row & ": " & Msg);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Fail;

   function Pad (X : String; N : Positive) return String is
      R : String (1 .. N) := (others => adaasn1rtl.NUL);
   begin
      R (1 .. X'Length) := X;
      return R;
   end Pad;

   Guard_Fill : constant Asn1Byte := 16#A5#;
   type Guard_Region is array (1 .. 4096) of Asn1Byte;
   type Guarded (N : Positive) is record
      G1 : Guard_Region;
      S  : adaasn1rtl.encoding.Bitstream (N);
      G2 : Guard_Region;
   end record;
   --  GNAT moves a component of dynamic size to the end of the record unless told not to
   pragma No_Component_Reordering (Guarded);
   type Guarded_Ptr is access Guarded;
   procedure Free is new Ada.Unchecked_Deallocation (Guarded, Guarded_Ptr);

   generic
      type T is private;
      with function Is_Valid (V : T) return ASN1_RESULT;
      with function Equal (V1, V2 : T) return Boolean;
      U_Size : Positive;
      with procedure U_Enc (V : T; S : out adaasn1rtl.encoding.Bitstream; R : out ASN1_RESULT);
      with procedure U_Dec (V : out T; S : in out adaasn1rtl.encoding.Bitstream; R : out ASN1_RESULT);
      A_Size : Positive;
      with procedure A_Enc (V : T; S : out adaasn1rtl.encoding.Bitstream; R : out ASN1_RESULT);
      with procedure A_Dec (V : out T; S : in out adaasn1rtl.encoding.Bitstream; R : out ASN1_RESULT);
   package Checks is
      procedure Invalid (Row : String; V : T; Err : Integer);
      procedure Valid (Row : String; V : T; U_Bytes : OctetBuffer; U_Bits : Natural;
                       A_Bytes : OctetBuffer; A_Bits : Natural);
      procedure Decode_U (Row : String; Stream : OctetBuffer; Err : Integer; Pos : Natural);
      procedure Decode_A (Row : String; Stream : OctetBuffer; Err : Integer; Pos : Natural);
   end Checks;

   package body Checks is

      procedure Guarded_Encode (Row, Enc : String; V : T; Size : Positive; Err : Integer; Is_U : Boolean) is
         G : Guarded_Ptr := new Guarded (Size);
         R : ASN1_RESULT;
      begin
         G.G1 := (others => Guard_Fill);
         G.G2 := (others => Guard_Fill);
         if Is_U then
            U_Enc (V, G.S, R);
         else
            A_Enc (V, G.S, R);
         end if;
         if R.Success or else R.ErrorCode /= Err then
            Fail (Row, Enc & " encoder: not rejected with the error code" & Integer'Image (Err) & ", got"
                  & Integer'Image (R.ErrorCode));
         end if;
         if G.S.Current_Bit_Pos /= 0 then
            Fail (Row, Enc & " encoder: wrote before rejecting");
         end if;
         if (for some B of G.G1 => B /= Guard_Fill) or else (for some B of G.G2 => B /= Guard_Fill) then
            Fail (Row, Enc & " encoder: wrote outside the stream");
         end if;
         Free (G);
      end Guarded_Encode;

      procedure Invalid (Row : String; V : T; Err : Integer) is
         R : constant ASN1_RESULT := Is_Valid (V);
      begin
         if R.Success or else R.ErrorCode /= Err then
            Fail (Row, "IsConstraintValid: not rejected with the error code" & Integer'Image (Err));
         end if;
         Guarded_Encode (Row, "uPER", V, U_Size, Err, True);
         Guarded_Encode (Row, "ACN", V, A_Size, Err, False);
      end Invalid;

      procedure Round_Trip (Row, Enc : String; V : T; Size : Positive; Expected : OctetBuffer; Bits : Natural;
                            Is_U : Boolean) is
         S : adaasn1rtl.encoding.Bitstream (Size);
         D : T;
         R : ASN1_RESULT;
      begin
         if Is_U then
            U_Enc (V, S, R);
         else
            A_Enc (V, S, R);
         end if;
         if not R.Success or else R.ErrorCode /= 0 then
            Fail (Row, Enc & " encode");
            return;
         end if;
         if S.Current_Bit_Pos /= Bits or else Expected'Length /= (Bits + 7) / 8
           or else S.Buffer (1 .. Expected'Length) /= Expected
         then
            Fail (Row, Enc & " encoded bytes");
         end if;
         S.Buffer := (others => 0);
         S.Buffer (1 .. Expected'Length) := Expected;
         S.Current_Bit_Pos := 0;
         if Is_U then
            U_Dec (D, S, R);
         else
            A_Dec (D, S, R);
         end if;
         if not R.Success or else not Equal (D, V) or else S.Current_Bit_Pos /= Bits then
            Fail (Row, Enc & " decode");
         end if;
      end Round_Trip;

      procedure Valid (Row : String; V : T; U_Bytes : OctetBuffer; U_Bits : Natural;
                       A_Bytes : OctetBuffer; A_Bits : Natural) is
         R : constant ASN1_RESULT := Is_Valid (V);
      begin
         if not R.Success then
            Fail (Row, "IsConstraintValid rejected a valid value");
         end if;
         Round_Trip (Row, "uPER", V, U_Size, U_Bytes, U_Bits, True);
         Round_Trip (Row, "ACN", V, A_Size, A_Bytes, A_Bits, False);
      end Valid;

      procedure Decode (Row, Enc : String; Size : Positive; Stream : OctetBuffer; Err : Integer; Pos : Natural;
                        Is_U : Boolean) is
         S : adaasn1rtl.encoding.Bitstream (Size);
         D : T;
         R : ASN1_RESULT;
      begin
         S.Buffer := (others => 0);
         S.Buffer (1 .. Stream'Length) := Stream;
         S.Current_Bit_Pos := 0;
         if Is_U then
            U_Dec (D, S, R);
         else
            A_Dec (D, S, R);
         end if;
         if R.Success or else R.ErrorCode /= Err then
            Fail (Row, Enc & " decoder: not rejected with the error code" & Integer'Image (Err) & ", got"
                  & Integer'Image (R.ErrorCode));
         end if;
         if S.Current_Bit_Pos /= Pos then
            Fail (Row, Enc & " decoder: stopped at bit" & Integer'Image (S.Current_Bit_Pos) & ", expected"
                  & Integer'Image (Pos));
         end if;
      end Decode;

      procedure Decode_U (Row : String; Stream : OctetBuffer; Err : Integer; Pos : Natural) is
      begin
         Decode (Row, "uPER", U_Size, Stream, Err, Pos, True);
      end Decode_U;

      procedure Decode_A (Row : String; Stream : OctetBuffer; Err : Integer; Pos : Natural) is
      begin
         Decode (Row, "ACN", A_Size, Stream, Err, Pos, False);
      end Decode_A;
   end Checks;


   package C_OutK is new Checks
     (OutK, OutK_IsConstraintValid, OutK_Equal, OutK_REQUIRED_BYTES_FOR_ENCODING, OutK_Encode, OutK_Decode,
      OutK_ACN_STREAM_SIZE, OutK_ACN_Encode, OutK_ACN_Decode);
   package C_SubK is new Checks
     (SubK, SubK_IsConstraintValid, SubK_Equal, SubK_REQUIRED_BYTES_FOR_ENCODING, SubK_Encode, SubK_Decode,
      SubK_ACN_STREAM_SIZE, SubK_ACN_Encode, SubK_ACN_Decode);
   package C_OutV is new Checks
     (OutV, OutV_IsConstraintValid, OutV_Equal, OutV_REQUIRED_BYTES_FOR_ENCODING, OutV_Encode, OutV_Decode,
      OutV_ACN_STREAM_SIZE, OutV_ACN_Encode, OutV_ACN_Decode);
   package C_Out2 is new Checks
     (Out2, Out2_IsConstraintValid, Out2_Equal, Out2_REQUIRED_BYTES_FOR_ENCODING, Out2_Encode, Out2_Decode,
      Out2_ACN_STREAM_SIZE, Out2_ACN_Encode, Out2_ACN_Decode);
   package C_OutO is new Checks
     (OutO, OutO_IsConstraintValid, OutO_Equal, OutO_REQUIRED_BYTES_FOR_ENCODING, OutO_Encode, OutO_Decode,
      OutO_ACN_STREAM_SIZE, OutO_ACN_Encode, OutO_ACN_Decode);
   package C_OutB is new Checks
     (OutB, OutB_IsConstraintValid, OutB_Equal, OutB_REQUIRED_BYTES_FOR_ENCODING, OutB_Encode, OutB_Decode,
      OutB_ACN_STREAM_SIZE, OutB_ACN_Encode, OutB_ACN_Decode);
   package C_OutS is new Checks
     (OutS, OutS_IsConstraintValid, OutS_Equal, OutS_REQUIRED_BYTES_FOR_ENCODING, OutS_Encode, OutS_Decode,
      OutS_ACN_STREAM_SIZE, OutS_ACN_Encode, OutS_ACN_Decode);
   package C_OutKF is new Checks
     (OutKF, OutKF_IsConstraintValid, OutKF_Equal, OutKF_REQUIRED_BYTES_FOR_ENCODING, OutKF_Encode, OutKF_Decode,
      OutKF_ACN_STREAM_SIZE, OutKF_ACN_Encode, OutKF_ACN_Decode);
   package C_OutN is new Checks
     (OutN, OutN_IsConstraintValid, OutN_Equal, OutN_REQUIRED_BYTES_FOR_ENCODING, OutN_Encode, OutN_Decode,
      OutN_ACN_STREAM_SIZE, OutN_ACN_Encode, OutN_ACN_Decode);
   package C_DirK is new Checks
     (DirK, DirK_IsConstraintValid, DirK_Equal, DirK_REQUIRED_BYTES_FOR_ENCODING, DirK_Encode, DirK_Decode,
      DirK_ACN_STREAM_SIZE, DirK_ACN_Encode, DirK_ACN_Decode);
   package C_OutI is new Checks
     (OutI, OutI_IsConstraintValid, OutI_Equal, OutI_REQUIRED_BYTES_FOR_ENCODING, OutI_Encode, OutI_Decode,
      OutI_ACN_STREAM_SIZE, OutI_ACN_Encode, OutI_ACN_Decode);
   package C_OutE is new Checks
     (OutE, OutE_IsConstraintValid, OutE_Equal, OutE_REQUIRED_BYTES_FOR_ENCODING, OutE_Encode, OutE_Decode,
      OutE_ACN_STREAM_SIZE, OutE_ACN_Encode, OutE_ACN_Decode);
   package C_SubU is new Checks
     (SubU, SubU_IsConstraintValid, SubU_Equal, SubU_REQUIRED_BYTES_FOR_ENCODING, SubU_Encode, SubU_Decode,
      SubU_ACN_STREAM_SIZE, SubU_ACN_Encode, SubU_ACN_Decode);
   package C_OutU is new Checks
     (OutU, OutU_IsConstraintValid, OutU_Equal, OutU_REQUIRED_BYTES_FOR_ENCODING, OutU_Encode, OutU_Decode,
      OutU_ACN_STREAM_SIZE, OutU_ACN_Encode, OutU_ACN_Decode);
   package C_OptU is new Checks
     (OptU, OptU_IsConstraintValid, OptU_Equal, OptU_REQUIRED_BYTES_FOR_ENCODING, OptU_Encode, OptU_Decode,
      OptU_ACN_STREAM_SIZE, OptU_ACN_Encode, OptU_ACN_Decode);
   package C_SubX is new Checks
     (SubX, SubX_IsConstraintValid, SubX_Equal, SubX_REQUIRED_BYTES_FOR_ENCODING, SubX_Encode, SubX_Decode,
      SubX_ACN_STREAM_SIZE, SubX_ACN_Encode, SubX_ACN_Decode);
   package C_OptK is new Checks
     (OptK, OptK_IsConstraintValid, OptK_Equal, OptK_REQUIRED_BYTES_FOR_ENCODING, OptK_Encode, OptK_Decode,
      OptK_ACN_STREAM_SIZE, OptK_ACN_Encode, OptK_ACN_Decode);
   package C_ChK is new Checks
     (ChK, ChK_IsConstraintValid, ChK_Equal, ChK_REQUIRED_BYTES_FOR_ENCODING, ChK_Encode, ChK_Decode,
      ChK_ACN_STREAM_SIZE, ChK_ACN_Encode, ChK_ACN_Decode);
   package C_OutEO is new Checks
     (OutEO, OutEO_IsConstraintValid, OutEO_Equal, OutEO_REQUIRED_BYTES_FOR_ENCODING, OutEO_Encode, OutEO_Decode,
      OutEO_ACN_STREAM_SIZE, OutEO_ACN_Encode, OutEO_ACN_Decode);
   package C_MixA is new Checks
     (MixA, MixA_IsConstraintValid, MixA_Equal, MixA_REQUIRED_BYTES_FOR_ENCODING, MixA_Encode, MixA_Decode,
      MixA_ACN_STREAM_SIZE, MixA_ACN_Encode, MixA_ACN_Decode);
   package C_MixB is new Checks
     (MixB, MixB_IsConstraintValid, MixB_Equal, MixB_REQUIRED_BYTES_FOR_ENCODING, MixB_Encode, MixB_Decode,
      MixB_ACN_STREAM_SIZE, MixB_ACN_Encode, MixB_ACN_Decode);
   package C_MixC is new Checks
     (MixC, MixC_IsConstraintValid, MixC_Equal, MixC_REQUIRED_BYTES_FOR_ENCODING, MixC_Encode, MixC_Decode,
      MixC_ACN_STREAM_SIZE, MixC_ACN_Encode, MixC_ACN_Decode);
   package C_MixD is new Checks
     (MixD, MixD_IsConstraintValid, MixD_Equal, MixD_REQUIRED_BYTES_FOR_ENCODING, MixD_Encode, MixD_Decode,
      MixD_ACN_STREAM_SIZE, MixD_ACN_Encode, MixD_ACN_Decode);
   package C_MixE is new Checks
     (MixE, MixE_IsConstraintValid, MixE_Equal, MixE_REQUIRED_BYTES_FOR_ENCODING, MixE_Encode, MixE_Decode,
      MixE_ACN_STREAM_SIZE, MixE_ACN_Encode, MixE_ACN_Decode);
   package C_LngT is new Checks
     (LngT, LngT_IsConstraintValid, LngT_Equal, LngT_REQUIRED_BYTES_FOR_ENCODING, LngT_Encode, LngT_Decode,
      LngT_ACN_STREAM_SIZE, LngT_ACN_Encode, LngT_ACN_Decode);
   package C_LngU is new Checks
     (LngU, LngU_IsConstraintValid, LngU_Equal, LngU_REQUIRED_BYTES_FOR_ENCODING, LngU_Encode, LngU_Decode,
      LngU_ACN_STREAM_SIZE, LngU_ACN_Encode, LngU_ACN_Decode);
   package C_LngW is new Checks
     (LngW, LngW_IsConstraintValid, LngW_Equal, LngW_REQUIRED_BYTES_FOR_ENCODING, LngW_Encode, LngW_Decode,
      LngW_ACN_STREAM_SIZE, LngW_ACN_Encode, LngW_ACN_Decode);
   package C_LngV is new Checks
     (LngV, LngV_IsConstraintValid, LngV_Equal, LngV_REQUIRED_BYTES_FOR_ENCODING, LngV_Encode, LngV_Decode,
      LngV_ACN_STREAM_SIZE, LngV_ACN_Encode, LngV_ACN_Decode);

begin
   declare
      V : OutK := OutK_Init;
   begin
      V.k.Length := 3;
      V.k.Data (1) := Pad ("abcdef", V.k.Data (1)'Length);
      V.k.Data (2) := Pad ("abcdef", V.k.Data (2)'Length);
      V.k.Data (3) := Pad ("abcdef", V.k.Data (3)'Length);
      V.b := True;
      C_OutK.Invalid ("OutK", V, ERR_OUTK_K_2);
   end;
   declare
      V : SubK := SubK_Init;
   begin
      V.Length := 3;
      V.Data (1) := Pad ("abcdef", V.Data (1)'Length);
      V.Data (2) := Pad ("abcdef", V.Data (2)'Length);
      V.Data (3) := Pad ("abcdef", V.Data (3)'Length);
      C_SubK.Invalid ("SubK", V, ERR_SUBK_2);
   end;
   declare
      V : OutV := OutV_Init;
   begin
      V.k.Length := 3;
      V.k.Data (1) := Pad ("abcdef", V.k.Data (1)'Length);
      V.k.Data (2) := Pad ("abcdef", V.k.Data (2)'Length);
      V.k.Data (3) := Pad ("abcdef", V.k.Data (3)'Length);
      V.b := True;
      C_OutV.Invalid ("OutV", V, ERR_OUTV_K_2);
   end;
   declare
      V : OutV := OutV_Init;
   begin
      V.k.Length := 2;
      V.k.Data (1) := Pad ("ab", V.k.Data (1)'Length);
      V.k.Data (2) := Pad ("ba", V.k.Data (2)'Length);
      V.b := True;
      C_OutV.Invalid ("OutVba", V, ERR_OUTV_K_2);
   end;
   declare
      V : Out2 := Out2_Init;
   begin
      V.x.k.Length := 3;
      V.x.k.Data (1) := Pad ("abcdef", V.x.k.Data (1)'Length);
      V.x.k.Data (2) := Pad ("abcdef", V.x.k.Data (2)'Length);
      V.x.k.Data (3) := Pad ("abcdef", V.x.k.Data (3)'Length);
      V.x.b := True;
      C_Out2.Invalid ("Out2", V, ERR_OUT2_X_2);
   end;
   declare
      V : OutO := OutO_Init;
   begin
      V.k.Length := 3;
      V.k.Data (1).Length := 6;
      V.k.Data (1).Data := (others => 16#55#);
      V.k.Data (2).Length := 6;
      V.k.Data (2).Data := (others => 16#55#);
      V.k.Data (3).Length := 6;
      V.k.Data (3).Data := (others => 16#55#);
      V.b := True;
      C_OutO.Invalid ("OutO", V, ERR_OUTO_K_2);
   end;
   declare
      V : OutB := OutB_Init;
   begin
      V.k.Length := 3;
      V.k.Data (1).Length := 20;
      V.k.Data (1).Data := (others => 0);
      V.k.Data (1).Data (1) := 1;
      V.k.Data (1).Data (2) := 1;
      V.k.Data (1).Data (3) := 1;
      V.k.Data (1).Data (4) := 1;
      V.k.Data (1).Data (5) := 1;
      V.k.Data (1).Data (6) := 1;
      V.k.Data (1).Data (7) := 1;
      V.k.Data (1).Data (8) := 1;
      V.k.Data (1).Data (9) := 1;
      V.k.Data (1).Data (10) := 1;
      V.k.Data (1).Data (11) := 1;
      V.k.Data (1).Data (12) := 1;
      V.k.Data (1).Data (13) := 1;
      V.k.Data (1).Data (14) := 1;
      V.k.Data (1).Data (15) := 1;
      V.k.Data (1).Data (16) := 1;
      V.k.Data (1).Data (17) := 1;
      V.k.Data (1).Data (18) := 1;
      V.k.Data (1).Data (19) := 1;
      V.k.Data (1).Data (20) := 1;
      V.k.Data (2).Length := 20;
      V.k.Data (2).Data := (others => 0);
      V.k.Data (2).Data (1) := 1;
      V.k.Data (2).Data (2) := 1;
      V.k.Data (2).Data (3) := 1;
      V.k.Data (2).Data (4) := 1;
      V.k.Data (2).Data (5) := 1;
      V.k.Data (2).Data (6) := 1;
      V.k.Data (2).Data (7) := 1;
      V.k.Data (2).Data (8) := 1;
      V.k.Data (2).Data (9) := 1;
      V.k.Data (2).Data (10) := 1;
      V.k.Data (2).Data (11) := 1;
      V.k.Data (2).Data (12) := 1;
      V.k.Data (2).Data (13) := 1;
      V.k.Data (2).Data (14) := 1;
      V.k.Data (2).Data (15) := 1;
      V.k.Data (2).Data (16) := 1;
      V.k.Data (2).Data (17) := 1;
      V.k.Data (2).Data (18) := 1;
      V.k.Data (2).Data (19) := 1;
      V.k.Data (2).Data (20) := 1;
      V.k.Data (3).Length := 20;
      V.k.Data (3).Data := (others => 0);
      V.k.Data (3).Data (1) := 1;
      V.k.Data (3).Data (2) := 1;
      V.k.Data (3).Data (3) := 1;
      V.k.Data (3).Data (4) := 1;
      V.k.Data (3).Data (5) := 1;
      V.k.Data (3).Data (6) := 1;
      V.k.Data (3).Data (7) := 1;
      V.k.Data (3).Data (8) := 1;
      V.k.Data (3).Data (9) := 1;
      V.k.Data (3).Data (10) := 1;
      V.k.Data (3).Data (11) := 1;
      V.k.Data (3).Data (12) := 1;
      V.k.Data (3).Data (13) := 1;
      V.k.Data (3).Data (14) := 1;
      V.k.Data (3).Data (15) := 1;
      V.k.Data (3).Data (16) := 1;
      V.k.Data (3).Data (17) := 1;
      V.k.Data (3).Data (18) := 1;
      V.k.Data (3).Data (19) := 1;
      V.k.Data (3).Data (20) := 1;
      V.b := True;
      C_OutB.Invalid ("OutB", V, ERR_OUTB_K_2);
   end;
   declare
      V : OutS := OutS_Init;
   begin
      V.k.Length := 3;
      V.k.Data (1).s := Pad ("abcdef", V.k.Data (1).s'Length);
      V.k.Data (1).n := 1000;
      V.k.Data (2).s := Pad ("abcdef", V.k.Data (2).s'Length);
      V.k.Data (2).n := 1000;
      V.k.Data (3).s := Pad ("abcdef", V.k.Data (3).s'Length);
      V.k.Data (3).n := 1000;
      V.b := True;
      C_OutS.Invalid ("OutS", V, ERR_OUTS_K_2);
   end;
   declare
      V : OutS := OutS_Init;
   begin
      V.k.Length := 1;
      V.k.Data (1).s := Pad ("ab", V.k.Data (1).s'Length);
      V.k.Data (1).n := 11;
      V.b := True;
      C_OutS.Invalid ("OutSn", V, ERR_OUTS_K_2);
   end;
   declare
      V : OutKF := OutKF_Init;
   begin
      V.k.Data (1) := Pad ("abcdef", V.k.Data (1)'Length);
      V.k.Data (2) := Pad ("abcdef", V.k.Data (2)'Length);
      V.b := True;
      C_OutKF.Invalid ("OutKF", V, ERR_OUTKF_K_2);
   end;
   declare
      V : OutN := OutN_Init;
   begin
      V.k.Length := 2;
      V.k.Data (1).Length := 3;
      V.k.Data (1).Data (1) := Pad ("abcdef", V.k.Data (1).Data (1)'Length);
      V.k.Data (1).Data (2) := Pad ("abcdef", V.k.Data (1).Data (2)'Length);
      V.k.Data (1).Data (3) := Pad ("abcdef", V.k.Data (1).Data (3)'Length);
      V.k.Data (2).Length := 3;
      V.k.Data (2).Data (1) := Pad ("abcdef", V.k.Data (2).Data (1)'Length);
      V.k.Data (2).Data (2) := Pad ("abcdef", V.k.Data (2).Data (2)'Length);
      V.k.Data (2).Data (3) := Pad ("abcdef", V.k.Data (2).Data (3)'Length);
      V.b := True;
      C_OutN.Invalid ("OutN", V, ERR_OUTN_K_2);
   end;
   declare
      V : DirK := DirK_Init;
   begin
      V.Length := 3;
      V.Data (1) := Pad ("abcdef", V.Data (1)'Length);
      V.Data (2) := Pad ("abcdef", V.Data (2)'Length);
      V.Data (3) := Pad ("abcdef", V.Data (3)'Length);
      C_DirK.Invalid ("DirK", V, ERR_DIRK);
   end;
   declare
      V : OutI := OutI_Init;
   begin
      V.k.Length := 2;
      V.k.Data (1) := 1000;
      V.k.Data (2) := 5;
      V.b := True;
      C_OutI.Invalid ("OutI", V, ERR_OUTI_K_2);
   end;
   declare
      V : OutE := OutE_Init;
   begin
      V.k.Length := 2;
      V.k.Data (1) := e1;
      V.k.Data (2) := e3;
      V.b := True;
      C_OutE.Invalid ("OutE", V, ERR_OUTE_K_2);
   end;
   declare
      V : SubU := SubU_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("a", V.Data (1)'Length);
      V.Data (2) := Pad ("bc", V.Data (2)'Length);
      C_SubU.Invalid ("SubUm", V, ERR_SUBU_2);
   end;
   declare
      V : OutU := OutU_Init;
   begin
      V.k.Length := 2;
      V.k.Data (1) := Pad ("ab", V.k.Data (1)'Length);
      V.k.Data (2) := Pad ("c", V.k.Data (2)'Length);
      V.b := True;
      C_OutU.Invalid ("OutUm", V, ERR_OUTU_K_2);
   end;
   declare
      V : OptU := OptU_Init;
   begin
      V.exist.a := 1;
      V.a := 15;
      V.b := 55;
      C_OptU.Invalid ("OptUa", V, ERR_OPTU);
   end;
   declare
      V : SubX := SubX_Init;
   begin
      V.Length := 1;
      V.Data (1) := Pad ("abcd", V.Data (1)'Length);
      C_SubX.Invalid ("SubXs", V, ERR_SUBX_2);
   end;
   declare
      V : SubX := SubX_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("abc", V.Data (1)'Length);
      V.Data (2) := Pad ("abc", V.Data (2)'Length);
      C_SubX.Invalid ("SubXe", V, ERR_SUBX_2);
   end;
   declare
      V : OptK := OptK_Init;
   begin
      V.exist.k := 1;
      V.k.Length := 1;
      V.k.Data (1) := Pad ("abc", V.k.Data (1)'Length);
      V.b := True;
      C_OptK.Invalid ("OptK", V, ERR_OPTK);
   end;
   declare
      V : ChK := ChK_Init;
   begin
      V := (kind => k_PRESENT, k => (Length => 1, Data => (others => Pad ("abc", 7))));
      C_ChK.Invalid ("ChKk", V, ERR_CHK);
   end;
   declare
      V : OutEO := OutEO_Init;
   begin
      V.k.Length := 1;
      V.k.Data (1).exist.s := 0;
      V.k.Data (1).n := 3;
      V.b := True;
      C_OutEO.Invalid ("OutEOa", V, ERR_OUTEO_K_2);
   end;
   declare
      V : OutEO := OutEO_Init;
   begin
      V.k.Length := 1;
      V.k.Data (1).exist.s := 1;
      V.k.Data (1).s := Pad ("abc", V.k.Data (1).s'Length);
      V.k.Data (1).n := 3;
      V.b := True;
      C_OutEO.Invalid ("OutEOs", V, ERR_OUTEO_K_2);
   end;
   declare
      V : MixA := MixA_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("abc", V.Data (1)'Length);
      V.Data (2) := Pad ("d", V.Data (2)'Length);
      C_MixA.Invalid ("MixAm", V, ERR_MIXA_2);
   end;
   declare
      V : MixB := MixB_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("abc", V.Data (1)'Length);
      V.Data (2) := Pad ("d", V.Data (2)'Length);
      C_MixB.Invalid ("MixBm", V, ERR_MIXB_2);
   end;
   declare
      V : MixC := MixC_Init;
   begin
      V.Length := 1;
      V.Data (1) := Pad ("abc", V.Data (1)'Length);
      C_MixC.Invalid ("MixCl", V, ERR_MIXC_2);
   end;
   declare
      V : MixC := MixC_Init;
   begin
      V.Length := 3;
      V.Data (1) := Pad ("a", V.Data (1)'Length);
      V.Data (2) := Pad ("b", V.Data (2)'Length);
      V.Data (3) := Pad ("c", V.Data (3)'Length);
      C_MixC.Invalid ("MixCs", V, ERR_MIXC_2);
   end;
   declare
      V : MixD := MixD_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("abc", V.Data (1)'Length);
      V.Data (2) := Pad ("def", V.Data (2)'Length);
      C_MixD.Invalid ("MixDm", V, ERR_MIXD_2);
   end;
   declare
      V : MixE := MixE_Init;
   begin
      V.Length := 3;
      V.Data (1) := Pad ("a", V.Data (1)'Length);
      V.Data (2) := Pad ("b", V.Data (2)'Length);
      V.Data (3) := Pad ("c", V.Data (3)'Length);
      C_MixE.Invalid ("MixEs", V, ERR_MIXE_2);
   end;
   declare
      V : MixE := MixE_Init;
   begin
      V.Length := 1;
      V.Data (1) := Pad ("abc", V.Data (1)'Length);
      C_MixE.Invalid ("MixEl", V, ERR_MIXE_2);
   end;
   declare
      V : LngT := LngT_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("a", V.Data (1)'Length);
      V.Data (2) := Pad ("zz", V.Data (2)'Length);
      C_LngT.Invalid ("LngTm", V, ERR_LNGT_2);
   end;
   declare
      V : LngU := LngU_Init;
   begin
      V := Pad ("zz", V'Length);
      C_LngU.Invalid ("LngUm", V, ERR_LNGU_2);
   end;
   declare
      V : LngW := LngW_Init;
   begin
      V.x := Pad ("zz", V.x'Length);
      C_LngW.Invalid ("LngWm", V, ERR_LNGW_X_2);
   end;
   declare
      V : LngV := LngV_Init;
   begin
      V.x := Pad ("zz", V.x'Length);
      C_LngV.Invalid ("LngVm", V, ERR_LNGV);
   end;
   declare
      V : OutK := OutK_Init;
   begin
      V.k.Length := 3;
      V.k.Data (1) := Pad ("ab", V.k.Data (1)'Length);
      V.k.Data (2) := Pad ("a", V.k.Data (2)'Length);
      V.k.Data (3) := Pad ("ba", V.k.Data (3)'Length);
      V.b := True;
      C_OutK.Valid
        ("OutKv", V, (16#8E#, 16#1C#, 16#43#, 16#09#, 16#C5#, 16#86#), 47,
         (16#8E#, 16#1C#, 16#43#, 16#09#, 16#C5#, 16#86#), 47);
   end;
   declare
      V : SubK := SubK_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("ab", V.Data (1)'Length);
      V.Data (2) := Pad ("z", V.Data (2)'Length);
      C_SubK.Valid
        ("SubKv", V, (16#4E#, 16#1C#, 16#43#, 16#D0#), 29,
         (16#4E#, 16#1C#, 16#43#, 16#D0#), 29);
   end;
   declare
      V : OutV := OutV_Init;
   begin
      V.k.Length := 2;
      V.k.Data (1) := Pad ("a", V.k.Data (1)'Length);
      V.k.Data (2) := Pad ("ab", V.k.Data (2)'Length);
      V.b := True;
      C_OutV.Valid
        ("OutVv", V, (16#46#, 16#13#, 16#87#, 16#14#), 30,
         (16#46#, 16#13#, 16#87#, 16#14#), 30);
   end;
   declare
      V : Out2 := Out2_Init;
   begin
      V.x.k.Length := 1;
      V.x.k.Data (1) := Pad ("ab", V.x.k.Data (1)'Length);
      V.x.b := False;
      C_Out2.Valid
        ("Out2v", V, (16#0E#, 16#1C#, 16#40#), 20,
         (16#0E#, 16#1C#, 16#40#), 20);
   end;
   declare
      V : OutO := OutO_Init;
   begin
      V.k.Length := 2;
      V.k.Data (1).Length := 2;
      V.k.Data (1).Data := (others => 16#A5#);
      V.k.Data (2).Length := 2;
      V.k.Data (2).Data := (others => 16#A5#);
      V.b := True;
      C_OutO.Valid
        ("OutOv", V, (16#4D#, 16#2D#, 16#29#, 16#A5#, 16#A5#, 16#80#), 41,
         (16#74#, 16#B4#, 16#BA#, 16#5A#, 16#58#), 37);
   end;
   declare
      V : OutB := OutB_Init;
   begin
      V.k.Length := 3;
      V.k.Data (1).Length := 4;
      V.k.Data (1).Data := (others => 0);
      V.k.Data (1).Data (1) := 1;
      V.k.Data (2).Length := 4;
      V.k.Data (2).Data := (others => 0);
      V.k.Data (2).Data (1) := 1;
      V.k.Data (3).Length := 4;
      V.k.Data (3).Data := (others => 0);
      V.k.Data (3).Data (1) := 1;
      V.b := True;
      C_OutB.Valid
        ("OutBv", V, (16#87#, 16#03#, 16#81#, 16#C4#), 30,
         (16#B8#, 16#E3#, 16#88#), 21);
   end;
   declare
      V : OutS := OutS_Init;
   begin
      V.k.Length := 2;
      V.k.Data (1).s := Pad ("ab", V.k.Data (1).s'Length);
      V.k.Data (1).n := 10;
      V.k.Data (2).s := Pad ("a", V.k.Data (2).s'Length);
      V.k.Data (2).n := 0;
      V.b := True;
      C_OutS.Valid
        ("OutSv", V, (16#4E#, 16#1C#, 16#40#, 16#50#, 16#C2#, 16#00#, 16#40#), 50,
         (16#4E#, 16#1C#, 16#40#, 16#50#, 16#C2#, 16#00#, 16#40#), 50);
   end;
   declare
      V : OutKF := OutKF_Init;
   begin
      V.k.Data (1) := Pad ("ab", V.k.Data (1)'Length);
      V.k.Data (2) := Pad ("c", V.k.Data (2)'Length);
      V.b := True;
      C_OutKF.Valid
        ("OutKFv", V, (16#38#, 16#71#, 16#0C#, 16#70#), 28,
         (16#38#, 16#71#, 16#0C#, 16#70#), 28);
   end;
   declare
      V : OutN := OutN_Init;
   begin
      V.k.Length := 2;
      V.k.Data (1).Length := 1;
      V.k.Data (1).Data (1) := Pad ("ab", V.k.Data (1).Data (1)'Length);
      V.k.Data (2).Length := 2;
      V.k.Data (2).Data (1) := Pad ("a", V.k.Data (2).Data (1)'Length);
      V.k.Data (2).Data (2) := Pad ("b", V.k.Data (2).Data (2)'Length);
      V.b := True;
      C_OutN.Valid
        ("OutNv", V, (16#87#, 16#0E#, 16#24#, 16#61#, 16#18#, 16#A0#), 43,
         (16#87#, 16#0E#, 16#24#, 16#61#, 16#18#, 16#A0#), 43);
   end;
   declare
      V : DirK := DirK_Init;
   begin
      V.Length := 1;
      V.Data (1) := Pad ("xy", V.Data (1)'Length);
      C_DirK.Valid
        ("DirKv", V, (16#0F#, 16#8F#, 16#20#), 19,
         (16#0F#, 16#8F#, 16#20#), 19);
   end;
   declare
      V : OutI := OutI_Init;
   begin
      V.k.Length := 3;
      V.k.Data (1) := 10;
      V.k.Data (2) := 0;
      V.k.Data (3) := 7;
      V.b := True;
      C_OutI.Valid
        ("OutIv", V, (16#80#, 16#A0#, 16#00#, 16#07#, 16#80#), 33,
         (16#80#, 16#A0#, 16#00#, 16#07#, 16#80#), 33);
   end;
   declare
      V : OutE := OutE_Init;
   begin
      V.k.Length := 2;
      V.k.Data (1) := e1;
      V.k.Data (2) := e2;
      V.b := False;
      C_OutE.Valid
        ("OutEv", V, (1 => 16#44#), 7,
         (1 => 16#44#), 7);
   end;
   declare
      V : SubU := SubU_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("a", V.Data (1)'Length);
      V.Data (2) := Pad ("b", V.Data (2)'Length);
      C_SubU.Valid
        ("SubU1", V, (16#46#, 16#11#, 16#88#), 22,
         (16#46#, 16#11#, 16#88#), 22);
   end;
   declare
      V : SubU := SubU_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("ab", V.Data (1)'Length);
      V.Data (2) := Pad ("cd", V.Data (2)'Length);
      C_SubU.Valid
        ("SubU2", V, (16#4E#, 16#1C#, 16#47#, 16#1E#, 16#40#), 36,
         (16#4E#, 16#1C#, 16#47#, 16#1E#, 16#40#), 36);
   end;
   declare
      V : OutU := OutU_Init;
   begin
      V.k.Length := 3;
      V.k.Data (1) := Pad ("ab", V.k.Data (1)'Length);
      V.k.Data (2) := Pad ("cd", V.k.Data (2)'Length);
      V.k.Data (3) := Pad ("ef", V.k.Data (3)'Length);
      V.b := True;
      C_OutU.Valid
        ("OutU2", V, (16#8E#, 16#1C#, 16#47#, 16#1E#, 16#43#, 16#97#, 16#34#), 54,
         (16#8E#, 16#1C#, 16#47#, 16#1E#, 16#43#, 16#97#, 16#34#), 54);
   end;
   declare
      V : OptU := OptU_Init;
   begin
      V.exist.a := 0;
      V.b := 55;
      C_OptU.Valid
        ("OptUb", V, (1 => 16#37#), 8,
         (1 => 16#37#), 8);
   end;
   declare
      V : OptU := OptU_Init;
   begin
      V.exist.a := 1;
      V.a := 25;
      V.b := 55;
      C_OptU.Valid
        ("OptUp", V, (16#99#, 16#6E#), 15,
         (16#99#, 16#6E#), 15);
   end;
   declare
      V : OptU := OptU_Init;
   begin
      V.exist.a := 1;
      V.a := 5;
      V.b := 5;
      C_OptU.Valid
        ("OptU1", V, (16#85#, 16#0A#), 15,
         (16#85#, 16#0A#), 15);
   end;
   declare
      V : SubX := SubX_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("ab", V.Data (1)'Length);
      V.Data (2) := Pad ("abc", V.Data (2)'Length);
      C_SubX.Valid
        ("SubXv", V, (16#4E#, 16#1C#, 16#4B#, 16#0E#, 16#2C#, 16#60#), 43,
         (16#4E#, 16#1C#, 16#4B#, 16#0E#, 16#2C#, 16#60#), 43);
   end;
   declare
      V : OptK := OptK_Init;
   begin
      V.exist.k := 0;
      V.b := False;
      C_OptK.Valid
        ("OptKa", V, (1 => 16#00#), 2,
         (1 => 16#00#), 2);
   end;
   declare
      V : OptK := OptK_Init;
   begin
      V.exist.k := 1;
      V.k.Length := 2;
      V.k.Data (1) := Pad ("ab", V.k.Data (1)'Length);
      V.k.Data (2) := Pad ("c", V.k.Data (2)'Length);
      V.b := True;
      C_OptK.Valid
        ("OptKp", V, (16#A7#, 16#0E#, 16#21#, 16#8E#), 31,
         (16#A7#, 16#0E#, 16#21#, 16#8E#), 31);
   end;
   declare
      V : ChK := ChK_Init;
   begin
      V := (kind => k_PRESENT, k => (Length => 2, Data => (1 => Pad ("ab", 7), others => Pad ("c", 7))));
      C_ChK.Valid
        ("ChKkv", V, (16#27#, 16#0E#, 16#21#, 16#8C#), 30,
         (16#27#, 16#0E#, 16#21#, 16#8C#), 30);
   end;
   declare
      V : ChK := ChK_Init;
   begin
      V := (kind => n_PRESENT, n => 5);
      C_ChK.Valid
        ("ChKn", V, (1 => 16#D0#), 4,
         (1 => 16#D0#), 4);
   end;
   declare
      V : OutEO := OutEO_Init;
   begin
      V.k.Length := 2;
      V.k.Data (1).exist.s := 1;
      V.k.Data (1).s := Pad ("ab", V.k.Data (1).s'Length);
      V.k.Data (1).n := 3;
      V.k.Data (2).exist.s := 1;
      V.k.Data (2).s := Pad ("z", V.k.Data (2).s'Length);
      V.k.Data (2).n := 0;
      V.b := False;
      C_OutEO.Valid
        ("OutEOv", V, (16#67#, 16#0E#, 16#23#, 16#8F#, 16#40#), 40,
         (16#4E#, 16#1C#, 16#46#, 16#3D#, 16#00#), 38);
   end;
   declare
      V : MixA := MixA_Init;
   begin
      V.Length := 1;
      V.Data (1) := Pad ("abc", V.Data (1)'Length);
      C_MixA.Valid
        ("MixA1", V, (16#16#, 16#1C#, 16#58#, 16#C0#), 26,
         (16#16#, 16#1C#, 16#58#, 16#C0#), 26);
   end;
   declare
      V : MixA := MixA_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("ab", V.Data (1)'Length);
      V.Data (2) := Pad ("cd", V.Data (2)'Length);
      C_MixA.Valid
        ("MixA2", V, (16#4E#, 16#1C#, 16#47#, 16#1E#, 16#40#), 36,
         (16#4E#, 16#1C#, 16#47#, 16#1E#, 16#40#), 36);
   end;
   declare
      V : MixB := MixB_Init;
   begin
      V.Length := 1;
      V.Data (1) := Pad ("abc", V.Data (1)'Length);
      C_MixB.Valid
        ("MixB1", V, (16#16#, 16#1C#, 16#58#, 16#C0#), 26,
         (16#16#, 16#1C#, 16#58#, 16#C0#), 26);
   end;
   declare
      V : MixB := MixB_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("ab", V.Data (1)'Length);
      V.Data (2) := Pad ("cd", V.Data (2)'Length);
      C_MixB.Valid
        ("MixB2", V, (16#4E#, 16#1C#, 16#47#, 16#1E#, 16#40#), 36,
         (16#4E#, 16#1C#, 16#47#, 16#1E#, 16#40#), 36);
   end;
   declare
      V : MixC := MixC_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("ab", V.Data (1)'Length);
      V.Data (2) := Pad ("c", V.Data (2)'Length);
      C_MixC.Valid
        ("MixCv", V, (16#9C#, 16#38#, 16#86#, 16#30#), 28,
         (16#9C#, 16#38#, 16#86#, 16#30#), 28);
   end;
   declare
      V : MixD := MixD_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("ab", V.Data (1)'Length);
      V.Data (2) := Pad ("abc", V.Data (2)'Length);
      C_MixD.Valid
        ("MixDv", V, (16#4E#, 16#1C#, 16#4B#, 16#0E#, 16#2C#, 16#60#), 43,
         (16#4E#, 16#1C#, 16#4B#, 16#0E#, 16#2C#, 16#60#), 43);
   end;
   declare
      V : MixE := MixE_Init;
   begin
      V.Length := 1;
      V.Data (1) := Pad ("ab", V.Data (1)'Length);
      C_MixE.Valid
        ("MixEv", V, (16#0E#, 16#1C#, 16#40#), 19,
         (16#0E#, 16#1C#, 16#40#), 19);
   end;
   declare
      V : LngT := LngT_Init;
   begin
      V.Length := 2;
      V.Data (1) := Pad ("a", V.Data (1)'Length);
      V.Data (2) := Pad ("a", V.Data (2)'Length);
      C_LngT.Valid
        ("LngTv", V, (16#58#, 16#58#, 16#40#), 18,
         (16#58#, 16#58#, 16#40#), 18);
   end;
   declare
      V : LngU := LngU_Init;
   begin
      V := Pad ("a", V'Length);
      C_LngU.Valid
        ("LngUv", V, (1 => 16#61#), 8,
         (1 => 16#61#), 8);
   end;
   declare
      V : LngW := LngW_Init;
   begin
      V.x := Pad ("a", V.x'Length);
      C_LngW.Valid
        ("LngWv", V, (1 => 16#61#), 8,
         (1 => 16#61#), 8);
   end;
   declare
      V : LngV := LngV_Init;
   begin
      V.x := Pad ("a", V.x'Length);
      C_LngV.Valid
        ("LngVv", V, (1 => 16#61#), 8,
         (1 => 16#61#), 8);
   end;
   C_OutV.Decode_A ("DecOutVba", (16#4E#, 16#1C#, 16#47#, 16#16#, 16#18#), ERR_OUTV_K_2, 37);
   C_OutI.Decode_A ("DecOutI", (16#7E#, 16#80#, 16#16#), ERR_OUTI_K_2, 23);
   C_OutE.Decode_A ("DecOutE", (1 => 16#4A#), ERR_OUTE_K_2, 7);
   C_OutK.Decode_U ("DecOutK", (16#AE#, 16#1C#, 16#58#, 16#F2#, 16#65#, 16#CD#, 16#70#, 16#E2#, 16#C7#, 16#93#, 16#2E#, 16#6B#, 16#87#, 16#16#, 16#3C#, 16#99#, 16#73#, 16#40#), ERR_OUTK_K_2, 138);
   C_OutS.Decode_U ("DecOutS", (16#AE#, 16#1C#, 16#58#, 16#F2#, 16#65#, 16#CD#, 16#F4#, 16#5C#, 16#38#, 16#B1#, 16#E4#, 16#CB#, 16#9B#, 16#E8#, 16#B8#, 16#71#, 16#63#, 16#C9#, 16#97#, 16#37#, 16#D1#), ERR_OUTS_K_2, 168);
   C_OptU.Decode_A ("DecOptUa", (16#8F#, 16#6E#), ERR_OPTU, 15);
   C_ChK.Decode_U ("DecChKk", (16#0B#, 16#0E#, 16#2C#, 16#60#), ERR_CHK, 27);
   C_MixA.Decode_U ("DecMixAm", (16#56#, 16#1C#, 16#58#, 16#C6#, 16#40#), ERR_MIXA_2, 36);
   C_LngT.Decode_A ("DecLngTm", (16#58#, 16#7E#, 16#BD#, 16#00#), ERR_LNGT_2, 25);
   Ada.Text_IO.Put_Line ("acn-wire 018 (Ada):34 invalid,35 valid,9 decode rows done");
end Wire_Test;
