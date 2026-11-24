--  Checks of 017.asn1 / 017.acn in Ada (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, with and without
--  -gnata; Issue 171, Issue 172): the rows of 017_wire_test.c, decoded from streams of exactly REQUIRED_BYTES, with the
--  same result, error code and end bit position as C; valid rows encoded again give the same bytes. Before the fix the
--  decoders read past REQUIRED_BITS (-gnata: an RTL precondition or the decoder's postcondition failed; without it, a
--  later code / position, or FALSE with error code 0) and the value-set check called BitString_equal /
--  OctetString_equal with length 0 (RTL precondition).
with Ada.Command_Line;
with Ada.Text_IO;
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding;
with acn_wire_site_decode; use acn_wire_site_decode;
procedure Wire_Test is

   procedure Fail (Msg : String) is
   begin
      Ada.Text_IO.Put_Line ("FAILED: " & Msg);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Fail;

   generic
      type T is private;
      Size : Positive;
      with procedure Encode
        (V : T; S : out adaasn1rtl.encoding.Bitstream; R : out ASN1_RESULT);
      with procedure Decode
        (V : out T; S : in out adaasn1rtl.encoding.Bitstream; R : out ASN1_RESULT);
   procedure Row (Name : String; Src : OctetBuffer; Ok : Boolean; Err : Integer; Pos : Natural);

   --  decode Src from exactly Size octets (a longer row is cut): result, error code, end bit position; a valid row is encoded
   --  again into exactly Size octets and must give the row's bytes
   procedure Row (Name : String; Src : OctetBuffer; Ok : Boolean; Err : Integer; Pos : Natural) is
      S : adaasn1rtl.encoding.Bitstream (Size);
      V : T;
      R : ASN1_RESULT;
   begin
      S.Buffer := (others => 0);
      for I in Src'Range loop
         exit when I > S.Buffer'Last;
         S.Buffer (I) := Src (I);
      end loop;
      S.Current_Bit_Pos := 0;
      Decode (V, S, R);
      if R.Success /= Ok or else S.Current_Bit_Pos /= Pos
        or else (if Err < 0 then R.ErrorCode = 0 else R.ErrorCode /= Err)
      then
         Fail (Name & ": decode" & Boolean'Image (R.Success) & Integer'Image (R.ErrorCode) &
               Integer'Image (S.Current_Bit_Pos));
         return;
      end if;
      if Ok then
         Encode (V, S, R);
         if not R.Success or else S.Current_Bit_Pos /= Pos
           or else Src'Length /= (Pos + 7) / 8
           or else S.Buffer (1 .. Src'Length) /= Src
         then
            Fail (Name & ": encode");
         end if;
      end if;
   exception
      when others =>
         Fail (Name & ": exception");
   end Row;

   procedure U_OuterB4 is new Row (OuterB4, OuterB4_REQUIRED_BYTES_FOR_ENCODING, OuterB4_Encode, OuterB4_Decode);
   procedure A_OuterE is new Row (OuterE, OuterE_ACN_STREAM_SIZE, OuterE_ACN_Encode, OuterE_ACN_Decode);
   procedure A_OuterF is new Row (OuterF, OuterF_ACN_STREAM_SIZE, OuterF_ACN_Encode, OuterF_ACN_Decode);
   procedure A_OuterG is new Row (OuterG, OuterG_ACN_STREAM_SIZE, OuterG_ACN_Encode, OuterG_ACN_Decode);
   procedure A_OuterG2 is new Row (OuterG2, OuterG2_ACN_STREAM_SIZE, OuterG2_ACN_Encode, OuterG2_ACN_Decode);
   procedure A_OuterH is new Row (OuterH, OuterH_ACN_STREAM_SIZE, OuterH_ACN_Encode, OuterH_ACN_Decode);
   procedure A_OuterJ is new Row (OuterJ, OuterJ_ACN_STREAM_SIZE, OuterJ_ACN_Encode, OuterJ_ACN_Decode);
   procedure A_OuterK is new Row (OuterK, OuterK_ACN_STREAM_SIZE, OuterK_ACN_Encode, OuterK_ACN_Decode);
   procedure A_OuterM is new Row (OuterM, OuterM_ACN_STREAM_SIZE, OuterM_ACN_Encode, OuterM_ACN_Decode);
   procedure A_OuterM2 is new Row (OuterM2, OuterM2_ACN_STREAM_SIZE, OuterM2_ACN_Encode, OuterM2_ACN_Decode);
   procedure A_OuterM5 is new Row (OuterM5, OuterM5_ACN_STREAM_SIZE, OuterM5_ACN_Encode, OuterM5_ACN_Decode);
   procedure A_OuterN is new Row (OuterN, OuterN_ACN_STREAM_SIZE, OuterN_ACN_Encode, OuterN_ACN_Decode);
   procedure U_OuterO4 is new Row (OuterO4, OuterO4_REQUIRED_BYTES_FOR_ENCODING, OuterO4_Encode, OuterO4_Decode);
   procedure A_OuterQ is new Row (OuterQ, OuterQ_ACN_STREAM_SIZE, OuterQ_ACN_Encode, OuterQ_ACN_Decode);
   procedure A_OuterS is new Row (OuterS, OuterS_ACN_STREAM_SIZE, OuterS_ACN_Encode, OuterS_ACN_Decode);
   procedure A_PDU is new Row (PDU, PDU_ACN_STREAM_SIZE, PDU_ACN_Encode, PDU_ACN_Decode);

begin
   --  S_a: valid
   A_OuterS ("S_a", OctetBuffer'(1 => 16#18#, 2 => 16#60#), True, 0, 11);
   --  S_ab: valid
   A_OuterS ("S_ab", OctetBuffer'(1 => 16#38#, 2 => 16#71#, 3 => 16#00#), True, 0, 18);
   --  S_len3: length 3 > 2: rejected after the length field
   A_OuterS ("S_len3", OctetBuffer'(1 => 16#58#, 2 => 16#71#, 3 => 16#63#), False, ERR_ACN_DECODE_OUTERS_X_S, 3);
   --  S_len6: length 6
   A_OuterS ("S_len6", OctetBuffer'(1 => 16#B8#, 2 => 16#71#, 3 => 16#63#), False, ERR_ACN_DECODE_OUTERS_X_S, 3);
   --  S_len7: length 7, outside the PER-visible range too
   A_OuterS ("S_len7", OctetBuffer'(1 => 16#D8#, 2 => 16#71#, 3 => 16#63#), False, ERR_ACN_DECODE_OUTERS_X_S, 3);
   --  N_12: valid
   A_OuterN ("N_12", OctetBuffer'(1 => 16#12#, 2 => 16#38#), True, 0, 13);
   --  N_345: valid
   A_OuterN ("N_345", OctetBuffer'(1 => 16#24#, 2 => 16#56#, 3 => 16#00#), True, 0, 17);
   --  N_len4: length 4 (fits the stream slack)
   A_OuterN ("N_len4", OctetBuffer'(1 => 16#32#, 2 => 16#34#, 3 => 16#58#), False, ERR_ACN_DECODE_OUTERN_X_N, 4);
   --  N_len12: length 12
   A_OuterN ("N_len12", OctetBuffer'(1 => 16#B2#, 2 => 16#34#, 3 => 16#56#), False, ERR_ACN_DECODE_OUTERN_X_N, 4);
   --  P_ok: valid
   A_PDU ("P_ok", OctetBuffer'(1 => 16#12#, 2 => 16#3D#, 3 => 16#00#), True, 0, 17);
   --  P_len4: length 4
   A_PDU ("P_len4", OctetBuffer'(1 => 16#32#, 2 => 16#34#, 3 => 16#5C#), False, ERR_ACN_DECODE_OUTERN_X_N, 4);
   --  P_len5: length 5
   A_PDU ("P_len5", OctetBuffer'(1 => 16#42#, 2 => 16#34#, 3 => 16#56#), False, ERR_ACN_DECODE_OUTERN_X_N, 4);
   --  F_ab: valid
   A_OuterF ("F_ab", OctetBuffer'(1 => 16#58#, 2 => 16#71#, 3 => 16#40#), True, 0, 18);
   --  F_len0: length 0 < 1: the validation rejects it
   A_OuterF ("F_len0", OctetBuffer'(1 => 16#10#), False, ERR_OUTERF_X_2, 4);
   --  F_len3: length 3
   A_OuterF ("F_len3", OctetBuffer'(1 => 16#78#, 2 => 16#71#, 3 => 16#63#), False, ERR_ACN_DECODE_OUTERF_X_S, 3);
   --  F_len7: length 7
   A_OuterF ("F_len7", OctetBuffer'(1 => 16#F8#, 2 => 16#71#, 3 => 16#63#), False, ERR_ACN_DECODE_OUTERF_X_S, 3);
   --  E_empty: valid
   A_OuterE ("E_empty", OctetBuffer'(1 => 16#20#), True, 0, 3);
   --  E_c: valid
   A_OuterE ("E_c", OctetBuffer'(1 => 16#68#), True, 0, 5);
   --  E_len2: length 2
   A_OuterE ("E_len2", OctetBuffer'(1 => 16#98#), False, ERR_ACN_DECODE_OUTERE_X_S, 2);
   --  E_len3: length 3
   A_OuterE ("E_len3", OctetBuffer'(1 => 16#DB#), False, ERR_ACN_DECODE_OUTERE_X_S, 2);
   --  M_abc: valid
   A_OuterM ("M_abc", OctetBuffer'(1 => 16#16#, 2 => 16#1C#, 3 => 16#58#, 4 => 16#E0#), True, 0, 27);
   --  M_len1: length 1 < 3: the validation rejects it
   A_OuterM ("M_len1", OctetBuffer'(1 => 16#06#, 2 => 16#18#), False, ERR_OUTERM_X_2, 13);
   --  M_len5: length 5
   A_OuterM ("M_len5", OctetBuffer'(1 => 16#26#, 2 => 16#1C#, 3 => 16#58#, 4 => 16#F2#, 5 => 16#65#), False, ERR_ACN_DECODE_OUTERM_X_S, 5);
   --  M_len20: length 20
   A_OuterM ("M_len20", OctetBuffer'(1 => 16#9E#, 2 => 16#1C#, 3 => 16#58#, 4 => 16#F2#, 5 => 16#65#), False, ERR_ACN_DECODE_OUTERM_X_S, 5);
   --  M2_a: valid
   A_OuterM2 ("M2_a", OctetBuffer'(1 => 16#18#, 2 => 16#60#), True, 0, 11);
   --  M2_abcde: valid
   A_OuterM2 ("M2_abcde", OctetBuffer'(1 => 16#98#, 2 => 16#71#, 3 => 16#63#, 4 => 16#C9#, 5 => 16#96#), True, 0, 39);
   --  M2_abc: length 3, inside the hull: the validation rejects it
   A_OuterM2 ("M2_abc", OctetBuffer'(1 => 16#58#, 2 => 16#71#, 3 => 16#63#, 4 => 16#80#), False, ERR_OUTERM2_X_2, 25);
   --  M2_len6: length 6
   A_OuterM2 ("M2_len6", OctetBuffer'(1 => 16#B8#, 2 => 16#71#, 3 => 16#63#, 4 => 16#C9#, 5 => 16#94#), False, ERR_ACN_DECODE_OUTERM2_X_S, 3);
   --  M5_a: valid
   A_OuterM5 ("M5_a", OctetBuffer'(1 => 16#18#, 2 => 16#60#), True, 0, 11);
   --  M5_ab: valid
   A_OuterM5 ("M5_ab", OctetBuffer'(1 => 16#38#, 2 => 16#71#, 3 => 16#40#), True, 0, 18);
   --  M5_len3: length 3 > 2 (SIZE(MIN..2) = 1..2, Issue 173): rejected after the length field
   A_OuterM5 ("M5_len3", OctetBuffer'(1 => 16#58#, 2 => 16#71#, 3 => 16#63#), False, ERR_ACN_DECODE_OUTERM5_X_S, 3);
   --  M5_len6: length 6
   A_OuterM5 ("M5_len6", OctetBuffer'(1 => 16#B8#, 2 => 16#71#, 3 => 16#63#), False, ERR_ACN_DECODE_OUTERM5_X_S, 3);
   --  G_ab: valid
   A_OuterG ("G_ab", OctetBuffer'(1 => 16#02#, 2 => 16#C3#, 3 => 16#8A#), True, 0, 23);
   --  G_len1: length 1 < 2: the validation rejects it
   A_OuterG ("G_len1", OctetBuffer'(1 => 16#01#, 2 => 16#C3#), False, ERR_OUTERG_X_2, 16);
   --  G_len3: length 3
   A_OuterG ("G_len3", OctetBuffer'(1 => 16#03#, 2 => 16#C3#, 3 => 16#88#), False, ERR_ACN_DECODE_OUTERG_X_S, 8);
   --  G_C1: a 16K fragment
   A_OuterG ("G_C1", OctetBuffer'(1 => 16#C1#, 2 => 16#C3#, 3 => 16#88#), False, ERR_ACN_DECODE_OUTERG_X_S, 8);
   --  G_8080: a two-octet length 128
   A_OuterG ("G_8080", OctetBuffer'(1 => 16#80#, 2 => 16#80#), False, ERR_ACN_DECODE_OUTERG_X_S, 16);
   --  G2_200: valid
   A_OuterG2 ("G2_200", OctetBuffer'(1 => 16#80#, 2 => 16#C8#, 3 => 16#C3#, 4 => 16#87#, 5 => 16#0E#, 6 => 16#1C#, 7 => 16#38#, 8 => 16#70#, 9 => 16#E1#, 10 => 16#C3#, 11 => 16#87#, 12 => 16#0E#, 13 => 16#1C#, 14 => 16#38#, 15 => 16#70#, 16 => 16#E1#, 17 => 16#C3#, 18 => 16#87#, 19 => 16#0E#, 20 => 16#1C#, 21 => 16#38#, 22 => 16#70#, 23 => 16#E1#, 24 => 16#C3#, 25 => 16#87#, 26 => 16#0E#, 27 => 16#1C#, 28 => 16#38#, 29 => 16#70#, 30 => 16#E1#, 31 => 16#C3#, 32 => 16#87#, 33 => 16#0E#, 34 => 16#1C#, 35 => 16#38#, 36 => 16#70#, 37 => 16#E1#, 38 => 16#C3#, 39 => 16#87#, 40 => 16#0E#, 41 => 16#1C#, 42 => 16#38#, 43 => 16#70#, 44 => 16#E1#, 45 => 16#C3#, 46 => 16#87#, 47 => 16#0E#, 48 => 16#1C#, 49 => 16#38#, 50 => 16#70#, 51 => 16#E1#, 52 => 16#C3#, 53 => 16#87#, 54 => 16#0E#, 55 => 16#1C#, 56 => 16#38#, 57 => 16#70#, 58 => 16#E1#, 59 => 16#C3#, 60 => 16#87#, 61 => 16#0E#, 62 => 16#1C#, 63 => 16#38#, 64 => 16#70#, 65 => 16#E1#, 66 => 16#C3#, 67 => 16#87#, 68 => 16#0E#, 69 => 16#1C#, 70 => 16#38#, 71 => 16#70#, 72 => 16#E1#, 73 => 16#C3#, 74 => 16#87#, 75 => 16#0E#, 76 => 16#1C#, 77 => 16#38#, 78 => 16#70#, 79 => 16#E1#, 80 => 16#C3#, 81 => 16#87#, 82 => 16#0E#, 83 => 16#1C#, 84 => 16#38#, 85 => 16#70#, 86 => 16#E1#, 87 => 16#C3#, 88 => 16#87#, 89 => 16#0E#, 90 => 16#1C#, 91 => 16#38#, 92 => 16#70#, 93 => 16#E1#, 94 => 16#C3#, 95 => 16#87#, 96 => 16#0E#, 97 => 16#1C#, 98 => 16#38#, 99 => 16#70#, 100 => 16#E1#, 101 => 16#C3#, 102 => 16#87#, 103 => 16#0E#, 104 => 16#1C#, 105 => 16#38#, 106 => 16#70#, 107 => 16#E1#, 108 => 16#C3#, 109 => 16#87#, 110 => 16#0E#, 111 => 16#1C#, 112 => 16#38#, 113 => 16#70#, 114 => 16#E1#, 115 => 16#C3#, 116 => 16#87#, 117 => 16#0E#, 118 => 16#1C#, 119 => 16#38#, 120 => 16#70#, 121 => 16#E1#, 122 => 16#C3#, 123 => 16#87#, 124 => 16#0E#, 125 => 16#1C#, 126 => 16#38#, 127 => 16#70#, 128 => 16#E1#, 129 => 16#C3#, 130 => 16#87#, 131 => 16#0E#, 132 => 16#1C#, 133 => 16#38#, 134 => 16#70#, 135 => 16#E1#, 136 => 16#C3#, 137 => 16#87#, 138 => 16#0E#, 139 => 16#1C#, 140 => 16#38#, 141 => 16#70#, 142 => 16#E1#, 143 => 16#C3#, 144 => 16#87#, 145 => 16#0E#, 146 => 16#1C#, 147 => 16#38#, 148 => 16#70#, 149 => 16#E1#, 150 => 16#C3#, 151 => 16#87#, 152 => 16#0E#, 153 => 16#1C#, 154 => 16#38#, 155 => 16#70#, 156 => 16#E1#, 157 => 16#C3#, 158 => 16#87#, 159 => 16#0E#, 160 => 16#1C#, 161 => 16#38#, 162 => 16#70#, 163 => 16#E1#, 164 => 16#C3#, 165 => 16#87#, 166 => 16#0E#, 167 => 16#1C#, 168 => 16#38#, 169 => 16#70#, 170 => 16#E1#, 171 => 16#C3#, 172 => 16#87#, 173 => 16#0E#, 174 => 16#1C#, 175 => 16#38#, 176 => 16#70#, 177 => 16#E1#, 178 => 16#80#), True, 0, 1417);
   --  G2_199: length 199: the validation rejects it
   A_OuterG2 ("G2_199", OctetBuffer'(1 => 16#80#, 2 => 16#C7#, 3 => 16#C3#, 4 => 16#87#, 5 => 16#0E#, 6 => 16#1C#, 7 => 16#38#, 8 => 16#70#, 9 => 16#E1#, 10 => 16#C3#, 11 => 16#87#, 12 => 16#0E#, 13 => 16#1C#, 14 => 16#38#, 15 => 16#70#, 16 => 16#E1#, 17 => 16#C3#, 18 => 16#87#, 19 => 16#0E#, 20 => 16#1C#, 21 => 16#38#, 22 => 16#70#, 23 => 16#E1#, 24 => 16#C3#, 25 => 16#87#, 26 => 16#0E#, 27 => 16#1C#, 28 => 16#38#, 29 => 16#70#, 30 => 16#E1#, 31 => 16#C3#, 32 => 16#87#, 33 => 16#0E#, 34 => 16#1C#, 35 => 16#38#, 36 => 16#70#, 37 => 16#E1#, 38 => 16#C3#, 39 => 16#87#, 40 => 16#0E#, 41 => 16#1C#, 42 => 16#38#, 43 => 16#70#, 44 => 16#E1#, 45 => 16#C3#, 46 => 16#87#, 47 => 16#0E#, 48 => 16#1C#, 49 => 16#38#, 50 => 16#70#, 51 => 16#E1#, 52 => 16#C3#, 53 => 16#87#, 54 => 16#0E#, 55 => 16#1C#, 56 => 16#38#, 57 => 16#70#, 58 => 16#E1#, 59 => 16#C3#, 60 => 16#87#, 61 => 16#0E#, 62 => 16#1C#, 63 => 16#38#, 64 => 16#70#, 65 => 16#E1#, 66 => 16#C3#, 67 => 16#87#, 68 => 16#0E#, 69 => 16#1C#, 70 => 16#38#, 71 => 16#70#, 72 => 16#E1#, 73 => 16#C3#, 74 => 16#87#, 75 => 16#0E#, 76 => 16#1C#, 77 => 16#38#, 78 => 16#70#, 79 => 16#E1#, 80 => 16#C3#, 81 => 16#87#, 82 => 16#0E#, 83 => 16#1C#, 84 => 16#38#, 85 => 16#70#, 86 => 16#E1#, 87 => 16#C3#, 88 => 16#87#, 89 => 16#0E#, 90 => 16#1C#, 91 => 16#38#, 92 => 16#70#, 93 => 16#E1#, 94 => 16#C3#, 95 => 16#87#, 96 => 16#0E#, 97 => 16#1C#, 98 => 16#38#, 99 => 16#70#, 100 => 16#E1#, 101 => 16#C3#, 102 => 16#87#, 103 => 16#0E#, 104 => 16#1C#, 105 => 16#38#, 106 => 16#70#, 107 => 16#E1#, 108 => 16#C3#, 109 => 16#87#, 110 => 16#0E#, 111 => 16#1C#, 112 => 16#38#, 113 => 16#70#, 114 => 16#E1#, 115 => 16#C3#, 116 => 16#87#, 117 => 16#0E#, 118 => 16#1C#, 119 => 16#38#, 120 => 16#70#, 121 => 16#E1#, 122 => 16#C3#, 123 => 16#87#, 124 => 16#0E#, 125 => 16#1C#, 126 => 16#38#, 127 => 16#70#, 128 => 16#E1#, 129 => 16#C3#, 130 => 16#87#, 131 => 16#0E#, 132 => 16#1C#, 133 => 16#38#, 134 => 16#70#, 135 => 16#E1#, 136 => 16#C3#, 137 => 16#87#, 138 => 16#0E#, 139 => 16#1C#, 140 => 16#38#, 141 => 16#70#, 142 => 16#E1#, 143 => 16#C3#, 144 => 16#87#, 145 => 16#0E#, 146 => 16#1C#, 147 => 16#38#, 148 => 16#70#, 149 => 16#E1#, 150 => 16#C3#, 151 => 16#87#, 152 => 16#0E#, 153 => 16#1C#, 154 => 16#38#, 155 => 16#70#, 156 => 16#E1#, 157 => 16#C3#, 158 => 16#87#, 159 => 16#0E#, 160 => 16#1C#, 161 => 16#38#, 162 => 16#70#, 163 => 16#E1#, 164 => 16#C3#, 165 => 16#87#, 166 => 16#0E#, 167 => 16#1C#, 168 => 16#38#, 169 => 16#70#, 170 => 16#E1#, 171 => 16#C3#, 172 => 16#87#, 173 => 16#0E#, 174 => 16#1C#, 175 => 16#38#, 176 => 16#70#, 177 => 16#C0#), False, ERR_OUTERG2_X_2, 1410);
   --  G2_201: length 201
   A_OuterG2 ("G2_201", OctetBuffer'(1 => 16#80#, 2 => 16#C9#, 3 => 16#C3#, 4 => 16#87#, 5 => 16#0E#, 6 => 16#1C#, 7 => 16#38#, 8 => 16#70#, 9 => 16#E1#, 10 => 16#C3#, 11 => 16#87#, 12 => 16#0E#, 13 => 16#1C#, 14 => 16#38#, 15 => 16#70#, 16 => 16#E1#, 17 => 16#C3#, 18 => 16#87#, 19 => 16#0E#, 20 => 16#1C#, 21 => 16#38#, 22 => 16#70#, 23 => 16#E1#, 24 => 16#C3#, 25 => 16#87#, 26 => 16#0E#, 27 => 16#1C#, 28 => 16#38#, 29 => 16#70#, 30 => 16#E1#, 31 => 16#C3#, 32 => 16#87#, 33 => 16#0E#, 34 => 16#1C#, 35 => 16#38#, 36 => 16#70#, 37 => 16#E1#, 38 => 16#C3#, 39 => 16#87#, 40 => 16#0E#, 41 => 16#1C#, 42 => 16#38#, 43 => 16#70#, 44 => 16#E1#, 45 => 16#C3#, 46 => 16#87#, 47 => 16#0E#, 48 => 16#1C#, 49 => 16#38#, 50 => 16#70#, 51 => 16#E1#, 52 => 16#C3#, 53 => 16#87#, 54 => 16#0E#, 55 => 16#1C#, 56 => 16#38#, 57 => 16#70#, 58 => 16#E1#, 59 => 16#C3#, 60 => 16#87#, 61 => 16#0E#, 62 => 16#1C#, 63 => 16#38#, 64 => 16#70#, 65 => 16#E1#, 66 => 16#C3#, 67 => 16#87#, 68 => 16#0E#, 69 => 16#1C#, 70 => 16#38#, 71 => 16#70#, 72 => 16#E1#, 73 => 16#C3#, 74 => 16#87#, 75 => 16#0E#, 76 => 16#1C#, 77 => 16#38#, 78 => 16#70#, 79 => 16#E1#, 80 => 16#C3#, 81 => 16#87#, 82 => 16#0E#, 83 => 16#1C#, 84 => 16#38#, 85 => 16#70#, 86 => 16#E1#, 87 => 16#C3#, 88 => 16#87#, 89 => 16#0E#, 90 => 16#1C#, 91 => 16#38#, 92 => 16#70#, 93 => 16#E1#, 94 => 16#C3#, 95 => 16#87#, 96 => 16#0E#, 97 => 16#1C#, 98 => 16#38#, 99 => 16#70#, 100 => 16#E1#, 101 => 16#C3#, 102 => 16#87#, 103 => 16#0E#, 104 => 16#1C#, 105 => 16#38#, 106 => 16#70#, 107 => 16#E1#, 108 => 16#C3#, 109 => 16#87#, 110 => 16#0E#, 111 => 16#1C#, 112 => 16#38#, 113 => 16#70#, 114 => 16#E1#, 115 => 16#C3#, 116 => 16#87#, 117 => 16#0E#, 118 => 16#1C#, 119 => 16#38#, 120 => 16#70#, 121 => 16#E1#, 122 => 16#C3#, 123 => 16#87#, 124 => 16#0E#, 125 => 16#1C#, 126 => 16#38#, 127 => 16#70#, 128 => 16#E1#, 129 => 16#C3#, 130 => 16#87#, 131 => 16#0E#, 132 => 16#1C#, 133 => 16#38#, 134 => 16#70#, 135 => 16#E1#, 136 => 16#C3#, 137 => 16#87#, 138 => 16#0E#, 139 => 16#1C#, 140 => 16#38#, 141 => 16#70#, 142 => 16#E1#, 143 => 16#C3#, 144 => 16#87#, 145 => 16#0E#, 146 => 16#1C#, 147 => 16#38#, 148 => 16#70#, 149 => 16#E1#, 150 => 16#C3#, 151 => 16#87#, 152 => 16#0E#, 153 => 16#1C#, 154 => 16#38#, 155 => 16#70#, 156 => 16#E1#, 157 => 16#C3#, 158 => 16#87#, 159 => 16#0E#, 160 => 16#1C#, 161 => 16#38#, 162 => 16#70#, 163 => 16#E1#, 164 => 16#C3#, 165 => 16#87#, 166 => 16#0E#, 167 => 16#1C#, 168 => 16#38#, 169 => 16#40#), False, ERR_ACN_DECODE_OUTERG2_X_S, 16);
   --  H_a: valid
   A_OuterH ("H_a", OctetBuffer'(1 => 16#0C#, 2 => 16#30#), True, 0, 12);
   --  H_z: valid
   A_OuterH ("H_z", OctetBuffer'(1 => 16#C0#), True, 0, 2);
   --  H_len3: length 3
   A_OuterH ("H_len3", OctetBuffer'(1 => 16#2C#, 2 => 16#38#, 3 => 16#B1#, 4 => 16#80#), False, ERR_ACN_DECODE_OUTERH_A_S, 4);
   --  J_ok: valid
   A_OuterJ ("J_ok", OctetBuffer'(1 => 16#18#, 2 => 16#67#, 3 => 16#0E#, 4 => 16#20#), True, 0, 29);
   --  J_2nd: second element, length 3
   A_OuterJ ("J_2nd", OctetBuffer'(1 => 16#18#, 2 => 16#6B#, 3 => 16#0E#, 4 => 16#2C#, 5 => 16#60#), False, ERR_ACN_DECODE_OUTERJ_ELM_S, 14);
   --  J_1st: first element, length 6
   A_OuterJ ("J_1st", OctetBuffer'(1 => 16#B8#, 2 => 16#71#, 3 => 16#63#, 4 => 16#C8#), False, ERR_ACN_DECODE_OUTERJ_ELM_S, 3);
   --  K_ok: valid
   A_OuterK ("K_ok", OctetBuffer'(1 => 16#46#, 2 => 16#13#, 3 => 16#87#, 4 => 16#14#), True, 0, 30);
   --  K_len3: element length 3
   A_OuterK ("K_len3", OctetBuffer'(1 => 16#16#, 2 => 16#1C#, 3 => 16#58#, 4 => 16#C0#), False, ERR_ACN_DECODE_OUTERK_K_ELM, 5);
   --  K_3rd: third element, length 6
   A_OuterK ("K_3rd", OctetBuffer'(1 => 16#8E#, 2 => 16#1C#, 3 => 16#47#, 4 => 16#0E#, 5 => 16#2B#, 6 => 16#87#, 7 => 16#10#), False, ERR_ACN_DECODE_OUTERK_K_ELM, 39);
   --  Q_ok: valid
   A_OuterQ ("Q_ok", OctetBuffer'(1 => 16#00#, 2 => 16#61#, 3 => 16#9C#, 4 => 16#38#, 5 => 16#A0#), True, 0, 35);
   --  Q_ok2: valid
   A_OuterQ ("Q_ok2", OctetBuffer'(1 => 16#28#, 2 => 16#E1#, 3 => 16#C5#, 4 => 16#01#, 5 => 16#18#, 6 => 16#60#, 7 => 16#40#, 8 => 16#60#), True, 0, 59);
   --  Q_w2len4: w2 length 4
   A_OuterQ ("Q_w2len4", OctetBuffer'(1 => 16#00#, 2 => 16#61#, 3 => 16#BC#, 4 => 16#38#, 5 => 16#B1#, 6 => 16#E4#), False, -1, 20);
   --  Q_w1len5: w1 length 5
   A_OuterQ ("Q_w1len5", OctetBuffer'(1 => 16#92#, 2 => 16#61#, 3 => 16#C5#, 4 => 16#8F#, 5 => 16#26#, 6 => 16#50#), False, -1, 9);
   --  B4_1010: valid
   U_OuterB4 ("B4_1010", OctetBuffer'(1 => 16#12#, 2 => 16#A0#), True, 0, 11);
   --  B4_len0: length 0: the value set rejects it
   U_OuterB4 ("B4_len0", OctetBuffer'(1 => 16#02#), False, ERR_OUTERB4_X_2, 7);
   --  B4_len3: length 3: the value set rejects it
   U_OuterB4 ("B4_len3", OctetBuffer'(1 => 16#0E#, 2 => 16#C0#), False, ERR_OUTERB4_X_2, 10);
   --  O4_0102: valid
   U_OuterO4 ("O4_0102", OctetBuffer'(1 => 16#20#, 2 => 16#10#, 3 => 16#28#), True, 0, 21);
   --  O4_len0: length 0: the value set rejects it
   U_OuterO4 ("O4_len0", OctetBuffer'(1 => 16#08#), False, ERR_OUTERO4_X_2, 5);
end Wire_Test;
