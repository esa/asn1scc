pragma Style_Checks (Off); -- turn off style checks
with Ada.Strings; use Ada.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Ada.Direct_IO;

with Ada.Characters.Latin_1;

with Interfaces; use Interfaces;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with adaasn1rtl;             use adaasn1rtl;

with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Integer_Text_IO;

package body XER_RTL is

   WORD_ID : constant Character := Character'Val (1);

   function Strlen (Str : in String) return Integer is
      ret : Integer := 0;
   begin
      ret :=
        Index (Str, To_Set (Ada.Characters.Latin_1.NUL), Outside, Backward);
      if ret = 0 then
         return Str'Length;
      end if;

      return ret;
   end Strlen;

   procedure BS_Append_String
     (Strm   : in out CharStream;
      str    : in     XString;
      Result :    out ASN1_RESULT)
   is
      len : Integer := Strlen (str);
   begin
      Result := (Success => False, ErrorCode => ERR_INSUFFICIENT_DATA);
      if (Strm.CurrentByte + len - 1 > Strm.Data'Last) then
         return;
      end if;

      Strm.Data (Strm.CurrentByte .. Strm.CurrentByte + len - 1) :=
        str (str'First .. str'First + len - 1);
      Strm.CurrentByte := Strm.CurrentByte + len;
      Result           := (Success => True, ErrorCode => 0);
   end BS_Append_String;

   procedure BS_PutSpace
     (Strm   : in out CharStream;
      level  : in     Integer;
      Result :    out ASN1_RESULT)
   is
   begin
      if Strm.EncodeWhiteSpace then
         BS_Append_String (Strm, level * "  ", Result);
      else
         Result := (Success => True, ErrorCode => 0);
      end if;
   end BS_PutSpace;

   procedure BS_PutNL (Strm : in out CharStream; Result : out ASN1_RESULT) is
   begin
      if Strm.EncodeWhiteSpace then
         BS_Append_String (Strm, "" & LF, Result);
      else
         Result := (Success => True, ErrorCode => 0);
      end if;
   end BS_PutNL;

   procedure Xer_EncodePrimitiveElement
     (Strm       : in out CharStream;
      elementTag : in     String;
      value      : in     String;
      Result     :    out ASN1_RESULT;
      level      :        Integer)
   is
   begin
      BS_PutSpace (Strm, level, Result);
      if not Result.Success then
         return;
      end if;

      if Strlen (value) /= 0 then
         BS_Append_String (Strm, "<" & elementTag & ">", Result);
         if not Result.Success then
            return;
         end if;

         BS_Append_String (Strm, value, Result);
         if not Result.Success then
            return;
         end if;

         BS_Append_String (Strm, "</" & elementTag & ">", Result);
         if not Result.Success then
            return;
         end if;
         BS_PutNL (Strm, Result);
      else
         BS_Append_String (Strm, "<" & elementTag & "/>", Result);
         if not Result.Success then
            return;
         end if;
         BS_PutNL (Strm, Result);
      end if;

   end Xer_EncodePrimitiveElement;

   procedure Xer_EncodeXmlHeader
     (Strm      : in out CharStream;
      XmlHeader : in     XString;
      Result    :    out ASN1_RESULT)
   is
   begin

      if XmlHeader = "" then
         BS_Append_String
           (Strm,
            "<?xml version=""1.0"" encoding=""UTF-8""?>",
            Result);
         if not Result.Success then
            return;
         end if;
         BS_PutNL (Strm, Result);
      else
         BS_Append_String (Strm, XmlHeader, Result);
         if not Result.Success then
            return;
         end if;
         BS_PutNL (Strm, Result);
      end if;
   end Xer_EncodeXmlHeader;

   procedure Xer_EncodeComment
     (Strm    : in out CharStream;
      comment : in     XString;
      Result  :    out ASN1_RESULT)
   is
   begin
      null;
   end Xer_EncodeComment;

   procedure Xer_EncodeInteger
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     Asn1Int;
      Result     :    out ASN1_RESULT;
      level      : in     Integer)
   is
   begin
      Xer_EncodePrimitiveElement
        (Strm,
         elementTag,
         Trim (value'Img, Ada.Strings.Both),
         Result,
         level);
   end Xer_EncodeInteger;

   procedure Xer_EncodeBoolean
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     Asn1Boolean;
      Result     :    out ASN1_RESULT;
      level      : in     Integer)
   is
   begin
      if elementTag = "" then
         if value then
            Xer_EncodePrimitiveElement (Strm, "true", "", Result, level);
         else
            Xer_EncodePrimitiveElement (Strm, "false", "", Result, level);
         end if;
      else
         if value then
            Xer_EncodePrimitiveElement
              (Strm,
               elementTag,
               "<true/>",
               Result,
               level);
         else
            Xer_EncodePrimitiveElement
              (Strm,
               elementTag,
               "<false/>",
               Result,
               level);
         end if;
      end if;
   end Xer_EncodeBoolean;

   procedure Xer_EncodeEnumerated
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     XString;
      Result     :    out ASN1_RESULT;
      level      : in     Integer)
   is
   begin
      if elementTag = "" then
         Xer_EncodePrimitiveElement (Strm, value, "", Result, level);
      else
         Xer_EncodePrimitiveElement
           (Strm,
            elementTag,
            "<" & value & "/>",
            Result,
            level);
      end if;
   end Xer_EncodeEnumerated;

   procedure Xer_EncodeReal
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     Asn1Real;
      Result     :    out ASN1_RESULT;
      level      : in     Integer)
   is
      s : String := Trim (value'Img, Ada.Strings.Both);
   begin
      for I in s'Range loop
         if s (I) = '+' then
            s (I) := '0';
         end if;
      end loop;

      Xer_EncodePrimitiveElement (Strm, elementTag, s, Result, level);
   end Xer_EncodeReal;

   procedure Xer_EncodeString
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     XString;
      Result     :    out ASN1_RESULT;
      level      : in     Integer)
   is
   begin
      Xer_EncodePrimitiveElement (Strm, elementTag, value, Result, level);
   end Xer_EncodeString;

   procedure Xer_EncodeOctetString
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     OctetBuffer;
      len        : in     Integer;
      Result     :    out ASN1_RESULT;
      level      : in     Integer)
   is
      str    : String := 10 * ' ';
      str2   : String := 2 * '0';
      i1, i2 : Integer;
   begin
      BS_PutSpace (Strm, level, Result);
      if not Result.Success then
         return;
      end if;

      BS_Append_String (Strm, "<" & elementTag & ">", Result);
      if not Result.Success then
         return;
      end if;

      for I in value'First .. len loop
         Ada.Integer_Text_IO.Put
           (To   => str,
            Item => Integer (value (I)),
            Base => 16);
         i1 := Index (str, "#", Forward) + 1;
         i2 := Index (str, "#", Backward) - 1;

         str2 := 2 * '0';
         if i2 - i1 = 1 then
            str2 := str (i1 .. i2);
         elsif i2 = i1 then
            str2 (2 .. 2) := str (i1 .. i2);
         else
            raise Program_Error;
         end if;

         BS_Append_String (Strm, str2, Result);
         if not Result.Success then
            return;
         end if;
      end loop;
      BS_Append_String (Strm, "</" & elementTag & ">", Result);
      if not Result.Success then
         return;
      end if;
      BS_PutNL (Strm, Result);

      if not Result.Success then
         return;
      end if;

   end Xer_EncodeOctetString;

   procedure Xer_EncodeBitString
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     BitArray;
      len        : in     Integer;
      Result     :    out ASN1_RESULT;
      level      : in     Integer)
   is
   begin
      BS_PutSpace (Strm, level, Result);
      if not Result.Success then
         return;
      end if;

      BS_Append_String (Strm, "<" & elementTag & ">", Result);
      if not Result.Success then
         return;
      end if;

      for I in value'First .. len loop
         if value (I) = 1 then
            BS_Append_String (Strm, "1", Result);
         else
            BS_Append_String (Strm, "0", Result);
         end if;
         if not Result.Success then
            return;
         end if;
      end loop;
      BS_Append_String (Strm, "</" & elementTag & ">", Result);
      if not Result.Success then
         return;
      end if;
      BS_PutNL (Strm, Result);

      if not Result.Success then
         return;
      end if;
   end Xer_EncodeBitString;

   type Token is record
      TokenID     : Character         := Character'Val (0);
      ValueLength : Integer           := 0;
      Value       : String (1 .. 100) := (1 .. 100 => ' ');
   end record;

   function IsPartOfID (C : in Character) return Boolean is
   begin
      if Is_Alphanumeric (C) or C = '.' or C = '+' or C = '-' or C = '_' then
         return True;
      end if;

      return False;
   end IsPartOfID;

   function Is_Space (c : in Character) return Boolean is
   begin
      if c = ' ' or c = HT or c = LF or c = CR then
         return True;
      end if;
      return False;
   end Is_Space;

   procedure NT (Strm : in out CharStream; tok : out Token) is
      spTokens : String := "<>/=""";
      s1       : String := " ";
   begin
      while Is_Space (Strm.Data (Strm.CurrentByte)) loop
         Strm.CurrentByte := Strm.CurrentByte + 1;
      end loop;

      if Strm.CurrentByte + 1 <= Strm.Data'Last then
         if Strm.Data (Strm.CurrentByte) = '<' and
           Strm.Data (Strm.CurrentByte + 1) = '?'
         then
            Strm.CurrentByte := Strm.CurrentByte + 1;
            while not
              (Strm.Data (Strm.CurrentByte - 1) = '?' and
               Strm.Data (Strm.CurrentByte) = '>')
            loop
               Strm.CurrentByte := Strm.CurrentByte + 1;
            end loop;
            Strm.CurrentByte := Strm.CurrentByte + 1;

            while Is_Space (Strm.Data (Strm.CurrentByte)) loop
               Strm.CurrentByte := Strm.CurrentByte + 1;
            end loop;
         end if;
      end if;

      if Strm.CurrentByte + 2 <= Strm.Data'Last then
         if Strm.Data (Strm.CurrentByte) = '<' and
           Strm.Data (Strm.CurrentByte + 1) = '!' and
           Strm.Data (Strm.CurrentByte + 2) = '-' and
           Strm.Data (Strm.CurrentByte + 1) = '-'
         then
            Strm.CurrentByte := Strm.CurrentByte + 2;
            while not
              (Strm.Data (Strm.CurrentByte - 2) = '-' and
               Strm.Data (Strm.CurrentByte - 1) = '-' and
               Strm.Data (Strm.CurrentByte) = '>')
            loop
               Strm.CurrentByte := Strm.CurrentByte + 1;
            end loop;
            Strm.CurrentByte := Strm.CurrentByte + 1;

            while Is_Space (Strm.Data (Strm.CurrentByte)) loop
               Strm.CurrentByte := Strm.CurrentByte + 1;
            end loop;
         end if;
      end if;

      s1 (1) := Strm.Data (Strm.CurrentByte);

      if Index (spTokens, s1, Forward) /= 0 then
         tok.TokenID      := Strm.Data (Strm.CurrentByte);
         Strm.CurrentByte := Strm.CurrentByte + 1;
         return;
      end if;

      tok.ValueLength := 1;
      while IsPartOfID (Strm.Data (Strm.CurrentByte)) loop
         tok.TokenID                 := WORD_ID;
         tok.Value (tok.ValueLength) := Strm.Data (Strm.CurrentByte);
         Strm.CurrentByte            := Strm.CurrentByte + 1;
         tok.ValueLength             := tok.ValueLength + 1;
      end loop;

   end NT;

   procedure LA (Strm : in out CharStream; tok : out Token) is
      I : Integer := Strm.CurrentByte;
   begin
      NT (Strm, tok);
      Strm.CurrentByte := I;
   end LA;

   procedure Xer_DecodePrimitiveElement
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out XString;
      valLength  :    out Integer;
      Result     :    out ASN1_RESULT)
   is
      tok : Token;
   begin
      Result    := (Success => False, ErrorCode => ERR_INCORRECT_STREAM);
      valLength := 0;

      NT (Strm, tok);
      if tok.TokenID /= '<' then
         return;
      end if;

      NT (Strm, tok);
      if tok.TokenID /= WORD_ID or tok.ValueLength = 0 then
         return;
      end if;
      if Trim (tok.Value, Ada.Strings.Both) /=
        Trim (elementTag, Ada.Strings.Both)
      then
         return;
      end if;

      NT (Strm, tok);
      if tok.TokenID = '/' then
         NT (Strm, tok);
         if tok.TokenID = '>' then
            Result := (Success => True, ErrorCode => 0);
            return;
         end if;
      else
         if tok.TokenID /= '>' then
            return;
         end if;
      end if;

      valLength := 0;
      while Strm.CurrentByte <= Strm.Data'Last and
        Strm.Data (Strm.CurrentByte) /= '<'
      loop
         value (value'First + valLength) := Strm.Data (Strm.CurrentByte);
         Strm.CurrentByte                := Strm.CurrentByte + 1;
         valLength                       := valLength + 1;
      end loop;

      NT (Strm, tok);
      if tok.TokenID /= '<' then
         return;
      end if;

      NT (Strm, tok);
      if tok.TokenID /= '/' then
         return;
      end if;

      NT (Strm, tok);
      if tok.TokenID /= WORD_ID or tok.ValueLength = 0 then
         return;
      end if;
      if Trim (tok.Value, Ada.Strings.Both) /=
        Trim (elementTag, Ada.Strings.Both)
      then
         return;
      end if;

      NT (Strm, tok);
      if tok.TokenID /= '>' then
         return;
      end if;

      Result := (Success => True, ErrorCode => 0);
   end Xer_DecodePrimitiveElement;

   procedure Xer_DecodeComplexElementStart
     (Strm       : in out CharStream;
      elementTag : in     XString;
      Result     :    out ASN1_RESULT)
   is
      tok : Token;
   begin
      Result := (Success => False, ErrorCode => ERR_INCORRECT_STREAM);

      NT (Strm, tok);
      if tok.TokenID /= '<' then
         return;
      end if;

      NT (Strm, tok);
      if tok.TokenID /= WORD_ID or tok.ValueLength = 0 then
         return;
      end if;
      if Trim (tok.Value, Ada.Strings.Both) /=
        Trim (elementTag, Ada.Strings.Both)
      then
         return;
      end if;

      NT (Strm, tok);
      if tok.TokenID = '/' then
         NT (Strm, tok);
         if tok.TokenID = '>' then
            Result := (Success => True, ErrorCode => 0);
            return;
         end if;
      else
         if tok.TokenID /= '>' then
            return;
         end if;
      end if;

      Result := (Success => True, ErrorCode => 0);
   end Xer_DecodeComplexElementStart;

   procedure Xer_DecodeComplexElementEnd
     (Strm       : in out CharStream;
      elementTag : in     XString;
      Result     :    out ASN1_RESULT)
   is
      tok : Token;
   begin
      Result := (Success => False, ErrorCode => ERR_INCORRECT_STREAM);

      NT (Strm, tok);
      if tok.TokenID /= '<' then
         return;
      end if;

      NT (Strm, tok);
      if tok.TokenID /= '/' then
         return;
      end if;

      NT (Strm, tok);
      if tok.TokenID /= WORD_ID or tok.ValueLength = 0 then
         return;
      end if;
      if Trim (tok.Value, Ada.Strings.Both) /=
        Trim (elementTag, Ada.Strings.Both)
      then
         return;
      end if;

      NT (Strm, tok);
      if tok.TokenID /= '>' then
         return;
      end if;

      Result := (Success => True, ErrorCode => 0);
   end Xer_DecodeComplexElementEnd;

   procedure Xer_DecodeInteger
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out Asn1Int;
      Result     :    out ASN1_RESULT)
   is
      str : XString (1 .. 100) := (1 .. 100 => ' ');
      len : Integer;
   begin
      value := 0;
      Xer_DecodePrimitiveElement (Strm, elementTag, str, len, Result);
      if not Result.Success then
         return;
      end if;
      value  := Asn1Int'Value (str);
      Result := (Success => True, ErrorCode => 0);
   end Xer_DecodeInteger;

   procedure Xer_DecodeBoolean
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out Asn1Boolean;
      Result     :    out ASN1_RESULT)
   is
      str : XString (1 .. 100) := (1 .. 100 => ' ');
      len : Integer;
   begin
      Result := (Success => False, ErrorCode => ERR_INCORRECT_STREAM);
      value  := False;
      if elementTag /= "" then
         Xer_DecodeComplexElementStart (Strm, elementTag, Result);
         if not Result.Success then
            return;
         end if;
      end if;

      Xer_LA_NextElementTag (Strm, str, Result);
      if not Result.Success then
         return;
      end if;

      if Trim (str, Ada.Strings.Both) = "true" then
         value := True;
         Xer_DecodePrimitiveElement (Strm, "true", str, len, Result);
         if not Result.Success then
            return;
         end if;
      else
         value := False;
         Xer_DecodePrimitiveElement (Strm, "false", str, len, Result);
         if not Result.Success then
            return;
         end if;
      end if;

      if elementTag /= "" then
         Xer_DecodeComplexElementEnd (Strm, elementTag, Result);
         if not Result.Success then
            return;
         end if;
      end if;

   end Xer_DecodeBoolean;

   procedure Xer_DecodeEnumerated
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out XString;
      Result     :    out ASN1_RESULT)
   is
      str : XString (1 .. 100) := (1 .. 100 => ' ');
      len : Integer;
   begin
      Result := (Success => False, ErrorCode => ERR_INCORRECT_STREAM);

      if elementTag /= "" then
         Xer_DecodeComplexElementStart (Strm, elementTag, Result);
         if not Result.Success then
            return;
         end if;
      end if;

      Xer_LA_NextElementTag (Strm, value, Result);
      if not Result.Success then
         return;
      end if;

      Xer_DecodePrimitiveElement (Strm, value, str, len, Result);
      if not Result.Success then
         return;
      end if;

      if elementTag /= "" then
         Xer_DecodeComplexElementEnd (Strm, elementTag, Result);
         if not Result.Success then
            return;
         end if;
      end if;
   end Xer_DecodeEnumerated;

   procedure Xer_DecodeReal
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out Asn1Real;
      Result     :    out ASN1_RESULT)
   is
      str : XString (1 .. 100) := (1 .. 100 => ' ');
      len : Integer;
   begin
      value := 0.0;
      Xer_DecodePrimitiveElement (Strm, elementTag, str, len, Result);
      if not Result.Success then
         return;
      end if;
      value  := Asn1Real'Value (str);
      Result := (Success => True, ErrorCode => 0);
   end Xer_DecodeReal;

   procedure Xer_DecodeString
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out XString;
      Result     :    out ASN1_RESULT)
   is
      len : Integer;
   begin
      for I in value'Range loop
         value (I) := Character'Val (0);
      end loop;
      Xer_DecodePrimitiveElement (Strm, elementTag, value, len, Result);
   end Xer_DecodeString;

   function CharToNibble (c : Character) return Unsigned_8 is
   begin
      if c >= '0' and c <= '9' then
         return Unsigned_8 (Character'Pos (c) - Character'Pos ('0'));
      elsif c >= 'A' and c <= 'F' then
         return Unsigned_8 (Character'Pos (c) - Character'Pos ('A') + 10);
      elsif c >= 'a' and c <= 'f' then
         return Unsigned_8 (Character'Pos (c) - Character'Pos ('a') + 10);
      else
         raise Program_Error;
      end if;
   end CharToNibble;

   procedure Xer_DecodeOctetString
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out OctetBuffer;
      len        :    out Integer;
      Result     :    out ASN1_RESULT)
   is
      str  : XString (1 .. 32768) := (1 .. 32768 => ' ');
      len2 : Integer;
      J    : Integer              := 1;
      idx  : Integer;
   begin
      len := 0;
      Xer_DecodePrimitiveElement (Strm, elementTag, str, len2, Result);
      if not Result.Success then
         return;
      end if;
      for I in 1 .. len2 loop
         if not Is_Space (str (I)) then
            str (J) := str (I);
            J       := J + 1;
         end if;
      end loop;

      len2 := J - 1;

      idx := value'First;
      for I in 1 .. len2 loop
         if I mod 2 = 1 then
            value (idx) := Shift_Left (CharToNibble (str (I)), 4);
         else
            value (idx) := value (idx) or CharToNibble (str (I));
            idx         := idx + 1;
         end if;
      end loop;

      len := len2 / 2 + len2 mod 2;

      Result := (Success => True, ErrorCode => 0);
   end Xer_DecodeOctetString;

   procedure Xer_DecodeBitString
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out BitArray;
      len        :    out Integer;
      Result     :    out ASN1_RESULT)
   is
      str : XString (1 .. 32768) := (1 .. 32768 => ' ');
      J   : Integer              := 1;
   begin
      len := 0;
      Xer_DecodePrimitiveElement (Strm, elementTag, str, len, Result);
      if not Result.Success then
         return;
      end if;

      for I in 1 .. len loop
         if not Is_Space (str (I)) then
            str (J) := str (I);
            J       := J + 1;
         end if;
      end loop;

      len := J - 1;

      for I in 1 .. len loop
         if str (I) = '0' then
            value (value'First - 1 + I) := 0;
         else
            value (value'First - 1 + I) := 1;
         end if;
      end loop;
   end Xer_DecodeBitString;

   procedure Xer_EncodeComplexElementStart
     (Strm       : in out CharStream;
      elementTag : in     XString;
      Result     :    out ASN1_RESULT;
      level      : in     Integer)
   is
   begin
      BS_PutSpace (Strm, level, Result);
      if not Result.Success then
         return;
      end if;

      BS_Append_String (Strm, "<" & elementTag & ">", Result);
      if not Result.Success then
         return;
      end if;
      BS_PutNL (Strm, Result);

   end Xer_EncodeComplexElementStart;

   procedure Xer_EncodeComplexElementEnd
     (Strm       : in out CharStream;
      elementTag : in     XString;
      Result     :    out ASN1_RESULT;
      level      : in     Integer)
   is
   begin
      BS_PutSpace (Strm, level, Result);
      if not Result.Success then
         return;
      end if;

      BS_Append_String (Strm, "</" & elementTag & ">", Result);
      if not Result.Success then
         return;
      end if;
      BS_PutNL (Strm, Result);

   end Xer_EncodeComplexElementEnd;

   function Xer_NextEndElementIs
     (Strm       : in CharStream;
      elementTag : in XString) return Asn1Boolean
   is
      I : Integer := Strm.CurrentByte;
      J : Integer := elementTag'First;
   begin
      while Is_Space (Strm.Data (I)) loop
         I := I + 1;
      end loop;

      if Strm.Data (I) /= '<' then
         return False;
      end if;
      if Strm.Data (I + 1) /= '/' then
         return False;
      end if;

      I := I + 2;
      while Strm.Data (I) /= '>' loop
         if J > elementTag'Last then
            return False;
         end if;
         if I > Strm.Data'Last then
            return False;
         end if;
         if elementTag (J) /= Strm.Data (I) then
            return False;
         end if;
         I := I + 1;
         J := J + 1;
      end loop;

      return True;
   end Xer_NextEndElementIs;

   function Xer_NextStartElementIs
     (Strm       : in CharStream;
      elementTag : in XString) return Asn1Boolean
   is
      I : Integer := Strm.CurrentByte;
      J : Integer := elementTag'First;
   begin
      while Is_Space (Strm.Data (I)) loop
         I := I + 1;
      end loop;

      if Strm.Data (I) /= '<' then
         return False;
      end if;

      I := I + 1;
      while Strm.Data (I) /= '>' loop
         if J > elementTag'Last then
            return False;
         end if;
         if I > Strm.Data'Last then
            return False;
         end if;
         if elementTag (J) /= Strm.Data (I) then
            return False;
         end if;
         I := I + 1;
         J := J + 1;
      end loop;

      return True;
   end Xer_NextStartElementIs;

   procedure Xer_LA_NextElementTag
     (Strm       : in     CharStream;
      elementTag :    out XString;
      Result     :    out ASN1_RESULT)
   is
      I : Integer := Strm.CurrentByte;
      J : Integer := elementTag'First;
   begin
      Result := (Success => False, ErrorCode => ERR_INCORRECT_STREAM);

      while Is_Space (Strm.Data (I)) loop
         I := I + 1;
      end loop;

      if Strm.Data (I) /= '<' then
         return;
      end if;

      I := I + 1;
      while Strm.Data (I) /= '/' and Strm.Data (I) /= '>' loop
         if J > elementTag'Last then
            return;
         end if;
         if I > Strm.Data'Last then
            return;
         end if;
         elementTag (J) := Strm.Data (I);
         I              := I + 1;
         J              := J + 1;
      end loop;

      if Strm.Data (I) = '/' then
         I := I + 1;
         if I > Strm.Data'Last then
            return;
         end if;
         if Strm.Data (I) /= '>' then
            return;
         end if;
      end if;

      Result := (Success => True, ErrorCode => 0);
   end Xer_LA_NextElementTag;

--   begin
--      Int := Integer'Value(Numeric_String);
--   exception
--      when Constraint_Error =>
--         Put_Line(Numeric_String & " is not an Integer");
--   end;
--   begin
--      Flt := Float'Value(Numeric_String);
--   exception
--      when Constraint_Error =>
--         Put_Line(Numeric_String & " is not a float");
--   end;

   BER_AUX : constant array (Integer range 1 .. 8) of Asn1UInt :=
     (16#FF#,
      16#FF00#,
      16#FF0000#,
      16#FF000000#,
      16#FF00000000#,
      16#FF0000000000#,
      16#FF000000000000#,
      16#FF00000000000000#);

   -- The following functions are used to load an XML file (by removing white spaces) into an ByteStream

   package Ran_IO is new Ada.Direct_IO (Character);

   type STATE_ID is
     (XmlStart,
      XmlHeader,
      XmlStartTag,
      XmlContent,
      XmlEndTag,
      XmlMixedContent,
      XmlComment);

   PreviousState : STATE_ID := XmlStart;

   type FunType is access procedure
     (c       : in     Character;
      l1      : in     Character;
      Strm    : in out CharStream;
      tmpStrm : in out CharStream;
      I       : in out Ran_IO.Count;
      state   :    out STATE_ID;
      success :    out Boolean);

   procedure ByteStream_AppendChar
     (Strm    : in out CharStream;
      c       : in     Character;
      success :    out Boolean)
   is
      Result : ASN1_RESULT;
   begin
      BS_Append_String (Strm, "" & c, Result);
      success := Result.Success;
   end ByteStream_AppendChar;

   procedure OnXmlStart
     (c       : in     Character;
      l1      : in     Character;
      Strm    : in out CharStream;
      tmpStrm : in out CharStream;
      I       : in out Ran_IO.Count;
      state   :    out STATE_ID;
      success :    out Boolean)
   is
   begin
      success := True;

      if c /= '<' then
         state := XmlStart;
         return;
      end if;

      if l1 = '!' then
         state         := XmlComment;
         PreviousState := XmlStart;
         return;
      end if;

      if l1 = '?' then
         state := XmlHeader;
         return;
      end if;

      state := XmlStartTag;
      ByteStream_AppendChar (Strm, c, success);

   end OnXmlStart;

   procedure OnXmlHeader
     (c       : in     Character;
      l1      : in     Character;
      Strm    : in out CharStream;
      tmpStrm : in out CharStream;
      I       : in out Ran_IO.Count;
      state   :    out STATE_ID;
      success :    out Boolean)
   is
      use Ran_IO;
   begin
      success := True;
      if c = '?' and l1 = '>' then
         I     := I + 1;
         state := XmlStart;
      else
         state := XmlHeader;
      end if;

   end OnXmlHeader;

   procedure OnXmlStartTag
     (c       : in     Character;
      l1      : in     Character;
      Strm    : in out CharStream;
      tmpStrm : in out CharStream;
      I       : in out Ran_IO.Count;
      state   :    out STATE_ID;
      success :    out Boolean)
   is
   begin
      if c = '>' then
         state := XmlContent;
      else
         state := XmlStartTag;
      end if;

      ByteStream_AppendChar (Strm, c, success);
   end OnXmlStartTag;

   procedure OnXmlContent
     (c       : in     Character;
      l1      : in     Character;
      Strm    : in out CharStream;
      tmpStrm : in out CharStream;
      I       : in out Ran_IO.Count;
      state   :    out STATE_ID;
      success :    out Boolean)
   is
   begin
      success := True;
      if c /= '<' then
         state := XmlContent;
         ByteStream_AppendChar (tmpStrm, c, success);
      else
         if l1 = '!' then
            state         := XmlComment;
            PreviousState := XmlContent;
            return;
         end if;

         if l1 = '/' then
            state := XmlEndTag;
            --copy data from tmp buf to main steam
            for J in 1 .. tmpStrm.CurrentByte - 1 loop
               ByteStream_AppendChar (Strm, tmpStrm.Data (J), success);
               if not success then
                  return;
               end if;
            end loop;

            --Discard tmp buf
            tmpStrm.CurrentByte := 1;

         else
            state := XmlStartTag;
            -- Discard tmp buf
            tmpStrm.CurrentByte := 1;
         end if;

         ByteStream_AppendChar (Strm, c, success);
      end if;
   end OnXmlContent;

   procedure OnXmlEndTag
     (c       : in     Character;
      l1      : in     Character;
      Strm    : in out CharStream;
      tmpStrm : in out CharStream;
      I       : in out Ran_IO.Count;
      state   :    out STATE_ID;
      success :    out Boolean)
   is
   begin
      if c = '>' then
         state := XmlMixedContent;
      else
         state := XmlEndTag;
      end if;

      ByteStream_AppendChar (Strm, c, success);

   end OnXmlEndTag;

   procedure OnXmlMixedContent
     (c       : in     Character;
      l1      : in     Character;
      Strm    : in out CharStream;
      tmpStrm : in out CharStream;
      I       : in out Ran_IO.Count;
      state   :    out STATE_ID;
      success :    out Boolean)
   is
   begin
      success := True;
      if c /= '<' then
         state := XmlMixedContent;
      else
         if l1 = '!' then
            state         := XmlComment;
            PreviousState := XmlMixedContent;
            return;
         end if;

         if l1 = '/' then
            state := XmlEndTag;
         else
            state := XmlStartTag;
         end if;

         ByteStream_AppendChar (Strm, c, success);
      end if;
   end OnXmlMixedContent;

   procedure OnXmlComment
     (c       : in     Character;
      l1      : in     Character;
      Strm    : in out CharStream;
      tmpStrm : in out CharStream;
      I       : in out Ran_IO.Count;
      state   :    out STATE_ID;
      success :    out Boolean)
   is
      use Ran_IO;
   begin
      success := True;
      if c = '-' and l1 = '>' then
         I     := I + 1;
         state := PreviousState;
      else
         state := XmlComment;
      end if;

   end OnXmlComment;

   procedure LoadXmlFile
     (fileName    : in     XString;
      Strm        :    out CharStream;
      BytesLoaded :    out Integer;
      success     :    out Boolean)
   is
      use Ran_IO;

      file         : Ran_IO.File_Type;
      c            : Character;     -- current character
      l1           : Character;    -- next character (look ahead 1)
      I            : Ran_IO.Count                := 0;
      StateMachine : array (STATE_ID) of FunType :=
        (OnXmlStart'Access,
         OnXmlHeader'Access,
         OnXmlStartTag'Access,
         OnXmlContent'Access,
         OnXmlEndTag'Access,
         OnXmlMixedContent'Access,
         OnXmlComment'Access);
      curFunction : FunType;
      state       : STATE_ID := XmlStart;
      tmpStrm     : CharStream (16384);

      eofReached : Boolean := False;

   begin
      success     := True;
      BytesLoaded := 0;
      Ran_IO.Open (file, Ran_IO.In_File, fileName);

      while not Ran_IO.End_Of_File (file) loop
         Ran_IO.Read (file, c);
         if Ran_IO.End_Of_File (file) then
            l1         := c;
            eofReached := True;
         else
            I := Ran_IO.Index (file);
            Ran_IO.Read (file, l1); -- read look ahead character
            Ran_IO.Set_Index (file, I);
         end if;

         curFunction := StateMachine (state);
         curFunction (c, l1, Strm, tmpStrm, I, state, success);
         if not success then
            Ran_IO.Close (file);
            return;
         else
            if not eofReached then
               Ran_IO.Set_Index (file, I);
            end if;
         end if;

      end loop;

      BytesLoaded      := Strm.CurrentByte - 1;
      Strm.CurrentByte := 1;
      Ran_IO.Close (file);
   end LoadXmlFile;

end XER_RTL;
