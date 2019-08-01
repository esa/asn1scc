pragma Style_Checks (Off); -- turn off style checks
--with Interfaces; use Interfaces;

--with adaasn1rtl; use adaasn1rtl;

--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package adaasn1rtl.encoding.xer is

   subtype XString is String;

   type CharStream (N : Integer) is record
      CurrentByte      : Natural          := 1;
      Data             : XString (1 .. N) := (1 .. N => Character'Val (0));
      EncodeWhiteSpace : Boolean          := False;
   end record;

   procedure Write_CharStream_To_File (bs : in CharStream; Filename : in String);


   procedure Xer_EncodeXmlHeader
     (Strm      : in out CharStream;
      XmlHeader : in     XString;
      Result    :    out ASN1_RESULT);
   procedure Xer_EncodeComment
     (Strm    : in out CharStream;
      comment : in     XString;
      Result  :    out ASN1_RESULT);

   procedure Xer_EncodeInteger
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     Asn1Int;
      Result     :    out ASN1_RESULT;
      level      : in     Integer);

   procedure Xer_EncodePosInteger
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     Asn1UInt;
      Result     :    out ASN1_RESULT;
      level      : in     Integer);

   procedure Xer_EncodeNull (Strm : in out CharStream; elementTag : in XString; value: Asn1NullType; Result : out ASN1_RESULT; level : in Integer);


   procedure Xer_EncodeBoolean
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     Asn1Boolean;
      Result     :    out ASN1_RESULT;
      level      : in     Integer);
   procedure Xer_EncodeEnumerated
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     XString;
      Result     :    out ASN1_RESULT;
      level      : in     Integer);
   procedure Xer_EncodeReal
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     Asn1Real;
      Result     :    out ASN1_RESULT;
      level      : in     Integer);
   procedure Xer_EncodeString
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     XString;
      Result     :    out ASN1_RESULT;
      level      : in     Integer);
   procedure Xer_EncodeOctetString
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     OctetBuffer;
      len        : in     Integer;
      Result     :    out ASN1_RESULT;
      level      : in     Integer);
   procedure Xer_EncodeBitString
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     BitArray;
      len        : in     Integer;
      Result     :    out ASN1_RESULT;
      level      : in     Integer);

   procedure Xer_EncodeObjectIdentifier
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      : in     Asn1ObjectIdentifier;
      Result     :    out ASN1_RESULT;
      level      : in     Integer);



   procedure Xer_DecodeInteger
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out Asn1Int;
      Result     :    out ASN1_RESULT);

   procedure Xer_DecodePosInteger
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out Asn1UInt;
      Result     :    out ASN1_RESULT);

   procedure Xer_DecodeNull (Strm : in out CharStream; elementTag : in XString; value: out Asn1NullType; Result : out ASN1_RESULT);

   procedure Xer_DecodeBoolean
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out Asn1Boolean;
      Result     :    out ASN1_RESULT);
   procedure Xer_DecodeEnumerated
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out XString;
      Result     :    out ASN1_RESULT);
   procedure Xer_DecodeReal
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out Asn1Real;
      Result     :    out ASN1_RESULT);
   procedure Xer_DecodeString
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out XString;
      Result     :    out ASN1_RESULT);
   procedure Xer_DecodeOctetString
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out OctetBuffer;
      len        :    out Integer;
      Result     :    out ASN1_RESULT);
   procedure Xer_DecodeBitString
     (Strm       : in out CharStream;
      elementTag : in     XString;
      value      :    out BitArray;
      len        :    out Integer;
      Result     :    out ASN1_RESULT);

   procedure Xer_DecodeObjectIdentifier(Strm : in out CharStream; elementTag : in XString; value : out Asn1ObjectIdentifier; Result : out ASN1_RESULT);


   procedure Xer_EncodeComplexElementStart
     (Strm       : in out CharStream;
      elementTag : in     XString;
      Result     :    out ASN1_RESULT;
      level      : in     Integer);
   procedure Xer_EncodeComplexElementEnd
     (Strm       : in out CharStream;
      elementTag : in     XString;
      Result     :    out ASN1_RESULT;
      level      : in     Integer);
   procedure Xer_DecodeComplexElementStart
     (Strm       : in out CharStream;
      elementTag : in     XString;
      Result     :    out ASN1_RESULT);
   procedure Xer_DecodeComplexElementEnd
     (Strm       : in out CharStream;
      elementTag : in     XString;
      Result     :    out ASN1_RESULT);

   function Xer_NextEndElementIs
     (Strm       : in CharStream;
      elementTag : in XString) return Asn1Boolean;
   function Xer_NextStartElementIs
     (Strm       : in CharStream;
      elementTag : in XString) return Asn1Boolean;
   procedure Xer_LA_NextElementTag
     (Strm       : in     CharStream;
      elementTag :    out XString;
      Result     :    out ASN1_RESULT);

--     procedure LoadXmlFile
--       (fileName    : in     XString;
--        Strm        :    out CharStream;
--        BytesLoaded :    out Integer;
--        success     :    out Boolean);

end adaasn1rtl.encoding.xer;
