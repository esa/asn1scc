with Ada.Assertions;
with Ada.Text_IO;
with adaasn1rtl;
with adaasn1rtl.encoding;
with TEST_CASE;

procedure Decoder_Contracts is
   use TEST_CASE;
   use type ASN1SCC_Item;
   use type adaasn1rtl.OctetBuffer;

   Result : adaasn1rtl.ASN1_RESULT;
   Value : ASN1SCC_Items := ASN1SCC_Items_Init;
   Decoded : ASN1SCC_Items;
   Required : ASN1SCC_RequiredItems;
   Encoded : ASN1SCC_Items_ACN_Stream;
   One : adaasn1rtl.encoding.Bitstream :=
     adaasn1rtl.encoding.BitStream_init (1);
   Excess : adaasn1rtl.encoding.Bitstream :=
     adaasn1rtl.encoding.BitStream_init (3);
   Item_Value : ASN1SCC_Item := 0;
   Rejected : Boolean;

   procedure Check (Condition : Boolean; Message : String);
   procedure Check (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         raise Program_Error with Message;
      end if;
   end Check;
begin
   --  Small and maximum values must work with an exactly bounded view.
   for Length in 1 .. 2 loop
      Value.Length := Length;
      Value.Data := (others => 42);
      ASN1SCC_Items_ACN_Encode (Value, Encoded, Result);
      Check (Result.Success, "encode valid items");
      declare
         View : adaasn1rtl.encoding.Bitstream :=
           adaasn1rtl.encoding.BitStream_init (Length);
      begin
         View.Buffer := Encoded.Buffer (1 .. Length);
         ASN1SCC_ItemsAliasChain_ACN_Decode_aux (Decoded, View, Result);
         Check (Result.Success and then
           ASN1SCC_Items_Equal (Value, Decoded), "bounded alias round trip");
         Check (View.Current_Bit_Pos = Length * 8, "bounded cursor");
      end;
   end loop;

   --  Positive discriminants represent an empty view using an end cursor.
   One.Buffer := (others => 165);
   One.Current_Bit_Pos := 8;
   ASN1SCC_Items_ACN_Decode_aux (Decoded, One, Result);
   Check (Result.Success and then Decoded.Length = 0, "empty list");
   Check (One.Current_Bit_Pos = 8 and then
     One.Buffer = adaasn1rtl.OctetBuffer'(1 => 165), "empty view untouched");
   ASN1SCC_RequiredItems_ACN_Decode_aux (Required, One, Result);
   Check (not Result.Success and then One.Current_Bit_Pos = 8,
          "empty required list must fail without reading");

   ASN1SCC_Items_ACN_Decode_aux (Decoded, Excess, Result);
   Check (not Result.Success and then Excess.Current_Bit_Pos = 0,
          "excess element count must fail before reading");

   One.Current_Bit_Pos := 0;
   One.Buffer := (others => 255);
   ASN1SCC_Items_ACN_Decode_aux (Decoded, One, Result);
   Check (not Result.Success and then One.Current_Bit_Pos = 8,
          "invalid element must return a decode error");

   --  An incomplete element is not read; a required element is missing.
   One.Current_Bit_Pos := 4;
   ASN1SCC_RequiredItems_ACN_Decode_aux (Required, One, Result);
   Check (not Result.Success and then One.Current_Bit_Pos = 4,
          "partial required element");

   --  Neither malformed cursors nor ordinary scalar/encoder calls gain
   --  permission to read/write beyond the physical buffer.
   Rejected := False;
   One.Current_Bit_Pos := 9;
   begin
      ASN1SCC_Items_ACN_Decode_aux (Decoded, One, Result);
   exception
      when Ada.Assertions.Assertion_Error => Rejected := True;
   end;
   Check (Rejected, "invalid cursor must violate the contract");
   Rejected := False;
   One.Current_Bit_Pos := 8;
   begin
      ASN1SCC_Item_ACN_Decode_aux (Item_Value, One, Result);
   exception
      when Ada.Assertions.Assertion_Error => Rejected := True;
   end;
   Check (Rejected and then Item_Value = 0,
          "scalar capacity contract retained");
   Rejected := False;
   One.Current_Bit_Pos := 0;
   Value.Length := 1;
   begin
      ASN1SCC_Items_ACN_Encode_aux (Value, One, Result);
   exception
      when Ada.Assertions.Assertion_Error => Rejected := True;
   end;
   Check (Rejected, "encoder maximum-capacity contract retained");
   Ada.Text_IO.Put_Line ("bounded decoder contracts OK");
end Decoder_Contracts;
