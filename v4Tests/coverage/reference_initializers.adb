with Ada.Text_IO;
with TEST_CASE;

procedure Reference_Initializers is
   use TEST_CASE;
   use type ASN1SCC_MyArray_elem;

   procedure Check (Condition : Boolean; Message : String);
   procedure Check (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         raise Program_Error with Message;
      end if;
   end Check;

   Array_Value : constant ASN1SCC_MySeqArray := ASN1SCC_MySeqArray_Init;
   Pdu_Value : constant ASN1SCC_MyPDU := ASN1SCC_MyPDU_Init;
   Seq_Value : constant ASN1SCC_MyChoice := ASN1SCC_MyChoice_Init;
begin
   Check (ASN1SCC_MySeqArray_elem_Init = 1, "alias element initializer");
   Check (ASN1SCC_MyPDU_data_elem_Init = 1, "nested element initializer");
   Check (ASN1SCC_MyChoice_data1_elem_Init = 1, "first sibling initializer");
   Check (ASN1SCC_MyChoice_data2_elem_Init = 1, "second sibling initializer");
   Check (ASN1SCC_MySeqArray_IsConstraintValid (Array_Value).Success,
          "alias initializer must satisfy constraints");
   Check (ASN1SCC_MyPDU_IsConstraintValid (Pdu_Value).Success,
          "nested initializer must satisfy constraints");
   Check (ASN1SCC_MyChoice_IsConstraintValid (Seq_Value).Success,
          "sibling initializer must satisfy constraints");
   for I in 1 .. 20 loop
      Check (Array_Value.Data (I) = 1 and then Pdu_Value.data.Data (I) = 1
             and then Seq_Value.data1.Data (I) = 1
             and then Seq_Value.data2.Data (I) = 1,
             "parent and element initializers must agree");
   end loop;
   Ada.Text_IO.Put_Line ("reference initializer values and constraints OK");
end Reference_Initializers;
