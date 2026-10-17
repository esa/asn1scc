package adaasn1rtl.encoding.test_cases_aux is

   procedure Write_BitStream_To_File (bs : Bitstream; Filename : String);

   --  Decode fault sweep of the automatic tests (asn1scc -atc, uPER and ACN).
   --  After a successful round trip, each byte of the encoded value
   --  (bytes 1 .. (bs.Current_Bit_Pos + 7) / 8) is set to 16#00#, then to
   --  16#FF#, decoded, and restored. Decode must not raise an exception; it
   --  must fail with a nonzero error code or succeed with a valid value.
   --  At most Sweep_Positions positions at each end of the encoding, and
   --  about Budget decoded bytes (asn1scc -atcSweepBudget); at least 2
   --  positions at each end. Stops at the first violation, prints it and
   --  returns (Success => False, ErrorCode => 7); bs.Current_Bit_Pos and
   --  bs.Buffer are restored.
   Sweep_Positions : constant := 128;

   procedure Decode_Corruption_Sweep
     (bs     : in out Bitstream;
      Name   : String;
      Budget : Long_Long_Integer;
      Decode : not null access procedure
        (bs    : in out Bitstream;
         Res   : out ASN1_RESULT;
         Valid : out Boolean);
      Result : out ASN1_RESULT);

end adaasn1rtl.encoding.test_cases_aux;
