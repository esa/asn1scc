with adaasn1rtl.encoding;
use  adaasn1rtl.encoding;



package user_code is

   procedure fetch_data(bs : in out BitStream; prm : Integer);
   procedure push_data(bs : in out BitStream; prm : Integer);

   procedure streaming_mode_test;

end user_code;
