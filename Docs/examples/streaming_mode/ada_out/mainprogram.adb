pragma Style_Checks (Off);

with user_code;

function MainProgram return Integer is
begin
   user_code.streaming_mode_test;
   return 0;
end MainProgram;
