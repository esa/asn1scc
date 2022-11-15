with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with ada_proxy; use ada_proxy;
with Ada.Long_Long_Integer_Text_IO;
with Interfaces;

procedure ada_main is

begin
if (Ada.Command_Line.Argument_Count/=1) then
  return;
end if;

if (Ada.Command_Line.Argument(1)="encode") then
  ada_proxy.proxy_encode;
elsif (Ada.Command_Line.Argument(1)="decode") then
  ada_proxy.proxy_decode;
else
  Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error,"Unrecognized option: "
    & Ada.Command_Line.Argument(1));
end if;

end ada_main;
