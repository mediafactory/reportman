program reportservercon;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  urepserver in 'urepserver.pas' {modserver: TDataModule};

var
 amod:TModServer;
begin
  IsMultiThread:=True;
  { TODO -oUser -cConsole Main : Insert code here }
  amod:=StartServer;
  while true do
  begin
   sleep(5000);
   WriteLn('Server Running');
  end;
end.
