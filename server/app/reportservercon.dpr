program reportservercon;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  urepserver in 'urepserver.pas' {modserver: TDataModule};

type
 TRpLogObject=class(TObject)
 private
  procedure OnLog(Sender:TObject;aMessage:WideString);
 public
 end;

 procedure TRpLogObject.OnLog(Sender:TObject;aMessage:WideString);
 begin
  Writeln(aMessage);
 end;


var
 amod:TModServer;
 logobject:TRpLogObject;
begin
  IsMultiThread:=True;
  { TODO -oUser -cConsole Main : Insert code here }
  logobject:=TRpLogObject.Create;
  try
   amod:=StartServer(logobject.OnLog);
   while true do
   begin
    sleep(5000);
    WriteLn('Server Running');
   end;
  finally
   logobject.free;
  end;
end.
