program txttorep;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
{$IFDEF MSWINDOWS}
  rpwriter in '..\..\..\rpwriter.pas',
  rpconsts in '..\..\..\rpconsts.pas';
{$ENDIF}

{$IFDEF LINUX}
  rpwriter in '../../../rpwriter.pas',
  rpconsts in '../../../rpconsts.pas';
{$ENDIF}

procedure PrintHelp;
begin
 Writeln(SRpTxtToRep1);
 Writeln(SRpTxtToRep2);
 Writeln(SRpTxtToRep3);
end;

begin
  { TODO -oUser -cConsole Main : Insert code here }
  if ParamCount<>2 then
   PrintHelp
  else
   PlainTextToFileReport(ParamStr(1),ParamStr(2));
end.
