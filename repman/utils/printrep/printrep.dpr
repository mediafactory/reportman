program printrep;

{$APPTYPE CONSOLE}

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  rpreport in '..\..\..\rpreport.pas',
  rpconsts in '..\..\..\rpconsts.pas',
  rptypes in '..\..\..\rptypes.pas',
  rpsubreport in '..\..\..\rpsubreport.pas',
  rpsection in '..\..\..\rpsection.pas',
  rpsecutil in '..\..\..\rpsecutil.pas';
{$ENDIF}

{$IFDEF LINUX}
  rpreport in '../../../rpreport.pas',
  rpconsts in '../../../rpconsts.pas',
  rptypes in '../../../rptypes.pas',
  rpsubreport in '../../../rpsubreport.pas',
  rpsection in '../../../rpsection.pas',
  rpsecutil in '../../../rpsecutil.pas';
{$ENDIF}

procedure PrintHelp;
begin
 Writeln(SRpPrintRep1);
 Writeln(SRpPrintRep2);
 Writeln(SRpPrintRep3);
end;

begin
  { TODO -oUser -cConsole Main : Insert code here }
  if ParamCount<>1 then
   PrintHelp
  else
   PrintReportFile(ParamStr(1));
end.
