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
  rpsecutil in '..\..\..\rpsecutil.pas',
  rpqtdriver in '..\..\..\rpqtdriver.pas';
{$ENDIF}

{$IFDEF LINUX}
  rpreport in '../../../rpreport.pas',
  rpconsts in '../../../rpconsts.pas',
  rptypes in '../../../rptypes.pas',
  rpsubreport in '../../../rpsubreport.pas',
  rpsection in '../../../rpsection.pas',
  rpsecutil in '../../../rpsecutil.pas';
  rpqtdriver in '../../../rpqtdriver.pas';
{$ENDIF}

var
 report:TRpReport;

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
  begin
   report:=TRpReport.Create(nil);
   try
    report.LoadFromFile(ParamStr(1));
    if CalcReportWidthProgress(report) then
     PrintMetafile(report.Metafile,ParamStr(1),true);
   finally
    report.free;
   end;
  end;
end.
