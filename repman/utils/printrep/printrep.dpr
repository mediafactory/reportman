{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       printrep                                        }
{                                                       }
{       Process and Prints a report                     }
{       you can select the pages to print               }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

program printrep;

{$APPTYPE CONSOLE}

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  midaslib,
  rpreport in '..\..\..\rpreport.pas',
  rpparams in '..\..\..\rpparams.pas',
  rpmdconsts in '..\..\..\rpmdconsts.pas',
  rptypes in '..\..\..\rptypes.pas',
  rpsubreport in '..\..\..\rpsubreport.pas',
  rpsection in '..\..\..\rpsection.pas',
  rpsecutil in '..\..\..\rpsecutil.pas',
  rpqtdriver in '..\..\..\rpqtdriver.pas';
{$ENDIF}

{$IFDEF LINUX}
  rpreport in '../../../rpreport.pas',
  rpparams in '../../../rpparams.pas',
  rpmdconsts in '../../../rpmdconsts.pas',
  rptypes in '../../../rptypes.pas',
  rpsubreport in '../../../rpsubreport.pas',
  rpsection in '../../../rpsection.pas',
  rpsecutil in '../../../rpsecutil.pas',
  rpqtdriver in '../../../rpqtdriver.pas';
{$ENDIF}

var
 report:TRpReport;
 indexparam:integer;
 showprogress:boolean;
 filename:string;
 allpages:boolean;
 frompage:integer;
 topage:integer;
 copies,acopies:integer;
 collate:boolean;

procedure PrintHelp;
begin
 Writeln(SRpPrintRep1+' '+RM_VERSION);
 Writeln(SRpPrintRep2);
 Writeln(SRpPrintRep3);
 Writeln(SRpPrintRep4);
 Writeln(SRpPrintRep5);
 Writeln(SRpPrintRep6);
 Writeln(SRpPrintRep7);
 Writeln(SRpPrintRep8);
 Writeln(SRpParseParamsH);
end;

begin
  { TODO -oUser -cConsole Main : Insert code here }
  try
   if ParamCount<1 then
    PrintHelp
   else
   begin
   showprogress:=true;
   collate:=false;
   allpages:=true;
   frompage:=1;
   acopies:=0;
   topage:=999999999;
   indexparam:=1;
   filename:='';
   // Get the options
   while indexparam<ParamCount+1 do
   begin
    if ParamStr(indexparam)='-q' then
     showprogress:=false
    else
    if ParamStr(indexparam)='-from' then
    begin
     inc(indexparam);
     if indexparam>=Paramcount+1 then
      Raise Exception.Create(SRpNumberexpected);
     frompage:=StrToInt(ParamStr(indexparam));
     allpages:=false;
    end
    else
     if ParamStr(indexparam)='-to' then
     begin
      inc(indexparam);
      if indexparam>=Paramcount+1 then
       Raise Exception.Create(SRpNumberexpected);
      topage:=StrToInt(ParamStr(indexparam));
      allpages:=false;
     end
     else
     if ParamStr(indexparam)='-copies' then
     begin
      inc(indexparam);
      if indexparam>=Paramcount+1 then
       Raise Exception.Create(SRpNumberexpected);
      acopies:=StrToInt(ParamStr(indexparam));
      if acopies<=0 then
       acopies:=1;
     end
     else
     if ParamStr(indexparam)='-collate' then
     begin
      collate:=true;
     end
     else
     if Pos('-param',ParamStr(indexparam))<>1 then
     begin
      filename:=ParamStr(indexparam);
      inc(indexparam);
      break;
     end;
    inc(indexparam);
   end;
   if indexparam<ParamCount+1 then
   begin
    Raise Exception.Create(SRpTooManyParams)
   end;
   if Length(filename)<1 then
   begin
    PrintHelp;
   end
   else
   begin
    report:=TRpReport.Create(nil);
    try
     report.LoadFromFile(filename);
     if acopies=0 then
      copies:=report.Copies
     else
      copies:=acopies;
     ParseCommandLineParams(report.Params);
     PrintReport(report,filename,showprogress,allpages,
      frompage,topage,copies,collate);
    finally
     report.free;
    end;
   end;
  end;
 except
  On E:Exception do
  begin
   WriteLn(E.Message);
   raise;
  end;
 end;
end.
