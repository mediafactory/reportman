{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       printreptopdf                                   }
{                                                       }
{       Preoces and exports to pdf a report             }
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

program printreptopdf;

{$APPTYPE CONSOLE}

uses
  SysUtils,Classes,
{$IFDEF MSWINDOWS}
  midaslib,
  rpreport in '..\..\..\rpreport.pas',
  rpparams in '..\..\..\rpparams.pas',
  rpmdconsts in '..\..\..\rpmdconsts.pas',
  rptypes in '..\..\..\rptypes.pas',
  rpsubreport in '..\..\..\rpsubreport.pas',
  rpsection in '..\..\..\rpsection.pas',
  rpsecutil in '..\..\..\rpsecutil.pas',
  rppdfdriver in '..\..\..\rppdfdriver.pas';
{$ENDIF}

{$IFDEF LINUX}
  rpreport in '../../../rpreport.pas',
  rpparams in '../../../rpparams.pas',
  rpmdconsts in '../../../rpmdconsts.pas',
  rptypes in '../../../rptypes.pas',
  rpsubreport in '../../../rpsubreport.pas',
  rpsection in '../../../rpsection.pas',
  rpsecutil in '../../../rpsecutil.pas',
  rppdfdriver in '../../../rppdfdriver.pas';
{$ENDIF}

var
 report:TRpReport;
 indexparam:integer;
 showprogress:boolean;
 filename:string;
 pdffilename:string;
 allpages:boolean;
 frompage:integer;
 topage:integer;
 copies,acopies:integer;
 compress:boolean;
 collate:boolean;
 doprintmetafile:boolean;
 afstream:TFileStream;

procedure PrintHelp;
begin
 Writeln(SRpPrintPDFRep1+' '+RM_VERSION);
 Writeln(SRpPrintPDFRep2);
 Writeln(SRpPrintPDFRep3);
 Writeln(SRpPrintPDFRep4);
 Writeln(SRpPrintPDFRep5);
 Writeln(SRpPrintPDFRep6);
 Writeln(SRpPrintPDFRep7);
 Writeln(SRpPrintPDFRep8);
 Writeln(SRpPrintPDFRep9);
 Writeln(SRpPrintRep8);
 Writeln(SRpParseParamsH);
end;

begin
  doprintmetafile:=false;
  { TODO -oUser -cConsole Main : Insert code here }
  try
   if ParamCount<1 then
    PrintHelp
   else
   begin
   showprogress:=true;
   compress:=true;
   allpages:=true;
   frompage:=1;
   acopies:=0;
   topage:=999999999;
   collate:=false;
   indexparam:=1;
   filename:='';
   pdffilename:='';
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
     if ParamStr(indexparam)='-u' then
     begin
      compress:=false;
     end
     else
     if ParamStr(indexparam)='-m' then
     begin
      doprintmetafile:=true;
     end
     else
     if ParamStr(indexparam)='-collate' then
     begin
      collate:=true;
     end
     else
     begin
      if length(filename)>0 then
      begin
       pdffilename:=ParamStr(indexparam);
       inc(indexparam);
       break;
      end
      else
      begin
       filename:=ParamStr(indexparam);
      end;
     end;
    inc(indexparam);
   end;
   if indexparam<ParamCount+1 then
   begin
    Raise Exception.Create(SRpTooManyParams)
   end;
   if ((Length(filename)<1) or (Length(pdffilename)<1)) then
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
     if doprintmetafile then
     begin
      afstream:=TFileStream.Create(PDFfilename,fmCreate);
      try
       PrintReportMetafileStream(report,'',showprogress,allpages,frompage,topage,
        copies,afstream,compress,collate);
      finally
       afstream.free;
      end;
     end
     else
     begin
      PrintReportPDF(report,filename,showprogress,
        allpages,frompage,topage,copies,
         PDFfilename,compress,collate);
     end;
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
