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

{$I rpconf.inc}

{$APPTYPE CONSOLE}

uses
  SysUtils,Classes,
{$IFDEF MSWINDOWS}
  midaslib,ActiveX,
  QThemed in '..\..\QThemed.pas',
  QThemeSrv in '..\..\QThemeSrv.pas',
  TmSchema in '..\..\TmSchema.pas',
  rpreport in '..\..\..\rpreport.pas',
  rpparams in '..\..\..\rpparams.pas',
  rpmdconsts in '..\..\..\rpmdconsts.pas',
  rptypes in '..\..\..\rptypes.pas',
  rpsubreport in '..\..\..\rpsubreport.pas',
  rpsection in '..\..\..\rpsection.pas',
  rppreview in '..\..\..\rppreview.pas',
  rpsecutil in '..\..\..\rpsecutil.pas',
  rprfparams in '..\..\..\rprfparams.pas',
  rpqtdriver in '..\..\..\rpqtdriver.pas';
{$ENDIF}

{$IFDEF LINUX}
  LibcExec in '../../LibcExec.pas',
  rpreport in '../../../rpreport.pas',
  rpparams in '../../../rpparams.pas',
  rpmdconsts in '../../../rpmdconsts.pas',
  rptypes in '../../../rptypes.pas',
  rpsubreport in '../../../rpsubreport.pas',
  rpsection in '../../../rpsection.pas',
  rppreview in '../../../rppreview.pas',
  rpsecutil in '../../../rpsecutil.pas',
  rprfparams in '../../../rprfparams.pas',
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
 preview,modified:boolean;
 pdialog:boolean;
 compress,doprint:boolean;

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
 Writeln(SRpPrintRep9);
 Writeln(SRpPrintRep10);
 Writeln(SRpPrintRep12);
 Writeln(SRpPrintPDFRep8);
 Writeln(SRpPrintPDFRep9);
 Writeln(SRpPrintRep14);
 Writeln(SRpParseParamsH);
 Writeln(SRpCommandLineStdIN);
end;

var
 isstdin:Boolean;
 memstream:TMemoryStream;
 topdf,tometafile,showparams:Boolean;
 outputfilename:String;

begin
 topdf:=false;
 tometafile:=false;
 outputfilename:='';
{$IFDEF USEADO}
  CoInitialize(nil);
{$ENDIF}
  isstdin:=false;
  showparams:=False;
  { TODO -oUser -cConsole Main : Insert code here }
  try
   if ParamCount<1 then
    PrintHelp
   else
   begin
   compress:=true;
   showprogress:=true;
   collate:=false;
   allpages:=true;
   preview:=false;
   pdialog:=false;
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
    if ParamStr(indexparam)='-preview' then
     preview:=true
    else
    if ParamStr(indexparam)='-showparams' then
     showparams:=true
    else
    if ParamStr(indexparam)='-pdialog' then
     pdialog:=true
    else
    if ParamStr(indexparam)='-u' then
     compress:=false
    else
    if ParamStr(indexparam)='-stdin' then
     isstdin:=true
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
    if ParamStr(indexparam)='-pdf' then
    begin
     topdf:=true;
    end
    else
    if ParamStr(indexparam)='-m' then
    begin
     tometafile:=true;
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
      if (isstdin or (Length(filename)>0)) then
      begin
       outputfilename:=ParamStr(indexparam);
       inc(indexparam);
       break;
      end
      else
      begin
       filename:=ParamStr(indexparam);
       if ((not topdf) and (not tometafile)) then
       begin
        inc(indexparam);
        break;
       end;
      end;
     end;
    inc(indexparam);
   end;
   if indexparam<ParamCount+1 then
   begin
    PrintHelp;
    Raise Exception.Create(SRpTooManyParams)
   end;
   if ((Length(filename)<1) and (not isstdin)) then
   begin
    PrintHelp;
   end
   else
   begin
    report:=TRpReport.Create(nil);
    try
     if isstdin then
     begin
      memstream:=ReadFromStdInputStream;
      try
       memstream.Seek(0,soFromBeginning);
       report.LoadFromStream(memstream);
      finally
       memstream.free;
      end;
     end
     else
      report.LoadFromFile(filename);
     if acopies=0 then
      copies:=report.Copies
     else
      copies:=acopies;
     ParseCommandLineParams(report.Params);
     doprint:=True;
     if showparams then
      doprint:=rprfparams.SHowUserParams(report.Params);
     if not preview then
      if pdialog then
       doprint:=rpqtdriver.DoShowPrintDialog(allpages,frompage,topage,copies,collate);
     if doprint then
     begin
      if topdf or tometafile then
      begin
       memstream:=TMemoryStream.Create;
       try
        rpqtdriver.ExportReportToPDFMetaStream(report,filename,showprogress,
         allpages,frompage,topage,pdialog,memstream,compress,collate,tometafile);
        memstream.Seek(0,soFromBeginning);
        if Length(outputfilename)>0 then
         memstream.SaveToFile(outputfilename)
        else
         WriteStreamToStdOutput(memstream);
       finally
        memstream.Free;
       end;
      end
      else
      if preview then
       rppreview.ShowPreview(report,filename,true,modified)
      else
      begin
       if pdialog then
        doprint:=rpqtdriver.DoShowPrintDialog(allpages,frompage,topage,copies,collate);
       if doprint then
        PrintReport(report,filename,showprogress,allpages,
         frompage,topage,copies,collate);
      end;
     end;
    finally
     report.free;
    end;
   end;
  end;
 except
  On E:Exception do
  begin
   WriteToStdError(E.Message+LINE_FEED);
   ExitCode:=1;
  end;
 end;
end.
