{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpdllutil                                       }
{       Exported functions for the Standarc C Library   }
{                                                       }
{       Copyright (c) 1994-2003 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rpdllutil;

{$I rpconf.inc}

interface

uses SysUtils,Classes,rpreport,rpmdconsts,rppdfdriver,
{$IFDEF USEVARIANTS}
 Variants,
{$ENDIF}
 rptypes,rpeval,rptypeval,rpdatainfo,rppdfreport,
 rpparams;

var
 lreports:TStringList;
 lasthandle:integer;
 rplasterror:String;

function rp_new:integer;stdcall;
function rp_open(filename:PChar):integer;stdcall;
function rp_execute(hreport:integer;outputfilename:PChar;metafile,
 compressed:integer):integer;stdcall;
function rp_executeremote(hostname:PChar;port:integer;user,password,aliasname,reportname:PChar;outputfilename:PChar;metafile,
 compressed:integer):integer;stdcall;
function rp_executeremote_report(hreport:integer;hostname:PChar;port:integer;user,password,aliasname,reportname:PChar;outputfilename:PChar;metafile,
 compressed:integer):integer;stdcall;
function rp_getremoteparams(hreport:integer;hostname:PChar;port:integer;user,password,aliasname,reportname:PChar):integer;stdcall;
function rp_close(hreport:integer):integer;stdcall;
function rp_lasterror:PChar;stdcall;
function rp_setparamvalue(hreport:integer;paramname:pchar;paramtype:integer;
 paramvalue:Pointer):integer;stdcall;
function rp_getparamcount(hreport:integer;var paramcount:Integer):integer;stdcall;
function rp_getparamname(hreport:integer;index:integer;
 abuffer:PChar):integer;stdcall;

{$IFDEF MSWINDOWS}
function rp_setparamvaluevar(hreport:integer;paramname:pchar;
 paramvalue:OleVariant):integer;stdcall;
function rp_setadoconnectionstring(hreport:integer;conname:pchar;
 constring:PChar):integer;stdcall;
{$ENDIF}

function FindReportIndex(hreport:integer):integer;
function FindReport(hreport:integer):TRpReport;
procedure rplibdoinit;
{$IFDEF LINUX}
procedure DLLHandler(Reason: Integer);
{$ENDIF}

implementation

procedure rplibdoinit;
var
 found:boolean;
 aclass:TPersistentClass;
begin
 if Assigned(lreports) then
  exit;
 lreports:=TStringList.Create;
 lreports.sorted:=True;
 lasthandle:=1;
 rplasterror:='';
 aclass:=GetClass('TRpReport');
 found:=true;
 if aclass=nil then
  found:=false;
 if found then
 begin
  rpreport.RegisterRpReportClasses;
  rptypeval.DefaultDecimals:=2;
 end;
end;


function FindReportIndex(hreport:integer):integer;
var
 index:integer;
begin
 rplibdoinit;
 index:=lreports.IndexOf(IntToStr(hreport));
 if index<0 then
  Raise Exception.Create(SRpSInvReportHandle+' - '+IntTostr(hreport));
 Result:=index;
end;

function FindReport(hreport:integer):TRpReport;
var
 index:integer;
begin
 rplibdoinit;
 index:=FindReportIndex(hreport);
 Result:=TRpReport(lreports.Objects[index]);
end;

function rp_open(filename:PChar):integer;
var
 report:TRpReport;
begin
 rplibdoinit;
 rplasterror:='';
 try
  report:=TRpReport.Create(nil);
  try
   report.LoadFromFile(filename);
   inc(lasthandle);
   Result:=lasthandle;
   lreports.AddObject(IntToStr(lasthandle),report);
  except
   on E:Exception do
   begin
    report.free;
    raise;
   end;
  end;
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;

function rp_new:integer;
var
 report:TRpReport;
begin
 rplibdoinit;
 rplasterror:='';
 try
  report:=TRpReport.Create(nil);
  try
   inc(lasthandle);
   Result:=lasthandle;
   lreports.AddObject(IntToStr(lasthandle),report);
  except
   on E:Exception do
   begin
    report.free;
    raise;
   end;
  end;
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;


function rp_execute(hreport:integer;outputfilename:PChar;metafile,compressed:integer):integer;
var
 report:TRpReport;
 acompressed:boolean;
begin
 rplibdoinit;
 rplasterror:='';
 Result:=1;
 try
  if compressed=0 then
   acompressed:=false
  else
   acompressed:=true;
  report:=FindReport(hreport);
  if metafile=0 then
  begin
   rppdfdriver.PrintReportPDF(report,'',false,true,1,99999,1,
    StrPas(outputfilename),acompressed,false);
  end
  else
  begin
   rppdfdriver.PrintReportToMetafile(report,'',false,true,1,99999,1,
    StrPas(outputfilename),false);
  end;
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;

function rp_getremoteparams(hreport:integer;hostname:PChar;port:integer;user,password,aliasname,reportname:PChar):integer;stdcall;
var
  pdfreport:TPDFReport;
  report:TRpReport;
begin
 rplibdoinit;
 rplasterror:='';
 Result:=1;
 try
  report:=FindReport(hreport);
  pdfreport:=TPDFReport.Create(nil);
  try
   pdfreport.GetRemoteParams(hostname,port,user,password,aliasname,reportname);
   report.params.Assign(pdfreport.Report.Params);
  finally
   pdfreport.free;
  end;
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;

function rp_executeremote_report(hreport:integer;hostname:PChar;port:integer;user,password,aliasname,reportname:PChar;outputfilename:PChar;metafile,
 compressed:integer):integer;
var
 pdfreport:TPDFReport;
 report:TRpReport;
 memstream:TMemoryStream;
begin
 rplibdoinit;
 rplasterror:='';
 Result:=1;
 try
  report:=FindReport(hreport);
  pdfreport:=TPDFReport.Create(nil);
  try
   memstream:=TMemoryStream.Create;
   try
    report.SaveToStream(memstream);
    memstream.Seek(0,soFromBeginning);
    pdfreport.LoadFromStream(memstream);
   finally
    memstream.free;
   end;
   pdfreport.PDFFilename:=outputfilename;
   pdfreport.Compressed:=(compressed<>0);
   pdfreport.AsMetafile:=(metafile<>0);
   pdfreport.ExecuteRemote(hostname,port,user,password,aliasname,reportname);
  finally
   pdfreport.free;
  end;
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;

function rp_executeremote(hostname:PChar;port:integer;user,password,aliasname,reportname:PChar;outputfilename:PChar;metafile,
 compressed:integer):integer;
var
 pdfreport:TPDFReport;
begin
 rplibdoinit;
 rplasterror:='';
 Result:=1;
 try
  pdfreport:=TPDFReport.Create(nil);
  try
   pdfreport.PDFFilename:=outputfilename;
   pdfreport.Compressed:=(compressed<>0);
   pdfreport.AsMetafile:=(metafile<>0);
   pdfreport.ExecuteRemote(hostname,port,user,password,aliasname,reportname);
  finally
   pdfreport.free;
  end;
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;


function rp_close(hreport:integer):integer;
var
 index:integer;
begin
 rplibdoinit;
 rplasterror:='';
 Result:=1;
 try
  index:=FindReportIndex(hreport);
  TRpReport(lreports.Objects[index]).free;
  lreports.delete(index);
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;

function rp_setparamvalue(hreport:integer;paramname:pchar;paramtype:integer;
 paramvalue:Pointer):integer;
var
 report:TRpReport;
 aparam:TRpParam;
begin
 rplibdoinit;
 rplasterror:='';
 Result:=1;
 try
  report:=FindReport(hreport);
  aparam:=report.params.ParamByName(Strpas(paramname));
  case paramtype of
   1:
    aparam.Value:=Null;
   3:
    aparam.Value:=integer(paramvalue^);
   5:
    aparam.Value:=double(paramvalue^);
   6:
    aparam.Value:=Currency(paramvalue^);
   7:
    aparam.Value:=TDateTime(paramvalue^);
   8:
    aparam.Value:=WideCharToString(PWideChar(paramvalue));
   11:
    aparam.Value:=Boolean(paramvalue^);
   14:
{$IFDEF USEVARIANTS}
    aparam.Value:=Int64(paramvalue^);
{$ENDIF}
{$IFNDEF USEVARIANTS}
    aparam.Value:=Integer(paramvalue^);
{$ENDIF}
   256:
    aparam.Value:=StrPas(PChar(paramvalue));
   else
    Raise Exception.Create(SRpEvalType+' - '+IntToStr(paramtype));
  end;
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;

function rp_getparamname(hreport:integer;index:integer;abuffer:PChar):integer;
var
 report:TRpReport;
 aparam:TRpParam;
begin
 rplibdoinit;
 rplasterror:='';
 Result:=1;
 try
  report:=FindReport(hreport);
  aparam:=report.params.items[index];
  StrPCopy(abuffer,aparam.Name);
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;


function rp_getparamcount(hreport:integer;var paramcount:Integer):integer;
var
 report:TRpReport;
begin
 rplibdoinit;
 rplasterror:='';
 Result:=1;
 try
  report:=FindReport(hreport);
  paramcount:=report.Params.Count;
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;


{$IFDEF MSWINDOWS}
function rp_setparamvaluevar(hreport:integer;paramname:pchar;
 paramvalue:OleVariant):integer;
var
 report:TRpReport;
 aparam:TRpParam;
begin
 rplibdoinit;
 rplasterror:='';
 Result:=1;
 try
  report:=FindReport(hreport);
  aparam:=report.params.ParamByName(Strpas(paramname));
  aparam.Value:=paramvalue;
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;

function rp_setadoconnectionstring(hreport:integer;conname:pchar;
 constring:PChar):integer;
var
 report:TRpReport;
 dbinfo:TRpDatabaseInfoItem;
 index:Integer;
begin
 rplibdoinit;
 rplasterror:='';
 Result:=1;
 try
  report:=FindReport(hreport);
  index:=report.DatabaseInfo.IndexOf(StrPas(conname));
  if index<0 then
   Raise Exception.Create(SRPDabaseAliasNotFound+' - '+StrPas(conname));
  dbinfo:=report.DatabaseInfo.Items[index];
  dbinfo.ADOConnectionString:=StrPas(constring);
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;

{$ENDIF}


function rp_lasterror:PChar;
begin
 rplibdoinit;
 Result:=PChar(rplasterror);
end;

procedure FreeAllReports;
var
 i:integer;
begin
 for i:=0 to lreports.count-1 do
 begin
  TRpReport(lreports.Objects[i]).free;
 end;
 lreports.clear;
end;

{$IFDEF LINUX}
procedure DLLHandler(Reason: Integer);
begin
 // 0 means unloading, 1 means loading.
 if Reason = 0 then
 // Now we want to remove our signal handler.
 UnhookSignal(RTL_SIGDEFAULT);
end;
{$ENDIF}



initialization
 lreports:=nil;
 rplibdoinit;
finalization
 FreeAllReports;
 lreports.free;
 lreports:=nil;
end.
