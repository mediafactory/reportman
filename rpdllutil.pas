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

uses SysUtils,Classes,rpreport,rpmdconsts,rppdfdriver;

var
 lreports:TStringList;
 lasthandle:integer;
 rplasterror:String;

function rp_open(filename:PChar):integer;stdcall;
function rp_execute(hreport:integer;outputfilename:PChar;metafile,compressed:integer):integer;stdcall;
function rp_close(hreport:integer):integer;stdcall;
function rp_lasterror:PChar;stdcall;
function FindReportIndex(hreport:integer):integer;
function FindReport(hreport:integer):TRpReport;

implementation

function FindReportIndex(hreport:integer):integer;
var
 index:integer;
begin
 index:=lreports.IndexOf(IntToStr(hreport));
 if index<0 then
  Raise Exception.Create(SRpSInvReportHandle);
 Result:=index;
end;

function FindReport(hreport:integer):TRpReport;
var
 index:integer;
begin
 index:=FindReportIndex(hreport);
 Result:=TRpReport(lreports.Objects[index]);
end;

function rp_open(filename:PChar):integer;
var
 report:TRpReport;
begin
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


function rp_execute(hreport:integer;outputfilename:PChar;metafile,compressed:integer):integer;
var
 report:TRpReport;
 acompressed:boolean;
begin
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

function rp_close(hreport:integer):integer;
var
 index:integer;
begin
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

function rp_lasterror:PChar;
begin
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

initialization
 lreports:=TStringList.Create;
 lreports.sorted:=True;
 lasthandle:=1;
 rplasterror:='';
finalization
 FreeAllReports;
 lreports.free;
end.
