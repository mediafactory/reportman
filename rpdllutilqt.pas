{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpdllutilqt                                     }
{       Exported functions for the Standarc C Libraary  }
{       Functions dependent on X Server running         }
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

unit rpdllutilqt;

{$I rpconf.inc}

interface

uses SysUtils,Classes,rpreport,rpmdconsts,
{$IFDEF MSWINDOWS}
 rpgdidriver,
 rpvpreview,
{$ENDIF}
{$IFDEF LINUX}
 rpqtdriver,
 rppreview,
{$ENDIF}
 rpdllutil;


function rp_print(hreport:integer;Title:PChar;
 showprogress,ShowPrintDialog:integer):integer;stdcall;
function rp_preview(hreport:integer;Title:PChar):integer;stdcall;

implementation

function rp_print(hreport:integer;Title:PChar;
 showprogress,ShowPrintDialog:integer):integer;
var
 report:TRpReport;
 allpages,collate:boolean;
 frompage,topage,copies:integer;
 ashowprogress:boolean;
begin
 rplasterror:='';
 Result:=1;
 try
  ashowprogress:=true;
  if showprogress=0 then
   ashowprogress:=false;
  report:=FindReport(hreport);
  allpages:=true;
  collate:=report.CollateCopies;
  frompage:=1; topage:=999999;
  copies:=report.Copies;
  if ShowPrintDialog<>0 then
  begin
   if DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
   begin
    if Not PrintReport(report,Title,aShowprogress,allpages,frompage,
     topage,copies,collate) then
     Result:=0;
   end
   else
    Result:=0;
  end
  else
  begin
   if not PrintReport(report,Title,aShowprogress,true,1,
     9999999,report.copies,report.collatecopies) then
    Result:=0;
  end;
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;

function rp_preview(hreport:integer;Title:PChar):integer;
var
 report:TRpReport;
begin
 rplasterror:='';
 Result:=1;
 try
  report:=FindReport(hreport);
  ShowPreview(report,Title);
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;


end.
