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


uses ShareExcept,SysUtils,Classes,rpreport,rpmdconsts,
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
 showprogress,ShowPrintDialog:integer):integer;
{$IFDEF MSWINDOWS}
stdcall;
{$ENDIF}
{$IFDEF LINUX}
cdecl;
{$ENDIF}

function rp_preview(hreport:integer;Title:PChar):integer;
{$IFDEF MSWINDOWS}
stdcall;
{$ENDIF}
{$IFDEF LINUX}
cdecl;
{$ENDIF}

{$IFDEF LINUX}
exports
 rp_open,
 rp_execute,
 rp_close,
 rp_lasterror,
 rp_print,
 rp_preview;
{$ENDIF}

implementation

function rp_print(hreport:integer;Title:PChar;
 showprogress,ShowPrintDialog:integer):integer;
var
 report:TRpReport;
 allpages,collate:boolean;
 frompage,topage,copies:integer;
 ashowprogress:boolean;
begin
 rplibdoinit;
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
 rplibdoinit;
 Writeln('Hello1');
 rplasterror:='';
 Result:=1;
 try
  Writeln('Hello2');
  report:=FindReport(hreport);
  Writeln('Hello3');
{$IFDEF MSWINDOWS}
  ShowPreview(report,Title);
{$ENDIF}
{$IFDEF LINUX}
  ShowPreview(report,Title,true);
{$ENDIF}
  Writeln('Hello4');
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;


end.
