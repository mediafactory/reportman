{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpdllutilqt                                     }
{       Exported functions for the Standarc C Libraary  }
{       Functions dependent on X Server running         }
{                                                       }
{       Copyright (c) 1994-2003 Toni Martir             }
{       to7ni@pala.com                                   }
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


uses
{$IFDEF LINUX}
 ShareExcept,
{$ENDIF}
 SysUtils,Classes,rpreport,rpmdconsts,
{$IFDEF MSWINDOWS}
 rpgdidriver,
 rpvpreview,rpvclreport,
{$ENDIF}
{$IFDEF LINUX}
 rpqtdriver,
 rppreview,rpclxreport,
{$ENDIF}
 rpdllutil;



function rp_print(hreport:integer;Title:PChar;
 showprogress,ShowPrintDialog:integer):integer;stdcall;
function rp_preview(hreport:integer;Title:PChar):integer;stdcall;
function rp_previewremote(hostname:PChar;port:integer;user,password,aliasname,reportname,title:PChar):integer;stdcall;
function rp_printremote(hostname:PChar;port:integer;user,password,aliasname,reportname,title:PChar;showprogress,showprintdialog:integer):integer;stdcall;


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
 modified:boolean;
begin
 rplibdoinit;
 rplasterror:='';
 Result:=1;
 try
  report:=FindReport(hreport);
{$IFDEF MSWINDOWS}
  ShowPreview(report,Title,modified);
{$ENDIF}
{$IFDEF LINUX}
  ShowPreview(report,Title,true);
{$ENDIF}
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;


function rp_previewremote(hostname:PChar;port:integer;user,password,aliasname,reportname,title:PChar):integer;stdcall;
var
{$IFDEF MSWINDOWS}
 rep:TVCLReport;
{$ENDIF}
{$IFDEF LINUX}
 rep:TCLXReport;
{$ENDIF}
begin
 rplibdoinit;
 rplasterror:='';
 Result:=1;
 try
{$IFDEF MSWINDOWS}
  rep:=TVCLReport.Create(nil);
{$ENDIF}
{$IFDEF LINUX}
  rep:=TCLXReport.Create(nil);
{$ENDIF}
  try
   rep.Preview:=true;
   rep.Title:=title;
   rep.ExecuteRemote(hostname,port,user,password,aliasname,reportname);
  finally
   rep.free;
  end;
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;

function rp_printremote(hostname:PChar;port:integer;user,password,aliasname,reportname,title:PChar;showprogress,showprintdialog:integer):integer;stdcall;
var
{$IFDEF MSWINDOWS}
 rep:TVCLReport;
{$ENDIF}
{$IFDEF LINUX}
 rep:TCLXReport;
{$ENDIF}
begin
 rplibdoinit;
 rplasterror:='';
 Result:=1;
 try
{$IFDEF MSWINDOWS}
  rep:=TVCLReport.Create(nil);
{$ENDIF}
{$IFDEF LINUX}
  rep:=TCLXReport.Create(nil);
{$ENDIF}
  try
   rep.Preview:=false;
   rep.Title:=title;
   rep.ShowPrintDialog:=(showprintdialog<>0);
   rep.ShowProgress:=(showprogress<>0);
   rep.ExecuteRemote(hostname,port,user,password,aliasname,reportname);
  finally
   rep.free;
  end;
 except
  on E:Exception do
  begin
   rplasterror:=E.Message;
   Result:=0;
  end;
 end;
end;

end.
