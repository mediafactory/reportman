{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpclxreport                                    }
{       Report component for clx applications           }
{                                                       }
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

unit rpclxreport;

interface

{$I rpconf.inc}

uses Classes,Sysutils,rpreport,
 rpconsts,rpcompobase,
{$IFDEF HORZPAPERBUG}
 rpmetafile,
{$ENDIF}
 QPrinters,rpqtdriver,rppreview,rpalias,rprfparams;

type
 TCLXReport=class(TCBaseReport)
  private
   FUseSystemPrintDialog:boolean;
  protected
  public
   function Execute:boolean;override;
   procedure PrinterSetup;override;
   function ShowParams:boolean;override;
   procedure SaveToPDF(filename:string);
   constructor Create(AOwner:TComponent);override;
   function PrintRange(frompage:integer;topage:integer;
    copies:integer;collate:boolean):boolean;override;
 published
   property Filename;
   property Preview;
   property ShowProgress;
   property Title;
   property ShowPrintDialog;
   property AliasList;
   property Language;
   property UseSystemPrintDialog:Boolean read
    FUseSystemPrintDialog write FUseSystemPrintDialog default true;
  end;

implementation

uses rpprintdia;


constructor TCLXReport.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FUseSystemPrintDialog:=true;
end;

procedure TCLXReport.PrinterSetup;
begin
 Printer.ExecuteSetup;
end;


function TCLXReport.ShowParams:boolean;
begin
 CheckLoaded;
 Result:=ShowUserParams(report);
end;


function TCLXReport.Execute:boolean;
var
 allpages,collate:boolean;
 frompage,topage,copies:integer;
 dook:boolean;
begin
 inherited Execute;
 if Preview then
 begin
  Result:=ShowPreview(report,Title,FUseSystemPrintDialog);
 end
 else
 begin
  allpages:=true;
  collate:=report.CollateCopies;
  frompage:=1; topage:=999999;
  copies:=report.Copies;
{$IFDEF HORZPAPERBUG}
  if report.PageOrientation=rpOrientationPortrait then
  begin
   printer.Orientation:=poPortrait;
  end
  else
   if report.PageOrientation=rpOrientationLandscape then
   begin
    printer.Orientation:=poLandscape;
   end;
{$ENDIF}


  if ShowPrintDialog then
  begin
   if FUseSystemPrintDialog then
    dook:=rpqtdriver.DoShowPrintDialog(allpages,frompage,topage,copies,collate)
   else
    dook:=rpprintdia.DoShowPrintDialog(allpages,frompage,topage,copies,collate);
   if dook then
   begin
    Result:=PrintReport(report,Title,Showprogress,allpages,frompage,
     topage,copies,collate);
   end
   else
    Result:=false;
  end
  else
  begin
    Result:=PrintReport(report,Title,Showprogress,true,1,
     9999999,report.copies,report.collatecopies);
  end;
 end;
end;

procedure TCLXReport.SaveToPDF(filename:string);
begin
 CheckLoaded;
 rpqtdriver.ExportReportToPDF(report,filename,true,true,1,999999,
  false,filename,true)
end;

function TCLXReport.PrintRange(frompage:integer;topage:integer;
    copies:integer;collate:boolean):boolean;
begin
 Result:=rpqtdriver.PrintReport(Report,Title,ShowProgress,false,
  frompage,topage,copies,collate);
end;


end.
