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

uses Classes,Sysutils,rpreport,rpconsts,rpcompobase,
 QPrinters,rpqtdriver,rppreview,rpalias,rprfparams;

type
 TCLXReport=class(TCBaseReport)
  private
  protected
  public
   function Execute:boolean;override;
   procedure PrinterSetup;override;
   function ShowParams:boolean;override;
   procedure SaveToPDF(filename:string);
  published
   property Filename;
   property Preview;
   property ShowProgress;
   property Title;
   property ShowPrintDialog;
   property AliasList;
   property Language;
  end;

implementation

{$IFDEF MSWINDOWS}
uses rpprintdia;
{$ENDIF}



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
begin
 inherited Execute;
 if Preview then
 begin
  Result:=ShowPreview(report,Title);
 end
 else
 begin
  allpages:=true;
  collate:=report.CollateCopies;
  frompage:=1; topage:=999999;
  copies:=report.Copies;
  if ShowPrintDialog then
  begin
{$IFDEF MSWINDOWS}
   if rpprintdia.DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
{$ENDIF}
{$IFDEF LINUX}
   if rpqtdriver.DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
{$ENDIF}
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

end.
