{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpvclreport                                     }
{       Report component for vcl applications           }
{       Delphi 5 for example                            }
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

unit rpvclreport;

interface

uses Classes,Sysutils,rpreport,rpconsts,rpcompobase,
 rpgdidriver,rpalias,dialogs,rprfvparams,rpvpreview;

type
 TVCLReport=class(TCBaseReport)
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

procedure TVCLReport.PrinterSetup;
var
 psetup:TPrinterSetUpDialog;
begin
 psetup:=TPRinterSetupDialog.Create(nil);
 try
  psetup.execute;
 finally
  psetup.free;
 end;
end;

function TVCLReport.ShowParams:boolean;
begin
 CheckLoaded;
 Result:=ShowUserParams(report);
end;

function TVCLReport.Execute:boolean;
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
   if DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
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

procedure TVCLReport.SaveToPDF(filename:string);
begin
 CheckLoaded;
 rpgdidriver.ExportReportToPDF(report,filename,true,true,1,999999,
  false,filename,true)
end;

end.
