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

uses Classes,Sysutils,rpreport,rpconsts,
 rpgdidriver,rpalias,dialogs,rprfvparams,rpvpreview;

type
 TVCLReport=class(TComponent)
  private
   FFilename:TFilename;
   FReport:TRpReport;
   FPreview:Boolean;
   FShowProgress:Boolean;
   FTitle:WideString;
   FAliasList:TRpAlias;
   FShowPrintDialog:boolean;
   FLanguage:integer;
   function GetReport:TRpReport;
  protected
   procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
   function Execute:boolean;
   procedure PrinterSetup;
   constructor Create(AOwner:TComponent);override;
   procedure CheckLoaded;
   procedure SetFileName(Value:TFilename);
   property Report:TRpReport read GetReport;
   function ShowParams:boolean;
   procedure SaveToPDF(filename:string);
  published
   property Filename:TFilename read FFilename write SetFilename;
   property Preview:Boolean read FPreview write FPreview default true;
   property ShowProgress:boolean read FShowProgress write FShowProgress
    default true;
   property Title:widestring read FTitle write FTitle;
   property ShowPrintDialog:boolean read FShowPrintDialog
    write FShowPrintDialog default true;
   property AliasList:TRpAlias read FAliasList write FAliasList;
   property Language:integer read FLanguage write FLanguage default -1;
  end;

implementation


constructor TVCLReport.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FShowProgress:=true;
 FFilename:='';
 FPreview:=true;
 FTitle:=SRpUntitled;
 FShowPrintDialog:=True;
 FLanguage:=-1;
end;

procedure TVCLReport.Notification(AComponent: TComponent; Operation: TOperation);
begin
 inherited Notification(AComponent,Operation);

 if Assigned(AComponent) then
  if Operation=OpRemove then
   if AComponent=FAliasList then
    FAliasList:=nil;
end;

procedure TVCLReport.SetFileName(Value:TFilename);
begin
 if (csloading in ComponentState) then
 begin
  FFilename:=Value;
  exit;
 end;
 if FFilename<>Value then
 begin
  if Assigned(FReport) then
  begin
   FReport.free;
   FReport:=nil;
  end;
  FFilename:=Value;
 end;
end;



function TVCLReport.GetReport:TRpReport;
begin
 CheckLoaded;
 Result:=FReport;
end;


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

procedure TVCLReport.CheckLoaded;
begin
 // Loads the report
 if Length(FFilename)<1 then
  Raise Exception.Create(SRpNoFilename);
 if Assigned(FReport) then
  exit;
 FReport:=TRpReport.Create(Self);
 try
  FReport.LoadFromFile(FFilename);
 except
  FReport.Free;
  FReport:=nil;
  raise;
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
 CheckLoaded;
 report.AliasList:=AliasList;
 if FLanguage>=0 then
  report.Language:=FLanguage;
 if FPreview then
 begin
  Result:=ShowPreview(report,Title);
 end
 else
 begin
  allpages:=true;
  collate:=report.CollateCopies;
  frompage:=1; topage:=999999;
  copies:=report.Copies;
  if FShowPrintDialog then
  begin
   if DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
   begin
    Result:=PrintReport(report,Title,FShowprogress,allpages,frompage,
     topage,copies,collate);
   end
   else
    Result:=false;
  end
  else
  begin
    Result:=PrintReport(report,Title,FShowprogress,true,1,
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
