{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpclxreportc                                    }
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

uses Classes,Sysutils,rpreport,rpconsts,
 QPrinters,rpqtdriver,rppreview,rpalias;

type
 TCLXReport=class(TComponent)
  private
   FFilename:TFilename;
   FReport:TRpReport;
   FPreview:Boolean;
   FShowProgress:Boolean;
   FTitle:WideString;
   FAliasList:TRpAlias;
   FShowPrintDialog:boolean;
  protected
   procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
   function Execute:boolean;
   procedure PrinterSetup;
   function GetReport:TRpReport;
   constructor Create(AOwner:TComponent);override;
   procedure CheckLoaded;
   procedure SetFileName(Value:TFilename);
   property Report:TRpReport read GetReport;
  published
   property Filename:TFilename read FFilename write SetFilename;
   property Preview:Boolean read FPreview write FPreview default true;
   property ShowProgress:boolean read FShowProgress write FShowProgress
    default true;
   property Title:widestring read FTitle write FTitle;
   property ShowPrintDialog:boolean read FShowPrintDialog
    write FShowPrintDialog default true;
   property AliasList:TRpAlias read FAliasList write FAliasList;
  end;

implementation

uses rpprintdia;

constructor TCLXReport.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FShowProgress:=true;
 FFilename:='';
 FPreview:=true;
 FTitle:=SRpUntitled;
 FShowPrintDialog:=True;
end;

procedure TCLXReport.Notification(AComponent: TComponent; Operation: TOperation);
begin
 inherited Notification(AComponent,Operation);

 if Assigned(AComponent) then
  if Operation=OpRemove then
   if AComponent=FAliasList then
    FAliasList:=nil;
end;

procedure TCLXReport.SetFileName(Value:TFilename);
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



function TCLXReport.GetReport:TRpReport;
begin
 CheckLoaded;
 Result:=FReport;
end;


procedure TCLXReport.PrinterSetup;
begin
 Printer.ExecuteSetup;
end;

procedure TCLXReport.CheckLoaded;
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


function TCLXReport.Execute:boolean;
var
 allpages,collate:boolean;
 frompage,topage,copies:integer;
begin
 CheckLoaded;
 report.AliasList:=AliasList;
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


end.
