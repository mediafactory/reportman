{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpcompobase                                     }
{       Report component base                           }
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

unit rpcompobase;

interface

{$I rpconf.inc}

uses Classes,Sysutils,rpreport,rpmdconsts,
 rpalias,rpsubreport,rpsection,rpprintitem;

type
 TCBaseReport=class(TComponent)
  private
   FFilename:TFilename;
   FReport:TRpReport;
   FPreview:Boolean;
   FShowProgress:Boolean;
   FTitle:WideString;
   FAliasList:TRpAlias;
   FShowPrintDialog:boolean;
   FLanguage:integer;
   FOnBeforePrint:TNotifyEvent;
   procedure InternalSetBeforePrint;
   function GetReport:TRpReport;
   procedure SetFileName(Value:TFilename);
   procedure SetOnBeforePrint(NewValue:TNotifyEvent);
  protected
   procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
   function Execute:boolean;virtual;
   procedure PrinterSetup;virtual;abstract;
   constructor Create(AOwner:TComponent);override;
   procedure CheckLoaded;
   property Report:TRpReport read GetReport;
   function ShowParams:boolean;virtual;abstract;
   function PrintRange(frompage:integer;topage:integer;
    copies:integer;collate:boolean):boolean;virtual;abstract;
  // Defined as public but will be published in descendants
  public
   procedure LoadFromFile(AFilename:string);
   procedure LoadFromStream(stream:TStream);
   property Filename:TFilename read FFilename write SetFilename;
   property Preview:Boolean read FPreview write FPreview default true;
   property ShowProgress:boolean read FShowProgress write FShowProgress
    default true;
   property Title:widestring read FTitle write FTitle;
   property ShowPrintDialog:boolean read FShowPrintDialog
    write FShowPrintDialog default true;
   property AliasList:TRpAlias read FAliasList write FAliasList;
   property Language:integer read FLanguage write FLanguage default -1;
  published
   property OnBeforePrint:TNotifyEvent read FOnBeforePrint
    write SetOnBeforePrint;
  end;


implementation


constructor TCBaseReport.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FShowProgress:=true;
 FFilename:='';
 FPreview:=true;
 FTitle:=SRpUntitled;
 FShowPrintDialog:=True;
 FLanguage:=-1;
end;

procedure TCBaseReport.Notification(AComponent: TComponent; Operation: TOperation);
begin
 inherited Notification(AComponent,Operation);

 if Assigned(AComponent) then
  if Operation=OpRemove then
   if AComponent=FAliasList then
    FAliasList:=nil;
end;

procedure TCBaseReport.SetFileName(Value:TFilename);
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



function TCBaseReport.GetReport:TRpReport;
begin
 CheckLoaded;
 Result:=FReport;
end;



procedure TCBaseReport.CheckLoaded;
begin
 // Loads the report
 if Assigned(FReport) then
 begin
  FReport.AliasList:=AliasList;
  exit;
 end;
 if Length(FFilename)<1 then
  Raise Exception.Create(SRpNoFilename);
 LoadFromFile(FFilename);
 InternalSetBeforePrint;
 if Assigned(FReport) then
 begin
  FReport.AliasList:=AliasList;
 end;
end;

procedure TCBaseReport.SetOnBeforePrint(NewValue:TNotifyEvent);
begin
 FOnBeforePrint:=NewValue;
 InternalSetBeforePrint;
end;


function TCBaseReport.Execute:boolean;
begin
 CheckLoaded;
 if FLanguage>=0 then
  report.Language:=FLanguage;
 Result:=false;
end;

procedure TCBaseReport.LoadFromFile(AFilename:string);
begin
 if Assigned(FReport) then
 begin
  FReport.Free;
  FReport:=nil;
 end;
 FReport:=TRpReport.Create(Self);
 try
  FReport.LoadFromFile(AFilename);
 except
  FReport.Free;
  FReport:=nil;
  raise;
 end;
end;


procedure TCBaseReport.LoadFromStream(stream:TStream);
begin
 if Assigned(FReport) then
 begin
  FReport.Free;
  FReport:=nil;
 end;
 FReport:=TRpReport.Create(Self);
 try
  FReport.LoadFromStream(Stream);
  InternalSetBeforePrint;
 except
  FReport.Free;
  FReport:=nil;
  raise;
 end;
end;

procedure TCBaseReport.InternalSetBeforePrint;
var
 i,j,k:integer;
 subrep:TRpSubReport;
 sec:TRpSection;
begin
 If not Assigned(FReport) then
  exit;
 for i:=0 to FReport.SubReports.Count-1 do
 begin
  subrep:=FReport.SubReports.Items[i].SubReport;
  for j:=0 to subrep.Sections.Count-1 do
  begin
   sec:=subrep.Sections.Items[j].Section;
   sec.OnBeforePrint:=FOnBeforePrint;
   for k:=0 to sec.ReportComponents.Count-1 do
   begin
    TRpCommonComponent(sec.ReportComponents.Items[k].Component).OnBeforePrint:=FOnBeforePrint;
   end;
  end;
 end;
end;


end.
