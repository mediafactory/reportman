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

uses Classes,Sysutils,rpreport,rpconsts,rpalias;

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
   function GetReport:TRpReport;
  protected
   procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
   function Execute:boolean;virtual;
   procedure PrinterSetup;virtual;abstract;
   constructor Create(AOwner:TComponent);override;
   procedure CheckLoaded;
   procedure SetFileName(Value:TFilename);
   property Report:TRpReport read GetReport;
   function ShowParams:boolean;virtual;abstract;
  // Defined as public but will be published in descendants
  public
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



function TCBaseReport.Execute:boolean;
begin
 CheckLoaded;
 report.AliasList:=AliasList;
 if FLanguage>=0 then
  report.Language:=FLanguage;
 Result:=false;
end;


end.
