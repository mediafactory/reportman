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
 QPrinters,rpqtdriver,rppreview;

type
 TCLXReport=class(TComponent)
  private
   FFilename:TFilename;
   FReport:TRpReport;
   FPreview:Boolean;
   FShowProgress:Boolean;
   FTitle:WideString;
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
  end;

implementation

constructor TCLXReport.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FShowProgress:=true;
 FFilename:='';
 FPreview:=true;
 FTitle:=SRpUntitled;
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
begin
 CheckLoaded;
 if FPreview then
 begin
  Result:=ShowPreview(report,Title);
 end
 else
 begin
  Result:=PrintReport(report,Title,FShowprogress);
 end;
end;


end.
