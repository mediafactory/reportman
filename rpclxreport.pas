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
   constructor Create(AOwner:TComponent);override;
   property Report:TRpReport read FReport;
  published
   property Filename:TFilename read FFilename write FFilename;
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
 FReport:=TRpReport.Create(Self);
 FTitle:=SRpUntitled;
end;

procedure TCLXReport.PrinterSetup;
begin
 Printer.ExecuteSetup;
end;

function TCLXReport.Execute:boolean;
begin
 // Loads the report
 if Length(FFilename)<1 then
  Raise Exception.Create(SRpNoFilename);
 FReport.LoadFromFile(FFilename);
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
