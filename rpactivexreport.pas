{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpactivexreport                                 }
{       Base for ActiveX Report                         }
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

unit rpactivexreport;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls,
  rpvclreport,Graphics,rpreport;


const
 AX_CONSWIDTH=50;
 AX_CONSHEIGHT=20;

type
  TRpActiveXReport = class(TCustomControl)
  private
    { Private declarations }
    FVCLReport:TVCLReport;
    FFilename:String;
    FPreview:boolean;
    FTitle:string;
    FLanguage:integer;
    FSHowProgress:boolean;
    FShowPrintDIalog:boolean;
    procedure SetFilename(Value:string);
    procedure SetPreview(Value:boolean);
    procedure SetShowProgress(Value:boolean);
    procedure SetShowPrintDialog(Value:boolean);
    procedure SetTitle(Value:string);
    procedure SetLanguage(Value:integer);
  protected
    { Protected declarations }
    procedure Paint;override;
  public
   function Execute:Boolean;
   procedure PrinterSetup;
   function ShowParams:boolean;
   procedure SaveToPDF(filename:string);
   function PrintRange(frompage:integer;topage:integer;
    copies:integer;collate:boolean):boolean;
   procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);override;
   constructor Create(AOwner:TComponent);override;
   function GetReport:TRpReport;
   { Public declarations }
  published
    { Published declarations }
    property Filename:string read FFilename write SetFilename;
    property Preview:boolean read FPreview write SetPreview default true;
    property ShowProgress:boolean read FShowProgress write SetShowProgress;
    property ShowPrintDialog:boolean read FShowPrintDialog write SetShowPrintDialog;
    property Title:string read FTitle write SetTitle;
    property Language:integer read FLanguage write SetLanguage;
    property Report:TRpReport read GetReport;
  end;


implementation

procedure TRpActiveXReport.Paint;
begin
 Canvas.Brush.Color:=clWhite;
 Canvas.Pen.Color:=clBlack;
 Canvas.Rectangle(ClientRect);
 Canvas.TextOut(2,2,'AXReport');
end;

procedure TRpActiveXReport.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
 AWidth:=AX_CONSWIDTH;
 AHeight:=AX_CONSHEIGHT;
 inherited SetBounds(ALeft,ATop,AWidth,AHeight);
end;

constructor TRpActiveXReport.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 Width:=AX_CONSWIDTH;
 Height:=AX_CONSHEIGHT;
 FVCLReport:=TVCLReport.Create(Self);
end;

procedure TRpActiveXReport.SetFilename(Value:string);
begin
 FVCLReport.Filename:=Value;
 FFilename:=Value;
end;

procedure TRpActiveXReport.SetPreview(Value:boolean);
begin
 FVCLReport.Preview:=Value;
 FPreview:=Value;
end;

function TRpActiveXReport.Execute:boolean;
begin
 Result:=FVCLReport.Execute;
end;

procedure TRpActiveXReport.SetShowProgress(Value:Boolean);
begin
 FVCLReport.ShowProgress:=Value;
 FShowProgress:=Value;
end;

procedure TRpActiveXReport.SetShowPrintDialog(Value:boolean);
begin
 FVCLReport.ShowPrintDialog:=Value;
 FShowProgress:=Value;
end;

procedure TRpActiveXReport.SetTitle(Value:string);
begin
 FVCLReport.Title:=Value;
 FTitle:=Value;
end;

procedure TRpActiveXReport.SetLanguage(Value:integer);
begin
 FVCLReport.Language:=Value;
 FLanguage:=Value;
end;

procedure TRpActiveXReport.PrinterSetup;
begin
 FVCLReport.PrinterSetup;
end;

function TRpActiveXReport.ShowParams:boolean;
begin
 Result:=FVCLReport.ShowParams;
end;

procedure TRpActiveXReport.SaveToPDF(filename:string);
begin
 FVCLReport.SaveToPDF(filename);
end;

function TRpActiveXReport.PrintRange(frompage:integer;topage:integer;
    copies:integer;collate:boolean):boolean;
begin
 Result:=FVCLReport.PrintRange(frompage,topage,copies,collate);
end;

function TRpActiveXReport.GetReport:TRpReport;
begin
 Result:=FVCLReport.Report;
end;

end.

