{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpgdidriver                                     }
{       TRpQTDriver: Printer driver for  QT Libs        }
{       can be used for windows and linux               }
{       it includes printer and bitmap support          }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}

unit rpqtdriver;

interface

uses

{$IFDEF LINUX}
  Libc,
{$ENDIF}
{$IFDEF MSWINDOWS}
  mmsystem,windows,
{$ENDIF}
Classes,sysutils,rpmetafile,rpconsts,QGraphics,QForms,
 rpmunits,QPrinters,QDialogs,rpgraphutils, QControls,
 QStdCtrls,QExtCtrls,types,dateutils,
 rpreport;


const
 METAPRINTPROGRESS_INTERVAL=20;
type
  TFRpQtProgress = class(TForm)
    CancelBtn: TButton;
    Label1: TLabel;
    LRecordCount: TLabel;
    Label2: TLabel;
    LTittle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure AppIdle(Sender:TObject;var done:boolean);
    procedure AppIdleReport(Sender:TObject;var done:boolean);
    procedure RepProgress(Sender:TRpReport;var docancel:boolean);
  public
    { Public declarations }
    cancelled:boolean;
    oldonidle:TIdleEvent;
    tittle:string;
    metafile:TRpMetafileReport;
    report:TRpReport;
  end;


 TRpQtDriver=class(TInterfacedObject,IRpPrintDriver)
  private
   intdpix,intdpiy:integer;
  public
   bitmap:TBitmap;
   dpi:integer;
   toprinter:boolean;
   procedure NewDocument(report:TrpMetafileReport);stdcall;
   procedure EndDocument;stdcall;
   procedure AbortDocument;stdcall;
   procedure NewPage;stdcall;
   procedure EndPage;stdcall;
   procedure DrawObject(page:TRpMetaFilePage;obj:TRpMetaObject);stdcall;
   function AllowCopies:boolean;stdcall;
   constructor Create;
  end;

function PrintMetafile(metafile:TRpMetafileReport;tittle:string;showprogress:boolean):boolean;
function CalcReportWidthProgress(report:TRpReport):boolean;

implementation

{$R *.xfm}

constructor TRpQtDriver.Create;
begin
 // By default 1:1 scale
 dpi:=Screen.PixelsPerInch;
end;

procedure TRpQtDriver.NewDocument(report:TrpMetafileReport);
begin
 if ToPrinter then
 begin
  printer.Title:='Untitled';
  printer.SetPrinter(Printer.Printers.Strings[0]);
  printer.BeginDoc;
  intdpix:=printer.XDPI;
  intdpiy:=printer.YDPI;
 end
 else
 begin
  if assigned(bitmap) then
  begin
   bitmap.free;
   bitmap:=nil;
  end;
  // The scale is the screen resolution by default
  // in dots per inch but the paper size is in tenths of
  // milimeter
  bitmap:=TBitmap.Create;
  bitmap.PixelFormat:=pf32bit;
  bitmap.Width:=Round(report.CustomX*dpi/(CMS_PER_INCHESS*100));
  bitmap.Height:=Round(report.CustomY*dpi/(CMS_PER_INCHESS*100));
 end;
end;

procedure TRpQtDriver.EndDocument;
begin
 if toprinter then
 begin
  printer.EndDoc;
 end
 else
 begin
  // Does nothing because the last bitmap can be usefull
 end;
end;

procedure TRpQtDriver.AbortDocument;
begin
 if toprinter then
 begin
  printer.Abort;
 end
 else
 begin
  if assigned(bitmap) then
   bitmap.free;
  bitmap:=nil;
 end;
end;

procedure TRpQtDriver.NewPage;
begin
 if toprinter then
 begin
  printer.NewPage;
 end
 else
 begin
  bitmap.free;
  bitmap:=nil;
  bitmap:=TBitmap.create;
  bitmap.PixelFormat:=pf32bit;
 end;
end;

procedure TRpQtDriver.EndPage;
begin
 // Does nothing
end;

procedure PrintObject(Canvas:TCanvas;page:TRpMetafilePage;obj:TRpMetaObject;dpix,dpiy:integer);
var
 posx,posy:integer;
 rec:TRect;
 X, Y, W, H, S: Integer;
 Width,Height:integer;
begin
 // Switch to device points
 posx:=round(obj.Left*dpix/TWIPS_PER_INCHESS);
 posy:=round(obj.Top*dpiy/TWIPS_PER_INCHESS);
 case obj.Metatype of
  rpMetaText:
   begin
{$IFDEF MSWINDOWS}
    Canvas.Font.Name:=page.GetWFontName(Obj);
{$ENDIF}
{$IFDEF LINUX}
    Canvas.Font.Name:=page.GetLFontName(Obj);
{$ENDIF}
    Canvas.Font.Color:=Obj.FontColor;
    Canvas.Font.Style:=IntegerToFontStyle(obj.FontStyle);
    if obj.CutText then
    begin
     rec.Top:=posx;
     rec.Left:=posy;
     rec.Right:=posx+round(obj.Width*dpix/TWIPS_PER_INCHESS);
     rec.Bottom:=posy+round(obj.Height*dpiy/TWIPS_PER_INCHESS);
     Canvas.TextRect(rec,posx,posy,page.GetText(Obj));
    end
    else
     Canvas.TextOut(posx,posy,page.GetText(Obj));
   end;
  rpMetaDraw:
   begin
    Width:=round(obj.Width*dpix/TWIPS_PER_INCHESS);
    Height:=round(obj.Height*dpiy/TWIPS_PER_INCHESS);
    Canvas.Brush.Style:=TBrushStyle(obj.BrushStyle);
    Canvas.Pen.Style:=TPenStyle(obj.PenStyle);
    Canvas.Pen.Color:=obj.Pencolor;
    Canvas.Brush.Color:=obj.BrushColor;
    Canvas.Pen.Width:=obj.PenWidth;
    X := Canvas.Pen.Width div 2;
    Y := X;
    W := Width - Canvas.Pen.Width + 1;
    H := Height - Canvas.Pen.Width + 1;
    if Canvas.Pen.Width = 0 then
    begin
     Dec(W);
     Dec(H);
    end;
    if W < H then
     S := W
    else
     S := H;
    if TShapeType(obj.DrawStyle) in [stSquare, stRoundSquare, stCircle] then
    begin
     Inc(X, (W - S) div 2);
     Inc(Y, (H - S) div 2);
     W := S;
     H := S;
    end;
    case TShapeType(obj.DrawStyle) of
     stRectangle, stSquare:
      Canvas.Rectangle(X+PosX, Y+PosY, X+PosX + W, Y +PosY+ H);
     stRoundRect, stRoundSquare:
      Canvas.RoundRect(X, Y, X + W, Y + H, S div 4, S div 4);
     stCircle, stEllipse:
      Canvas.Ellipse(X+PosX, Y+PosY, X+PosX + W, Y+PosY + H);
    end;
   end;
  rpMetaImage:
   begin

   end;
 end;
end;

procedure TRpQtDriver.DrawObject(page:TRpMetaFilePage;obj:TRpMetaObject);
var
 dpix,dpiy:integer;
 Canvas:TCanvas;
begin
 if (toprinter) then
 begin
  if not printer.Printing then
   Raise Exception.Create(SRpQtDriverNotInit);
  dpix:=intdpix;
  dpiy:=intdpiy;
  Canvas:=printer.canvas;
 end
 else
 begin
  if not Assigned(bitmap) then
   Raise Exception.Create(SRpQtDriverNotInit);
  Canvas:=bitmap.canvas;
  dpix:=dpi;
  dpiy:=dpi;
 end;
 PrintObject(Canvas,page,obj,dpix,dpiy);


end;

function TRpQtDriver.AllowCopies:boolean;
begin
 Result:=false;
end;


procedure DoPrintMetafile(metafile:TRpMetafileReport;tittle:string;aform:TFRpQtProgress);
var
 i:integer;
 j:integer;
 apage:TRpMetafilePage;
 dpix,dpiy:integer;
{$IFDEF MSWINDOWS}
    mmfirst,mmlast:DWORD;
{$ENDIF}
{$IFDEF LINUX}
    milifirst,mililast:TDatetime;
{$ENDIF}
    difmilis:int64;
begin
 // Get the time
{$IFDEF MSWINDOWS}
 mmfirst:=TimeGetTime;
{$ENDIF}
{$IFDEF LINUX}
 milifirst:=now;
{$ENDIF}
 printer.Title:=tittle;
 printer.Begindoc;
 try
  dpix:=printer.XDPI;
  dpiy:=printer.YDPI;
  for i:=0 to metafile.PageCount-1 do
  begin
   if i>0 then
    printer.NewPage;
   apage:=metafile.Pages[i];
   for j:=0 to apage.ObjectCount-1 do
   begin
    PrintObject(Printer.Canvas,apage,apage.Objects[j],dpix,dpiy);
    if assigned(aform) then
    begin
{$IFDEF MSWINDOWS}
     mmlast:=TimeGetTime;
     difmilis:=(mmlast-mmfirst);
{$ENDIF}
{$IFDEF LINUX}
     mililast:=now;
     difmilis:=MillisecondsBetween(mililast,milifirst);
{$ENDIF}
     if difmilis>MILIS_PROGRESS then
     begin
      // Get the time
{$IFDEF MSWINDOWS}
      mmfirst:=TimeGetTime;
{$ENDIF}
{$IFDEF LINUX}
      milifirst:=now;
{$ENDIF}
      aform.LRecordCount.Caption:=SRpPage+':'+ IntToStr(i+1)+
        ' - '+SRpItem+':'+ IntToStr(j+1);
      Application.ProcessMessages;
      if aform.cancelled then
       Raise Exception.Create(SRpOperationAborted);
     end;
    end;
   end;
   if assigned(aform) then
   begin
     Application.ProcessMessages;
     if aform.cancelled then
      Raise Exception.Create(SRpOperationAborted);
   end;
  end;
  Printer.EndDoc;
 except
  printer.Abort;
  raise;
 end;
 if assigned(aform) then
  aform.close;
end;

function PrintMetafile(metafile:TRpMetafileReport;tittle:string;showprogress:boolean):boolean;
var
 dia:TFRpQtProgress;
begin
 Result:=true;
 if Not ShowProgress then
 begin
  DoPrintMetafile(metafile,tittle,nil);
  exit;
 end;
 dia:=TFRpQtProgress.Create(Application);
 try
  dia.oldonidle:=Application.OnIdle;
  try
   dia.metafile:=metafile;
   dia.tittle:=tittle;
   Application.OnIdle:=dia.AppIdle;
   dia.ShowModal;
   Result:=Not dia.cancelled;
  finally
   Application.OnIdle:=dia.oldonidle;
  end;
 finally

 end;
end;


procedure TFRpQtProgress.FormCreate(Sender: TObject);
begin
 LRecordCount.Font.Style:=[fsBold];
 LTittle.Font.Style:=[fsBold];
end;

procedure TFRpQtProgress.AppIdle(Sender:TObject;var done:boolean);
begin
 cancelled:=false;
 Application.OnIdle:=nil;
 done:=false;
 LTittle.Caption:=tittle;
 Label2.Visible:=true;
 DoPrintMetafile(metafile,tittle,self);
end;


procedure TFRpQtProgress.CancelBtnClick(Sender: TObject);
begin
 cancelled:=true;
end;


procedure TFRpQtProgress.RepProgress(Sender:TRpReport;var docancel:boolean);
begin
 LRecordCount.Caption:=IntToStr(Sender.CurrentSubReportIndex)+':'+FormatFloat('#########,####',Sender.RecordCount);
 Application.ProcessMessages;
 if cancelled then
  docancel:=true;
end;

procedure TFRpQtProgress.AppIdleReport(Sender:TObject;var done:boolean);
var
 oldprogres:TRpProgressEvent;
begin
 Application.Onidle:=nil;
 done:=false;
 oldprogres:=RepProgress;
 try
  report.OnProgress:=RepProgress;
  report.BeginPrint;
  try
   while Not report.PrintNextPage do;
  finally
   report.EndPrint;
  end;
 finally
  report.OnProgress:=oldprogres;
 end;
 Close;
end;

function CalcReportWidthProgress(report:TRpReport):boolean;
var
 dia:TFRpQTProgress;
begin
 dia:=TFRpQTProgress.Create(Application);
 try
  dia.oldonidle:=Application.OnIdle;
  try
   dia.report:=report;
   Application.OnIdle:=dia.AppIdleReport;
   dia.ShowModal;
   Result:=Not dia.cancelled;
  finally
   Application.onidle:=dia.oldonidle;
  end;
 finally
  dia.free;
 end;
end;


end.

