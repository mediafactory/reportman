{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpqtriver                                       }
{       TRpQTDriver: Printer driver for  QT Libs        }
{       can be used for windows and linux               }
{       it includes printer and bitmap support          }
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

unit rpqtdriver;

interface

{$I rpconf.inc}

uses
{$IFDEF LINUX}
  Libc,
{$ENDIF}
{$IFDEF MSWINDOWS}
  mmsystem,windows,winspool,
{$ENDIF}
 Classes,sysutils,rpmetafile,rpmdconsts,QGraphics,QForms,
 rpmunits,QPrinters,QDialogs,rpgraphutils, QControls,
 QStdCtrls,QExtCtrls,types,DateUtils,rptypes,Qt,
 rpreport,rppdfdriver;

const
 METAPRINTPROGRESS_INTERVAL=20;
 SCROLLBAR_HX=20;
 SCROLLBAR_VX=20;
type
  TRpQtDriver=class;
  TFRpQtProgress = class(TForm)
    BCancel: TButton;
    LProcessing: TLabel;
    LRecordCount: TLabel;
    LTitle: TLabel;
    LTittle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    allpages,collate:boolean;
    frompage,topage,copies:integer;
    printerindex:TRpPrinterSelect;
    procedure AppIdle(Sender:TObject;var done:boolean);
    procedure AppIdleReport(Sender:TObject;var done:boolean);
    procedure AppIdlePrintPDF(Sender:TObject;var done:boolean);
    procedure AppIdlePrintRange(Sender:TObject;var done:boolean);
    procedure RepProgress(Sender:TRpReport;var docancel:boolean);
  public
    { Public declarations }
    pdfcompressed:boolean;
    cancelled:boolean;
    oldonidle:TIdleEvent;
    tittle:string;
    filename:string;
    metafile:TRpMetafileReport;
    report:TRpReport;
    qtdriver:TRpQtDriver;
    aqtdriver:IRpPrintDriver;
    pdfdriver:TRpPDFDriver;
    apdfdriver:IRpPrintDriver;
  end;


 TRpQtDriver=class(TInterfacedObject,IRpPrintDriver)
  private
   intdpix,intdpiy:integer;
   FOrientation:TRpOrientation;
   FIntPageSize:TPageSizeQt;
   OldOrientation:TPrinterOrientation;
   function InternalSetPagesize(PagesizeQt:integer):TPoint;
  public
   bitmap:TBitmap;
   dpi:integer;
   toprinter:boolean;
   scale:double;
   offset:TPoint;
   bitmapwidth,bitmapheight:integer;
   PreviewStyle:TRpPreviewStyle;
   clientwidth,clientheight:integer;
   procedure NewDocument(report:TrpMetafileReport);stdcall;
   procedure EndDocument;stdcall;
   procedure AbortDocument;stdcall;
   procedure NewPage;stdcall;
   procedure EndPage;stdcall;
   procedure DrawObject(page:TRpMetaFilePage;obj:TRpMetaObject);stdcall;
   procedure DrawPage(apage:TRpMetaFilePage);stdcall;
   procedure TextExtent(atext:TRpTextObject;var extent:TPoint);stdcall;
   procedure GraphicExtent(Stream:TMemoryStream;var extent:TPoint;dpi:integer);stdcall;
   function AllowCopies:boolean;stdcall;
   procedure SelectPrinter(printerindex:TRpPrinterSelect);stdcall;
   function GetPageSize:TPoint;stdcall;
   function SetPagesize(PagesizeQt:TPageSizeQt):TPoint;stdcall;
   procedure SetOrientation(Orientation:TRpOrientation);stdcall;
   constructor Create;
   destructor Destroy;override;
  end;

function PrintMetafile(metafile:TRpMetafileReport;tittle:string;
 showprogress,allpages:boolean;frompage,topage,copies:integer;
  collate:boolean;printerindex:TRpPrinterSelect):boolean;
function CalcReportWidthProgress(report:TRpReport):boolean;
function PrintReport(report:TRpReport;Caption:string;progress:boolean;
  allpages:boolean;frompage,topage,copies:integer;collate:boolean):Boolean;
function ExportReportToPDF(report:TRpReport;Caption:string;progress:boolean;
  allpages:boolean;frompage,topage:integer;
  showprintdialog:boolean;filename:string;compressed:boolean):Boolean;


// Because copies and collation not work in Windows we
// use the ShowPrintdialog in rpprintdia
function DoShowPrintDialog(var allpages:boolean;
 var frompage,topage,copies:integer;var collate:boolean;disablecopies:boolean=false):boolean;
function PrinterSelection(printerindex:TRpPrinterSelect):TPoint;
procedure PageSizeSelection(rpPageSize:TPageSizeQt);
procedure OrientationSelection(neworientation:TRpOrientation);

 var
{$IFDEF MSWINDOWS}
  kylixprintbug:boolean=false;
{$ENDIF}
{$IFDEF LINUX}
  kylixprintbug:boolean=false;
{$ENDIF}

implementation

uses rpprintdia;

{$R *.xfm}


function DoShowPrintDialog(var allpages:boolean;
 var frompage,topage,copies:integer;var collate:boolean;disablecopies:boolean=false):boolean;
begin
 Result:=false;
 QPrinter_setMinMax(QPrinterH(Printer.Handle),1,999999);
 QPrinter_setNumCopies(QprinterH(printer.Handle),copies);
 QPrinter_setFromTo(QPrinterH(Printer.Handle),frompage,topage);
 if Not Collate then
  QPrinter_setPageOrder(QPrinterH(Printer.Handle),qt.QPrinterPageOrder_FirstPageFirst)
 else
  QPrinter_setPageOrder(QPrinterH(Printer.Handle),qt.QPrinterPageOrder_LastPageFirst);
 if Integer(QPrinter_setup(QPrinterH(Printer.handle),nil))<>0 then
 begin
  frompage:=QPrinter_fromPage(QPrinterH(Printer.handle));
  topage:=QPrinter_toPage(QPrinterH(Printer.handle));
  // Collate does not work, copies does not work
  copies:=QPrinter_numCopies(QPrinterH(Printer.Handle));
  collate:=qt.QPrinterPageOrder_LastPageFirst=QPrinter_PageOrder(QPrinterH(Printer.Handle));
  allpages:=false;
  Result:=true;
 end;
end;

constructor TRpQtDriver.Create;
begin
 // By default 1:1 scale
 FIntPageSize.Custom:=false;
 dpi:=Screen.PixelsPerInch;
 scale:=1;
end;

destructor TRpQtDriver.Destroy;
begin
 if assigned(bitmap) then
 begin
  bitmap.free;
  bitmap:=nil;
 end;
 inherited Destroy;
end;

procedure TRpQtDriver.NewDocument(report:TrpMetafileReport);
var
 awidth,aheight:integer;
 rec:TRect;
 asize:TPoint;
 scale2:double;
begin
 if ToPrinter then
 begin
  scale:=1.0;
  printer.Title:='Untitled';
  SetOrientation(report.Orientation);
  // Sets pagesize, only supports default and qt index
  if report.PageSize<0 then
  begin
   asize:=GetPageSize;
  end
  else
  begin
   asize:=InternalSetPageSize(report.PageSize);
  end;
  if Length(printer.Title)<1 then
   printer.Title:=SRpUntitled;
  QPrinter_setFullPage(QPrinterH(Printer.Handle),true);
//  begin
//   QPrinter_setOrientation(QPrinterH(Printer.Handle),QPrinterOrientation_Portrait);
//   QPrinter_setOrientation(QPrinterH(Printer.Handle),QPrinterOrientation_Landscape);
//  end;
  if Not Printer.Printing then
   printer.BeginDoc;
  intdpix:=printer.XDPI;
  intdpiy:=printer.YDPI;
 end
 else
 begin
  // Offset is 0 in preview
  offset.X:=0;
  offset.Y:=0;
  if assigned(bitmap) then
  begin
   bitmap.free;
   bitmap:=nil;
  end;
  bitmap:=TBitmap.Create;
  bitmap.PixelFormat:=pf32bit;
  // Set full page
  QPrinter_setFullPage(QPrinterH(Printer.Handle),true);

  // Sets Orientation
  SetOrientation(report.Orientation);
  // Sets pagesize
  if report.PageSize<0 then
  begin
   asize:=GetPageSize;
  end
  else
  begin
   asize:=InternalSetPageSize(report.PageSize);
  end;
  bitmapwidth:=Round((asize.x/TWIPS_PER_INCHESS)*dpi);
  bitmapheight:=Round((asize.y/TWIPS_PER_INCHESS)*dpi);

  awidth:=bitmapwidth;
  aheight:=bitmapheight;

  if clientwidth>0 then
  begin
   // Calculates the scale
   case PreviewStyle of
    spWide:
     begin
      // Adjust clientwidth to bitmap width
      scale:=(clientwidth-SCROLLBAR_VX)/bitmapwidth;
     end;
    spNormal:
     begin
      scale:=1.0;
     end;
    spEntirePage:
     begin
      // Adjust client to bitmap with an height
      scale:=(clientwidth-1)/bitmapwidth;
      scale2:=(clientheight-1)/bitmapheight;
      if scale2<scale then
       scale:=scale2;
     end;
   end;
  end;
  if scale<0.01 then
   scale:=0.01;
  if scale>10 then
   scale:=10;
  // Sets page size and orientation
  bitmap.Width:=Round(awidth*scale);
  bitmap.Height:=Round(aheight*scale);
  if bitmap.Width<1 then
   bitmap.Width:=1;
  if bitmap.Height<1 then
   bitmap.Height:=1;

  Bitmap.Canvas.Brush.Style:=bsSolid;
  Bitmap.Canvas.Brush.Color:=report.BackColor;
  rec.Top:=0;
  rec.Left:=0;
  rec.Right:=Bitmap.Width-1;
  rec.Bottom:=Bitmap.Height-1;
  bitmap.Canvas.FillRect(rec);

  // Draw Page Margins none for qt driver because they
  // are very innacurate
{  QPrinter_margins(QPrinterH(Printer.Handle),@amargins);
  bitmap.Canvas.Pen.Style:=psSolid;
  bitmap.Canvas.Pen.Color:=clBlack;
  bitmap.Canvas.Brush.Style:=bsclear;
  bitmap.Canvas.rectangle(amargins.cx,amargins.cy,
  rec.Right-amargins.cx,rec.Bottom-amargins.cy);
}

 end;
end;

procedure TRpQtDriver.EndDocument;
begin
 FIntPageSize.Custom:=false;
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

procedure TRpQtDriver.TextExtent(atext:TRpTextObject;var extent:TPoint);
var
 dpix,dpiy:integer;
 Canvas:TCanvas;
 aalign:integeR;
 arec:Trect;
begin
 if atext.FontRotation<>0 then
  exit;
 if atext.CutText then
  exit;
 if (toprinter) then
 begin
  // If to printer then begin doc
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
{$IFDEF MSWINDOWS}
 Canvas.Font.Name:=atext.WFontName;
{$ENDIF}
{$IFDEF LINUX}
 Canvas.Font.Name:=atext.LFontName;
{$ENDIF}
 Canvas.Font.Style:=IntegerToFontStyle(atext.FontStyle);
 Canvas.Font.Size:=atext.FontSize;
 aalign:=atext.Alignment;
 if Not atext.CutText then
  aalign:=aalign or Integer(AlignmentFlags_DontClip);
 if atext.Wordwrap then
  aalign:=aalign or Integer(AlignmentFlags_WordBreak);
 arec.Left:=0;
 arec.Top:=0;
 arec.Right:=Round(extent.X*dpix/TWIPS_PER_INCHESS);
 arec.Bottom:=0;
 Canvas.TextExtent(atext.Text,arec,aalign);
 extent.Y:=Round(arec.Bottom/dpiy*TWIPS_PER_INCHESS);
 extent.X:=Round(arec.Right/dpix*TWIPS_PER_INCHESS);
end;

procedure PrintObject(Canvas:TCanvas;page:TRpMetafilePage;obj:TRpMetaObject;
 dpix,dpiy:integer;scale:double;offset:TPoint);
var
 posx,posy:integer;
 rec:TRect;
 atext:Widestring;
// recsrc:TRect;
 X, Y, W, H, S: Integer;
 Width,Height:integer;
 stream:TMemoryStream;
 bitmap:TBitmap;
 aalign:Integer;
 arec,R:TRect;
begin
 // Switch to device points
 posx:=round((obj.Left+offset.X)*dpix*scale/TWIPS_PER_INCHESS);
 posy:=round((obj.Top+offset.Y)*dpiy*scale/TWIPS_PER_INCHESS);
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
    Canvas.Font.Size:=Obj.FontSize;
    aalign:=obj.Alignment;
    if Not obj.CutText then
     aalign:=aalign or Integer(AlignmentFlags_DontClip);
    if obj.Wordwrap then
     aalign:=aalign or Integer(AlignmentFlags_WordBreak);
    rec.Left:=posx;
    rec.Top:=posy;
    rec.Right:=posx+round(obj.Width*dpix*scale/TWIPS_PER_INCHESS);
    rec.Bottom:=posy+round(obj.Height*dpiy*scale/TWIPS_PER_INCHESS);
    // Not Transparent
    if Not obj.Transparent then
    begin
     arec:=rec;
     Canvas.TextExtent(page.GetText(obj),arec,aalign);
     Canvas.Brush.Style:=bsSolid;
     Canvas.Brush.Color:=obj.BackColor;
     Canvas.FillRect(arec);
    end;
    Canvas.Brush.Style:=bsClear;
    atext:=page.GetText(Obj);
    if obj.FontRotation<>0 then
    begin
     Canvas.Start;
     try
      QPainter_setFont(Canvas.Handle, Canvas.Font.Handle);
      QPainter_setPen(Canvas.Handle, Canvas.Font.FontPen);
      QPainter_save(Canvas.Handle);
      try
       QPainter_translate(Canvas.Handle,posx,posy+Round(Canvas.Font.Size/72*dpiy)*scale);
       QPainter_rotate(Canvas.Handle,-obj.FontRotation/10);
       QPainter_scale(Canvas.Handle,scale,scale);
       // Qt driver does not support oriented multiline text
       QPainter_drawText(Canvas.Handle,0,0,PWideString(@atext),Length(Atext));
      finally
       QPainter_restore(Canvas.Handle);
      end;
     finally
      Canvas.Stop;
     end;
    end
    else
    begin
     posx:=round((obj.Left+offset.X)*dpix/TWIPS_PER_INCHESS);
     posy:=round((obj.Top+offset.X)*dpiy/TWIPS_PER_INCHESS);
     rec.Left:=posx;
     rec.Top:=posy;
     rec.Right:=posx+round(obj.Width*dpix/TWIPS_PER_INCHESS);
     rec.Bottom:=posy+round(obj.Height*dpiy/TWIPS_PER_INCHESS);
     Canvas.Start;
     try      R.Left := posx;
      R.Top := posy;
      R.Right := rec.Right;
      R.Bottom := rec.Bottom;
      QPainter_setFont(Canvas.Handle, Canvas.Font.Handle);
      QPainter_setPen(Canvas.Handle, Canvas.Font.FontPen);
      QPainter_save(Canvas.Handle);
      try
       QPainter_scale(Canvas.Handle,scale,scale);       QPainter_drawText(Canvas.Handle, @R, aalign, PWideString(@atext), -1,        @Rec, nil);      finally       QPainter_restore(Canvas.Handle);
      end;
     finally
       Canvas.Stop;
      end;
//      Canvas.TextRect(rec,posx,posy,atext,aalign);
    end;
   end;
  rpMetaDraw:
   begin
    Width:=round(obj.Width*dpix*scale/TWIPS_PER_INCHESS);
    Height:=round(obj.Height*dpiy*scale/TWIPS_PER_INCHESS);
    Canvas.Pen.Color:=obj.Pencolor;
    Canvas.Pen.Style:=TPenStyle(obj.PenStyle);
    Canvas.Brush.Color:=obj.BrushColor;
    Canvas.Brush.Style:=TBrushStyle(obj.BrushStyle);
    Canvas.Pen.Width:=Round(dpix*scale*obj.PenWidth/TWIPS_PER_INCHESS);
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
    if TRpShapeType(obj.DrawStyle) in [rpsSquare, rpsRoundSquare, rpsCircle] then
    begin
     Inc(X, (W - S) div 2);
     Inc(Y, (H - S) div 2);
     W := S;
     H := S;
    end;
    case TRpShapeType(obj.DrawStyle) of
     rpsRectangle, rpsSquare:
      Canvas.Rectangle(X+PosX, Y+PosY, X+PosX + W, Y +PosY+ H);
     rpsRoundRect, rpsRoundSquare:
      Canvas.RoundRect(X, Y, X + W, Y + H, S div 4, S div 4);
     rpsCircle, rpsEllipse:
      Canvas.Ellipse(X+PosX, Y+PosY, X+PosX + W, Y+PosY + H);
     rpsHorzLine:
      begin
       Canvas.MoveTo(X+PosX, Y+PosY);
       Canvas.LineTo(X+PosX+W, Y+PosY);
      end;
     rpsVertLine:
      begin
       Canvas.MoveTo(X+PosX, Y+PosY);
       Canvas.LineTo(X+PosX, Y+PosY+H);
      end;
     rpsOblique1:
      begin
       Canvas.MoveTo(X+PosX, Y+PosY);
       Canvas.LineTo(X+PosX+W, Y+PosY+H);
      end;
     rpsOblique2:
      begin
       Canvas.MoveTo(X+PosX, Y+PosY+H);
       Canvas.LineTo(X+PosX+W, Y+PosY);
      end;
    end;
   end;
  rpMetaImage:
   begin
    Width:=round(obj.Width*dpix*scale/TWIPS_PER_INCHESS);
    Height:=round(obj.Height*dpiy*scale/TWIPS_PER_INCHESS);
    rec.Top:=PosY;
    rec.Left:=PosX;
    rec.Bottom:=rec.Top+Height-1;
    rec.Right:=rec.Left+Width-1;

    stream:=page.GetStream(obj);
    bitmap:=TBitmap.Create;
    try
     bitmap.LoadFromStream(stream);
//     Copy mode does not work for Stretchdraw
//     Canvas.CopyMode:=TCopyMode(obj.CopyMode);

     case TRpImageDrawStyle(obj.DrawImageStyle) of
      rpDrawFull:
       begin
        rec.Bottom:=rec.Top+round((bitmap.height/obj.dpires)*dpiy*scale)-1;
        rec.Right:=rec.Left+round((bitmap.width/obj.dpires)*dpix*scale)-1;
        Canvas.StretchDraw(rec,bitmap);
       end;
      rpDrawStretch:
       Canvas.StretchDraw(rec,bitmap);
      rpDrawCrop:
       begin
//      Crop Should cut graphic but it don't work
//        recsrc.Top:=0;
//        recsrc.Left:=0;
//        recsrc.Bottom:=Height-1;
//        recsrc.Right:=Width-1;
//        Canvas.CopyRect(rec,bitmap.canvas,recsrc);
        Canvas.Draw(PosX,PosY,bitmap);
       end;
      rpDrawTile:
       begin
        Canvas.TiledDraw(rec,bitmap);
       end;
     end;
    finally
     bitmap.Free;
    end;
   end;
 end;
end;

procedure TRpQtDriver.DrawPage(apage:TRpMetaFilePage);
var
 j:integer;
begin
 for j:=0 to apage.ObjectCount-1 do
 begin
  DrawObject(apage,apage.Objects[j]);
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
 PrintObject(Canvas,page,obj,dpix,dpiy,scale,offset);
end;

function TRpQtDriver.AllowCopies:boolean;
begin
 Result:=false;
end;


function TrpQtDriver.GetPageSize:TPoint;
begin
 // If no printer installed get A4 pagesize
 if Printer.printers.count<1 then
 begin
  result.y:=16637;
  result.x:=12047;
  exit;
 end;
 if FIntPageSize.Custom then
 begin
  Result.X:=FIntPageSize.CustomWidth;
  Result.Y:=FIntPageSize.CustomHeight;
 end
 else
 begin
  Result.x:=Round((Printer.PageWidth/Printer.XDPI)*TWIPS_PER_INCHESS);
  Result.y:=Round((Printer.PageHeight/Printer.YDPI)*TWIPS_PER_INCHESS);
 end;
end;


function TRpQTDriver.SetPagesize(PagesizeQt:TPageSizeQt):TPoint;
begin
 FIntPageSize:=PageSizeQt;
 if FIntPageSize.Custom then
 begin
  Result.X:=PagesizeQt.CustomWidth;
  Result.Y:=PagesizeQt.CustomHeight;
 end
 else
  Result:=InternalSetPageSize(PagesizeQT.Indexqt);
end;

function TRpQTDriver.InternalSetPagesize(PagesizeQt:integer):TPoint;
begin
 Printer.PrintAdapter.PageSize:=TPageSize(PagesizeQT);
 if FOrientation<>rpOrientationDefault then
 begin
  if FOrientation=rpOrientationPortrait then
   Printer.Orientation:=poPortrait
  else
   Printer.Orientation:=poLandscape;
 end;
 Result:=GetPageSize;
end;

procedure TRpQTDriver.SetOrientation(Orientation:TRpOrientation);
begin
 FOrientation:=Orientation;
 OldOrientation:=Printer.Orientation;
 if Orientation=rpOrientationPortrait then
 begin
  if Printer.Orientation<>poPortrait then
   Printer.Orientation:=poPortrait;
 end
 else
 if Orientation=rpOrientationLandscape then
  if Printer.Orientation<>poLandsCape then
   Printer.Orientation:=poLandsCape;
end;


procedure DoPrintMetafile(metafile:TRpMetafileReport;tittle:string;
 aform:TFRpQtProgress;allpages:boolean;frompage,topage,copies:integer;
 collate:boolean;printerindex:TRpPrinterSelect);
var
 i:integer;
 j:integer;
 apage:TRpMetafilePage;
 pagecopies:integer;
 reportcopies:integer;
 dpix,dpiy:integer;
 count1,count2:integer;
{$IFDEF MSWINDOWS}
    mmfirst,mmlast:DWORD;
{$ENDIF}
{$IFDEF LINUX}
    milifirst,mililast:TDatetime;
{$ENDIF}
    difmilis:int64;
 totalcount:integer;
 offset:TPoint;
begin
 offset:=PrinterSelection(printerindex);
 // Get the time
{$IFDEF MSWINDOWS}
 mmfirst:=TimeGetTime;
{$ENDIF}
{$IFDEF LINUX}
 milifirst:=now;
{$ENDIF}
 printer.Title:=tittle;
 // Sets page size and orientation
 if metafile.Orientation<>rpOrientationDefault then
 begin
  if metafile.Orientation=rpOrientationPortrait then
   printer.Orientation:=poPortrait
  else
   printer.Orientation:=poLandscape;
 end;
 // Sets pagesize
 if metafile.PageSize>=0 then
  Printer.PrintAdapter.PageSize:=TPageSize(metafile.PageSize);

 pagecopies:=1;
 reportcopies:=1;
 if copies>1 then
 begin
  if collate then
   reportcopies:=copies
  else
   pagecopies:=copies;
 end;
 if allpages then
 begin
  frompage:=0;
  topage:=metafile.PageCount-1;
 end
 else
 begin
  frompage:=frompage-1;
  topage:=topage-1;
  if topage>metafile.PageCount-1 then
   topage:=metafile.PageCount-1;
 end;
 printer.Begindoc;
 try
  dpix:=printer.XDPI;
  dpiy:=printer.YDPI;
  totalcount:=0;
  for count1:=0 to reportcopies-1 do
  begin
   for i:=frompage to topage do
   begin
    for count2:=0 to pagecopies-1 do
    begin
     if totalcount>0 then
      printer.NewPage;
     inc(totalcount);
     apage:=metafile.Pages[i];
     for j:=0 to apage.ObjectCount-1 do
     begin
      PrintObject(Printer.Canvas,apage,apage.Objects[j],dpix,dpiy,1,offset);
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

function PrintMetafile(metafile:TRpMetafileReport;tittle:string;
 showprogress,allpages:boolean;frompage,topage,copies:integer;
  collate:boolean;printerindex:TRpPrinterSelect):boolean;
var
 dia:TFRpQtProgress;
begin
 Result:=true;
 if Not ShowProgress then
 begin
  DoPrintMetafile(metafile,tittle,nil,allpages,frompage,topage,copies,collate,printerindex);
  exit;
 end;
 dia:=TFRpQtProgress.Create(Application);
 try
  dia.oldonidle:=Application.OnIdle;
  try
   dia.metafile:=metafile;
   dia.printerindex:=printerindex;
   dia.tittle:=tittle;
   dia.allpages:=allpages;
   dia.frompage:=frompage;
   dia.topage:=topage;
   dia.copies:=copies;
   dia.collate:=collate;
   Application.OnIdle:=dia.AppIdle;
   dia.ShowModal;
   Result:=Not dia.cancelled;
  finally
   Application.OnIdle:=dia.oldonidle;
  end;
 finally
  dia.free;
 end;
end;

procedure TFRpQtProgress.FormCreate(Sender: TObject);
begin
 LRecordCount.Font.Style:=[fsBold];
 LTittle.Font.Style:=[fsBold];

 BCancel.Caption:=TranslateStr(94,BCancel.Caption);
 LTitle.Caption:=TranslateStr(252,LTitle.Caption);
 LProcessing.Caption:=TranslateStr(253,LProcessing.Caption);

 SetInitialBounds;
end;

procedure TFRpQtProgress.AppIdle(Sender:TObject;var done:boolean);
begin
 cancelled:=false;
 Application.OnIdle:=nil;
 done:=false;
 LTittle.Caption:=tittle;
 Lprocessing.Visible:=true;
 DoPrintMetafile(metafile,tittle,self,allpages,frompage,topage,copies,collate,printerindex);
end;


procedure TFRpQtProgress.BCancelClick(Sender: TObject);
begin
 cancelled:=true;
end;


procedure TFRpQtProgress.RepProgress(Sender:TRpReport;var docancel:boolean);
begin
 if Not Assigned(LRecordCount) then
  exit;
 LRecordCount.Caption:=IntToStr(Sender.CurrentSubReportIndex)+':'+SRpPage+':'+
 FormatFloat('#########,####',Sender.PageNum)+'-'+FormatFloat('#########,####',Sender.RecordCount);
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

 qtdriver:=TRpQtDriver.Create;
 aqtdriver:=qtdriver;
 oldprogres:=report.OnProgress;
 try
  report.OnProgress:=RepProgress;
  report.PrintAll(qtdriver);
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


procedure TFRpQtProgress.AppIdlePrintRange(Sender:TObject;var done:boolean);
var
 oldprogres:TRpProgressEvent;
begin
 Application.Onidle:=nil;
 done:=false;

 qtdriver:=TRpQtDriver.Create;
 qtdriver.toprinter:=true;
 aqtdriver:=qtdriver;
 oldprogres:=RepProgress;
 try
  report.OnProgress:=RepProgress;
  report.PrintRange(aqtdriver,allpages,frompage,topage,copies);
 finally
  report.OnProgress:=oldprogres;
 end;
 Close;
end;

procedure TFRpQtProgress.AppIdlePrintPDF(Sender:TObject;var done:boolean);
var
 oldprogres:TRpProgressEvent;
begin
 Application.Onidle:=nil;
 done:=false;

 pdfdriver:=TRpPDFDriver.Create;
 pdfdriver.filename:=filename;
 pdfdriver.compressed:=pdfcompressed;
 apdfdriver:=pdfdriver;
 oldprogres:=RepProgress;
 try
  report.OnProgress:=RepProgress;
  report.PrintRange(apdfdriver,allpages,frompage,topage,copies);
 finally
  report.OnProgress:=oldprogres;
 end;
 Close;
end;



function PrintReport(report:TRpReport;Caption:string;progress:boolean;
  allpages:boolean;frompage,topage,copies:integer;collate:boolean):Boolean;
var
{$IFDEF LINUXPRINTBUG}
 abuffer:array [0..L_tmpnam] of char;
 theparams:array [0..20] of pchar;
 params:array[0..20] of string;
 paramcount:integer;
 afilename:string;
 child:__pid_t;
 i:integer;
{$ENDIF}
 qtdriver:TRpQtDriver;
 aqtdriver:IRpPrintDriver;
 forcecalculation:boolean;
 dia:TFRpQtProgress;
 oldonidle:TIdleEvent;
begin
 Result:=true;
 forcecalculation:=false;
 if kylixprintbug then
  forcecalculation:=true;
 if ((report.copies>1) and (report.CollateCopies)) then
 begin
  forcecalculation:=true;
 end;
 if report.TwoPass then
  forcecalculation:=true;
 if forcecalculation then
 begin
  if progress then
  begin
   if Not CalcReportWidthProgress(report) then
   begin
    Result:=false;
    exit;
   end;
  end
  else
  begin
   qtdriver:=TRpQtDriver.Create;
   aqtdriver:=qtdriver;
   report.PrintAll(qtdriver);
  end;
 end;
 // A bug in Kylix 2 does not allow printing
 // when using dbexpress in some systems
 if not kylixprintbug then
 begin
   if forcecalculation then
    PrintMetafile(report.Metafile,Caption,progress,allpages,frompage,topage,copies,collate,report.PrinterSelect)
   else
   begin
    if progress then
    begin
     // Assign appidle frompage to page...
     dia:=TFRpQtProgress.Create(Application);
     try
      dia.allpages:=allpages;
      dia.frompage:=frompage;
      dia.topage:=topage;
      dia.copies:=copies;
      dia.report:=report;
      dia.collate:=collate;
      oldonidle:=Application.Onidle;
      try
       Application.OnIdle:=dia.AppIdlePrintRange;
       dia.ShowModal;
      finally
       Application.OnIdle:=oldonidle;
      end;
     finally
      dia.Free;
     end;
    end
    else
    begin
     qtdriver:=TRpQtDriver.Create;
     aqtdriver:=qtdriver;
     qtdriver.toprinter:=true;
     report.PrintRange(aqtdriver,allpages,frompage,topage,copies);
    end;
   end;
 end
{$IFDEF LINUXPRINTBUG}
 else
 begin
   // When compiling metaview the bug can be skiped
   // Saves the metafile
   // Selects the printer for that report
   tmpnam(abuffer);
   afilename:=StrPas(abuffer);
   report.Metafile.SaveToFile(afilename);
   params[0]:='metaprint';
   params[1]:='-d';
   params[2]:='-copies';
   params[3]:=IntToStr(copies);
   params[4]:='-p';
   params[5]:=IntToStr(integer(report.PrinterSelect));
   paramcount:=6;
   if collate then
   begin
    params[paramcount]:='-collate';
    inc(paramcount);
   end;
   if not allpages then
   begin
    params[paramcount]:='-from';
    inc(paramcount);
    params[paramcount]:=IntToStr(frompage);
    inc(paramcount);
    params[paramcount]:='-to';
    inc(paramcount);
    params[paramcount]:=IntToStr(topage);
    inc(paramcount);
   end;
   if not progress then
   begin
    params[paramcount]:='-q';
    inc(paramcount);
   end;
   params[paramcount]:=afilename;
   inc(paramcount);

   for i:=0 to paramcount-1 do
   begin
    theparams[i]:=Pchar(params[i]);
   end;
   theparams[paramcount]:=nil;

   child:=fork;
   if child=-1 then
    Raise Exception.Create(SRpErrorFork);
   if child<>0 then
   begin
    if (-1=execv(theparams[0],PPChar(@theparams))) then
    begin
     try
      RaiseLastOsError;
     except
      on E:Exception do
      begin
       E.Message:=SRpErrorFork+':execv:'+IntToStr(errno)+'-'+E.Message;
       Raise;
      end;
     end;
    end;
   end
 end;
{$ENDIF}
end;


procedure TFRpQtProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 qtdriver:=nil;
end;


function ExportReportToPDF(report:TRpReport;Caption:string;progress:boolean;
  allpages:boolean;frompage,topage:integer;
  showprintdialog:boolean;filename:string;compressed:boolean):Boolean;
var
 copies:integer;
 collate:boolean;
 dia:TFRpQtProgress;
 oldonidle:TIdleEvent;
 pdfdriver:TRpPDFDriver;
 apdfdriver:IRpPrintDriver;
begin
 Result:=false;
 allpages:=true;
 collate:=false;
 copies:=1;
 if showprintdialog then
 begin
  if Not rpprintdia.DoShowPrintDialog(allpages,frompage,topage,copies,collate,true) then
   exit;
 end;
 if progress then
 begin
  // Assign appidle frompage to page...
  dia:=TFRpQtProgress.Create(Application);
  try
   dia.allpages:=allpages;
   dia.frompage:=frompage;
   dia.topage:=topage;
   dia.copies:=copies;
   dia.report:=report;
   dia.filename:=filename;
   dia.pdfcompressed:=compressed;
   oldonidle:=Application.Onidle;
   try
    Application.OnIdle:=dia.AppIdlePrintPdf;
    dia.ShowModal;
   finally
    Application.OnIdle:=oldonidle;
   end;
  finally
   dia.Free;
  end;
 end
 else
 begin
  pdfdriver:=TRpPDFDriver.Create;
  pdfdriver.filename:=filename;
  pdfdriver.compressed:=compressed;
  apdfdriver:=pdfdriver;
  report.PrintRange(apdfdriver,allpages,frompage,topage,copies);
  Result:=True;
 end;
end;

procedure TRpQtDriver.GraphicExtent(Stream:TMemoryStream;var extent:TPoint;dpi:integer);
var
 graphic:TBitmap;
begin
 if dpi<=0 then
  exit;
 graphic:=TBitmap.Create;
 try
  Graphic.LoadFromStream(Stream);
  extent.X:=Round(graphic.width/dpi*TWIPS_PER_INCHESS);
  extent.Y:=Round(graphic.height/dpi*TWIPS_PER_INCHESS);
 finally
  graphic.Free;
 end;
end;

function PrinterSelection(printerindex:TRpPrinterSelect):TPoint;
var
 printername:String;
 abuffer:widestring;
 index:integer;
 qtprintername:WideString;
 offset:TPoint;
begin
 printername:=GetPrinterConfigName(printerindex);
 offset:=GetPrinterOffset(printerindex);
 if length(printername)>0 then
 begin
  index:=Printer.Printers.IndexOf(printername);
  if index>=0 then
  begin
   Printer.SetPrinter(printername);
  end;
 end;
 // Gets the printer name if no printer selected select the first one
 if Printer.Printers.Count>0 then
 begin
  SetLength(abuffer,500);
  QPrinter_printerName(QPrinterH(Printer.Handle),@abuffer);
  qtprintername:=abuffer;
  if Length(qtprintername)<1 then
  begin
   Printer.SetPrinter(Printer.Printers.Strings[0]);
  end;
 end;
 Result:=offset;
end;

procedure TRpQtDriver.SelectPrinter(printerindex:TRpPrinterSelect);
begin
 offset:=PrinterSelection(printerindex);
end;

procedure PageSizeSelection(rpPageSize:TPageSizeQt);
begin
 if Printer.Printers.Count<1 then
  exit;
 if rpPageSize.Custom then
  exit;
 Printer.PrintAdapter.PageSize:=TPageSize(rpPagesize.Indexqt);
end;

procedure OrientationSelection(neworientation:TRpOrientation);
begin
 if Printer.Printers.Count<1 then
  exit;
 if neworientation=rpOrientationDefault then
  exit;
 if neworientation=rpOrientationPortrait then
  Printer.Orientation:=poPortrait
 else
  Printer.Orientation:=poLandscape;
end;

end.



