{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpgdidriver                                     }
{       TRpGDIDriver: Printer driver for  VCL Lib       }
{       can be used only for windows                    }
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

unit rpgdidriver;

interface

{$I rpconf.inc}

uses
 mmsystem,windows,
 Classes,sysutils,rpmetafile,rpmdconsts,Graphics,Forms,
 rpmunits,Printers,Dialogs, Controls,rpgdifonts,
 StdCtrls,ExtCtrls,rppdffile,rpgraphutilsvcl,
{$IFDEF USEVARIANTS}
 types,
{$ENDIF}
 rptypes,rpvgraphutils,jpeg,
 rpreport,rppdfdriver, QStdCtrls, QControls;


const
 METAPRINTPROGRESS_INTERVAL=20;
type
  TRpGDIDriver=class;
  TFRpVCLProgress = class(TForm)
    BCancel: TButton;
    LProcessing: TLabel;
    LRecordCount: TLabel;
    LTitle: TLabel;
    LTittle: TLabel;
    BOK: TButton;
    GPrintRange: TGroupBox;
    EFrom: TEdit;
    ETo: TEdit;
    LTo: TLabel;
    LFrom: TLabel;
    RadioAll: TRadioButton;
    RadioRange: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    allpages,collate:boolean;
    frompage,topage,copies:integer;
    devicefonts:boolean;
    printerindex:TRpPrinterSelect;
    dook:boolean;
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
    GDIDriver:TRpGDIDriver;
    aGDIDriver:IRpPrintDriver;
    pdfdriver:TRpPDFDriver;
    apdfdriver:IRpPrintDriver;
  end;


 TRpGDIDriver=class(TInterfacedObject,IRpPrintDriver)
  private
   intdpix,intdpiy:integer;
   metacanvas:TMetafilecanvas;
   meta:TMetafile;
   pagecliprec:TRect;
   selectedprinter:TRpPrinterSelect;
   procedure SendAfterPrintOperations;
  public
   offset:TPoint;
   lockedpagesize:boolean;
   CurrentPageSize:Tpoint;
   bitmap:TBitmap;
   dpi:integer;
   toprinter:boolean;
   scale:double;
   pagemargins:TRect;
   drawclippingregion:boolean;
   oldpagesize,pagesize:TGDIPageSize;
   oldorientation:TPrinterOrientation;
   orientationset:boolean;
   devicefonts:boolean;
   neverdevicefonts:boolean;
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
   function AllowCopies:boolean;stdcall;
   function GetPageSize:TPoint;stdcall;
   function SetPagesize(PagesizeQt:TPageSizeQt):TPoint;stdcall;
   procedure TextExtent(atext:TRpTextObject;var extent:TPoint);stdcall;
   procedure GraphicExtent(Stream:TMemoryStream;var extent:TPoint;dpi:integer);stdcall;
   procedure SetOrientation(Orientation:TRpOrientation);stdcall;
   procedure SelectPrinter(printerindex:TRpPrinterSelect);stdcall;
   constructor Create;
   destructor Destroy;override;
  end;

function PrintMetafile (metafile:TRpMetafileReport; tittle:string;
 showprogress,allpages:boolean; frompage,topage,copies:integer;
  collate:boolean; devicefonts:boolean; printerindex:TRpPrinterSelect=pRpDefaultPrinter):boolean;
function CalcReportWidthProgress (report:TRpReport):boolean;
function PrintReport (report:TRpReport; Caption:string; progress:boolean;
  allpages:boolean; frompage,topage,copies:integer; collate:boolean):Boolean;
function ExportReportToPDF (report:TRpReport; Caption:string; progress:boolean;
  allpages:boolean; frompage,topage:integer;
  showprintdialog:boolean; filename:string; compressed:boolean;collate:boolean):Boolean;
function DoShowPrintDialog (var allpages:boolean;
 var frompage,topage,copies:integer; var collate:boolean;disablecopies:boolean=false) :boolean;
function PrinterSelection (printerindex:TRpPrinterSelect) :TPoint;
procedure PageSizeSelection (rpPageSize:TPageSizeQt);
procedure OrientationSelection (neworientation:TRpOrientation);

implementation



{$R *.dfm}

const
 AlignmentFlags_SingleLine=64;
 AlignmentFlags_AlignHCenter = 4 { $4 };
 AlignmentFlags_AlignTop = 8 { $8 };
 AlignmentFlags_AlignBottom = 16 { $10 };
 AlignmentFlags_AlignVCenter = 32 { $20 };
 AlignmentFlags_AlignLeft = 1 { $1 };
 AlignmentFlags_AlignRight = 2 { $2 };

 // Qt Page sizes
type
  TPageSize = (psA4, psB5, psLetter, psLegal, psExecutive, psA0, psA1, psA2,
    psA3, psA5, psA6, psA7, psA8, psA9, psB0, psB1, psB10, psB2, psB3, psB4, psB6,
    psB7, psB8, psB9, psC5E, psComm10E, psDLE, psFolio, psLedger, psTabloid, psNPageSize);
  TPageWidthHeight = record
    Width: Integer;
    Height: Integer;
  end;


const PageSizeNames: array [psA4..psNPageSize] of widestring =
('A4', 'B5','Letter','Legal','Executive','A0', 'A1', 'A2',
    'A3', 'A5', 'A6', 'A7', 'A8', 'A9', 'B0', 'B1', 'B10', 'B2',
     'B3', 'B4', 'B6','B7', 'B8', 'B9', 'C5E', 'Comm10E',
     'DLE', 'Folio', 'Ledger', 'Tabloid', 'psNPageSize');

const
  PageSizeArray: array[0..30] of TPageWidthHeight =
    (
      (Width: 8268; Height: 11693),  // psA4
      (Width: 7165; Height: 10118),  // psB5
      (Width: 8500; Height: 11000),  // psLetter
      (Width: 8500; Height: 14000),  // psLegal
      (Width: 7500; Height: 10000),  // psExecutive
      (Width: 33110; Height: 46811), // psA0
      (Width: 23386; Height: 33110), // psA1
      (Width: 16535; Height: 23386), // psA2
      (Width: 11693; Height: 16535), // psA3
      (Width: 5827; Height: 8268),   // psA5
      (Width: 4134; Height: 5827),   // psA6
      (Width: 2913; Height: 4134),   // psA7
      (Width: 2047; Height: 2913),   // psA8
      (Width: 1457; Height: 2047),   // psA9
      (Width: 40551; Height: 57323), // psB0
      (Width: 28661; Height: 40551), // psB1
      (Width: 1260; Height: 1772),   // psB10
      (Width: 20276; Height: 28661), // psB2
      (Width: 14331; Height: 20276), // psB3
      (Width: 10118; Height: 14331), // psB4
      (Width: 5039; Height: 7165),   // psB6
      (Width: 3583; Height: 5039),   // psB7
      (Width: 2520; Height: 3583),   // psB8
      (Width: 1772; Height: 2520),   // psB9
      (Width: 6417; Height: 9016),   // psC5E
      (Width: 4125; Height: 9500),   // psComm10E
      (Width: 4331; Height: 8661),   // psDLE
      (Width: 8250; Height: 13000),  // psFolio
      (Width: 17000; Height: 11000), // psLedger
      (Width: 11000; Height: 17000), // psTabloid
      (Width: -1; Height: -1)        // psNPageSize
    );

function DoShowPrintDialog(var allpages:boolean;
 var frompage,topage,copies:integer;var collate:boolean;disablecopies:boolean=false):boolean;
var
 dia:TPrintDialog;
 diarange:TFRpVCLProgress;
begin
 Result:=False;
 if disablecopies then
 begin
  diarange:=TFRpVCLProgress.Create(Application);
  try
   diarange.BOK.Visible:=true;
   diarange.GPrintRange.Visible:=true;
   diarange.RadioAll.Checked:=allpages;
   diarange.RadioRange.Checked:=not allpages;
   diarange.ActiveControl:=diarange.BOK;
   diarange.Frompage:=frompage;
   diarange.ToPage:=topage;
   diarange.showmodal;
   if diarange.dook then
   begin
    frompage:=diarange.frompage;
    topage:=diarange.topage;
    allpages:=diarange.Radioall.Checked;
    Result:=true;
   end
  finally
   diarange.free;
  end;
  exit;
 end;
 dia:=TPrintDialog.Create(Application);
 try
  dia.Options:=[poPageNums,poWarning,
        poPrintToFile];
  dia.MinPage:=1;
  dia.MaxPage:=65535;
  dia.collate:=collate;
  dia.copies:=copies;
  dia.frompage:=frompage;
  dia.topage:=topage;
  if dia.execute then
  begin
   allpages:=false;
   collate:=dia.collate;
   copies:=dia.copies;
   frompage:=dia.frompage;
   topage:=dia.topage;
   Result:=True;
  end;
 finally
  dia.free;
 end;
end;


constructor TRpGDIDriver.Create;
begin
 // By default 1:1 scale
 dpi:=Screen.PixelsPerInch;
 drawclippingregion:=true;
 oldpagesize.PageIndex:=-1;
 scale:=1;
end;

destructor TRpGDIDriver.Destroy;
begin
 if assigned(metacanvas) then
 begin
  metacanvas.free;
  metacanvas:=nil;
 end;
 if assigned(meta) then
 begin
  meta.free;
  meta:=nil;
 end;
 if assigned(bitmap) then
 begin
  bitmap.free;
  bitmap:=nil;
 end;
 inherited Destroy;
end;

procedure TRpGDIDriver.NewDocument(report:TrpMetafileReport);
var
 awidth,aheight:integer;
 rec:TRect;
 asize:TPoint;
 aregion:HRGN;
 scale2:double;
begin
 if devicefonts then
 begin
  UpdatePrinterFontList;
 end;
 if ToPrinter then
 begin
  printer.Title:='Untitled';
  SetOrientation(report.Orientation);
  // Gets pagesize
  asize:=GetPageSize;
  pagemargins:=GetPageMarginsTWIPS;
  if Length(printer.Title)<1 then
   printer.Title:=SRpUntitled;
  printer.BeginDoc;
  intdpix:=GetDeviceCaps(Printer.Canvas.handle,LOGPIXELSX); //  printer.XDPI;
  intdpiy:=GetDeviceCaps(Printer.Canvas.handle,LOGPIXELSY);  // printer.YDPI;
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
  // Sets Orientation
  SetOrientation(report.Orientation);
  // Gets pagesize
  if lockedpagesize then
  begin
   asize:=CurrentPageSize;
  end
  else
  begin
   asize:=GetPageSize;
   pagemargins:=GetPageMarginsTWIPS;
   CurrentPageSize:=asize;
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
      scale:=(clientwidth-GetSystemMetrics(SM_CYHSCROLL))/bitmapwidth;
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

  bitmap.Width:=Round(awidth*scale);
  bitmap.Height:=Round(aheight*scale);
  if bitmap.Width<1 then
   bitmap.Width:=1;
  if bitmap.Height<1 then
   bitmap.Height:=1;

  Bitmap.Canvas.Brush.Style:=bsSolid;
  Bitmap.Canvas.Brush.Color:=CLXColorToVCLColor(report.BackColor);
  rec.Top:=0;
  rec.Left:=0;
  rec.Right:=Bitmap.Width-1;
  rec.Bottom:=Bitmap.Height-1;
  bitmap.Canvas.FillRect(rec);
  // Define clipping region
  rec.Left:=Round((pagemargins.Left/TWIPS_PER_INCHESS)*dpi*scale);
  rec.Top:=Round((pagemargins.Top/TWIPS_PER_INCHESS)*dpi*scale);
  rec.Right:=Round((pagemargins.Right/TWIPS_PER_INCHESS)*dpi*scale);
  rec.Bottom:=Round((pagemargins.Bottom/TWIPS_PER_INCHESS)*dpi*scale);
  pagecliprec:=rec;
  if (Not drawclippingregion) then
  begin
   aregion:=CreateRectRgn(rec.Left,rec.Top,rec.Right,rec.Bottom);
   SelectClipRgn(bitmap.Canvas.handle,aregion);
  end;
 end;
end;



procedure TRpGDIDriver.EndDocument;
begin
 if toprinter then
 begin
  printer.EndDoc;
  // Send Especial operations
  SendAfterPrintOperations;
 end
 else
 begin
  // Does nothing because the last bitmap can be usefull
 end;
 if orientationset then
 begin
  Printer.Orientation:=oldorientation;
  orientationset:=false;
 end;
 if oldpagesize.PageIndex<>-1 then
 begin
  SetCurrentPaper(oldpagesize);
  oldpagesize.PageIndex:=-1;
 end;
end;

procedure TRpGDIDriver.AbortDocument;
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
 if orientationset then
 begin
  Printer.Orientation:=oldorientation;
  orientationset:=false;
 end;
 if oldpagesize.PageIndex<>-1 then
 begin
  SetCurrentPaper(oldpagesize);
  oldpagesize.PageIndex:=-1;
 end;
end;

procedure TRpGDIDriver.NewPage;
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

procedure TRpGDIDriver.EndPage;
var
 rec:TREct;
begin
 // If drawclippingregion then
 if not toprinter then
 begin
  if Not Assigned(bitmap) then
   exit;
  rec:=pagecliprec;
  if drawclippingregion then
  begin
   bitmap.Canvas.Pen.Style:=psSolid;
   bitmap.Canvas.Pen.Color:=clBlack;
   bitmap.Canvas.Brush.Style:=bsclear;
   bitmap.Canvas.rectangle(rec.Left,rec.Top,rec.Right,rec.Bottom);
  end
 end;
end;


procedure TRpGDIDriver.TextExtent(atext:TRpTextObject;var extent:TPoint);
var
 Canvas:TCanvas;
 dpix,dpiy:integer;
 aalign:Cardinal;
 aatext:widestring;
 aansitext:string;
 arec:TRect;
begin
 if atext.FontRotation<>0 then
  exit;
 if atext.CutText then
  exit;
 if (toprinter) then
 begin
  if not printer.Printing then
   Raise Exception.Create(SRpGDIDriverNotInit);
  dpix:=intdpix;
  dpiy:=intdpiy;
  Canvas:=printer.canvas;
 end
 else
 begin
  if not Assigned(bitmap) then
   Raise Exception.Create(SRpGDIDriverNotInit);
  if not Assigned(metacanvas) then
  begin
   if assigned(meta) then
   begin
    meta.free;
    meta:=nil;
   end;
   meta:=TMetafile.Create;
   meta.Enhanced:=true;
   meta.Width:=bitmapwidth;
   meta.Height:=bitmapheight;
   metacanvas:=TMetafileCanvas.Create(meta,0);
  end;
  Canvas:=metacanvas;
  dpix:=Screen.PixelsPerInch;
  dpiy:=Screen.PixelsPerInch;
 end;
 Canvas.Font.Name:=atext.WFontName;
 Canvas.Font.Style:=CLXIntegerToFontStyle(atext.FontStyle);
 Canvas.Font.Size:=atext.FontSize;
 // Find device font
 if devicefonts then
  FindDeviceFont(Canvas.Handle,Canvas.Font,FontSizeToStep(Canvas.Font.Size));
 aalign:=DT_NOPREFIX;
 if (atext.AlignMent AND AlignmentFlags_AlignHCenter)>0 then
  aalign:=aalign or DT_CENTER;
 if (atext.AlignMent AND AlignmentFlags_SingleLine)>0 then
  aalign:=aalign or DT_SINGLELINE;
 if (atext.AlignMent AND AlignmentFlags_AlignLEFT)>0 then
  aalign:=aalign or DT_LEFT;
 if (atext.AlignMent AND AlignmentFlags_AlignRight)>0 then
  aalign:=aalign or DT_RIGHT;
 if atext.WordWrap then
  aalign:=aalign or DT_WORDBREAK;
 if Not atext.CutText then
  aalign:=aalign or DT_NOCLIP;
 aatext:=atext.text;
 aansitext:=aatext;
 arec.Left:=0;
 arec.Top:=0;
 arec.Bottom:=0;
 arec.Right:=Round(extent.X*dpix/TWIPS_PER_INCHESS);
 // calculates the text extent
 // Win9x does not support drawing WideChars
 if IsWindowsNT then
  DrawTextW(Canvas.Handle,PWideChar(aatext),Length(aatext),arec,aalign or DT_CALCRECT)
 else
  DrawTextA(Canvas.Handle,PChar(aansitext),Length(aansitext),arec,aalign or DT_CALCRECT);
 // Transformates to twips
 extent.X:=Round(arec.Right/dpix*TWIPS_PER_INCHESS);
 extent.Y:=Round(arec.Bottom/dpiy*TWIPS_PER_INCHESS);
end;

procedure PrintObject(Canvas:TCanvas;page:TRpMetafilePage;obj:TRpMetaObject;dpix,dpiy:integer;toprinter:boolean;pagemargins:TRect;devicefonts:boolean;offset:TPoint);
var
 posx,posy:integer;
 rec,recsrc:TRect;
 X, Y, W, H, S: Integer;
 Width,Height:integer;
 stream:TMemoryStream;
 bitmap:TBitmap;
 aalign:Cardinal;
 abrushstyle:integer;
 atext:widestring;
 aansitext:string;
 arec:TRect;
 calcrect:boolean;
 alvbottom,alvcenter:boolean;
 rotrad,fsize:double;
 oldrgn:HRGN;
 newrgn:HRGN;
 aresult:integer;
 jpegimage:TJPegImage;
 bitmapwidth,bitmapheight:integer;
begin
 // Switch to device points
 if toprinter then
 begin
  // If printer then must be displaced
  posx:=round((obj.Left-pagemargins.Left+offset.X)*dpix/TWIPS_PER_INCHESS);
  posy:=round((obj.Top-pagemargins.Top+offset.Y)*dpiy/TWIPS_PER_INCHESS);
 end
 else
 begin
  posx:=round(obj.Left*dpix/TWIPS_PER_INCHESS);
  posy:=round(obj.Top*dpiy/TWIPS_PER_INCHESS);
 end;
 case obj.Metatype of
  rpMetaText:
   begin
    Canvas.Font.Name:=page.GetWFontName(Obj);
    Canvas.Font.Color:=CLXColorToVCLColor(Obj.FontColor);
    Canvas.Font.Style:=CLXIntegerToFontStyle(obj.FontStyle);
    Canvas.Font.Size:=Obj.FontSize;
    if obj.FontRotation<>0 then
    begin
     // Find rotated font
     Canvas.Font.Handle:=FindRotatedFont(Canvas.Handle,Canvas.Font,obj.FontRotation);
     // Moves the print position
     rotrad:=obj.FontRotation/10*(2*PI/360);
     fsize:=Obj.FontSize/72*dpiy;
     posx:=posx-Round(fsize*sin(rotrad));
     posy:=posy+Round(fsize-fsize*cos(rotrad));
    end
    else
    begin
     // Find device font
     if devicefonts then
      FindDeviceFont(Canvas.Handle,Canvas.Font,FontSizeToStep(Canvas.Font.Size));
    end;


    aalign:=DT_NOPREFIX;
    if (obj.AlignMent AND AlignmentFlags_AlignHCenter)>0 then
     aalign:=aalign or DT_CENTER;
    if (obj.AlignMent AND AlignmentFlags_SingleLine)>0 then
     aalign:=aalign or DT_SINGLELINE;
    if (obj.AlignMent AND AlignmentFlags_AlignLEFT)>0 then
     aalign:=aalign or DT_LEFT;
    if (obj.AlignMent AND AlignmentFlags_AlignRight)>0 then
     aalign:=aalign or DT_RIGHT;
    if obj.WordWrap then
     aalign:=aalign or DT_WORDBREAK;
    if Not obj.CutText then
     aalign:=aalign or DT_NOCLIP;
    rec.Left:=posx;
    rec.Top:=posy;
    rec.Right:=posx+round(obj.Width*dpix/TWIPS_PER_INCHESS);
    rec.Bottom:=posy+round(obj.Height*dpiy/TWIPS_PER_INCHESS);
    atext:=page.GetText(Obj);
    aansitext:=atext;
    alvbottom:=(obj.AlignMent AND AlignmentFlags_AlignBottom)>0;
    alvcenter:=(obj.AlignMent AND AlignmentFlags_AlignVCenter)>0;

    calcrect:=(not obj.Transparent) or alvbottom or alvcenter;
    arec:=rec;
    if calcrect then
    begin
     // First calculates the text extent
     // Win9x does not support drawing WideChars
     if IsWindowsNT then
      DrawTextW(Canvas.Handle,PWideChar(atext),Length(atext),arec,aalign or DT_CALCRECT)
     else
      DrawTextA(Canvas.Handle,PChar(aansitext),Length(aansitext),arec,aalign or DT_CALCRECT);
     Canvas.Brush.Style:=bsSolid;
     Canvas.Brush.Color:=obj.BackColor;
    end
    else
     Canvas.Brush.Style:=bsClear;
    if alvbottom then
    begin
     rec.Top:=rec.Top+(rec.bottom-arec.bottom)
    end;
    if alvcenter then
    begin
     rec.Top:=rec.Top+((rec.bottom-arec.bottom) div 2);
    end;
    if obj.Transparent then
     SetBkMode(Canvas.Handle,TRANSPARENT)
    else
     SetBkMode(Canvas.Handle,OPAQUE);

    if IsWindowsNT then
     DrawTextW(Canvas.Handle,PWideChar(atext),Length(atext),rec,aalign)
    else
     DrawTextA(Canvas.Handle,PChar(aansitext),Length(aansitext),rec,aalign)
   end;
  rpMetaDraw:
   begin
    Width:=round(obj.Width*dpix/TWIPS_PER_INCHESS);
    Height:=round(obj.Height*dpiy/TWIPS_PER_INCHESS);
    abrushstyle:=obj.BrushStyle;
    if obj.BrushStyle>integer(bsDiagCross) then
     abrushstyle:=integer(bsDiagCross);
    Canvas.Pen.Color:=CLXColorToVCLColor(obj.Pencolor);
    Canvas.Pen.Style:=TPenStyle(obj.PenStyle);
    Canvas.Brush.Color:=CLXColorToVCLColor(obj.BrushColor);
    Canvas.Brush.Style:=TBrushStyle(abrushstyle);
    Canvas.Pen.Width:=Round(dpix*obj.PenWidth/TWIPS_PER_INCHESS);
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
    Width:=round(obj.Width*dpix/TWIPS_PER_INCHESS);
    Height:=round(obj.Height*dpiy/TWIPS_PER_INCHESS);
    rec.Top:=PosY;
    rec.Left:=PosX;
    rec.Bottom:=rec.Top+Height-1;
    rec.Right:=rec.Left+Width-1;

    stream:=page.GetStream(obj);
    bitmap:=TBitmap.Create;
    try
     bitmap.PixelFormat:=pf32bit;
     bitmap.HandleType:=bmDIB;
     if GetJPegInfo(stream,bitmapwidth,bitmapheight) then
     begin
      jpegimage:=TJPegImage.Create;
      try
       jpegimage.LoadFromStream(stream);
       bitmap.Assign(jpegimage);
      finally
       jpegimage.free;
      end;
     end
     else
     // Looks if it's a jpeg image
      bitmap.LoadFromStream(stream);
//     Copy mode does not work for StretDIBBits
//     Canvas.CopyMode:=CLXCopyModeToCopyMode(obj.CopyMode);

     case TRpImageDrawStyle(obj.DrawImageStyle) of
      rpDrawFull:
       begin
        rec.Bottom:=rec.Top+round(bitmap.height/obj.dpires*dpiy)-1;
        rec.Right:=rec.Left+round(bitmap.width/obj.dpires*dpix)-1;
        recsrc.Left:=0;
        recsrc.Top:=0;
        recsrc.Right:=bitmap.Width-1;
        recsrc.Bottom:=bitmap.Height-1;
        DrawBitmap(Canvas,bitmap,rec,recsrc);
       end;
      rpDrawStretch:
       begin
        recsrc.Left:=0;
        recsrc.Top:=0;
        recsrc.Right:=bitmap.Width-1;
        recsrc.Bottom:=bitmap.Height-1;
        DrawBitmap(Canvas,bitmap,rec,recsrc);
       end;
      rpDrawCrop:
       begin
        recsrc.Left:=0;
        recsrc.Top:=0;
        recsrc.Right:=rec.Right-rec.Left;
        recsrc.Bottom:=rec.Bottom-rec.Top;
        DrawBitmap(Canvas,bitmap,rec,recsrc);
       end;
      rpDrawTile:
       begin
        // Set clip region
        oldrgn:=CreateRectRgn(0,0,2,2);
        aresult:=GetClipRgn(Canvas.Handle,oldrgn);
        newrgn:=CreateRectRgn(rec.Left,rec.Top,rec.Right,rec.Bottom);
        SelectClipRgn(Canvas.handle,newrgn);
        DrawBitmapMosaicSlow(Canvas,rec,bitmap);
        if aresult=0 then
         SelectClipRgn(Canvas.handle,0)
        else
         SelectClipRgn(Canvas.handle,oldrgn);
       end;
     end;
    finally
     bitmap.Free;
    end;
   end;
 end;
end;

procedure TRpGDIDriver.DrawPage(apage:TRpMetaFilePage);
var
 j:integer;
 rec:TRect;
begin
 if toprinter then
 begin
  for j:=0 to apage.ObjectCount-1 do
  begin
   DrawObject(apage,apage.Objects[j]);
  end;
 end
 else
 begin
  if assigned(metacanvas) then
  begin
   metacanvas.free;
   metacanvas:=nil;
  end;
  if assigned(meta) then
  begin
   meta.free;
   meta:=nil;
  end;
  meta:=TMetafile.Create;
  try
   meta.Enhanced:=true;
   meta.Width:=bitmapwidth;
   meta.Height:=bitmapheight;
   metacanvas:=TMetafileCanvas.Create(meta,0);
   try
    for j:=0 to apage.ObjectCount-1 do
    begin
     DrawObject(apage,apage.Objects[j]);
    end;
   finally
    metacanvas.free;
    metacanvas:=nil;
   end;
   // Draws the metafile scaled
   if Round(scale*1000)=1000 then
   begin
    Bitmap.Canvas.Draw(0,0,Meta);
   end
   else
   begin
    rec.Top:=0;
    rec.Left:=0;
    rec.Right:=bitmap.Width-1;
    rec.Bottom:=bitmap.Height-1;
    Bitmap.Canvas.StretchDraw(rec,Meta);
   end;
  finally
   meta.free;
   meta:=nil;
  end;
 end;
end;

procedure TRpGDIDriver.DrawObject(page:TRpMetaFilePage;obj:TRpMetaObject);
var
 dpix,dpiy:integer;
 Canvas:TCanvas;
begin
 if (toprinter) then
 begin
  if not printer.Printing then
   Raise Exception.Create(SRpGDIDriverNotInit);
  dpix:=intdpix;
  dpiy:=intdpiy;
  Canvas:=printer.canvas;
 end
 else
 begin
  if not Assigned(bitmap) then
   Raise Exception.Create(SRpGDIDriverNotInit);
  if not Assigned(metacanvas) then
   Raise Exception.Create(SRpGDIDriverNotInit);
  Canvas:=metacanvas;
  dpix:=Screen.PixelsPerInch;
  dpiy:=Screen.PixelsPerInch;
 end;
 PrintObject(Canvas,page,obj,dpix,dpiy,toprinter,pagemargins,devicefonts,offset);
end;

function TRpGDIDriver.AllowCopies:boolean;
begin
 Result:=false;
end;

function TrpGDIDriver.GetPageSize:TPoint;
begin
 Result:=GetPhysicPageSizeTwips;
end;

function TRpGDIDriver.SetPagesize(PagesizeQt:TPageSizeQt):TPoint;stdcall;
begin
 pagesize:=QtPageSizeToGDIPageSize(PagesizeQT);
 oldpagesize:=GetCurrentPaper;
 SetCurrentPaper(pagesize);

 Result:=GetPageSize;
end;

procedure TRpGDIDriver.SetOrientation(Orientation:TRpOrientation);
begin
 if Orientation=rpOrientationPortrait then
 begin
  if Printer.Orientation<>poPortrait then
  begin
   orientationset:=true;
   oldorientation:=Printer.Orientation;
   Printer.Orientation:=poPortrait;
  end;
 end
 else
 if Orientation=rpOrientationLandscape then
 begin
  if Printer.Orientation<>poLandscape then
  begin
   orientationset:=true;
   oldorientation:=Printer.Orientation;
   Printer.Orientation:=poLandsCape;
  end;
 end;
end;

procedure DoPrintMetafile(metafile:TRpMetafileReport;tittle:string;
 aform:TFRpVCLProgress;allpages:boolean;frompage,topage,copies:integer;
 collate:boolean;devicefonts:boolean;printerindex:TRpPrinterSelect=pRpDefaultPrinter);
var
 i:integer;
 j:integer;
 apage:TRpMetafilePage;
 pagecopies:integer;
 reportcopies:integer;
 dpix,dpiy:integer;
 count1,count2:integer;
 mmfirst,mmlast:DWORD;
 difmilis:int64;
 totalcount:integer;
 pagemargins:TRect;
 offset:TPoint;
begin
 if printerindex<>pRpDefaultPrinter then
  offset:=PrinterSelection(printerindex)
 else
 begin
  if metafile.PrinterSelect<>pRpDefaultPrinter then
   offset:=PrinterSelection(metafile.PrinterSelect);
 end;
 UpdatePrinterFontList;
 pagemargins:=GetPageMarginsTWIPS;
 // Get the time
 mmfirst:=TimeGetTime;
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
  dpix:=GetDeviceCaps(Printer.Canvas.handle,LOGPIXELSX);
  dpiy:=GetDeviceCaps(Printer.Canvas.handle,LOGPIXELSY);
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
      PrintObject(Printer.Canvas,apage,apage.Objects[j],dpix,dpiy,true,pagemargins,devicefonts,offset);
      if assigned(aform) then
      begin
       mmlast:=TimeGetTime;
       difmilis:=(mmlast-mmfirst);
       if difmilis>MILIS_PROGRESS then
       begin
        // Get the time
        mmfirst:=TimeGetTime;
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
  collate:boolean;devicefonts:boolean;printerindex:TRpPrinterSelect=pRpDefaultPrinter):boolean;
var
 dia:TFRpVCLProgress;
begin
 Result:=true;
 if Not ShowProgress then
 begin
  DoPrintMetafile(metafile,tittle,nil,allpages,frompage,topage,copies,collate,devicefonts,printerindex);
  exit;
 end;
 dia:=TFRpVCLProgress.Create(Application);
 try
  dia.oldonidle:=Application.OnIdle;
  try
   dia.metafile:=metafile;
   dia.tittle:=tittle;
   dia.allpages:=allpages;
   dia.frompage:=frompage;
   dia.topage:=topage;
   dia.copies:=copies;
   dia.collate:=collate;
   dia.devicefonts:=devicefonts;
   dia.printerindex:=printerindex;
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


procedure TFRpVCLProgress.FormCreate(Sender: TObject);
begin
 LRecordCount.Font.Style:=[fsBold];
 LTittle.Font.Style:=[fsBold];

 BOK.Caption:=TranslateStr(93,BOK.Caption);
 BCancel.Caption:=TranslateStr(94,BCancel.Caption);
 LTitle.Caption:=TranslateStr(252,LTitle.Caption);
 LProcessing.Caption:=TranslateStr(253,LProcessing.Caption);
 GPrintRange.Caption:=TranslateStr(254,GPrintRange.Caption);
 LFrom.Caption:=TranslateStr(255,LFrom.Caption);
 LTo.Caption:=TranslateStr(256,LTo.Caption);
 RadioAll.Caption:=TranslateStr(257,RadioAll.Caption);
 RadioRange.Caption:=TranslateStr(258,RadioRange.Caption);
 Caption:=TranslateStr(259,Caption);

end;

procedure TFRpVCLProgress.AppIdle(Sender:TObject;var done:boolean);
begin
 cancelled:=false;
 Application.OnIdle:=nil;
 done:=false;
 LTittle.Caption:=tittle;
 LProcessing.Visible:=true;
 DoPrintMetafile(metafile,tittle,self,allpages,frompage,topage,copies,collate,devicefonts,printerindex);
end;


procedure TFRpVCLProgress.BCancelClick(Sender: TObject);
begin
 cancelled:=true;
end;


procedure TFRpVCLProgress.RepProgress(Sender:TRpReport;var docancel:boolean);
begin
 if Not Assigned(LRecordCount) then
  exit;
 LRecordCount.Caption:=IntToStr(Sender.CurrentSubReportIndex)+':'+SRpPage+':'+
 FormatFloat('#########,####',Sender.PageNum)+'-'+FormatFloat('#########,####',Sender.RecordCount);
 Application.ProcessMessages;
 if cancelled then
  docancel:=true;
end;

procedure TFRpVCLProgress.AppIdleReport(Sender:TObject;var done:boolean);
var
 oldprogres:TRpProgressEvent;
begin
 Application.Onidle:=nil;
 done:=false;

 try
  GDIDriver:=TRpGDIDriver.Create;
  aGDIDriver:=GDIDriver;
  if report.PrinterFonts=rppfontsalways then
   gdidriver.devicefonts:=true
  else
   gdidriver.devicefonts:=false;
  gdidriver.neverdevicefonts:=report.PrinterFonts=rppfontsnever;

  oldprogres:=RepProgress;
  try
   report.OnProgress:=RepProgress;
   report.PrintAll(GDIDriver);
  finally
   report.OnProgress:=oldprogres;
  end;
  Close;
 except
  cancelled:=True;
  Close;
  Raise;
 end;
end;

function CalcReportWidthProgress(report:TRpReport):boolean;
var
 dia:TFRpVCLProgress;
begin
 dia:=TFRpVCLProgress.Create(Application);
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


procedure TFRpVCLProgress.AppIdlePrintRange(Sender:TObject;var done:boolean);
var
 oldprogres:TRpProgressEvent;
begin
 Application.Onidle:=nil;
 done:=false;

 GDIDriver:=TRpGDIDriver.Create;
 GDIDriver.toprinter:=true;
 aGDIDriver:=GDIDriver;
 if report.PrinterFonts=rppfontsalways then
  gdidriver.devicefonts:=true
 else
  gdidriver.devicefonts:=false;
 gdidriver.neverdevicefonts:=report.PrinterFonts=rppfontsnever;
 oldprogres:=RepProgress;
 try
  report.OnProgress:=RepProgress;
  report.PrintRange(aGDIDriver,allpages,frompage,topage,copies,collate);
 finally
  report.OnProgress:=oldprogres;
 end;
 Close;
end;

procedure TFRpVCLProgress.AppIdlePrintPDF(Sender:TObject;var done:boolean);
var
 oldprogres:TRpProgressEvent;
begin
 Application.Onidle:=nil;
 done:=false;

 try
  pdfdriver:=TRpPDFDriver.Create;
  pdfdriver.filename:=filename;
  pdfdriver.compressed:=pdfcompressed;
  apdfdriver:=pdfdriver;
  oldprogres:=RepProgress;
  try
   report.OnProgress:=RepProgress;
   report.PrintRange(apdfdriver,allpages,frompage,topage,copies,collate);
  finally
   report.OnProgress:=oldprogres;
  end;
 finally
  Close;
 end;
end;



function PrintReport(report:TRpReport;Caption:string;progress:boolean;
  allpages:boolean;frompage,topage,copies:integer;collate:boolean):Boolean;
var
 GDIDriver:TRpGDIDriver;
 aGDIDriver:IRpPrintDriver;
 forcecalculation:boolean;
 dia:TFRpVCLProgress;
 oldonidle:TIdleEvent;
 devicefonts:boolean;
begin
 if report.PrinterFonts=rppfontsalways then
  devicefonts:=true
 else
  devicefonts:=false;
 Result:=true;
 forcecalculation:=false;
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
   GDIDriver:=TRpGDIDriver.Create;
   aGDIDriver:=GDIDriver;
   if report.PrinterFonts=rppfontsalways then
    gdidriver.devicefonts:=true
   else
    gdidriver.devicefonts:=false;
   gdidriver.neverdevicefonts:=report.PrinterFonts=rppfontsnever;
   report.PrintAll(GDIDriver);
  end;
 end;
 if forcecalculation then
  PrintMetafile(report.Metafile,Caption,progress,allpages,frompage,topage,copies,collate,devicefonts)
 else
 begin
  if progress then
  begin
   // Assign appidle frompage to page...
   dia:=TFRpVCLProgress.Create(Application);
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
   GDIDriver:=TRpGDIDriver.Create;
   aGDIDriver:=GDIDriver;
   GDIDriver.toprinter:=true;
   if report.PrinterFonts=rppfontsalways then
    gdidriver.devicefonts:=true
   else
    gdidriver.devicefonts:=false;
   gdidriver.neverdevicefonts:=report.PrinterFonts=rppfontsnever;
   report.PrintRange(aGDIDriver,allpages,frompage,topage,copies,collate);
  end;
 end;
end;


procedure TFRpVCLProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 GDIDriver:=nil;
end;


function ExportReportToPDF(report:TRpReport;Caption:string;progress:boolean;
  allpages:boolean;frompage,topage:integer;
  showprintdialog:boolean;filename:string;compressed:boolean;collate:Boolean):Boolean;
var
 copies:integer;
 dia:TFRpVCLProgress;
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
  if Not DoShowPrintDialog(allpages,frompage,topage,copies,collate,true) then
   exit;
 end;
 if progress then
 begin
  // Assign appidle frompage to page...
  dia:=TFRpVCLProgress.Create(Application);
  try
   dia.allpages:=allpages;
   dia.frompage:=frompage;
   dia.topage:=topage;
   dia.copies:=copies;
   dia.report:=report;
   dia.filename:=filename;
   dia.pdfcompressed:=compressed;
   dia.collate:=collate;
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
  report.PrintRange(apdfdriver,allpages,frompage,topage,copies,collate);
  Result:=True;
 end;
end;

procedure TFRpVCLProgress.BOKClick(Sender: TObject);
begin
 FromPage:=StrToInt(EFrom.Text);
 ToPage:=StrToInt(ETo.Text);
 if FromPage<1 then
  FromPage:=1;
 if ToPage<FromPage then
  ToPage:=FromPage;
 Close;
 dook:=true;
end;

procedure TFRpVCLProgress.FormShow(Sender: TObject);
begin
 if BOK.Visible then
 begin
  EFrom.Text:=IntToStr(FromPage);
  ETo.Text:=IntToStr(ToPage);
 end;
end;

procedure TRpGDIDriver.GraphicExtent(Stream:TMemoryStream;var extent:TPoint;dpi:integer);
var
 graphic:TBitmap;
 jpegimage:TJpegImage;
 bitmapwidth,bitmapheight:integer;
begin
 if dpi<=0 then
  exit;
 graphic:=TBitmap.Create;
 try
  if GetJPegInfo(Stream,bitmapwidth,bitmapheight) then
  begin
   jpegimage:=TJpegImage.Create;
   try
    jpegimage.LoadFromStream(Stream);
    graphic.Assign(jpegimage);
   finally
    jpegimage.free;
   end;
  end
  else
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
 index:integer;
 offset:TPoint;
begin
 printername:=GetPrinterConfigName(printerindex);
 offset:=GetPrinterOffset(printerindex);
 if length(printername)>0 then
 begin
  index:=Printer.Printers.IndexOf(printername);
  if index>=0 then
   Printer.PrinterIndex:=index;
 end;
 Result:=offset;
end;

procedure TRpGDIDriver.SelectPrinter(printerindex:TRpPrinterSelect);
begin
 offset:=PrinterSelection(printerindex);
 selectedprinter:=printerindex;
 if neverdevicefonts then
  exit;
 if devicefonts then
  exit;
 devicefonts:=GetDeviceFontsOption(printerindex);
 if devicefonts then
  UpdatePrinterFontList;
end;

procedure TRpGDIDriver.SendAfterPrintOperations;
var
 Operation:String;
 i:TPrinterRawOp;
begin
 for i:=Low(TPrinterRawOp) to High(TPrinterRawOp) do
 begin
  Operation:=GetPrinterRawOp(selectedprinter,i);
  if Length(Operation)>0 then
   SendControlCodeToPrinter(Operation);
 end;
end;

procedure PageSizeSelection(rpPageSize:TPageSizeQt);
var
 pagesize:TGDIPageSize;
begin
 if Printer.Printers.Count<1 then
  exit;
 pagesize:=QtPageSizeToGDIPageSize(rppagesize);
 SetCurrentPaper(pagesize);
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

