{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpqtriver                                     }
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
 Classes,sysutils,rpmetafile,rpconsts,QGraphics,QForms,
 rpmunits,QPrinters,QDialogs,rpgraphutils, QControls,
 QStdCtrls,QExtCtrls,types,DateUtils,rptypes,Qt,
 rpreport,rppdfdriver;


const
 METAPRINTPROGRESS_INTERVAL=20;
type
  TRpQtDriver=class;
  TFRpQtProgress = class(TForm)
    CancelBtn: TButton;
    Label1: TLabel;
    LRecordCount: TLabel;
    Label2: TLabel;
    LTittle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    allpages,collate:boolean;
    frompage,topage,copies:integer;
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
   procedure DrawPage(apage:TRpMetaFilePage);stdcall;
   function AllowCopies:boolean;stdcall;
   function GetPageSize:TPoint;stdcall;
   function SetPagesize(PagesizeQt:integer):TPoint;stdcall;
   procedure SetOrientation(Orientation:TRpOrientation);stdcall;
   constructor Create;
  end;

function PrintMetafile(metafile:TRpMetafileReport;tittle:string;
 showprogress,allpages:boolean;frompage,topage,copies:integer;
  collate:boolean):boolean;
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

 var
{$IFDEF MSWINDOWS}
  kylixprintbug:boolean=false;
{$ENDIF}
{$IFDEF LINUX}
  kylixprintbug:boolean=true;
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
 if QPrinter_setup(QPrinterH(Printer.handle),nil) then
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
 dpi:=Screen.PixelsPerInch;
end;

procedure TRpQtDriver.NewDocument(report:TrpMetafileReport);
var
 awidth,aheight:integer;
 rec:TRect;
 asize:TPoint;
begin
 if ToPrinter then
 begin
  printer.Title:='Untitled';
  printer.SetPrinter(Printer.Printers.Strings[0]);
  SetOrientation(report.Orientation);
  // Sets pagesize
  if report.PageSize<0 then
  begin
   asize:=GetPageSize;
  end
  else
  begin
   asize:=SetPageSize(report.PageSize);
  end;
  if Length(printer.Title)<1 then
   printer.Title:=SRpUntitled;
  QPrinter_setFullPage(QPrinterH(Printer.Handle),true);
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
  bitmap:=TBitmap.Create;
  bitmap.PixelFormat:=pf32bit;
  // Sets Orientation
  SetOrientation(report.Orientation);
  // Sets pagesize
  if report.PageSize<0 then
  begin
   asize:=GetPageSize;
  end
  else
  begin
   asize:=SetPageSize(report.PageSize);
  end;
  awidth:=Round((asize.x/TWIPS_PER_INCHESS)*dpi);
  aheight:=Round((asize.y/TWIPS_PER_INCHESS)*dpi);
  // Sets page size and orientation
  bitmap.Width:=awidth;
  bitmap.Height:=aheight;

  Bitmap.Canvas.Brush.Style:=bsSolid;
  Bitmap.Canvas.Brush.Color:=report.BackColor;
  rec.Top:=0;
  rec.Left:=0;
  rec.Right:=Bitmap.Width-1;
  rec.Bottom:=Bitmap.Height-1;
  bitmap.Canvas.FillRect(rec);
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
 atext:Widestring;
// recsrc:TRect;
 X, Y, W, H, S: Integer;
 Width,Height:integer;
 stream:TMemoryStream;
 bitmap:TBitmap;
 aalign:Integer;
 arec:TRect;
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
    Canvas.Font.Size:=Obj.FontSize;
    aalign:=obj.Alignment;
    if Not obj.CutText then
     aalign:=aalign or Integer(AlignmentFlags_DontClip);
    if obj.Wordwrap then
     aalign:=aalign or Integer(AlignmentFlags_WordBreak);
    rec.Left:=posx;
    rec.Top:=posy;
    rec.Right:=posx+round(obj.Width*dpix/TWIPS_PER_INCHESS);
    rec.Bottom:=posy+round(obj.Height*dpiy/TWIPS_PER_INCHESS);
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
       QPainter_translate(Canvas.Handle,posx,posy);
       QPainter_rotate(Canvas.Handle,-obj.FontRotation/10);
       QPainter_drawText(Canvas.Handle,0,0,PWideString(@atext),Length(Atext));
      finally
       QPainter_restore(Canvas.Handle);
      end;
     finally
      Canvas.Stop;
     end;
    end
    else
     Canvas.TextRect(rec,posx,posy,atext,aalign);
   end;
  rpMetaDraw:
   begin
    Width:=round(obj.Width*dpix/TWIPS_PER_INCHESS);
    Height:=round(obj.Height*dpiy/TWIPS_PER_INCHESS);
    Canvas.Pen.Color:=obj.Pencolor;
    Canvas.Pen.Style:=TPenStyle(obj.PenStyle);
    Canvas.Brush.Color:=obj.BrushColor;
    Canvas.Brush.Style:=TBrushStyle(obj.BrushStyle);
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
     bitmap.LoadFromStream(stream);
//     Copy mode does not work for Stretchdraw
//     Canvas.CopyMode:=TCopyMode(obj.CopyMode);

     case TRpImageDrawStyle(obj.DrawImageStyle) of
      rpDrawFull:
       begin
        rec.Bottom:=rec.Top+round(bitmap.height/obj.dpires)*dpiy-1;
        rec.Right:=rec.Left+round(bitmap.width/obj.dpires)*dpix-1;
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
 PrintObject(Canvas,page,obj,dpix,dpiy);
end;

function TRpQtDriver.AllowCopies:boolean;
begin
 Result:=false;
end;


function TrpQtDriver.GetPageSize:TPoint;
begin
 Result.x:=Round((Printer.PageWidth/Printer.XDPI)*TWIPS_PER_INCHESS);
 Result.y:=Round((Printer.PageHeight/Printer.YDPI)*TWIPS_PER_INCHESS);
end;

function TRpQTDriver.SetPagesize(PagesizeQt:integer):TPoint;
begin
 Printer.PrintAdapter.PageSize:=TPageSize(PagesizeQT);
 Result:=GetPageSize;
end;

procedure TRpQTDriver.SetOrientation(Orientation:TRpOrientation);
begin
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
 collate:boolean);
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
begin
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
  collate:boolean):boolean;
var
 dia:TFRpQtProgress;
begin
 Result:=true;
 if Not ShowProgress then
 begin
  DoPrintMetafile(metafile,tittle,nil,allpages,frompage,topage,copies,collate);
  exit;
 end;
 dia:=TFRpQtProgress.Create(Application);
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
 DoPrintMetafile(metafile,tittle,self,allpages,frompage,topage,copies,collate);
end;


procedure TFRpQtProgress.CancelBtnClick(Sender: TObject);
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
 // when using dbexpress
 if not kylixprintbug then
 begin
   if forcecalculation then
    PrintMetafile(report.Metafile,Caption,progress,allpages,frompage,topage,copies,collate)
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
   tmpnam(abuffer);
   afilename:=StrPas(abuffer);
   report.Metafile.SaveToFile(afilename);
   params[0]:='metaprint';
   params[1]:='-d';
   params[2]:='-copies';
   params[3]:=IntToStr(copies);
   paramcount:=4;
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
    execvp(theparams[0],PPChar(@theparams))
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
  if Not DoShowPrintDialog(allpages,frompage,topage,copies,collate,true) then
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

end.

