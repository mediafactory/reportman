{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpgdidriver                                     }
{       TRpGDIDriver: Printer driver for  VCL Lib       }
{       can be used only for windows                   }
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
 Classes,sysutils,rpmetafile,rpconsts,Graphics,Forms,
 rpmunits,Printers,Dialogs, Controls,
 StdCtrls,ExtCtrls,
{$IFDEF USEVARIANTS}
 types,
{$ENDIF}
 rptypes,rpvgraphutils,
 rpreport,rppdfdriver;


const
 METAPRINTPROGRESS_INTERVAL=20;
type
  TRpGDIDriver=class;
  TFRpVCLProgress = class(TForm)
    CancelBtn: TButton;
    Label1: TLabel;
    LRecordCount: TLabel;
    Label2: TLabel;
    LTittle: TLabel;
    OKBtn: TButton;
    GroupBox1: TGroupBox;
    EFrom: TEdit;
    ETo: TEdit;
    Label5: TLabel;
    Label4: TLabel;
    RadioAll: TRadioButton;
    RadioRange: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    allpages,collate:boolean;
    frompage,topage,copies:integer;
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
function DoShowPrintDialog(var allpages:boolean;
 var frompage,topage,copies:integer;var collate:boolean;disablecopies:boolean=false):boolean;

implementation

const
 AlignmentFlags_SingleLine=64;
 AlignmentFlags_AlignHCenter = 4 { $4 };
 AlignmentFlags_AlignTop = 8 { $8 };
 AlignmentFlags_AlignBottom = 16 { $10 };
 AlignmentFlags_AlignVCenter = 32 { $20 };
 AlignmentFlags_AlignLeft = 1 { $1 };
 AlignmentFlags_AlignRight = 2 { $2 };

{$R *.dfm}


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
   diarange.OKBtn.Visible:=true;
   diarange.GroupBox1.Visible:=true;
   diarange.RadioAll.Checked:=allpages;
   diarange.RadioRange.Checked:=not allpages;
   diarange.ActiveControl:=diarange.OKBtn;
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
end;

procedure TRpGDIDriver.NewDocument(report:TrpMetafileReport);
var
 awidth,aheight:integer;
 rec:TRect;
 asize:TPoint;
begin
 if ToPrinter then
 begin
  printer.Title:='Untitled';
  SetOrientation(report.Orientation);
  // Sets pagesize
  if report.PageSize>=0 then
  begin
   asize:=GetPageSize;
  end
  else
  begin
   asize:=SetPageSize(report.PageSize);
  end;
  printer.BeginDoc;
  intdpix:=Round(GetDeviceCaps(Printer.Canvas.handle,LOGPIXELSX)*2.51); //  printer.XDPI;
  intdpiy:=Round(GetDeviceCaps(Printer.Canvas.handle,LOGPIXELSY)*2.51)  // printer.YDPI;
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
  if report.PageSize>=0 then
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
  Bitmap.Canvas.Brush.Color:=CLXColorToVCLColor(report.BackColor);
  rec.Top:=0;
  rec.Left:=0;
  rec.Right:=Bitmap.Width-1;
  rec.Bottom:=Bitmap.Height-1;
  bitmap.Canvas.FillRect(rec);
 end;
end;

procedure TRpGDIDriver.EndDocument;
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
begin
 // Does nothing
end;

procedure PrintObject(Canvas:TCanvas;page:TRpMetafilePage;obj:TRpMetaObject;dpix,dpiy:integer);
var
 posx,posy:integer;
 rec,recsrc:TRect;
// recsrc:TRect;
 X, Y, W, H, S: Integer;
 Width,Height:integer;
 stream:TMemoryStream;
 bitmap:TBitmap;
 aalign:Cardinal;
 abrushstyle:integer;
 atext:widestring;
 arec:TRect;
begin
 // Switch to device points
 posx:=round(obj.Left*dpix/TWIPS_PER_INCHESS);
 posy:=round(obj.Top*dpiy/TWIPS_PER_INCHESS);
 case obj.Metatype of
  rpMetaText:
   begin
    Canvas.Font.Name:=page.GetWFontName(Obj);
    Canvas.Font.Color:=CLXColorToVCLColor(Obj.FontColor);
    Canvas.Font.Style:=CLXIntegerToFontStyle(obj.FontStyle);
    Canvas.Font.Size:=Obj.FontSize;
    aalign:=DT_NOPREFIX;
    if (obj.AlignMent AND AlignmentFlags_AlignHCenter)>0 then
     aalign:=aalign or DT_CENTER;
    if (obj.AlignMent AND AlignmentFlags_SingleLine)>0 then
     aalign:=aalign or DT_SINGLELINE;
    if (obj.AlignMent AND AlignmentFlags_AlignVCenter)>0 then
    begin
     aalign:=aalign or DT_VCENTER;
     aalign:=aalign or DT_SINGLELINE;
    end;
    if (obj.AlignMent AND AlignmentFlags_AlignBottom)>0 then
    begin
     aalign:=aalign or DT_BOTTOM;
     aalign:=aalign or DT_SINGLELINE;
    end;
    if (obj.AlignMent AND AlignmentFlags_AlignTop)>0 then
    begin
     aalign:=aalign or DT_TOP;
     aalign:=aalign or DT_SINGLELINE;
    end;
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
    if Not obj.Transparent then
    begin
     // First calculates the text extent
     arec:=rec;
     DrawTextW(Canvas.Handle,PWideChar(atext),Length(atext),arec,aalign or DT_CALCRECT);
     Canvas.Brush.Style:=bsSolid;
     Canvas.Brush.Color:=obj.BackColor;
    end
    else
     Canvas.Brush.Style:=bsClear;
    DrawTextW(Canvas.Handle,PWideChar(atext),Length(atext),rec,aalign);
   end;
  rpMetaDraw:
   begin
    Width:=round(obj.Width*dpix/TWIPS_PER_INCHESS);
    Height:=round(obj.Height*dpiy/TWIPS_PER_INCHESS);
    abrushstyle:=obj.BrushStyle;
    if obj.BrushStyle>integer(bsDiagCross) then
     abrushstyle:=integer(bsDiagCross);
    Canvas.Brush.Style:=TBrushStyle(abrushstyle);
    Canvas.Pen.Style:=TPenStyle(obj.PenStyle);
    Canvas.Pen.Color:=CLXColorToVCLColor(obj.Pencolor);
    Canvas.Brush.Color:=CLXColorToVCLColor(obj.BrushColor);
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
//     Canvas.CopyMode:=CLXCopyModeToCopyMode(obj.CopyMode);

     case TRpImageDrawStyle(obj.DrawImageStyle) of
      rpDrawFull:
       begin
        rec.Bottom:=rec.Top+round(bitmap.height/obj.dpires)*dpiy-1;
        rec.Right:=rec.Left+round(bitmap.width/obj.dpires)*dpix-1;
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
        DrawBitmapMosaicSlow(Canvas,rec,bitmap);
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
begin
 for j:=0 to apage.ObjectCount-1 do
 begin
  DrawObject(apage,apage.Objects[j]);
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
  Canvas:=bitmap.canvas;
  dpix:=dpi;
  dpiy:=dpi;
 end;
 PrintObject(Canvas,page,obj,dpix,dpiy);
end;

function TRpGDIDriver.AllowCopies:boolean;
begin
 Result:=false;
end;

function TrpGDIDriver.GetPageSize:TPoint;
begin
 if Printer.Printing then
 begin
  Result.x:=Round((Printer.PageWidth/Round(GetDeviceCaps(Printer.Canvas.handle,LOGPIXELSX)*2.51))*TWIPS_PER_INCHESS);
  Result.y:=Round((Printer.PageHeight/Round(GetDeviceCaps(Printer.Canvas.handle,LOGPIXELSY)*2.51))*TWIPS_PER_INCHESS);
 end
 else
 begin
  Result.X:=11760;
  Result.Y:=17049;
 end;
end;

function TRpGDIDriver.SetPagesize(PagesizeQt:integer):TPoint;
begin
// Printer.PrintAdapter.PageSize:=TPageSize(PagesizeQT);
 Result:=GetPageSize;
end;

procedure TRpGDIDriver.SetOrientation(Orientation:TRpOrientation);
begin
 if Orientation=rpOrientationPortrait then
 begin
  Printer.Orientation:=poPortrait;
 end
 else
 if Orientation=rpOrientationLandscape then
  Printer.Orientation:=poLandsCape;
end;

procedure DoPrintMetafile(metafile:TRpMetafileReport;tittle:string;
 aform:TFRpVCLProgress;allpages:boolean;frompage,topage,copies:integer;
 collate:boolean);
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
begin
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
// if metafile.PageSize>=0 then
//  Printer.PrintAdapter.PageSize:=TPageSize(metafile.PageSize);

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
  dpix:=Round(GetDeviceCaps(Printer.Canvas.handle,LOGPIXELSX)*2.51);
  dpiy:=Round(GetDeviceCaps(Printer.Canvas.handle,LOGPIXELSY)*2.51);
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
  collate:boolean):boolean;
var
 dia:TFRpVCLProgress;
begin
 Result:=true;
 if Not ShowProgress then
 begin
  DoPrintMetafile(metafile,tittle,nil,allpages,frompage,topage,copies,collate);
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
   Application.OnIdle:=dia.AppIdle;
   dia.ShowModal;
   Result:=Not dia.cancelled;
  finally
   Application.OnIdle:=dia.oldonidle;
  end;
 finally

 end;
end;


procedure TFRpVCLProgress.FormCreate(Sender: TObject);
begin
 LRecordCount.Font.Style:=[fsBold];
 LTittle.Font.Style:=[fsBold];
end;

procedure TFRpVCLProgress.AppIdle(Sender:TObject;var done:boolean);
begin
 cancelled:=false;
 Application.OnIdle:=nil;
 done:=false;
 LTittle.Caption:=tittle;
 Label2.Visible:=true;
 DoPrintMetafile(metafile,tittle,self,allpages,frompage,topage,copies,collate);
end;


procedure TFRpVCLProgress.CancelBtnClick(Sender: TObject);
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

 GDIDriver:=TRpGDIDriver.Create;
 aGDIDriver:=GDIDriver;
 oldprogres:=RepProgress;
 try
  report.OnProgress:=RepProgress;
  report.PrintAll(GDIDriver);
 finally
  report.OnProgress:=oldprogres;
 end;
 Close;
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
 oldprogres:=RepProgress;
 try
  report.OnProgress:=RepProgress;
  report.PrintRange(aGDIDriver,allpages,frompage,topage,copies);
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
 GDIDriver:TRpGDIDriver;
 aGDIDriver:IRpPrintDriver;
 forcecalculation:boolean;
 dia:TFRpVCLProgress;
 oldonidle:TIdleEvent;
begin
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
   report.PrintAll(GDIDriver);
  end;
 end;
 // A bug in Kylix 2 does not allow printing
 // when using dbexpress
 if forcecalculation then
  PrintMetafile(report.Metafile,Caption,progress,allpages,frompage,topage,copies,collate)
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
   report.PrintRange(aGDIDriver,allpages,frompage,topage,copies);
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
  showprintdialog:boolean;filename:string;compressed:boolean):Boolean;
var
 copies:integer;
 collate:boolean;
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

procedure TFRpVCLProgress.OKBtnClick(Sender: TObject);
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
 if OKBtn.Visible then
 begin
  EFrom.Text:=IntToStr(FromPage);
  ETo.Text:=IntToStr(ToPage);
 end;
end;

end.

