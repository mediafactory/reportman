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
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
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
 QStdCtrls,QExtCtrls,types,DateUtils,rptypes,Qt,
 rpreport;


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
    qtdriver:TRpQtDriver;
    aqtdriver:IRpPrintDriver;
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
   function GetPageSize:TPoint;stdcall;
   function SetPagesize(PagesizeQt:integer):TPoint;stdcall;
   procedure SetOrientation(Orientation:TRpOrientation);stdcall;
   constructor Create;
  end;

function PrintMetafile(metafile:TRpMetafileReport;tittle:string;showprogress:boolean):boolean;
function CalcReportWidthProgress(report:TRpReport):boolean;
function PrintReport(report:TRpReport;Caption:string;progress:boolean):Boolean;

implementation

{$R *.xfm}

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
  if report.PageSize>=0 then
  begin
   asize:=GetPageSize;
  end
  else
  begin
   asize:=SetPageSize(report.PageSize);
  end;
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
// recsrc:TRect;
 X, Y, W, H, S: Integer;
 Width,Height:integer;
 stream:TMemoryStream;
 bitmap:TBitmap;
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
    if obj.CutText then
    begin
     rec.Top:=posx;
     rec.Left:=posy;
     rec.Right:=posx+round(obj.Width*dpix/TWIPS_PER_INCHESS);
     rec.Bottom:=posy+round(obj.Height*dpiy/TWIPS_PER_INCHESS);
     Canvas.TextRect(rec,posx,posy,page.GetText(Obj),obj.Alignment);
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
     Canvas.CopyMode:=TCopyMode(obj.CopyMode);

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
 Result.x:=Round((Printer.PageWidth/251)*TWIPS_PER_INCHESS);
 Result.y:=Round((Printer.PageHeight/251)*TWIPS_PER_INCHESS);
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
  Printer.Orientation:=poPortrait;
 end
 else
 if Orientation=rpOrientationLandscape then
  Printer.Orientation:=poLandsCape;
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
 oldprogres:=RepProgress;
 try
  report.OnProgress:=RepProgress;
  report.BeginPrint(qtdriver);
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

function PrintReport(report:TRpReport;Caption:string;progress:boolean):Boolean;
var
{$IFDEF LINUX}
 abuffer:array [0..L_tmpnam] of char;
 theparams:array [0..3] of pchar;
 param1:string;
 param2:string;
 param3:string;
 afilename:string;
 child:__pid_t;
{$ENDIF}
 qtdriver:TRpQtDriver;
 aqtdriver:IRpPrintDriver;
begin
 Result:=true;
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

 // A bug in Kylix 2 does not allow printing
 // when using dbexpress
{$IFDEF MSWINDOWS}
 PrintMetafile(report.Metafile,Caption,progress);
{$ENDIF}
{$IFDEF LINUX}
 // Saves the metafile
 tmpnam(abuffer);
 afilename:=StrPas(abuffer);
 report.Metafile.SaveToFile(afilename);
 param1:='metaprint';
 param2:='-d';
 param3:=afilename;
 theparams[0]:=Pchar(param1);
 theparams[1]:=Pchar(param2);
 theparams[2]:=PChar(afilename);
 theparams[3]:=nil;

 child:=fork;
 if child=-1 then
  Raise Exception.Create(SRpErrorFork);
 if child<>0 then
 begin
  execvp(theparams[0],PPChar(@theparams))
 end
{$ENDIF}
end;


procedure TFRpQtProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 qtdriver:=nil;
end;

end.

