{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rppdfdriver                                     }
{       TRpPDFDriver: Printer driver for                }
{       generating pdf files                            }
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

unit rppdfdriver;

interface

{$I rpconf.inc}

uses Classes,Sysutils,
{$IFDEF USEVARIANTS}
 Types,
{$ENDIF}
{$IFDEF MSWINDOWS}
 Windows,
{$ENDIF}
 rptypes,rpmetafile,rppdffile,
 rpmunits,rpreport,rpmdconsts;

type
 TRpPdfDriver=class(TInterfacedObject,IRpPrintDriver)
  private
   FPDFFIle:TRpPDFFile;
   FOrientation:TRpOrientation;
   FPageWidth,FPageHeight:integer;
   procedure RepProgress(Sender:TRpReport;var docancel:boolean);
  public
   filename:string;
   Compressed:boolean;
   DestStream:TStream;
   constructor Create;
   destructor Destroy;override;
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
   property PDFFile:TRpPDFFile read FPDFFile;
  end;


procedure SaveMetafileToPDF(metafile:TRpMetafileReport;
 filename:string;compressed:boolean);
procedure SaveMetafileToPDFStream(metafile:TRpMetafileReport;
 Stream:TStream;compressed:boolean);
function PrintReportPDF(report:TRpReport;Caption:string;progress:boolean;
     allpages:boolean;frompage,topage,copies:integer;
     filename:string;compressed:boolean):Boolean;
function PrintReportPDFStream(report:TRpReport;Caption:string;progress:boolean;
     allpages:boolean;frompage,topage,copies:integer;
     Stream:TStream;compressed:boolean):Boolean;



implementation

const
 AlignmentFlags_SingleLine=64;

type
  TPageWidthHeight = record
    Width: Integer;
    Height: Integer;
  end;

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



constructor TRpPDFDriver.Create;
begin
 FPageWidth:= 12048;
 FPageHeight:= 17039;
 FPDFFile:=TRpPDFFile.Create(nil);
end;

destructor TRpPDFDriver.Destroy;
begin
 if Assigned(FPDFFile) then
 begin
  FPDFFile.free;
  FPDFFile:=nil;
 end;
end;


procedure TRpPDFDriver.NewDocument(report:TrpMetafileReport);
begin
 if Assigned(FPDFFile) then
 begin
  FPDFFile.Free;
  FPDFFile:=nil;
 end;
 FPDFFile:=TRpPDFFile.Create(nil);
 FPDFFile.FileName:=filename;
 if Assigned(DestStream) then
  FPDFFile.DestStream:=DestStream;
 FPDFFile.Compressed:=Compressed;
 FPDFFile.PageWidth:=report.CustomX;
 FPDFFile.PageHeight:=report.CustomY;
 FPDFFile.BeginDoc;
end;

procedure TRpPDFDriver.EndDocument;
begin
 FPDFFile.EndDoc;
 FPDFFile.Free;
 FPDFFile:=nil;
end;

procedure TRpPDFDriver.AbortDocument;
begin
 FPDFFile.Free;
 FPDFFile:=nil;
end;

procedure TRpPDFDriver.NewPage;
begin
 FPDFFile.NewPage;
end;


procedure TRpPDFDriver.TextExtent(atext:TRpTextObject;var extent:TPoint);
var
 singleline:boolean;
 rect:TRect;
begin
 if atext.FontRotation<>0 then
  exit;
 if atext.CutText then
  exit;
 // single line
 singleline:=(atext.Alignment AND AlignmentFlags_SingleLine)>0;
 FPDFFile.Canvas.Font.Name:=TRpType1Font(atext.Type1Font);
 FPDFFile.Canvas.Font.Size:=atext.FontSize;
 FPDFFile.Canvas.Font.Bold:=(atext.Fontstyle and 1)>0;
 FPDFFile.Canvas.Font.Italic:=(atext.Fontstyle and (1 shl 1))>0;
 Rect.Left:=0;
 Rect.Top:=0;
 Rect.Bottom:=0;
 Rect.Right:=extent.X;
 FPDFFile.Canvas.TextExtent(atext.Text,Rect,atext.WordWrap,singleline);
 extent.X:=Rect.Right;
 extent.Y:=Rect.Bottom;
end;

procedure TRpPDFDriver.EndPage;
begin

end;

procedure TRpPDFDriver.DrawObject(page:TRpMetaFilePage;obj:TRpMetaObject);
var
 X, Y, W, H, S: Integer;
 Width,Height,posx,posy:integer;
 rec:TRect;
 aalign:integer;
 stream:TStream;
// bitmap:TBitmap;
begin
 posx:=obj.Left;
 posy:=obj.Top;
 case obj.Metatype of
  rpMetaText:
   begin
{$IFDEF MSWINDOWS}
//    Canvas.Font.Name:=page.GetWFontName(Obj);
{$ENDIF}
{$IFDEF LINUX}
//    Canvas.Font.Name:=page.GetLFontName(Obj);
{$ENDIF}
//    Canvas.Font.Style:=IntegerToFontStyle(obj.FontStyle);
    // Transparent ?
    FPDFFile.Canvas.Font.Name:=TrpType1Font(obj.Type1Font);
    FPDFFile.Canvas.Font.Size:=obj.FontSize;
    FPDFFile.Canvas.Font.Color:=obj.FontColor;
    FPDFFile.Canvas.Font.Bold:=(obj.Fontstyle and 1)>0;
    FPDFFile.Canvas.Font.Italic:=(obj.Fontstyle and (1 shl 1))>0;
    FPDFFile.Canvas.Font.UnderLine:=(obj.Fontstyle  and (1 shl 2))>0;
    FPDFFile.Canvas.Font.StrikeOut:=(obj.Fontstyle and (1 shl 3))>0;
    aalign:=obj.Alignment;
    rec.Left:=posx;
    rec.Top:=posy;
    rec.Right:=posx+round(obj.Width);
    rec.Bottom:=posy+round(obj.Height);
    FPDFFile.Canvas.TextRect(rec,page.GetText(Obj),aalign,obj.cuttext,
    obj.WordWrap,obj.FontRotation);
   end;
  rpMetaDraw:
   begin
    Width:=obj.Width;
    Height:=obj.Height;
    FPDFFile.Canvas.BrushStyle:=obj.BrushStyle;
    FPDFFile.Canvas.PenStyle:=obj.PenStyle;
    FPDFFile.Canvas.PenColor:=obj.Pencolor;
    FPDFFile.Canvas.BrushColor:=obj.BrushColor;
    FPDFFile.Canvas.PenWidth:=obj.PenWidth;
    X := FPDFFile.Canvas.PenWidth div 2;
    Y := X;
    W := Width - FPDFFile.Canvas.PenWidth + 1;
    H := Height - FPDFFile.Canvas.PenWidth + 1;
    if FPDFFile.Canvas.PenWidth = 0 then
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
      begin
       FPDFFile.Canvas.Rectangle(X+PosX, Y+PosY, X+PosX + W, Y +PosY+ H);
      end;
     rpsRoundRect, rpsRoundSquare:
      begin
//      Canvas.RoundRect(X, Y, X + W, Y + H, S div 4, S div 4);
       FPDFFile.Canvas.Rectangle(X+PosX, Y+PosY, X+PosX + W, Y +PosY+ H);
      end;
     rpsCircle, rpsEllipse:
      begin
       FPDFFile.Canvas.Ellipse(X+PosX, Y+PosY, X+PosX + W, Y+PosY + H);
      end;
     rpsHorzLine:
      begin
       FPDFFile.Canvas.Line(X+PosX, Y+PosY,X+PosX+W, Y+PosY);
       if obj.PenStyle in [3,4] then
       begin
        FPDFFile.Canvas.PenStyle:=6;
        FPDFFile.Canvas.Line(X+PosX, Y+PosY,X+PosX, Y+PosY+H);
       end;
      end;
     rpsVertLine:
      begin
       FPDFFile.Canvas.Line(X+PosX, Y+PosY,X+PosX, Y+PosY+H);
      end;
     rpsOblique1:
      begin
       FPDFFile.Canvas.Line(X+PosX, Y+PosY,X+PosX+W, Y+PosY+H);
      end;
     rpsOblique2:
      begin
       FPDFFile.Canvas.Line(X+PosX, Y+PosY+H,X+PosX+W, Y+PosY);
      end;
    end;
   end;
  rpMetaImage:
   begin
    Width:=round(obj.Width);
    Height:=round(obj.Height);
    rec.Top:=PosY;
    rec.Left:=PosX;
    rec.Bottom:=rec.Top+Height-1;
    rec.Right:=rec.Left+Width-1;
    stream:=page.GetStream(obj);
    case TRpImageDrawStyle(obj.DrawImageStyle) of
     rpDrawFull:
      begin
       FPDFFile.Canvas.DrawImage(rec,stream,obj.dpires,false,false);
      end;
     rpDrawStretch:
      begin
       FPDFFile.Canvas.DrawImage(rec,stream,0,false,false);
      end;
     rpDrawCrop:
       begin
        FPDFFile.Canvas.DrawImage(rec,stream,CONS_PDFRES,false,true);
       end;
     rpDrawTile:
      begin
       FPDFFile.Canvas.DrawImage(rec,stream,CONS_PDFRES,true,true);
      end;
    end;
   end;
 end;
end;

procedure TRpPDFDriver.DrawPage(apage:TRpMetaFilePage);
var
 j:integer;
begin
 for j:=0 to apage.ObjectCount-1 do
 begin
  DrawObject(apage,apage.Objects[j]);
 end;
end;

function TRpPDFDriver.AllowCopies:boolean;
begin
 Result:=false;
end;

function TRpPDFDriver.GetPageSize:TPoint;
begin
 Result.X:=FPageWidth;
 Result.Y:=FPageHeight;
end;

function TRpPDFDriver.SetPagesize(PagesizeQt:TPageSizeQt):TPoint;stdcall;
var
 newwidth,newheight:integer;
begin
 // Sets the page size for the pdf file, first if it's a qt page
 if PagesizeQt.Custom then
 begin
  newwidth:=PagesizeQt.CustomWidth;
  newheight:=PagesizeQt.CustomHeight;
 end
 else
 begin
  newWidth:=Round(PageSizeArray[PagesizeQt.Indexqt].Width/1000*TWIPS_PER_INCHESS);
  newheight:=Round(PageSizeArray[PagesizeQt.Indexqt].Height/1000*TWIPS_PER_INCHESS);
 end;
 if FOrientation=rpOrientationLandscape then
 begin
  FPageWidth:=NewHeight;
  FPageHeight:=NewWidth;
 end
 else
 begin
  FPageWidth:=NewWidth;
  FPageHeight:=NewHeight;
 end;
 Result.X:=FPageWidth;
 Result.Y:=FPageHeight;
end;

procedure TRpPDFDriver.SetOrientation(Orientation:TRpOrientation);
begin
 FOrientation:=Orientation;
end;


procedure TRpPDFDriver.RepProgress(Sender:TRpReport;var docancel:boolean);
begin
{$IFDEF USEVARIANTS}
 WriteLn(SRpRecordCount+' '+IntToStr(Sender.CurrentSubReportIndex)
  +':'+SRpPage+':'+FormatFloat('#########,####',Sender.PageNum)+'-'+
  FormatFloat('#########,####',Sender.RecordCount));
{$ELSE}
 WriteLn(String(SRpRecordCount+' '+IntToStr(Sender.CurrentSubReportIndex)
  +':'+SRpPage+':'+FormatFloat('#########,####',Sender.PageNum)+'-'+
  FormatFloat('#########,####',Sender.RecordCount)));
{$ENDIF}

end;


function PrintReportPDF(report:TRpReport;Caption:string;progress:boolean;
     allpages:boolean;frompage,topage,copies:integer;
     filename:string;compressed:boolean):Boolean;
var
 pdfdriver:TRpPDFDriver;
 apdfdriver:IRpPrintDriver;
 oldprogres:TRpProgressEvent;
begin
 if Length(Trim(filename))<0 then
  Raise Exception.Create(SRpNoFileNameProvided+':PDF');
 pdfdriver:=TRpPDFDriver.Create;
 pdfdriver.filename:=filename;
 pdfdriver.compressed:=compressed;
 apdfdriver:=pdfdriver;
 // If report progress must print progress
 oldprogres:=report.OnProgress;
 try
  if progress then
   report.OnProgress:=pdfdriver.RepProgress;
  report.PrintRange(apdfdriver,allpages,frompage,topage,copies);
 finally
  report.OnProgress:=oldprogres;
 end;
 Result:=True;
end;

procedure TRpPDFDriver.SelectPrinter(printerindex:TRpPrinterSelect);stdcall;
begin
 // No printer to select
end;

procedure TRpPDFDriver.GraphicExtent(Stream:TMemoryStream;var extent:TPoint;dpi:integer);
var
 imagesize:integer;
 width,height:integer;
begin
 width:=0;
 height:=0;
 GetBitmapInfo(Stream,width,height,imagesize,nil);
 if dpi<=0 then
  exit;
 extent.X:=Round(width/dpi*TWIPS_PER_INCHESS);
 extent.Y:=Round(height/dpi*TWIPS_PER_INCHESS);
end;


procedure SaveMetafileToPDF(metafile:TRpMetafileReport;
 filename:string;compressed:boolean);
var
 adriver:TRpPDFDriver;
 i:integer;
begin
 adriver:=TRpPDFDriver.Create;
 adriver.filename:=filename;
 adriver.compressed:=compressed;
 adriver.NewDocument(metafile);
 try
  for i:=0 to metafile.PageCount-1 do
  begin
   adriver.DrawPage(metafile.Pages[i]);
   if i<metafile.PageCount-1 then
    adriver.NewPage;
  end;
  adriver.EndDocument;
 except
  adriver.AbortDocument;
  raise;
 end;
end;

procedure SaveMetafileToPDFStream(metafile:TRpMetafileReport;
 Stream:TStream;compressed:boolean);
var
 adriver:TRpPDFDriver;
 i:integer;
begin
 adriver:=TRpPDFDriver.Create;
 adriver.compressed:=compressed;
 adriver.NewDocument(metafile);
 try
  for i:=0 to metafile.PageCount-1 do
  begin
   adriver.DrawPage(metafile.Pages[i]);
   if i<metafile.PageCount-1 then
    adriver.NewPage;
  end;
  adriver.EndDocument;
  adriver.PDFFile.MainPDF.Seek(0,soFromBeginning);
  Stream.Write(adriver.PDFFile.MainPDF.Memory^,adriver.PDFFile.MainPDF.Size);
 except
  adriver.AbortDocument;
  raise;
 end;
end;

function PrintReportPDFStream(report:TRpReport;Caption:string;progress:boolean;
     allpages:boolean;frompage,topage,copies:integer;
     Stream:TStream;compressed:boolean):Boolean;
var
 pdfdriver:TRpPDFDriver;
 apdfdriver:IRpPrintDriver;
 oldprogres:TRpProgressEvent;
begin
 pdfdriver:=TRpPDFDriver.Create;
 pdfdriver.compressed:=compressed;
 pdfdriver.DestStream:=Stream;
 apdfdriver:=pdfdriver;
 // If report progress must print progress
 oldprogres:=report.OnProgress;
 try
  if progress then
   report.OnProgress:=pdfdriver.RepProgress;
  report.PrintRange(apdfdriver,allpages,frompage,topage,copies);
  Stream.Seek(0,soFromBeginning);
 finally
  report.OnProgress:=oldprogres;
 end;
 Result:=True;
end;

end.
