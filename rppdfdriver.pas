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
{$IFDEF VARIANTS}
 Types,
{$ENDIF}
{$IFDEF MSWINDOWS}
 Windows,
{$ENDIF}
 rptypes,rpmetafile,rppdffile,
 rpmunits;

type
 TRpPdfDriver=class(TInterfacedObject,IRpPrintDriver)
  private
   FPDFFIle:TRpPDFFile;
   FOrientation:TRpOrientation;
   FPageWidth,FPageHeight:integer;
  public
   filename:string;
   Compressed:boolean;
   constructor Create;
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
   property PDFFile:TRpPDFFile read FPDFFile;
  end;


procedure SaveMetafileToPDF(metafile:TRpMetafileReport;
 filename:string;compressed:boolean);

implementation

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

constructor TRpPDFDriver.Create;
begin
 FPageWidth:= 12048;
 FPageHeight:= 17039;
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
 bitmap:TBitmap;
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
    FPDFFile.Canvas.Font.Size:=obj.FontSize;
    FPDFFile.Canvas.Font.Color:=obj.FontColor;
    aalign:=obj.Alignment;
    if Not obj.Wordwrap then
    begin
     rec.Left:=posx;
     rec.TOp:=posy;
     rec.Right:=posx+round(obj.Width);
     rec.Bottom:=posy+round(obj.Height);
     FPDFFile.Canvas.TextRect(rec,page.GetText(Obj),aalign,obj.cuttext);
    end
    else
     FPDFFile.Canvas.TextOut(obj.Left,obj.Top,page.GetText(obj));
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
//     Canvas.CopyMode:=TCopyMode(obj.CopyMode);
{    bitmap:=TBitmap.Create;
    try
     bitmap.LoadFromStream(stream);
     case TRpImageDrawStyle(obj.DrawImageStyle) of
      rpDrawFull:
       begin
        rec.Bottom:=rec.Top+round(bitmap.height/obj.dpires)*TWIPS_PER_INCHESS-1;
        rec.Right:=rec.Left+round(bitmap.width/obj.dpires)*TWIPS_PER_INCHESS-1;
        FPDFFile.Canvas.StretchDraw(rec,bitmap);
       end;
      rpDrawStretch:
       begin
        FPDFFile.Canvas.StretchDraw(rec,bitmap);
       end;
      rpDrawCrop:
       begin
  //      Crop Should cut graphic but it don't work
  //        recsrc.Top:=0;
  //        recsrc.Left:=0;
  //        recsrc.Bottom:=Height-1;
  //        recsrc.Right:=Width-1;
  //        Canvas.CopyRect(rec,bitmap.canvas,recsrc);
        //Canvas.Draw(PosX,PosY,bitmap);
       end;
      rpDrawTile:
       begin
//        Canvas.TiledDraw(rec,bitmap);
       end;
     end;
    finally
     bitmap.Free;
    end;
}   end;
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

function TRpPDFDriver.SetPagesize(PagesizeQt:integer):TPoint;
begin
 // Sets the page size for the pdf file
 if FOrientation=rpOrientationLandscape then
 begin
  FPageWidth:=Round(PageSizeArray[PagesizeQt].Height/1000*TWIPS_PER_INCHESS);
  FPageheight:=Round(PageSizeArray[PagesizeQt].Width/1000*TWIPS_PER_INCHESS);
 end
 else
 begin
  FPageWidth:=Round(PageSizeArray[PagesizeQt].Width/1000*TWIPS_PER_INCHESS);
  FPageheight:=Round(PageSizeArray[PagesizeQt].Height/1000*TWIPS_PER_INCHESS);
 end;
 Result.X:=FPageWidth;
 Result.Y:=FPageHeight;
end;

procedure TRpPDFDriver.SetOrientation(Orientation:TRpOrientation);
begin
 FOrientation:=Orientation;
end;




end.
