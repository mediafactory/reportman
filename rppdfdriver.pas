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

uses Classes,Sysutils,Types,rptypes,rpmetafile,rppdffile,
 rpmunits;

type
 TRpPdfDriver=class(TInterfacedObject,IRpPrintDriver)
  private
   FPDFFIle:TRpPDFFile;
  public
   filename:string;
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


procedure SaveMetafileToPDF(metafile:TRpMetafileReport;filename:string);

implementation




procedure SaveMetafileToPDF(metafile:TRpMetafileReport;filename:string);
var
 adriver:TRpPDFDriver;
 i:integer;
 j:integer;
 apage:TRpMetafilePage;
begin
 adriver:=TRpPDFDriver.Create;
 adriver.filename:=filename;
 adriver.NewDocument(metafile);
 try
  for i:=0 to metafile.PageCount-1 do
  begin
   apage:=metafile.Pages[i];
   for j:=0 to apage.ObjectCount-1 do
   begin
    adriver.DrawObject(apage,apage.Objects[j]);
   end;
   if i<metafile.PageCount-1 then
    adriver.NewPage;
  end;
  adriver.EndDocument;
 except
  adriver.AbortDocument;
  raise;
 end;
end;


procedure TRpPDFDriver.NewDocument(report:TrpMetafileReport);
begin
 if Assigned(FPDFFile) then
 begin
  FPDFFile.Free;
 end;
 FPDFFile:=TRpPDFFile.Create(nil);
 FPDFFile.FileName:=filename;
 FPDFFile.Compressed:=false;
 FPDFFile.PageWidth:=report.CustomX;
 FPDFFile.PageHeight:=report.CustomY;
 FPDFFile.BeginDoc;
end;

procedure TRpPDFDriver.EndDocument;
begin
 FPDFFile.EndDoc;
end;

procedure TRpPDFDriver.AbortDocument;
begin

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
    if obj.PenStyle in [3,4] then
    begin
     if (Not (TRpShapeType(obj.DrawStyle) in [rpsVertLine,rpsHorzLine])) then
      FPDFFile.Canvas.PenStyle:=1;
    end;
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
       if obj.PenStyle in [3,4] then
       begin
        FPDFFile.Canvas.PenStyle:=6;
        FPDFFile.Canvas.Line(X+PosX, Y+PosY,X+PosX, Y+PosY+H);
       end;
      end;
    end;
   end;
 end;
end;

procedure TRpPDFDriver.DrawPage(apage:TRpMetaFilePage);
begin

end;

function TRpPDFDriver.AllowCopies:boolean;
begin
 Result:=false;
end;

function TRpPDFDriver.GetPageSize:TPoint;
begin

end;

function TRpPDFDriver.SetPagesize(PagesizeQt:integer):TPoint;
begin

end;

procedure TRpPDFDriver.SetOrientation(Orientation:TRpOrientation);
begin

end;




end.
