{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       TRpInfoProvider CLX                             }
{       Provides information about fonts and bitmaps    }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{                                                       }
{*******************************************************}

unit rpinfoprovclx;


interface

{$I rpconf.inc}

uses Classes,SysUtils,QForms,
    QGraphics,rpinfoprovid,types,Qt,
    rpgraphutils;


type
 TRpCLXInfoProvider=class(TInterfacedObject,IRpInfoProvider)
  FBitmap:TBitmap;
  procedure FillFontInfo(pdffont:TRpPDFFont;info:TRpTTFontInfo);
  procedure FillFontData(pdffont:TRpPDFFont;data:TRpTTFontData);
  constructor Create;
  destructor destroy;override;
 end;

implementation

constructor TRpCLXInfoProvider.Create;
begin
 FBitmap:=TBitmap.Create;
 FBitmap.Width:=100;
 FBitmap.Height:=100;
end;

destructor TRpCLXInfoProvider.destroy;
begin
 FBitmap.free;

 inherited destroy;
end;


procedure TRpCLXInfoProvider.FillFontInfo(pdffont:TRpPDFFont;info:TRpTTFontInfo);
var
 i:integer;
 conversionfactor:double;
 fm:QFontMetricsH;
 wc:WideChar;
begin
 conversionfactor:=0.75;
 FBitmap.Canvas.Font.Name:=pdfFont.WFontName;
 FBitmap.Canvas.Font.Size:=1000;
 FBitmap.Canvas.Font.Style:=IntegerToFontStyle(pdfFont.Style);

 fm:=QFontMetrics_create(FBitmap.Canvas.Font.Handle);
 try
  for i:=32 to 255 do
  begin
   wc:=Widechar(i);
   info.charwidths[i]:=Round(conversionfactor*QFontMetrics_width(fm,@wc));
  end;
 finally
  QFontMetrics_destroy(fm);
 end;
end;

procedure TRpCLXInfoProvider.FillFontData(pdffont:TRpPDFFont;data:TRpTTFontData);
var
 fm:QFontMetricsH;
 maxleft,maxright:integer;
 leftb,rightb:integer;
 i:integer;
 wc:WideChar;
 conversionfactor:double;
begin
 conversionfactor:=0.75;
 FBitmap.Canvas.Font.Name:=pdfFont.WFontName;
 FBitmap.Canvas.Font.Size:=1000;
 FBitmap.Canvas.Font.Style:=IntegerToFontStyle(pdfFont.Style);
 data.postcriptname:=pdfFont.WFontName;
 data.FullName:=pdfFont.WFontName;
 data.FaceName:=pdfFont.WFontName;
 data.StyleName:=pdfFont.WFontName;
 data.Encoding:='WinAnsiEncoding';
 data.FontWeight:=QFont_weight(FBitmap.Canvas.Font.Handle)*10;
 fm:=QFontMetrics_create(FBitmap.Canvas.Font.Handle);
 try
  data.Ascent:=Round(conversionfactor*QFontMetrics_ascent(fm));
  data.Descent:=-Round(conversionfactor*QFontMetrics_descent(fm));
  data.MaxWidth:=Round(conversionfactor*QFontMetrics_MaxWidth(fm));
  wc:=Widechar(32);
  maxleft:=QFontMetrics_leftBearing(fm,@wc);
  maxright:=QFontMetrics_rightBearing(fm,@wc);
  for i:=33 to 255 do
  begin
   wc:=Widechar(i);
   leftb:=QFontMetrics_leftBearing(fm,@wc);
   rightb:=QFontMetrics_rightBearing(fm,@wc);
   if leftb<maxleft then
    maxleft:=leftb;
   if rightb>maxright then
    maxright:=rightb;
  end;
  data.FontBBox.Left:=Round(0.705*maxleft);
  data.FontBBox.Top:=data.Ascent;
  data.FontBBox.Bottom:=data.Descent;
  data.FontBBox.Right:=data.MaxWidth+data.FontBBox.Left;
  data.Leading:=Round(conversionfactor*QFontMetrics_leading(fm));
  data.CapHeight:=data.Ascent;
  data.Flags:=32;
  data.ItalicAngle:=0;

  if fsItalic in FBitmap.Canvas.Font.Style then
  begin
   data.Flags:=data.Flags+64;
   data.ItalicAngle:=-15;
  end;
  data.AvgWidth:=0;
  data.StemV:=0;
 finally
  QFontMetrics_destroy(fm);
 end;
end;

end.
