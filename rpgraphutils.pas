unit rpgraphutils;

interface

uses SysUtils, Classes, QGraphics, QForms,Types,
  QButtons, QExtCtrls, QControls, QStdCtrls,rpmunits,Qt;


type
  TFRpGraphProgres = class(TForm)
    CancelBtn: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure DrawGrid(Canvas:TCanvas;XWidth,XHeight,PixelsWidth,PixelsHeight:integer;Color:TColor;lines:boolean;XOffset,YOffset:integer);
function twipstopixels(ATwips:integer):integer;
function pixelstotwips(apixels:integer):integer;

implementation

{$R *.xfm}

procedure DrawGrid(Canvas:TCanvas;XWidth,XHeight,PixelsWidth,PixelsHeight:integer;Color:TColor;lines:boolean;XOffset,YOffset:integer);
var
 pixelsperinchx,pixelsperinchy:integer;
 rect:TRect;
 han:QPainterh;
 hanbrush:QBrushH;
 windowwidth,windowheight:integer;
 x,y:integer;
 pixelwidth:integer;
 pixelheight:integer;
 xof,yof:integer;
begin
 if XHeight<=0 then
  exit;
 if XWidth<=0 then
  exit;

 Rect.Left:=0;
 Rect.Top:=0;
 Rect.Right:=PixelsWidth+XOffset;
 Rect.Bottom:=PixelsHeight+YOffset;

 if Screen.PixelsPerInch<=0 then
  exit;
 pixelsperinchx:=Screen.PixelsPerInch;
 pixelsperinchy:=Screen.PixelsPerInch;
 xof:=Round(XOffset/pixelsperinchx*TWIPS_PER_INCHESS);
 yof:=Round(YOffset/pixelsperinchy*TWIPS_PER_INCHESS);
 windowwidth:=Round(TWIPS_PER_INCHESS*(rect.right+XOffset)/pixelsperinchx);
 windowheight:=Round(TWIPS_PER_INCHESS*(rect.bottom+YOffset)/pixelsperinchy);
 Canvas.Pen.Color:=Color;
 Canvas.Brush.Color:=Color;
 han:=Canvas.Handle;
 hanbrush:=Canvas.Brush.handle;

 pixelwidth:=Round(TWIPS_PER_INCHESS/pixelsperinchx)+1;
 pixelheight:=Round(TWIPS_PER_INCHESS/pixelsperinchy)+1;

 // Draws the grid
 // Painting of the ruler
 Canvas.start;
 // Get the pixels per inch
 try
  // 1440 points per inches
  QPainter_SetViewport(han,rect.left,rect.top,
   rect.Right,rect.Bottom);
  QPainter_SetWindow(han,rect.left,rect.top,windowwidth
   ,windowheight);
  if lines then
  begin
   x:=xof+XWidth;
   y:=yof+Xheight;
   while ((x<windowwidth) or (y<windowheight)) do
   begin
    if (x<windowwidth) then
    begin
     Canvas.MoveTo(x,yof);
     Canvas.LineTo(x,windowheight);
      x:=x+XWidth;
    end;
    if (y<windowheight) then
    begin
     Canvas.MoveTo(xof,y);
     Canvas.LineTo(windowwidth,y);
     y:=y+XHeight;
    end;
   end;
  end
  else
  begin
   x:=xof+XWidth;
   while (x<windowwidth) do
   begin
    rect.Left:=x;
    rect.Right:=x+pixelwidth;
    y:=yof+XHeight;
    while y<windowheight do
    begin
     rect.Top:=y;
     rect.Bottom:=y+pixelheight;
     QPainter_fillrect(han,@rect,hanbrush);
 //    Canvas.FillRect(rect);
     y:=y+XHeight;
    end;
    x:=x+XWidth;
   end;
  end;
 finally
  Canvas.Stop;
 end;
end;

function twipstopixels(ATwips:integer):integer;
begin
 Result:=Round((ATwips/TWIPS_PER_INCHESS)*Screen.PixelsPerInch);
end;

function pixelstotwips(apixels:integer):integer;
begin
 Result:=Round((APixels/Screen.PixelsPerInch)*TWIPS_PER_INCHESS);
end;

end.
