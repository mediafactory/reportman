unit rpvgraphutils;

interface

uses Classes,SysUtils,Windows,Graphics,rpmunits,Printers;

procedure DrawBitmap (Destination:TCanvas;Bitmap:TBitmap;Rec,RecSrc:TRect);
procedure DrawBitmapMosaic(canvas:TCanvas;rec:TRect;bitmap:TBitmap);
function CLXColorToVCLColor(CLXColor:integer):integer;
function CLXIntegerToFontStyle(intfontstyle:integer):TFontStyles;
procedure DrawBitmapMosaicSlow(canvas:TCanvas;rec:Trect;bitmap:TBitmap);
function GetPhysicPageSizeTwips:TPoint;
function GetPageSizeTwips:TPoint;
function GetPageMarginsTWIPS:TRect;

implementation


{ Rutina que imprime un bitmap  }
{ Bitmap: el bitmap a imprimir }
{ REcDesti.Top,Left Situacino en coord.logicas }
procedure DrawBitmap (Destination:TCanvas;Bitmap:TBitmap;Rec,RecSrc:TRect);
var
  Info:PBitmapInfo;
  InfoSize:DWORD;
  Image:Pointer;
  ImageSize:DWORD;
begin
 With Bitmap do
 begin
  Graphics.GetDIBSizes(Handle,InfoSize,ImageSize);
  Info:=AllocMem(InfoSize);
  try
   Image:=AllocMem(Imagesize);
   try
    GetDIB(Handle,Palette,Info^,Image^);
    if not Monochrome then
     SetStretchBltMode(Destination.handle,STRETCH_DELETESCANS);
    with info^.bmiHeader do
     StretchDIBits(Destination.Handle,rec.Left,rec.Top,rec.Right-rec.Left+1,
        rec.Bottom-rec.Top+1,recsrc.Left,recsrc.Top,
         recsrc.Right-recsrc.Left+1,recsrc.Bottom-recsrc.Top+1,Image,Info^,
        DIB_RGB_COLORS,SRCCOPY);
   finally
    {$R-}
    Dispose(Image);
    {$R+}
   end;
  finally
   FreeMem(Info,InfoSize);
  end;
 end;
end;


procedure DrawBitmapMosaic(canvas:TCanvas;rec:Trect;bitmap:TBitmap);
var
 x,y:integer;
 aheight,awidth:integer;
 source,destination:Trect;
begin
 x:=rec.Left;
 y:=rec.Top;
 aheight:=bitmap.height;
 awidth:=bitmap.width;
 Canvas.Draw(x,y,bitmap);
 While ((y+aheight<rec.Bottom) or (x+awidth<rec.Right)) do
 begin
  // Copy the bitmap to three positions
  source.left:=x;
  source.top:=y;
  source.right:=awidth;
  source.bottom:=aheight;
  // Right
  destination.Left:=x+awidth;
  destination.top:=y;
  destination.right:=x+awidth+awidth;
  destination.bottom:=y+aheight;
  Canvas.CopyRect(destination,canvas,source);
  // Down
  destination.Left:=x;
  destination.top:=y+aheight;
  destination.right:=x+awidth;
  destination.bottom:=y+aheight+aheight;
  Canvas.CopyRect(destination,canvas,source);
  // Down-Right
  destination.Left:=x+awidth;
  destination.top:=y+aheight;
  destination.right:=x+awidth+awidth;
  destination.bottom:=y+aheight+aheight;
  Canvas.CopyRect(destination,canvas,source);

  // NextStep
  aheight:=aheight*2;
  awidth:=awidth*2;
 end;
end;


function CLXIntegerToFontStyle(intfontstyle:integer):TFontStyles;
begin
 Result:=[];
 if (intfontstyle and 1)>0 then
  include(Result,fsBold);
 if (intfontstyle and (1 shl 1))>0 then
  include(Result,fsItalic);
 if (intfontstyle and (1 shl 2))>0 then
  include(Result,fsUnderline);
 if (intfontstyle and (1 shl 3))>0 then
  include(Result,fsStrikeOut);
end;


function CLXColorToVCLColor(CLXColor:integer):integer;
begin
 Result:=CLXColor AND $00FFFFFF;
end;

procedure DrawBitmapMosaicSlow(canvas:TCanvas;rec:Trect;bitmap:TBitmap);
var
 x,y:integer;
 arec,recsrc:TRect;
begin
 recsrc.Left:=0;
 recsrc.Top:=0;
 recsrc.Right:=Bitmap.Width-1;
 recsrc.Bottom:=Bitmap.Height-1;
 x:=rec.Left;
 y:=rec.Top;
 While y<rec.Bottom do
 begin
  While x<rec.right do
  begin
   arec.Left:=x;
   arec.Top:=y;
   arec.Bottom:=y+bitmap.Height-1;
   arec.Right:=x+bitmap.Width-1;
   DrawBitmap(Canvas,Bitmap,arec,recsrc);
   x:=x+bitmap.width;
  end;
  x:=rec.left;
  y:=y+bitmap.Height;
 end;
end;

function GetPageMarginsTWIPS:TRect;
var rec:TRect;
    DC:HDC;
    pagesize:TPoint;
    physical:TPoint;
    offset:TPoint;
    dpix,dpiy:integer;
    apagewidth,apageheight:integer;
begin
 if Printer.printers.count<1 then
 begin
  result.Left:=0;
  result.Top:=0;
  result.Bottom:=16637;
  result.Right:=12047;
  exit;
 end;
 DC:=Printer.handle;

 dpix:=GetDeviceCaps(DC,LOGPIXELSX); //  printer.XDPI;
 dpiy:=GetDeviceCaps(DC,LOGPIXELSY);  // printer.YDPI;

 apagewidth:=GetDeviceCaps(DC,HORZRES);
 apageheight:=GetDeviceCaps(DC,VERTRES);
 pagesize.x:=Round(apagewidth/dpix*TWIPS_PER_INCHESS);
 pagesize.y:=Round(apageheight/dpix*TWIPS_PER_INCHESS);

 physical.x:=GetDeviceCaps(DC,PHYSICALWIDTH);
 physical.y:=GetDeviceCaps(DC,PHYSICALHEIGHT);
 // Transform to twips
 physical.X:=Round(physical.X/dpix*TWIPS_PER_INCHESS);
 physical.Y:=Round(physical.Y/dpiy*TWIPS_PER_INCHESS);

 // Gets top/left offser
 offset.x:=GetDeviceCaps(DC,PHYSICALOFFSETX);
 offset.y:=GetDeviceCaps(DC,PHYSICALOFFSETY);
 // Transform to twips
 offset.X:=Round(offset.X/dpix*TWIPS_PER_INCHESS);
 offset.Y:=Round(offset.Y/dpiy*TWIPS_PER_INCHESS);

 rec.Left:=offset.X;
 rec.Top:=offset.Y;
 rec.Right:=physical.X-(physical.X-pagesize.X-offset.X);
 rec.Bottom:=physical.Y-(physical.Y-pagesize.Y-offset.Y);

 Result:=rec;
end;


function GetPageSizeTwips:TPoint;
var
 DC:HDC;
 dpix,dpiy:integer;
 apagewidth,apageheight:integer;
begin
 if Printer.printers.count<1 then
 begin
  result.y:=16637;
  result.x:=12047;
  exit;
 end;
 DC:=Printer.handle;
 // Get the device units of
 dpix:=GetDeviceCaps(DC,LOGPIXELSX); //  printer.XDPI;
 dpiy:=GetDeviceCaps(DC,LOGPIXELSY);  // printer.YDPI;

 apagewidth:=GetDeviceCaps(DC,HORZRES);
 apageheight:=GetDeviceCaps(DC,VERTRES);
 Result.x:=Round(apagewidth/dpix*TWIPS_PER_INCHESS);
 Result.y:=Round(apageheight/dpiy*TWIPS_PER_INCHESS);
end;

function GetPhysicPageSizeTwips:TPoint;
var
 DC:HDC;
 dpix,dpiy:integer;
begin
 if Printer.printers.count<1 then
 begin
  result.y:=16637;
  result.x:=12047;
  exit;
 end;
 DC:=Printer.handle;
 // Get the device units of
 dpix:=GetDeviceCaps(DC,LOGPIXELSX); //  printer.XDPI;
 dpiy:=GetDeviceCaps(DC,LOGPIXELSY);  // printer.YDPI;

 Result.x:=GetDeviceCaps(DC,PHYSICALWIDTH);
 Result.y:=GetDeviceCaps(DC,PHYSICALHEIGHT);
 // Transform to twips
 Result.X:=Round(Result.X/dpix*TWIPS_PER_INCHESS);
 Result.Y:=Round(Result.Y/dpiy*TWIPS_PER_INCHESS);
end;


end.
