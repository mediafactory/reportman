unit fsectionint;

interface

uses SysUtils, Classes, QGraphics, QForms,Types,
  QButtons, QExtCtrls, QControls, QStdCtrls,
  rpobinsint,rpreport,rpprintitem,rpgraphutils,
  rpconsts,rpsection;


type
  TFSectionProps = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TRpSectionInterface=class(TRpSizeInterface)
  protected
   procedure Paint;override;
  public
   constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);override;
  end;


procedure FreeGridBitmap;

implementation

const
 MIN_GRID_BITMAP_WITH=800;
 MIN_GRID_BITMAP_HEIGHT=600;

var
 fbitmap:TBitmap;
 fbwidth,fbheight:integer;
 fxgrid,fygrid:integer;
 fcolor:TColor;
 flines:boolean;

function DrawBitmapGrid(width,height,xgrid,ygrid:integer;color:TColor;lines:boolean):TBitmap;
var
 rec:TRect;
begin
 if ((width=0) or (height=0)) then
 begin
  Result:=nil;
  exit;
 end;
 if ((width<0) or (height<0) or (xgrid<=0) or (ygrid<=0)) then
 begin
  Raise Exception.Create(SRpIncorrectCalltoDeawGrid);
 end;
 if Assigned(fbitmap) then
 begin
  if ((fbwidth>=width) and (fbheight>=height) and
   (fcolor=color) and (fxgrid=xgrid) and
   (fygrid=ygrid) and (flines=lines)) then
  begin
   Result:=fbitmap;
   exit;
  end;
 end;
 FreeGridBitmap;
 fbitmap:=TBitmap.Create;
 if width<MIN_GRID_BITMAP_WITH then
 begin
  width:=MIN_GRID_BITMAP_WITH;
 end;
 if height<MIN_GRID_BITMAP_HEIGHT then
 begin
  height:=MIN_GRID_BITMAP_HEIGHT;
 end;
 fBitmap.PixelFormat:=pf32bit;
 fbitmap.Width:=width;
 fbitmap.Height:=height;
 // Then draws the bitmap
 fbitmap.Canvas.Brush.Style:=bsSolid;
 rec.Top:=0;rec.Left:=0;
 rec.Right:=fbitmap.Width;
 rec.Bottom:=fbitmap.Height;
 fbitmap.Canvas.FillRect(rec);

 DrawGrid(fbitmap.Canvas,xgrid,ygrid,Width,Height,color,lines,0,0);
 fbheight:=height;
 fbwidth:=width;
 fcolor:=color;
 flines:=lines;
 fygrid:=ygrid;
 fxgrid:=xgrid;

 Result:=fbitmap;
end;


constructor TRpSectionInterface.Create(AOwner:TComponent;pritem:TRpCommonComponent);
var
 opts:TControlStyle;
begin
 if Not (pritem is TRpSection) then
  Raise Exception.Create(SRpIncorrectComponentForInterface);
 inherited Create(AOwner,pritem);
 opts:=ControlStyle;
 include(opts,csCaptureMouse);
 ControlStyle:=opts;
end;


{$R *.xfm}



procedure TRpSectionInterface.Paint;
var
 report:TRpReport;
 rec:TRect;
 bitmap:TBitmap;
begin
 if Not Assigned(fprintitem) then
  exit;
 if Not Assigned(fprintitem.Owner) then
  exit;
 if Not (fprintitem.Owner is TRpReport) then
  exit;
 report:=TRpReport(fprintitem.Owner);
 rec.Top:=0;rec.Left:=0;

 // Draws the grid bitmap

 bitmap:=DrawBitmapGrid(width,height,report.GridWidth,report.GridHeight,report.GridColor,report.GridLines);
 if assigned(bitmap) then
 begin
  Canvas.Draw(0,0,bitmap);
 end;
end;

procedure FreeGridBitmap;
begin
 if Assigned(FBitmap) then
 begin
  fbitmap.free;
  fbitmap:=nil;
 end;
end;



initialization
fbitmap:=nil;
fbwidth:=0;
fbheight:=0;
fcolor:=clBlack;

end.
