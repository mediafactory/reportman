{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       Rpsectionint                                    }
{       Implementation of section designer              }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}

unit fsectionint;

interface

uses SysUtils, Classes, QGraphics, QForms,Types,
  QButtons, QExtCtrls, QControls, QStdCtrls,
  rpobinsint,rpreport,rpprintitem,rpgraphutils,
  rpobjinsp,frpstruc,flabelint,rplabelitem,
  rpconsts,rpsection,rptypes;


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
   private
   protected
    procedure Paint;override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
   public
    OnPosChange:TNotifyEvent;
    freportstructure:TFRpStructure;
    childlist:TList;
    constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);override;
    destructor destroy;override;
    procedure CreateChilds;
  end;


procedure FreeGridBitmap;

implementation

uses fmain, fdesign;

{$R *.xfm}

const
 MIN_GRID_BITMAP_WITH=800;
 MIN_GRID_BITMAP_HEIGHT=600;

var
 fbitmap:TBitmap;
 fbwidth,fbheight:integer;
 fxgrid,fygrid:integer;
 fcolor:TColor;
 flines:boolean;

procedure FreeGridBitmap;
begin
 if Assigned(FBitmap) then
 begin
  fbitmap.free;
  fbitmap:=nil;
 end;
end;

function DrawBitmapGrid(width,height,xgrid,ygrid:integer;color:TColor;lines:boolean):TBitmap;
var
 rec:TRect;
begin
 if ((width=0) or (height=0)) then
 begin
  Result:=nil;
  exit;
 end;
 try
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
 except
  FreeGridBitmap;
  Result:=nil;
 end;
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
 ChildList:=TList.Create;
end;

destructor TRpSectionInterface.destroy;
begin
 childlist.free;
 inherited destroy;
end;




procedure TRpSectionInterface.Paint;
var
 report:TRpReport;
 rec:TRect;
 bitmap:TBitmap;
begin
 if Assigned(OnPosChange) then
  OnPosChange(Self);
 if Not Assigned(fprintitem) then
  exit;
 if Not Assigned(fprintitem.Owner) then
  exit;
 if Not (fprintitem.Owner is TRpReport) then
  exit;
 report:=TRpReport(fprintitem.Owner);
 if report.GridVisible then
 begin
  bitmap:=DrawBitmapGrid(width,height,report.GridWidth,report.GridHeight,report.GridColor,report.GridLines);
  if assigned(bitmap) then
  begin
   Canvas.Draw(0,0,bitmap);
   exit;
  end;
 end;

 rec.Top:=0;rec.Left:=0;
 rec.Bottom:=Height;
 rec.Right:=Width;
 Canvas.Brush.Color:=clwhite;
 Canvas.FillRect(rec);
end;

procedure TRpSectionInterface.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
var
 labelint:TRpLabelInterface;
 alabel:TRpLabel;
 aitem:TRpCommonListItem;
begin
 inherited MouseDown(Button,Shift,X,Y);

 // There's a selected item insert it
 if fmainf.BArrow.Down then
 begin
  // Selects object inspector section properties
  freportstructure.SelectDataItem(printitem);
  TFObjInsp(fobjinsp).CompItem:=self;
  exit;
 end;
 if fmainf.BLabel.Down then
 begin
  alabel:=TRpLabel.Create(printitem.Owner);
  alabel.PosX:=pixelstotwips(X);
  alabel.PosY:=pixelstotwips(Y);
  GenerateNewName(alabel);
  aitem:=TRpSection(printitem).Components.Add;
  aitem.Component:=alabel;
  labelint:=TRpLabelInterface.Create(Self,alabel);
  labelint.Parent:=parent;
  labelint.UpdatePos;
  labelint.fobjinsp:=fobjinsp;

  childlist.Add(labelint);
  fmainf.BArrow.Down:=true;
 end;
end;

procedure TRpSectionInterface.CreateChilds;
var
 sec:TRpSection;
 i:integer;
 compo:TRpCommonComponent;
 labelint:TRpLabelInterface;
begin
 sec:=TRpSection(printitem);
 for i:=0 to sec.Components.Count-1 do
 begin
  compo:=sec.Components.Items[i].Component;
  if compo is TRpLabel then
  begin
   labelint:=TRpLabelInterface.Create(Self,compo);
   labelint.Parent:=parent;
   labelint.UpdatePos;
   labelint.fobjinsp:=fobjinsp;
   childlist.Add(labelint)
  end;
 end;
end;



initialization
fbitmap:=nil;
fbwidth:=0;
fbheight:=0;
fcolor:=clBlack;

end.
