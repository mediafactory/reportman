{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       fdrawint                                        }
{       Implementation draw item and image interface    }
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

unit fdrawint;

interface

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,types,
  rpprintitem,rpdrawitem,rpobinsint,rpconsts,
  rpgraphutils,rpmunits,rptypes;

type
 TRpDrawInterface=class(TRpSizePosInterface)
  private
  protected
   procedure Paint;override;
  public
   constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);override;
   procedure GetProperties(lnames,ltypes,lvalues:TStrings);override;
   procedure SetProperty(pname:string;value:string);override;
   function GetProperty(pname:string):string;override;
   procedure GetPropertyValues(pname:string;lpossiblevalues:TStrings);override;
 end;

 TRpImageInterface=class(TRpSizePosInterface)
  private
   FBitmap:TBitmap;
  protected
   procedure Paint;override;
  public
   constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);override;
   destructor Destroy;override;
   procedure GetProperties(lnames,ltypes,lvalues:TStrings);override;
   procedure SetProperty(pname:string;value:string);override;
   function GetProperty(pname:string):string;override;
   procedure GetPropertyValues(pname:string;lpossiblevalues:TStrings);override;
   procedure SetProperty(pname:string;stream:TMemoryStream);override;
   procedure GetProperty(pname:string;var Stream:TMemoryStream);override;
 end;

  TFDrawInterface = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


const
 StringPenStyle:array [psSolid..psClear] of String=
  (SRpSPSolid, SRpSPDash, SRpSPDot, SRpSPDashDot, SRpSPDashDotDot, SRpSPClear);
 StringBrushStyle:array [bsSolid..bsDense7] of String=
  (SRpSBSolid, SRpSBClear, SRpSBHorizontal, SRpSBVertical, SRpSBFDiagonal,
  SRpSBBDiagonal, SRpSBCross, SRpSBDiagCross, SRpSBDense1, SRpSBDense2,
  SRpSBDense3, SRpSBDense4, SRpSBDense5, SRpSBDense6,SRpSBDense7);
 StringShapeType:array [stRectangle..stCircle] of String=(
  SRpsSRectangle,SRpsSSquare,SRpsSRoundRect,
  SRpsSRoundSquare,SRpsSEllipse,SRpsSCircle);
 StringDrawStyles:array [rpDrawCrop..rpDrawTile] of string=(
  SRPSDrawCrop,SRPSDrawStretch,SRPSDrawFull,SRpDrawTile);
 StringCopyModes:array [cmBlackness..cmCreateMask] of string=(
  SRpBlackness, SRpDstInvert, SRpMergeCopy, SRpMergePaint,
  SRpNotSrcCopy, SRpNotSrcErase, SRpPatCopy, SRpPatInvert,
  SRpPatPaint, SRpSrcAnd, SRpSrcCopy, SRpSrcErase,
  SRpSrcInvert, SRpSrcPaint, SRpWhiteness, SRpCreateMask);
// TCopyMode = (cmBlackness, cmDstInvert, cmMergeCopy, cmMergePaint,
// cmNotSrcCopy, cmNotSrcErase, cmPatCopy, cmPatInvert,
// cmPatPaint, cmSrcAnd, cmSrcCopy, cmSrcErase,
// cmSrcInvert, cmSrcPaint, cmWhiteness, cmCreateMask);

function StringPenStyleToInt(Value:String):integer;
function StringBrushStyleToInt(Value:String):integer;
function StringShapeTypeToInt(Value:String):integer;
function StringDrawStyleToDrawStyle(Value:string):TRpImageDrawStyle;
function StringCopyModeToCopyMode(Value:string):TCopyMode;

implementation

{$R *.xfm}


constructor TRpDrawInterface.Create(AOwner:TComponent;pritem:TRpCommonComponent);
begin
 if Not (pritem is TRpShape) then
  Raise Exception.Create(SRpIncorrectComponentForInterface);
 inherited Create(AOwner,pritem);
end;

procedure TRpDrawInterface.GetProperties(lnames,ltypes,lvalues:TStrings);
begin
 inherited GetProperties(lnames,ltypes,lvalues);


 // Shape
 lnames.Add(SrpSShape);
 ltypes.Add(SRpSList);
 lvalues.Add(StringShapeType[TShapeType(TRpShape(printitem).Shape)]);

 // Pen style
 lnames.Add(SrpSPenStyle);
 ltypes.Add(SRpSList);
 lvalues.Add(StringPenStyle[TPenStyle(TRpShape(printitem).PenStyle)]);

 // Pen Color
 lnames.Add(SrpSPenColor);
 ltypes.Add(SRpSColor);
 lvalues.Add(IntToStr(TRpShape(printitem).PenColor));

 // PenWidth
 lnames.Add(SrpSPenWidth);
 ltypes.Add(SRpSString);
 lvalues.Add(gettextfromtwips(TRpShape(printitem).PenWidth));


 // Brush style
 lnames.Add(SrpSBrushStyle);
 ltypes.Add(SRpSList);
 lvalues.Add(StringBrushStyle[TBrushStyle(TRpShape(printitem).BrushStyle)]);

 // Brush Color
 lnames.Add(SrpSBrushColor);
 ltypes.Add(SRpSColor);
 lvalues.Add(IntToStr(TRpShape(printitem).BrushColor));

end;

function StringPenStyleToInt(Value:String):integer;
var
 i:TPenStyle;
begin
 Result:=0;
 for i:=psSolid to psClear do
 begin
  if Value=StringPenStyle[i] then
  begin
   Result:=Integer(i);
   break;
  end;
 end;
end;

function StringShapeTypeToInt(Value:String):integer;
var
 i:TShapetype;
begin
 Result:=0;
 for i:=stRectangle to stCircle do
 begin
  if Value=StringShapeType[i] then
  begin
   Result:=Integer(i);
   break;
  end;
 end;
end;


function StringBrushStyleToInt(Value:String):integer;
var
 i:TBrushStyle;
begin
 Result:=0;
 for i:=bsSolid to bsDense7 do
 begin
  if Value=StringBrushStyle[i] then
  begin
   Result:=Integer(i);
   break;
  end;
 end;
end;

function StringDrawStyleToDrawStyle(Value:string):TRpImageDrawStyle;
var
 i:TRpImageDrawStyle;
begin
 Result:=rpDrawCrop;
 for i:=rpDrawCrop to rpDrawTile do
 begin
  if Value=StringDrawStyles[i] then
  begin
   Result:=i;
   break;
  end;
 end;
end;

function StringCopyModeToCopyMode(Value:string):TCopyMode;
var
 i:TCopyMode;
begin
 Result:=cmSrcCopy;
 for i:=cmBlackness to cmCreateMask do
 begin
  if Value=StringCopyModes[i] then
  begin
   Result:=i;
   break;
  end;
 end;
end;

procedure TRpDrawInterface.SetProperty(pname:string;value:string);
begin
 if length(value)<1 then
  exit;
 if pname=SRpSShape then
 begin
  TRpShape(fprintitem).Shape:=StringShapeTypeToInt(Value);
  invalidate;
  exit;
 end;
 if pname=SRpSPenStyle then
 begin
  TRpShape(fprintitem).PenStyle:=StringPenStyleToInt(Value);
  invalidate;
  exit;
 end;
 if pname=SRpSPenColor then
 begin
  TRpShape(fprintitem).PenColor:=StrToInt(Value);
  invalidate;
  exit;
 end;
 if pname=SRpSPenWidth then
 begin
  TRpShape(fprintitem).PenWidth:=gettwipsfromtext(value);
  invalidate;
  exit;
 end;
 if pname=SRpSBrushStyle then
 begin
  TRpShape(fprintitem).BrushStyle:=StringBrushStyleToInt(Value);
  invalidate;
  exit;
 end;
 if pname=SRpSBrushColor then
 begin
  TRpShape(fprintitem).BrushColor:=StrToInt(Value);
  invalidate;
  exit;
 end;

 inherited SetProperty(pname,value);
end;

function TRpDrawInterface.GetProperty(pname:string):string;
begin
 Result:='';
 if pname=SrpSShape then
 begin
  Result:=StringShapeType[TShapeType(TRpShape(printitem).Shape)];
  exit;
 end;
 if pname=SrpSPenStyle then
 begin
  Result:=StringPenStyle[TPenStyle(TRpShape(printitem).PenStyle)];
  exit;
 end;
 if pname=SrpSPenColor then
 begin
  Result:=IntToStr(TRpShape(printitem).PenColor);
  exit;
 end;
 if pname=SrpSPenWidth then
 begin
  Result:=gettextfromtwips(TRpShape(printitem).PenWidth);
  exit;
 end;

 if pname=SrpSBrushStyle then
 begin
  Result:=StringBrushStyle[TBrushStyle(TRpShape(printitem).BrushStyle)];
  exit;
 end;
 if pname=SrpSBrushColor then
 begin
  Result:=IntToStr(TRpShape(printitem).BrushColor);
  exit;
 end;

 Result:=inherited GetProperty(pname);
end;



procedure TRpDrawInterface.Paint;
var
 ashape:TRpShape;
 X, Y, W, H, S: Integer;
begin
 ashape:=TRpShape(printitem);
 Canvas.Brush.Style:=TBrushStyle(ashape.BrushStyle);
 Canvas.Pen.Style:=TPenStyle(ashape.PenStyle);
 Canvas.Pen.Color:=ashape.Pencolor;
 Canvas.Brush.Color:=ashape.BrushColor;
 Canvas.Pen.Width:=ashape.PenWidth;

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
 if TShapeType(ashape.Shape) in [stSquare, stRoundSquare, stCircle] then
 begin
  Inc(X, (W - S) div 2);
  Inc(Y, (H - S) div 2);
  W := S;
  H := S;
 end;
 case TShapeType(ashape.Shape) of
  stRectangle, stSquare:
   Canvas.Rectangle(X, Y, X + W, Y + H);
  stRoundRect, stRoundSquare:
   Canvas.RoundRect(X, Y, X + W, Y + H, S div 4, S div 4);
  stCircle, stEllipse:
   Canvas.Ellipse(X, Y, X + W, Y + H);
 end;
end;



procedure TRpDrawInterface.GetPropertyValues(pname:string;lpossiblevalues:TStrings);
var
 pi:TPenStyle;
 bi:TBrushStyle;
 shi:TShapeType;
begin
 if pname=SrpSShape then
 begin
  lpossiblevalues.clear;
  for shi:=stRectangle to stCircle do
  begin
   lpossiblevalues.Add(StringShapeType[shi]);
  end;
  exit;
 end;
 if pname=SrpSPenStyle then
 begin
  lpossiblevalues.clear;
  for pi:=psSolid to psClear do
  begin
   lpossiblevalues.Add(StringPenStyle[pi]);
  end;
  exit;
 end;
 if pname=SrpSBrushStyle then
 begin
  lpossiblevalues.clear;
  for bi:=bsSolid to bsDense7 do
  begin
   lpossiblevalues.Add(StringBrushStyle[bi]);
  end;
  exit;
 end;
 inherited GetPropertyValues(pname,lpossiblevalues);
end;

// Image Interface

constructor TRpImageInterface.Create(AOwner:TComponent;pritem:TRpCommonComponent);
begin
 if Not (pritem is TRpImage) then
  Raise Exception.Create(SRpIncorrectComponentForInterface);
 inherited Create(AOwner,pritem);
end;

destructor TRpImageInterface.Destroy;
begin
 if Assigned(FBitmap) then
  FBitmap.free;
 inherited destroy;
end;

procedure TRpImageInterface.GetProperties(lnames,ltypes,lvalues:TStrings);
begin
 inherited GetProperties(lnames,ltypes,lvalues);


 // DrawStyle
 lnames.Add(SRpDrawStyle);
 ltypes.Add(SRpSList);
 lvalues.Add(StringDrawStyles[TRpImage(printitem).DrawStyle]);

 // Expression
 lnames.Add(SrpSExpression);
 ltypes.Add(SRpSExpression);
 lvalues.Add(TRpImage(printitem).Expression);

 // Image
 lnames.Add(SrpSImage);
 ltypes.Add(SRpSImage);
 lvalues.Add('['+FormatFloat('###,###0.00',TRpImage(printitem).Stream.Size/1024)+
  SRpKbytes+']');

 // DPI
 lnames.Add(SRpDPIRes);
 ltypes.Add(SRpSString);
 lvalues.Add(IntToStr(TRpImage(printitem).DPIRes));

 // CopyMode
 lnames.Add(SRpCopyMode);
 ltypes.Add(SRpSList);
 lvalues.Add(StringCopyModes[TCopyMode(TRpImage(printitem).CopyMode)]);
end;



procedure TRpImageInterface.SetProperty(pname:string;value:string);
begin
 if length(value)<1 then
  exit;
 if pname=SRpSExpression then
 begin
  TRpImage(fprintitem).Expression:=Value;
  invalidate;
  exit;
 end;
 if pname=SRpDrawStyle then
 begin
  TRpImage(fprintitem).DrawStyle:=StringDrawStyleToDrawStyle(Value);
  invalidate;
  exit;
 end;
 if pname=SRpCopyMode then
 begin
  TRpImage(fprintitem).CopyMode:=Integer(StringCopyModeToCopyMode(Value));
  invalidate;
  exit;
 end;
 if pname=SRpDPIRes then
 begin
  TRpImage(fprintitem).DPIRes:=StrToInt(Value);
  if TRpImage(fprintitem).DPIRes<=0 then
   TRpImage(fprintitem).DPIRes:=100;
  invalidate;
  exit;
 end;
 inherited SetProperty(pname,value);
end;

function TRpImageInterface.GetProperty(pname:string):string;
begin
 Result:='';
 if pname=SrpSExpression then
 begin
  Result:=TRpImage(printitem).Expression;
  exit;
 end;
 if pname=SrpDrawStyle then
 begin
  Result:=StringDrawStyles[TRpImage(printitem).DrawStyle];
  exit;
 end;
 if pname=SrpCopyMode then
 begin
  Result:=StringCopyModes[TCopyMode(TRpImage(printitem).CopyMode)];
  exit;
 end;
 if pname=SRpDPIRes then
 begin
  Result:=IntToStr(TRpImage(fprintitem).DPIRes);
  exit;
 end;
 Result:=inherited GetProperty(pname);
end;



procedure TRpImageInterface.Paint;
var
 aimage:TRpImage;
 rec:TRect;
 dpix,dpiy:integer;
begin
 aimage:=TRpImage(printitem);
 try
  Canvas.Rectangle(0,0,Width,Height);
  if aimage.Stream.Size>0 then
  begin
   if Not Assigned(FBitmap) then
   begin
    FBitmap:=TBitmap.Create;
    FBitmap.PixelFormat:=pf32bit;
    // Try to load it
    aimage.Stream.Seek(soFromBeginning,0);
    try
     FBitmap.LoadFromStream(aimage.Stream);
    except
     FBitmap.free;
     FBitmap:=nil;
     raise;
    end;
   end;
   Canvas.CopyMode:=TCopyMode(aimage.CopyMode);
   rec.Top:=0;rec.Left:=0;
   rec.Bottom:=Height-1;rec.Right:=Width-1;
   // Draws it with the style
   case aimage.DrawStyle of
    rpDrawStretch:
     begin
      Canvas.StretchDraw(rec,fbitmap);
     end;
    rpDrawCrop:
     begin
      Canvas.Draw(0,0,fbitmap);
     end;
    rpDrawFull:
     begin
      dpix:=Screen.PixelsPerInch;
      dpiy:=Screen.PixelsPerInch;
      rec.Bottom:=round(fbitmap.height/aimage.dpires)*dpiy-1;
      rec.Right:=round(fbitmap.width/aimage.dpires)*dpix-1;
      Canvas.StretchDraw(rec,fbitmap);
     end;
    rpDrawTile:
     begin
      Canvas.TiledDraw(rec,fbitmap);
     end;
   end;
  end;
  // Draws the expresion
  Canvas.Brush.Style:=bsClear;
  Canvas.Rectangle(0,0,Width,Height);
  Canvas.TextOut(0,0,SRpSImage+aimage.Expression);
 except
  Canvas.TextOut(0,0,SRpInvalidImageFormat);
 end;
end;


procedure TRpImageInterface.SetProperty(pname:string;stream:TMemoryStream);
begin
 if pname=SrpSImage then
 begin
  TRpImage(printitem).Stream:=stream;
  FBitmap.Free;
  FBitmap:=nil;
  Invalidate;
  exit;
 end;
 inherited SetProperty(pname,stream);
end;

procedure TRpImageInterface.GetProperty(pname:string;var Stream:TMemoryStream);
begin
 if pname=SrpSImage then
 begin
  Stream:=TRpImage(printitem).Stream;
  exit;
 end;
 inherited GetProperty(pname,stream);
end;



procedure TRpImageInterface.GetPropertyValues(pname:string;lpossiblevalues:TStrings);
var
 i:TRpImageDrawStyle;
 k:TCopyMode;
begin
 if pname=SrpDrawStyle then
 begin
  lpossiblevalues.clear;
  for i:=rpDrawCrop to rpDrawTile do
  begin
   lpossiblevalues.Add(StringDrawStyles[i]);
  end;
  exit;
 end;
 if pname=SrpCopyMode then
 begin
  lpossiblevalues.clear;
  for k:=cmBlackness to cmCreateMask do
  begin
   lpossiblevalues.Add(StringCopyModes[k]);
  end;
  exit;
 end;
 inherited GetPropertyValues(pname,lpossiblevalues);
end;


end.
