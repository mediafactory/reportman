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
  rpgraphutils,rpmunits;

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
  
function StringPenStyleToInt(Value:String):integer;
function StringBrushStyleToInt(Value:String):integer;
function StringShapeTypeToInt(Value:String):integer;

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

end.
