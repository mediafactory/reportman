{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       Rpmdobinsintvcl                                 }
{                                                       }
{       Basic properties editor, size, position         }
{       Especial controls to modify basic properties    }
{       And notify object inspector about object        }
{       selection                                       }
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

unit rpmdobinsintvcl;

interface

{$I rpconf.inc}

uses
{$IFDEF USEVARIANTS}
 Types,
{$ENDIF}
  Graphics,Forms,Controls,Dialogs,
 rpmdconsts,classes,sysutils,rpmunits,
 rpprintitem,rpvgraphutils,rpgraphutilsvcl,rpsection,
 rpreport,rptypes;

const
 CONS_MODIWIDTH=5;
 CONS_MINIMUMMOVE=5;
 CONS_MINHEIGHT=5;
 CONS_MINWIDTH=5;
 CONS_SELWIDTH=7;
 MAX_CHeight=144000;
 MAX_CWidth=144000;
type
 TRpPropertytype=(rppinteger,rppcurrency,rppstring,rpplist,rpcustom);


 // The implementation and size
 TRpSizeInterface=class(TGraphicControl)
  private
  protected
   FSelected:boolean;
   procedure SetSelected(Value:boolean);
  protected
   fprintitem:TRpCommonComponent;
   procedure Paint;override;
   procedure DrawSelected;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
   procedure DblClick; override;
  public
   fobjinsp:TComponent;
   procedure UpdatePos;virtual;
   procedure GetProperties(lnames,ltypes,lvalues:TStrings);virtual;
   procedure GetPropertyValues(pname:string;lpossiblevalues:TStrings);virtual;
   procedure SetProperty(pname:string;value:Widestring);overload;virtual;
   procedure SetProperty(pname:string;stream:TMemoryStream);overload;virtual;
   procedure GetProperty(pname:string;var Stream:TMemoryStream);overload;virtual;
   function GetProperty(pname:string):Widestring;overload;virtual;
   constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);reintroduce;overload;virtual;
   property printitem:TRpCommonComponent read fprintitem;
   property Selected:Boolean read FSelected write SetSelected;
 end;

 TRpSizePosInterfaceClass=class of TRpSizePosInterface;

 TRpRectangle=class(TWinControl)
  protected
//   procedure Paint;override;
  public
   Solid:boolean;
   constructor Create(AOwner:TComponent);override;
 end;

 // The implementation for and size and position
 TRpSizePosInterface=class(TRpSizeInterface)
  private
   FXOrigin,FYOrigin:integer;
   FBlocked:boolean;
   FRectangle:TRpRectangle;
   FRectangle2:TRpRectangle;
   FRectangle3:TRpRectangle;
   FRectangle4:TRpRectangle;
  protected
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
   procedure MouseMove(Shift: TShiftState; X, Y: Integer);override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
   procedure Paint;override;
  public
   SectionInt:TRpSizeInterface;
   procedure DoSelect;
   procedure UpdatePos;override;
   procedure GetProperties(lnames,ltypes,lvalues:TStrings);override;
   procedure SetProperty(pname:string;value:Widestring);override;
   function GetProperty(pname:string):Widestring;override;
   procedure GetPropertyValues(pname:string;lpossiblevalues:TStrings);override;
   class procedure FillAncestors(alist:TStrings);virtual;
   constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);override;
 end;


 TRpGenTextInterface=class(TRpSizePosInterface)
  private
  protected
  public
   class procedure FillAncestors(alist:TStrings);override;
   constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);override;
   procedure GetProperties(lnames,ltypes,lvalues:TStrings);override;
   procedure SetProperty(pname:string;value:Widestring);override;
   function GetProperty(pname:string):Widestring;override;
   procedure GetPropertyValues(pname:string;lpossiblevalues:TStrings);override;
 end;


 TRpBlackControl=class(TGraphicControl)
  private
   FXOrigin,FYOrigin:integer;
   FRectangle:TRpRectangle;
   FRectangle2:TRpRectangle;
   FRectangle3:TRpRectangle;
   FRectangle4:TRpRectangle;
   FControl:TControl;
   FAllowOverSize:boolean;
   procedure CalcNewCoords(var NewLeft,
    NewTop,NewWidth,NewHeight,X,Y:integer);
  protected
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
   procedure MouseMove(Shift: TShiftState; X, Y: Integer);override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
  protected
   procedure Paint;override;

  public
   constructor Create(AOwner:TComponent);override;
   property Control:TControl read FControl write FControl;
  end;

 TRpSizeModifier=class(TComponent)
  private
   FOnSizeChange:TNotifyEvent;
   FAllowOverSize:boolean;
   FBlacks:array[0..3] of TRpBlackControl;
   FControl:TControl;
   FOnlysize:boolean;
   FGridEnabled:boolean;
   FGridX,FGridY:integer;
   procedure SetControl(Value:TControl);
   procedure SetOnlySize(Value:Boolean);
   procedure SetAllowOversize(Value:Boolean);
  public
   procedure UpdatePos;
   constructor Create(AOwner:TComponent);override;
  published
   property Control:TControl read FControl write SetControl;
   property OnSizeChange:TNotifyEvent read FOnSizeChange write FOnSizeChange;
   property OnlySize:Boolean read FOnlySize write SetOnlySize default false;
   property AllowOverSize:Boolean read FAllowOverSize write SetAllowOverSize default false;
   property GridX:integer read FGridX write FGridX;
   property GridY:integer read FGridY write FGridY;
   property GridEnabled:boolean read FGridEnabled write FGridEnabled;
  end;

implementation

uses rpmdobjinspvcl;


const
 AlignmentFlags_SingleLine=64;
 AlignmentFlags_AlignHCenter = 4 { $4 };
 AlignmentFlags_AlignTop = 8 { $8 };
 AlignmentFlags_AlignBottom = 16 { $10 };
 AlignmentFlags_AlignVCenter = 32 { $20 };
 AlignmentFlags_AlignLeft = 1 { $1 };
 AlignmentFlags_AlignRight = 2 { $2 };

constructor TRpSizeInterface.Create(AOwner:TComponent;pritem:TRpCommonComponent);
begin
 inherited Create(AOwner);
 fprintitem:=pritem;
 UpdatePos;
end;

procedure TRpSizeInterface.UpdatePos;
var
 NewWidth,NewHeight:integer;
begin
 NewWidth:=twipstopixels(TRpCOmmonPosComponent(printitem).Width);
 NewHeight:=twipstopixels(TRpCOmmonPosComponent(printitem).Height);
 SetBounds(Left,Top,NewWidth,NewHeight);
end;


procedure TRpSizeInterface.GetProperties(lnames,ltypes,lvalues:TStrings);
begin
 lnames.clear;
 ltypes.clear;
 if Assigned(lvalues) then
  lvalues.Clear;
 // PrintCondition
 lnames.Add(SrpSPrintCondition);
 ltypes.Add(SRpSExpression);
 if Assigned(lvalues) then
  lvalues.Add(printitem.PrintCondition);
 // Before Print
 lnames.Add(SrpSBeforePrint);
 ltypes.Add(SRpSExpression);
 if Assigned(lvalues) then
  lvalues.Add(printitem.DoBeforePrint);
 // After Print
 lnames.Add(SrpSAfterPrint);
 ltypes.Add(SRpSExpression);
 if Assigned(lvalues) then
  lvalues.Add(printitem.DoAfterPrint);


 // Width
 lnames.Add(SrpSWidth);
 ltypes.Add(SRpSCurrency);
 if Assigned(lvalues) then
  lvalues.Add(gettextfromtwips(printitem.Width));
 // Height
 lnames.Add(SrpSHeight);
 ltypes.Add(SRpSCurrency);
 if Assigned(lvalues) then
  lvalues.Add(gettextfromtwips(printitem.Height));
end;

procedure TRpSizeInterface.GetPropertyValues(pname:string;lpossiblevalues:TStrings);
begin
 Raise Exception.Create(SRpPropertyHaveNoListValues+pname);
end;

procedure TRpSizeInterface.SetProperty(pname:string;stream:TMemoryStream);
begin
 Raise Exception.Create(SRpPropertyisnotstream+pname);
end;

procedure TRpSizeInterface.GetProperty(pname:string;var Stream:TMemoryStream);
begin
 Raise Exception.Create(SRpPropertyisnotstream+pname);
end;

procedure TRpSizeInterface.SetProperty(pname:string;value:Widestring);
begin
 if pname=SRpSPrintCOndition then
 begin
  fprintitem.PrintCondition:=Value;
  exit;
 end;
 if pname=SRpSAfterPrint then
 begin
  fprintitem.DoAfterPrint:=Value;
  exit;
 end;
 if pname=SRpSBeforePrint then
 begin
  fprintitem.DoBeforePrint:=Value;
  exit;
 end;
 if pname=SRpSWidth then
 begin
  fprintitem.Width:=gettwipsfromtext(value);
  if fprintitem.Width<0 then
   fprintitem.Width:=0;
  if fprintitem.Width>MAX_CWIDTH then
   fprintitem.Width:=MAX_CWIDTH;
  UpdatePos;
  exit;
 end;
 if pname=SRpSHeight then
 begin
  fprintitem.Height:=gettwipsfromtext(value);
  if fprintitem.Height<0 then
   fprintitem.Height:=0;
  if fprintitem.Height>MAX_CHeight then
   fprintitem.Height:=MAX_CHeight;
  UpdatePos;
  exit;
 end;
 Raise Exception.Create(SRpPropertyNotFound+pname);
end;

function TRpSizeInterface.GetProperty(pname:string):Widestring;
begin
 Result:='';
 if pname=SRpSPrintCondition then
 begin
  Result:=printitem.PrintCondition;
  exit;
 end;
 if pname=SRpSBeforePrint then
 begin
  Result:=printitem.DoBeforePrint;
  exit;
 end;
 if pname=SRpSAfterPrint then
 begin
  Result:=printitem.DoAfterPrint;
  exit;
 end;
 if pname=SRpSWidth then
 begin
  Result:=gettextfromtwips(printitem.Width);
  exit;
 end;
 if pname=SRpSHeight then
 begin
  Result:=gettextfromtwips(printitem.Height);
  exit;
 end;
 Raise Exception.Create(SRpPropertyNotFound+pname);
end;



constructor TRpSizePosInterface.Create(AOwner:TComponent;pritem:TRpCommonComponent);
var
 opts:TControlStyle;
begin
 if Not (pritem is TRpCommonPosComponent) then
  Raise Exception.Create(SRpIncorrectComponentForInterface);
 inherited Create(AOwner,pritem);
 Top:=TRpCommonPosComponent(pritem).PosY;
 Left:=TRpCommonPosComponent(pritem).PosY;
 opts:=ControlStyle;
 include(opts,csCaptureMouse);
 ControlStyle:=opts;
end;

class procedure TRpSizePosInterface.FillAncestors(alist:TStrings);
begin
 alist.clear;
 alist.Add('TRpSizePosInterface');
end;

procedure TRpSizePosInterface.DoSelect;
begin
 MouseDown(mbLeft,[],0,0);
 MouseUp(mbLeft,[],0,0);
end;

function AlignToStr(value:TRpPosAlign):string;
begin
 case value of
  rpalnone:
   REsult:=SRpNone;
  rpalbottom:
   Result:=SRPBottom;
  rpalright:
   Result:=SRPSRight;
  rpalbotright:
   Result:=SRPBottom+'/'+SRpSRight;
 end;
end;

function StrToAlign(value:string):TRpPosAlign;
begin
 Result:=rpalnone;
 if value=SRPBottom then
 begin
  Result:=rpalbottom;
  exit;
 end;
 if value=SRPSRight then
 begin
  Result:=rpalright;
  exit;
 end;
 if value=SRPBottom+'/'+SRpSRight then
 begin
  Result:=rpalbotright;
  exit;
 end;
end;

procedure TRpSizePosInterface.GetProperties(lnames,ltypes,lvalues:TStrings);
begin
 inherited GetProperties(lnames,ltypes,lvalues);

 // Top
 lnames.Add(SrpSTop);
 ltypes.Add(SRpSCurrency);
 if Assigned(lvalues) then
  lvalues.Add(gettextfromtwips(TRpCommonPosComponent(printitem).PosY));
 // Left
 lnames.Add(SrpSLeft);
 ltypes.Add(SRpSCurrency);
 if Assigned(lvalues) then
  lvalues.Add(gettextfromtwips(TRpCommonPosComponent(printitem).PosX));

 lnames.Add(SRPAlign);
 ltypes.Add(SRpSList);
 if Assigned(lvalues) then
  lvalues.Add(AlignToStr(TRpCommonPosComponent(printitem).Align));
end;



procedure TRpSizePosInterface.SetProperty(pname:string;value:Widestring);
begin
 if pname=SRpSTop then
 begin
  TRpCommonPosComponent(fprintitem).PosY:=gettwipsfromtext(value);
  UpdatePos;
  exit;
 end;
 if pname=SRpSLeft then
 begin
  TRpCommonPosComponent(fprintitem).PosX:=gettwipsfromtext(value);
  UpdatePos;
  exit;
 end;
 if pname=SRPAlign then
 begin
  TRpCommonPosComponent(fprintitem).Align:=StrToAlign(Value);
  exit;
 end;
 inherited SetProperty(pname,value);
end;


procedure TRpSizePosInterface.GetPropertyValues(pname:string;
 lpossiblevalues:TStrings);
begin
 if pname=SRpAlign then
 begin
  lpossiblevalues.clear;
  lpossiblevalues.Add(SRpNone);
  lpossiblevalues.Add(SRpBottom);
  lpossiblevalues.Add(SRpSRight);
  lpossiblevalues.Add(SRPBottom+'/'+SRpSRight);
  exit;
 end;
 inherited GetPropertyValues(pname,lpossiblevalues);
end;

function TRpSizePosInterface.GetProperty(pname:string):Widestring;
begin
 Result:='';
 if pname=SRpSTop then
 begin
  Result:=gettextfromtwips(TRpCommonPosComponent(printitem).PosY);
  exit;
 end;
 if pname=SRpSLeft then
 begin
  Result:=gettextfromtwips(TRpCommonPosComponent(printitem).PosX);
  exit;
 end;
 if pname=SRPAlign then
 begin
  Result:=AlignToStr(TRpCommonPosComponent(fprintitem).Align);
  exit;
 end;
 Result:=inherited GetProperty(pname);
end;

procedure TRpSizePosInterface.UpdatePos;
var
 NewLeft,NewTop,NewWidth,NewHeight:integer;
begin
 NewLeft:=twipstopixels(TRpCOmmonPosComponent(printitem).PosX);
 NewWidth:=twipstopixels(TRpCOmmonPosComponent(printitem).Width);
 NewTop:=twipstopixels(TRpCOmmonPosComponent(printitem).PosY);
 NewHeight:=twipstopixels(TRpCOmmonPosComponent(printitem).Height);
 SetBounds(NewLeft,NewTop,NewWidth,NewHeight);
end;

procedure TRpSizeInterface.Paint;
begin
 Canvas.Brush.Style:=bsSolid;
 Canvas.Brush.Color:=clWhite;
 Canvas.Pen.Style:=psDashDot;
 Canvas.Rectangle(0,0,Width,Height);
 Canvas.TextOut(0,0,SRpUndefinedPaintInterface);
 Canvas.TextOut(0,Canvas.TextHeight('gW'),ClassName);
 DrawSelected;
end;

procedure TRpSizeInterface.DrawSelected;
begin
 if Not Selected then
  exit;
 Canvas.Brush.Style:=bsSolid;
 Canvas.Pen.Style:=psSolid;
 Canvas.Pen.Color:=clBtnFace;
 Canvas.Brush.Color:=clBtnFace;
 Canvas.Rectangle(0,0,CONS_SELWIDTH,CONS_SELWIDTH);
 Canvas.Rectangle(0,Height-CONS_SELWIDTH,CONS_SELWIDTH,Height);
 Canvas.Rectangle(Width-CONS_SELWIDTH,Height-CONS_SELWIDTH,Width,Height);
 Canvas.Rectangle(Width-CONS_SELWIDTH,0,Width,CONS_SELWIDTH);
end;

procedure TRpSizeInterface.SetSelected(Value:boolean);
begin
 FSelected:=Value;
 Invalidate;
end;

procedure TRpSizeInterface.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
 inherited MouseDown(Button,Shift,X,Y);

end;

procedure TRpSizeInterface.DblClick;
begin
 inherited DblClick;

end;

constructor TRpBlackControl.Create(AOwner:TComponent);
var
 opts:TControlStyle;
begin
 inherited Create(AOwner);
 Width:=CONS_MODIWIDTH;
 Height:=CONS_MODIWIDTH;
 opts:=ControlStyle;
 Include(opts,csCaptureMouse);
 ControlStyle:=opts;
end;

procedure TRpBlackControl.Paint;
begin
 Canvas.Brush.Color:=clBlack;
 Canvas.Rectangle(0,0,Width,Height);
end;

constructor TRpSizeModifier.Create(AOwner:TComponent);
var
 i:integer;
begin
 inherited Create(AOwner);

 for i:=0 to 3 do
 begin
  FBlacks[i]:=TRpBlackControl.Create(Self);
  FBlacks[i].Tag:=i;
  FBlacks[i].Visible:=false;
 end;
 FBlacks[0].Cursor:=crSizeNWSE;
 FBlacks[1].Cursor:=crSizeNESW;
 FBlacks[2].Cursor:=crSizeNESW;
 FBlacks[3].Cursor:=crSizeNWSE;
end;

procedure TRpSizeModifier.UpdatePos;
var
 i:integer;
begin
 if Not Assigned(FCOntrol) then
 begin
  for i:=0 to 3 do
  begin
   FBlacks[i].Parent:=nil;
  end;
  exit;
 end;
 if Not Assigned(FCOntrol.Parent) then
  exit;
 for i:=0 to 3 do
 begin
  FBlacks[i].Visible:=false;
  FBlacks[i].Parent:=FControl.Parent;
 end;
 if Not Assigned(FControl) then
 begin
  Exit;
 end;
 FBlacks[0].Left:=FControl.Left-CONS_MODIWIDTH div 2;
 FBlacks[0].Top:=FControl.Top-CONS_MODIWIDTH div 2;
 FBlacks[1].Left:=FControl.Left+FControl.Width-CONS_MODIWIDTH div 2;
 FBlacks[1].Top:=FControl.Top-CONS_MODIWIDTH div 2;
 FBlacks[2].Left:=FControl.Left-CONS_MODIWIDTH div 2;
 FBlacks[2].Top:=FControl.Top+FControl.Height-CONS_MODIWIDTH div 2;
 FBlacks[3].Left:=FControl.Left+FControl.Width-CONS_MODIWIDTH div 2;
 FBlacks[3].Top:=FControl.Top+FControl.Height-CONS_MODIWIDTH div 2;
 if FOnlySize then
  FBlacks[3].Visible:=true
 else
 begin
  for i:=0 to 3 do
  begin
   FBlacks[i].Visible:=true;
   FBlacks[i].BringToFront;
  end;
 end;
end;

procedure TRpSizeModifier.SetControl(Value:TControl);
var
 i:integer;
begin
 FControl:=Value;
 for i:=0 to 3 do
 begin
  FBlacks[i].Control:=FControl;
 end;

 UpdatePos;
end;


procedure TRpSizePosInterface.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
 inherited MouseDown(Button,Shift,X,Y);

 if Not Assigned(FRectangle) then
 begin
  FRectangle:=TRpRectangle.Create(Self);
  FRectangle2:=TRpRectangle.Create(Self);
  FRectangle3:=TRpRectangle.Create(Self);
  FRectangle4:=TRpRectangle.Create(Self);

  FRectangle.SetBounds(Left,Top,Width,1);
  FRectangle2.SetBounds(Left,Top+Height,Width,1);
  FRectangle3.SetBounds(Left,Top,1,Height);
  FRectangle4.SetBounds(Left+Width,Top,1,Height);

  FRectangle.Parent:=Parent;
  FRectangle2.Parent:=Parent;
  FRectangle3.Parent:=Parent;
  FRectangle4.Parent:=Parent;
  // It seems a Bug forces me to invalidate section after
  // a aselection
{  if assigned(parent) then
   parent.invalidate;
} end;


 FXOrigin:=X;
 FYOrigin:=Y;
 FBlocked:=True;
end;

procedure TRpSizePosInterface.MouseMove(Shift: TShiftState; X, Y: Integer);
var
 NewLeft,NewTop:integer;
begin
 inherited MouseMove(Shift,X,Y);

 if MouseCapture then
 begin
  if ((Abs(X-FXOrigin)>CONS_MINIMUMMOVE) OR
    (Abs(Y-FYOrigin)>CONS_MINIMUMMOVE)) then
     FBlocked:=False;

  if Assigned(FRectangle) AND (Not FBlocked) then
  begin
   NewLeft:=Left-FXOrigin+X;
   if NewLeft<0 then
    NewLeft:=0;
   NewTop:=Top-FYOrigin+Y;
   if NewTop<0 then
    NewTop:=0;
   if NewLeft+Width>Parent.Width then
    NewLeft:=Parent.Width-Width;
   if NewTop+Height>Parent.Height then
    NewTop:=Parent.Height-Height;
   if NewLeft<0 then
    NewLeft:=0;
   if NewTop<0 then
    NewTop:=0;
   // Align to grid
   if (TRpReport(printitem.Report).GridEnabled) then
   begin
    NewLeft:=AlignToGridPixels(NewLeft,TRpReport(printitem.Report).GridWidth);
    NewTop:=AlignToGridPixels(NewTop,TRpReport(printitem.Report).GridHeight);
   end;
   FRectangle.SetBounds(Newleft,NewTop,Width,1);
   FRectangle2.SetBounds(Newleft,NewTop+Height,Width,1);
   FRectangle3.SetBounds(Newleft,NewTop,1,Height);
   FRectangle4.SetBounds(Newleft+Width,NewTop,1,Height);
   FRectangle.Parent.Update;
  end;
 end;
end;

procedure TRpSizePosInterface.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 NewLeft,NewTop:integer;
begin
 inherited MouseUp(Button,Shift,X,Y);

 if Assigned(FRectangle) then
 begin
  FRectangle.Free;
  FRectangle:=nil;
  FRectangle2.Free;
  FRectangle:=nil;
  FRectangle3.Free;
  FRectangle:=nil;
  FRectangle4.Free;
  FRectangle:=nil;
  // New position
  NewLeft:=Left-FXOrigin+X;
  if NewLeft<0 then
   NewLeft:=0;
  NewTop:=Top-FYOrigin+Y;
  if NewTop<0 then
   NewTop:=0;
  if NewLeft+Width>Parent.Width then
   NewLeft:=Parent.Width-Width;
  if NewTop+Height>Parent.Height then
   NewTop:=Parent.Height-Height;
  // Align to grid
  if (TRpReport(printitem.Report).GridEnabled) then
  begin
   NewLeft:=AlignToGridPixels(NewLeft,TRpReport(printitem.Report).GridWidth);
   NewTop:=AlignToGridPixels(NewTop,TRpReport(printitem.Report).GridHeight);
  end;

  TRpCOmmonPosComponent(printitem).PosX:=pixelstotwips(NewLeft);
  TRpCOmmonPosComponent(printitem).PosY:=pixelstotwips(NewTop);
  UpdatePos;
  if Assigned(fobjinsp) then
   TFRpObjInspVCL(fobjinsp).AddCompItem(Self,Not (ssShift in Shift));
 end;
end;



procedure TRpSizePosInterface.Paint;
begin
 inherited Paint;
end;

{procedure TRpRectangle.Paint;
begin
 Canvas.Pen.Color:=clBlack;
 Canvas.Pen.Style:=psSolid;
 Canvas.Brush.Color:=clWhite;
 if Solid then
  Canvas.Brush.Style:=bsSolid
 else
  Canvas.Brush.Style:=bsClear;
 Canvas.Rectangle(0,0,Width,Height);
end;
}
constructor TRpRectangle.Create(AOwner:TComponent);
var
 opts:TControlStyle;
begin
 inherited Create(AOwner);

// opts:=ControlStyle;
 Color:=clBlack;
// include(opts,csOpaque);
 opts:=[csNoStdEvents];
// ControlStyle:=opts;
end;


procedure TRpBlackControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
 inherited MouseDown(Button,Shift,X,Y);

 if Not Assigned(FRectangle) then
 begin
  FRectangle:=TRpRectangle.Create(Self);
  FRectangle2:=TRpRectangle.Create(Self);
  FRectangle3:=TRpRectangle.Create(Self);
  FRectangle4:=TRpRectangle.Create(Self);

  FRectangle.SetBounds(Left,Top,Width,1);
  FRectangle2.SetBounds(Left,Top+Height,Width,1);
  FRectangle3.SetBounds(Left,Top,1,Height);
  FRectangle4.SetBounds(Left+Width,Top,1,Height);

  FRectangle.Parent:=Parent;
  FRectangle2.Parent:=Parent;
  FRectangle3.Parent:=Parent;
  FRectangle4.Parent:=Parent;
 end;


 FXOrigin:=X;
 FYOrigin:=Y;
end;


procedure TRpBlackControl.CalcNewCoords(var NewLeft,
 NewTop,NewWidth,NewHeight,X,Y:integer);
begin
  // Depending on tag must do different coordinates
  case Tag of
   0:
    begin
     NewLeft:=Control.Left-FXOrigin+X;
     NewTop:=Control.Top-FYOrigin+Y;
     // Align to grid
     if TRpSizeModifier(Owner).GridEnabled then
     begin
      NewLeft:=AlignToGridPixels(NewLeft,TRpSizeModifier(Owner).GridX);
      NewTop:=AlignToGridPixels(NewTop,TRpSizeModifier(Owner).GridY);
     end;
     // It mantains the bottom corner position
     if NewTop>Control.Top+Control.Height then
     begin
      NewHeight:=NewTop-(Control.Top+Control.Height);
      NewTop:=Control.Top+Control.Height;
     end
     else
     begin
      if NewTop<0 then
       NewTop:=0;
      NewHeight:=Control.Height+(Control.Top-NewTop);
     end;

     if NewLeft>Control.Left+Control.Width then
     begin
      NewWidth:=NewLeft-(Control.Left+Control.Width);
      NewLeft:=Control.Left+Control.Width;
     end
     else
     begin
      if NewLeft<0 then
       NewLeft:=0;
      NewWidth:=Control.Width+(Control.Left-NewLeft);
     end;
    end;
   1:
    begin
     NewLeft:=Control.Left;
     NewWidth:=Control.Width-FXOrigin+X;
     if TRpSizeModifier(Owner).GridEnabled then
      NewWidth:=AlignToGridPixels(NewLeft+NewWidth,TRpSizeModifier(Owner).GridX)-NewLeft;
     NewTop:=Control.Top-FYOrigin+Y;

     // Align to grid
     if TRpSizeModifier(Owner).GridEnabled then
     begin
      NewTop:=AlignToGridPixels(NewTop,TRpSizeModifier(Owner).GridY);
     end;
     // It mantains the bottom corner position
     if NewTop>Control.Top+Control.Height then
     begin
      NewHeight:=NewTop-(Control.Top+Control.Height);
      NewTop:=Control.Top+Control.Height;
     end
     else
     begin
      if NewTop<0 then
       NewTop:=0;
      NewHeight:=Control.Height+(Control.Top-NewTop);
     end;

     if NewWidth<0 then
     begin
      NewWidth:=-NewWidth;
      if NewWidth>Control.Left then
       NewWidth:=Control.Left;
      NewLeft:=Control.Left-NewWidth;
     end
    end;
   2:
    begin
     NewLeft:=Control.Left-FXOrigin+X;
     NewHeight:=Control.Height-FYOrigin+Y;
     NewTop:=Control.Top;
     if TRpSizeModifier(Owner).GridEnabled then
      NewHeight:=AlignToGridPixels(NewTop+NewHeight,TRpSizeModifier(Owner).GridY)-NewTop;

     // Align to grid
     if TRpSizeModifier(Owner).GridEnabled then
     begin
      NewLeft:=AlignToGridPixels(NewLeft,TRpSizeModifier(Owner).GridX);
     end;

     if NewHeight<0 then
     begin
      NewHeight:=-NewHeight;
      if NewHeight>Control.Top then
       NewHeight:=Control.Top;
      NewTop:=Control.Top-NewHeight;
     end;

     if NewLeft>Control.Left+Control.Width then
     begin
      NewWidth:=NewLeft-(Control.Left+Control.Width);
      NewLeft:=Control.Left+Control.Width;
     end
     else
     begin
      if NewLeft<0 then
       NewLeft:=0;
      NewWidth:=Control.Width+(Control.Left-NewLeft);
     end;

    end;
   3:
    begin
     NewHeight:=Control.Height-FYOrigin+Y;
     NewTop:=Control.Top;
     NewLeft:=Control.Left;
     NewWidth:=Control.Width-FXOrigin+X;
     if TRpSizeModifier(Owner).GridEnabled then
     begin
      NewWidth:=AlignToGridPixels(NewLeft+NewWidth,TRpSizeModifier(Owner).GridX)-NewLeft;
      NewHeight:=AlignToGridPixels(NewTop+NewHeight,TRpSizeModifier(Owner).GridY)-NewTop;
     end;
     if NewHeight<0 then
     begin
      NewHeight:=-NewHeight;
      if NewHeight>Control.Top then
       NewHeight:=Control.Top;
      NewTop:=Control.Top-NewHeight;
     end;
     if NewWidth<0 then
     begin
      NewWidth:=-NewWidth;
      if NewWidth>Control.Left then
       NewWidth:=Control.Left;
      NewLeft:=Control.Left-NewWidth;
     end

    end;
  end;
  if NewLeft<0 then
    NewLeft:=0;
  if NewTop<0 then
   NewTop:=0;
  if Not FAllowOverSize then
  begin
   if NewLeft+NewWidth>Parent.Width then
    NewWidth:=Parent.Width-NewLeft-1;
   if NewTop+NewHeight>Parent.Height then
    NewHeight:=Parent.Height-NewTop-1;
  end;
end;


procedure TRpBlackControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var NewLeft,
 NewTop,NewWidth,NewHeight:integer;
begin
 inherited MouseMove(Shift,X,Y);

 if Not Assigned(FControl) then
  exit;
 if MouseCapture then
 begin
  if (Not Assigned(FRectangle)) then
   exit;
  CalcNewCoords(NewLeft,NewTop,NewWidth,NewHeight,X,Y);

  FRectangle.SetBounds(Newleft,NewTop,NewWidth,1);
  FRectangle2.SetBounds(Newleft,NewTop+NewHeight,NewWidth,1);
  FRectangle3.SetBounds(Newleft,NewTop,1,NewHeight);
  FRectangle4.SetBounds(Newleft+NewWidth,NewTop,1,NewHeight);
  FRectangle.Parent.Update;
 end;
end;

procedure TRpBlackControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 NewLeft,NewTop,NewWidth,NewHeight:integer;
begin
 inherited MouseUp(Button,Shift,X,Y);

 if Assigned(FRectangle) then
 begin
  FRectangle.Free;
  FRectangle:=nil;
  FRectangle2.Free;
  FRectangle:=nil;
  FRectangle3.Free;
  FRectangle:=nil;
  FRectangle4.Free;
  FRectangle:=nil;

  CalcNewCoords(NewLeft,NewTop,NewWidth,NewHeight,X,Y);

  if NewWidth<CONS_MINWIDTH then
   NewWidth:=CONS_MINWIDTH;
  if NewHeight<CONS_MINHEIGHT then
   NewHeight:=CONS_MINHEIGHT;

  // New position
  Control.SetBounds(NewLeft,NewTop,NewWidth,NewHeight);
  if Assigned(TRpSizeModifier(Owner).OnSizeChange) then
   TRpSizeModifier(Owner).OnSizeChange(Owner);
  TRpSizeModifier(Owner).UpdatePos;
  if Control is TRpSizePosInterface then
  begin
   if Assigned(TRpSizePosInterface(Control).fobjinsp) then
    TFRpObjInspVCL(TRpSizePosInterface(Control).fobjinsp).AddCompItem(TRpSizePosInterface(Control),true);
  end;
 end;
end;

procedure TRpSizeModifier.SetOnlySize(Value:Boolean);
begin
 FOnlySize:=Value;
 Control:=FControl;
end;

procedure TRpSizeModifier.SetAllowOverSize(Value:Boolean);
var
 i:integer;
begin
 FAllowOverSize:=Value;
 for i:=0 to 3 do
 begin
  FBlacks[i].FAllowOverSize:=Value;
 end;
end;


constructor TRpGenTextInterface.Create(AOwner:TComponent;pritem:TRpCommonComponent);
begin
 if Not (pritem is TRpGenTextComponent) then
  Raise Exception.Create(SRpIncorrectComponentForInterface);
 inherited Create(AOwner,pritem);
end;

class procedure TRpGenTextInterface.FillAncestors(alist:TStrings);
begin
 inherited FillAncestors(alist);
 alist.Add('TRpGenTextInterface');
end;

function HAlignmentToText(value:integer):string;
begin
 Result:=SRpSAlignNone;
 if (value=integer(AlignmentFlags_AlignLeft)) then
 begin
  Result:=SRpSAlignLeft;
 end;
 if (value=integer(AlignmentFlags_AlignRight)) then
 begin
  Result:=SRpSAlignRight;
 end;
 if (value=integer(AlignmentFlags_AlignHCenter)) then
 begin
  Result:=SRpSAlignCenter;
 end;
end;

function VAlignmentToText(value:integer):string;
begin
 Result:=SRpSAlignNone;
 if (value=integer(AlignmentFlags_AlignTop)) then
 begin
  Result:=SRpSAlignTop;
 end;
 if (value=integer(AlignmentFlags_AlignBottom)) then
 begin
  Result:=SRpSAlignBottom;
 end;
 if (value=integer(AlignmentFlags_AlignVCenter)) then
 begin
  Result:=SRpSAlignCenter;
 end;
end;

function StringVAlignmentToInt(value:string):integer;
begin
 Result:=0;
 if (value=SRpSAlignTop) then
 begin
  Result:=integer(AlignmentFlags_AlignTop);
 end;
 if (value=SRpSAlignBottom) then
 begin
  Result:=integer(AlignmentFlags_AlignBottom);
 end;
 if (value=SRpSAlignCenter) then
 begin
  Result:=integer(AlignmentFlags_AlignVCenter);
 end;
end;

function StringHAlignmentToInt(value:string):integer;
begin
 Result:=0;
 if (value=SRpSAlignLeft) then
 begin
  Result:=integer(AlignmentFlags_AlignLeft);
 end;
 if (value=SRpSAlignRight) then
 begin
  Result:=integer(AlignmentFlags_AlignRight);
 end;
 if (value=SRpSAlignCenter) then
 begin
  Result:=integer(AlignmentFlags_AlignHCenter);
 end;
end;

function Type1FontToText(value:TRpType1Font):string;
begin
 case value of
  poHelvetica:
   Result:='Helvetica';
  poCourier:
   Result:='Courier';
  poTimesRoman:
   Result:='Times Roman';
  poSymbol:
   Result:='Symbol';
  poZapfDingbats:
   Result:='ZapfDingbats';
 end;
end;

function TextToType1Font(value:string):TRpType1Font;
begin
 Result:=poHelvetica;
 if value='Helvetica' then
 begin
  Exit;
 end;
 if value='Courier' then
 begin
  Result:=poCourier;
  Exit;
 end;
 if value='Times Roman' then
 begin
  Result:=poTimesRoman;
  Exit;
 end;
 if value='Symbol' then
 begin
  Result:=poSymbol;
  Exit;
 end;
 if value='ZapfDingbats' then
 begin
  Result:=poZapfDingbats;
  Exit;
 end;
end;

procedure TRpGenTextInterface.GetProperties(lnames,ltypes,lvalues:TStrings);
begin
 inherited GetProperties(lnames,ltypes,lvalues);


 // Alignment
 lnames.Add(SrpSAlignment);
 ltypes.Add(SRpSList);
 if Assigned(lvalues) then
  lvalues.Add(HAlignmentToText(TRpGenTextComponent(printitem).Alignment));

 // VAlignment
 lnames.Add(SrpSVAlignment);
 ltypes.Add(SRpSList);
 if Assigned(lvalues) then
  lvalues.Add(VAlignmentToText(TRpGenTextComponent(printitem).VAlignment));

 // Font Name
 lnames.Add(SrpSWFontName);
 ltypes.Add(SRpSWFontName);
 if Assigned(lvalues) then
  lvalues.Add(TRpGenTextComponent(printitem).WFontName);

 // Linux Font Name
 lnames.Add(SrpSLFontName);
 ltypes.Add(SRpSLFontName);
 if Assigned(lvalues) then
  lvalues.Add(TRpGenTextComponent(printitem).LFontName);

 // Type1 Font Name
 lnames.Add(SRpSType1Font);
 ltypes.Add(SRpSList);
 if Assigned(lvalues) then
  lvalues.Add(Type1FontToText(TRpGenTextComponent(printitem).Type1Font));


 // Font Size
 lnames.Add(SrpSFontSize);
 ltypes.Add(SRpSFontSize);
 if Assigned(lvalues) then
  lvalues.Add(IntToStr(TRpGenTextComponent(printitem).FontSize));

 // Font Color
 lnames.Add(SrpSFontColor);
 ltypes.Add(SRpSColor);
 if Assigned(lvalues) then
  lvalues.Add(IntToStr(TRpGenTextComponent(printitem).FontColor));

 // Font Style
 lnames.Add(SrpSFontStyle);
 ltypes.Add(SrpSFontStyle);
 if Assigned(lvalues) then
  lvalues.Add(IntToStr(TRpGenTextComponent(printitem).FontStyle));

 // Right To Left
 lnames.Add(SrpSRightToLeft);
 ltypes.Add(SRpSList);
 if Assigned(lvalues) then
  lvalues.Add(RpBidiModeToString(TRpGenTextComponent(printitem).BidiMode));

 // Back Color
 lnames.Add(SrpSBackColor);
 ltypes.Add(SRpSColor);
 if Assigned(lvalues) then
  lvalues.Add(IntToStr(TRpGenTextComponent(printitem).BackColor));

 // Transparent
 lnames.Add(SrpSTransparent);
 ltypes.Add(SRpSBool);
 if Assigned(lvalues) then
  lvalues.Add(BoolToStr(TRpGenTextComponent(printitem).Transparent,true));

 // Cut Text
 lnames.Add(SrpSCutText);
 ltypes.Add(SRpSBool);
 if Assigned(lvalues) then
  lvalues.Add(BoolToStr(TRpGenTextComponent(printitem).CutText,true));

 // Work wrap
 lnames.Add(SrpSWordwrap);
 ltypes.Add(SRpSBool);
 if Assigned(lvalues) then
  lvalues.Add(BoolToStr(TRpGenTextComponent(printitem).WordWrap,true));

 // Single line
 lnames.Add(SrpSSingleLine);
 ltypes.Add(SRpSBool);
 if Assigned(lvalues) then
  lvalues.Add(BoolToStr(TRpGenTextComponent(printitem).SingleLine,true));

 // Font Rotation in degrees
 lnames.Add(SRpSFontRotation);
 ltypes.Add(SrpSString);
 if Assigned(lvalues) then
  lvalues.Add(FormatCurr('#####0.0',TRpGenTextComponent(printitem).FontRotation/10));
end;

procedure TRpGenTextInterface.SetProperty(pname:string;value:Widestring);
begin
 if pname=SRpSAlignment then
 begin
  TRpGenTextComponent(fprintitem).Alignment:=StringHAlignmentToInt(Value);
  Invalidate;
  exit;
 end;
 if pname=SRpSVAlignment then
 begin
  TRpGenTextComponent(fprintitem).VAlignment:=StringVAlignmentToInt(Value);
  Invalidate;
  exit;
 end;
 if pname=SRpSWFontName then
 begin
  TRpGenTextComponent(fprintitem).WFontName:=value;
  Invalidate;
  exit;
 end;
 if pname=SRpSLFontName then
 begin
  TRpGenTextComponent(fprintitem).LFontName:=value;
  Invalidate;
  exit;
 end;
 if pname=SRpSType1Font then
 begin
  TRpGenTextComponent(fprintitem).Type1Font:=TextToType1Font(value);
  exit;
 end;
 if pname=SRpSFontSize then
 begin
  TRpGenTextComponent(fprintitem).FontSize:=StrToInt(value);
  Invalidate;
  exit;
 end;
 if pname=SRpSFontStyle then
 begin
  TRpGenTextComponent(fprintitem).FontStyle:=StrToInt(value);
  Invalidate;
  exit;
 end;
 if pname=SRpSFontColor then
 begin
  TRpGenTextComponent(fprintitem).FontColor:=StrToInt(value);
  Invalidate;
  exit;
 end;
 if pname=SRpSRightToLeft then
 begin
  TRpGenTextComponent(fprintitem).BidiMode:=StringToRpBidiMode(value);
  Invalidate;
  exit;
 end;
 if pname=SRpSBackColor then
 begin
  TRpGenTextComponent(fprintitem).BackColor:=StrToInt(value);
  Invalidate;
  exit;
 end;
 if pname=SRpSTransParent then
 begin
  TRpGenTextComponent(fprintitem).Transparent:=StrToBool(value);
  Invalidate;
  exit;
 end;
 if pname=SRpSCutText then
 begin
  TRpGenTextComponent(fprintitem).CutText:=StrToBool(value);
  Invalidate;
  exit;
 end;
 if pname=SRpSWordWrap then
 begin
  TRpGenTextComponent(fprintitem).WordWrap:=StrToBool(value);
  Invalidate;
  exit;
 end;
 if pname=SRpSSingleLine then
 begin
  TRpGenTextComponent(fprintitem).SingleLine:=StrToBool(value);
  Invalidate;
  exit;
 end;
 if pname=SRpSFontRotation then
 begin
  TRpGenTextComponent(fprintitem).FontRotation:=Round(StrToCurr(Value)*10);
  exit;
 end;

 inherited SetProperty(pname,value);
end;

function TRpGenTextInterface.GetProperty(pname:string):Widestring;
begin
 Result:='';
 if pname=SrpSAlignMent then
 begin
  Result:=HAlignmentToText(TRpGenTextComponent(printitem).AlignMent);
  exit;
 end;
 if pname=SrpSVAlignMent then
 begin
  Result:=VAlignmentToText(TRpGenTextComponent(printitem).VAlignMent);
  exit;
 end;
 if pname=SrpSWFontName then
 begin
  Result:=TRpGenTextComponent(printitem).WFontName;
  exit;
 end;
 if pname=SrpSLFontName then
 begin
  Result:=TRpGenTextComponent(printitem).LFontName;
  exit;
 end;
 if pname=SRpSType1Font then
 begin
  Result:=Type1FontToText(TRpGenTextComponent(printitem).Type1FOnt);
  exit;
 end;
 if pname=SrpSFontSize then
 begin
  Result:=IntToStr(TRpGenTextComponent(printitem).FontSize);
  exit;
 end;
 if pname=SrpSFontStyle then
 begin
  Result:=IntToStr(TRpGenTextComponent(printitem).FontStyle);
  exit;
 end;
 if pname=SrpSFontColor then
 begin
  Result:=IntToStr(TRpGenTextComponent(printitem).FontColor);
  exit;
 end;
 if pname=SrpSRightToLeft then
 begin
  Result:=RpBidiModeToString(TRpGenTextComponent(printitem).BidiMode);
  exit;
 end;
 if pname=SrpSBackColor then
 begin
  Result:=IntToStr(TRpGenTextComponent(printitem).BackColor);
  exit;
 end;
 if pname=SrpSTransparent then
 begin
  Result:=BoolToStr(TRpGenTextComponent(printitem).Transparent,true);
  exit;
 end;
 if pname=SrpSCutText then
 begin
  Result:=BoolToStr(TRpGenTextComponent(printitem).CutText,true);
  exit;
 end;
 if pname=SRpSFontRotation then
 begin
  Result:=FormatCurr('#####0.0',TRpGenTextComponent(printitem).FontRotation/10);
  exit;
 end;
 if pname=SrpSWordWrap then
 begin
  Result:=BoolToStr(TRpGenTextComponent(printitem).WordWrap,true);
  exit;
 end;
 if pname=SrpSSingleLine then
 begin
  Result:=BoolToStr(TRpGenTextComponent(printitem).SingleLine,true);
  exit;
 end;
 Result:=inherited GetProperty(pname);
end;


procedure TRpGenTextInterface.GetPropertyValues(pname:string;lpossiblevalues:TStrings);
begin
 if pname=SrpSRightToLeft then
 begin
  GetBidiDescriptions(lpossiblevalues);
  exit;
 end;
 if pname=SrpSAlignment then
 begin
  lpossiblevalues.clear;
  lpossiblevalues.Add(SrpSAlignNone);
  lpossiblevalues.Add(SrpSAlignLeft);
  lpossiblevalues.Add(SrpSAlignRight);
  lpossiblevalues.Add(SrpSAlignCenter);
  exit;
 end;
 if pname=SrpSVAlignment then
 begin
  lpossiblevalues.clear;
  lpossiblevalues.Add(SrpSAlignNone);
  lpossiblevalues.Add(SrpSAlignTop);
  lpossiblevalues.Add(SrpSAlignBottom);
  lpossiblevalues.Add(SrpSAlignCenter);
  exit;
 end;
 if pname=SRpSType1Font then
 begin
  lpossiblevalues.clear;
  lpossiblevalues.Add('Helvetica');
  lpossiblevalues.Add('Courier');
  lpossiblevalues.Add('Times Roman');
  lpossiblevalues.Add('Symbol');
  lpossiblevalues.Add('ZapfDingbats');
  exit;
 end;
 inherited GetPropertyValues(pname,lpossiblevalues);
end;


initialization


end.
