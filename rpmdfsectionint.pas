{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       Rpmdsectionint                                  }
{       Implementation of section designer              }
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

unit rpmdfsectionint;

interface

uses SysUtils, Classes, QGraphics, QForms,Types,
  QButtons, QExtCtrls, QControls, QStdCtrls,
  rpmdobinsint,rpreport,rpprintitem,rpgraphutils,
  rpmdobjinsp,rpmdfstruc,rpmdflabelint,rplabelitem,
  rpconsts,rpsection,rptypes,rpdrawitem,rpmdfdrawint,
  rpsubreport;


type

  TRpSectionInterface=class;

  TRpSectionIntf=class(TCustomControl)
   private
    calledposchange:Boolean;
    secint:TRpSectionInterface;
   protected
    procedure Paint;override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
   public
    OnPosChange:TNotifyEvent;
    constructor Create(AOwner:TComponent);override;

   end;

  TRpSectionInterface=class(TRpSizeInterface)
   private
    FOnDestroy:TNotifyEvent;
    FInterface:TRpSectionIntf;
    FOnPosChange:TNotifyEvent;
    procedure SetOnPosChange(AValue:TNotifyEvent);
   protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
   public
    freportstructure:TFRpStructure;
    childlist:TList;
    procedure UpdatePos;override;
    property OnDestroy:TNotifyEvent read FOnDestroy write FOnDestroy;
    constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);override;
    destructor destroy;override;
    procedure GetProperties(lnames,ltypes,lvalues:TStrings);override;
    procedure SetProperty(pname:string;value:string);override;
    function GetProperty(pname:string):string;override;
    procedure CreateChilds;
    procedure InvalidateAll;
    property OnPosChange:TNotifyEvent read FOnPosChange write SetOnPosChange;
    procedure DoDeleteComponent(aitem:TComponent);
  end;



procedure FreeGridBitmap;

implementation

uses rpmdfmain, rpmdfdesign;


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
 FInterface:=TRpSectionIntf.Create(Self);
 Visible:=false;
 FInterface.secint:=Self;
end;

destructor TRpSectionInterface.destroy;
begin
 if Assigned(FOnDestroy) then
 begin
  FOnDestroy(Self);
 end;
 childlist.free;
 inherited destroy;
end;


procedure TRpSectionInterface.SetOnPosChange(AValue:TNotifyEvent);
begin
 FOnPosChange:=AValue;
 FInterface.OnPosChange:=AValue;
end;


procedure TRpSectionInterface.GetProperties(lnames,ltypes,lvalues:TStrings);
begin
 inherited GetProperties(lnames,ltypes,lvalues);

 lnames.Add(SRpSAutoExpand);
 ltypes.Add(SRpSBool);
 lvalues.Add(BoolToStr(TRpSection(printitem).AutoExpand,true));

 lnames.Add(SRpSAutoContract);
 ltypes.Add(SRpSBool);
 lvalues.Add(BoolToStr(TRpSection(printitem).AutoContract,true));

 if (TrpSection(printitem).SectionType in [rpsecgheader,rpsecgfooter]) then
 begin
  lnames.Add(SRpSGroupName);
  ltypes.Add(SRpSString);
  lvalues.Add(TRpSection(printitem).GroupName);

  lnames.Add(SRpSGroupExpression);
  ltypes.Add(SRpSExpression);
  lvalues.Add(TRpSection(printitem).ChangeExpression);

  lnames.Add(SRpSChangeBool);
  ltypes.Add(SRpSBool);
  lvalues.Add(BoolToStr(TRpSection(printitem).ChangeBool,true));

  if TrpSection(printitem).SectionType=rpsecgheader then
  begin
   lnames.Add(SRpSPageRepeat);
   ltypes.Add(SRpSBool);
   lvalues.Add(BoolToStr(TRpSection(printitem).PageRepeat,true));
  end;
 end;
 if (TrpSection(printitem).SectionType in [rpsecgheader,rpsecgfooter,rpsecdetail]) then
 begin
  lnames.Add(SRpSBeginPage);
  ltypes.Add(SRpSExpression);
  lvalues.Add(TRpSection(printitem).BeginPageExpression);

  lnames.Add(SRpSkipPage);
  ltypes.Add(SRpSBool);
  lvalues.Add(BoolToStr(TRpSection(printitem).SkipPage,true));

  lnames.Add(SRPAlignBottom);
  ltypes.Add(SRpSBool);
  lvalues.Add(BoolToStr(TRpSection(printitem).AlignBottom,true));

  lnames.Add(SRPHorzDesp);
  ltypes.Add(SRpSBool);
  lvalues.Add(BoolToStr(TRpSection(printitem).HorzDesp,true));

 end;
end;

procedure TRpSectionInterface.SetProperty(pname:string;value:string);
begin
 if length(value)<1 then
  exit;
 if pname=SRpSAutoExpand then
 begin
  TRpSection(fprintitem).Autoexpand:=StrToBool(Value);
  exit;
 end;
 if pname=SRpSAutoContract then
 begin
  TRpSection(fprintitem).AutoContract:=StrToBool(Value);
  exit;
 end;
 if (TrpSection(printitem).SectionType in [rpsecgheader,rpsecgfooter]) then
 begin
  if pname=SRpSGroupName then
  begin
   TRpSection(fprintitem).groupname:=Value;
   exit;
  end;
  if pname=SRpSGroupExpression then
  begin
   TRpSection(fprintitem).ChangeExpression:=Value;
   exit;
  end;
  if pname=SRpSChangeBool then
  begin
   TRpSection(fprintitem).ChangeBool:=StrToBool(Value);
   exit;
  end;
  if pname=SRPSPageRepeat then
  begin
   TRpSection(fprintitem).PageRepeat:=StrToBool(Value);
   exit;
  end;
 end;
 if (TrpSection(printitem).SectionType in [rpsecgheader,rpsecgfooter,rpsecdetail]) then
 begin
  if pname=SRPSBeginPage then
  begin
   TRpSection(fprintitem).BeginPageExpression:=Value;
   exit;
  end;
  if pname=SRPSkipPage then
  begin
   TRpSection(fprintitem).SkipPage:=StrToBool(Value);
   exit;
  end;
  if pname=SRPAlignBottom then
  begin
   TRpSection(fprintitem).AlignBottom:=StrToBool(Value);
   exit;
  end;
  if pname=SRpHorzDesp then
  begin
   TRpSection(fprintitem).HorzDesp:=StrToBool(Value);
   exit;
  end;
 end;
 inherited SetProperty(pname,value);
end;

function TRpSectionInterface.GetProperty(pname:string):string;
begin
 if pname=SRpSAutoContract then
 begin
  Result:=BoolToStr(TRpSection(fprintitem).AutoContract,true);
  exit;
 end;
 if pname=SRpSAutoExpand then
 begin
  Result:=BoolToStr(TRpSection(fprintitem).AutoExpand,true);
  exit;
 end;
 if (TrpSection(printitem).SectionType in [rpsecgheader,rpsecgfooter]) then
 begin
  if pname=SRpSGroupName then
  begin
   Result:=TRpSection(fprintitem).groupname;
   exit;
  end;
  if pname=SRpSGroupExpression then
  begin
   Result:=TRpSection(fprintitem).ChangeExpression;
   exit;
  end;
  if pname=SRpSChangeBool then
  begin
   Result:=BoolToStr(TRpSection(fprintitem).ChangeBool,true);
   exit;
  end;
  if pname=SRpSPageRepeat then
  begin
   Result:=BoolToStr(TRpSection(fprintitem).PageRepeat,true);
   exit;
  end;
 end;
 if (TrpSection(printitem).SectionType in [rpsecgheader,rpsecgfooter,rpsecdetail]) then
 begin
  if pname=SRPSBeginPage then
  begin
   Result:=TRpSection(fprintitem).BeginPageExpression;
   exit;
  end;
  if pname=SRPSkipPage then
  begin
   Result:=BoolToStr(TRpSection(fprintitem).SkipPage,true);
   exit;
  end;
  if pname=SRPAlignBottom then
  begin
   Result:=BoolToStr(TRpSection(fprintitem).AlignBottom,true);
   exit;
  end;
  if pname=SRPHorzDesp then
  begin
   Result:=BoolToStr(TRpSection(fprintitem).HorzDesp,true);
   exit;
  end;
 end;
 Result:=inherited GetProperty(pname);
end;


constructor TRpSectionIntf.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

end;


procedure TRpSectionIntf.Paint;
var
 report:TRpReport;
 rec:TRect;
 bitmap:TBitmap;
begin
 if not assigned(secint) then
  exit;
 // Bug in WINDOWS when changing the size of a section
 // Remove this when clx updated
 if not calledposchange then
 begin
  calledposchange:=true;
  try
   if Assigned(OnPosChange) then
    OnPosChange(Self);
  finally
   calledposchange:=false;
  end;
 end;
 if Not Assigned(secint.fprintitem) then
  exit;
 if Not Assigned(secint.fprintitem.Owner) then
  exit;
 if Not (secint.fprintitem.Owner is TRpReport) then
  exit;
 report:=TRpReport(secint.fprintitem.Owner);
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

procedure TRpSectionIntf.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
 secint.MouseDown(Button,Shift,X,Y);
end;


procedure TRpSectionInterface.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
var
 asizeposint:TRpSizePosInterface;
 asizepos:TRpCommonPosComponent;
 aitem:TRpCommonListItem;
 FRpMainf:TFRpMainF;
begin
 inherited MouseDown(Button,Shift,X,Y);

 FRpMainf:=TFRpMainF(Owner.Owner);
 // There's a selected item insert it
 if FRpMainf.BArrow.Down then
 begin
  // Selects object inspector section properties
  freportstructure.SelectDataItem(printitem);
  TFRpObjInsp(fobjinsp).CompItem:=self;
  exit;
 end;
 asizepos:=nil;
 asizeposint:=nil;
 if FRpMainf.BLabel.Down then
 begin
  asizepos:=TRpLabel.Create(printitem.Owner);
  TRpLabel(asizepos).Text:=SRpSampleTextToLabels;
  asizeposint:=TRpLabelInterface.Create(Self,asizepos);
 end;
 if FRpMainf.BExpression.Down then
 begin
  asizepos:=TRpExpression.Create(printitem.Owner);
  // Search if theres a selected field
  TRpExpression(asizepos).Expression:=FRpMainf.GetExpressionText;
  asizeposint:=TRpExpressionInterface.Create(Self,asizepos);
 end;
 if FRpMainf.BShape.Down then
 begin
  asizepos:=TRpShape.Create(printitem.Owner);
  asizeposint:=TRpDrawInterface.Create(Self,asizepos);
 end;
 if FRpMainf.BImage.Down then
 begin
  asizepos:=TRpImage.Create(printitem.Owner);
  asizeposint:=TRpImageInterface.Create(Self,asizepos);
 end;


 if Assigned(asizepos) then
 begin
  if TRpReport(printitem.Owner).GridEnabled then
  begin
   asizepos.PosX:=pixelstotwips(AlignToGridPixels(X,TRpReport(printitem.Owner).GridWidth));
   asizepos.PosY:=pixelstotwips(AlignToGridPixels(Y,TRpReport(printitem.Owner).GridHeight));
  end
  else
  begin
   asizepos.PosX:=pixelstotwips(X);
   asizepos.PosY:=pixelstotwips(Y);
  end;
  GenerateNewName(asizepos);
  aitem:=TRpSection(printitem).Components.Add;
  aitem.Component:=asizepos;
  asizeposint.Parent:=FInterface;
  asizeposint.sectionint:=self;
  asizeposint.UpdatePos;
  asizeposint.fobjinsp:=fobjinsp;
  childlist.Add(asizeposint);
  if assigned(TFRpObjInsp(fobjinsp).Combo) then
  begin
   TFRpObjInsp(fobjinsp).Combo.Items.AddObject(asizepos.Name,asizeposint);
   TFRpObjInsp(fobjinsp).Combo.ItemIndex:=TFRpObjInsp(fobjinsp).Combo.Items.IndexOfObject(asizeposint);
  end;
  TFRpObjInsp(fobjinsp).CompItem:=asizeposint;
  if (Not (SSShift in Shift)) then
   FRpMainf.BArrow.Down:=true;
 end;


end;

procedure TRpSectionInterface.CreateChilds;
var
 sec:TRpSection;
 i:integer;
 compo:TRpCommonPosComponent;
 labelint:TRpSizePosInterface;
begin
 sec:=TRpSection(printitem);
 for i:=0 to sec.Components.Count-1 do
 begin
  compo:=TRpCommonPosComponent(sec.Components.Items[i].Component);
  labelint:=nil;
  if compo is TRpLabel then
  begin
   labelint:=TRpLabelInterface.Create(Self,compo);
  end;
  if compo is TRpExpression then
  begin
   labelint:=TRpExpressionInterface.Create(Self,compo);
  end;
  if compo is TRpShape then
  begin
   labelint:=TRpDrawInterface.Create(Self,compo);
  end;
  if compo is TRpImage then
  begin
   labelint:=TRpImageInterface.Create(Self,compo);
  end;
  if Assigned(labelint) then
  begin
   labelint.Parent:=FInterface;
   labelint.sectionint:=self;
   labelint.UpdatePos;
   labelint.fobjinsp:=fobjinsp;
   childlist.Add(labelint)
  end;
 end;
end;

procedure TRpSectionInterface.InvalidateAll;
var
 i:integer;
begin
 for i:=0 to childlist.count-1 do
 begin
  TRpSizeInterface(childlist.items[i]).Invalidate;
 end;
end;

procedure TRpSectionInterface.UpdatePos;
begin
 inherited UpdatePos;

 if Assigned(FInterface) then
 begin
  FInterface.SetBounds(Left,Top,Width,Height);
  FInterface.Parent:=Parent;
 end;
end;


procedure TRpSectionInterface.DoDeleteComponent(aitem:TComponent);
var
 i:integer;
begin
 for i:=0 to childlist.Count-1 do
 begin
  if TRpSizePosInterface(childlist.items[i]).printitem=aitem then
  begin
   TRpSizePosInterface(childlist.items[i]).Parent:=nil;
   TRpSizePosInterface(childlist.items[i]).Free;
   TRpSection(printitem).DeleteComponent(TRpCommonCOmponent(aitem));
  end;
 end;
end;


initialization
fbitmap:=nil;
fbwidth:=0;
fbheight:=0;
fcolor:=clBlack;

end.
