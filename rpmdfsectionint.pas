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

{$I rpconf.inc}

uses SysUtils, Classes,Types,
  QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,
  rpmdobinsint,rpreport,rpprintitem,rpgraphutils,
  rpmdobjinsp,rpmdfstruc,rpmdflabelint,rplabelitem,
  rpmdconsts,rpsection,rptypes,rpdrawitem,rpmdfdrawint,
  rpsubreport,rpmdbarcode,rpmdchart,
  rpmdfbarcodeint,rpmdfchartint;

const
 CONS_MINWIDTH=5;
 CONS_MINHEIGHT=5;
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
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
   public
    OnPosChange:TNotifyEvent;
    constructor Create(AOwner:TComponent);override;
   end;

  TRpSectionInterface=class(TRpSizeInterface)
   private
    FOnDestroy:TNotifyEvent;
    FInterface:TRpSectionIntf;
    FOnPosChange:TNotifyEvent;
    FXOrigin,FYOrigin:integer;
    FRectangle:TRpRectangle;
    FRectangle2:TRpRectangle;
    FRectangle3:TRpRectangle;
    FRectangle4:TRpRectangle;
    procedure SetOnPosChange(AValue:TNotifyEvent);
    procedure CalcNewCoords(var NewLeft,
     NewTop,NewWidth,NewHeight,X,Y:integer);
    function DoSelectControls(NewLeft,NewTop,NewWidth,NewHeight:integer):Boolean;
   protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
   public
    freportstructure:TFRpStructure;
    childlist:TList;
    procedure UpdatePos;override;
    property OnDestroy:TNotifyEvent read FOnDestroy write FOnDestroy;
    constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);override;
    destructor destroy;override;
    procedure GetProperties(lnames,ltypes,lvalues:TStrings);override;
    procedure SetProperty(pname:string;value:Widestring);override;
    function GetProperty(pname:string):Widestring;override;
    procedure GetPropertyValues(pname:string;lpossiblevalues:TStrings);override;
    procedure CreateChilds;
    procedure InvalidateAll;
    property OnPosChange:TNotifyEvent read FOnPosChange write SetOnPosChange;
    procedure DoDeleteComponent(aitem:TComponent);
  end;



procedure FreeGridBitmap;

implementation

uses rpmdfmain, rpmdfdesign;


const
 MIN_GRID_BITMAP_WITH=1024;
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
 if Assigned(lvalues) then
  lvalues.Add(BoolToStr(TRpSection(printitem).AutoExpand,true));

 lnames.Add(SRpSAutoContract);
 ltypes.Add(SRpSBool);
 if Assigned(lvalues) then
  lvalues.Add(BoolToStr(TRpSection(printitem).AutoContract,true));

 if (TrpSection(printitem).SectionType in [rpsecgheader,rpsecgfooter]) then
 begin
  lnames.Add(SRpSGroupName);
  ltypes.Add(SRpSString);
  if Assigned(lvalues) then
   lvalues.Add(TRpSection(printitem).GroupName);

  lnames.Add(SRpSGroupExpression);
  ltypes.Add(SRpSExpression);
  if Assigned(lvalues) then
   lvalues.Add(TRpSection(printitem).ChangeExpression);

  lnames.Add(SRpSChangeBool);
  ltypes.Add(SRpSBool);
  if Assigned(lvalues) then
   lvalues.Add(BoolToStr(TRpSection(printitem).ChangeBool,true));

  if TrpSection(printitem).SectionType=rpsecgheader then
  begin
   lnames.Add(SRpSPageRepeat);
   ltypes.Add(SRpSBool);
   if Assigned(lvalues) then
    lvalues.Add(BoolToStr(TRpSection(printitem).PageRepeat,true));
  end;
 end;
 if (TrpSection(printitem).SectionType in [rpsecgheader,rpsecgfooter,rpsecdetail]) then
 begin
  lnames.Add(SRpSBeginPage);
  ltypes.Add(SRpSExpression);
  if Assigned(lvalues) then
   lvalues.Add(TRpSection(printitem).BeginPageExpression);

  lnames.Add(SRpSkipPage);
  ltypes.Add(SRpSBool);
  if Assigned(lvalues) then
   lvalues.Add(BoolToStr(TRpSection(printitem).SkipPage,true));

  lnames.Add(SRPAlignBottom);
  ltypes.Add(SRpSBool);
  if Assigned(lvalues) then
   lvalues.Add(BoolToStr(TRpSection(printitem).AlignBottom,true));

  lnames.Add(SRPHorzDesp);
  ltypes.Add(SRpSBool);
  if Assigned(lvalues) then
   lvalues.Add(BoolToStr(TRpSection(printitem).HorzDesp,true));
 end;
 if (TrpSection(printitem).SectionType=rpsecpfooter) then
 begin
  lnames.Add(SRpSForcePrint);
  ltypes.Add(SRpSBool);
  if Assigned(lvalues) then
   lvalues.Add(BoolToStr(TRpSection(printitem).FooterAtReportEnd,true));
 end;
 // Child Subreport
 lnames.Add(SRpChildSubRep);
 ltypes.Add(SRpSList);
 if Assigned(lvalues) then
  lvalues.Add(TRpSection(printitem).GetChildSubReportName);
 // External section
 lnames.Add(SRpSExternalPath);
 ltypes.Add(SRpSExternalpath);
 if Assigned(lvalues) then
  lvalues.Add(TRpSection(printitem).ExternalFilename);
 lnames.Add(SRpSExternalData);
 ltypes.Add(SRpSExternalData);
 if Assigned(lvalues) then
  lvalues.Add(TRpSection(printitem).GetExternalDataDescription);
end;

procedure TRpSectionInterface.SetProperty(pname:string;value:Widestring);
begin
 if pname=SRpSExternalData then
 begin
  exit;
 end;
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
 if (TrpSection(printitem).SectionType=rpsecpfooter) then
 begin
  if pname=SRpSForcePrint then
  begin
   TRpSection(fprintitem).FooterAtReportEnd:=StrToBool(Value);
   exit;
  end;
 end;
 if pname=SRpChildSubRep then
 begin
  TRpSection(fprintitem).SetChildSubReportByName(Value);
  exit;
 end;
 if pname=SRpSExternalPath then
 begin
  TRpSection(fprintitem).ExternalFilename:=Trim(Value);
  exit;
 end;

 inherited SetProperty(pname,value);
end;

function TRpSectionInterface.GetProperty(pname:string):Widestring;
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
 if (TrpSection(printitem).SectionType=rpsecpfooter) then
 begin
  if pname=SRpSForcePrint then
  begin
   Result:=BoolToStr(TRpSection(fprintitem).FooterAtReportEnd,true);
   exit;
  end;
 end;
 if pname=SRpChildSubRep then
 begin
  Result:=TRpSection(fprintitem).GetChildSubReportName;
  exit;
 end;
 if pname=SRpSExternalPath then
 begin
  Result:=TRpSection(fprintitem).ExternalFileName;
  exit;
 end;
 if pname=SRpSExternalData then
 begin
  Result:=TRpSection(fprintitem).GetExternalDataDescription;
  exit;
 end;

 Result:=inherited GetProperty(pname);
end;

procedure TRpSectionInterface.GetPropertyValues(pname:string;lpossiblevalues:TStrings);
begin
 if pname=SRpChildSubRep then
 begin
  TRpSection(printitem).GetChildSubReportPossibleValues(lpossiblevalues);
  exit;
 end;

 inherited GetPropertyValues(pname,lpossiblevalues);
end;


constructor TRpSectionIntf.Create(AOwner:TComponent);
var
 opts:TControlStyle;
begin
 inherited Create(AOwner);

 opts:=ControlStyle;
 include(opts,csCaptureMouse);
 ControlStyle:=opts;
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
 if Not Assigned(secint.fprintitem.Report) then
  exit;
 report:=TRpReport(secint.fprintitem.Report);
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

procedure TRpSectionIntf.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 if MouseCapture then
  secint.MouseMove(Shift,X,Y);
end;

procedure TRpSectionIntf.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if MouseCapture then
  secint.MouseUp(Button,Shift,X,Y);
end;

procedure TRpSectionIntf.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
 secint.MouseDown(Button,Shift,X,Y);
end;

procedure TRpSectionInterface.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
 // Captures the origin X and Y
 FXOrigin:=X;
 FYOrigin:=Y;
 if Assigned(FRectangle) then
 begin
  FRectangle.free;
  FRectangle2.free;
  FRectangle3.free;
  FRectangle4.free;

  FRectangle:=nil;
 end;
end;

procedure TRpSectionInterface.CalcNewCoords(var NewLeft,
 NewTop,NewWidth,NewHeight,X,Y:integer);
var
 gridenabled:boolean;
 gridx,gridy:integer;
 FRpMainf:TFRpMainF;
begin
 gridenabled:=false;
 gridx:=1;
 gridy:=1;
 FRpMainf:=TFRpMainF(Owner.Owner);
 // There's a selected item insert it
 if Not FRpMainf.BArrow.Down then
 begin
  if FRpMainF.report.GridEnabled then
  begin
   GridEnabled:=True;
   GridX:=FRpMainf.Report.GridWidth;
   GridY:=FRpMainf.Report.GridHeight;
  end;
 end;
 if X<0 then
  X:=0;
 if Y<0 then
  Y:=0;
 if X>FXOrigin then
 begin
  NewLeft:=FXOrigin;
  if gridenabled then
   NewLeft:=AlignToGridPixels(NewLeft,GridX);
  NewWidth:=X-FXOrigin;
 end
 else
 begin
  NewLeft:=X;
  if gridenabled then
   NewLeft:=AlignToGridPixels(NewLeft,GridX);
  NewWidth:=FXOrigin-X;
 end;
 if Y>FYOrigin then
 begin
  NewTop:=FYOrigin;
  if gridenabled then
   NewTop:=AlignToGridPixels(NewTop,GridY);
  NewHeight:=Y-FYOrigin;
 end
 else
 begin
  NewTop:=Y;
  if gridenabled then
   NewTop:=AlignToGridPixels(NewTop,GridY);
  NewHeight:=FYOrigin-Y;
 end;
 if NewLeft+NewWidth>FInterface.Width then
  NewWidth:=FInterface.Width-NewLeft;
 if NewTop+NewHeight>FInterface.Height then
  NewHeight:=FInterface.Height-NewTop;
 // Align to grid width and height
 if GridEnabled then
  NewWidth:=AlignToGridPixels(NewLeft+NewWidth,GridX)-NewLeft;
 if GridEnabled then
  NewHeight:=AlignToGridPixels(NewTop+NewHeight,GridY)-NewTop;
 if NewHeight<CONS_MINHEIGHT then
  Newheight:=CONS_MINHEIGHT;
 if NewWidth<CONS_MINWIDTH then
  NewWidth:=CONS_MINWIDTH;
end;

procedure TRpSectionInterface.MouseMove(Shift: TShiftState; X, Y: Integer);
var NewLeft,
 NewTop,NewWidth,NewHeight:integer;
begin
 // Gets diference
 if Not Assigned(FRectangle) then
 begin
  if ((Abs(X-FXOrigin)<CONS_MINIMUMMOVE) AND
    (Abs(Y-FYOrigin)<CONS_MINIMUMMOVE)) then
    exit;
  // Creates the rectangle
  FRectangle:=TRpRectangle.Create(Self);
  FRectangle2:=TRpRectangle.Create(Self);
  FRectangle3:=TRpRectangle.Create(Self);
  FRectangle4:=TRpRectangle.Create(Self);

  FRectangle.Parent:=FInterface;
  FRectangle2.Parent:=FInterface;
  FRectangle3.Parent:=FInterface;
  FRectangle4.Parent:=FInterface;
  FInterface.Invalidate;
 end;
 CalcNewCoords(NewLeft,NewTop,NewWidth,NewHeight,X,Y);

 FRectangle.SetBounds(Newleft,NewTop,NewWidth,1);
 FRectangle2.SetBounds(Newleft,NewTop+NewHeight,NewWidth,1);
 FRectangle3.SetBounds(Newleft,NewTop,1,NewHeight);
 FRectangle4.SetBounds(Newleft+NewWidth,NewTop,1,NewHeight);
end;


function TRpSectionInterface.DoSelectControls(NewLeft,NewTop,NewWidth,NewHeight:integer):boolean;
var
 i:integer;
 aitem:TRpSizePosInterface;
 rec1,rec2:TRect;
 arec:Trect;
begin
 Result:=false;
 rec1.Left:=NewLeft;
 rec1.Top:=NewTop;
 rec1.Bottom:=NewTop+NewHeight;
 rec1.Right:=NewLeft+NewWidth;
 for i:=0 to childlist.Count-1 do
 begin
  aitem:=TRpSizePosInterface(childlist.Items[i]);
  if aitem.Visible then
  begin
   rec2.Left:=aitem.Left;
   rec2.Top:=aitem.Top;
   rec2.Bottom:=aitem.Top+aitem.Height;
   rec2.Right:=aitem.Left+aitem.Width;
   if IntersectRect(arec,Rec1,Rec2) then
   begin
    TFRpObjInsp(fobjinsp).AddCompItem(aitem,false);
    Result:=True;
   end;
  end;
 end;
end;

procedure TRpSectionInterface.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
var
 asizeposint:TRpSizePosInterface;
 asizepos:TRpCommonPosComponent;
 aitem:TRpCommonListItem;
 FRpMainf:TFRpMainF;
 NewLeft,NewTop,NewWidth,NewHeight:integer;
begin
 inherited MouseDown(Button,Shift,X,Y);

 CalcNewCoords(NewLeft,NewTop,NewWidth,NewHeight,X,Y);
 if Assigned(FRectangle) then
 begin
  FRectangle.free;
  FRectangle2.free;
  FRectangle3.free;
  FRectangle4.free;
  FRectangle:=nil;
 end;
 FRpMainf:=TFRpMainF(Owner.Owner);
 // There's a selected item insert it
 if FRpMainf.BArrow.Down then
 begin
  // Selects object inspector section properties
  if (Not (ssshift in Shift)) then
   TFRpObjInsp(fobjinsp).ClearMultiSelect;
  if  Not DoSelectControls(NewLeft,NewTop,NewWidth,NewHeight) then
  begin
   freportstructure.SelectDataItem(printitem);
  end;
  exit;
 end;
 asizepos:=nil;
 asizeposint:=nil;
 if FRpMainf.BLabel.Down then
 begin
  asizepos:=TRpLabel.Create(printitem.Report);
  TRpLabel(asizepos).Text:=SRpSampleTextToLabels;
  asizeposint:=TRpLabelInterface.Create(Self,asizepos);
 end;
 if FRpMainf.BExpression.Down then
 begin
  asizepos:=TRpExpression.Create(printitem.Report);
  // Search if theres a selected field
  TRpExpression(asizepos).Expression:=FRpMainf.GetExpressionText;
  asizeposint:=TRpExpressionInterface.Create(Self,asizepos);
 end;
 if FRpMainf.BBarcode.Down then
 begin
  asizepos:=TRpBarcode.Create(printitem.Report);
  // Search if theres a selected field
  TRpBarcode(asizepos).Expression:=FRpMainf.GetExpressionText;
  if (TRpBarcode(asizepos).Expression='2+2') then
   TRpBarcode(asizepos).Expression:=QuotedStr(SRpSampleBarCode);
  asizeposint:=TRpBarcodeInterface.Create(Self,asizepos);
 end;
 if FRpMainf.BChart.Down then
 begin
  asizepos:=TRpChart.Create(printitem.Report);
  // Search if theres a selected field
  TRpChart(asizepos).ValueExpression:=FRpMainf.GetExpressionText;
  asizeposint:=TRpChartInterface.Create(Self,asizepos);
 end;
 if FRpMainf.BShape.Down then
 begin
  asizepos:=TRpShape.Create(printitem.Report);
  asizeposint:=TRpDrawInterface.Create(Self,asizepos);
 end;
 if FRpMainf.BImage.Down then
 begin
  asizepos:=TRpImage.Create(printitem.Report);
  asizeposint:=TRpImageInterface.Create(Self,asizepos);
 end;


 if Assigned(asizepos) then
 begin
  asizepos.PosX:=pixelstotwips(NewLeft);
  asizepos.PosY:=pixelstotwips(NewTop);
  asizepos.Height:=pixelstotwips(NewHeight);
  asizepos.Width:=pixelstotwips(NewWidth);
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
  TFRpObjInsp(fobjinsp).AddCompItem(asizeposint,true);
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
  if compo is TRpBarcode then
  begin
   labelint:=TRpBarcodeInterface.Create(Self,compo);
  end;
  if compo is TRpChart then
  begin
   labelint:=TRpChartInterface.Create(Self,compo);
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
   labelint.Visible:=compo.visible;
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
