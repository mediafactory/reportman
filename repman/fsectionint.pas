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
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit fsectionint;

interface

uses SysUtils, Classes, QGraphics, QForms,Types,
  QButtons, QExtCtrls, QControls, QStdCtrls,
  rpobinsint,rpreport,rpprintitem,rpgraphutils,
  rpobjinsp,frpstruc,flabelint,rplabelitem,
  rpconsts,rpsection,rptypes,rpdrawitem,fdrawint,
{$IFNDEF PROFILE}  rpsubreport;{$ENDIF}
{$IFDEF PROFILE}  rpsubreport ,Proftimx;{$ENDIF}


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
    FOnDestroy:TNotifyEvent;
   protected
    procedure Paint;override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
   public
    OnPosChange:TNotifyEvent;
    freportstructure:TFRpStructure;
    childlist:TList;
    property OnDestroy:TNotifyEvent read FOnDestroy write FOnDestroy;
    constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);override;
    destructor destroy;override;
    procedure GetProperties(lnames,ltypes,lvalues:TStrings);override;
    procedure SetProperty(pname:string;value:string);override;
    function GetProperty(pname:string):string;override;
    procedure CreateChilds;
    procedure InvalidateAll;
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
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,117; xor eax,eax; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 if Assigned(FBitmap) then
 begin
  fbitmap.free;
  fbitmap:=nil;
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,117; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

function DrawBitmapGrid(width,height,xgrid,ygrid:integer;color:TColor;lines:boolean):TBitmap;
var
 rec:TRect;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,118; xor eax,eax; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,118; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


constructor TRpSectionInterface.Create(AOwner:TComponent;pritem:TRpCommonComponent);
var
 opts:TControlStyle;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,119; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 if Not (pritem is TRpSection) then
  Raise Exception.Create(SRpIncorrectComponentForInterface);
 inherited Create(AOwner,pritem);
 opts:=ControlStyle;
 include(opts,csCaptureMouse);
 ControlStyle:=opts;
 ChildList:=TList.Create;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,119; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

destructor TRpSectionInterface.destroy;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,120; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 if Assigned(FOnDestroy) then
 begin
  FOnDestroy(Self);
 end;
 childlist.free;
 inherited destroy;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,120; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


procedure TRpSectionInterface.GetProperties(lnames,ltypes,lvalues:TStrings);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,121; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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

  lnames.Add(SRpSPageRepeat);
  ltypes.Add(SRpSBool);
  lvalues.Add(BoolToStr(TRpSection(printitem).PageRepeat,true));
 end;
 if (TrpSection(printitem).SectionType in [rpsecgheader,rpsecgfooter,rpsecdetail]) then
 begin
  lnames.Add(SRpSBeginPage);
  ltypes.Add(SRpSBool);
  lvalues.Add(BoolToStr(TRpSection(printitem).BeginPage,true));

  lnames.Add(SRpSkipPage);
  ltypes.Add(SRpSBool);
  lvalues.Add(BoolToStr(TRpSection(printitem).SkipPage,true));

  lnames.Add(SRPAlignBottom);
  ltypes.Add(SRpSBool);
  lvalues.Add(BoolToStr(TRpSection(printitem).AlignBottom,true));

 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,121; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TRpSectionInterface.SetProperty(pname:string;value:string);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,122; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
   TRpSection(fprintitem).BeginPage:=StrToBool(Value);
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
 end;
 inherited SetProperty(pname,value);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,122; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

function TRpSectionInterface.GetProperty(pname:string):string;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,123; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
   Result:=BoolToStr(TRpSection(fprintitem).BeginPage,true);
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
 end;
 Result:=inherited GetProperty(pname);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,123; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TRpSectionInterface.Paint;
var
 report:TRpReport;
 rec:TRect;
 bitmap:TBitmap;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,124; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,124; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TRpSectionInterface.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
var
 asizeposint:TRpSizePosInterface;
 asizepos:TRpCommonPosComponent;
 aitem:TRpCommonListItem;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,125; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 inherited MouseDown(Button,Shift,X,Y);

 // There's a selected item insert it
 if fmainf.BArrow.Down then
 begin
  // Selects object inspector section properties
  freportstructure.SelectDataItem(printitem);
  TFObjInsp(fobjinsp).CompItem:=self;
  exit;
 end;
 asizepos:=nil;
 asizeposint:=nil;
 if fmainf.BLabel.Down then
 begin
  asizepos:=TRpLabel.Create(printitem.Owner);
  TRpLabel(asizepos).Text:=SRpSampleTextToLabels;
  asizeposint:=TRpLabelInterface.Create(Self,asizepos);
 end;
 if fmainf.BExpression.Down then
 begin
  asizepos:=TRpExpression.Create(printitem.Owner);
  TRpExpression(asizepos).Expression:= SRpSampleExpression;
  asizeposint:=TRpExpressionInterface.Create(Self,asizepos);
 end;
 if fmainf.BShape.Down then
 begin
  asizepos:=TRpShape.Create(printitem.Owner);
  asizeposint:=TRpDrawInterface.Create(Self,asizepos);
 end;
 if fmainf.BImage.Down then
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
  asizeposint.Parent:=parent;
  asizeposint.sectionint:=self;
  asizeposint.UpdatePos;
  asizeposint.fobjinsp:=fobjinsp;
  childlist.Add(asizeposint);
  if assigned(TFObjInsp(fobjinsp).Combo) then
  begin
   TFObjInsp(fobjinsp).Combo.Items.AddObject(asizepos.Name,asizeposint);
   TFObjInsp(fobjinsp).Combo.ItemIndex:=TFObjInsp(fobjinsp).Combo.Items.IndexOfObject(asizeposint);
  end;
  TFObjInsp(fobjinsp).CompItem:=asizeposint;
  if (Not (SSShift in Shift)) then
   fmainf.BArrow.Down:=true;
 end;


{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,125; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TRpSectionInterface.CreateChilds;
var
 sec:TRpSection;
 i:integer;
 compo:TRpCommonPosComponent;
 labelint:TRpSizePosInterface;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,126; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
   labelint.Parent:=parent;
   labelint.UpdatePos;
   labelint.fobjinsp:=fobjinsp;
   labelint.sectionint:=self;
   childlist.Add(labelint)
  end;
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,126; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TRpSectionInterface.InvalidateAll;
var
 i:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,127; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 for i:=0 to childlist.count-1 do
 begin
  TRpSizeInterface(childlist.items[i]).Invalidate;
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,127; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


initialization
fbitmap:=nil;
fbwidth:=0;
fbheight:=0;
fcolor:=clBlack;

end.
