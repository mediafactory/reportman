{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       Rpmdobjinsp                                     }
{                                                       }
{       Object inspector frame                          }
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

unit rpmdobjinsp;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  rpmdobinsint,QGrids,rpconsts,rpprintitem,QStdCtrls,
  QExtCtrls,rpgraphutils,rpsection,rpmunits, rpexpredlg,
  rpalias,rpreport,Qt,rpsubreport,rpmdflabelint,rplabelitem;

const
  CONS_LEFTGAP=3;
  CONS_CONTROLPOS=90;
  CONS_LABELTOPGAP=2;
  CONS_RIGHTBARGAP=25;
  CONS_BUTTONWIDTH=15;
  CONS_MINWIDTH=160;
type
  TRpPanelObj=class(TScrollBox)
   private
    FCompItem:TRpSizeInterface;
    subrep:TRpSubreport;
    LNames:TStringList;
    LTypes:TStringList;
    LValues:TStringList;
    combo:TComboBox;
    LLabels:TList;
    LControls:TStringList;
    AList:TStringList;
    // Alias for report datasets
    comboalias:TComboBox;
    procedure ComboObjectChange(Sender:TObject);
    procedure EditChange(Sender:TObject);
    procedure SendToBackClick(Sender:TObject);
    procedure BringToFrontClick(Sender:TObject);
    procedure ShapeMouseUp(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
    procedure FontClick(Sender:TObject);
    procedure ImageClick(Sender:TObject);
    procedure ImageKeyDown(Sender: TObject;
     var Key: Word; Shift: TShiftState);
    procedure ExpressionClick(Sender:TObject);
//    procedure  Subreportprops;
    procedure ComboAliasChange(Sender:TObject);
    procedure UpdatePosValues;
    procedure CreateControlsSubReport;
    procedure SelectProperty(propname:string);
   public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure CreateControls(acompo:TRpSizeInterface);
    procedure AssignPropertyValues;
  end;

  TFRpObjInsp = class(TFrame)
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    RpAlias1: TRpAlias;
    RpExpreDialog1: TRpExpreDialog;
    OpenDialog1: TOpenDialog;
  private
    { Private declarations }
    FProppanels:TStringList;
    FDesignFrame:TObject;
    procedure SetCompItem(Value:TRpSizeInterface);
    function FindPanelForClass(acompo:TRpSizeInterface):TRpPanelObj;
    function CreatePanel(acompo:TRpSizeInterface):TRpPanelObj;
    function GetComboBox:TComboBox;
    procedure ChangeSizeChange(Sender:TObject);
    function GetCompItem:TRpSizeInterface;
    function GetCurrentPanel:TRpPanelObj;
  public
    { Public declarations }
    fchangesize:TRpSizeModifier;
    procedure InvalidatePanels;
    procedure SelectProperty(propname:string);
    procedure RecreateChangeSize;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property CompItem:TRpSizeInterface read GetCompItem write SetCompItem;
    property DesignFrame:TObject read FDesignFrame write FDesignFrame;
    property Combo:TComboBox read GetComboBox;
  end;



implementation

{$R *.xfm}

uses rpmdfdesign,rpmdfsectionint, rpmdfmain;


constructor TrpPanelObj.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 Align:=alClient;

 LNames:=TStringList.Create;
 LTypes:=TStringList.Create;
 LValues:=TStringList.Create;
 LLabels:=TList.Create;
 LControls:=TStringList.Create;
 AList:=TStringList.Create;
 BorderStyle:=bsNone;

end;

destructor TrpPanelObj.Destroy;
begin
 LNames.Free;
 LTypes.Free;
 LValues.Free;
 LLabels.Free;
 LControls.Free;
 AList.Free;

 inherited Destroy;
end;


function FindClassName(acompo:TRpSizeInterface):string;
var
 asec:TRpSection;
begin
 if not assigned(acompo) then
 begin
  Result:='TRpSubReport';
  exit;
 end;
 if acompo is TRpSectionInterface then
 begin
  asec:=TrpSection(TRpSectionInterface(acompo).printitem);
  Result:=asec.Name;
  case asec.SectionType of
   rpsecgheader:
    Result:='TRpSectionGroupHeader';
   rpsecgfooter:
    Result:='TRpSectionGroupFooter';
   rpsecdetail:
    Result:='TRpSectionDetail';
   rpsecpheader:
    Result:='TRpSectionPageHeader';
   rpsecpfooter:
    Result:='TRpSectionPageFooter';
  end;
 end
 else
 begin
  Result:=acompo.ClassName;
 end;
end;


// Creates an object inspector panel for the component
procedure TrpPanelObj.CreateControlsSubReport;
var
 alabel:TLabel;
 posy:integer;
 totalwidth:integer;
begin
 totalwidth:=parent.WIdth;
 if totalwidth<CONS_MINWIDTH then
  totalwidth:=CONS_MINWIDTH;
 posy:=0;
 ALabel:=TLabel.Create(Self);
 LLabels.Add(ALabel);
 ALabel.Caption:=SRpMainDataset;
 ALabel.Left:=CONS_LEFTGAP;
 ALabel.Top:=posy+CONS_LABELTOPGAP;
 ALabel.parent:=self;

 ComboAlias:=TComboBox.Create(Self);
 ComboAlias.Style:=csDropDownList;
 ComboAlias.Top:=Posy;
 ComboAlias.Left:=CONS_CONTROLPOS;
 ComboAlias.Width:=TotalWidth-ComboAlias.Left-CONS_RIGHTBARGAP;
 ComboAlias.parent:=self;

 LControls.AddObject(SRpMainDataset,ComboAlias);
end;




procedure TRpPanelObj.CreateControls(acompo:TRpSizeInterface);
var
 totalwidth,aheight:integer;
 posy,i:integer;
 ALabel:TLabel;
 control:TControl;
 typename:string;
 Control2:TControl;
 FRpMainf:TFRpMainF;
 AScrollBox:TScrollBox;
 APanelTop:TPanel;
 APanelBottom:TPanel;
begin
 FRpMainf:=TFRpMainF(Owner.Owner);
 FCompItem:=acompo;
 totalwidth:=parent.WIdth;
 if totalwidth<CONS_MINWIDTH then
  totalwidth:=CONS_MINWIDTH;
 aheight:=0;

 // Creates the labels and controls
 posy:=0;
 // The combobox
 APanelTop:=TPanel.Create(Self);
 APanelTop.BevelInner:=bvNone;
 APanelTop.BevelOuter:=bvNone;
 APanelTop.Align:=alTop;
 APanelTop.Parent:=Self;

 Combo:=TComboBox.Create(Self);
 Combo.Width:=TotalWidth-CONS_RIGHTBARGAP;
 Combo.Style:=csDropDownList;
 Combo.Name:='TopCombobox'+FCompItem.classname;
 combo.OnChange:=ComboObjectChange;
 APanelTop.Height:=Combo.height;
 Combo.Parent:=APanelTop;

 AScrollBox:=TScrollBox.Create(Self);
 AScrollBox.Align:=alClient;
 AScrollBox.BorderStyle:=bsNone;
 AScrollBox.Parent:=Self;

 FCompItem.GetProperties(LNames,LTypes,LValues);
 for i:=0 to LNames.Count-1 do
 begin
  ALabel:=TLabel.Create(Self);
  LLabels.Add(ALabel);
  ALabel.Caption:=LNames.Strings[i];
  ALabel.Left:=CONS_LEFTGAP;
  ALabel.Top:=posy+CONS_LABELTOPGAP;
  ALabel.parent:=AScrollBox;
  typename:=LTypes.Strings[i];
  if LTypes.Strings[i]=SRpSBool then
  begin
   Control:=TComboBox.Create(Self);
   TComboBox(Control).Items.Add(FalseBoolStrs[0]);
   TComboBox(Control).Items.Add(TrueBoolStrs[0]);
   TComboBox(Control).Style:=csDropDownList;
   TCOmboBox(Control).OnChange:=EditChange;
  end
  else
  if LTypes.Strings[i]=SRpSList then
  begin
   Control:=TComboBox.Create(Self);
   FCompItem.GetPropertyValues(LNames.Strings[i],TComboBox(Control).Items);
   TComboBox(Control).Style:=csDropDownList;
   TCOmboBox(Control).OnChange:=EditChange;
  end
  else
  if LTypes.Strings[i]=SRpSColor then
  begin
   Control:=TShape.Create(Self);
   Control.Height:=aheight;
   TShape(Control).Shape:=stRectangle;
   TShape(Control).OnMouseUp:=ShapeMouseUp;
  end
  else
  if LTypes.Strings[i]=SRpSImage then
  begin
   Control:=TEdit.Create(Self);
   TEdit(Control).ReadOnly:=True;
   TEdit(Control).Color:=clInfoBk;
   TEdit(Control).OnClick:=ImageClick;
   TEdit(Control).OnKeyDown:=ImageKeyDown;
  end
  else
  if LTypes.Strings[i]=SRpGroup then
  begin
   Control:=TComboBox.Create(Self);
   subrep:=FRpMainf.freportstructure.FindSelectedSubreport;
   TComboBox(Control).Style:=csDropDownList;
   subrep.GetGroupNames(TComboBox(Control).Items);
   TComboBox(Control).Items.Insert(0,'');
   TComboBox(Control).OnChange:=EditChange;
  end
  else
  if LTypes.Strings[i]=SRpSFontStyle then
  begin
   Control:=TEdit.Create(Self);
   TEdit(Control).ReadOnly:=True;
   TEdit(Control).Color:=clInfoBk;
   TEdit(Control).OnClick:=FontClick;
  end
  else
  begin
   Control:=TEdit.Create(Self);
   TEdit(Control).OnChange:=EditChange;
  end;
  Control.Top:=Posy;
  Control.Left:=CONS_CONTROLPOS;
  Control.Width:=TotalWidth-Control.Left-CONS_RIGHTBARGAP;
  control.parent:=AScrollBox;
  if aheight=0 then
   aheight:=Control.Height;
  Control.tag:=i;
  LControls.AddObject(LNames.Strings[i],Control);
  // Font button
{$IFDEF MSWINDOWS}
  if LTypes.Strings[i]=SRpSWFontName then
  begin
   TEdit(Control).OnDblClick:=FontClick;
  end;
{$ENDIF}
{$IFDEF LINUX}
  if LTypes.Strings[i]=SRpSLFontName then
  begin
   TEdit(Control).OnDblClick:=FontClick;
  end;
{$ENDIF}
{$IFDEF MSWINDOWS}
  if LTypes.Strings[i]=SRpSWFontName then
{$ENDIF}
{$IFDEF LINUX}
  if LTypes.Strings[i]=SRpSLFontName then
{$ENDIF}
  begin
   Control2:=TButton.Create(Self);
   Control2.Width:=CONS_BUTTONWIDTH;
   Control2.Top:=Control.Top;
   Control2.Left:=Control.Left+Control.Width-CONS_BUTTONWIDTH;
   Control2.Height:=COntrol.Height;
   Control2.Tag:=i;
   Control.Width:=Control.Width-CONS_BUTTONWIDTH;
   TButton(Control2).OnClick:=FontClick;
   TButton(Control2).Caption:='...';
   Control2.Parent:=AScrollBox;
  end;
  if (LTypes.Strings[i]=SRpSExpression) then
  begin
   Control2:=TButton.Create(Self);
   Control2.Width:=CONS_BUTTONWIDTH;
   Control2.Top:=Control.Top;
   Control2.Left:=Control.Left+Control.Width-CONS_BUTTONWIDTH;
   Control2.Height:=COntrol.Height;
   Control.Width:=Control.Width-CONS_BUTTONWIDTH;
   Control2.Tag:=i;
   TButton(Control2).OnClick:=ExpressionClick;
   TButton(Control2).Caption:='...';
   Control2.Parent:=AScrollBox;
  end;

  posy:=posy+control.height;
 end;

 // Send to back and bring to front buttons
 if (FCompItem is TRpSizePosInterface) then
 begin
  APanelBottom:=TPanel.Create(Self);
  APanelBottom.BevelInner:=bvNone;
  APanelBottom.BevelOuter:=bvNone;
  APanelBottom.Align:=alBottom;
  APanelBottom.Parent:=Self;
  Control:=TButton.Create(Self);
  Control.Left:=0;
  Control.Top:=0;
  Control.Height:=aheight;
  Control.Width:=(TotalWidth-CONS_RIGHTBARGAP) div 2;
  TBUtton(Control).Caption:=SRpSendToBack;
  TButton(Control).OnClick:=SendToBackClick;
  Control.parent:=APanelBottom;
  Control2:=TButton.Create(Self);
  Control2.Left:=Control.Width;
  Control2.Top:=0;
  Control2.Height:=aheight;
  APanelBottom.Height:=aheight;
  Control2.Width:=(TotalWidth-CONS_RIGHTBARGAP) div 2;
  Control2.parent:=APanelBottom;
  TButton(Control2).OnClick:=BringToFrontClick;
  TBUtton(Control2).Caption:=SRpBringToFront;
 end;
end;

// Creates an object inspector panel for the component
function TFRpObjInsp.CreatePanel(acompo:TRpSizeInterface):TRpPanelObj;
var
 apanel:TRpPanelObj;
begin
 apanel:=TRpPanelObj.Create(Self);
 apanel.Visible:=false;
 apanel.Parent:=Self;
 if Not Assigned(acompo) then
 begin
  apanel.CreateControlsSubReport;
 end
 else
  // Creates a panel and fills it
  apanel.CreateControls(acompo);
 Result:=apanel;
end;

function TFRpObjInsp.FindPanelForClass(acompo:TRpSizeInterface):TRpPanelObj;
var
 newclassname:string;
 i,index:integer;
begin
 newclassname:=FindClassName(acompo);
 // Looks if the panel exists
 index:=FPropPanels.IndexOf(newclassname);
 if (index>=0) then
 begin
  Result:=TrpPanelObj(FPropPanels.Objects[index]);
 end
 else
 begin
  // Creates the panel
  Result:=CreatePanel(acompo);
  FPropPanels.AddObject(newclassname,Result);
 end;
 // Invisible all other panels
 for i:=0 to FPropPanels.Count-1 do
 begin
  if FPropPanels.Objects[i]<>Result then
   TRpPanelObj(FPropPanels.Objects[i]).Visible:=False;
 end;
 // Visible this panel
 if Not Result.Visible then
 begin
  HorzScrollBar.Position:=0;
  VertScrollBar.Position:=0;
 end;
 Result.Visible:=True;
end;

procedure TRpPanelObj.AssignPropertyValues;
var
 FRpMainF:TFRpMainF;
 i:integer;
 typename:String;
 control:TControl;
 secint:TRpSectionInterface;
 asecitem:TRpSizeInterface;
begin
 FRpMainF:=TFRpMainF(Owner.Owner);
 if NOt Assigned(FCompItem) then
 begin
  TFRpObjInsp(Owner).fchangesize.Control:=nil;
  FRpMainf.ACut.Enabled:=false;
  FRpMainf.ACopy.Enabled:=false;
  FRpMainf.AHide.Enabled:=false;
  FRpMainf.APaste.Enabled:=false;
  // Assigns the datasets
  ComboAlias.OnChange:=nil;
  alist.clear;
  alist.add('');
  for i:=0 to FRpMainf.report.DataInfo.Count-1 do
  begin
   alist.Add(FRpMainf.report.DataInfo.items[i].Alias);
  end;
  ComboAlias.Items.Assign(alist);
  ComboAlias.Itemindex:=ComboAlias.Items.IndexOf(subrep.Alias);
  ComboAlias.OnChange:=ComboAliasChange;
  exit;
 end;
 if FCompItem is TRpSizePosInterface then
 begin
  TFRpObjInsp(Owner).fchangesize.GridEnabled:=FRpMainf.report.GridEnabled;
  TFRpObjInsp(Owner).fchangesize.GridX:=FRpMainf.report.GridWidth;
  TFRpObjInsp(Owner).fchangesize.GridY:=FRpMainf.report.GridHeight;
  TFRpObjInsp(Owner).fchangesize.Control:=FCompItem;
  FRpMainf.ACut.Enabled:=true;
  FRpMainf.ACopy.Enabled:=true;
  FRpMainf.AHide.Enabled:=true;
  secint:=TrpSectionInterface(TRpSizePosInterface(FCompItem).SectionInt);
 end
 else
 begin
  TFRpObjInsp(Owner).fchangesize.Control:=nil;
  secint:=TrpSectionInterface(FCompItem);
 end;
 FRpMainf.APaste.Enabled:=true;

 // Assigns combo values
 combo.OnChange:=nil;
 alist.clear;
 alist.Add('');
 for i:=0 to secint.childlist.count-1 do
 begin
  asecitem:=TRpSizePosInterface(secint.childlist.items[i]);
  alist.AddObject(asecitem.PrintItem.Name,asecitem);
 end;
 combo.items.assign(alist);
 if FCompItem is TRpSizePosInterface then
  combo.Itemindex:=alist.Indexof(FCompItem.Printitem.Name)
 else
  combo.Itemindex:=-1;
 combo.OnChange:=ComboObjectChange;


 FCompItem.GetProperties(LNames,LTypes,LValues);
 for i:=0 to LNames.Count-1 do
 begin
  typename:=LTypes.Strings[i];
  if LTypes.Strings[i]=SRpSBool then
  begin
   Control:=TControl(LControls.Objects[i]);
   TCOmboBox(Control).OnChange:=nil;
   TComboBox(Control).ItemIndex:=TComboBox(Control).Items.IndexOf(LValues.Strings[i]);
   TCOmboBox(Control).OnChange:=EditChange;
  end
  else
  if LTypes.Strings[i]=SRpSList then
  begin
   Control:=TControl(LControls.Objects[i]);
   TCOmboBox(Control).OnChange:=nil;
   TComboBox(Control).ItemIndex:=TComboBox(Control).Items.IndexOf(LValues.Strings[i]);
   TCOmboBox(Control).OnChange:=EditChange;
  end
  else
  if LTypes.Strings[i]=SRpSColor then
  begin
   Control:=TControl(LControls.Objects[i]);
   TShape(Control).Brush.Color:=StrToInt(LValues.Strings[i]);
  end
  else
  if LTypes.Strings[i]=SRpSImage then
  begin
   Control:=TControl(LControls.Objects[i]);
   TEdit(Control).Text:=LValues.Strings[i];
  end
  else
  if LTypes.Strings[i]=SRpGroup then
  begin
   Control:=TControl(LControls.Objects[i]);
   subrep:=FRpMainf.freportstructure.FindSelectedSubreport;
   alist.clear;
   subrep.GetGroupNames(alist);
   alist.Insert(0,'');
   TComboBox(Control).OnChange:=nil;
   TComboBox(Control).Items.Assign(alist);
   if FCompItem is TRpExpressionInterface then
    TComboBox(Control).ItemIndex:=TComboBox(Control).Items.IndexOf(
      TRpExpression(TRpExpressionInterface(FCompItem).printitem).GroupName);
   TComboBox(Control).OnChange:=EditChange;
  end
  else
  if LTypes.Strings[i]=SRpSFontStyle then
  begin
   Control:=TControl(LControls.Objects[i]);
   TEdit(Control).Text:=IntegerFontStyleToString(StrToInt(LValues.Strings[i]));
  end
  else
  begin
   Control:=TControl(LControls.Objects[i]);
   TEdit(Control).OnChange:=nil;
   TEdit(Control).Text:=LValues.Strings[i];
   TEdit(Control).OnChange:=EditChange;
  end;
 end;
end;

function TFRpObjInsp.GetCompItem:TRpSizeInterface;
var
 i:integer;
begin
 Result:=nil;
 for i:=0 to FPropPanels.Count-1 do
 begin
  if TRpPanelObj(FPropPanels.Objects[i]).Visible then
  begin
   Result:=TRpPanelObj(FPropPanels.Objects[i]).FCompItem;
  end;
 end;
end;

procedure TRpPanelObj.SelectProperty(propname:string);
var
 index:integer;
 AControl:TWinControl;
begin
 index:=LControls.IndexOf(propname);
 if index>=0  then
 begin
  AControl:=TWinControl(LControls.Objects[index]);
  AControl.SetFocus;
 end;
end;

function TFRpObjInsp.GetCurrentPanel:TRpPanelObj;
var
 i:integer;
begin
 Result:=nil;
 for i:=0 to FPRopPanels.Count-1 do
 begin
  if TRpPanelObj(FPropPanels.Objects[i]).Visible then
  begin
   Result:=TRpPanelObj(FPropPanels.Objects[i]);
   break;
  end;
 end;
end;

procedure TFRpObjInsp.SelectProperty(propname:string);
var
 FCurrentPanel:TRpPanelObj;
begin
 FCurrentPanel:=GetCurrentPanel;
 if Assigned(FCurrentPanel) then
  FCurrentPanel.SelectProperty(propname);
end;

procedure TFRpObjInsp.SetCompItem(Value:TRpSizeInterface);
var
 FRpMainf:TFRpMainF;
 FCurrentPanel:TRpPanelObj;
begin
 FRpMainf:=TFRpMainF(Owner);
 FCurrentPanel:=FindPanelForClass(Value);
 FCurrentPanel.FCompItem:=Value;
 FCurrentPanel.Subrep:=FRpMainf.freportstructure.FindSelectedSubreport;
 FCurrentPanel.AssignPropertyValues;


end;

constructor TFRpObjInsp.Create(AOwner:TComponent);
var
 FRpMainF:TFRpMainF;
begin
 inherited Create(AOwner);
 FProppanels:=TStringList.Create;

 FRpMainF:=TFRpMainF(Owner);

 fchangesize:=TRpSizeModifier.Create(Self);
 fchangesize.OnSizeChange:=changesizechange;


 if Screen.PixelsPerInch>90 then
 begin
  Font.Size:=7;
 end;

 RpExpreDialog1.evaluator.AddVariable('Page',FRpMainf.report.idenpagenum);
 RpExpreDialog1.evaluator.AddVariable('FREE_SPACE',FRpMainf.report.idenfreespace);
 RpExpreDialog1.evaluator.AddVariable('CURRENTGROUP',FRpMainf.report.idencurrentgroup);
 RpExpreDialog1.evaluator.AddVariable('FREE_SPACE_CMS',FRpMainf.report.idenfreespacecms);
 RpExpreDialog1.evaluator.AddVariable('FREE_SPACE_INCH',FRpMainf.report.idenfreespaceinch);
end;

destructor TFRpObjInsp.Destroy;
begin
 FPropPanels.Free;
 inherited Destroy;
end;

procedure TRpPanelObj.EditChange(Sender:TObject);
var
 index:integer;
 aname:string;
begin
 index:=TControl(Sender).tag;
 aname:=Lnames.strings[index];
 FCompItem.SetProperty(aname,TEdit(Sender).Text);
 if (FCompItem is TRpSectionInterface) then
 begin
  if ((aname=SRpsWidth) or (aname=SRpsHeight)) then
   if Assigned(TFRpObjInsp(Owner).FDesignFrame) then
    TFRpDesignFrame(TFRpObjInsp(Owner).FDesignFrame).UpdateInterface;
 end;
 // If the property es positional update position
 if Assigned(TFRpObjInsp(Owner).fchangesize) then
 begin
  if ((aname=SRpSWidth) or (aname=SRpsHeight) or
   (aname=SRpSTop) or (aname=SRpSLeft)) then
  begin
   TFRpObjInsp(Owner).fchangesize.UpdatePos;
  end;
 end;
end;

procedure TRpPanelObj.ShapeMouseUp(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
var
 AShape:TShape;
begin
 AShape:=TShape(Sender);
 TFRpObjInsp(Owner).ColorDialog1.COlor:=StrToInt(LValues.Strings[AShape.Tag]);
 if TFRpObjInsp(Owner).ColorDialog1.Execute then
 begin
  AShape.Brush.Color:=TFRpObjInsp(Owner).ColorDialog1.Color;
  FCompItem.SetProperty(Lnames.strings[AShape.Tag],IntToStr(TFRpObjInsp(Owner).ColorDialog1.Color));
 end;
end;

procedure TRpPanelObj.FontClick(Sender:TObject);
var
 index:integer;
begin
{$IFDEF MSWINDOWS}
 TFRpObjInsp(Owner).FontDialog1.Font.Name:=FCompItem.GetProperty(SRpSWFontName);
{$ENDIF}
{$IFDEF LINUX}
 TFRpObjInsp(Owner).FontDialog1.Font.Name:= FCompItem.GetProperty(SRpSLFontName);
{$ENDIF}
 TFRpObjInsp(Owner).FontDialog1.Font.Size:= StrToInt(FCompItem.GetProperty(SRpSFontSize));
 TFRpObjInsp(Owner).FontDialog1.Font.Color:= StrToInt(FCompItem.GetProperty(SRpSFontColor));
 TFRpObjInsp(Owner).FontDialog1.Font.Style:=IntegerToFontStyle(StrToInt(FCompItem.GetProperty(SrpSFontStyle)));
 if TFRpObjInsp(Owner).FontDialog1.Execute then
 begin
  index:=TComponent(Sender).Tag;
  if index>=0 then
  begin
   TEdit(LControls.Objects[index]).Text:=TFRpObjInsp(Owner).FontDialog1.Font.Name;
  end;
  index:=LNames.IndexOf(SrpSFontSize);
  if index>=0 then
  begin
   TEdit(LControls.Objects[index]).Text:=IntToStr(TFRpObjInsp(Owner).FontDialog1.Font.Size);
  end;
  index:=LNames.IndexOf(SrpSFontColor);
  if index>=0 then
  begin
   TShape(LControls.Objects[index]).Brush.Color:=TFRpObjInsp(Owner).FontDialog1.Font.Color;
   FCompItem.SetProperty(SRpSFontColor,IntToStr(TFRpObjInsp(Owner).FontDialog1.Font.Color));
  end;
  index:=LNames.IndexOf(SrpSFontStyle);
  if index>=0 then
  begin
   TEdit(LControls.Objects[index]).Text:=IntegerFontStyleToString(FontStyleToInteger(TFRpObjInsp(Owner).Fontdialog1.Font.Style));
   FCompItem.SetProperty(SRpSFontStyle,IntToStr(FontStyleToInteger(TFRpObjInsp(Owner).Fontdialog1.Font.Style)));
  end;
 end;
end;

procedure TRpPanelObj.ComboObjectChange(Sender:TObject);
begin
 TFRpObjInsp(Owner).CompItem:=TRpSizeInterface(TComboBox(Sender).Items.Objects[TComboBox(Sender).ItemIndex]);
end;

procedure TFRpObjInsp.ChangeSizeChange(Sender:TObject);
var
 FCurrentPanel:TRpPanelObj;
begin
 // Read bounds Values and assign
 if Not Assigned(fchangesize.Control) then
  exit;
 FCurrentPanel:=GetCurrentPanel;
 if Assigned(FCurrentPanel) then
  FCurrentPanel.UpdatePosValues;
end;

procedure TRpPanelObj.SendToBackClick(Sender:TObject);
var
 section:TRpSection;
 item:TRpCommonListItem;
 pitem:TRpCommonComponent;
 index:integer;
begin
 FCompItem.SendToBack;
 TRpSizePosInterface(FCompItem).SectionInt.SendToBack;
 pitem:=FCompItem.printitem;
 section:=TRpSection(TRpSizePosInterface(FCompItem).SectionInt.printitem);
 index:=0;
 while index<section.Components.Count do
 begin
  if (section.Components.Items[index].Component=pitem) then
   break;
  inc(index);
 end;
 if index>=section.Components.Count then
  exit;
 section.Components.Delete(index);
 item:=section.Components.Insert(0);
 item.Component:=pitem;
end;

procedure TRpPanelObj.BringToFrontClick(Sender:TObject);
var
 section:TRpSection;
 item:TRpCommonListItem;
 pitem:TRpCommonComponent;
 index:integer;
begin
 FCompItem.BringToFront;
 TFRpObjInsp(Owner).fchangesize.UpdatePos;

 pitem:=FCompItem.printitem;
 section:=TRpSection(TRpSizePosInterface(FCompItem).SectionInt.printitem);
 index:=0;
 while index<section.Components.Count do
 begin
  if (section.Components.Items[index].Component=pitem) then
   break;
  inc(index);
 end;
 if index>=section.Components.Count then
  exit;
 section.Components.Delete(index);
 item:=section.Components.Add;
 item.Component:=pitem;
end;



procedure TRpPanelObj.ExpressionClick(Sender:TObject);
var
 report:TRpReport;
 i:integer;
 item:TRpAliaslistItem;
 FRpMainF:TFRpMainF;
begin
 FRpMainF:=TFRpMainF(Owner.Owner);
 report:=FRpMainf.report;
 try
  report.ActivateDatasets;
 except
  on E:Exception do
  begin
   ShowMessage(E.Message);
  end;
 end;

 TFRpObjInsp(Owner).RpAlias1.List.Clear;
 for i:=0 to report.DataInfo.Count-1 do
 begin
  item:=TFRpObjInsp(Owner).RpAlias1.List.Add;
  item.Alias:=report.DataInfo.Items[i].Alias;
  item.Dataset:=report.DataInfo.Items[i].Dataset;
 end;
 TFRpObjInsp(Owner).RpExpreDialog1.Expresion.Text:=TEdit(LControls.Objects[TButton(Sender).Tag]).Text;
 if TFRpObjInsp(Owner).RpExpreDialog1.Execute then
  TEdit(LControls.Objects[TButton(Sender).Tag]).Text:=Trim(TFRpObjInsp(Owner).RpExpreDialog1.Expresion.Text);
end;

procedure TRpPanelObj.UpdatePosValues;
var
 index:integer;
 sizeposint:TRpSizePosInterface;
 NewLeft,NewTop,NewWidth,NewHeight:integer;
begin
 sizeposint:=TRpSizePosInterface(TFRpObjInsp(Owner).fchangesize.control);
 NewLeft:=sizeposint.Left;
 NewTop:=sizeposint.Top;
 NewWidth:=sizeposint.Width;
 NewHeight:=sizeposint.Height;
 index:=LNames.IndexOf(SRpSLeft);
 if index>=0 then
 begin
  sizeposint.SetProperty(SRpSLeft,gettextfromtwips(pixelstotwips(NewLeft)));
 end;
 index:=LNames.IndexOf(SRpSTop);
 if index>=0 then
 begin
  sizeposint.SetProperty(SRpSTop,gettextfromtwips(pixelstotwips(NewTop)));
 end;
 index:=LNames.IndexOf(SRpSWidth);
 if index>=0 then
 begin
  sizeposint.SetProperty(SRpSWidth,gettextfromtwips(pixelstotwips(NewWidth)));
 end;
 index:=LNames.IndexOf(SRpSHeight);
 if index>=0 then
 begin
  sizeposint.SetProperty(SRpSHeight,gettextfromtwips(pixelstotwips(NewHeight)));
 end;
end;

procedure TRpPanelObj.ImageClick(Sender:TObject);
var
 Stream:TMemoryStream;
begin
 if TFRpObjInsp(Owner).OpenDialog1.Execute then
 begin
  Stream:=TMemoryStream.Create;
  try
   Stream.LoadFromFile(TFRpObjInsp(Owner).OpenDialog1.FileName);
   Stream.Seek(0,soFromBeginning);
   FCompItem.SetProperty(LNames.Strings[TComponent(Sender).Tag],stream);
  finally
   Stream.Free;
  end;
 end;
end;

procedure TRpPanelObj.ImageKeyDown(Sender: TObject;
     var Key: Word; Shift: TShiftState);
var
 Stream:TMemoryStream;
begin
 if ((Key=Key_BackTab) or (Key=Key_Delete)) then
 begin
  Stream:=TMemoryStream.Create;
  try
   FCompItem.SetProperty(LNames.Strings[TComponent(Sender).Tag],stream);
  finally
   Stream.Free;
  end;
 end;
end;


procedure TRpPanelObj.ComboAliasChange(Sender:TObject);
begin
 subrep.Alias:=TComboBox(Sender).Text;
end;

procedure TFRpObjInsp.RecreateChangeSize;
begin
 fchangesize.free;
 fchangesize:=nil;
 fchangesize:=TRpSizeModifier.Create(Self);
 fchangesize.OnSizeChange:=changesizechange;
end;


procedure TFRpObjInsp.InvalidatePanels;
var
 i:integer;
begin
 // Panels must be resized when show
 for i:=0 to FPropPanels.Count-1 do
 begin
  FPropPanels.Objects[i].Free;
 end;
 FPropPanels.Clear;
end;


function TFRpObjInsp.GetComboBox:TComboBox;
var
 i:integer;
begin
 Result:=nil;
 for i:=0 to FPropPanels.Count-1 do
 begin
  if TrpPanelObj(FPropPanels.Objects[i]).Visible then
  begin
   Result:=TrpPanelObj(FPropPanels.Objects[i]).Combo;
  end;
 end;
end;


initialization


end.
