{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       Rpobjinsp                                       }
{                                                       }
{       Object inspector frame                          }
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

unit rpobjinsp;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  rpobinsint,QGrids,rpconsts,rpprintitem,QStdCtrls,
  QExtCtrls,rpgraphutils,rpsection,rpmunits, rpexpredlg,
  rpalias,rpreport,Qt;

const
  CONS_LEFTGAP=3;
  CONS_CONTROLPOS=70;
  CONS_LABELTOPGAP=2;
  CONS_RIGHTBARGAP=25;
  CONS_BUTTONWIDTH=15;
  CONS_MINWIDTH=130;
type
  TFObjInsp = class(TFrame)
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    RpAlias1: TRpAlias;
    RpExpreDialog1: TRpExpreDialog;
    OpenDialog1: TOpenDialog;
  private
    { Private declarations }
    dontfreecombo:Boolean;
    FCompItem:TRpSizeInterface;
    FDesignFrame:TObject;
    LNames:TStringList;
    LTypes:TStringList;
    LValues:TStringList;
    fchangesize:TRpSizeModifier;
    procedure SetCompItem(Value:TRpSizeInterface);
    procedure ReleaseAllControls;
    procedure EditChange(Sender:TObject);
    procedure ChangeSizeChange(Sender:TObject);
    procedure ComboObjectChange(Sender:TObject);
    procedure SendToBackClick(Sender:TObject);
    procedure BringToFrontClick(Sender:TObject);
    procedure ShapeMouseUp(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
    procedure FontClick(Sender:TObject);
    procedure ImageClick(Sender:TObject);
    procedure ImageKeyDown(Sender: TObject;
     var Key: Word; Shift: TShiftState);
    procedure ExpressionClick(Sender:TObject);
  public
    { Public declarations }
    combo:TComboBox;
    LLabels:TList;
    LControls:TStringList;
    LControlsToFree:TList;
    procedure UpdatePosValues;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property CompItem:TRpSizeInterface read FCompItem write SetCompItem;
    property DesignFrame:TObject read FDesignFrame write FDesignFrame;
  end;



implementation

{$R *.xfm}

uses fdesign,fsectionint, fmain;

procedure TFObjInsp.ReleaseAllControls;
var
 i:integer;
begin
 for i:=0 to LLabels.Count-1 do
 begin
  TObject(LLabels.items[i]).Free;
  LLabels.items[i]:=nil;
 end;
 Combo:=nil;
 LLabels.Clear;
 for i:=0 to LControlsToFree.Count-1 do
 begin
  if dontfreecombo then
  begin
   if TComponent(LControlsToFree.Items[i]).Name='TopCombobox' then
   begin
    Combo:=TCombobox(LControlsToFree.Items[i]);
   end
   else
    TObject(LControlsToFree.Items[i]).Free;
  end
  else
   TObject(LControlsToFree.Items[i]).Free;
 end;
 LCOntrols.Clear;
 LCOntrolsToFree.Clear;
 if dontfreecombo then
  if assigned(combo) then
   LControlsToFree.Add(Combo);
end;


procedure TFObjInsp.SetCompItem(Value:TRpSizeInterface);
var
 i:integer;
 ALabel:TLabel;
 posy:integer;
 control:TControl;
 typename:string;
 aheight:integer;
 Control2:TControl;
 alist:TStringList;
 sectionint:TRpSectionInterface;
 compo:TComponent;
 dontrelease:boolean;
 totalwidth:integer;
begin
 totalwidth:=WIdth;
 if totalwidth<CONS_MINWIDTH then
  totalwidth:=CONS_MINWIDTH;
 aheight:=0;
 dontrelease:=false;
 if Assigned(FCompItem) then
  if Assigned(Value) then
   if FCompItem.ClassName=Value.ClassName then
     dontrelease:=true;
 if not dontrelease then
  ReleaseAllControls;
 FCompItem:=Value;
 if Not Assigned(Value) then
 begin
  fchangesize.Control:=nil;
  fmainf.ACut.Enabled:=false;
  fmainf.ACopy.Enabled:=false;
  fmainf.APaste.Enabled:=false;
  exit;
 end;
 if CompItem is TRpSizePosInterface then
 begin
  fchangesize.GridEnabled:=fmainf.report.GridEnabled;
  fchangesize.GridX:=fmainf.report.GridWidth;
  fchangesize.GridY:=fmainf.report.GridHeight;
  fchangesize.Control:=CompItem;
  fmainf.ACut.Enabled:=true;
  fmainf.ACopy.Enabled:=true;
 end
 else
  fchangesize.Control:=nil;
 fmainf.APaste.Enabled:=true;
 if not dontrelease then
 begin
  HorzScrollBar.Position:=0;
  VertScrollBar.Position:=0;
 end;
 // Creates the labels and controls

 posy:=0;

 if ((not (dontfreecombo)) and (not dontrelease)) then
 begin
  // Fills the combox of components
  alist:=TStringList.Create;
  try
   alist.sorted:=true;
   if CompItem is TRpSectionInterface then
   begin
    sectionint:=TRpSectionInterface(CompItem);
   end
   else
   begin
    sectionint:=TRpSectionInterface(TRpSizePosInterface(Compitem).SectionInt);
   end;
   for i:=0 to sectionint.childlist.Count-1 do
   begin
    compo:=TRpSizeInterface(sectionint.childlist.Items[i]).printitem;
    alist.AddObject(compo.Name,sectionint.childlist.Items[i]);
   end;
   Combo:=TComboBox.Create(Self);
   Combo.Width:=TotalWidth-CONS_RIGHTBARGAP;
   Combo.Style:=csDropDownList;
   Combo.Items.Assign(alist);
   Combo.Name:='TopCombobox';
   Combo.ItemIndex:=combo.Items.IndexOfObject(CompItem);
   combo.OnChange:=ComboObjectChange;
   Combo.Parent:=Self;
   LControlsToFree.Add(Combo);
   posy:=posy+Combo.height;
  finally
   alist.free;
  end;
 end
 else
 begin
  posy:=posy+Combo.Height;
  Combo.ItemIndex:=combo.Items.IndexOfObject(CompItem.printitem);
 end;
 FCompItem.GetProperties(LNames,LTypes,LValues);
 for i:=0 to LNames.Count-1 do
 begin
  if not dontrelease then
  begin
   ALabel:=TLabel.Create(Self);
   LLabels.Add(ALabel);
   ALabel.Caption:=LNames.Strings[i];
   ALabel.Left:=CONS_LEFTGAP;
   ALabel.Top:=posy+CONS_LABELTOPGAP;
   ALabel.parent:=self;
  end;
  typename:=LTypes.Strings[i];
  if LTypes.Strings[i]=SRpSBool then
  begin
   if dontrelease then
    Control:=TControl(LControls.Objects[i])
   else
   begin
    Control:=TComboBox.Create(Self);
    TComboBox(Control).Items.Add(FalseBoolStrs[0]);
    TComboBox(Control).Items.Add(TrueBoolStrs[0]);
    TComboBox(Control).Style:=csDropDownList;
    TCOmboBox(Control).OnChange:=EditChange;
   end;
   TComboBox(Control).ItemIndex:=TComboBox(Control).Items.IndexOf(LValues.Strings[i]);
  end
  else
  if LTypes.Strings[i]=SRpSList then
  begin
   if dontrelease then
    Control:=TControl(LControls.Objects[i])
   else
   begin
    Control:=TComboBox.Create(Self);
    CompItem.GetPropertyValues(LNames.Strings[i],TComboBox(Control).Items);
    TComboBox(Control).Style:=csDropDownList;
    TCOmboBox(Control).OnChange:=EditChange;
   end;
   TComboBox(Control).ItemIndex:=TComboBox(Control).Items.IndexOf(LValues.Strings[i]);
  end
  else
  if LTypes.Strings[i]=SRpSColor then
  begin
   if dontrelease then
    Control:=TControl(LControls.Objects[i])
   else
    Control:=TShape.Create(Self);
   Control.Height:=aheight;
   TShape(Control).Shape:=stRectangle;
   TShape(Control).Brush.Color:=StrToInt(LValues.Strings[i]);
   TShape(Control).OnMouseUp:=ShapeMouseUp;
  end
  else
  if LTypes.Strings[i]=SRpSImage then
  begin
   if dontrelease then
    Control:=TControl(LControls.Objects[i])
   else
    Control:=TEdit.Create(Self);
   TEdit(Control).Text:=LValues.Strings[i];
   TEdit(Control).ReadOnly:=True;
   TEdit(Control).Color:=clInfoBk;
   TEdit(Control).OnClick:=ImageClick;
   TEdit(Control).OnKeyDown:=ImageKeyDown;
  end
  else
  if LTypes.Strings[i]=SRpSFontStyle then
  begin
   if dontrelease then
    Control:=TControl(LControls.Objects[i])
   else
    Control:=TEdit.Create(Self);
   TEdit(Control).Text:=IntegerFontStyleToString(StrToInt(LValues.Strings[i]));
   TEdit(Control).ReadOnly:=True;
   TEdit(Control).Color:=clInfoBk;
   TEdit(Control).OnClick:=FontClick;
  end
  else
  begin
   if dontrelease then
    Control:=TControl(LControls.Objects[i])
   else
    Control:=TEdit.Create(Self);
   TEdit(Control).Text:=LValues.Strings[i];
   TEdit(Control).OnChange:=EditChange;
  end;

  if not dontrelease then
  begin
   Control.Top:=Posy;
   Control.Left:=CONS_CONTROLPOS;
   Control.Width:=TotalWidth-Control.Left-CONS_RIGHTBARGAP;
   control.parent:=self;
  end;
  if aheight=0 then
   aheight:=Control.Height;
  Control.tag:=i;
  if not dontrelease then
  begin
   LControls.AddObject(LNames.Strings[i],Control);
   LControlsToFree.Add(Control);
  end;
  // Font button
  if not dontrelease then
  begin
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
    TButton(Control2).OnClick:=FontClick;
    TButton(Control2).Caption:='...';
    Control2.Parent:=Self;
    LControlsToFree.Add(Control2);
   end;
   if (LNames.Strings[i]=SRpSExpression) then
   begin
    Control2:=TButton.Create(Self);
    Control2.Width:=CONS_BUTTONWIDTH;
    Control2.Top:=Control.Top;
    Control2.Left:=Control.Left+Control.Width-CONS_BUTTONWIDTH;
    Control2.Height:=COntrol.Height;
    Control2.Tag:=i;
    TButton(Control2).OnClick:=ExpressionClick;
    TButton(Control2).Caption:='...';
    Control2.Parent:=Self;
    LControlsToFree.Add(Control2);
   end;
  end;

  posy:=posy+control.height;
 end;
 // Send to back and bring to front buttons
 if not dontrelease then
 begin
  if (CompItem is TRpSizePosInterface) then
  begin
   Control:=TButton.Create(Self);
   Control.Left:=0;
   Control.Top:=posy;
   Control.Height:=aheight;
   Control.Width:=(TotalWidth-CONS_RIGHTBARGAP) div 2;
   TBUtton(Control).Caption:=SRpSendToBack;
   TButton(Control).OnClick:=SendToBackClick;
   Control.parent:=Self;
   LControlsToFree.Add(Control);

   Control2:=TButton.Create(Self);
   Control2.Left:=Control.Width;
   Control2.Top:=posy;
   Control2.Height:=aheight;
   Control2.Width:=(TotalWidth-CONS_RIGHTBARGAP) div 2;
   Control2.parent:=Self;
   TButton(Control2).OnClick:=BringToFrontClick;
   TBUtton(Control2).Caption:=SRpBringToFront;
   LControlsToFree.Add(Control2);
 //  posy:=posy+control.height;
  end;
 end;
end;

constructor TFObjInsp.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 fchangesize:=TRpSizeModifier.Create(Self);
 fchangesize.OnSizeChange:=changesizechange;

 LNames:=TStringList.Create;
 LValues:=TStringList.Create;
 LTypes:=TStringList.Create;

 LLabels:=TList.Create;
 LControls:=TStringList.Create;
 LCOntrolsToFree:=TList.Create;
{$IFDEF LINUX}
 Font.Size:=10;
{$ENDIF}
end;

destructor TFObjInsp.Destroy;
begin
 LNames.free;
 LValues.free;
 LTypes.free;
 LLabels.free;
 LControls.free;
 LControlsToFree.Free;
 inherited Destroy;
end;

procedure TFObjInsp.EditChange(Sender:TObject);
var
 index:integer;
 aname:string;
begin
 index:=TControl(Sender).tag;
 aname:=Lnames.strings[index];
 FCompItem.SetProperty(aname,TEdit(Sender).Text);
 if Assigned(FDesignFrame) then
  TFDesignFrame(FDesignFrame).UpdateInterface;
 // If the property es positional update position
 if Assigned(fchangesize) then
 begin
  if ((aname=SRpSWidth) or (aname=SRpsHeight) or
   (aname=SRpSTop) or (aname=SRpSLeft)) then
  begin
   fchangesize.UpdatePos;
  end;
 end;
end;

procedure TFObjInsp.ShapeMouseUp(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
var
 AShape:TShape;
begin
 AShape:=TShape(Sender);
 ColorDialog1.COlor:=StrToInt(LValues.Strings[AShape.Tag]);
 if ColorDialog1.Execute then
 begin
  AShape.Brush.Color:=ColorDialog1.Color;
  FCompItem.SetProperty(Lnames.strings[AShape.Tag],IntToStr(ColorDialog1.Color));
 end;
end;

procedure TFObjInsp.FontClick(Sender:TObject);
var
 index:integer;
begin
{$IFDEF MSWINDOWS}
 FontDialog1.Font.Name:= CompItem.GetProperty(SRpSWFontName);
{$ENDIF}
{$IFDEF LINUX}
 FontDialog1.Font.Name:= CompItem.GetProperty(SRpSLFontName);
{$ENDIF}
 FontDialog1.Font.Size:= StrToInt(CompItem.GetProperty(SRpSFontSize));
 FontDialog1.Font.Color:= StrToInt(CompItem.GetProperty(SRpSFontColor));
 FontDialog1.Font.Style:=IntegerToFontStyle(StrToInt(CompItem.GetProperty(SrpSFontStyle)));
 if FontDialog1.Execute then
 begin
  index:=TComponent(Sender).Tag;
  if index>=0 then
  begin
   TEdit(LControls.Objects[index]).Text:=FontDialog1.Font.Name;
  end;
  index:=LNames.IndexOf(SrpSFontSize);
  if index>=0 then
  begin
   TEdit(LControls.Objects[index]).Text:=IntToStr(FontDialog1.Font.Size);
  end;
  index:=LNames.IndexOf(SrpSFontColor);
  if index>=0 then
  begin
   TShape(LControls.Objects[index]).Brush.Color:=FontDialog1.Font.Color;
   CompItem.SetProperty(SRpSFontColor,IntToStr(FontDialog1.Font.Color));
  end;
  index:=LNames.IndexOf(SrpSFontStyle);
  if index>=0 then
  begin
   TEdit(LControls.Objects[index]).Text:=IntegerFontStyleToString(FontStyleToInteger(Fontdialog1.Font.Style));
   CompItem.SetProperty(SRpSFontStyle,IntToStr(FontStyleToInteger(Fontdialog1.Font.Style)));
  end;
 end;
end;

procedure TFObjInsp.ComboObjectChange(Sender:TObject);
begin
 dontfreecombo:=true;
 try
  CompItem:=TRpSizeInterface(TComboBox(Sender).Items.Objects[TComboBox(Sender).ItemIndex]);
 finally
  dontfreecombo:=false;
 end;
end;

procedure TFObjInsp.ChangeSizeChange(Sender:TObject);
begin
 // Read bounds Values and assign
 if Not Assigned(fchangesize.Control) then
  exit;
 UpdatePosValues;
end;

procedure TFObjInsp.SendToBackClick(Sender:TObject);
var
 section:TRpSection;
 item:TRpCommonListItem;
 pitem:TRpCommonComponent;
 index:integer;
begin
 CompItem.SendToBack;
 TRpSizePosInterface(CompItem).SectionInt.SendToBack;
 pitem:=CompItem.printitem;
 section:=TRpSection(TRpSizePosInterface(CompItem).SectionInt.printitem);
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

procedure TFObjInsp.BringToFrontClick(Sender:TObject);
var
 section:TRpSection;
 item:TRpCommonListItem;
 pitem:TRpCommonComponent;
 index:integer;
begin
 CompItem.BringToFront;
 fchangesize.UpdatePos;

 pitem:=CompItem.printitem;
 section:=TRpSection(TRpSizePosInterface(CompItem).SectionInt.printitem);
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



procedure TFObjInsp.ExpressionClick(Sender:TObject);
var
 report:TRpReport;
 i:integer;
 item:TRpAliaslistItem;
begin
 report:=fmainf.report;
 try
  report.ActivateDatasets;
 except
  on E:Exception do
  begin
   ShowMessage(E.Message);
  end;
 end;

 RpAlias1.List.Clear;
 for i:=0 to report.DataInfo.Count-1 do
 begin
  item:=RpAlias1.List.Add;
  item.Alias:=report.DataInfo.Items[i].Alias;
  item.Dataset:=report.DataInfo.Items[i].Dataset;
 end;
 RpExpreDialog1.Expresion.Text:=TEdit(LControls.Objects[TButton(Sender).Tag]).Text;
 if RpExpreDialog1.Execute then
  TEdit(LControls.Objects[TButton(Sender).Tag]).Text:=Trim(RpExpreDialog1.Expresion.Text);
end;

procedure TFObjInsp.UpdatePosValues;
var
 index:integer;
 sizeposint:TRpSizePosInterface;
 NewLeft,NewTop,NewWidth,NewHeight:integer;
begin
 sizeposint:=TRpSizePosInterface(fchangesize.control);
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

procedure TFObjInsp.ImageClick(Sender:TObject);
var
 Stream:TMemoryStream;
begin
 if OpenDialog1.Execute then
 begin
  Stream:=TMemoryStream.Create;
  try
   Stream.LoadFromFile(OpenDialog1.FileName);
   Stream.Seek(soFromBeginning,0);
   CompItem.SetProperty(LNames.Strings[TComponent(Sender).Tag],stream);
  finally
   Stream.Free;
  end;
 end;
end;

procedure TFObjInsp.ImageKeyDown(Sender: TObject;
     var Key: Word; Shift: TShiftState);
var
 Stream:TMemoryStream;
begin
 if ((Key=Key_BackTab) or (Key=Key_Delete)) then
 begin
  Stream:=TMemoryStream.Create;
  try
   CompItem.SetProperty(LNames.Strings[TComponent(Sender).Tag],stream);
  finally
   Stream.Free;
  end;
 end;
end;

end.
