{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpfparamsvcl                                    }
{                                                       }
{       Parameter definition form                       }
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

unit rpfparamsvcl;

interface

{$I rpconf.inc}

uses SysUtils, Classes,
  Graphics, Forms,Dialogs, ActnList, ImgList, ComCtrls,
  Buttons, ExtCtrls, Controls, StdCtrls,Mask,
  rpdatainfo,
{$IFDEF USEVARIANTS}
  Variants,
{$ENDIF}
  DB,rpmdconsts,rpparams,
  rpgraphutilsvcl, ToolWin,rptypes, rpmaskedit;

type
  TFRpParamsVCL = class(TForm)
    Panel1: TPanel;
    GProperties: TGroupBox;
    LDescription: TLabel;
    EDescription: TEdit;
    LDataType: TLabel;
    ComboDataType: TComboBox;
    LValue: TLabel;
    EValue: TRpMaskEdit;
    CheckVisible: TCheckBox;
    CheckNull: TCheckBox;
    LAssign: TLabel;
    ComboDatasets: TComboBox;
    BAdddata: TButton;
    BDeleteData: TButton;
    LDatasets: TListBox;
    Panel2: TPanel;
    LParams: TListBox;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    ANewParam: TAction;
    ADelete: TAction;
    AUp: TAction;
    ADown: TAction;
    ARename: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    Panel3: TPanel;
    BCancel: TButton;
    BOK: TButton;
    ESearch: TEdit;
    LSearch: TLabel;
    GValues: TGroupBox;
    MItems: TMemo;
    MValues: TMemo;
    CheckAllowNulls: TCheckBox;
    EHint: TEdit;
    LHint: TLabel;
    CheckNeverVisible: TCheckBox;
    CheckReadOnly: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LParamsClick(Sender: TObject);
    procedure EValueExit(Sender: TObject);
    procedure EDescriptionChange(Sender: TObject);
    procedure BAdddataClick(Sender: TObject);
    procedure BDeleteDataClick(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BRenameClick(Sender: TObject);
    procedure BUpClick(Sender: TObject);
    procedure BDownClick(Sender: TObject);
  private
    { Private declarations }
    updating:boolean;
    params:TRpParamList;
    datainfo:TRpDatainfoList;
    dook:boolean;
    procedure FillParamList;
    procedure UpdateValue(param:TRpParam);
  public
    { Public declarations }
  end;


procedure ShowParamDef(params:TRpParamList;datainfo:TRpDatainfoList);

implementation

{$R *.dfm}

procedure ShowParamDef(params:TRpParamList;datainfo:TRpDatainfoList);
var
 dia:TFRpParamsVCL;
begin
 dia:=TFRpParamsVCL.Create(Application);
 try
  dia.params.Assign(params);
  dia.datainfo:=datainfo;
  dia.ShowModal;
  if dia.dook then
   params.assign(dia.params);
 finally
  dia.free;
 end;
end;

procedure TFRpParamsVCL.FormCreate(Sender: TObject);
begin
 params:=TRpParamList.Create(Self);

 BOK.Caption:=TranslateStr(93,BOK.Caption);
 BCancel.Caption:=TranslateStr(94,BCancel.Caption);

 ANewParam.Caption:=TranslateStr(186,ANewParam.Caption);
 ANewParam.Hint:=TranslateStr(187,ANewParam.Hint);
 ADelete.Caption:=TranslateStr(188,ADelete.Caption);
 ADelete.Hint:=TranslateStr(189,ADelete.Hint);
 AUp.Hint:=TranslateStr(190,AUp.Hint);
 ADown.Hint:=TranslateStr(191,ADown.Hint);
 ARename.Hint:=TranslateStr(192,ARename.Hint);
 LDataType.Caption:=TranslateStr(193,LDatatype.Caption);
 LValue.Caption:=TranslateStr(194,LValue.Caption);
 CheckVisible.Caption:=TranslateStr(195,CheckVisible.Caption);
 CheckVisible.Hint:=TranslateStr(952,CheckVisible.Caption);
 CheckAllowNulls.Caption:=SRpAllowNulls;
 CheckAllowNulls.Hint:=SRpAllowNullsHint;
 CheckNull.Caption:=TranslateStr(196,CheckNull.Caption);
 LDescription.Caption:=TranslateStr(197,LDescription.Caption);
 LAssign.Caption:=TranslateStr(198,LAssign.Caption);
 Caption:=TranslateStr(199,Caption);
 GetPossibleDataTypesA(ComboDataType.Items);
 ComboDataType.Hint:=TranslateStr(944,ComboDataType.Hint);
 CheckNull.Hint:=TranslateStr(945,CheckNull.Hint);
 LSearch.Caption:=TranslateStr(946,CheckNull.Hint);
 ComboDatasets.Hint:=TranslateStr(947,ComboDatasets.Hint);
 BAddData.Hint:=TranslateStr(948,BAddData.Hint);
 BDeleteData.Hint:=TranslateStr(949,BDeleteData.Hint);
 LDatasets.Hint:=TranslateStr(950,LDatasets.Hint);
 GValues.Caption:=SRpSParamListDesc;
end;

procedure TFRpParamsVCL.BOKClick(Sender: TObject);
begin
 if EValue.Visible then
 begin
  if GProperties.Visible then
   EValueExit(Self);
 end;
 dook:=true;
 close;
end;

procedure TFRpParamsVCL.FormShow(Sender: TObject);
var
 i:integer;
begin
 if Assigned(datainfo) then
 begin
  for i:=0 to datainfo.count-1 do
  begin
   ComboDatasets.Items.Add(datainfo.items[i].Alias);
  end;
  if ComboDatasets.Items.Count>0 then
   ComboDatasets.ItemIndex:=0;
 end;
 FillParamList;
end;

procedure TFRpParamsVCL.FillParamList;
var
 i:integer;
begin
 LParams.Clear;
 for i:=0 to params.Count-1 do
 begin
  LParams.Items.Add(params.items[i].Name);
 end;
 LParamsClick(Self);
end;


procedure TFRpParamsVCL.LParamsClick(Sender: TObject);
var
 param:TRpParam;
begin
 if (LParams.Items.Count<1) then
 begin
  GProperties.Visible:=false;
  exit;
 end;
 updating:=true;
 try
  if LParams.Itemindex<0 then
   LParams.ItemIndex:=0;
  GProperties.Visible:=True;
  param:=params.ParamByName(LParams.Items.Strings[LParams.Itemindex]);
  CheckVisible.Checked:=param.Visible;
  CheckNeverVisible.Checked:=param.NeverVisible;
  CheckReadOnly.Checked:=param.IsReadOnly;
  CheckAllowNulls.Checked:=param.AllowNulls;
   CheckNull.Checked:=param.Value=Null;
  EDescription.Text:=param.Description;
  EHint.Text:=param.Hint;
  ESearch.Text:=param.Search;
  MValues.Lines.Assign(param.Values);
  MItems.Lines.Assign(param.Items);
  LDatasets.Clear;
  LDatasets.items.Assign(param.Datasets);
  if LDatasets.items.count>0 then
   LDatasets.ItemIndex:=0;

  ComboDataType.ItemIndex:=
   ComboDataType.Items.IndexOf(ParamTypeToString(param.ParamType));
  EValue.EditType:=teGeneral;
  EValue.Text:='';
  if (param.Value<>Null) then
  begin
   case param.ParamType of
    rpParamString,rpParamExpreA,rpParamExpreB,rpParamSubst,rpParamList,rpParamUnknown:
     EValue.Text:=param.AsString;
    rpParamInteger:
     begin
      EValue.Text:=IntToStr(param.Value);
      EValue.EditType:=teInteger;
     end;
    rpParamDouble:
     begin
      EValue.Text:=FloatToStr(param.Value);
      EValue.EditType:=teFloat;
     end;
    rpParamCurrency:
     begin
      EValue.Text:=CurrToStr(param.Value);
      EValue.EditType:=teCurrency;
     end;
    rpParamDate:
     EValue.Text:=DateToStr(param.Value);
    rpParamTime:
     EValue.Text:=TimeToStr(param.Value);
    rpParamDateTime:
     EValue.Text:=DateTimeToStr(param.Value);
    rpParamBool:
     EValue.Text:=BoolToStr(param.Value,true);
   end;
  end;
 finally
  updating:=false;
 end;
 EDescriptionChange(CheckNull);
end;

procedure TFRpParamsVCL.EValueExit(Sender: TObject);
var
 param:TRpParam;
begin
 // Validate the input value
 if (LParams.Itemindex<0) then
  exit;
 param:=params.ParamByName(LParams.items.strings[LParams.ItemIndex]);
 UpdateValue(param);
end;

procedure TFRpParamsVCL.UpdateValue(param:TRpParam);
begin
 ESearch.Visible:=param.ParamType=rpParamSubst;
 GValues.Visible:=param.ParamType=rpParamList;
 LSearch.Visible:=ESearch.Visible;
 if (EValue.Text='') then
 begin
  case param.ParamType of
   rpParamString,rpParamExpreA,rpParamExpreB,rpParamSubst,rpParamList,rpParamUnknown:
    EValue.Text:='';
   rpParamInteger:
    EValue.Text:=IntToStr(0);
   rpParamDouble:
    EValue.Text:=FloatToStr(0.0);
   rpParamCurrency:
    EValue.Text:=CurrToStr(0.0);
   rpParamDate:
    EValue.Text:=DateToStr(Date);
   rpParamTime:
    EValue.Text:=TimeToStr(Time);
   rpParamDateTime:
    EValue.Text:=DateTimeToStr(Now);
   rpParamBool:
    EValue.Text:=BoolToStr(False);
  end;
 end;
 if CheckNull.Checked then
 begin
  param.Value:=null;
  EValue.Visible:=false;
 end
 else
 begin
   EValue.Visible:=true;
   case param.ParamType of
    rpParamString,rpParamExpreA,rpParamExpreB,rpParamSubst,rpParamList,rpParamUnknown:
     param.Value:=EValue.Text;
    rpParamInteger:
     param.Value:=StrToInt(EValue.Text);
    rpParamDouble:
     param.Value:=StrToFloat(EValue.Text);
    rpParamCurrency:
     param.Value:=StrToCurr(EValue.Text);
    rpParamDate:
     param.Value:=StrToDate(EValue.Text);
    rpParamTime:
     param.Value:=StrToTime(EValue.Text);
    rpParamDateTime:
     param.Value:=StrToDateTime(EValue.Text);
    rpParamBool:
     param.Value:=StrToBool(EValue.Text);
   end;
 end;
end;

procedure TFRpParamsVCL.EDescriptionChange(Sender: TObject);
var
 param:TRpParam;
begin
 if updating then
  exit;
 // Validate the input value
 if (LParams.Itemindex<0) then
  exit;
 param:=params.ParamByName(LParams.items.strings[LParams.ItemIndex]);
 if Sender=EDescription then
  param.Description:=EDescription.Text
 else
 if Sender=EHint then
  param.Hint:=EHint.Text
 else
 if Sender=ESearch then
  param.Search:=ESearch.Text
 else
 if Sender=MItems then
  param.Items:=MItems.Lines
 else
 if Sender=MValues then
  param.Values:=MValues.Lines
 else
  if (Sender=CheckVisible) then
   param.Visible:=CheckVisible.Checked
  else
  if (Sender=CheckNeverVisible) then
   param.NeverVisible:=CheckneverVisible.Checked
  else
  if (Sender=CheckReadOnly) then
   param.IsReadOnly:=CheckReadOnly.Checked
  else
  if (Sender=CheckAllowNulls) then
   param.AllowNulls:=CheckAllowNulls.Checked
  else
   if (Sender=CheckNull) then
   begin
    UpdateValue(param);
    if CheckNull.Checked then
     param.Value:=null;
   end
   else
    if (Sender=ComboDataType) then
    begin
     if (param.ParamType=StringToParamType(COmboDataType.Text)) then
      exit;
     param.ParamType:=StringToParamType(COmboDataType.Text);
     EValue.Text:='';
     UpdateValue(param);
    end;
end;

procedure TFRpParamsVCL.BAdddataClick(Sender: TObject);
var
 index:integer;
 param:TRpParam;
begin
 if ComboDatasets.ItemIndex<0 then
  exit;
 param:=params.ParamByName(LParams.items.strings[LParams.ItemIndex]);
 index:=LDatasets.Items.IndexOf(ComboDatasets.Text);
 if index>=0 then
  exit;
 LDatasets.items.Add(COmboDatasets.Text);
 if LDatasets.itemindex<0 then
  LDatasets.ItemIndex:=0;
 param.Datasets.Assign(LDatasets.Items);
end;

procedure TFRpParamsVCL.BDeleteDataClick(Sender: TObject);
var
 param:TRpParam;
begin
 if LDatasets.itemindex<0 then
  exit;
 param:=params.ParamByName(LParams.items.strings[LParams.ItemIndex]);
 LDatasets.Items.Delete(LDatasets.ItemIndex);
 if LDatasets.items.count>0 then
  LDatasets.ItemIndex:=0;
 param.Datasets.Assign(LDatasets.Items);
end;

procedure TFRpParamsVCL.BAddClick(Sender: TObject);
var
 paramname:string;
 aparam:TRpParam;
begin
 paramname:=RpInputBox(SRpNewParam,SRpParamName,'');
 paramname:=AnsiUpperCase(Trim(paramname));
 if Length(paramname)<1 then
  exit;

 // Adds a param
 aparam:=params.Add(paramname);
 aparam.AllowNulls:=false;
 aparam.Value:='';

 FillParamList;
 LParams.ItemIndex:=LParams.Items.Count-1;
 LParamsClick(Self);
end;

procedure TFRpParamsVCL.BDeleteClick(Sender: TObject);
var
 index:integer;
begin
 if LParams.itemindex<0 then
  exit;
 index:=params.IndexOf(LParams.Items.strings[LParams.Itemindex]);
 params.Delete(index);
 FillParamList;
end;

procedure TFRpParamsVCL.BRenameClick(Sender: TObject);
var
 paramname:string;
 index:integer;
 param:TRpParam;
begin
 if LParams.itemindex<0 then
  exit;
 param:=params.ParamByName(LParams.Items.strings[LParams.Itemindex]);
 paramname:=RpInputBox(SRpRenameParam,SRpParamName,param.Name);
 paramname:=AnsiUpperCase(Trim(paramname));

 index:=params.IndexOf(paramname);
 if index>=0 then
   Raise Exception.Create(SRpParamNameExists);
 param.Name:=paramname;
 LParams.Items.strings[LParams.Itemindex]:=paramname;
end;

procedure TFRpParamsVCL.BUpClick(Sender: TObject);
var
 index:integer;
 reftemp:TRpParamList;
 aname:string;
begin
 if LParams.ItemIndex<0 then
  exit;
 if LParams.Items.count<2 then
  exit;
 index:=LParams.itemindex;
 if index<1 then
  exit;
 aname:=LParams.items.Strings[index];
 reftemp:=TRpParamList.create(Self);
 try
  reftemp.assign(params);
  // intercanviem
  reftemp.Items[index-1].assign(params.items[index]);
  reftemp.items[index].Assign(params.items[index-1]);
  params.Assign(reftemp);
 finally
  reftemp.free;
 end;
 FillParamList;
 index:=LParams.Items.IndexOf(aname);
 if index>=0 then
 begin
  LParams.itemindex:=index;
  LParamsclick(self);
 end;
end;


procedure TFRpParamsVCL.BDownClick(Sender: TObject);
var
 index:integer;
 reftemp:TRpParamList;
 aname:string;
begin
 if LParams.ItemIndex<0 then
  exit;
 if LParams.Items.count<2 then
  exit;
 index:=LParams.itemindex;
 if (index>=LParams.items.count-1) then
  exit;
 aname:=LParams.items.Strings[index];
 reftemp:=TRpParamList.create(Self);
 try
  reftemp.assign(params);
  // interchange
  reftemp.Items[index+1].assign(params.items[index]);
  reftemp.items[index].Assign(params.items[index+1]);
  params.Assign(reftemp);
 finally
  reftemp.free;
 end;
 FillParamList;
 index:=LParams.Items.IndexOf(aname);
 if index>=0 then
 begin
  LParams.itemindex:=index;
  LParamsclick(self);
 end;
end;

end.
