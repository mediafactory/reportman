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
  rpdatainfo,Variants,DB,rpmdconsts,rpparams,
  rpgraphutilsvcl, ToolWin;

type
  TFRpParamsVCL = class(TForm)
    Panel1: TPanel;
    GProperties: TGroupBox;
    LDescription: TLabel;
    EDescription: TEdit;
    LDataType: TLabel;
    ComboDataType: TComboBox;
    LValue: TLabel;
    EValue: TMaskEdit;
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
 CheckNull.Caption:=TranslateStr(196,CheckNull.Caption);
 LDescription.Caption:=TranslateStr(197,LDescription.Caption);
 LAssign.Caption:=TranslateStr(198,LAssign.Caption);
 Caption:=TranslateStr(199,Caption);
 ComboDataType.Items.Strings[0]:=TranslateStr(200,ComboDataType.Items.Strings[0]);
 ComboDataType.Items.Strings[1]:=TranslateStr(201,ComboDataType.Items.Strings[1]);
 ComboDataType.Items.Strings[2]:=TranslateStr(202,ComboDataType.Items.Strings[2]);
 ComboDataType.Items.Strings[3]:=TranslateStr(203,ComboDataType.Items.Strings[3]);
 ComboDataType.Items.Strings[4]:=TranslateStr(204,ComboDataType.Items.Strings[4]);
 ComboDataType.Items.Strings[5]:=TranslateStr(205,ComboDataType.Items.Strings[5]);
 ComboDataType.Items.Strings[6]:=TranslateStr(206,ComboDataType.Items.Strings[6]);
 ComboDataType.Items.Strings[7]:=TranslateStr(207,ComboDataType.Items.Strings[7]);
 ComboDataType.Items.Strings[8]:=TranslateStr(208,ComboDataType.Items.Strings[8]);

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
   CheckNull.Checked:=param.Value=Null;
  EDescription.Text:=param.Description;
  LDatasets.Clear;
  LDatasets.items.Assign(param.Datasets);
  if LDatasets.items.count>0 then
   LDatasets.ItemIndex:=0;

  ComboDataType.ItemIndex:=Integer(param.ParamType);
  EValue.Text:='';
  if (param.Value<>Null) then
  begin
   case param.ParamType of
    rpParamString,rpParamExpre:
     EValue.Text:=param.Value;
    rpParamInteger:
     EValue.Text:=IntToStr(param.Value);
    rpParamDouble:
     EValue.Text:=FloatToStr(param.Value);
    rpParamCurrency:
     EValue.Text:=CurrToStr(param.Value);
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
 if (EValue.Text='') then
 begin
  case param.ParamType of
   rpParamString,rpParamExpre:
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
    rpParamString,rpParamExpre:
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
  if (Sender=CheckVisible) then
   param.Visible:=CheckVisible.Checked
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
     if (param.ParamType=TRpParamType(COmboDataType.ItemIndex)) then
      exit;
     param.ParamType:=TRpParamType(COmboDataType.ItemIndex);
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
begin
 paramname:=RpInputBox(SRpNewParam,SRpParamName,'');
 paramname:=AnsiUpperCase(Trim(paramname));
 if Length(paramname)<1 then
  exit;

 // Adds a param
 params.Add(paramname);
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
 paramname:=RpInputBox(SRpRenameParam,SRpParamName,'');
 paramname:=AnsiUpperCase(Trim(paramname));

 param:=params.ParamByName(LParams.Items.strings[LParams.Itemindex]);
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