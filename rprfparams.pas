{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rprfparams                                      }
{                                                       }
{       User parameters form                            }
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

unit rprfparams;

interface

{$I rpconf.inc}

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,
  rpconsts,
{$IFDEF USEVARIANTS}
  Variants,
{$ENDIF}
  rpreport,rpparams;

const
  CONS_LEFTGAP=3;
  CONS_CONTROLPOS=360;
  CONS_LABELTOPGAP=2;
  CONS_CONTROLGAP=5;
  CONS_RIGHTBARGAP=25;
  CONS_NULLWIDTH=40;

type
  TFRpRunTimeParams = class(TForm)
    PModalButtons: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    MainScrollBox: TScrollBox;
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    fparams:TRpParamList;
    dook:boolean;
    lnulls,lcontrols:TStringList;
    procedure SetParams(avalue:TRpParamList);
    procedure SaveParams;
  public
    { Public declarations }
    procedure CheckNullClick(Sender:TObject);
    property params:TRpParamList read fparams write Setparams;
  end;


function ShowUserParams(report:TRpReport):boolean;

implementation

{$R *.xfm}

function ShowUserParams(report:TRpReport):boolean;
var
 dia:TFRpRunTimeParams;
 oneparam:boolean;
 i:integer;
begin
 Result:=false;
 oneparam:=false;
 for i:=0 to report.params.count-1 do
 begin
  if report.params.items[i].Visible then
  begin
   oneparam:=true;
   break;
  end;
 end;
 if not oneparam then
 begin
  Result:=true;
  exit;
 end;
 dia:=TFRpRunTimeParams.Create(Application);
 try
  dia.params:=report.Params;
  dia.showmodal;
  if dia.dook then
  begin
   report.params.Assign(dia.Params);
   Result:=true;
  end;
 finally
  dia.Free;
 end;
end;

procedure TFRpRunTimeParams.OKBtnClick(Sender: TObject);
begin
 SaveParams;
 dook:=true;
 close;
end;

procedure TFRpRunTimeParams.FormCreate(Sender: TObject);
begin
 fparams:=TRpParamList.Create(Self);
 lcontrols:=TStringList.Create;
 lnulls:=TStringList.Create;
end;

procedure TFRpRunTimeParams.SetParams(avalue:TRpParamList);
var
 i:integer;
 alabel:TLabel;
 acontrol:TControl;
 posy:integer;
 aparam:TRpParam;
 TotalWidth:integer;
 achecknull:TCheckBox;
begin
 acontrol:=nil;
 fparams.assign(avalue);
 TotalWidth:=MainScrollBox.Width-CONS_NULLWIDTH-CONS_RIGHTBARGAP;
 posy:=CONS_CONTROLGAP;
 // Creates all controls from params
 for i:=0 to fparams.Count-1 do
 begin
  aparam:=fparams.Items[i];
  if aparam.Visible then
  begin
   alabel:=TLabel.Create(Self);
   alabel.Caption:=aparam.Description;
   aLabel.Left:=CONS_LEFTGAP;
   aLabel.Top:=posy+CONS_LABELTOPGAP;
   alabel.Parent:=MainScrollBox;
   achecknull:=TCheckBox.Create(Self);
   achecknull.Left:=TotalWidth-CONS_NULLWIDTH;
   achecknull.Top:=posy;
   achecknull.Tag:=i;
   achecknull.Width:=CONS_NULLWIDTH;
   achecknull.Left:=TotalWidth;
   achecknull.Caption:=SRpNull;
   achecknull.Parent:=MainScrollBox;
   achecknull.OnClick:=CheckNullClick;
   lnulls.AddObject(aparam.Name,acheckNull);
   case aparam.ParamType of
    rpParamString:
     begin
      acontrol:=TEdit.Create(Self);
      acontrol.tag:=i;
      lcontrols.AddObject(aparam.Name,acontrol);
      TEdit(acontrol).Text:='';
      if aparam.Value=Null then
      begin
       achecknull.Checked:=true;
      end
      else
      begin
       TEdit(acontrol).Text:=aparam.Value;
      end;
     end;
   rpParamInteger,rpParamDouble,rpParamCurrency:
     begin
      acontrol:=TEdit.Create(Self);
      acontrol.tag:=i;
      lcontrols.AddObject(aparam.Name,acontrol);
      TEdit(acontrol).Text:='0';
      if aparam.Value=Null then
      begin
       achecknull.Checked:=true;
      end
      else
      begin
       TEdit(acontrol).Text:=VarToStr(aparam.Value);
      end;
     end;
   rpParamDate:
     begin
      acontrol:=TEdit.Create(Self);
      acontrol.tag:=i;
      lcontrols.AddObject(aparam.Name,acontrol);
      TEdit(acontrol).Text:=DateToStr(Date);
      if aparam.Value=Null then
      begin
       achecknull.Checked:=true;
      end
      else
      begin
       TEdit(acontrol).Text:=VarToStr(aparam.Value);
      end;
     end;
   rpParamTime:
     begin
      acontrol:=TEdit.Create(Self);
      acontrol.tag:=i;
      lcontrols.AddObject(aparam.Name,acontrol);
      TEdit(acontrol).Text:=TimeToStr(Time);
      if aparam.Value=Null then
      begin
       achecknull.Checked:=true;
      end
      else
      begin
       TEdit(acontrol).Text:=VarToStr(aparam.Value);
      end;
     end;
   rpParamDateTime:
     begin
      acontrol:=TEdit.Create(Self);
      acontrol.tag:=i;
      lcontrols.AddObject(aparam.Name,acontrol);
      TEdit(acontrol).Text:=DateTimeToStr(now);
      if aparam.Value=Null then
      begin
       achecknull.Checked:=true;
      end
      else
      begin
       TEdit(acontrol).Text:=VarToStr(aparam.Value);
      end;
     end;
   rpParamBool:
     begin
      acontrol:=TComboBox.Create(Self);
      acontrol.tag:=i;
      lcontrols.AddObject(aparam.Name,acontrol);
      TComboBox(acontrol).Style:=csDropDownList;
      TComboBox(acontrol).Items.Add(BoolToStr(false,true));
      TComboBox(acontrol).Items.Add(BoolToStr(true,true));
      TComboBox(acontrol).ItemIndex:=0;
      if aparam.Value=Null then
      begin
       achecknull.Checked:=true;
      end
      else
      begin
       if aparam.value then
        TComboBox(acontrol).ItemIndex:=1
       else
        TComboBox(acontrol).ItemIndex:=0;
      end;
     end;
   end;
   acontrol.Top:=Posy;
   acontrol.Left:=CONS_CONTROLPOS;
   acontrol.Width:=TotalWidth-acontrol.Left;
   acontrol.parent:=self;
   Posy:=PosY+acontrol.Height+CONS_CONTROLGAP;
  end;
 end;
 // Set the height of the form
 ClientHeight:=PModalButtons.Height+PosY+CONS_CONTROLGAP;
 SetInitialBounds;
end;

procedure TFRpRunTimeParams.FormDestroy(Sender: TObject);
begin
 lcontrols.free;
 lnulls.free;
end;

procedure TFRpRunTimeParams.CheckNullClick(Sender:TObject);
begin
 if TCheckBox(Sender).Checked then
 begin
  TControl(lcontrols.Objects[TCheckBox(Sender).Tag]).Visible:=false;
 end
 else
 begin
  TControl(lcontrols.Objects[TCheckBox(Sender).Tag]).Visible:=true;
 end;
end;

procedure TFRpRunTimeParams.SaveParams;
begin

end;

end.
