{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       FSampledata                                     }
{       Show data of a unidirectional query             }
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

unit fsampledata;

interface

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,DB, QDBCtrls, QGrids, QDBGrids,
{$IFNDEF PROFILE}  QComCtrls;{$ENDIF}
{$IFDEF PROFILE}  QComCtrls ,Proftimx;{$ENDIF}

const
 DCONTROL_DISTANCEY=5;
 DCONTROL_DISTANCEX=10;
 DCONTROL_DISTANCEX2=150;
 DCONTROL_WIDTHX=200;
 DLABEL_INCY=1;

type
  TFShowSampledata = class(TForm)
    DataSource1: TDataSource;
    ToolBar1: TToolBar;
    DBNavigator1: TDBNavigator;
    ScrollBox1: TScrollBox;
  private
    { Private declarations }
    procedure CreateControls;
  public
    { Public declarations }
  end;

procedure ShowDataset(Data:TDataset);

implementation

{$R *.xfm}
procedure ShowDataset(Data:TDataset);
var
 dia:TFShowSampledata;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,115; xor eax,eax; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 dia:=TFShowSampledata.Create(Application);
 try
  dia.DataSource1.DataSet:=data;
  dia.CreateControls;
  dia.ShowModal;
 finally
  dia.free;
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,115; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFShowSampledata.CreateControls;
var
 i:integer;
 dataset:TDataset;
 label1:TLabel;
 Control:TControl;
 top:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,116; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 if Not Assigned(Datasource1.dataset) then
  exit;
 if not Datasource1.dataset.active then
  exit;
 dataset:=Datasource1.dataset;
 top:=DCONTROL_DISTANCEY;
 for i:=0 to dataset.FieldCount-1 do
 begin
  label1:=Tlabel.Create(self);
  label1.Top:=top+DLABEL_INCY;
  label1.Left:=DCONTROL_DISTANCEX;
  label1.caption:=Dataset.fields[i].FieldName;
  label1.Parent:=ScrollBox1;

  control:=TDBTExt.Create(self);
  TDBText(control).Font.Style:=[fsBold];
  TDBText(control).Datasource:=datasource1;
  TDBText(control).DataField:=Dataset.fields[i].FieldName;

  control.top:=top;
  control.left:=DCONTROL_DISTANCEX2;
  control.Width:=DCONTROL_WIDTHX;
  control.Height:=Canvas.TextHeight('Wg');
  control.parent:=SCrollbox1;

  top:=top+Control.Height+DCONTROL_DISTANCEY;
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,116; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


end.
