unit fsampledata;

interface

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,DB, QDBCtrls, QGrids, QDBGrids,
  QComCtrls;

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
 dia:=TFShowSampledata.Create(Application);
 try
  dia.DataSource1.DataSet:=data;
  dia.CreateControls;
  dia.ShowModal;
 finally
  dia.free;
 end;
end;

procedure TFShowSampledata.CreateControls;
var
 i:integer;
 dataset:TDataset;
 label1:TLabel;
 Control:TControl;
 top:integer;
begin
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
end;


end.
