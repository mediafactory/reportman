{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpeditconnvcl                                   }
{                                                       }
{                                                       }
{       Connection List editor, VCL version             }
{                                                       }
{       Copyright (c) 1994-2003 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rpeditconnvcl;

interface

{$I rpconf.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, ActnList,rpalias,
  DB,rpdatainfo,rpdbxconfigvcl,rpmdconsts,
{$IFDEF USEADO}
  adodb,
{$ENDIF}
  ImgList, StdCtrls;

type
  TFRpEditConVCL = class(TForm)
    ImageList1: TImageList;
    ActionList1: TActionList;
    ANewParam: TAction;
    ADelete: TAction;
    ARename: TAction;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton5: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    PBottom: TPanel;
    BOK: TButton;
    BCancel: TButton;
    PConnections: TPanel;
    Splitter2: TSplitter;
    LConnections: TListBox;
    PCon2: TPanel;
    EReportGroupsTable: TEdit;
    EReportSearchfield: TEdit;
    EReportField: TEdit;
    EReportTable: TEdit;
    ComboDriver: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    CheckLoginPrompt: TCheckBox;
    CheckLoadDriverParams: TCheckBox;
    CheckLoadParams: TCheckBox;
    BConfig: TButton;
    EAdoConnection: TEdit;
    Label1: TLabel;
    Button1: TButton;
    BTest: TButton;
    BCreateLib: TButton;
    BBrowse: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ANewParamExecute(Sender: TObject);
    procedure LAliasesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ADeleteExecute(Sender: TObject);
    procedure ARenameExecute(Sender: TObject);
    procedure EReportTableChange(Sender: TObject);
    procedure BConfigClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BTestClick(Sender: TObject);
    procedure BCreateLibClick(Sender: TObject);
    procedure BBrowseClick(Sender: TObject);
  private
    { Private declarations }
    rpalias1:TRpAlias;
    procedure UpdateConList;
  public
    { Public declarations }
  end;


function ShowModifyConnections(Connections:TRpDatabaseInfoList):Boolean;


implementation

uses rpmdfopenlibvcl;

{$R *.DFM}


function ShowModifyConnections(Connections:TRpDatabaseInfoList):Boolean;
var
 dia:TFRpEditConVCL;
begin
 Result:=False;
 dia:=TFRpEditConVCL.Create(Application);
 try
  dia.rpalias1.Connections.Assign(Connections);
  if (dia.ShowModal=mrOK) then
  begin
   Result:=True;
   Connections.Assign(dia.rpalias1.Connections);
  end;
 finally
  dia.free;
 end;
end;



procedure TFRpEditConVCL.ANewParamExecute(Sender: TObject);
var
 aname:String;
begin
 aname:=InputBox('New connection','Connection name','');
 aname:=Trim(UpperCase(aname));
 if rpalias1.Connections.indexof(aname)>0 then
  Raise Exception.Create('Connection name already exists');
 rpalias1.Connections.Add(aname);
 UpdateConList;
 LConnections.ItemIndex:=LConnections.Items.Count-1;
 LAliasesClick(Self);
end;




procedure TFRpEditConVCL.UpdateConList;
var
 i:integer;
begin
 LConnections.Clear;
 for i:=0 to rpalias1.Connections.count-1 do
 begin
  LConnections.Items.Add(rpalias1.Connections.Items[i].Alias);
 end;
end;


procedure TFRpEditConVCL.FormShow(Sender: TObject);
begin
 PConnections.Visible:=True;
 PConnections.Align:=alClient;
 UpdateConList;
 GetRpDatabaseDrivers(ComboDriver.Items);
 if LConnections.Items.Count>0 then
  LConnections.ItemIndex:=0
 else
  LConnections.ItemIndex:=-1;
 LAliasesClick(Self);
end;




procedure TFRpEditConVCL.ADeleteExecute(Sender: TObject);
var
 oldindex:integer;
begin
 if LConnections.Items.Count<1 then
 begin
  PCon2.Visible:=False;
  exit;
 end;
 if LConnections.ItemIndex<0 then
 begin
  PCon2.Visible:=False;
  exit;
 end;
 oldindex:=LConnections.ItemIndex;
 rpalias1.Connections.Delete(LConnections.ItemIndex);
 UpdateConList;
 dec(oldindex);
 if oldindex<0 then
  oldindex:=0;
 if LConnections.Items.Count>0 then
 begin
  LConnections.ItemIndex:=oldindex;
  LAliasesClick(Self);
 end;
end;

procedure TFRpEditConVCL.LAliasesClick(Sender: TObject);
var
 dbitem:TRpDatabaseInfoItem;
begin
 if LConnections.Items.Count<1 then
 begin
  PCon2.Visible:=False;
  exit;
 end;
 if LConnections.ItemIndex<0 then
 begin
  PCon2.Visible:=False;
  exit;
 end;
 dbitem:=rpalias1.Connections.Items[LConnections.ItemIndex];
 ComboDriver.ItemIndex:=Integer(dbitem.Driver);
 CheckLoadParams.Checked:=dbitem.LoadParams;
 CheckLoadDriverParams.Checked:=dbitem.LoadDriverParams;
 CheckLoginPrompt.Checked:=dbitem.LoginPrompt;
 EReportTable.TExt:=dbitem.ReportTable;
 EReportField.Text:=dbitem.ReportField;
 EReportSearchfield.Text:=dbitem.ReportSearchField;
 EReportGroupsTable.Text:=dbitem.ReportGroupsTable;
 EAdoConnection.Text:=dbitem.ADOConnectionString;

 PCon2.Visible:=True;
end;



procedure TFRpEditConVCL.ARenameExecute(Sender: TObject);
var
 aname:String;
 adbitem:TRpDatabaseInfoItem;
begin
 if LConnections.Items.Count<1 then
  exit;
 if LConnections.ItemIndex<0 then
  exit;
 aname:=InputBox('Rename connection','New connection name','');
 aname:=Trim(UpperCase(aname));
 if rpalias1.Connections.indexof(aname)>0 then
  Raise Exception.Create('Alias name already exists');
 adbitem:=rpalias1.Connections.Items[LConnections.ItemIndex];
 adbitem.Alias:=aname;
 UpdateConList;
 LConnections.ItemIndex:=LConnections.Items.IndexOf(aname);
 LAliasesClick(Self);
end;

procedure TFRpEditConVCL.EReportTableChange(Sender: TObject);
var
 dbitem:TRpDatabaseInfoItem;
begin
 // Change any data
 if LConnections.Items.Count<1 then
  exit;
 if LConnections.ItemIndex<0 then
  exit;
 dbitem:=rpalias1.Connections.Items[LConnections.ItemIndex];
 if Sender=CheckLoadParams then
  dbitem.LoadParams:=CheckLoadParams.Checked
 else
 if Sender=CheckLoadDriverParams then
  dbitem.LoadDriverParams:=CheckLoadDriverParams.Checked
 else
 if Sender=CheckLoginPrompt then
  dbitem.LoginPrompt:=CheckLoginPrompt.Checked
 else
 if Sender=ComboDriver then
  dbitem.Driver:=TRpDbDriver(ComboDriver.ItemIndex)
 else
 if Sender=EReportTable then
  dbitem.ReportTable:=EReportTable.TExt
 else
 if Sender=EReportField then
  dbitem.ReportField:=EReportField.Text
 else
 if Sender=EReportSearchField then
  dbitem.ReportSearchField:=EReportSearchfield.Text
 else
 if Sender=EReportGroupsTable then
  dbitem.ReportGroupsTable:=EReportGroupsTable.Text
 else
 if Sender=EAdoConnection then
  dbitem.ADOConnectionString:=EAdoConnection.Text;
end;

procedure TFRpEditConVCL.FormCreate(Sender: TObject);
begin
 rpalias1:=TRpAlias.Create(Self);
end;


procedure TFRpEditConVCL.BConfigClick(Sender: TObject);
begin
 ShowDBXConfig(TRpDbDriver(ComboDriver.ItemIndex) in [rpdataibx,rpdataibo,rpdatamybase]);
 conadmin.free;
 conadmin:=TRPCOnnAdmin.Create;
// conadmin.GetConnectionNames(ComboAvailable.Items,'');
end;

procedure TFRpEditConVCL.Button1Click(Sender: TObject);
begin
{$IFDEF USEADO}
  if LConnections.ItemIndex<0 then
   Raise Exception.Create(SRpSelectAddConnection);
  EADOConnection.Text:=PromptDataSource(0,EADOConnection.Text);
{$ENDIF}
end;

procedure TFRpEditConVCL.BTestClick(Sender: TObject);
var
 dbinfo:TRpDatabaseInfoItem;
begin
 dbinfo:=rpalias1.Connections.Items[LConnections.ItemIndex];
 dbinfo.Connect;
 try
  ShowMessage(SRpConnectionOk);
 finally
  dbinfo.DisConnect;
 end;
end;

procedure TFRpEditConVCL.BCreateLibClick(Sender: TObject);
var
 dbitem:TRpDatabaseinfoItem;
begin
 // Change any data
 if LConnections.Items.Count<1 then
  exit;
 if LConnections.ItemIndex<0 then
  exit;
 dbitem:=rpalias1.Connections.Items[LConnections.ItemIndex];
 dbitem.CreateLibrary(dbitem.ReportTable,dbitem.ReportField,dbitem.ReportSearchField,dbitem.ReportGroupsTable);
end;

procedure TFRpEditConVCL.BBrowseClick(Sender: TObject);
var
 dbitem:TRpDatabaseinfoItem;
begin
 // Change any data
 if LConnections.Items.Count<1 then
  exit;
 if LConnections.ItemIndex<0 then
  exit;
 dbitem:=rpalias1.Connections.Items[LConnections.ItemIndex];
 SelectReportFromLibrary(dbitem);
end;

end.
