{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       fdatainfo                                       }
{       Form for configuration of report datasets       }
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

unit fdatainfo;

interface

{$I rpconf.inc}

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,
{$IFDEF USEBDE}
  dbtables,
{$ENDIF}
{$IFDEF USEADO}
  adodb,
{$ENDIF}
  rpreport,rpconsts,rpdatainfo,DBConnAdmin,QDialogs,
  rpparams,rpfparams;

type
  TFDatainfoconfig = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    GroupBox1: TGroupBox;
    LConnections: TListBox;
    BAddCon: TButton;
    BDeletecon: TButton;
    Label1: TLabel;
    ComboAvailable: TComboBox;
    BConfig: TButton;
    CheckLoginPrompt: TCheckBox;
    CheckLoadParams: TCheckBox;
    CheckLoadDriverParams: TCheckBox;
    GroupBox2: TGroupBox;
    LDatasets: TListBox;
    BAdd: TButton;
    BDelete: TButton;
    BRename: TButton;
    GDataProps: TGroupBox;
    Label2: TLabel;
    ComboConnection: TComboBox;
    LMasterDataset: TLabel;
    ComboDataSource: TComboBox;
    LSQL: TLabel;
    MSQL: TMemo;
    BShowData: TButton;
    BParams: TButton;
    GDriver: TRadioGroup;
    EMyBase: TEdit;
    LMyBase: TLabel;
    BMyBase: TButton;
    OpenDialog1: TOpenDialog;
    EIndexFields: TEdit;
    LIndexFields: TLabel;
    LConnectionString: TLabel;
    EConnectionString: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BConfigClick(Sender: TObject);
    procedure BAddConClick(Sender: TObject);
    procedure LConnectionsClick(Sender: TObject);
    procedure CheckLoginPromptClick(Sender: TObject);
    procedure LDatasetsClick(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BRenameClick(Sender: TObject);
    procedure MSQLChange(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure BDeleteconClick(Sender: TObject);
    procedure BShowDataClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BParamsClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure GDriverClick(Sender: TObject);
    procedure BMyBaseClick(Sender: TObject);
    procedure EConnectionStringChange(Sender: TObject);
  private
    { Private declarations }
    report:TRpReport;
    saved:boolean;
    databaseinfo:TRpDatabaseInfoList;
    datainfo:TRpDataInfoList;
    params:TRpParamList;
    conadmin:IConnectionAdmin;
    docancel:boolean;
    procedure DoSave;
    procedure  Removedependences(oldalias:string);
    procedure FillCurrentConnections;
    procedure FillDatasets;
    function FindDatabaseInfoItem:TRpDatabaseInfoItem;
    function FindDataInfoItem:TRpDataInfoItem;
  public
    { Public declarations }
  end;

procedure ShowDataConfig(report:TRpReport);


implementation

uses rpdbxconfig, fsampledata;

{$R *.xfm}

procedure ShowDataConfig(report:TRpReport);
var
 dia:TFDataInfoConfig;
begin
{$IFDEF USECONADMIN}
 UpdateConAdmin;
{$ENDIF}
 dia:=TFDataInfoConfig.Create(Application);
 try
  dia.report:=report;
  dia.databaseinfo.Assign(report.DatabaseInfo);
  dia.datainfo.Assign(report.DataInfo);
  dia.params.Assign(report.Params);
  dia.showmodal;
 finally
  dia.free;
 end;
end;

procedure TFDatainfoconfig.FormCreate(Sender: TObject);
begin
 GDriver.ItemIndex:=0;
 GDriver.Columns:=GDriver.Items.Count;
 databaseinfo:=TRpDatabaseInfoList.Create(Self);
 params:=TRpParamList.Create(Self);
 datainfo:=TRpDataInfoList.Create(Self);
 try
  ConAdmin:=GetConnectionAdmin;
 except
  on e:Exception do
  begin
   ShowMessage(E.message);
  end;
 end;
end;

procedure TFDatainfoconfig.FormDestroy(Sender: TObject);
begin
 datainfo.free;
 databaseinfo.Free;
end;

procedure TFDatainfoconfig.FormShow(Sender: TObject);
begin
 // Fills the info
 if Assigned(ConAdmin) then
 begin
  conadmin.GetConnectionNames(ComboAvailable.Items,'');
 end;
 if ComboAvailable.Items.count>0 then
  ComboAvailable.Itemindex:=0;
 FillCurrentConnections;
 FillDatasets;
end;

procedure TFDatainfoconfig.BConfigClick(Sender: TObject);
begin
 case TRpDBDriver(GDriver.ItemIndex) of
  rpdatadbexpress:
   begin
{$IFDEF USECONADMIN}
    ShowDBXConfig;
    UpdateConAdmin;
    if Assigned(ConAdmin) then
    begin
     ConAdmin:=nil;
     try
      ConAdmin:=GetConnectionAdmin;
     except
      on e:Exception do
      begin
       ShowMessage(E.message);
      end;
     end;
     conadmin.GetConnectionNames(ComboAvailable.Items,'');
    end;
{$ENDIF}
   end;
  rpdatamybase:
   begin
    // Does nothing
   end;
  rpdataado:
   begin
     // Gets connection string
{$IFDEF USEADO}
     EConnectionString.Text:=PromptDataSource(0,EConnectionString.Text);
{$ENDIF}
   end;
 end;
 FillCurrentConnections;
end;

procedure TFDatainfoconfig.BAddConClick(Sender: TObject);
var
 conname:string;
 item:TRpDatabaseInfoItem;
begin
 if Not ComboAvailable.Visible then
 begin
  conname:=UpperCase(Trim(InputBox(SRpNewConnection,SRpConnectionName,'')));
  if Length(conname)<1 then
   exit;
  item:=databaseinfo.Add(conname);
  item.Driver:=TRpDbDriver(GDriver.ItemIndex);
  FillCurrentConnections;
  exit;
 end;
 if ComboAvailable.itemindex<0 then
  exit;
 conname:=AnsiUpperCase(ComboAvailable.Items.strings[ComboAvailable.itemindex]);
 if databaseinfo.IndexOf(conname)<0 then
 begin
  item:=databaseinfo.Add(conname);
  item.Driver:=TRpDbDriver(GDriver.ItemIndex);
  FillCurrentConnections;
 end;
end;

procedure TFDatainfoconfig.FillCurrentConnections;
var
 i:integer;
 oldtext:string;
begin
 LConnections.Clear;
 for i:=0 to databaseinfo.Count-1 do
 begin
  LConnections.Items.Add(databaseinfo.Items[i].Alias)
 end;
 if LConnections.items.Count>0 then
  LConnections.ItemIndex:=0;
 oldtext:=ComboConnection.Text;
 ComboConnection.Items.Assign(LConnections.items);
 ComboConnection.ItemIndex:=ComboCOnnection.Items.IndexOf(oldtext);
 LConnectionsClick(Self);
end;


procedure TFDatainfoconfig.LConnectionsClick(Sender: TObject);
var
 dinfoitem:TRpDatabaseinfoitem;
begin
 if LConnections.ItemIndex<0 then
 begin
  CheckLoginPrompt.Visible:=false;
  CheckLoadParams.Visible:=false;
  CheckLoadDriverParams.Visible:=false;
  exit;
 end;
 CheckLoginPrompt.Visible:=true;
 CheckLoadParams.Visible:=true;
 CheckLoadDriverParams.Visible:=true;
 dinfoitem:=FindDatabaseInfoItem;
 if Not Assigned(dinfoitem) then
  exit;
 CheckLoginPrompt.Checked:=dinfoitem.LoginPrompt;
 CheckLoadParams.Checked:=dinfoitem.LoadParams;
 CheckLoadDriverParams.Checked:=dinfoitem.LoadDriverParams;
 EConnectionString.Text:=dinfoitem.ADOConnectionString;
 GDriver.ItemIndex:=integer(dinfoitem.Driver);
 GDriverClick(Self);
end;

function TFDatainfoconfig.FindDatabaseInfoItem:TRpDatabaseInfoItem;
var
 index:integer;
begin
 Result:=nil;
 if LConnections.ItemIndex<0 then
  exit;
 index:=databaseinfo.IndexOf(LConnections.Items.Strings[LConnections.itemindex]);
 if index>=0 then
  Result:=databaseinfo.items[index];
end;

function TFDatainfoconfig.FindDataInfoItem:TRpDataInfoItem;
var
 index:integer;
begin
 Result:=nil;
 if LDatasets.ItemIndex<0 then
  exit;
 index:=datainfo.IndexOf(LDatasets.Items.Strings[LDatasets.itemindex]);
 if index>=0 then
  Result:=datainfo.items[index];
end;

procedure TFDatainfoconfig.CheckLoginPromptClick(Sender: TObject);
var
 dinfoitem:TRpDatabaseinfoitem;
begin
 dinfoitem:=FindDatabaseInfoItem;
 if Not Assigned(dinfoitem) then
  exit;
 if Sender=CheckLoginPrompt then
 begin
  dinfoitem.LoginPrompt:=CheckLoginPrompt.Checked;
 end
 else
 if Sender=CheckLoadParams then
 begin
  dinfoitem.LoadParams:=CheckLoadParams.Checked;
 end
 else
 begin
  dinfoitem.LoadDriverParams:=CheckLoadDriverParams.Checked;
 end;
end;


procedure TFDatainfoconfig.FillDatasets;
var
 i:integer;
begin
 LDatasets.Clear;
 for i:=0 to datainfo.Count-1 do
 begin
  LDatasets.Items.Add(datainfo.Items[i].Alias)
 end;
 if LDatasets.items.Count>0 then
  LDatasets.ItemIndex:=0;
 LDatasetsClick(Self);
end;


procedure TFDatainfoconfig.BAddClick(Sender: TObject);
var
 aliasname:string;
 aitem:TRpDataInfoItem;
 index:integer;
begin
 aliasname:=Trim(InputBox(SrpNewDataset,SRpAliasName,''));
 if Length(aliasname)<1 then
  exit;
 aitem:=datainfo.Add(aliasname);
 if LConnections.ItemIndex>=0 then
  aitem.DatabaseAlias:=LConnections.Items.strings[LConnections.Itemindex];
 FillDatasets;
 index:=LDatasets.items.indexof(AnsiUppercase(aliasname));
 if index>=0 then
 begin
  LDatasets.ItemIndex:=index;
  LDatasetsClick(Self);
 end;
end;

procedure  TFDatainfoconfig.Removedependences(oldalias:string);
var
 i:integer;
begin
 for i:=0 to datainfo.count-1 do
 begin
  if AnsiUpperCase(oldalias)=AnsiUpperCase(datainfo.items[i].datasource) then
   datainfo.items[i].datasource:='';
 end;
end;

procedure TFDatainfoconfig.BDeleteClick(Sender: TObject);
var
 index:integer;
 oldalias:string;
begin
 if LDatasets.itemindex<0 then
  exit;
 index:=datainfo.IndexOf(LDatasets.Items.strings[Ldatasets.itemindex]);
 if index>=0 then
 begin
  oldalias:=datainfo.items[index].Alias;
  datainfo.Delete(index);
  Removedependences(oldalias);
 end;
 FillDatasets;
end;

procedure TFDatainfoconfig.BRenameClick(Sender: TObject);
var
 dinfo:TRpDatainfoitem;
 aliasname:string;
 index:integer;
begin
 aliasname:=Trim(InputBox(SrpRenameDataset,SRpAliasName,''));
 if Length(aliasname)<1 then
  exit;
 index:=datainfo.IndexOf(aliasname);
 if index>=0 then
  Raise Exception.Create(SRpAliasExists);
 dinfo:=FindDataInfoItem;
 if Not Assigned(dinfo) then
  exit;
 dinfo.Alias:=aliasname;
 FillDatasets;
end;

procedure TFDatainfoconfig.LDatasetsClick(Sender: TObject);
var
 dinfo:TRpDatainfoItem;
 index:integer;
begin
 // Fils the info of the current dataset
 dinfo:=FindDataInfoItem;
 if dinfo=nil then
 begin
  GDataProps.Visible:=false;
  exit;
 end;
 GDataProps.Visible:=true;
 MSQL.Text:=dinfo.SQL;
 EMyBase.Text:=dinfo.MyBaseFilename;
 EIndexFields.Text:=dinfo.MyBaseIndexFields;
 index:=ComboConnection.Items.IndexOf(dinfo.DatabaseAlias);
 if index<0 then
  dinfo.DatabaseAlias:='';
 ComboConnection.ItemIndex:=Index;


 ComboDataSource.Items.Assign(Ldatasets.Items);
 index:=ComboDataSource.Items.IndexOf(dinfo.alias);
 if index>=0 then
  ComboDataSource.Items.Delete(index);

 index:=ComboDataSource.Items.IndexOf(dinfo.DataSource);
 if index<0 then
 begin
  dinfo.DataSource:='';
 end;
 ComboDataSource.Items.Insert(0,'');
 inc(index);
 ComboDatasource.ItemIndex:=Index;
 MSQLChange(ComboConnection);
end;


procedure TFDatainfoconfig.MSQLChange(Sender: TObject);
var
 dinfo:TRpDatainfoItem;
 index:integer;
begin
 // Fils the info of the current dataset
 dinfo:=FindDataInfoItem;
 if dinfo=nil then
 begin
  LSQL.Visible:=false;
  MSQL.Visible:=false;
  LMyBase.Visible:=false;
  EMyBase.Visible:=false;
  BMyBase.Visible:=false;
  EIndexFields.Visible:=false;
  LMasterDataset.Visible:=false;
  LIndexFields.Visible:=false;
  ComboDataSource.Visible:=false;
  exit;
 end;
 if Sender=MSQL then
 begin
  dinfo.SQL:=TMemo(Sender).Text;
 end
 else
 if Sender=ComboConnection then
 begin
  dinfo.DatabaseAlias:=COmboConnection.Text;
  // Finds the driver
  index:=databaseinfo.IndexOf(dinfo.DatabaseAlias);
  if index<0 then
  begin
   LSQL.Visible:=false;
   MSQL.Visible:=false;
   LMyBase.Visible:=false;
   EMyBase.Visible:=false;
   BMyBase.Visible:=false;
   EIndexFields.Visible:=false;
   ComboDataSource.Visible:=false;
   LIndexFields.Visible:=false;
   LMasterDataset.Visible:=false;
   exit;
  end;
  if databaseinfo.items[index].Driver=rpdatamybase then
  begin
   LMyBase.Visible:=true;
   EMyBase.Visible:=true;
   BMyBase.Visible:=true;
   EIndexFields.Visible:=true;
   LMasterDataset.Visible:=false;
   LIndexFields.Visible:=true;
   ComboDataSource.Visible:=false;
   LSQL.Visible:=false;
   MSQL.Visible:=false;
  end
  else
  begin
   LSQL.Visible:=true;
   MSQL.Visible:=true;
   LMyBase.Visible:=false;
   EMyBase.Visible:=false;
   BMyBase.Visible:=false;
   EIndexFields.Visible:=false;
   ComboDataSource.Visible:=true;
   LIndexFields.Visible:=false;
   LMasterDataset.Visible:=true;
  end;
 end
 else
 if Sender=ComboDataSource then
 begin
  dinfo.DataSource:=ComboDataSource.Text;
 end
 else
 if Sender=EMyBase then
 begin
  dinfo.MyBaseFilename:=EMyBase.Text;
 end
 else
 if Sender=EIndexFields then
 begin
  dinfo.MyBaseIndexFields:=EIndexFields.Text;
 end;
end;

procedure TFDatainfoconfig.DoSave;
var
 i:integer;
 index:integer;
begin
 report.DatabaseInfo.Assign(databaseinfo);
 report.DataInfo.Assign(datainfo);
 report.Params:=Params;
 // Updates subreport aliases
 for i:=0 to report.SubReports.Count-1 do
 begin
  index:=report.DataInfo.IndexOf(report.SubReports.Items[i].SubReport.Alias);
  if index<0 then
  begin
   report.SubReports.Items[i].SubReport.Alias:='';
  end;
 end;
 saved:=true;
end;

procedure TFDatainfoconfig.OKBtnClick(Sender: TObject);
begin
 DoSave;
 Close;
end;

procedure TFDatainfoconfig.BDeleteconClick(Sender: TObject);
var
 index:integer;
begin
 if LConnections.Itemindex<0 then
  exit;
 index:=databaseinfo.IndexOf(LConnections.items.strings[LConnections.Itemindex]);
 if index>=0 then
 begin
  databaseinfo.Delete(index);
  FillCurrentConnections;
 end;
end;

procedure TFDatainfoconfig.BShowDataClick(Sender: TObject);
var
 dinfo:TRpDatainfoitem;
begin
 // Opens the dataset and show the data
 dinfo:=FindDataInfoItem;
 if dinfo=nil then
  exit;
 dinfo.Disconnect;
 dinfo.Connect(databaseinfo,params);
 try
  ShowDataset(dinfo.Dataset);
 finally
  // Left the dataset open for testing relations ...
//  dinfo.Disconnect;
 end;
end;

procedure TFDatainfoconfig.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
 res:TModalResult;
begin
 if saved then
  exit;
 if docancel then
  exit;
 res:=MessageDlg(SRpSaveChanges,mtWarning,[mbYes,mbNo,mbCancel],0);
 if res=mrCancel then
  Raise EAbort.Create(SRpSaveAborted);
 if res=mrYes then
  DoSave;
end;

procedure TFDatainfoconfig.BParamsClick(Sender: TObject);
begin
 ShowParamDef(params,datainfo);
end;

procedure TFDatainfoconfig.CancelBtnClick(Sender: TObject);
begin
 docancel:=true;
 Close;
end;



procedure TFDatainfoconfig.GDriverClick(Sender: TObject);
var
 index:integeR;
begin
 // Loads the alias config
 case TrpDbDriver(GDriver.ItemIndex) of
  // DBExpress
  rpdatadbexpress:
   begin
    LConnectionString.Visible:=False;
    EConnectionString.Visible:=False;
    BConfig.Visible:=true;
    LSQL.Visible:=true;
    ComboAvailable.Visible:=true;
    if Assigned(ConAdmin) then
    begin
     conadmin.GetConnectionNames(ComboAvailable.Items,'');
    end;
   end;
  // IBX and IBO
  rpdataibx,rpdataibo:
   begin
    LConnectionString.Visible:=False;
    EConnectionString.Visible:=False;
    BConfig.Visible:=true;
    LSQL.Visible:=true;
    ComboAvailable.Visible:=true;
    if Assigned(ConAdmin) then
    begin
     conadmin.GetConnectionNames(ComboAvailable.Items,'Interbase');
    end;
   end;
  // My Base
  rpdatamybase:
   begin
    LConnectionString.Visible:=False;
    EConnectionString.Visible:=False;
    BConfig.Visible:=false;
    ComboAvailable.Visible:=false;
    ComboAvailable.Items.Clear;
   end;
  // BDE
  rpdatabde:
   begin
{$IFDEF USEBDE}
    LConnectionString.Visible:=False;
    EConnectionString.Visible:=False;
    BConfig.Visible:=false;
    Session.GetAliasNames(ComboAvailable.Items);
    ComboAvailable.Visible:=true;
{$ENDIF}
   end;
  // ADO
  rpdataado:
   begin
{$IFDEF MSWINDOWS}
    LConnectionString.Visible:=True;
    EConnectionString.Visible:=True;
    BConfig.Visible:=true;
    ComboAvailable.Visible:=false;
    ComboAvailable.Items.Clear;
{$ENDIF}
   end;
 end;
 if ComboAvailable.Items.count>0 then
 begin
  ComboAvailable.Itemindex:=0;
  ComboAvailable.Invalidate;
 end;
 if LConnections.ItemIndex>=0 then
 begin
  index:=databaseinfo.IndexOf(Lconnections.Items.Strings[LConnections.ItemIndex]);
  if index>=0 then
  begin
   databaseinfo.items[index].Driver:=TRpDbDriver(GDriver.ItemIndex);
  end;
 end;
 MSQLChange(ComboConnection);
end;

procedure TFDatainfoconfig.BMyBaseClick(Sender: TObject);
begin
 if OpenDialog1.Execute then
 begin
  EMyBase.Text:=OpenDialog1.FileName;
 end;
end;

procedure TFDatainfoconfig.EConnectionStringChange(Sender: TObject);
var
 index:integer;
begin
 if LConnections.Itemindex>=0 then
 begin
  index:=databaseinfo.IndexOf(LConnections.Items.Strings[LConnections.ItemIndex]);
  if index>=0 then
  begin
   databaseinfo.items[index].ADOConnectionString:=EConnectionString.Text;
  end;
 end;
end;

end.
