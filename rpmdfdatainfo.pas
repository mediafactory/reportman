{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdfdatainfo                                   }
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

unit rpmdfdatainfo;

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
  rpreport,rpmdconsts,rpdatainfo,DBConnAdmin,QDialogs,
  rpparams,rpfparams, db,QComCtrls,rpgraphutils;

type
  TFRpDatainfoconfig = class(TForm)
    GConnections: TGroupBox;
    LConnections: TListBox;
    BAddCon: TButton;
    BDeletecon: TButton;
    Label1: TLabel;
    ComboAvailable: TComboBox;
    BConfig: TButton;
    CheckLoginPrompt: TCheckBox;
    CheckLoadParams: TCheckBox;
    CheckLoadDriverParams: TCheckBox;
    GDatasets: TGroupBox;
    GDataProps: TGroupBox;
    GDriver: TRadioGroup;
    OpenDialog1: TOpenDialog;
    LConnectionString: TLabel;
    EConnectionString: TEdit;
    BCancel: TButton;
    BOK: TButton;
    PControl: TPageControl;
    TabSQL: TTabSheet;
    MSQL: TMemo;
    Panel1: TPanel;
    BShowData: TButton;
    ComboDataSource: TComboBox;
    LMasterDataset: TLabel;
    ComboConnection: TComboBox;
    LConnection: TLabel;
    TabBDETable: TTabSheet;
    TabBDEType: TTabSheet;
    RBDEType: TRadioGroup;
    Panel2: TPanel;
    PBDEFilter: TPanel;
    MBDEFilter: TMemo;
    LBDEIndexFields: TLabel;
    LIndexName: TLabel;
    EBDEIndexFields: TComboBox;
    EBDEIndexName: TComboBox;
    EBDETable: TComboBox;
    LTable: TLabel;
    LMasterFields: TLabel;
    EBDEMasterFields: TEdit;
    TabMySQL: TTabSheet;
    EMyBase: TEdit;
    EIndexFields: TEdit;
    LIndexFields: TLabel;
    LMyBase: TLabel;
    BMyBase: TButton;
    Panel4: TPanel;
    BParams: TButton;
    BAdd: TButton;
    LDatasets: TListBox;
    BDelete: TButton;
    BRename: TButton;
    LNote: TLabel;
    LDataprops: TLabel;
    LRDataset: TLabel;
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
    procedure BOKClick(Sender: TObject);
    procedure BDeleteconClick(Sender: TObject);
    procedure BShowDataClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BParamsClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure GDriverClick(Sender: TObject);
    procedure BMyBaseClick(Sender: TObject);
    procedure EConnectionStringChange(Sender: TObject);
    procedure EBDETableDropDown(Sender: TObject);
    procedure EBDEIndexNameDropDown(Sender: TObject);
    procedure EBDEIndexFieldsDropDown(Sender: TObject);
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

uses rpdbxconfig, rpmdfsampledata;

{$R *.xfm}

procedure ShowDataConfig(report:TRpReport);
var
 dia:TFRpDataInfoConfig;
begin
{$IFDEF USECONADMIN}
 UpdateConAdmin;
{$ENDIF}
 dia:=TFRpDataInfoConfig.Create(Application);
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

procedure TFRpDatainfoconfig.FormCreate(Sender: TObject);
begin
 // Translation
 GConnections.Caption:=TranslateStr(142,GConnections.Caption);
 BConfig.Caption:=TranslateStr(143,BConfig.Caption);
 BOK.Caption:=TranslateStr(93,BOK.Caption);
 BCancel.Caption:=TranslateStr(94,BCancel.Caption);
 CheckLoginPrompt.Caption:=TranslateStr(144,CheckLoginPrompt.Caption);
 CheckLoadParams.Caption:=TranslateStr(145,CheckLoadParams.Caption);
 CheckLoadDriverParams.Caption:=TranslateStr(146,CheckLoadDriverParams.Caption);
 GDriver.Caption:=TranslateStr(147,GDriver.Caption);
 LRDataset.Caption:=TranslateStr(148,LRDataset.Caption);
 BAdd.Caption:=TranslateStr(149,BAdd.Caption);
 BDelete.Caption:=TranslateStr(150,BDelete.Caption);
 BRename.Caption:=TranslateStr(151,BRename.Caption);
 BParams.Caption:=TranslateStr(152,BParams.Caption);
 LDataprops.Caption:=TranslateStr(153,LDataprops.Caption);
 LConnection.Caption:=TranslateStr(154,LConnection.Caption);
 LMasterDataset.Caption:=TranslateStr(155,LMasterDataset.Caption);
 BShowData.Caption:=TranslateStr(156,BShowData.Caption);
 TAbBDEType.Caption:=TranslateStr(157,TabBDEType.Caption);
 TAbBDETable.Caption:=TranslateStr(158,TabBDETable.Caption);
 RBDEType.Items.Strings[0]:=TranslateStr(159,RBDEType.Items.Strings[0]);
 RBDEType.Items.Strings[1]:=TranslateStr(160,RBDEType.Items.Strings[1]);
 PBDEFilter.Caption:=TranslateStr(161,PBDEFilter.Caption);
 LTable.Caption:=TranslateStr(162,LTable.Caption);
 LIndexName.Caption:=TranslateStr(163,LIndexName.Caption);
 LBDEIndexFields.Caption:=TranslateStr(164,LBDEIndexFields.Caption);
 LMasterFields.Caption:=TranslateStr(165,LMasterFields.Caption);
 LNote.Caption:=TranslateStr(166,LNote.Caption);
 LMyBase.Caption:=TranslateStr(167,LMyBase.Caption);
 LIndexFields.Caption:=TranslateStr(164,LIndexFields.Caption);
 BMyBase.Caption:=TranslateStr(168,BMyBase.Caption);
 Caption:=TranslateStr(178,Caption);

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
 SetInitialBounds;
end;

procedure TFRpDatainfoconfig.FormDestroy(Sender: TObject);
begin
 datainfo.free;
 databaseinfo.Free;
end;

procedure TFRpDatainfoconfig.FormShow(Sender: TObject);
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

procedure TFRpDatainfoconfig.BConfigClick(Sender: TObject);
begin
 case TRpDBDriver(GDriver.ItemIndex) of
  rpdatadbexpress,rpdataibx,rpdataibo:
   begin
{$IFDEF USECONADMIN}
    ShowDBXConfig(TRpDbDriver(GDriver.ItemIndex) in [rpdataibx,rpdataibo]);
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
     if LConnections.ItemIndex<0 then
      Raise Exception.Create(SRpSelectAddConnection);
     EConnectionString.Text:=PromptDataSource(0,EConnectionString.Text);
{$ENDIF}
   end;
 end;
 FillCurrentConnections;
end;

procedure TFRpDatainfoconfig.BAddConClick(Sender: TObject);
var
 conname:string;
 item:TRpDatabaseInfoItem;
begin
 if Not ComboAvailable.Visible then
 begin
  conname:=UpperCase(Trim(RpInputBox(SRpNewConnection,SRpConnectionName,'')));
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

procedure TFRpDatainfoconfig.FillCurrentConnections;
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


procedure TFRpDatainfoconfig.LConnectionsClick(Sender: TObject);
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

function TFRpDatainfoconfig.FindDatabaseInfoItem:TRpDatabaseInfoItem;
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

function TFRpDatainfoconfig.FindDataInfoItem:TRpDataInfoItem;
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

procedure TFRpDatainfoconfig.CheckLoginPromptClick(Sender: TObject);
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


procedure TFRpDatainfoconfig.FillDatasets;
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


procedure TFRpDatainfoconfig.BAddClick(Sender: TObject);
var
 aliasname:string;
 aitem:TRpDataInfoItem;
 index:integer;
begin
 aliasname:=Trim(RpInputBox(SrpNewDataset,SRpAliasName,''));
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

procedure  TFRpDatainfoconfig.Removedependences(oldalias:string);
var
 i:integer;
begin
 for i:=0 to datainfo.count-1 do
 begin
  if AnsiUpperCase(oldalias)=AnsiUpperCase(datainfo.items[i].datasource) then
   datainfo.items[i].datasource:='';
 end;
end;

procedure TFRpDatainfoconfig.BDeleteClick(Sender: TObject);
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

procedure TFRpDatainfoconfig.BRenameClick(Sender: TObject);
var
 dinfo:TRpDatainfoitem;
 aliasname:string;
 index:integer;
begin
 aliasname:=Trim(RpInputBox(SrpRenameDataset,SRpAliasName,''));
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

procedure TFRpDatainfoconfig.LDatasetsClick(Sender: TObject);
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
 EBDEIndexFields.Text:=dinfo.BDEIndexFields;
 MBDEFilter.Text:=dinfo.BDEFilter;
 EBDEIndexName.Text:=dinfo.BDEIndexName;
 EBDETable.Text:=dinfo.BDETable;
 EBDEMasterFields.Text:=dinfo.BDEMasterFields;
 RBDEType.ItemIndex:=Integer(dinfo.BDEType);
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


procedure TFRpDatainfoconfig.MSQLChange(Sender: TObject);
var
 dinfo:TRpDatainfoItem;
 index:integer;
begin
 // Fils the info of the current dataset
 dinfo:=FindDataInfoItem;
 if dinfo=nil then
 begin
  TabSQL.TabVisible:=false;
  TabBDETable.TabVisible:=false;
  TabMySQL.TabVisible:=false;
  TabBDEType.TabVisible:=false;
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
   TabSQL.TabVisible:=false;
   TabBDETable.TabVisible:=false;
   TabMySQL.TabVisible:=false;
   TabBDEType.TabVisible:=false;
   exit;
  end;
  if databaseinfo.items[index].Driver=rpdatamybase then
  begin
   TabSQL.TabVisible:=false;
   TabBDETable.TabVisible:=false;
   TabMySQL.TabVisible:=True;
   TabBDEType.TabVisible:=false;
  end
  else
  begin
   if databaseinfo.items[index].Driver=rpdatabde then
   begin
    TabBDEType.TabVisible:=True;
    if (dinfo.BDEType=rpdtable) then
    begin
     TabSQL.TabVisible:=False;
     TabBDETable.TabVisible:=True;
     TabMySQL.TabVisible:=False;
    end
    else
    begin
     TabSQL.TabVisible:=True;
     TabBDETable.TabVisible:=False;
     TabMySQL.TabVisible:=False;
    end;
   end
   else
   begin
    TabSQL.TabVisible:=True;
    TabBDETable.TabVisible:=false;
    TabMySQL.TabVisible:=False;
    TabBDEType.TabVisible:=false;
   end;
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
 end
 else
 if Sender=RBDEType then
 begin
  dinfo.BDEType:=TRpDatasetType(RBDEType.ItemIndex);
  if dinfo.BDEType=rpdQuery then
  begin
   TabSQL.TabVisible:=true;
   TabBDETable.TabVisible:=false;
  end
  else
  begin
   TabSQL.TabVisible:=False;
   TabBDETable.TabVisible:=True;
  end;
 end
 else
 if Sender=EBDEIndexFields then
 begin
  dinfo.BDEIndexFields:=Trim(EBDEIndexFields.Text);
  if length(dinfo.BDEIndexFields)>0 then
   EBDEIndexName.Text:='';
 end
 else
 if Sender=EBDEIndexName then
 begin
  dinfo.BDEIndexName:=Trim(EBDEIndexName.Text);
  if length(dinfo.BDEIndexName)>0 then
   EBDEIndexFields.Text:='';
 end
 else
 if Sender=EBDETable then
 begin
  dinfo.BDETable:=EBDETable.Text;
 end
 else
 if Sender=MBDEFilter then
 begin
  dinfo.BDEFilter:=MBDEFilter.Text;
 end
 else
 if Sender=EBDEMasterFields then
 begin
  dinfo.BDEMasterFields:=EBDEMasterFields.Text;
 end;
end;

procedure TFRpDatainfoconfig.DoSave;
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

procedure TFRpDatainfoconfig.BOKClick(Sender: TObject);
begin
 DoSave;
 Close;
end;

procedure TFRpDatainfoconfig.BDeleteconClick(Sender: TObject);
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

procedure TFRpDatainfoconfig.BShowDataClick(Sender: TObject);
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

procedure TFRpDatainfoconfig.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
 res:TMessageButton;
begin
 if saved then
  exit;
 if docancel then
  exit;
 res:=RpMessageBox(SRpSaveChanges,SRpWarning,[smbYes,smbNo,smbCancel],smsWarning,smbYes);
 if (res=smbCancel) then
  Raise EAbort.Create(SRpSaveAborted);
 if res=smbYes then
  DoSave;
end;

procedure TFRpDatainfoconfig.BParamsClick(Sender: TObject);
begin
 ShowParamDef(params,datainfo);
end;

procedure TFRpDatainfoconfig.BCancelClick(Sender: TObject);
begin
 docancel:=true;
 Close;
end;



procedure TFRpDatainfoconfig.GDriverClick(Sender: TObject);
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

procedure TFRpDatainfoconfig.BMyBaseClick(Sender: TObject);
begin
 if OpenDialog1.Execute then
 begin
  EMyBase.Text:=OpenDialog1.FileName;
 end;
end;

procedure TFRpDatainfoconfig.EConnectionStringChange(Sender: TObject);
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

procedure TFRpDatainfoconfig.EBDETableDropDown(Sender: TObject);
{$IFDEF USEBDE}
var
 dinfo:TRpDatainfoItem;
{$ENDIF}
begin
{$IFDEF USEBDE}
 // Fils the info of the current dataset
 dinfo:=FindDataInfoItem;
 if dinfo=nil then
  exit;
 // Fills with tablenames, without extensions,
 // no system tables
 try
  Session.GetTableNames(dinfo.DatabaseAlias,'',True,False,EBDETable.Items);
 finally
  EBDETable.Items.Insert(0,'');
 end;
{$ENDIF}
end;

procedure TFRpDatainfoconfig.EBDEIndexNameDropDown(Sender: TObject);
{$IFDEF USEBDE}
var
 dinfo:TRpDatainfoItem;
 atable:TTable;
 i:integer;
{$ENDIF}
begin
{$IFDEF USEBDE}
 // Fils the info of the current dataset
 dinfo:=FindDataInfoItem;
 if dinfo=nil then
  exit;
 atable:=TTable.Create(Self);
 try
  EBDEIndexName.Items.Clear;
  atable.DatabaseName:=dinfo.DatabaseAlias;
  atable.TableName:=dinfo.BDETable;
  atable.IndexDefs.Update;
  EBDEIndexName.Items.Clear;
  for i:=0 to atable.IndexDefs.Count-1 do
  begin
   EBDEIndexName.Items.Add(atable.IndexDefs.Items[i].Name);
  end;
 finally
  atable.free;
  EBDEIndexName.Items.Insert(0,'');
 end;
{$ENDIF}
end;

{$IFDEF USEBDE}
procedure GetIndexFieldNames(atable:TTable;alist:TStrings);
var
 i:integer;
 aindexdef:TIndexDef;
 afields:string;
begin
 atable.IndexDefs.Update;
 alist.clear;
 for i:=0 to atable.IndexDefs.Count-1 do
 begin
  aindexdef:=atable.IndexDefs.Items[i];
  afields:='';
  if Length(aindexdef.Fields)>0 then
   afields:=aindexdef.Fields
  else
   if Length(aindexdef.DescFields)>0 then
   begin
    afields:=aindexdef.DescFields;
   end;
  if Length(afields)>0 then
   alist.add(afields);
 end;
end;
{$ENDIF}


procedure TFRpDatainfoconfig.EBDEIndexFieldsDropDown(Sender: TObject);
{$IFDEF USEBDE}
var
 dinfo:TRpDatainfoItem;
 atable:TTable;
{$ENDIF}
begin
{$IFDEF USEBDE}
 // Fils the info of the current dataset
 dinfo:=FindDataInfoItem;
 if dinfo=nil then
  exit;
 atable:=TTable.Create(Self);
 try
  EBDEIndexFields.Items.Clear;
  atable.DatabaseName:=dinfo.DatabaseAlias;
  atable.TableName:=dinfo.BDETable;
  GetIndexFieldNames(atable,EBDEIndexFields.Items);
 finally
  atable.free;
  EBDEIndexFields.Items.Insert(0,'');
 end;
{$ENDIF}
end;

end.
