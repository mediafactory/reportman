{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdfdatasetsvcl                                }
{                                                       }
{       Datasets definition frame                       }
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

unit rpmdfdatasetsvcl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ToolWin, ImgList,rpmdconsts,rpgraphutilsvcl,
  rptypes,rpdatainfo,rpreport,rpfparamsvcl,rpmdfsampledatavcl, ActnList;

type
  TFRpDatasetsVCL = class(TFrame)
    ImageList1: TImageList;
    PTop: TPanel;
    PBottom: TPanel;
    ToolBar1: TToolBar;
    LDatasets: TListBox;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Panel2: TPanel;
    MHelp: TMemo;
    BParams: TButton;
    OpenDialog1: TOpenDialog;
    ActionList1: TActionList;
    ANew: TAction;
    ADelete: TAction;
    ARename: TAction;
    GDatasets: TPanel;
    Panel3: TPanel;
    LMasterDataset: TLabel;
    LConnection: TLabel;
    BShowData: TButton;
    ComboDataSource: TComboBox;
    ComboConnection: TComboBox;
    PControl: TPageControl;
    TabSQL: TTabSheet;
    MSQL: TMemo;
    TabBDEType: TTabSheet;
    RBDEType: TRadioGroup;
    Panel4: TPanel;
    PBDEFilter: TPanel;
    MBDEFilter: TMemo;
    TabBDETable: TTabSheet;
    LBDEIndexFields: TLabel;
    LIndexName: TLabel;
    LTable: TLabel;
    LMasterFields: TLabel;
    LNote: TLabel;
    LFirstRange: TLabel;
    LLastRange: TLabel;
    LRange: TLabel;
    EBDEIndexFields: TComboBox;
    EBDEIndexName: TComboBox;
    EBDETable: TComboBox;
    EBDEMasterFields: TEdit;
    EBDEFirstRange: TMemo;
    EBDELastRange: TMemo;
    TabMySQL: TTabSheet;
    LIndexFields: TLabel;
    LMyBase: TLabel;
    EMyBase: TEdit;
    EIndexFields: TEdit;
    BMyBase: TButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure BParamsClick(Sender: TObject);
    procedure LDatasetsClick(Sender: TObject);
    procedure MSQLChange(Sender: TObject);
    procedure BMyBaseClick(Sender: TObject);
    procedure BShowDataClick(Sender: TObject);
    procedure ANewExecute(Sender: TObject);
    procedure ADeleteExecute(Sender: TObject);
    procedure ARenameExecute(Sender: TObject);
  private
    { Private declarations }
    Report:TRpReport;
    procedure SetDataInfo(Value:TRpDataInfoList);
    procedure SetDatabaseInfo(Value:TRpDatabaseInfoList);
    function GetDatabaseInfo:TRpDatabaseInfoList;
    function GetDataInfo:TRpDataInfoList;
//    function FindDatabaseInfoItem:TRpDatabaseInfoItem;
    function FindDataInfoItem:TRpDataInfoItem;
    procedure  Removedependences(oldalias:string);
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    procedure FillDatasets;
    property Datainfo:TRpDataInfoList read GetDatainfo
     write SetDataInfo;
    property Databaseinfo:TRpDatabaseInfoList read GetDatabaseinfo
     write SetDatabaseInfo;
  end;

implementation

{$R *.DFM}


constructor TFRpDatasetsVCL.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 Report:=TRpReport.Create(Self);
 MHelp.Text:=SRpNewDataInfo;
 BParams.Caption:=TranslateStr(152,BParams.Caption);
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
 LFirstRange.Caption:=TranslateStr(831,LFirstRange.Caption);
 LLastRange.Caption:=TranslateStr(832,LLastRange.Caption);
 LRange.Caption:=TranslateStr(833,LRange.Caption);
end;

procedure TFRpDatasetsVCL.SetDatabaseInfo(Value:TRpDatabaseInfoList);
begin
 report.DatabaseInfo.Assign(Value);
 FillDatasets;
end;

procedure TFRpDatasetsVCL.SetDataInfo(Value:TRpDataInfoList);
begin
 report.DataInfo.Assign(Value);
end;

procedure TFRpDatasetsVCL.FillDatasets;
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
 ComboConnection.Clear;
 for i:=0 to databaseinfo.Count-1 do
 begin
  ComboConnection.Items.Add(databaseinfo.items[i].Alias);
 end;
 ComboConnection.Items.Add(' ');
 LDatasetsClick(Self);
end;

function TFRpDatasetsVCL.GetDatabaseInfo:TRpDatabaseInfoList;
begin
 Result:=Report.DatabaseInfo;
end;

function TFRpDatasetsVCL.GetDataInfo:TRpDataInfoList;
begin
 Result:=Report.DataInfo;
end;

procedure TFRpDatasetsVCL.BParamsClick(Sender: TObject);
begin
 ShowParamDef(report.params,report.datainfo);
end;

procedure TFRpDatasetsVCL.LDatasetsClick(Sender: TObject);
var
 dinfo:TRpDatainfoItem;
 index:integer;
begin
 // Fils the info of the current dataset
 dinfo:=FindDataInfoItem;
 if dinfo=nil then
 begin
  GDatasets.Visible:=false;
  exit;
 end;
 GDatasets.Visible:=true;
 MSQL.Text:=WideStringToDOS(dinfo.SQL);
 EMyBase.Text:=dinfo.MyBaseFilename;
 EIndexFields.Text:=dinfo.MyBaseIndexFields;
 EBDEIndexFields.Text:=dinfo.BDEIndexFields;
 MBDEFilter.Text:=dinfo.BDEFilter;
 EBDEIndexName.Text:=dinfo.BDEIndexName;
 EBDEFirstRange.Text:=dinfo.BDEFirstRange;
 EBDELastRange.Text:=dinfo.BDELastRange;
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
 ComboDataSource.Items.Insert(0,' ');
 inc(index);
 ComboDatasource.ItemIndex:=Index;
 MSQLChange(ComboConnection);
end;

{function TFRpDatasetsVCL.FindDatabaseInfoItem:TRpDatabaseInfoItem;
var
 index:integer;
 dinfo:TRpDataInfoItem;
begin
 Result:=nil;
 dinfo:=FindDataInfoItem;
 if not assigned(dinfo) then
  exit;
 index:=report.databaseinfo.IndexOf(dinfo.DatabaseAlias);
 if index>=0 then
  Result:=databaseinfo.items[index];
end;
}

function TFRpDatasetsVCL.FindDataInfoItem:TRpDataInfoItem;
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



procedure TFRpDatasetsVCL.MSQLChange(Sender: TObject);
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
 end
 else
 if Sender=EBDEFirstRange then
 begin
  dinfo.BDEFirstRange:=EBDEFirstRange.Text;
 end
 else
 if Sender=EBDELastRange then
 begin
  dinfo.BDELastRange:=EBDELastRange.Text;
 end;
end;

procedure TFRpDatasetsVCL.BMyBaseClick(Sender: TObject);
begin
 if OpenDialog1.Execute then
 begin
  EMyBase.Text:=OpenDialog1.FileName;
 end;
end;

procedure TFRpDatasetsVCL.BShowDataClick(Sender: TObject);
var
 dinfo:TRpDatainfoitem;
begin
 // Opens the dataset and show the data
 dinfo:=FindDataInfoItem;
 if dinfo=nil then
  exit;
 dinfo.Disconnect;
 dinfo.Connect(databaseinfo,report.params);
 try
  ShowDataset(dinfo.Dataset);
 finally
  // Left the dataset open for testing relations ...
//  dinfo.Disconnect;
 end;
end;

procedure TFRpDatasetsVCL.ANewExecute(Sender: TObject);
var
 aliasname:string;
 aitem:TRpDataInfoItem;
 index:integer;
begin
 aliasname:=Trim(RpInputBox(SrpNewDataset,SRpAliasName,''));
 if Length(aliasname)<1 then
  exit;
 aitem:=datainfo.Add(aliasname);
 if databaseinfo.Count>0 then
  aitem.DatabaseAlias:=databaseinfo.Items[0].Alias;
 FillDatasets;
 index:=LDatasets.items.indexof(AnsiUppercase(aliasname));
 if index>=0 then
 begin
  LDatasets.ItemIndex:=index;
  LDatasetsClick(Self);
 end;
end;

procedure TFRpDatasetsVCL.ADeleteExecute(Sender: TObject);
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

procedure TFRpDatasetsVCL.ARenameExecute(Sender: TObject);
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

procedure  TFRpDatasetsVCL.Removedependences(oldalias:string);
var
 i:integer;
begin
 for i:=0 to datainfo.count-1 do
 begin
  if AnsiUpperCase(oldalias)=AnsiUpperCase(datainfo.items[i].datasource) then
   datainfo.items[i].datasource:='';
 end;
end;

end.