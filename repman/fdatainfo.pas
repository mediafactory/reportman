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

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,
  rpreport,rpconsts,rpdatainfo,DBConnAdmin,QDialogs,
{$IFNDEF PROFILE}  rpparams,rpfparams;{$ENDIF}
{$IFDEF PROFILE}  rpparams,rpfparams ,Proftimx;{$ENDIF}

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
    BAdd: TBitBtn;
    BDelete: TBitBtn;
    BRename: TBitBtn;
    GDataProps: TGroupBox;
    Label2: TLabel;
    ComboConnection: TComboBox;
    Label3: TLabel;
    ComboDataSource: TComboBox;
    Label4: TLabel;
    MSQL: TMemo;
    BShowData: TBitBtn;
    BParams: TButton;
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
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,2; xor eax,eax; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 UpdateConAdmin;
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,2; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.FormCreate(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,3; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,3; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.FormDestroy(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,4; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 datainfo.free;
 databaseinfo.Free;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,4; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.FormShow(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,5; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Fills the info
 if Assigned(ConAdmin) then
 begin
  conadmin.GetConnectionNames(ComboAvailable.Items,'');
 end;
 if ComboAvailable.Items.count>0 then
  ComboAvailable.Itemindex:=0;
 FillCurrentConnections;
 FillDatasets;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,5; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.BConfigClick(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,6; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
 FillCurrentConnections;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,6; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.BAddConClick(Sender: TObject);
var
 conname:string;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,7; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 if ComboAvailable.itemindex<0 then
  exit;
 conname:=AnsiUpperCase(ComboAvailable.Items.strings[ComboAvailable.itemindex]);
 if databaseinfo.IndexOf(conname)<0 then
 begin
  databaseinfo.Add(conname);
  FillCurrentConnections;
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,7; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.FillCurrentConnections;
var
 i:integer;
 oldtext:string;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,8; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,8; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


procedure TFDatainfoconfig.LConnectionsClick(Sender: TObject);
var
 dinfoitem:TRpDatabaseinfoitem;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,9; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,9; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

function TFDatainfoconfig.FindDatabaseInfoItem:TRpDatabaseInfoItem;
var
 index:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,10; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 Result:=nil;
 if LConnections.ItemIndex<0 then
  exit;
 index:=databaseinfo.IndexOf(LConnections.Items.Strings[LConnections.itemindex]);
 if index>=0 then
  Result:=databaseinfo.items[index];
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,10; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

function TFDatainfoconfig.FindDataInfoItem:TRpDataInfoItem;
var
 index:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,11; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 Result:=nil;
 if LDatasets.ItemIndex<0 then
  exit;
 index:=datainfo.IndexOf(LDatasets.Items.Strings[LDatasets.itemindex]);
 if index>=0 then
  Result:=datainfo.items[index];
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,11; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.CheckLoginPromptClick(Sender: TObject);
var
 dinfoitem:TRpDatabaseinfoitem;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,12; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,12; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


procedure TFDatainfoconfig.FillDatasets;
var
 i:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,13; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 LDatasets.Clear;
 for i:=0 to datainfo.Count-1 do
 begin
  LDatasets.Items.Add(datainfo.Items[i].Alias)
 end;
 if LDatasets.items.Count>0 then
  LDatasets.ItemIndex:=0;
 LDatasetsClick(Self);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,13; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


procedure TFDatainfoconfig.BAddClick(Sender: TObject);
var
 aliasname:string;
 aitem:TRpDataInfoItem;
 index:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,14; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,14; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure  TFDatainfoconfig.Removedependences(oldalias:string);
var
 i:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,15; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 for i:=0 to datainfo.count-1 do
 begin
  if AnsiUpperCase(oldalias)=AnsiUpperCase(datainfo.items[i].datasource) then
   datainfo.items[i].datasource:='';
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,15; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.BDeleteClick(Sender: TObject);
var
 index:integer;
 oldalias:string;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,16; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,16; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.BRenameClick(Sender: TObject);
var
 dinfo:TRpDatainfoitem;
 aliasname:string;
 index:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,17; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,17; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.LDatasetsClick(Sender: TObject);
var
 dinfo:TRpDatainfoItem;
 index:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,18; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Fils the info of the current dataset
 dinfo:=FindDataInfoItem;
 if dinfo=nil then
 begin
  GDataProps.Visible:=false;
  exit;
 end;
 GDataProps.Visible:=true;
 MSQL.Text:=dinfo.SQL;
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,18; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


procedure TFDatainfoconfig.MSQLChange(Sender: TObject);
var
 dinfo:TRpDatainfoItem;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,19; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Fils the info of the current dataset
 dinfo:=FindDataInfoItem;
 if dinfo=nil then
  exit;
 if Sender=MSQL then
 begin
  dinfo.SQL:=TMemo(Sender).Text;
 end
 else
 if Sender=ComboConnection then
 begin
  dinfo.DatabaseAlias:=COmboConnection.Text;
 end
 else
  dinfo.DataSource:=ComboDataSource.Text;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,19; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.DoSave;
var
 i:integer;
 index:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,20; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,20; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.OKBtnClick(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,21; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 DoSave;
 Close;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,21; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.BDeleteconClick(Sender: TObject);
var
 index:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,22; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 if LConnections.Itemindex<0 then
  exit;
 index:=databaseinfo.IndexOf(LConnections.items.strings[LConnections.Itemindex]);
 if index>=0 then
 begin
  databaseinfo.Delete(index);
  FillCurrentConnections;
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,22; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.BShowDataClick(Sender: TObject);
var
 dinfo:TRpDatainfoitem;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,23; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,23; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
 res:TModalResult;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,24; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 if saved then
  exit;
 if docancel then
  exit;
 res:=MessageDlg(SRpSaveChanges,mtWarning,[mbYes,mbNo,mbCancel],0);
 if res=mrCancel then
  Raise EAbort.Create(SRpSaveAborted);
 if res=mrYes then
  DoSave;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,24; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.BParamsClick(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,25; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 ShowParamDef(params,datainfo);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,25; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDatainfoconfig.CancelBtnClick(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,26; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 docancel:=true;
 Close;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,26; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;



end.
