{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdfconnection                                 }
{                                                       }
{       Connections definition frame                    }
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

unit rpmdfconnection;

interface

{$I rpconf.inc}

uses
  SysUtils,
{$IFDEF USEVARIANTS}
  Variants,Types,
{$ENDIF}
  Classes, QGraphics, QControls, QForms,rpreport,
  QDialogs, QStdCtrls, QExtCtrls, QActnList,
{$IFDEF USEBDE}
  dbtables,
{$ENDIF}
{$IFDEF USEADO}
  adodb,
{$ENDIF}
  rpdatainfo,rpmdconsts,
//  DBConnAdmin,
  rpgraphutils,rpdbxconfig,
  QMenus,
  QImgList, QTypes, QComCtrls;

type
  TFRpConnection = class(TFrame)
    ActionList1: TActionList;
    ANewConnection: TAction;
    ADelete: TAction;
    ToolBar1: TToolBar;
    BNew: TToolButton;
    ToolButton5: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    PParent: TPanel;
    PanelProps: TPanel;
    PopAdd: TPopupMenu;
    MNew: TMenuItem;
    PTop: TPanel;
    GDriver: TListBox;
    PDriver: TPanel;
    MHelp: TMemo;
    Panel1: TPanel;
    BConfig: TButton;
    GAvailable: TGroupBox;
    LConnections: TListBox;
    PConProps: TPanel;
    LConnectionString: TLabel;
    LAvailable: TLabel;
    LDriver: TLabel;
    CheckLoginPrompt: TCheckBox;
    CheckLoadParams: TCheckBox;
    CheckLoadDriverParams: TCheckBox;
    EConnectionString: TEdit;
    ComboAvailable: TComboBox;
    BBuild: TButton;
    ComboDriver: TComboBox;
    BTest: TButton;
    ImageList1: TImageList;
    procedure GDriverClick(Sender: TObject);
    procedure LConnectionsClick(Sender: TObject);
    procedure BNewClick(Sender: TObject);
    procedure MNewClick(Sender: TObject);
    procedure BConfigClick(Sender: TObject);
    procedure BBuildClick(Sender: TObject);
    procedure ANewConnectionExecute(Sender: TObject);
    procedure PopAddPopup(Sender: TObject);
    procedure ADeleteExecute(Sender: TObject);
    procedure ComboDriverClick(Sender: TObject);
    procedure BTestClick(Sender: TObject);
    procedure CheckLoginPromptClick(Sender: TObject);
    procedure EConnectionStringChange(Sender: TObject);
  private
    { Private declarations }
    conadmin:TRpCOnnAdmin;
    FDatabaseInfo:TRpDatabaseInfoList;
    report:TRpReport;
    procedure SetDatabaseInfo(Value:TRpDatabaseInfoList);
    procedure MenuAddClick(Sender:TObject);
    function FindDatabaseInfoItem:TRpDatabaseInfoItem;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property Databaseinfo:TRpDatabaseInfoList read FDatabaseinfo
     write SetDatabaseInfo;
  end;

implementation

{$R *.xfm}

constructor TFRpConnection.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 // Translations
 BConfig.Caption:=TranslateStr(143,BConfig.Caption);
 CheckLoginPrompt.Caption:=TranslateStr(144,CheckLoginPrompt.Caption);
 CheckLoadParams.Caption:=TranslateStr(145,CheckLoadParams.Caption);
 CheckLoadDriverParams.Caption:=TranslateStr(146,CheckLoadDriverParams.Caption);
 BTest.Caption:=TranslateStr(753,BTest.Caption);
 BBuild.Caption:=TranslateStr(168,BBuild.Caption);

 GAvailable.Caption:=TranslateStr(1098,GAvailable.Caption);
 LConnectionString.Caption:=TranslateStr(1099,LConnectionString.Caption);
 LAvailable.Caption:=TranslateStr(1100,LAvailable.Caption);
 LDriver.Caption:=TranslateStr(1101,LDriver.Caption);
 MNew.Caption:=TranslateStr(40,MNew.Caption);
 ANewConnection.Caption:=TranslateStr(1102,ANewConnection.Caption);
 ANewConnection.Hint:=TranslateStr(1103,ANewConnection.Hint);
 ADelete.Caption:=TranslateStr(1104,ADelete.Caption);
 ADelete.Hint:=TranslateStr(1105,ADelete.Hint);

 GetRpDatabaseDrivers(GDriver.Items);
 GetRpDatabaseDrivers(ComboDriver.Items);

 report:=TRPReport.Create(Self);
 FDatabaseInfo:=report.databaseinfo;

 ConAdmin:=TRpConnAdmin.Create;

 GDriver.ItemIndex:=0;
 GDriverClick(Self);
end;

destructor TFRpConnection.Destroy;
begin
 conadmin.free;
 inherited destroy;
end;

procedure TFRpConnection.SetDatabaseInfo(Value:TRpDatabaseInfoList);
var
 i:integer;
begin
 ComboDriver.Width:=PConProps.Width-ComboDriver.Left-20;
 EConnectionString.Width:=PConProps.Width-ECOnnectionString.Left-20;
 ComboAvailable.Width:=PConProps.Width-ComboAvailable.Left-20;
 ComboDriver.Anchors:=[akLeft,akTop,akRight];
 ComboAvailable.Anchors:=[akLeft,akTop,akRight];
 EConnectionString.Anchors:=[akLeft,akTop,akRight];

 if Value<>FDatabaseInfo then
  FDatabaseInfo.Assign(Value);
 LConnections.Clear;
 for i:=0 to FDatabaseinfo.Count-1 do
 begin
  LConnections.Items.Add(FDatabaseinfo.Items[i].Alias);
 end;
 if LConnections.Items.Count>0 then
  LConnections.ItemIndex:=0;
 LConnectionsClick(Self);
 GDriverClick(Self);
end;

procedure TFRpConnection.LConnectionsClick(Sender: TObject);
var
 dbinfo:TRpDatabaseInfoItem;
 index:integer;
begin
 if Not Assigned(FDatabaseInfo) then
  exit;
 If LConnections.Items.Count<1 then
 begin
  MHelp.Text:=SRpNewDatabaseInfo;
  CheckLoginPrompt.Visible:=False;
  CheckLoadParams.Visible:=False;
  CheckLoadDriverParams.Visible:=False;
  LConnectionString.Visible:=False;
  EConnectionString.Visible:=false;
  BBuild.Visible:=false;
  BTest.Visible:=false;
  ComboDriver.Visible:=false;
  LDriver.Visible:=false;
  Exit;
 end;
 If LConnections.ItemIndex<0 then
  exit;
 index:=FDatabaseInfo.IndexOf(LConnections.Items[LConnections.ItemIndex]);
 if index<0 then
  exit;
 CheckLoginPrompt.Visible:=True;
 CheckLoadParams.Visible:=True;
 BTest.Visible:=True;
 CheckLoadDriverParams.Visible:=True;
 ComboDriver.Visible:=true;
 LDriver.Visible:=true;
 // Get information about the dabaseinfo
 dbinfo:=FDatabaseinfo.Items[index];
 ComboDriver.ItemIndex:=Integer(dbinfo.Driver);
 ComboDriverClick(Self);
 CheckLoginPrompt.Checked:=dbinfo.LoginPrompt;
 CheckLoadParams.Checked:=dbinfo.LoadParams;
 CheckLoadDriverParams.Checked:=dbinfo.LoadDriverParams;
 EConnectionString.Text:=dbinfo.ADOConnectionString;
end;

procedure TFRpConnection.GDriverClick(Sender: TObject);
begin
 if Not Assigned(FDatabaseInfo) then
  exit;
 case GDriver.ItemIndex of
  0:
   MHelp.Lines.Text:=SRpDBExpressDesc;
  1:
   MHelp.Lines.Text:=SRpMyBaseDesc;
  2:
   MHelp.Lines.Text:=SRpIBXDesc;
  3:
   MHelp.Lines.Text:=SRpBDEDesc;
  4:
   MHelp.Lines.Text:=SRpADODesc;
  5:
   MHelp.Lines.Text:=SRpIBODesc;
 end;
 // Loads the alias config
 case TrpDbDriver(GDriver.ItemIndex) of
  // DBExpress
  rpdatadbexpress:
   begin
    BConfig.Visible:=true;
    if Assigned(ConAdmin) then
    begin
     conadmin.GetConnectionNames(ComboAvailable.Items,'');
    end;
   end;
  // IBX and IBO
  rpdataibx,rpdataibo:
   begin
    BConfig.Visible:=true;
    if Assigned(ConAdmin) then
    begin
     conadmin.GetConnectionNames(ComboAvailable.Items,'Interbase');
    end;
   end;
  // My Base
  rpdatamybase:
   begin
    BConfig.Visible:=True;
    ComboAvailable.Items.Clear;
   end;
  // BDE
  rpdatabde:
   begin
{$IFDEF USEBDE}
    BConfig.Visible:=true;
    Session.GetAliasNames(ComboAvailable.Items);
{$ENDIF}
   end;
  // ADO
  rpdataado:
   begin
{$IFDEF MSWINDOWS}
    BConfig.Visible:=false;
    BBuild.Visible:=false;
    ComboAvailable.Items.Clear;
{$ENDIF}
   end;
 end;
end;


procedure TFRpConnection.BNewClick(Sender: TObject);
var
 apoint:TPoint;
begin
 if Not Assigned(FDatabaseInfo) then
  exit;
 apoint.x:=BNew.Left;
 apoint.y:=BNew.Top+BNew.Height;
 apoint:=BNew.Parent.ClientToScreen(apoint);
 BNew.DropDownMenu.Popup(apoint.x,apoint.y);
end;

procedure TFRpConnection.MNewClick(Sender: TObject);
var
 conname:string;
 item:TRpDatabaseInfoItem;
 index:integer;
begin
 if Not Assigned(FDatabaseInfo) then
  exit;
 conname:=UpperCase(Trim(RpInputBox(SRpNewConnection,SRpConnectionName,'')));
 if Length(conname)<1 then
  exit;
 item:=Fdatabaseinfo.Add(conname);
 item.Driver:=TRpDbDriver(GDriver.ItemIndex);
 SetDatabaseInfo(Fdatabaseinfo);
 index:=FDatabaseinfo.IndexOf(conname);
 if index>=0 then
 begin
  LConnections.ItemIndex:=index;
  LConnectionsClick(Self);
 end;
end;

procedure TFRpConnection.BConfigClick(Sender: TObject);
begin
 ShowDBXConfig(TRpDbDriver(GDriver.ItemIndex) in [rpdataibx,rpdataibo,rpdatamybase]);
 conadmin.free;
 conadmin:=TRPCOnnAdmin.Create;
 conadmin.GetConnectionNames(ComboAvailable.Items,'');
end;

procedure TFRpConnection.BBuildClick(Sender: TObject);
begin
{$IFDEF USEADO}
  if LConnections.ItemIndex<0 then
   Raise Exception.Create(SRpSelectAddConnection);
  EConnectionString.Text:=PromptDataSource(0,EConnectionString.Text);
{$ENDIF}
end;

procedure TFRpConnection.ANewConnectionExecute(Sender: TObject);
var
 apoint:TPoint;
begin
 // Adds a new connection
 // Shows the popupmenu
 apoint.x:=BNew.Left;
 apoint.y:=BNew.Top+BNew.Height;
 apoint:=BNew.Parent.ClientToScreen(apoint);
 BNew.DropDownMenu.Popup(apoint.x,apoint.y);
end;


procedure TFRpConnection.PopAddPopup(Sender: TObject);
var
 aitem:TMenuItem;
 i:integer;
begin
 // Adds available items
 While PopAdd.Items.Count>1 do
  PopAdd.Items.Delete(1);
 for i:=0 to ComboAvailable.Items.Count-1 do
 begin
  aitem:=TMenuItem.Create(PopAdd);
  aitem.Caption:=ComboAvailable.Items.Strings[i];
  aitem.OnClick:=MenuAddClick;
  PopAdd.Items.Add(aitem);
 end;
end;

procedure TFRpConnection.ADeleteExecute(Sender: TObject);
var
 index:integer;
begin
 if LConnections.Itemindex<0 then
  exit;
 index:=databaseinfo.IndexOf(LConnections.items.strings[LConnections.Itemindex]);
 if index>=0 then
 begin
  databaseinfo.Delete(index);
  SetDatabaseInfo(databaseinfo);
 end;
end;

procedure TFRpConnection.ComboDriverClick(Sender: TObject);
var
 index:integeR;
begin
 if Not Assigned(FDatabaseInfo) then
  exit;
 if ComboDriver.ItemIndex<0 then
  exit;
 LConnectionString.Visible:=False;
 EConnectionString.Visible:=false;
 BBuild.Visible:=false;
 // Loads the alias config
 case TrpDbDriver(ComboDriver.ItemIndex) of
  // DBExpress
  rpdatadbexpress:
   begin
    LConnectionString.Visible:=False;
    EConnectionString.Visible:=False;
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
   end;
  // My Base
  rpdatamybase:
   begin
    LConnectionString.Visible:=False;
    EConnectionString.Visible:=False;
    BTest.Visible:=false;
   end;
  // BDE
  rpdatabde:
   begin
{$IFDEF USEBDE}
    LConnectionString.Visible:=False;
    EConnectionString.Visible:=False;
{$ENDIF}
   end;
  // ADO
  rpdataado:
   begin
{$IFDEF MSWINDOWS}
    LConnectionString.Visible:=True;
    EConnectionString.Visible:=True;
    BBuild.Visible:=true;
{$ENDIF}
   end;
 end;
 if LConnections.ItemIndex<0 then
  exit;
 index:=FDatabaseInfo.Indexof(LConnections.Items.Strings[LConnections.ItemIndex]);
 if index<0 then
  exit;
 FDatabaseInfo.Items[index].Driver:=TRpDbDriver(ComboDriver.ItemIndex);
end;

procedure TFRpConnection.MenuAddClick(Sender:TObject);
var
 conname:String;
 item:TRpDatabaseInfoItem;
 index:integer;
begin
 if Not Assigned(FDatabaseInfo) then
  exit;
 conname:=UpperCase(Trim(TMenuItem(Sender).Caption));
 if Length(conname)<1 then
  exit;
 item:=Fdatabaseinfo.Add(conname);
 item.Driver:=TRpDbDriver(GDriver.ItemIndex);
 SetDatabaseInfo(Fdatabaseinfo);
 index:=FDatabaseinfo.IndexOf(conname);
 if index>=0 then
 begin
  LConnections.ItemIndex:=index;
  LConnectionsClick(Self);
 end;
end;


function TFRpConnection.FindDatabaseInfoItem:TRpDatabaseInfoItem;
var
 index:integer;
begin
 Result:=nil;
 if Not Assigned(FDatabaseInfo) then
  exit;
 If LConnections.Items.Count<1 then
  exit;
 If LConnections.ItemIndex<0 then
  exit;
 index:=FDatabaseInfo.IndexOf(LConnections.Items[LConnections.ItemIndex]);
 if index<0 then
  exit;
 Result:=FDatabaseInfo.Items[index];
end;

procedure TFRpConnection.BTestClick(Sender: TObject);
var
 dbinfo:TRpDatabaseInfoItem;
begin
 dbinfo:=FindDatabaseInfoItem;
 if Not Assigned(dbinfo) then
  exit;
 dbinfo.Connect;
 try
  ShowMessage(SRpConnectionOk);
 finally
  dbinfo.DisConnect;
 end;
end;



procedure TFRpConnection.CheckLoginPromptClick(Sender: TObject);
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

procedure TFRpConnection.EConnectionStringChange(Sender: TObject);
var
 dinfoitem:TRpDatabaseinfoitem;
begin
 dinfoitem:=FindDatabaseInfoItem;
 if Not Assigned(dinfoitem) then
  exit;
 dinfoitem.ADOConnectionString:=EConnectionString.Text;
end;

end.
