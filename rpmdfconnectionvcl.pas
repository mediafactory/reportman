{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdfconnectionvcl                              }
{                                                       }
{       Connections definition frame                    }
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

unit rpmdfconnectionvcl;

interface

{$I rpconf.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ToolWin, ActnList, ImgList,
{$IFDEF USEBDE}
  dbtables,
{$ENDIF}
{$IFDEF USEADO}
  adodb,
{$ENDIF}
  rpdatainfo,rpmdconsts, DBConnAdmin,rpgraphutilsvcl,rpdbxconfigvcl,
  Menus;

type
  TFRpConnectionVCL = class(TFrame)
    ImageList1: TImageList;
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
    conadmin:IConnectionAdmin;
    FDatabaseInfo:TRpDatabaseInfoList;
    procedure SetDatabaseInfo(Value:TRpDatabaseInfoList);
    procedure MenuAddClick(Sender:TObject);
    function FindDatabaseInfoItem:TRpDatabaseInfoItem;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    property Databaseinfo:TRpDatabaseInfoList read FDatabaseinfo
     write SetDatabaseInfo;
  end;

implementation

{$R *.dfm}

constructor TFRpConnectionVCL.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 // Translations
 BConfig.Caption:=TranslateStr(143,BConfig.Caption);
 CheckLoginPrompt.Caption:=TranslateStr(144,CheckLoginPrompt.Caption);
 CheckLoadParams.Caption:=TranslateStr(145,CheckLoadParams.Caption);
 CheckLoadDriverParams.Caption:=TranslateStr(146,CheckLoadDriverParams.Caption);

 GetRpDatabaseDrivers(GDriver.Items);
 GetRpDatabaseDrivers(ComboDriver.Items);

{$IFDEF USECONADMIN}
 UpdateConAdmin;
{$ENDIF}

 try
  ConAdmin:=GetConnectionAdmin;
 except
  on e:Exception do
  begin
   ShowMessage(E.message);
  end;
 end;

 GDriver.ItemIndex:=0;
 GDriverClick(Self);
end;


procedure TFRpConnectionVCL.SetDatabaseInfo(Value:TRpDatabaseInfoList);
var
 i:integer;
begin
 FDatabaseInfo:=Value;
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

procedure TFRpConnectionVCL.LConnectionsClick(Sender: TObject);
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

procedure TFRpConnectionVCL.GDriverClick(Sender: TObject);
begin
 if Not Assigned(FDatabaseInfo) then
  exit;
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
    BConfig.Visible:=false;
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


procedure TFRpConnectionVCL.BNewClick(Sender: TObject);
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

procedure TFRpConnectionVCL.MNewClick(Sender: TObject);
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

procedure TFRpConnectionVCL.BConfigClick(Sender: TObject);
begin
{$IFDEF USECONADMIN}
    ConAdmin:=nil;
    ShowDBXConfig(TRpDbDriver(GDriver.ItemIndex) in [rpdataibx,rpdataibo]);
    UpdateConAdmin;
//    if Assigned(ConAdmin) then
//    begin
//     ConAdmin:=nil;
     try
      ConAdmin:=GetConnectionAdmin;
     except
      on e:Exception do
      begin
       ShowMessage(E.message);
      end;
     end;
     conadmin.GetConnectionNames(ComboAvailable.Items,'');
//    end;
{$ENDIF}
end;

procedure TFRpConnectionVCL.BBuildClick(Sender: TObject);
begin
{$IFDEF USEADO}
  if LConnections.ItemIndex<0 then
   Raise Exception.Create(SRpSelectAddConnection);
  EConnectionString.Text:=PromptDataSource(0,EConnectionString.Text);
{$ENDIF}
end;

procedure TFRpConnectionVCL.ANewConnectionExecute(Sender: TObject);
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


procedure TFRpConnectionVCL.PopAddPopup(Sender: TObject);
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

procedure TFRpConnectionVCL.ADeleteExecute(Sender: TObject);
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

procedure TFRpConnectionVCL.ComboDriverClick(Sender: TObject);
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

procedure TFRpConnectionVCL.MenuAddClick(Sender:TObject);
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


function TFRpConnectionVCL.FindDatabaseInfoItem:TRpDatabaseInfoItem;
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

procedure TFRpConnectionVCL.BTestClick(Sender: TObject);
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



procedure TFRpConnectionVCL.CheckLoginPromptClick(Sender: TObject);
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

procedure TFRpConnectionVCL.EConnectionStringChange(Sender: TObject);
var
 dinfoitem:TRpDatabaseinfoitem;
begin
 dinfoitem:=FindDatabaseInfoItem;
 if Not Assigned(dinfoitem) then
  exit;
 dinfoitem.ADOConnectionString:=EConnectionString.Text;
end;

end.
