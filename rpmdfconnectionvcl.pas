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
    GDriver: TListBox;
    BConfig: TButton;
    LConnections: TListBox;
    PConProps: TPanel;
    CheckLoginPrompt: TCheckBox;
    CheckLoadParams: TCheckBox;
    CheckLoadDriverParams: TCheckBox;
    LConnectionString: TLabel;
    EConnectionString: TEdit;
    ComboAvailable: TComboBox;
    LAvailable: TLabel;
    MHelp: TMemo;
    BBuild: TButton;
    PopAdd: TPopupMenu;
    MNew: TMenuItem;
    procedure GDriverClick(Sender: TObject);
    procedure LConnectionsClick(Sender: TObject);
    procedure BNewClick(Sender: TObject);
    procedure MNewClick(Sender: TObject);
    procedure BConfigClick(Sender: TObject);
    procedure BBuildClick(Sender: TObject);
    procedure ANewConnectionExecute(Sender: TObject);
    procedure PopAddPopup(Sender: TObject);
  private
    { Private declarations }
    conadmin:IConnectionAdmin;
    FDatabaseInfo:TRpDatabaseInfoList;
    procedure SetDatabaseInfo(Value:TRpDatabaseInfoList);
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
  Exit;
 end;
 CheckLoginPrompt.Visible:=True;
 CheckLoadParams.Visible:=True;
 CheckLoadDriverParams.Visible:=True;
end;

procedure TFRpConnectionVCL.GDriverClick(Sender: TObject);
var
 index:integeR;
begin
 if Not Assigned(FDatabaseInfo) then
  exit;
 LConnectionString.Visible:=False;
 EConnectionString.Visible:=false;
 BBuild.Visible:=false;
 // Loads the alias config
 case TrpDbDriver(GDriver.ItemIndex) of
  // DBExpress
  rpdatadbexpress:
   begin
    LConnectionString.Visible:=False;
//    LAvailable.Visible:=True;
    EConnectionString.Visible:=False;
    BConfig.Visible:=true;
//    ComboAvailable.Visible:=true;
    if Assigned(ConAdmin) then
    begin
     conadmin.GetConnectionNames(ComboAvailable.Items,'');
    end;
   end;
  // IBX and IBO
  rpdataibx,rpdataibo:
   begin
    LConnectionString.Visible:=False;
//    LAvailable.Visible:=True;
    EConnectionString.Visible:=False;
    BConfig.Visible:=true;
//    ComboAvailable.Visible:=true;
    if Assigned(ConAdmin) then
    begin
     conadmin.GetConnectionNames(ComboAvailable.Items,'Interbase');
    end;
   end;
  // My Base
  rpdatamybase:
   begin
    LConnectionString.Visible:=False;
//    LAvailable.Visible:=False;
    EConnectionString.Visible:=False;
    BConfig.Visible:=false;
//    ComboAvailable.Visible:=false;
    ComboAvailable.Items.Clear;
   end;
  // BDE
  rpdatabde:
   begin
{$IFDEF USEBDE}
    LConnectionString.Visible:=False;
//    LAvailable.Visible:=True;
    EConnectionString.Visible:=False;
    BConfig.Visible:=false;
    Session.GetAliasNames(ComboAvailable.Items);
//    ComboAvailable.Visible:=true;
{$ENDIF}
   end;
  // ADO
  rpdataado:
   begin
{$IFDEF MSWINDOWS}
    LConnectionString.Visible:=True;
//    LAvailable.Visible:=False;
    EConnectionString.Visible:=True;
    BConfig.Visible:=true;
    BBuild.Visible:=false;
//    ComboAvailable.Visible:=false;
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
 LConnectionsClick(Self);
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
  PopAdd.Items.Add(aitem);
 end;
end;

end.
