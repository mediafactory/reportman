{*******************************************************}
{                                                       }
{       Report Manager Server configuration             }
{                                                       }
{       mainf                                           }
{                                                       }
{       Main form to configure the server               }
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

unit mainf;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, rptranslator, DB, DBClient, QGrids, QDBGrids,
  rpmdrepclient,rpmdconsts;

type
  TFMain = class(TForm)
    LHost: TLabel;
    ComboHost: TComboBox;
    Trans: TRpTranslator;
    GUser: TGroupBox;
    LUserName: TLabel;
    EUserName: TEdit;
    EPassword: TEdit;
    LPassword: TLabel;
    BConnect: TButton;
    GServerinfo: TGroupBox;
    GUsers: TGroupBox;
    LUsers: TListBox;
    BDeleteUser: TButton;
    BAddUser: TButton;
    GReportDirectories: TGroupBox;
    DDirectories: TClientDataSet;
    DDirectoriesAlias: TStringField;
    DDirectoriesServerPath: TStringField;
    DBGrid1: TDBGrid;
    SDirectories: TDataSource;
    BAddAlias: TButton;
    BDeleteAlias: TButton;
    BPreviewTree: TButton;
    BCloseConnection: TButton;
    BChangePassword: TButton;
    LMessages: TListBox;
    ComboPort: TComboBox;
    LPort: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BConnectClick(Sender: TObject);
    procedure BCloseConnectionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BAddUserClick(Sender: TObject);
    procedure BDeleteUserClick(Sender: TObject);
    procedure BChangePasswordClick(Sender: TObject);
    procedure BAddAliasClick(Sender: TObject);
    procedure BDeleteAliasClick(Sender: TObject);
    procedure OnLog(Sender:TObject;aMessage:WideString);
    procedure BPreviewTreeClick(Sender: TObject);
  private
    { Private declarations }
    repclient:TModClient;
    procedure OnGetUsers(alist:TStringList);
    procedure OnGetTree(alist:TStringList);
    procedure OnGetAliases(alist:TStringList);
  public
    { Public declarations }
  end;

var
  FMain: TFMain;

implementation

uses unewuser, unewalias, ureptree;

{$R *.xfm}

procedure TFMain.FormCreate(Sender: TObject);
begin
 Trans.Active:=True;
 Caption:=Trans.LoadString(749,Caption);
 Application.Title:=Caption;
 LHost.Caption:=Trans.LoadString(747,LHost.Caption);
 GUser.Caption:=Trans.LoadString(750,GUser.Caption);
 LUserName.Caption:=Trans.LoadString(751,LUserName.Caption);
 LPassword.Caption:=Trans.LoadString(752,LPassword.Caption);
 BConnect.Caption:=Trans.LoadString(753,BConnect.Caption);
 GServerInfo.Caption:=Trans.LoadString(755,GServerinfo.Caption);
 GUsers.Caption:=Trans.LoadString(756,GUsers.Caption);
 BDeleteUser.Caption:=Trans.LoadString(150,BDeleteUser.Caption);
 BAddUser.Caption:=Trans.LoadString(149,BAddUser.Caption);
 BChangePassword.Caption:=Trans.LoadString(757,BChangePassword.Caption);
 GReportDirectories.Caption:=Trans.LoadString(758,GReportDirectories.Caption);
 DDirectoriesAlias.DisplayLabel:=Trans.LoadString(759,DDirectoriesAlias.DisplayLabel);
 DDirectoriesServerPath.DisplayLabel:=Trans.LoadString(760,DDirectoriesServerpath.DisplayLabel);
 DDirectories.CreateDataset;
 BDeleteAlias.Caption:=Trans.LoadString(150,BDeleteAlias.Caption);
 BAddAlias.Caption:=Trans.LoadString(149,BAddAlias.Caption);
 BPreviewTree.Caption:=Trans.LoadString(761,BPreviewTree.Caption);
 BCloseConnection.Caption:=Trans.LoadString(762,BCloseConnection.Caption);
 LPort.Caption:=TranslateStr(829,LPort.Caption);
end;

procedure TFMain.BConnectClick(Sender: TObject);
begin
 repclient:=Connect(ComboHost.Text,EUsername.Text,EPassword.Text,StrToInt(ComboPort.Text));
 ComboHost.Enabled:=False;
 ComboPort.Enabled:=False;
 GUser.Visible:=False;
 GServerInfo.Visible:=True;
 repclient.OnGetUsers:=OnGetUsers;
 repclient.OnGetAliases:=OnGetAliases;
 repclient.OnLog:=OnLog;
 repclient.OnError:=OnLog;
 repclient.OnGetTree:=OnGetTree;
 repclient.GetUsers;
 repclient.GetAliases;
end;

procedure TFMain.BCloseConnectionClick(Sender: TObject);
begin
 if not assigned(repclient) then
  exit;
 Disconnect(repclient);
 repclient:=nil;
 ComboHost.Enabled:=True;
 ComboPort.Enabled:=True;
 GServerInfo.Visible:=False;
 GUser.Visible:=True;
end;

procedure TFMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if Assigned(repclient) then
 begin
  BCloseConnectionClick(Self);
 end;
end;

procedure TFmain.OnGetUsers(alist:TStringList);
begin
 LUsers.Items.Assign(Alist);
end;

procedure TFmain.OnGetAliases(alist:TStringList);
var
 i:integer;
begin
 DDirectories.DisableControls;
 try
  DDirectories.Close;
  DDirectories.CreateDataSet;
  for i:=0 to alist.count-1 do
  begin
   DDirectories.Append;
   DDirectoriesAlias.AsString:=alist.Names[i];
   DDirectoriesServerPath.AsString:=alist.Values[alist.Names[i]];
   DDirectories.Post;
  end;
 finally
  DDirectories.EnableControls;
 end;
end;

procedure TFMain.BAddUserClick(Sender: TObject);
var
 username,password:string;
begin
 // Shows the create user dialog
 username:='';
 password:='';
 if Not AskUserNameAndPassword(username,password,false) then
  exit;
 repclient.AddUser(username,password);
 repclient.GetUsers;
end;

procedure TFMain.BDeleteUserClick(Sender: TObject);
begin
 if (LUSers.ItemIndex)<0 then
  exit;
 repclient.DeleteUser(LUSers.Items.Strings[LUsers.ItemIndex]);
 repclient.GetUsers;
end;

procedure TFMain.BChangePasswordClick(Sender: TObject);
var
 username,password:string;
begin
 if (LUSers.ItemIndex)<0 then
  exit;
 // Shows the create user dialog
 username:=LUsers.Items.Strings[LUsers.ItemIndex];
 password:='';
 if Not AskUserNameAndPassword(username,password,false) then
  exit;
 repclient.AddUser(username,password);
 repclient.GetUsers;
end;

procedure TFMain.BAddAliasClick(Sender: TObject);
var
 aliasname,path:string;
begin
 aliasname:='';
 path:='';
 if Not AskForNewAlias(aliasname,path) then
  exit;
 repclient.AddAlias(aliasname,path);
 repclient.GetAliases;
end;

procedure TFMain.BDeleteAliasClick(Sender: TObject);
begin
 if (DDirectories.EOF and DDirectories.BOF) then
  exit;
 repclient.Deletealias(DDirectoriesALIAS.AsString);
 repclient.GetAliases;
end;

procedure TFMain.OnLog(Sender:TObject;aMessage:WideString);
begin
 if amessage=SRpAuthFailed then
 begin
  ShowMessage(SRpUserorPasswordIncorrect);
  BCloseConnectionClick(Self);
 end;
 LMessages.Items.Insert(0,AMessage);
end;


procedure TFMain.BPreviewTreeClick(Sender: TObject);
begin
 if (DDirectories.EOF and DDirectories.BOF) then
  exit;
 repclient.GetTree(DDirectoriesAlias.AsString);;
end;

procedure TFMain.OnGetTree(alist:TStringList);
begin
 ShowReportTree(alist);
end;


end.
