unit mainf;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, rptranslator, DB, DBClient, QGrids, QDBGrids;

type
  TFMain = class(TForm)
    LHost: TLabel;
    ComboHost: TComboBox;
    BCheckRunning: TButton;
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
    BChangePassword: TButton;
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
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMain: TFMain;

implementation

{$R *.xfm}

procedure TFMain.FormCreate(Sender: TObject);
begin
 Trans.Active:=True;
 Caption:=Trans.LoadString(749,Caption);
 LHost.Caption:=Trans.LoadString(747,LHost.Caption);
 BCheckRunning.Caption:=Trans.LoadString(748,BCheckRunning.Caption);
 BCheckRunning.Hint:=Trans.LoadString(754,BCheckRunning.Hint);
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
end;

end.
