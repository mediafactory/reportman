{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpdbxconfig                                     }
{                                                       }
{       Configuration dialog for connections            }
{       it stores all info in config files              }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}

unit rpdbxconfig;

interface

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,QDialogs,
  SQLExpr,DBConnAdmin, DBXpress, DB,rpconsts, QComCtrls, QImgList;

const
 CONTROL_DISTANCEY=5;
 CONTROL_DISTANCEX=10;
 CONTROL_DISTANCEX2=150;
 CONTROL_WIDTHX=200;
 LABEL_INCY=4;
type
  TFDBXConfig = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    EDriversFile: TEdit;
    EConnectionsFile: TEdit;
    SQLConnection1: TSQLConnection;
    Label3: TLabel;
    ComboDrivers: TComboBox;
    LConnections: TListBox;
    ScrollParams: TScrollBox;
    ToolBar1: TToolBar;
    BAdd: TToolButton;
    BDelete: TToolButton;
    ToolButton1: TToolButton;
    BShowProps: TToolButton;
    ImageList1: TImageList;
    ToolButton2: TToolButton;
    BConnect: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure ComboDriversClick(Sender: TObject);
    procedure LConnectionsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BShowPropsClick(Sender: TObject);
    procedure BConnectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    DriversFile:string;
    ConAdmin:IConnectionAdmin;
    ConAdminObj:TConnectionAdmin;
    params:TStringList;
    connectionname:string;
    procedure FreeParamsControls;
    procedure CreateParamsControls;
    procedure Edit1Change(Sender:TObject);
  public
    { Public declarations }
    ConnectionsFile:string;
  end;

procedure ShowDBXConfig(ConnectionsFile:string='');

implementation

{$R *.xfm}


procedure ShowDBXConfig(ConnectionsFile:string);
var
 dia:TFDBXCOnfig;
begin
 dia:=TFDBXConfig.Create(Application);
 try
  dia.showmodal;
  dia.ConnectionsFile:=Trim(ConnectionsFile);
 finally
  dia.free;
 end;
end;

procedure TFDBXConfig.FormCreate(Sender: TObject);
begin
 params:=TStringList.Create;
 // Read the drivers file
 DriversFile:=GetDriverRegistryFile;
 EDriversFile.Text:=DriversFile;
 // Read the connections file
 if Length(ConnectionsFile)<1 then
  EConnectionsFile.Text:=GetConnectionRegistryFile
 else
  EConnectionsFile.Text:=ConnectionsFile;
 // Read the database connections
 ConAdmin:=GetConnectionAdmin;
 ConAdminObj:=TConnectionAdmin.Create;
 ConAdmin:=ConAdminObj;
 ConAdmin.GetDriverNames(ComboDrivers.Items);
 ComboDrivers.Items.Insert(0,SRpAllDriver);
 ComboDrivers.ItemIndex:=0;
 ComboDriversClick(Self);
end;

procedure TFDBXConfig.ComboDriversClick(Sender: TObject);
var
 drivername:string;
begin
 // Load the connections
 if Not Assigned(ConAdmin) then
  exit;
 drivername:=ComboDrivers.Text;
 if drivername=SRpAllDriver then
  drivername:='';
 ConAdmin.GetConnectionNames(LConnections.items,drivername);
 if LConnections.Items.Count>0 then
  LConnections.ItemIndex:=0
 else
  LConnections.ItemIndex:=-1;
 LConnectionsClick(Self);
end;


procedure TFDBXConfig.FreeParamsControls;
var
 i:integer;
begin
 i:=0;
 While  ScrollParams.ControlCount>0 do
 begin
  ScrollParams.Controls[i].Free;
 end;
end;

procedure TFDBXConfig.CreateParamsControls;
var
 i:integer;
 index:integer;
 label1:TLabel;
 Edit1:TWinControl;
 top:integer;
 alist:TStringList;
begin
 if Not Assigned(ConAdmin) then
  exit;
 alist:=TStringList.create;
 try
  ConAdminObj.DriverConfig.ReadSections(alist);
  top:=CONTROL_DISTANCEY;
  ConAdmin.GetConnectionParams(connectionname,params);
  for i:=0 to params.Count-1 do
  begin
   label1:=TLabel.Create(Self);
   label1.Caption:=params.Names[i];
   label1.Top:=Top+LABEL_INCY;
   label1.Left:=CONTROL_DISTANCEX;
   label1.Parent:=ScrollParams;
   // It can be a combo with different options
   index:=alist.indexof(params.Names[i]);
   if index<0 then
   begin
    Edit1:=TEdit.Create(Self);
    TEdit(Edit1).Text:=params.Values[params.Names[i]];
    if AnsiUpperCase(params.Names[i])='DRIVERNAME' then
    begin
     TEdit(Edit1).ReadOnly:=true;
     TEdit(Edit1).Color:=clButton;
    end
    else
     TEdit(Edit1).OnChange:=Edit1Change;
   end
   else
   begin
    Edit1:=TComboBox.Create(Self);
    TComboBox(Edit1).Style:=csDropDownList;
    ConAdminObj.DriverConfig.ReadSection(alist.strings[index],TComboBox(Edit1).Items);
    TComboBox(Edit1).Text:=params.Values[params.Names[i]];
    TComboBox(Edit1).ItemIndex:=TComboBox(Edit1).Items.IndexOf(params.Values[params.Names[i]]);
    TComboBox(Edit1).OnChange:=Edit1Change;
   end;

   Edit1.Tag:=i;
   Edit1.Top:=Top;
   Edit1.Left:=CONTROL_DISTANCEX2;
   Edit1.Width:=CONTROL_WIDTHX;

   Edit1.Parent:=ScrollParams;

   top:=top+Edit1.Height+CONTROL_DISTANCEY;
  end;
 finally
  alist.free;
 end;
end;

procedure TFDBXConfig.LConnectionsClick(Sender: TObject);
begin
 if LConnections.ItemIndex<0 then
 begin
  ScrollParams.Visible:=false;
  exit;
 end;
 ScrollParams.Visible:=true;
 connectionname:=LConnections.Items.strings[LConnections.ItemIndex];
 FreeParamsControls;
 CreateParamsControls;
end;


procedure TFDBXConfig.FormDestroy(Sender: TObject);
begin
 params.Free;
end;

procedure TFDBXConfig.Edit1Change(Sender:TObject);
var
 paramvalue:string;
 paramname:string;
begin
 if Not Assigned(ConAdmin) then
  exit;
 paramname:=params.Names[TEdit(Sender).Tag];
 paramvalue:=TEdit(Sender).Text;
 params.Values[paramname]:=paramvalue;
 ConAdmin.ModifyConnection(connectionname,params);
end;

procedure TFDBXConfig.BAddClick(Sender: TObject);
var
 newname:string;
begin
 if Not Assigned(ConAdmin) then
  exit;
 if ComboDrivers.ItemIndex=0 then
  Raise Exception.Create(SRpSelectDriver);
 newname:=Trim(InputBox(SRpNewConnection,SRpConnectionName,''));
 if Length(newname)<1 then
  exit;
 ConAdmin.AddConnection(newname,ComboDrivers.Text);
 ComboDriversClick(Self);
end;

procedure TFDBXConfig.BDeleteClick(Sender: TObject);
var
 conname:string;
begin
 if Not Assigned(ConAdmin) then
  exit;
 if LConnections.ItemIndex<0 then
  exit;
 conname:=LConnections.Items.Strings[LConnections.ItemIndex];
 if mrOk=MessageDlg(SRpDropConnection,SRpSureDropConnection+conname,mtWarning,[mbok,mbCancel],0) then
 begin
  ConAdmin.DeleteConnection(conname);
  ComboDriversCLick(Self);
 end;
end;

procedure TFDBXConfig.BShowPropsClick(Sender: TObject);
var
 VendorLib,LibraryName:string;
begin
 if Not Assigned(ConAdmin) then
  exit;
 if ComboDrivers.ItemIndex=0 then
  Raise Exception.Create(SRpSelectDriver);
 ConAdmin.GetDriverLibNames(ComboDrivers.Text,LibraryName,VendorLib);
 ShowMessage(SRpVendorLib+':'+VendorLib+#10+SRpLibraryName+':'+LibraryName);
end;

procedure TFDBXConfig.BConnectClick(Sender: TObject);
var
 conname:string;
 funcname,drivername,vendorlib,libraryname:string;
begin
 if Not Assigned(ConAdmin) then
  exit;
 if LConnections.ItemIndex<0 then
  Raise Exception.Create(SRpSelectConnectionFirst);
 conname:=LConnections.Items.strings[Lconnections.itemindex];
 // Assigns properties to SQLCOn.
 SQLConnection1.ConnectionName:=conname;
 ConAdmin.GetConnectionParams(conname,SQLConnection1.params);
 drivername:=SQLConnection1.params.Values['DriverName'];
 funcname:=ConAdminObj.DriverConfig.ReadString(drivername,'GetDriverFunc','');
 ConAdmin.GetDriverLibNames(drivername,LibraryName,VendorLib);
 SQLConnection1.DriverName:=drivername;
 SQLConnection1.VendorLib:=vendorlib;
 SQLConnection1.LibraryName:=libraryname;
 SQLConnection1.GetDriverFunc:=funcname;
 SQLConnection1.Connected:=true;
 ShowMessage(SRpConnectionOk);
 SQLConnection1.Connected:=false;
end;

procedure TFDBXConfig.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if Assigned(ConAdminObj) then
  ConAdminObj.ConnectionConfig.UpdateFile;

end;


end.
