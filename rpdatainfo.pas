{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpdatainfo                                      }
{                                                       }
{                                                       }
{       A collection of information for opening datasets}
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

unit rpdatainfo;

interface

uses Classes,SysUtils,SqlExpr,rpconsts, DBXpress,
 DB,rpparams,Inifiles,
 IdGlobal,SqlConst;

{$IFDEF LINUX}
const
 DBXDRIVERFILENAME='dbxdrivers';
 DBXCONFIGFILENAME='dbxconnections';
{$ENDIF}
type
 TRpConnAdmin=class(TObject)
  public
   driverfilename:string;
   configfilename:string;
   config:TMemInifile;
   drivers:TMemInifile;
   constructor Create;
   destructor destroy;override;
   procedure LoadConfig;
   procedure GetConnectionParams(conname:string;params:TStrings);
   procedure GetDriverLibNames(const drivername:string;var LibraryName,VendorLib:string);
 end;

 TRpDatabaseInfoItem=class(TCollectionItem)
  private
   FAlias:string;
   FSQLConnection:TSQLConnection;
   FSQLInternalConnection:TSQLConnection;
   FConfigFile:string;
   FLoadParams:boolean;
   FLoadDriverParams:boolean;
   FLoginPrompt:boolean;
   procedure SetAlias(Value:string);
   procedure SetConfigFile(Value:string);
   procedure SetLoadParams(Value:boolean);
   procedure SetLoadDriverParams(Value:boolean);
   procedure SetLoginPrompt(Value:boolean);
  public
   procedure Assign(Source:TPersistent);override;
   destructor Destroy;override;
   procedure Connect;
   procedure DisConnect;
   constructor Create(Collection:TCollection);override;
   property SQLConnection:TSQLConnection read FSQLConnection write FSQLConnection;
  published
   property Alias:string read FAlias write SetAlias;
   property ConfigFile:string read FConfigFile write SetConfigFile;
   property LoadParams:boolean read FLoadParams write SetLoadParams;
   property LoadDriverParams:boolean read FLoadDriverParams write SetLoadDriverParams;
   property LoginPrompt:boolean read FLoginPrompt write SetLoginPrompt;
  end;

  TRpDatabaseInfoList=class(TCollection)
  private
   FReport:TComponent;
   function GetItem(Index:Integer):TRpDatabaseInfoItem;
   procedure SetItem(index:integer;Value:TRpDatabaseInfoItem);
  public
   function Add(alias:string):TRpDatabaseInfoItem;
   function IndexOf(Value:string):integer;
   property Items[index:integer]:TRpDatabaseInfoItem read GetItem write SetItem;default;
   constructor Create(rep:TComponent);
   destructor Destroy;override;
  end;


 TRpDataInfoItem=class(TCollectionItem)
  private
   FDatabaseAlias:string;
   FSQL:widestring;
   FDataSource:string;
   FAlias:string;
   FDataset:TDataset;
   FSQLInternalQuery:TSQLQuery;
   connecting:boolean;
   procedure SetDatabaseAlias(Value:string);
   procedure SetAlias(Value:string);
   procedure SetDataSource(Value:string);
   procedure SetSQL(Value:widestring);
  public
   procedure Assign(Source:TPersistent);override;
   procedure Connect(databaseinfo:TRpDatabaseInfoList;params:TRpParamList);
   procedure Disconnect;
   destructor Destroy;override;
   property Dataset:TDataset read FDataset write FDataset;
  published
   property Alias:string read FAlias write SetAlias;
   property DatabaseAlias:string read FDatabaseAlias write SetDatabaseAlias;
   property SQL:widestring read FSQL write SetSQL;
   property DataSource:string read FDatasource write SetDataSource;
  end;

 TRpDataInfoList=class(TCollection)
  private
   FReport:TComponent;
   function GetItem(Index:Integer):TRpDataInfoItem;
   procedure SetItem(index:integer;Value:TRpDataInfoItem);
  public
   function Add(alias:string):TRpDataInfoItem;
   function IndexOf(Value:string):integer;
   property Items[index:integer]:TRpDataInfoItem read GetItem write SetItem;default;
   constructor Create(rep:TComponent);
  end;

procedure CreateConAdmin;

implementation

var
 ConAdmin:TRpConnAdmin;


procedure TRpDataInfoItem.SetDatabaseAlias(Value:string);
begin
 FDatabaseAlias:=AnsiUpperCase(Value);
 Changed(False);
end;

procedure TRpDataInfoItem.SetAlias(Value:string);
begin
 Value:=AnsiUpperCase(Value);
 FAlias:=AnsiUpperCase(Value);
 Changed(False);
end;

procedure TRpDataInfoItem.SetDataSource(Value:string);
begin
 Value:=TRim(AnsiUpperCase(Value));
 FDataSource:=AnsiUpperCase(Value);
 Changed(False);
end;

procedure TRpDataInfoItem.SetSQL(Value:widestring);
begin
 FSQL:=Value;
 Changed(False);
end;

procedure TRpDataInfoItem.Assign(Source:TPersistent);
begin
 if Source is TRpDataInfoItem then
 begin
  FAlias:=TRpDataInfoItem(Source).FAlias;
  FDatabaseAlias:=TRpDataInfoItem(Source).FDatabaseAlias;
  FDataSource:=TRpDataInfoItem(Source).FDataSource;
  FSQL:=TRpDataInfoItem(Source).FSQL;
 end
 else
  inherited Assign(Source);
end;

constructor TRpDataInfoList.Create(rep:TComponent);
begin
 inherited Create(TRpDataInfoItem);
 FReport:=rep;
end;


function TRpDataInfoList.GetItem(Index:Integer):TRpDataInfoItem;
begin
 Result:=TRpDataInfoItem(inherited GetItem(index));
end;

procedure TRpDataInfoList.SetItem(index:integer;Value:TRpDataInfoItem);
begin
 inherited SetItem(Index,Value);
end;

function TRpDataInfoList.Add(alias:string):TRpDataInfoItem;
begin
 // Then function is defined by teh class TCollectionItem
 alias:=AnsiUpperCase(alias);
 if Indexof(alias)>0 then
  Raise Exception.Create(SRpAliasExists);
 Result:=TRpDataInfoItem(inherited Add);
 Result.Alias:=alias;
end;

function TRpDataInfoList.IndexOf(Value:string):integer;
var
 i:integer;
begin
 Value:=AnsiUpperCase(Value);
 Result:=-1;
 i:=0;
 While i<count do
 begin
  if items[i].FAlias=Value then
  begin
   Result:=i;
   break;
  end;
  inc(i);
 end;
end;

// Database info


procedure TRpDatabaseInfoItem.SetAlias(Value:string);
begin
 Value:=AnsiUpperCase(Value);
 FAlias:=AnsiUpperCase(Value);
 Changed(False);
end;

destructor TRpDatabaseInfoList.Destroy;
begin
 inherited Destroy;
end;


destructor TRpDatabaseInfoItem.Destroy;
begin
 FSQLInternalConnection.free;
 inherited Destroy;
end;


procedure TRpDatabaseInfoItem.SetLoadParams(Value:boolean);
begin
 FLoadParams:=Value;
 Changed(False);
end;

procedure TRpDatabaseInfoItem.SetLoadDriverParams(Value:boolean);
begin
 FLoadDriverParams:=Value;
 Changed(False);
end;

procedure TRpDatabaseInfoItem.SetLoginPrompt(Value:boolean);
begin
 FLoginPrompt:=Value;
 Changed(False);
end;

procedure TRpDatabaseInfoItem.SetConfigFile(Value:string);
begin
 FConfigFile:=Value;
 Changed(False);
end;

procedure TRpDatabaseInfoItem.Assign(Source:TPersistent);
begin
 if Source is TRpDatabaseInfoItem then
 begin
  FAlias:=TRpDatabaseInfoItem(Source).FAlias;
  FLoadParams:=TRpDatabaseInfoItem(Source).FLoadParams;
  FLoginPrompt:=TRpDatabaseInfoItem(Source).FLoginPrompt;
  FLoadDriverParams:=TRpDatabaseInfoItem(Source).FLoadDriverParams;
  FConfigFile:=TRpDatabaseInfoItem(Source).FConfigFile;
 end
 else
  inherited Assign(Source);
end;

constructor TRpDatabaseInfoList.Create(rep:TComponent);
begin
 inherited Create(TRpDatabaseInfoItem);
 FReport:=rep;
end;

function TRpDatabaseInfoList.GetItem(Index:Integer):TRpDatabaseInfoItem;
begin
 Result:=TRpDatabaseInfoItem(inherited GetItem(index));
end;

procedure TRpDatabaseInfoList.SetItem(index:integer;Value:TRpDatabaseInfoItem);
begin
 inherited SetItem(Index,Value);
end;

function TRpDatabaseInfoList.Add(alias:string):TRpDatabaseInfoItem;
begin
 // Then function is defined by teh class TCollectionItem
 alias:=AnsiUpperCase(alias);
 if Indexof(alias)>0 then
  Raise Exception.Create(SRpAliasExists);
 Result:=TRpDatabaseInfoItem(inherited Add);
 Result.Alias:=alias;
end;

function TRpDatabaseInfoList.IndexOf(Value:string):integer;
var
 i:integer;
begin
 Value:=AnsiUppercase(Value);
 Result:=-1;
 i:=0;
 While i<count do
 begin
  if items[i].FAlias=Value then
  begin
   Result:=i;
   break;
  end;
  inc(i);
 end;
end;

constructor TRpDatabaseinfoitem.Create(Collection:TCollection);
begin
 inherited Create(Collection);

 FLoadParams:=true;
 FLoadDriverParams:=true;
end;



procedure CreateConAdmin;
begin
 ConAdmin:=TRpConnAdmin.Create;
end;

procedure TRpDatabaseinfoitem.Connect;
var
 conname:string;
 funcname,drivername,vendorlib,libraryname:string;
begin
 if Not Assigned(FSQLConnection) then
 begin
  if Not Assigned(FSQLInternalConnection) then
   FSQLInternalConnection:=TSQLConnection.Create(nil);
  FSQLConnection:=FSQLInternalConnection;
 end;
 if FSQLCOnnection.Connected then
  exit;
 FSQLConnection.LoginPrompt:=FLoginPrompt;
 conname:=alias;
 FSQLConnection.ConnectionName:=alias;
 // Load Connection parameters
 if (FLoadParams) then
 begin
  if Not Assigned(ConAdmin) then
   CreateConAdmin;
  ConAdmin.GetConnectionParams(conname,FSQLConnection.params);
 end;
 // Load vendor lib, library name...
 if (FLoadDriverParams) then
 begin
  if Not Assigned(ConAdmin) then
   CreateConAdmin;
  drivername:=FSQLCOnnection.DriverName;
  if Length(drivername)<1 then
   drivername:=FSQLConnection.params.Values['DriverName'];
  if Length(drivername)<1 then
   drivername:=FSQLConnection.params.Values['Drivername'];
  if Length(drivername)<1 then
   Raise Exception.Create(SRpNoDriverName+conname);
  funcname:=ConAdmin.drivers.ReadString(drivername,'GetDriverFunc','');
  ConAdmin.GetDriverLibNames(drivername,LibraryName,VendorLib);
  // Assigns all
  FSQLConnection.DriverName:=drivername;
  FSQLConnection.VendorLib:=vendorlib;
  FSQLConnection.LibraryName:=libraryname;
  FSQLConnection.GetDriverFunc:=funcname;

 end;

// FSQLConnection.Connected:=true;
end;

procedure TRpDatabaseinfoitem.DisConnect;
begin
 if Assigned(FSQLConnection) then
 begin
  if FSQLCOnnection=FSQLInternalConnection then
   FSQLConnection.Connected:=False;
 end;
end;

procedure TRpDataInfoItem.Connect(databaseinfo:TRpDatabaseInfoList;params:TRpParamList);
var
 index:integer;
 i:integer;
 datainfosource:TRpDatainfoItem;
 doexit:boolean;
 param:TRpParam;
begin
 if connecting then
  Raise Exception.Create(SRpCircularDatalink+alias);
 connecting:=true;
 try
  doexit:=false;
  datainfosource:=nil;
  if assigned(FDataset) then
  begin
   if FDataset.Active then
    doexit:=true;
   if FDataset<>FSQLInternalQuery then
   begin
    FDataset.Active:=true;
    doexit:=true;
   end;
  end;
  if (not doexit) then
  begin
   // Connect first the parent datasource
   if Length(DataSource)>0 then
   begin
    index:=TRpDatainfolist(Collection).IndexOf(datasource);
    if index<0 then
     Raise Exception.Create(SRPMasterNotFound+alias+' - '+datasource);
    datainfosource:=TRpDatainfolist(Collection).Items[index];
    datainfosource.connect(databaseinfo,params);
   end;
   if not assigned(FSQLInternalQuery) then
    FSQLInternalQuery:=TSQLQuery.Create(nil);
   FDataset:=FSQLInternalQuery;
   // Opens the connection
   index:=databaseinfo.IndexOf(Databasealias);
   if index<0 then
    Raise Exception.Create(SRPDabaseAliasNotFound+' : '+FDatabaseAlias);
   databaseinfo.items[index].Connect;

   FSQLInternalQuery.SQLConnection:=databaseinfo.items[index].SQLConnection;
   FSQLInternalQuery.SQL.Text:=SQL;
   // Use the datasource
   if Assigned(datainfosource) then
   begin
    if Not Assigned(FSQLInternalQuery.DataSource) then
     FSQLInternalQuery.DataSource:=TDataSource.Create(nil);
    FSQLInternalQuery.DataSource.DataSet:=datainfosource.Dataset;
   end;
   // Assigns parameters
   for i:=0 to params.count-1 do
   begin
    param:=params.items[i];
    index:=param.Datasets.IndexOf(Alias);
    if index>=0 then
    begin
     FSQLInternalQuery.ParamByName(param.Name).DataType:=
      ParamTypeToDataType(param.ParamType);
     FSQLInternalQuery.ParamByName(param.Name).Value:=param.Value;
    end;
   end;
   FSQLInternalQuery.Active:=true;
  end;
 finally
  connecting:=false;
 end;
end;

procedure TRpDataInfoItem.DisConnect;
begin
 if Assigned(FDataset) then
 begin
  FDataset.Active:=false;
 end;
end;

destructor TRpDataInfoItem.Destroy;
begin
 try
  if assigned(FSQLInternalQuery) then
  begin
   if Assigned(FSQLInternalQuery.datasource) then
    FSQLInternalQuery.datasource.free;
   FSQLInternalQuery.free;
  end;
 finally
  inherited Destroy;
 end;
end;

constructor TRpConnAdmin.Create;
begin
 LoadConfig;
end;

destructor TRpConnAdmin.Destroy;
begin
 if Assigned(config) then
 begin
  config.free;
  config:=nil;
 end;
 if Assigned(drivers) then
 begin
  drivers.free;
  drivers:=nil;
 end;
 inherited Destroy;
end;

procedure TRpConnAdmin.LoadConfig;
{$IFDEF LINUX}
var
 configdir:string;
{$ENDIF}
begin
 if Assigned(config) then
 begin
  config.free;
  config:=nil;
 end;
 if Assigned(drivers) then
 begin
  drivers.free;
  drivers:=nil;
 end;

 // Looks for ./borland in Linux registry in Windows
{$IFDEF LINUX}
 configdir:=GetEnvironmentVariable('HOME')+'/.borland';
 if Not DirectoryExists(configdir) then
  if Not CreateDir(configdir) then
   Raise Exception.Create(SRpDirCantBeCreated+configdir);
 driverfilename:=configdir+'/'+DBXDRIVERFILENAME;
 configfilename:=configdir+'/'+DBXCONFIGFILENAME;
{$ENDIF}
{$IFDEF MSWINDOWS}
 // Looks the registry and if there is not registry
 // Use current dir
 driverfilename:=GetDriverRegistryFile;
 configfilename:=GetConnectionRegistryFile;
{$ENDIF}
 if FileExists(driverfilename) then
 begin
  drivers:=TMemInifile.Create(driverfilename);
 end
 else
 begin
{$IFDEF MSWINDOWS}
   Raise Exception.Create(SRpConfigFileNotExists+driverfilename);
{$ENDIF}
{$IFDEF LINUX}
  // Check if exists in the current dir
  if FileExists(DBXDRIVERFILENAME) then
  begin
   drivers:=TMemIniFile.Create(DBXDRIVERFILENAME);
   CopyFileTo(DBXDRIVERFILENAME,driverfilename);
  end
  else
   Raise Exception.Create(SRpConfigFileNotExists+DBXDRIVERFILENAME);
{$ENDIF}
 end;
 if FileExists(configfilename) then
 begin
  config:=TMemInifile.Create(configfilename);
 end
 else
 begin
{$IFDEF MSWINDOWS}
   Raise Exception.Create(SRpConfigFileNotExists+driverfilename);
{$ENDIF}
{$IFDEF LINUX}
  // Check if exists in the current dir
  if FileExists(DBXCONFIGFILENAME) then
  begin
   config:=TMemInifile.Create(DBXCONFIGFILENAME);
   CopyFileTo(DBXCONFIGFILENAME,configfilename);
  end
  else
   Raise Exception.Create(SRpConfigFileNotExists+DBXCONFIGFILENAME);
{$ENDIF}
 end;
end;

procedure TRpConnAdmin.GetConnectionParams(conname:string;params:TStrings);
begin
 config.ReadSectionValues(conname,params);
end;

procedure TRpConnAdmin.GetDriverLibNames(const drivername:string;var LibraryName,VendorLib:string);
begin
 LibraryName:=drivers.ReadString(DriverName,DLLLIB_KEY,'');
 VendorLib:=drivers.ReadString(DriverName,VENDORLIB_KEY,'');
end;

initialization

ConAdmin:=nil;

finalization

if Assigned(ConAdmin) then
begin
 ConAdmin.free;
 ConAdmin:=nil;
end;

end.
