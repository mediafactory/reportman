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
 DB,rpparams,Inifiles,rptypes,
{$IFDEF MSWINDOWS}
  dbtables,
{$ENDIF}
 SqlConst;

{$IFDEF LINUX}
const
 DBXDRIVERFILENAME='dbxdrivers';
 DBXCONFIGFILENAME='dbxconnections';
{$ENDIF}
type
 TRpDbDriver=(rpdatadbexpress,rpdatabde,rpdataado,rpdataibx);


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
{$IFDEF MSWINDOWS}
   FBDEAlias:string;
   FBDEDatabase:TDatabase;
{$ENDIF}
   FDriver:TRpDbDriver;
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
   property Driver:TRpDbDriver read FDriver write FDriver default rpdatadbexpress;
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
   FSQLInternalQuery:TDataset;
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

procedure UpdateConAdmin;

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
 FBDEDatabase.free;
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
  FDriver:=TRpDatabaseInfoItem(Source).FDriver;
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
 FDriver:=rpdatadbexpress;
end;


procedure UpdateConAdmin;
begin
 if Assigned(ConAdmin) then
 begin
  ConAdmin.Free;
  ConAdmin:=nil;
 end;
 ConAdmin:=TRpConnAdmin.Create;
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
 case Fdriver of
  rpdatadbexpress:
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
      FSQLConnection.Connected:=true;
     end;
   end;
  rpdatabde:
   begin
{$IFDEF MSWINDOWS}
    FBDEAlias:=Alias;
    if Not Assigned(FBDEDatabase) then
     FBDEDatabase:=TDatabase.Create(nil);
    if FBDEDatabase.DatabaseName=FBDEAlias then
    begin
     if FBDEDatabase.Connected then
      exit;
    end
    else
    begin
     FBDEDatabase.Connected:=false;
    end;
    FBDEDatabase.DatabaseName:=FBDEAlias;
    if FLoadParams then
     Session.GetAliasParams(FBDEAlias,FBDEDatabase.Params);
    FBDEDatabase.LoginPrompt:=LoginPrompt;
    FBDEDatabase.Connected:=true;
{$ENDIF}
   end;
 end;
end;

procedure TRpDatabaseinfoitem.DisConnect;
begin
 if Assigned(FSQLConnection) then
 begin
  if FSQLCOnnection=FSQLInternalConnection then
  begin
   FSQLConnection.Connected:=False;
  end;
 end;
{$IFDEF MSWINDOWS}
 if Assigned(FBDEDatabase) then
  FBDEDatabase.Connected:=false;
{$ENDIF}
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
   // Opens the connection
   index:=databaseinfo.IndexOf(Databasealias);
   if index<0 then
    Raise Exception.Create(SRPDabaseAliasNotFound+' : '+FDatabaseAlias);
   databaseinfo.items[index].Connect;


   if not assigned(FSQLInternalQuery) then
   begin
    case databaseinfo.items[index].FDriver of
     rpdatadbexpress:
      begin
       FSQLInternalQuery:=TSQLQuery.Create(nil);
      end;
     rpdatabde:
      begin
       FSQLInternalQuery:=TQuery.Create(nil);
      end;
    end;
   end
   else
   begin
    case databaseinfo.items[index].FDriver of
     rpdatadbexpress:
      begin
       if Not (FSQLInternalQuery is TSQLQuery) then
       begin
        FSQLInternalQuery.Free;
        FSQLInternalQuery:=nil;
       end;
       FSQLInternalQuery:=TSQLQuery.Create(nil);
      end;
     rpdatabde:
      begin
       if Not (FSQLInternalQuery is TQuery) then
       begin
        FSQLInternalQuery.Free;
        FSQLInternalQuery:=nil;
       end;
       FSQLInternalQuery:=TQuery.Create(nil);
      end;
    end;
   end;

   FDataset:=FSQLInternalQuery;

   // Assigns the connectoin
   case databaseinfo.items[index].Driver of
    rpdatadbexpress:
     begin
      TSQLQuery(FSQLInternalQuery).SQLConnection:=
       databaseinfo.items[index].SQLConnection;
      TSQLQuery(FSQLInternalQuery).SQL.Text:=SQL;
     end;
    rpdatabde:
     begin
      TQuery(FSQLInternalQuery).DatabaseName:=databaseinfo[index].FBDEAlias;
      TQuery(FSQLInternalQuery).SQL.Text:=SQL;
     end;
   end;
   // Use the datasource
   if Assigned(datainfosource) then
   begin
    case databaseinfo.items[index].Driver of
     rpdatadbexpress:
      begin
       if Not Assigned(TSQLQuery(FSQLInternalQuery).DataSource) then
        TSQLQuery(FSQLInternalQuery).DataSource:=TDataSource.Create(nil);
       TSQLQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.Dataset;
      end;
     rpdatabde:
      begin
       if Not Assigned(TQuery(FSQLInternalQuery).DataSource) then
        TQuery(FSQLInternalQuery).DataSource:=TDataSource.Create(nil);
       TQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.Dataset;
      end;
    end;
   end;
   // Assigns parameters
   for i:=0 to params.count-1 do
   begin
    param:=params.items[i];
    index:=param.Datasets.IndexOf(Alias);
    if index>=0 then
    begin
     case databaseinfo.items[index].Driver of
      rpdatadbexpress:
       begin
        TSQLQuery(FSQLInternalQuery).ParamByName(param.Name).DataType:=
         ParamTypeToDataType(param.ParamType);
        TSQLQuery(FSQLInternalQuery).ParamByName(param.Name).Value:=param.Value;
       end;
      rpdatabde:
       begin
        TQuery(FSQLInternalQuery).ParamByName(param.Name).DataType:=
         ParamTypeToDataType(param.ParamType);
        TQuery(FSQLInternalQuery).ParamByName(param.Name).Value:=param.Value;
       end;
     end;
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
