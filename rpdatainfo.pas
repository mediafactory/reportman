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

{$I rpconf.inc}


uses Classes,SysUtils,
{$IFDEF MSWINDOWS}
 registry,windows,
{$ENDIF}
{$IFDEF LINUX}
 Libc,
{$ENDIF}
{$IFDEF USESQLEXPRESS}
 SqlExpr,DBXpress,SqlConst,//DBExpMYSQL,DbExpMyS,dbExpDB2,dbExpORA,dbExpINT
{$ENDIF}
 rpmdconsts,
 DB,rpparams,Inifiles,rptypes,DBClient,
{$IFDEF USEIBX}
 IBQuery,IBDatabase,
{$ENDIF}
{$IFDEF USEBDE}
  dbtables,
{$ENDIF}
{$IFDEF USEADO}
  adodb,
{$ENDIF}
{$IFDEF USEIBO}
  IB_Components,IBODataset,
{$ENDIF}
{$IFDEF USEVARIANTS}
  Variants,
{$ENDIF}
 rpdataset;

{$IFDEF LINUX}
const
 DBXDRIVERFILENAME='dbxdrivers';
 DBXCONFIGFILENAME='dbxconnections';
{$ENDIF}
type
 TRpDbDriver=(rpdatadbexpress,rpdatamybase,rpdataibx,
  rpdatabde,rpdataado,rpdataibo);


{$IFDEF USECONADMIN}
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
{$ENDIF}


 TRpDatabaseInfoItem=class(TCollectionItem)
  private
   FAlias:string;
{$IFDEF USESQLEXPRESS}
   FSQLConnection:TSQLConnection;
   FSQLInternalConnection:TSQLConnection;
{$ENDIF}
   FConfigFile:string;
   FLoadParams:boolean;
   FLoadDriverParams:boolean;
   FLoginPrompt:boolean;
   FADOConnectionString:widestring;
{$IFDEF USEIBX}
   FIBInternalDatabase:TIBDatabase;
   FIBDatabase:TIBDatabase;
{$ENDIF}
{$IFDEF USEADO}
   FADOConnection:TADOConnection;
{$ENDIF}
{$IFDEF USEBDE}
   FBDEDatabase:TDatabase;
   FBDEAlias:string;
   CreatedBDE:Boolean;
{$ENDIF}
{$IFDEF USEIBO}
   FIBODatabase: TIB_Database;
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
{$IFDEF USESQLEXPRESS}
   property SQLConnection:TSQLConnection read FSQLConnection write FSQLConnection;
{$ENDIF}
{$IFDEF USEIBX}
   property IBDatabase:TIBDatabase read FIBDatabase
    write FIBDatabase;
{$ENDIF}
   function GetStreamFromSQL(sqlsentence:String;bsmode:TBlobStreamMode;params:TStringList):TStream;
   procedure GetTableNames(Alist:TStrings);
   function OpenDatasetFromSQL(sqlsentence:String;params:TStringList;onlyexec:Boolean):TDataset;
  published
   property Alias:string read FAlias write SetAlias;
   property ConfigFile:string read FConfigFile write SetConfigFile;
   property LoadParams:boolean read FLoadParams write SetLoadParams;
   property LoadDriverParams:boolean read FLoadDriverParams write SetLoadDriverParams;
   property LoginPrompt:boolean read FLoginPrompt write SetLoginPrompt;
   property Driver:TRpDbDriver read FDriver write FDriver default rpdatadbexpress;
   property ADOConnectionString:widestring read FADOConnectionString write FADOConnectionString;
  end;

  TRpDatabaseInfoList=class(TCollection)
  private
   FReport:TComponent;
{$IFDEF USEBDE}
   FBDESession:TSession;
{$ENDIF}
   function GetItem(Index:Integer):TRpDatabaseInfoItem;
   procedure SetItem(index:integer;Value:TRpDatabaseInfoItem);
  public
{$IFDEF USEBDE}
   property BDESession:TSession read FBDESession write FBDESession;
{$ENDIF}
   function Add(alias:string):TRpDatabaseInfoItem;
   function IndexOf(Value:string):integer;
   property Items[index:integer]:TRpDatabaseInfoItem read GetItem write SetItem;default;
   constructor Create(rep:TComponent);
   destructor Destroy;override;
  end;

 TRpDatasetType=(rpdquery,rpdtable);


 TRpDataInfoItem=class(TCollectionItem)
  private
   FDatabaseAlias:string;
   FSQL:widestring;
   FDataSource:string;
   FAlias:string;
   FDataset:TDataset;
   FCachedDataset:TRpDataset;
   FSQLInternalQuery:TDataset;
   FMyBaseFilename:string;
   FMyBaseIndexFields:string;
   FBDEIndexFields:string;
   FBDEIndexName:string;
   FBDETable:string;
   FBDEType:TRpDatasetType;
   FBDEMasterFields:string;
   FBDEFilter:string;
   FBDEFirstRange,FBDELastRange:string;
   connecting:boolean;
   FCached:Boolean;
   procedure SetDatabaseAlias(Value:string);
   procedure SetAlias(Value:string);
   procedure SetDataSource(Value:string);
   procedure SetSQL(Value:widestring);
{$IFDEF USEBDE}
   procedure SetRangeForTable(lastrange:boolean);
{$ENDIF}
  public
   procedure Assign(Source:TPersistent);override;
   procedure Connect(databaseinfo:TRpDatabaseInfoList;params:TRpParamList);
   procedure Disconnect;
   destructor Destroy;override;
   constructor Create(Collection:TCollection);override;
   property Dataset:TDataset read FDataset write FDataset;
   property CachedDataset:TRpDataset read FCachedDataset;
   property Cached:Boolean read FCached write FCached;
  published
   property Alias:string read FAlias write SetAlias;
   property DatabaseAlias:string read FDatabaseAlias write SetDatabaseAlias;
   property SQL:widestring read FSQL write SetSQL;
   property DataSource:string read FDatasource write SetDataSource;
   property MyBaseFilename:string read FMyBaseFilename write FMyBaseFilename;
   property MyBaseIndexFields:string read FMyBaseIndexFields write FMyBaseIndexFields;
   property BDEIndexFields:string read FBDEIndexFields write FBDEIndexFields;
   property BDEIndexName:string read FBDEIndexName write FBDEIndexName;
   property BDETable:string read FBDETable write FBDETable;
   property BDEType:TRpDatasetType read FBDEType write FBDEType
    default rpdquery;
   property BDEFilter:string read FBDEFilter write FBDEFilter;
   property BDEMasterFields:string read FBDEMasterFields write FBDEMasterFields;
   property BDEFirstRange:string read FBDEFirstRange write FBDEFirstRange;
   property BDELastRange:string read FBDELastRange write FBDELastRange;
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

{$IFDEF USECONADMIN}
procedure UpdateConAdmin;
{$ENDIF}

implementation

{$IFDEF USEBDE}
uses rpreport,rpeval;
{$ENDIF}
const
  SDRIVERREG_SETTING = 'Driver Registry File';           { Do not localize }
  SCONNECTIONREG_SETTING = 'Connection Registry File';   { Do not localize }
  VENDORLIB_KEY = 'VendorLib';                  { Do not localize }
  DLLLIB_KEY = 'LibraryName';                   { Do not localize }
{$IFDEF MSWINDOWS}
  SDriverConfigFile = 'dbxdrivers.ini';            { Do not localize }
  SConnectionConfigFile = 'dbxconnections.ini';    { Do not localize }
  SDBEXPRESSREG_SETTING = '\Software\Borland\DBExpress'; { Do not localize }
{$ENDIF}
{$IFDEF LINUX}
  SDBEXPRESSREG_USERPATH = '/.borland/';          { Do not localize }
  SDBEXPRESSREG_GLOBALPATH = '/usr/local/etc/';   { Do not localize }
  SDriverConfigFile = 'dbxdrivers';                  { Do not localize }
  SConnectionConfigFile = 'dbxconnections';          { Do not localize }
  SConfExtension = '.conf';                       { Do not localize }
{$ENDIF}

{$IFDEF USECONADMIN}
var
 ConAdmin:TRpConnAdmin;
{$ENDIF}

{$IFDEF USEBDE}
procedure AddParamsFromDBXToBDE(paramssource,params:TStrings);
var
 index:integer;
begin
 index:=paramssource.IndexOfName('Password');
 if index>=0 then
  params.Add(paramssource.Strings[index]);
end;
{$ENDIF}

{$IFDEF USEIBX}
procedure ConvertParamsFromDBXToIBX(base:TIBDatabase);
var
 index:integer;
 params:TStrings;
begin
 params:=base.Params;
 index:=params.IndexOfName('DriverName');
 if index>=0 then
 begin
  if UpperCase(params.Values['DriverName'])<>'INTERBASE' then
   Raise Exception.Create(SRpDriverAliasIsNotInterbase);
  params.Delete(index);
 end;
 index:=params.IndexOfName('Database');
 if index<0 then
  Raise Exception.Create(SRpNoDatabase);
 base.DatabaseName:=params.Values['Database'];
 params.Delete(index);
 index:=params.IndexOfName('BlobSize');
 if index>=0 then
  params.Delete(index);
 index:=params.IndexOfName('CommitRetain');
 if index>=0 then
  params.Delete(index);
 index:=params.IndexOfName('Trim Char');
 if index>=0 then
  params.Delete(index);
 index:=params.IndexOfName('ErrorResourceFile');
 if index>=0 then
  params.Delete(index);
 index:=params.IndexOfName('LocaleCode');
 if index>=0 then
  params.Delete(index);
 index:=params.IndexOfName('Interbase TransIsolation');
 if index>=0 then
  params.Delete(index);
 index:=params.IndexOfName('WaitOnLocks');
 if index>=0 then
  params.Delete(index);
 index:=params.IndexOfName('SQLDialect');
 if index>=0 then
 begin
  base.SQLDialect:=StrToInt(params.Values['SQLDialect']);
  params.Delete(index);
 end;
 index:=params.IndexOfName('RoleName');
 if index>=0 then
 begin
  params.Add('sql_role_name='+params.Values['RoleName']);
  params.Delete(index);
 end;
 index:=params.IndexOfName('ServerCharSet');
 if index>=0 then
 begin
  params.Add('lc_ctype='+params.Values['ServerCharSet']);
  params.Delete(index);
 end;
end;
{$ENDIF}

{$IFDEF USEIBO}
procedure ConvertParamsFromDBXToIBO(base:TIB_Database);
var
 index:integer;
 params:TStrings;
begin
 params:=base.Params;
 index:=params.IndexOfName('DriverName');
 if index>=0 then
 begin
  if UpperCase(params.Values['DriverName'])<>'INTERBASE' then
   Raise Exception.Create(SRpDriverAliasIsNotInterbase);
  params.Delete(index);
 end;
 index:=params.IndexOfName('Database');
 if index<0 then
  Raise Exception.Create(SRpNoDatabase);
 base.DatabaseName:=params.Values['Database'];
 params.Delete(index);
 index:=params.IndexOfName('BlobSize');
 if index>=0 then
  params.Delete(index);
 index:=params.IndexOfName('CommitRetain');
 if index>=0 then
  params.Delete(index);
 index:=params.IndexOfName('ErrorResourceFile');
 if index>=0 then
  params.Delete(index);
 index:=params.IndexOfName('LocaleCode');
 if index>=0 then
  params.Delete(index);
 index:=params.IndexOfName('Interbase TransIsolation');
 if index>=0 then
  params.Delete(index);
 index:=params.IndexOfName('WaitOnLocks');
 if index>=0 then
  params.Delete(index);
 index:=params.IndexOfName('SQLDialect');
 if index>=0 then
 begin
  base.SQLDialect:=StrToInt(params.Values['SQLDialect']);
  params.Delete(index);
 end;
 index:=params.IndexOfName('RoleName');
 if index>=0 then
 begin
  base.SQLRole:=params.Values['RoleName'];
  params.Delete(index);
 end;
 index:=params.IndexOfName('user_name');
 if index>=0 then
 begin
  params.Add('USER NAME='+params.Values['user_name']);
  params.Delete(index);
 end;
 index:=params.IndexOfName('ServerCharSet');
 if index>=0 then
 begin
  base.Charset:=params.Values['ServerCharSet'];
  params.Delete(index);
 end;
end;
{$ENDIF}

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
  FMyBaseFilename:=TRpDataInfoItem(Source).FMyBaseFilename;
  FMyBaseIndexFields:=TRpDataInfoItem(Source).FMyBaseIndexFields;
  FBDEIndexFields:=TRpDataInfoItem(Source).FBDEIndexFields;
  FBDEMasterFields:=TRpDataInfoItem(Source).FBDEMasterFields;
  FBDEIndexName:=TRpDataInfoItem(Source).FBDEIndexName;
  FBDEFilter:=TRpDataInfoItem(Source).FBDEFilter;
  FBDETable:=TRpDataInfoItem(Source).FBDETable;
  FBDEType:=TRpDataInfoItem(Source).FBDEType;
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
 // Then function is defined by the class TCollectionItem
 alias:=AnsiUpperCase(alias);
 if Indexof(alias)>=0 then
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
{$IFDEF USESQLEXPRESS}
 FSQLInternalConnection.free;
{$ENDIF}
{$IFDEF USEIBX}
 if Assigned(FIBInternalDatabase) then
 begin
  FIBInternalDatabase.DefaultTransaction.Free;
  FIBInternalDatabase.Free;
 end;
{$ENDIF}
{$IFDEF USEIBO}
 if Assigned(FIBODatabase) then
 begin
  FIBODatabase.Free;
 end;
{$ENDIF}
{$IFDEF USEBDE}
 if CreatedBDE then
  FBDEDatabase.free;
{$ENDIF}
{$IFDEF USEADO}
 FADOConnection.free;
{$ENDIF}
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
  FADOConnectionString:=TRpDatabaseInfoItem(Source).FADOConnectionString;
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

{$IFDEF USECONADMIN}
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
{$ENDIF}

procedure TRpDatabaseinfoitem.Connect;
var
 conname:string;
{$IFDEF USESQLEXPRESS}
 funcname,drivername,vendorlib,libraryname:string;
{$ENDIF}
{$IFDEF USEBDE}
 i:integer;
 AlreadyOpen:boolean;
 OpenedName:string;
 FOpenedDatabase:TDatabase;
 ASession:TSession;
 adparams:TStrings;
{$ENDIF}
begin
 case Fdriver of
  rpdatadbexpress:
   begin
{$IFDEF USESQLEXPRESS}
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
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverDBX);
{$ENDIF}
   end;
  rpdataibx:
   begin
{$IFDEF USEIBX}
     if Not Assigned(FIBDatabase) then
     begin
      FIBInternalDatabase:=TIBDatabase.Create(nil);
      FIBInternalDatabase.DefaultTransaction:=TIBTransaction.Create(nil);
      FIBInternalDatabase.DefaultTransaction.DefaultDatabase:=FIBInternalDatabase;
      FIBInternalDatabase.DefaultTransaction.Params.Add('concurrency');
      FIBInternalDatabase.DefaultTransaction.Params.Add('nowait');
      FIBDatabase:=FIBInternalDatabase;
     end;
     if FIBDatabase.Connected then
      exit;
     FIBDatabase.LoginPrompt:=FLoginPrompt;
     conname:=alias;
     // Load Connection parameters
     if (FLoadParams) then
     begin
      if Not Assigned(ConAdmin) then
       CreateConAdmin;
      ConAdmin.GetConnectionParams(conname,FIBDatabase.params);
     end;
     ConvertParamsFromDBXToIBX(FIBDatabase);
     FIBDatabase.Connected:=true;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverIBX);
{$ENDIF}
   end;
  rpdatamybase:
   begin
    // Nothing to do
   end;
  rpdatabde:
   begin
{$IFDEF USEBDE}
    FOpenedDatabase:=nil;
    FBDEAlias:=Alias;
    CreatedBDE:=false;
    if Not Assigned(FBDEDatabase) then
    begin
     FBDEDatabase:=TDatabase.Create(nil);
     createdBDE:=true;
     FBDEDatabase.KeepConnection:=False;
     if Assigned(TRpDatabaseInfoList(Collection).FBDESession) then
      FBDEDatabase.SessionName:=TRpDatabaseInfoList(Collection).FBDESession.SessionName;
    end;
    if FBDEDatabase.Connected then
     exit;
    ASession:=Session;
    if Assigned(TRpDatabaseInfoList(Collection).FBDESession) then
     ASession:=TRpDatabaseInfoList(Collection).FBDESession;
    // Find an opened database in this session that
    // openend the alias
    OpenedName:=FBDEAlias;
    AlreadyOpen:=False;
    for i:=0 to ASession.DatabaseCount-1 do
    begin
     if ASession.Databases[i].DatabaseName=FBDEAlias then
     begin
      if ASession.Databases[i].Connected then
      begin
       AlreadyOpen:=True;
       OpenedName:=ASession.Databases[i].DatabaseName;
       FOpenedDatabase:=ASession.Databases[i];
       break;
      end
      else
       ASession.Databases[i].DatabaseName:='';
     end;
     if ASession.Databases[i].AliasName=FBDEAlias then
      if ASession.Databases[i].Connected then
      begin
       AlreadyOpen:=True;
       FOpenedDatabase:=ASession.Databases[i];
       OpenedName:=ASession.Databases[i].DatabaseName;
       break;
      end;
    end;
    if Not AlreadyOpen then
    begin
     FBDEDatabase.DatabaseName:=OpenedName;
     if FLoadParams then
     begin
      try
       FBDEDatabase.AliasName:=FBDEAlias;
       ASession.GetAliasParams(FBDEAlias,FBDEDatabase.Params);
       // Add aditional parameters
       adparams:=TStringList.Create;
       try
        if Not Assigned(ConAdmin) then
         CreateConAdmin;
        ConAdmin.GetConnectionParams(FBDEAlias,adparams);
        AddParamsFromDBXToBDE(adparams,FBDEDatabase.Params);
       finally
        adparams.free;
       end;
       FBDEDatabase.LoginPrompt:=LoginPrompt;
       FBDEDatabase.Connected:=true;
      except
       on E:Exception do
       begin
        E.Message:=E.Message+':BDE-ALIAS:'+FBDEAlias;
        Raise;
       end;
      end;
     end;
    end
    else
    begin
     if createdBDE then
     begin
      FBDEDatabase.free;
      FBDEDatabase:=nil;
      CreatedBDE:=false;
     end;
     FBDEDatabase:=FOpenedDatabase;
    end;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverBDE);
{$ENDIF}
   end;
  rpdataado:
   begin
{$IFDEF USEADO}
    if Not Assigned(FADOConnection) then
     FADOConnection:=TADOConnection.Create(nil);
    if FADOConnection.Connected then
     exit;
    FADOConnection.Mode:=cmRead;
    FADOConnection.ConnectionString:=ADOConnectionString;
    FADOConnection.LoginPrompt:=LoginPrompt;
    FADOConnection.Connected:=true;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverADO);
{$ENDIF}
   end;
  rpdataibo:
   begin
{$IFDEF USEIBO}
     if Not Assigned(FIBODatabase) then
     begin
      FIBODatabase:=TIB_Database.Create(nil);
     end;
     if FIBODatabase.Connected then
      exit;
     FIBODatabase.LoginPrompt:=FLoginPrompt;
     conname:=alias;
     // Load Connection parameters
     if (FLoadParams) then
     begin
      if Not Assigned(ConAdmin) then
       CreateConAdmin;
      ConAdmin.GetConnectionParams(conname,FIBODatabase.params);
     end;
     ConvertParamsFromDBXToIBO(FIBODatabase);
     FIBODatabase.Connected:=true;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverIBX);
{$ENDIF}
   end;
 end;
end;



{$IFDEF USEBDE}
procedure TRpDatainfoitem.SetRangeForTable(lastrange:boolean);
var
 report:TRpReport;
 eval:TRpEvaluator;
 atable:TTable;
 alist:TStringList;
 i:integer;
begin
 report:=TRpDataInfoList(Collection).FReport As TRpReport;
 eval:=report.Evaluator;
 atable:=TTable(FSQLInternalQuery);
 alist:=TStringList.Create;
 try
  if lastrange then
  begin
   atable.SetRangeEnd;
   alist.Text:=FBDELastRange;
  end
  else
  begin
   atable.SetRangeStart;
   alist.Text:=FBDEFirstRange;
  end;
  for i:=0 to alist.count-1 do
  begin
   if Length(alist.Strings[i])>0 then
   begin
    atable.IndexFields[i].AsVariant:=eval.EvaluateText(alist.Strings[i]);
   end;
  end;
  atable.ApplyRange;
 finally
  alist.free;
 end;
end;
{$ENDIF}

procedure TRpDatabaseinfoitem.DisConnect;
begin
{$IFDEF USESQLEXPRESS}
 if Assigned(FSQLConnection) then
 begin
  if FSQLCOnnection=FSQLInternalConnection then
  begin
   FSQLConnection.Connected:=False;
  end;
 end;
{$ENDIF}
{$IFDEF USEIBX}
 if Assigned(FIBInternalDatabase) then
 begin
  FIBInternalDatabase.Connected:=False;
 end;
{$ENDIF}
{$IFDEF USEBDE}
 if (Assigned(FBDEDatabase) and createdBDE) then
  FBDEDatabase.Connected:=false;
{$ENDIF}
{$IFDEF USEADO}
 if Assigned(FADOConnection) then
  FADOConnection.Connected:=false;
{$ENDIF}
{$IFDEF USEIBO}
 if Assigned(FIBODatabase) then
 begin
  FIBODatabase.Connected:=False;
 end;
{$ENDIF}
end;

procedure TRpDataInfoItem.Connect(databaseinfo:TRpDatabaseInfoList;params:TRpParamList);
var
 index:integer;
 i:integer;
 datainfosource:TRpDatainfoItem;
 baseinfo:TRpDatabaseInfoItem;
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
   begin
    // For opened datasets they must go to first record
    // Before printing
    FDataset.First;
    if cached then
    begin
     if Not FCachedDataset.Active then
     begin
      FCachedDataset.Dataset:=FDataset;
      FCachedDataset.DoOpen;
     end;
    end;
    doexit:=true;
   end;
   if ((FDataset<>FSQLInternalQuery) and (not doexit)) then
   begin
    FDataset.Active:=true;
    if cached then
    begin
     FCachedDataset.DoClose;
     FCachedDataset.Dataset:=FDataset;
     FCachedDataset.DoOpen;
    end;
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
   baseinfo:=databaseinfo.items[index];
   baseinfo.Connect;


   if not assigned(FSQLInternalQuery) then
   begin
    case baseinfo.FDriver of
     rpdatadbexpress:
      begin
{$IFDEF USESQLEXPRESS}
       FSQLInternalQuery:=TSQLQuery.Create(nil);
{$ELSE}
       Raise Exception.Create(SRpDriverNotSupported+SrpDriverDBX);
{$ENDIF}
      end;
     rpdataibx:
      begin
{$IFDEF USEIBX}
       FSQLInternalQuery:=TIBQuery.Create(nil);
{$ELSE}
       Raise Exception.Create(SRpDriverNotSupported+SrpDriverIBX);
{$ENDIF}
      end;
     rpdatamybase:
      begin
       FSQLInternalQuery:=TClientDataset.Create(nil);
      end;
     rpdatabde:
      begin
{$IFDEF USEBDE}
       if FBDEType=rpdquery then
       begin
        FSQLInternalQuery:=TQuery.Create(nil);
        if Assigned(databaseinfo.FBDESession) then
         TQuery(FSQLInternalQuery).SessionName:=databaseinfo.FBDESession.SessionName;
       end
       else
       begin
        FSQLInternalQuery:=TTable.Create(nil);
        if Assigned(databaseinfo.FBDESession) then
         TTable(FSQLInternalQuery).SessionName:=databaseinfo.FBDESession.SessionName;
       end;
{$ELSE}
       Raise Exception.Create(SRpDriverNotSupported+SrpDriverBDE);
{$ENDIF}
      end;
     rpdataado:
      begin
{$IFDEF USEADO}
       FSQLInternalQuery:=TADOQuery.Create(nil);
{$ELSE}
       Raise Exception.Create(SRpDriverNotSupported+SrpDriverADO);
{$ENDIF}
      end;
     rpdataibo:
      begin
{$IFDEF USEIBO}
       FSQLInternalQuery:=TIBOQuery.Create(nil);
{$ELSE}
       Raise Exception.Create(SRpDriverNotSupported+SrpDriverIBX);
{$ENDIF}
      end;
    end;
   end
   else
   begin
    case baseinfo.FDriver of
     rpdatadbexpress:
      begin
{$IFDEF USESQLEXPRESS}
       if Not (FSQLInternalQuery is TSQLQuery) then
       begin
        FSQLInternalQuery.Free;
        FSQLInternalQuery:=nil;
        FSQLInternalQuery:=TSQLQuery.Create(nil);
       end;
{$ENDIF}
      end;
     rpdataibx:
      begin
{$IFDEF USEIBX}
       if Not (FSQLInternalQuery is TIBQuery) then
       begin
        FSQLInternalQuery.Free;
        FSQLInternalQuery:=nil;
        FSQLInternalQuery:=TIBQuery.Create(nil);
       end;
{$ENDIF}
      end;
     rpdatabde:
      begin
{$IFDEF USEBDE}
       if FBDEType=rpdquery then
       begin
        if Not (FSQLInternalQuery is TQuery) then
        begin
         FSQLInternalQuery.Free;
         FSQLInternalQuery:=nil;
         FSQLInternalQuery:=TQuery.Create(nil);
         if Assigned(databaseinfo.FBDESession) then
          TQuery(FSQLInternalQuery).SessionName:=databaseinfo.FBDESession.SessionName;
        end;
       end
       else
       begin
        if Not (FSQLInternalQuery is TTable) then
        begin
         FSQLInternalQuery.Free;
         FSQLInternalQuery:=nil;
         FSQLInternalQuery:=TTable.Create(nil);
         if Assigned(databaseinfo.FBDESession) then
          TTable(FSQLInternalQuery).SessionName:=databaseinfo.FBDESession.SessionName;
        end;
       end;
{$ENDIF}
      end;
     rpdataado:
      begin
{$IFDEF USEADO}
       if Not (FSQLInternalQuery is TADOQuery) then
       begin
        FSQLInternalQuery.Free;
        FSQLInternalQuery:=nil;
        FSQLInternalQuery:=TADOQuery.Create(nil);
       end;
{$ENDIF}
      end;
     rpdataibo:
      begin
{$IFDEF USEIBO}
       if Not (FSQLInternalQuery is TIBOQuery) then
       begin
        FSQLInternalQuery.Free;
        FSQLInternalQuery:=nil;
        FSQLInternalQuery:=TIBOQuery.Create(nil);
       end;
{$ENDIF}
      end;
    end;
   end;

   FDataset:=FSQLInternalQuery;

   // Assigns the connectoin
   case baseinfo.Driver of
    rpdatadbexpress:
     begin
{$IFDEF USESQLEXPRESS}
      TSQLQuery(FSQLInternalQuery).SQLConnection:=
       baseinfo.SQLConnection;
      TSQLQuery(FSQLInternalQuery).SQL.Text:=SQL;
{$ENDIF}
     end;
    rpdataibx:
     begin
{$IFDEF USEIBX}
      TIBQuery(FSQLInternalQuery).Database:=
       baseinfo.FIBDatabase;
      TIBQuery(FSQLInternalQuery).SQL.Text:=SQL;
      if Assigned(TIBQuery(FSQLInternalQuery).Database) then
      begin
       TIBQuery(FSQLInternalQuery).Transaction:=
        TIBQuery(FSQLInternalQuery).Database.DefaultTransaction;
      end;
      TIBQuery(FSQLInternalQuery).UniDirectional:=true;
{$ENDIF}
     end;
    rpdatamybase:
     begin
      try
       TClientDataSet(FSQLInternalQuery).IndexFieldNames:=FMyBaseIndexFields;
       TClientDataSet(FSQLInternalQuery).LoadFromFile(FMyBaseFilename);
      except
       FDataset:=nil;
       FSQLInternalQuery.free;
       FSQLInternalQuery:=nil;
       raise;
      end;
     end;
    rpdatabde:
     begin
{$IFDEF USEBDE}
      if FBDEType=rpdquery then
      begin
       TQuery(FSQLInternalQuery).DatabaseName:=baseinfo.FBDEDatabase.DatabaseName;
      end
      else
      begin
       TTable(FSQLInternalQuery).DatabaseName:=baseinfo.FBDEDatabase.DatabaseName;
      end;
      TBDEDataset(FSQLInternalQuery).Filter:=FBDEFilter;
      if length(Trim(FBDEFilter))>0 then
       TBDEDataset(FSQLInternalQuery).Filtered:=True;
      if FBDEType=rpdquery then
      begin
       TQuery(FSQLInternalQuery).SQL.Text:=SQL;
       TQuery(FSQLInternalQUery).UniDirectional:=True;
      end
      else
      begin
       TTable(FSQLInternalQuery).Tablename:=FBDETable;
       TTable(FSQLInternalQUery).IndexFieldNames:='';
       TTable(FSQLInternalQUery).IndexName:='';
       if Length(FBDEIndexFields)>0 then
        TTable(FSQLInternalQUery).IndexFieldNames:=FBDEIndexFields;
       if Length(FBDEIndexName)>0 then
        TTable(FSQLInternalQUery).IndexName:=FBDEIndexName;
       // Set the range start and range end
       if Length(Trim(FBDEFirstRange))>0 then
       begin
        SetRangeForTable(false);
       end;
       if Length(Trim(FBDELastRange))>0 then
       begin
        SetRangeForTable(true);
       end;
      end;
{$ENDIF}
     end;
    rpdataado:
     begin
{$IFDEF USEADO}
      TADOQuery(FSQLInternalQuery).Connection:=baseinfo.FADOConnection;
      TADOQuery(FSQLInternalQuery).SQL.Text:=SQL;
      TADOQuery(FSQLInternalQuery).CursorType:=ctOpenForwardOnly;
//      Activating this switches break linked querys
//      TADOQuery(FSQLInternalQuery).CursorLocation:=clUseServer;
{$ENDIF}
     end;
    rpdataibo:
     begin
{$IFDEF USEIBO}
      TIBOQuery(FSQLInternalQuery).IB_Connection:=baseinfo.FIBODatabase;
      TIBOQuery(FSQLInternalQuery).SQL.Text:=SQL;
      TIBOQuery(FSQLInternalQuery).UniDirectional:=true;
{$ENDIF}
     end;
   end;
   // Use the datasource
   if Assigned(datainfosource) then
   begin
    case baseinfo.Driver of
     rpdatadbexpress:
      begin
{$IFDEF USESQLEXPRESS}
       if Not Assigned(TSQLQuery(FSQLInternalQuery).DataSource) then
        TSQLQuery(FSQLInternalQuery).DataSource:=TDataSource.Create(nil);
       if datainfosource.cached then
        TSQLQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.CachedDataset
       else
        TSQLQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.Dataset;
{$ENDIF}
      end;
     rpdataibx:
      begin
{$IFDEF USEIBX}
       if Not Assigned(TIBQuery(FSQLInternalQuery).DataSource) then
        TIBQuery(FSQLInternalQuery).DataSource:=TDataSource.Create(nil);
       if datainfosource.cached then
        TIBQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.CachedDataset
       else
        TIBQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.Dataset;
{$ENDIF}
      end;
     rpdatabde:
      begin
{$IFDEF USEBDE}
       if FBDEType=rpdquery then
       begin
        if Not Assigned(TQuery(FSQLInternalQuery).DataSource) then
         TQuery(FSQLInternalQuery).DataSource:=TDataSource.Create(nil);
        if datainfosource.cached then
         TQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.CachedDataset
        else
         TQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.Dataset;
       end
       else
       begin
        TTable(FSQLInternalQuery).MasterFields:=BDEMasterFields;
        if Not Assigned(TTable(FSQLInternalQuery).DataSource) then
         TTable(FSQLInternalQuery).MasterSource:=TDataSource.Create(nil);
        if datainfosource.cached then
         TTable(FSQLInternalQuery).DataSource.DataSet:=datainfosource.CachedDataset
        else
         TTable(FSQLInternalQuery).DataSource.DataSet:=datainfosource.Dataset;
       end;
{$ENDIF}
      end;
     rpdataado:
      begin
{$IFDEF USEADO}
       if Not Assigned(TADOQuery(FSQLInternalQuery).DataSource) then
        TADOQuery(FSQLInternalQuery).DataSource:=TDataSource.Create(nil);
       if datainfosource.cached then
        TADOQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.CachedDataset
       else
        TADOQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.Dataset;
{$ENDIF}
      end;
     rpdataibo:
      begin
{$IFDEF USEIBO}
       if Not Assigned(TIBOQuery(FSQLInternalQuery).DataSource) then
        TIBOQuery(FSQLInternalQuery).DataSource:=TDataSource.Create(nil);
       if datainfosource.cached then
        TIBOQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.CachedDataset
       else
        TIBOQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.Dataset;
{$ENDIF}
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
     case baseinfo.Driver of
      rpdatadbexpress:
       begin
{$IFDEF USESQLEXPRESS}
        TSQLQuery(FSQLInternalQuery).ParamByName(param.Name).DataType:=
         ParamTypeToDataType(param.ParamType);
        TSQLQuery(FSQLInternalQuery).ParamByName(param.Name).Value:=param.Value;
{$ENDIF}
       end;
      rpdataibx:
       begin
{$IFDEF USEIBX}
        TIBQuery(FSQLInternalQuery).ParamByName(param.Name).DataType:=
         ParamTypeToDataType(param.ParamType);
        TIBQuery(FSQLInternalQuery).ParamByName(param.Name).Value:=param.Value;
{$ENDIF}
       end;
      rpdatabde:
       begin
{$IFDEF USEBDE}
        if FBDEType=rpdquery then
        begin
         TQuery(FSQLInternalQuery).ParamByName(param.Name).DataType:=
          ParamTypeToDataType(param.ParamType);
         TQuery(FSQLInternalQuery).ParamByName(param.Name).Value:=param.Value;
        end
        else
         Raise Exception.Create(SrpParamBDENotSupported);
{$ENDIF}
       end;
      rpdataado:
       begin
{$IFDEF USEADO}
        TADOQuery(FSQLInternalQuery).Parameters.ParamByName(param.Name).DataType:=
         ParamTypeToDataType(param.ParamType);
        TADOQuery(FSQLInternalQuery).Parameters.ParamByName(param.Name).Value:=param.Value;
{$ENDIF}
       end;
      rpdataibo:
       begin
{$IFDEF USEIBO}
        TIBOQuery(FSQLInternalQuery).ParamByName(param.Name).AsVariant:=param.Value;
{$ENDIF}
       end;
     end;
    end;
   end;
   if Not Assigned(FSQLInternalQuery) then
    Raise Exception.Create(SRpDriverNotSupported);
   FSQLInternalQuery.Active:=true;
   if cached then
   begin
    FCachedDataset.DoClose;
    FCachedDataset.Dataset:=FDataset;
    FCachedDataset.DoOpen;
   end;
  end;
 finally
  connecting:=false;
 end;
end;

procedure TRpDataInfoItem.DisConnect;
begin
 if Assigned(FDataset) then
 begin
  if FDataset=FSQLInternalQuery then
   FDataset.Active:=false;
  if Assigned(FCachedDataset) then
   FCachedDataset.DoClose;
 end;
end;

constructor TRpDatainfoitem.Create(Collection:TCollection);
begin
 inherited Create(Collection);

 FBDEType:=rpdquery;
 FCachedDataset:=TRpDataset.Create(nil);
end;

destructor TRpDataInfoItem.Destroy;
begin
 try
  FCachedDataset.free;
  FCachedDataset:=nil;
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

{$IFDEF USECONADMIN}
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


function GetRegistryFile(Setting, Default: string; DesignMode: Boolean): string;
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
{$ENDIF}
//{$IFDEF LINUX}
//  GlobalFile: string;
//{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(SDBEXPRESSREG_SETTING) then
      Result := Reg.ReadString(Setting)
    else
    begin
     Reg.RootKey := HKEY_CURRENT_USER;
     if Reg.OpenKeyReadOnly(SDBEXPRESSREG_SETTING) then
      Result := Reg.ReadString(Setting)
    end;
  finally
    Reg.Free;
  end;
  if Result = '' then
    Result := ExtractFileDir(ParamStr(0)) + '\' + Default;
  {$ENDIF}
//  {$IFDEF LINUX}
//  Result := getenv('HOME') + SDBEXPRESSREG_USERPATH + Default;    { do not localize }
//  if not FileExists(Result) then
//  begin
//    GlobalFile := SDBEXPRESSREG_GLOBALPATH + Default + SConfExtension;
//    if FileExists(GlobalFile) then
//    begin
//      if DesignMode then
//      begin
//        if not CopyConfFile(GlobalFile, Result) then
//          DatabaseErrorFmt(SConfFileMoveError, [GlobalFile, Result])
//      end else
//        Result := GlobalFile;
//    end else
//      DatabaseErrorFmt(SMissingConfFile, [GlobalFile]);
//  end;
//  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
function ObtainDriverRegistryFile(DesignMode: Boolean = False): string;
begin
  Result := GetRegistryFile(SDRIVERREG_SETTING, sDriverConfigFile, DesignMode);
end;

function ObtainConnectionRegistryFile(DesignMode: Boolean = False): string;
begin
  Result := GetRegistryFile(SCONNECTIONREG_SETTING, sConnectionConfigFile, DesignMode);
end;
{$ENDIF}



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
 begin
  if not CreateDir(configdir) then
   Raise Exception.Create(SRpDirCantBeCreated+'-'+configdir);
 end;
 driverfilename:=configdir+'/'+DBXDRIVERFILENAME;
 configfilename:=configdir+'/'+DBXCONFIGFILENAME;
{$ENDIF}
{$IFDEF MSWINDOWS}
 // Looks the registry and if there is not registry
 // Use current dir
 driverfilename:=ObtainDriverRegistryFile;
 configfilename:=ObtainConnectionRegistryFile;
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
  begin
   // Check int /usr/local/etc
   if FileExists('/usr/local/etc/'+DBXDRIVERFILENAME+'.conf') then
   begin
    CopyFileTo('/usr/local/etc/'+DBXDRIVERFILENAME+'.conf',driverfilename);
    drivers:=TMemIniFile.Create(driverfilename);
   end
   else
    Raise Exception.Create(SRpConfigFileNotExists+DBXDRIVERFILENAME);
  end;
{$ENDIF}
 end;
 if FileExists(configfilename) then
 begin
  config:=TMemInifile.Create(configfilename);
{$IFDEF USEVARIANTS}
  config.CaseSensitive:=false;
{$ENDIF}
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
   config.CaseSensitive:=false;
   CopyFileTo(DBXCONFIGFILENAME,configfilename);
  end
  else
  begin
   // Check int /usr/local/etc
   if FileExists('/usr/local/etc/'+DBXCONFIGFILENAME+'.conf') then
   begin
    CopyFileTo('/usr/local/etc/'+DBXCONFIGFILENAME+'.conf',configfilename);
    config:=TMemIniFile.Create(configfilename);
    config.CaseSensitive:=false;
   end
   else
    Raise Exception.Create(SRpConfigFileNotExists+DBXCONFIGFILENAME);
  end
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
{$ENDIF}

function TRpDatabaseInfoItem.GetStreamFromSQL(sqlsentence:String;bsmode:TBlobStreamMode;params:TStringList):TStream;
var
 data:TDataset;
begin
 Result:=nil;
 data:=OpenDatasetFromSQL(sqlsentence,params,false);
 try
  if data.Eof then
   Raise Exception.Create(SRpExternalSectionNotFound);
  if data.FieldCount<1 then
   Raise Exception.Create(SRpExternalSectionNotFound);
  Result:=data.CreateBlobStream(data.fields[0],bsmode);
 finally
  data.free;
 end;
end;

procedure TRpDatabaseInfoItem.GetTableNames(Alist:TStrings);
begin
 Connect;

 case Driver of
  rpdatadbexpress:
   begin
{$IFDEF USESQLEXPRESS}
    SQLConnection.GetTableNames(alist);
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverDBX);
{$ENDIF}
   end;
  rpdataibx:
   begin
{$IFDEF USEIBX}
    FIBDatabase.GetTableNames(alist);
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverIBX);
{$ENDIF}
   end;
  rpdatamybase:
   begin
    alist.Clear;
   end;
  rpdatabde:
   begin
{$IFDEF USEBDE}
    FBDEDatabase.GetTableNames(alist);
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverBDE);
{$ENDIF}
   end;
  rpdataado:
   begin
{$IFDEF USEADO}
    FADOConnection.GetTableNames(alist);
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverADO);
{$ENDIF}
   end;
  rpdataibo:
   begin
{$IFDEF USEIBO}
    alist.clear;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverIBX);
{$ENDIF}
   end;
 end;
end;

function TRpDatabaseInfoItem.OpenDatasetFromSQL(sqlsentence:String;params:TStringList;onlyexec:Boolean):TDataset;
var
 FSQLInternalQuery:TDataset;
 i:integer;
 astream:TStream;
 param:TRpParamObject;
 paramname:String;
 avariant:Variant;
begin
 Result:=nil;
 FSQLInternalQuery:=nil;
 // Connects and opens the dataset
 Connect;
 case Driver of
  rpdatadbexpress:
   begin
{$IFDEF USESQLEXPRESS}
    FSQLInternalQuery:=TSQLQuery.Create(nil);
    TSQLQuery(FSQLInternalQuery).SQLConnection:=SQLConnection;
    TSQLQuery(FSQLInternalQuery).SQL.Text:=SQLsentence;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverDBX);
{$ENDIF}
   end;
  rpdataibx:
   begin
{$IFDEF USEIBX}
    FSQLInternalQuery:=TIBQuery.Create(nil);
    TIBQuery(FSQLInternalQuery).Database:=FIBDatabase;
    TIBQuery(FSQLInternalQuery).SQL.Text:=SQLsentence;
    if Assigned(TIBQuery(FSQLInternalQuery).Database) then
    begin
     TIBQuery(FSQLInternalQuery).Transaction:=
      TIBQuery(FSQLInternalQuery).Database.DefaultTransaction;
    end;
    TIBQuery(FSQLInternalQuery).UniDirectional:=true;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverIBX);
{$ENDIF}
   end;
  rpdatamybase:
   begin
    Raise Exception.Create(SRpDriverNotSupported);
   end;
  rpdatabde:
   begin
{$IFDEF USEBDE}
    FSQLInternalQuery:=TQuery.Create(nil);
    if Assigned(TRpDatabaseInfoList(Collection).FBDESession) then
     TQuery(FSQLInternalQuery).SessionName:=TRpDatabaseInfoList(Collection).FBDESession.SessionName;
    TQuery(FSQLInternalQuery).DatabaseName:=FBDEDatabase.DatabaseName;
    TQuery(FSQLInternalQuery).SQL.Text:=SQLsentence;
    TQuery(FSQLInternalQUery).UniDirectional:=True;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverBDE);
{$ENDIF}
   end;
  rpdataado:
   begin
{$IFDEF USEADO}
    FSQLInternalQuery:=TADOQuery.Create(nil);
    TADOQuery(FSQLInternalQuery).Connection:=FADOConnection;
    TADOQuery(FSQLInternalQuery).SQL.Text:=SQLsentence;
    TADOQuery(FSQLInternalQuery).CursorType:=ctOpenForwardOnly;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverADO);
{$ENDIF}
   end;
  rpdataibo:
   begin
{$IFDEF USEIBO}
    FSQLInternalQuery:=TIBOQuery.Create(nil);
    TIBOQuery(FSQLInternalQuery).IB_Connection:=FIBODatabase;
    TIBOQuery(FSQLInternalQuery).SQL.Text:=SQLsentence;
    TIBOQuery(FSQLInternalQuery).UniDirectional:=true;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+SrpDriverIBX);
{$ENDIF}
   end;
 end;
 // Assigns parameters
 if assigned(params) then
 begin
  for i:=0 to params.count-1 do
  begin
   param:=TRpParamObject(params.Objects[i]);
   if not assigned(param) then
    continue;
   paramname:=Trim(params.Strings[i]);
   if Length(paramname)<1 then
    continue;
   astream:=nil;
   avariant:=Null;
   if Assigned(param.stream) then
    astream:=param.stream
   else
    avariant:=param.Value;
   case Driver of
    rpdatadbexpress:
     begin
{$IFDEF USESQLEXPRESS}
      if assigned(astream) then
      begin
       TSQLQuery(FSQLInternalQuery).ParamByName(paramName).DataType:=ftBlob;
       TSQLQuery(FSQLInternalQuery).ParamByName(paramName).LoadFromStream(astream,ftBlob);
      end
      else
      begin
       TSQLQuery(FSQLInternalQuery).ParamByName(paramName).DataType:=
          VariantTypeToDataType(avariant);
       TSQLQuery(FSQLInternalQuery).ParamByName(paramName).Value:=avariant;
      end;
{$ENDIF}
     end;
    rpdataibx:
     begin
{$IFDEF USEIBX}
      if assigned(astream) then
      begin
       TIBQuery(FSQLInternalQuery).ParamByName(paramName).DataType:=ftBlob;
       TIBQuery(FSQLInternalQuery).ParamByName(paramName).LoadFromStream(astream,ftBlob);
      end
      else
      begin
       TIBQuery(FSQLInternalQuery).ParamByName(paramName).DataType:=
        VariantTypeToDataType(avariant);
       TIBQuery(FSQLInternalQuery).ParamByName(paramName).Value:=avariant;
      end;
{$ENDIF}
     end;
    rpdatabde:
     begin
{$IFDEF USEBDE}
      if assigned(astream) then
      begin
       TQuery(FSQLInternalQuery).ParamByName(paramName).DataType:=ftBlob;
       TQuery(FSQLInternalQuery).ParamByName(paramName).LoadFromStream(astream,ftBlob);
      end
      else
      begin
       TQuery(FSQLInternalQuery).ParamByName(paramName).DataType:=
        VariantTypeToDataType(avariant);
       TQuery(FSQLInternalQuery).ParamByName(paramName).Value:=avariant;
      end;
{$ENDIF}
     end;
    rpdataado:
     begin
{$IFDEF USEADO}
      if assigned(astream) then
      begin
       TADOQuery(FSQLInternalQuery).Parameters.ParamByName(paramName).DataType:=ftBlob;
       TADOQuery(FSQLInternalQuery).Parameters.ParamByName(paramName).LoadFromStream(astream,ftBlob);
      end
      else
      begin
       TADOQuery(FSQLInternalQuery).Parameters.ParamByName(paramName).DataType:=
        VariantTypeToDataType(avariant);
       TADOQuery(FSQLInternalQuery).Parameters.ParamByName(paramName).Value:=avariant;
      end;
{$ENDIF}
     end;
    rpdataibo:
     begin
{$IFDEF USEIBO}
      if assigned(astream) then
      begin
       TIBOQuery(FSQLInternalQuery).ParamByName(paramName).Assign(astream);
      end
      else
      begin
       TIBOQuery(FSQLInternalQuery).ParamByName(paramName).AsVariant:=avariant;
      end;
{$ENDIF}
     end;
   end;
  end;
 end;
 if onlyexec then
 begin
  // Executes
  case Driver of
   rpdatadbexpress:
    begin
 {$IFDEF USESQLEXPRESS}
     TSQLQuery(FSQLInternalQuery).ExecSQL;
 {$ENDIF}
    end;
   rpdataibx:
    begin
 {$IFDEF USEIBX}
     TIBQuery(FSQLInternalQuery).ExecSQL;
 {$ENDIF}
    end;
   rpdatamybase:
    begin
     Raise Exception.Create(SRpDriverNotSupported);
    end;
   rpdatabde:
    begin
 {$IFDEF USEBDE}
     TQuery(FSQLInternalQuery).ExecSQL;
 {$ENDIF}
    end;
   rpdataado:
    begin
 {$IFDEF USEADO}
     TADOQuery(FSQLInternalQuery).ExecSQL;
 {$ENDIF}
    end;
   rpdataibo:
    begin
 {$IFDEF USEIBO}
     TIBOQuery(FSQLInternalQuery).ExecSQL;
 {$ENDIF}
    end;
  end;
 end
 else
  FSQLInternalQuery.Active:=True;

 if onlyexec then
  FSQLInternalQuery.Free
 else
  Result:=FSQLInternalQuery;
end;

initialization

{$IFDEF USECONADMIN}
ConAdmin:=nil;
{$ENDIF}

finalization

{$IFDEF USECONADMIN}
if Assigned(ConAdmin) then
begin
 ConAdmin.free;
 ConAdmin:=nil;
end;
{$ENDIF}


end.



