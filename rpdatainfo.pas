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
{$IFDEF USEZEOS}
 ZDbcIntfs,ZAbstractRODataset, ZDataset, ZConnection,
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
 rpdataset,rpdatatext;

{$IFDEF LINUX}
const
 DBXDRIVERFILENAME='dbxdrivers';
 DBXCONFIGFILENAME='dbxconnections';
{$ENDIF}
type
 TRpDbDriver=(rpdatadbexpress,rpdatamybase,rpdataibx,
  rpdatabde,rpdataado,rpdataibo,rpdatazeos,rpdatadriver);


 TRpConnAdmin=class(TObject)
  public
   driverfilename:string;
   configfilename:string;
   config:TMemInifile;
   drivers:TMemInifile;
   constructor Create;
   destructor destroy;override;
   procedure LoadConfig;
   procedure GetDriverNames(alist:TStrings);
   procedure GetConnectionParams(conname:string;params:TStrings);
   procedure GetDriverLibNames(const drivername:string;var LibraryName,VendorLib:string);
   procedure GetConnectionNames(alist:TStrings;drivername:String);
   procedure AddConnection(newname:string;drivername:string);
   procedure DeleteConnection(conname:string);
 end;

 IRpDatabaseDriver=interface
  ['{B3BA37D5-5401-4B9E-8804-698C214F8B0C}']
  procedure Connect;stdcall;
  procedure AssignParams(params:TStrings);stdcall;
  procedure GetParams(params:TStrings);stdcall;
 end;

 IRpDataDriver=interface
  ['{5094336F-C953-4108-94E3-1EC0E3D3D94C}']
  function Open:TDataset;stdcall;
  procedure Close;stdcall;
  procedure SetDatabase(IDatabase:IRpDatabaseDriver);stdcall;
  procedure AssignParams(params:TStrings);stdcall;
  procedure GetParams(params:TStrings);stdcall;
 end;

 TRpDatabaseInfoItem=class(TCollectionItem)
  private
   FAlias:string;
   FMyBasePath:String;
{$IFDEF USESQLEXPRESS}
   FSQLConnection:TSQLConnection;
   FSQLInternalConnection:TSQLConnection;
{$ENDIF}
   FConfigFile:string;
   FLoadParams:boolean;
   FReportTable,FReportGroupsTable,FReportSearchField,FReportField:String;
   FLoadDriverParams:boolean;
   FLoginPrompt:boolean;
   FADOConnectionString:widestring;
{$IFDEF USEIBX}
   FIBInternalDatabase:TIBDatabase;
   FIBDatabase:TIBDatabase;
{$ENDIF}
{$IFDEF USEZEOS}
   FZInternalDatabase:TZConnection;
   FZConnection:TZConnection;
{$ENDIF}
{$IFDEF USEADO}
   FADOConnection:TADOConnection;
   FProvidedADOConnection:TADOConnection;
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
{$IFDEF USEADO}
   procedure SetADOConnection(Value:TADOConnection);
   function GetADOConnection:TADOConnection;
{$ENDIF}
   procedure ReadAdoConnectionString(Reader:TReader);
   procedure WriteAdoConnectionString(Writer:TWriter);
  protected
    procedure DefineProperties(Filer:TFiler);override;
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
{$IFDEF USEZEOS}
   property ZConnection:TZConnection read FZConnection
    write FZConnection;
{$ENDIF}
   function GetStreamFromSQL(sqlsentence:String;params:TStringList):TStream;
   procedure GetTableNames(Alist:TStrings);
   function OpenDatasetFromSQL(sqlsentence:String;params:TStringList;onlyexec:Boolean):TDataset;
   procedure CreateLibrary(reporttable,reportfield,reportsearchfield,groupstable:String);
{$IFDEF USEADO}
   property ADOConnection:TADOConnection read GetADOConnection write SetADOConnection;
{$ENDIF}
   property MyBasePath:String read FMyBasePath;
   property ADOConnectionString:widestring read FADOConnectionString write FADOConnectionString;
  published
   property Alias:string read FAlias write SetAlias;
   property ConfigFile:string read FConfigFile write SetConfigFile;
   property LoadParams:boolean read FLoadParams write SetLoadParams;
   property LoadDriverParams:boolean read FLoadDriverParams write SetLoadDriverParams;
   property LoginPrompt:boolean read FLoginPrompt write SetLoginPrompt;
   property Driver:TRpDbDriver read FDriver write FDriver default rpdatadbexpress;
   property ReportTable:String read FReportTable write FReportTable;
   property ReportSearchField:String read FReportSearchField write FReportSearchField;
   property ReportField:String read FReportField write FReportField;
   property ReportGroupsTable:String read FReportGroupsTable write FReportGroupsTable;
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
   procedure SaveToFile(ainifile:String);
   procedure LoadFromFile(ainifile:String);
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
   FMyBaseFields:String;
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
   FMasterSource:TDataSource;
   FDataUnions:TStrings;
   FGroupUnion:Boolean;
   procedure SetDataUnions(Value:TStrings);
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
   property MyBaseFields:string read FMyBaseFields write FMyBaseFields;
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
   property DataUnions:TStrings read FDataUnions write SetDataUnions;
   property GroupUnion:Boolean read FGroupUnion write FGroupUnion default false;
  end;

 TRpDataInfoList=class(TCollection)
  private
   FReport:TComponent;
   function GetItem(Index:Integer):TRpDataInfoItem;
   procedure SetItem(index:integer;Value:TRpDataInfoItem);
   procedure IntEnableLink(alist:TStringList;i:integer);
   procedure IntDisableLink(alist:TStringList;i:integer);
  public
   procedure DisableLinks;
   procedure EnableLinks;
   function Add(alias:string):TRpDataInfoItem;
   function IndexOf(Value:string):integer;
   property Items[index:integer]:TRpDataInfoItem read GetItem write SetItem;default;
   constructor Create(rep:TComponent);
  end;

procedure UpdateConAdmin;
procedure GetRpDatabaseDrivers(alist:TStrings);
procedure CombineAddDataset(client:TClientDataset;data:TDataset;group:boolean);

var
 ConAdmin:TRpConnAdmin;

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

procedure TRpDataInfoItem.SetDataUnions(Value:TStrings);
begin
 FDataUnions.Assign(Value);
 Changed(False);
end;


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
  FMyBaseFields:=TRpDataInfoItem(Source).FMyBaseFields;
  FMyBaseIndexFields:=TRpDataInfoItem(Source).FMyBaseIndexFields;
  FBDEIndexFields:=TRpDataInfoItem(Source).FBDEIndexFields;
  FBDEMasterFields:=TRpDataInfoItem(Source).FBDEMasterFields;
  FBDEIndexName:=TRpDataInfoItem(Source).FBDEIndexName;
  FBDEFilter:=TRpDataInfoItem(Source).FBDEFilter;
  FBDETable:=TRpDataInfoItem(Source).FBDETable;
  FBDEType:=TRpDataInfoItem(Source).FBDEType;
  FDataUnions.Assign(TRpDataInfoItem(Source).FDataUnions);
  FGroupUnion:=TRpDataInfoItem(Source).FGroupUnion;
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
{$IFDEF USEZEOS}
 if Assigned(FZInternalDatabase) then
 begin
  FZInternalDatabase.Free;
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
  FReportTable:=TRpDatabaseInfoItem(Source).FReportTable;
  FReportField:=TRpDatabaseInfoItem(Source).FReportField;
  FReportSearchField:=TRpDatabaseInfoItem(Source).FReportSearchField;
  FReportGroupsTable:=TRpDatabaseInfoItem(Source).FReportGroupsTable;
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
 if Indexof(alias)>=0 then
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
 FReportTable:='REPMAN_REPORTS';
 FReportGroupsTable:='REPMAN_GROUPS';
 FReportSearchField:='REPORT_NAME';
 FReportField:='REPORT';
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


{$IFDEF USEADO}
procedure TRpDatabaseinfoitem.SetADOConnection(Value:TADOConnection);
begin
 FProvidedADOConnection:=Value;
end;

function TRpDatabaseinfoitem.GetADOConnection:TADOConnection;
begin
 if Assigned(FProvidedADOConnection) then
  Result:=FProvidedADOConnection
 else
 begin
  if Not Assigned(FADOConnection) then
  begin
    FADOConnection:=TADOConnection.Create(nil);
    FADOConnection.KeepConnection:=false;
    FADOConnection.Mode:=cmRead;
  end;
  Result:=FADOConnection;
 end;
end;

{$ENDIF}

procedure TRpDatabaseinfoitem.Connect;
var
 conname:string;
 index:integer;
 alist:TStringList;
{$IFDEF USESQLEXPRESS}
 funcname,drivername,vendorlib,libraryname:string;
{$ENDIF}
{$IFDEF USEZEOS}
  transiso:String;
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
       Raise Exception.Create(SRpNoDriverName+' - '+conname);


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
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverDBX);
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
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverIBX);
{$ENDIF}
   end;
  rpdatazeos:
   begin
{$IFDEF USEZEOS}
     if Not Assigned(FZConnection) then
     begin
      FZInternalDatabase:=TZConnection.Create(nil);
      FZConnection:=FZInternalDatabase;
     end;
     if FZConnection.Connected then
      exit;
     FZConnection.LoginPrompt:=FLoginPrompt;
     conname:=alias;
     // Load Connection parameters
     if (FLoadParams) then
     begin
      if Not Assigned(ConAdmin) then
       CreateConAdmin;
      alist:=TStringList.Create;
      try
       ConAdmin.GetConnectionParams(conname,alist);
       FZConnection.User:=alist.Values['User_Name'];
       FZConnection.Password:=alist.Values['Password'];
       if length(alist.Values['Port'])>0 then
        FZConnection.Port:=StrToInt(alist.Values['Port']);
       FZConnection.HostName:=alist.Values['HostName'];
       FZConnection.Database:=alist.Values['Database'];
       FZConnection.Protocol:=alist.Values['Database Protocol'];
       transiso:=alist.Values['Zeos TransIsolation'];
       if (transiso='ReadCommited') then
        FZConnection.TransactIsolationLevel:=ZDbcIntfs.tiReadCommitted
       else
       if (transiso='ReadUnCommited') then
        FZConnection.TransactIsolationLevel:=ZDbcIntfs.tiReadUnCommitted
       else
       if (transiso='RepeatableRead') then
        FZConnection.TransactIsolationLevel:=ZDbcIntfs.tiRepeatableRead
       else
       if (transiso='Serializable') then
        FZConnection.TransactIsolationLevel:=ZDbcIntfs.tiSerializable
       else
        FZConnection.TransactIsolationLevel:=ZDbcIntfs.tiNone;
      finally
       alist.free;
      end;
     end;
     FZConnection.Connected:=true;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverZEOS);
{$ENDIF}
   end;
  rpdatamybase:
   begin
    // Connect will read the mybasepath variable
    // or will left blank if not found
    FMyBasePath:='';
    conname:=alias;
    // Load Connection parameters
    if (FLoadParams) then
    begin
     if Not Assigned(ConAdmin) then
      CreateConAdmin;
     alist:=TStringList.Create;
     try
      ConAdmin.GetConnectionParams(conname,alist);
      index:=alist.IndexOfName('Database');
      if index>=0 then
       FMyBasePath:=alist.Values['Database'];
     finally
      alist.free;
     end;
    end;
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
     if ((UpperCase(ASession.Databases[i].DatabaseName)=UpperCase(FBDEAlias)) or
      (UpperCase(ASession.Databases[i].AliasName)=UpperCase(FBDEAlias))) then
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
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverBDE);
{$ENDIF}
   end;
  rpdataado:
   begin
{$IFDEF USEADO}
    if ADOConnection.Connected then
     exit;
    if Not Assigned(FProvidedADOCOnnection) then
    begin
     ADOConnection.Mode:=cmRead;
     ADOConnection.ConnectionString:=ADOConnectionString;
     ADOConnection.LoginPrompt:=LoginPrompt;
     ADOConnection.Connected:=true;
    end
    else
     FProvidedADOConnection.Connected:=True;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverADO);
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
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverIBO);
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
{$IFDEF USEZEOS}
 if Assigned(FZInternalDatabase) then
 begin
  FZInternalDatabase.Connected:=False;
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
 afilename:string;
 datainfosource:TRpDatainfoItem;
 baseinfo:TRpDatabaseInfoItem;
 doexit:boolean;
 param:TRpParam;
 sqlsentence:widestring;
begin
 if connecting then
  Raise Exception.Create(SRpCircularDatalink+' - '+alias);
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
    try
     FDataset.Active:=true;
    except
     on E:Exception do
     begin
      Raise Exception.Create(E.Message);
     end;
    end;
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
   // Substitute text in parameters
   sqlsentence:=FSQL;
   for i:=0 to params.Count-1 do
   begin
    param:=params.items[i];
    if param.ParamType=rpParamSubst then
    begin
     index:=param.Datasets.IndexOf(Alias);
     if index>=0 then
     begin
      sqlsentence:=StringReplace(sqlsentence,param.Search,param.Value,[rfReplaceAll, rfIgnoreCase]);
     end;
    end;
   end;

   if Assigned(FMasterSource) then
   begin
    FMasterSource.Free;
    FMasterSource:=nil;
   end;
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
       Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverDBX);
{$ENDIF}
      end;
     rpdataibx:
      begin
{$IFDEF USEIBX}
       FSQLInternalQuery:=TIBQuery.Create(nil);
{$ELSE}
       Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverIBX);
{$ENDIF}
      end;
     rpdatazeos:
      begin
{$IFDEF USEZEOS}
       FSQLInternalQuery:=TZReadOnlyQuery.Create(nil);
{$ELSE}
       Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverZeos);
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
       Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverBDE);
{$ENDIF}
      end;
     rpdataado:
      begin
{$IFDEF USEADO}
       FSQLInternalQuery:=TADOQuery.Create(nil);
{$ELSE}
       Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverADO);
{$ENDIF}
      end;
     rpdataibo:
      begin
{$IFDEF USEIBO}
       FSQLInternalQuery:=TIBOQuery.Create(nil);
{$ELSE}
       Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverIBO);
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
     rpdatazeos:
      begin
{$IFDEF USEZEOS}
       if Not (FSQLInternalQuery is TZReadOnlyQuery) then
       begin
        FSQLInternalQuery.Free;
        FSQLInternalQuery:=nil;
        FSQLInternalQuery:=TZReadOnlyQuery.Create(nil);
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

   // Assigns the connection
   case baseinfo.Driver of
    rpdatadbexpress:
     begin
{$IFDEF USESQLEXPRESS}
      TSQLQuery(FSQLInternalQuery).SQLConnection:=
       baseinfo.SQLConnection;
      TSQLQuery(FSQLInternalQuery).SQL.Text:=SQLsentence;
      TSQLQuery(FSQLInternalQuery).DataSource:=nil;
{$ENDIF}
     end;
    rpdataibx:
     begin
{$IFDEF USEIBX}
      TIBQuery(FSQLInternalQuery).Database:=
       baseinfo.FIBDatabase;
      TIBQuery(FSQLInternalQuery).SQL.Text:=SQLsentence;
      if Assigned(TIBQuery(FSQLInternalQuery).Database) then
      begin
       TIBQuery(FSQLInternalQuery).Transaction:=
        TIBQuery(FSQLInternalQuery).Database.DefaultTransaction;
      end;
      TIBQuery(FSQLInternalQuery).UniDirectional:=true;
      TIBQuery(FSQLInternalQuery).DataSource:=nil;
{$ENDIF}
     end;
    rpdatazeos:
     begin
{$IFDEF USEZEOS}
      TZReadOnlyQuery(FSQLInternalQuery).Connection:=
       baseinfo.FZConnection;
      TZReadOnlyQuery(FSQLInternalQuery).SQL.Text:=SQLsentence;
//      TZReadOnlyQuery(FSQLInternalQuery).DataSource:=nil;
{$ENDIF}
     end;
    rpdatamybase:
     begin
      try
       TClientDataSet(FSQLInternalQuery).IndexName:='';
       TClientDataSet(FSQLInternalQuery).IndexFieldNames:='';
       if Length(FMyBaseFileName)>0 then
       begin
        // Adds the path
        afilename:=baseinfo.FMyBasePath+FMyBaseFilename;
        if Length(FMyBaseFields)>0 then
        begin
         TClientDataSet(FSQLInternalQuery).IndexDefs.Clear;
         TClientDataSet(FSQLInternalQuery).FieldDefs.Clear;
         FillClientDatasetFromFile(TClientDataSet(FSQLInternalQuery),baseinfo.FMyBasePath+FMyBaseFields,afilename,FMyBaseIndexFields);
        end
        else
        begin
         TClientDataSet(FSQLInternalQuery).IndexFieldNames:=FMyBaseIndexFields;
         TClientDataSet(FSQLInternalQuery).LoadFromFile(afilename);
        end;
       end
       else
       begin
        TClientDataSet(FSQLInternalQuery).IndexDefs.Clear;
        TClientDataSet(FSQLInternalQuery).FieldDefs.Clear;
        TClientDataSet(FSQLInternalQuery).IndexDefs.Add('IPRIM',FMyBaseIndexFields,[]);
        TClientDataSet(FSQLInternalQuery).IndexFieldNames:=FMyBaseIndexFields;
       end;
       for i:=0 to FDataUnions.Count-1 do
       begin
        index:=TRpDatainfolist(Collection).IndexOf(FDataUnions.Strings[i]);
        if index<0 then
         Raise Exception.Create(SRpDataUnionNotFound+' - '+Alias+' - '+FDataUnions.Strings[i]);
        TRpDatainfolist(Collection).Items[index].Connect(databaseinfo,params);
        CombineAddDataset(TClientDataSet(FSQLInternalQuery),TRpDatainfolist(Collection).Items[index].Dataset,FGroupUnion);
       end;
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
       TQuery(FSQLInternalQuery).SQL.Text:=SQLsentence;
       TQuery(FSQLInternalQUery).UniDirectional:=True;
       TQuery(FSQLInternalQuery).DataSource:=nil;
      end
      else
      begin
       TTable(FSQLInternalQuery).MasterSource:=nil;
       TTable(FSQLInternalQuery).Tablename:=FBDETable;
       TTable(FSQLInternalQUery).IndexFieldNames:='';
       TTable(FSQLInternalQUery).IndexName:='';
       if Length(FBDEIndexFields)>0 then
        TTable(FSQLInternalQUery).IndexFieldNames:=FBDEIndexFields;
       if Length(FBDEIndexName)>0 then
        TTable(FSQLInternalQUery).IndexName:=FBDEIndexName;
      end;
{$ENDIF}
     end;
    rpdataado:
     begin
{$IFDEF USEADO}
      TADOQuery(FSQLInternalQuery).Connection:=baseinfo.ADOConnection;
      TADOQuery(FSQLInternalQuery).SQL.Text:=SQLsentence;
      TADOQuery(FSQLInternalQuery).CursorType:=ctOpenForwardOnly;
      TADOQuery(FSQLInternalQuery).DataSource:=nil;
//      Activating this switches break linked querys
//      TADOQuery(FSQLInternalQuery).CursorLocation:=clUseServer;
{$ENDIF}
     end;
    rpdataibo:
     begin
{$IFDEF USEIBO}
      TIBOQuery(FSQLInternalQuery).IB_Connection:=baseinfo.FIBODatabase;
      TIBOQuery(FSQLInternalQuery).SQL.Text:=SQLsentence;
      TIBOQuery(FSQLInternalQuery).UniDirectional:=true;
      TIBOQuery(FSQLInternalQuery).DataSource:=nil;
{$ENDIF}
     end;
   end;
   // Use the datasource
   if Assigned(datainfosource) then
   begin
    FMasterSource:=TDataSource.Create(nil);
    case baseinfo.Driver of
     rpdatadbexpress:
      begin
{$IFDEF USESQLEXPRESS}
       TSQLQuery(FSQLInternalQuery).DataSource:=FMasterSource;
       if datainfosource.cached then
        FMasterSource.DataSet:=datainfosource.CachedDataset
       else
        FMasterSource.DataSet:=datainfosource.Dataset;
{$ENDIF}
      end;
     rpdataibx:
      begin
{$IFDEF USEIBX}
       TIBQuery(FSQLInternalQuery).DataSource:=FMasterSource;
       if datainfosource.cached then
        FMasterSource.DataSet:=datainfosource.CachedDataset
       else
        FMasterSource.DataSet:=datainfosource.Dataset;
{$ENDIF}
      end;
     rpdatazeos:
      begin
{$IFDEF USEZEOS}
{       TZReadOnlyQuery(FSQLInternalQuery).DataSource:=FMasterSource;
       if datainfosource.cached then
        FMasterSource.DataSet:=datainfosource.CachedDataset
       else
        FMasterSource.DataSet:=datainfosource.Dataset;
}
{$ENDIF}
      end;
     rpdatabde:
      begin
{$IFDEF USEBDE}
       if FBDEType=rpdquery then
       begin
        TQuery(FSQLInternalQuery).DataSource:=FMasterSource;
        if datainfosource.cached then
         FMasterSource.DataSet:=datainfosource.CachedDataset
        else
         FMasterSource.DataSet:=datainfosource.Dataset;
       end
       else
       begin
        TTable(FSQLInternalQuery).MasterFields:=BDEMasterFields;
        TTable(FSQLInternalQuery).MasterSource:=FMasterSource;
        if datainfosource.cached then
         FMasterSource.DataSet:=datainfosource.CachedDataset
        else
         FMasterSource.DataSet:=datainfosource.Dataset;
       end;
{$ENDIF}
      end;
     rpdataado:
      begin
{$IFDEF USEADO}
       TADOQuery(FSQLInternalQuery).DataSource:=FMasterSource;
       if datainfosource.cached then
        FMasterSource.DataSet:=datainfosource.CachedDataset
       else
        FMasterSource.DataSet:=datainfosource.Dataset;
{$ENDIF}
      end;
     rpdataibo:
      begin
{$IFDEF USEIBO}
       TIBOQuery(FSQLInternalQuery).DataSource:=FMasterSource;
       if datainfosource.cached then
        FMasterSource.DataSet:=datainfosource.CachedDataset
       else
        FMasterSource.DataSet:=datainfosource.Dataset;
{$ENDIF}
      end;
    end;
   end;
   // Assigns parameters
   for i:=0 to params.count-1 do
   begin
    param:=params.items[i];
    if param.ParamType=rpParamSubst then
     continue;
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
      rpdatazeos:
       begin
{$IFDEF USEZEOS}
        TZReadOnlyQuery(FSQLInternalQuery).ParamByName(param.Name).DataType:=
         ParamTypeToDataType(param.ParamType);
        TZReadOnlyQuery(FSQLInternalQuery).ParamByName(param.Name).Value:=param.Value;
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
{$IFDEF USEBDE}
   if (FSQLInternalQuery is TTable) then
   begin
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
   if cached then
   begin
    FCachedDataset.DoClose;
    FCachedDataset.Dataset:=FDataset;
    FCachedDataset.DoOpen;
   end;
  end;

  connecting:=false;
 except
  on E:Exception do
  begin
   connecting:=false;
   Raise Exception.Create(Alias+':'+E.Message);
  end;
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

 FDataUnions:=TStringList.Create;
 FBDEType:=rpdquery;
 FCachedDataset:=TRpDataset.Create(nil);
end;

destructor TRpDataInfoItem.Destroy;
begin
 try
  FDataUnions.free;
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

constructor TRpConnAdmin.Create;
begin
 LoadConfig;
end;

procedure TRpConnAdmin.DeleteConnection(conname:string);
begin
 config.EraseSection(conname);
end;

procedure TRpConnAdmin.AddConnection(newname:string;drivername:string);
var
 alist:TStringList;
 i:integer;
begin
 config.EraseSection(newname);
 alist:=TStringList.Create;
 try
  drivers.ReadSectionValues(drivername,alist);
  config.WriteString(newname,'DriverName',drivername);
  for i:=0 to alist.count-1 do
  begin
   if Uppercase(alist.Names[i])<>'GETDRIVERFUNC' then
    if Uppercase(alist.Names[i])<>'VENDORLIB' then
     if Uppercase(alist.Names[i])<>'LIBRARYNAME' then
     begin
      config.WriteString(newname,alist.Names[i],alist.Values[alist.Names[i]]);
     end;
  end;
 finally
  alist.free;
 end;
end;

procedure TRpConnAdmin.GetConnectionNames(alist:TStrings;drivername:String);
var
 alist2:TStringList;
 i:integer;
begin
 alist.clear;
 alist2:=TStringList.Create;
 try
  config.ReadSections(alist2);
  for i:=0 to alist2.Count-1 do
  begin
   if Length(drivername)>0 then
   begin
    if UpperCase(config.ReadString(alist2.Strings[i],'DriverName',''))=
     UpperCase(drivername) then
     alist.Add(alist2.Strings[i]);
   end
   else
    alist.Add(alist2.Strings[i]);
  end;
 finally
  alist2.free;
 end;
end;

procedure TRpConnAdmin.GetDriverNames(alist:TStrings);
var
 alist2:TStringList;
 i:integer;
begin
 alist.clear;
 alist2:=TStringList.Create;
 try
  drivers.ReadSection('Installed Drivers',alist2);
  for i:=0 to alist2.count-1 do
   alist.add(alist2.Strings[i]);
 finally
  alist2.free;
 end;
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
 if Length(configdir)>12 then
 begin
  if Not DirectoryExists(configdir) then
  begin
   if not CreateDir(configdir) then
    Raise Exception.Create(SRpDirCantBeCreated+'-'+configdir);
  end;
 end
 else
  configdir:='/usr/local/etc';
 driverfilename:=configdir+'/'+DBXDRIVERFILENAME;
 configfilename:=configdir+'/'+DBXCONFIGFILENAME;
 if  (configdir='/usr/local/etc') then
 begin
  driverfilename:=driverfilename+'.conf';
  configfilename:=configfilename+'.conf';
 end;
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
   Raise Exception.Create(SRpConfigFileNotExists+' - '+driverfilename);
{$ENDIF}
{$IFDEF LINUX}
  // Check if exists in the current dir
  if FileExists(DBXDRIVERFILENAME) then
  begin
   drivers:=TMemIniFile.Create(DBXDRIVERFILENAME);
   if configdir<>'/usr/local/etc' then
   begin
    CopyFileTo(DBXDRIVERFILENAME,driverfilename);
   end;
  end
  else
  begin
   // Check int /usr/local/etc
   if FileExists('/usr/local/etc/'+DBXDRIVERFILENAME+'.conf') then
   begin
    if configdir<>'/usr/local/etc' then
    begin
     CopyFileTo('/usr/local/etc/'+DBXDRIVERFILENAME+'.conf',driverfilename);
    end;
    drivers:=TMemIniFile.Create(driverfilename);
   end
   else
    Raise Exception.Create(SRpConfigFileNotExists+' - '+DBXDRIVERFILENAME);
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
   Raise Exception.Create(SRpConfigFileNotExists+' - '+driverfilename);
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
    Raise Exception.Create(SRpConfigFileNotExists+' - '+DBXCONFIGFILENAME);
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

function TRpDatabaseInfoItem.GetStreamFromSQL(sqlsentence:String;params:TStringList):TStream;
var
 data:TDataset;
 memstream:TMemoryStream;
 astream:TStream;
begin
 Result:=nil;
 data:=OpenDatasetFromSQL(sqlsentence,params,false);
 try
  if data.Eof then
   Raise Exception.Create(SRpExternalSectionNotFound);
  if data.FieldCount<1 then
   Raise Exception.Create(SRpExternalSectionNotFound);
  memstream:=TMemoryStream.Create;
  try
   astream:=data.CreateBlobStream(data.fields[0],bmRead);
   memstream.SetSize(astream.size);
   astream.Read(memstream.memory^,memstream.size);
   memstream.Seek(0,soFromBeginning);
  except
   memstream.free;
   Raise;
  end;
  Result:=memstream;
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
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverDBX);
{$ENDIF}
   end;
  rpdataibx:
   begin
{$IFDEF USEIBX}
    FIBDatabase.GetTableNames(alist);
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverIBX);
{$ENDIF}
   end;
  rpdatazeos:
   begin
{$IFDEF USEZEOS}
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverZeos);
//    FZConnection.GetTableNames(alist);
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverZeos);
{$ENDIF}
   end;
  rpdatamybase:
   begin
    alist.Clear;
   end;
  rpdatabde:
   begin
{$IFDEF USEBDE}
    if Assigned(TRpDatabaseInfoList(Collection).FBDESession) then
     TRpDatabaseInfoList(Collection).FBDESession.GetTableNames(FBDEDatabase.Databasename,'',False,False,alist)
    else
     Session.GetTableNames(FBDEDatabase.Databasename,'',False,False,alist);
//    GetTableNames(alist);
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverBDE);
{$ENDIF}
   end;
  rpdataado:
   begin
{$IFDEF USEADO}
    ADOConnection.GetTableNames(alist);
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverADO);
{$ENDIF}
   end;
  rpdataibo:
   begin
{$IFDEF USEIBO}
    alist.clear;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverIBO);
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
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverDBX);
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
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverIBX);
{$ENDIF}
   end;
  rpdatazeos:
   begin
{$IFDEF USEZEOS}
    FSQLInternalQuery:=TZReadOnlyQuery.Create(nil);
    TZReadOnlyQuery(FSQLInternalQuery).Connection:=FZConnection;
    TZReadOnlyQuery(FSQLInternalQuery).SQL.Text:=SQLsentence;
//    TZReadOnlyQuery(FSQLInternalQuery).UniDirectional:=true;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverZeos);
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
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverBDE);
{$ENDIF}
   end;
  rpdataado:
   begin
{$IFDEF USEADO}
    FSQLInternalQuery:=TADOQuery.Create(nil);
    TADOQuery(FSQLInternalQuery).Connection:=ADOConnection;
    TADOQuery(FSQLInternalQuery).SQL.Text:=SQLsentence;
    TADOQuery(FSQLInternalQuery).CursorType:=ctOpenForwardOnly;
{$ELSE}
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverADO);
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
    Raise Exception.Create(SRpDriverNotSupported+' - '+SrpDriverIBO);
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
    rpdatazeos:
     begin
{$IFDEF USEZEOS}
      if assigned(astream) then
      begin
       TZReadOnlyQuery(FSQLInternalQuery).ParamByName(paramName).DataType:=ftBlob;
       TZReadOnlyQuery(FSQLInternalQuery).ParamByName(paramName).LoadFromStream(astream,ftBlob);
      end
      else
      begin
       TZReadOnlyQuery(FSQLInternalQuery).ParamByName(paramName).DataType:=
        VariantTypeToDataType(avariant);
       TZReadOnlyQuery(FSQLInternalQuery).ParamByName(paramName).Value:=avariant;
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
   rpdatazeos:
    begin
 {$IFDEF USEZEOS}
     TZReadOnlyQuery(FSQLInternalQuery).ExecSQL;
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

procedure GetRpDatabaseDrivers(alist:TStrings);
begin
 alist.clear;
 alist.Add('Borland DBExpress');
 alist.Add('B.MyBase and text files');
 alist.Add('Interbase Express');
 alist.Add('Borland Database Engine');
 alist.Add('Microsoft DAO');
 alist.Add('Interbase Objects');
 alist.Add('Zeos Database Objects');
end;


procedure TRpDataInfoList.IntDisableLink(alist:TStringList;i:integer);
var
 index:integer;
 j:integer;
begin
 for j:=0 to Count-1 do
 begin
  index:=alist.IndexOf(IntToStr(i));
  if index>=0 then
  begin
   if i<>j then
   begin
    if assigned(items[j].FMasterSource) then
    begin
     if items[j].FDataSource=items[i].Alias then
     begin
      IntDisableLink(alist,j);
     end;
    end;
   end;
  end;
 end;
 if Assigned(Items[i].FMasterSource) then
 begin
  index:=alist.IndexOf(IntToStr(i));
  if index>=0 then
  begin
   Items[i].FMasterSource.Enabled:=false;
   alist.Delete(index);
  end;
 end;
end;



procedure TRpDataInfoList.DisableLinks;
var
 alist:TStringList;
 i:integer;
begin
 alist:=TStringList.Create;
 try
  for i:=0 to Count-1 do
  begin
   alist.Add(IntToStr(i));
  end;
  for i:=0 to Count-1 do
  begin
   IntDisableLink(alist,i);
  end;
 finally
  alist.free;
 end;
end;

procedure TRpDataInfoList.IntEnableLink(alist:TStringList;i:integer);
var
 index:integer;
 sindex:integer;
begin
 index:=alist.IndexOf(IntToStr(i));
 if index<0 then
  exit;
 // Enables first the master source
 if Assigned(Items[i].FMasterSource) then
 begin
  sindex:=IndexOf(Items[i].FDataSource);
  if sindex>=0 then
  begin
   IntEnableLink(alist,sindex);
  end;
  Items[i].FMasterSource.Enabled:=True;
 end;
 alist.Delete(index);
end;

procedure TRpDataInfoList.EnableLinks;
var
 alist:TStringList;
 i:integer;
begin
 alist:=TStringList.Create;
 try
  for i:=0 to Count-1 do
  begin
   alist.Add(IntToStr(i));
  end;
  for i:=0 to Count-1 do
  begin
   IntEnableLink(alist,i);
  end;
 finally
  alist.free;
 end;
end;

procedure ParseFields(fields:String;list:TStrings);
var
 fieldrest:string;
 index:integer;
begin
 fieldrest:=fields;
 list.clear;
 repeat
  index:=Pos(';',fieldrest);
  if index>0 then
  begin
   list.add(Copy(fieldrest,1,index-1));
   fieldrest:=Copy(fieldrest,index+1,Length(fieldrest));
  end;
 until index<1;
 list.add(fieldrest);
end;

procedure CombineAddDataset(client:TClientDataset;data:TDataset;group:boolean);
var
 i,index:integer;
 groupfields:TStringList;
 groupfieldindex:TStringList;
 grouped:boolean;
begin
 groupfields:=TStringList.Create;
 groupfieldindex:=TStringList.Create;
 try
  // Combine the two datasets
  if client.FieldDefs.Count<1 then
  begin
   client.Close;
   client.FieldDefs.Assign(data.FieldDefs);
   client.CreateDataSet;
  end;
  if data.fields.Count>client.Fields.Count then
  begin
   Raise Exception.Create(SRpCannotCombine);
  end;
  if group then
  begin
   ParseFields(client.IndexFieldNames,groupfields);
   for i:=0 to groupfields.count-1 do
   begin
    groupfieldindex.Add(IntToStr(client.FieldByName(groupfields.strings[i]).index));
   end;
  end;
  while not data.eof do
  begin
   grouped:=false;
   if Group then
   begin
    client.SetKey;
    for i:=0 to groupfieldindex.count-1 do
    begin
     index:=StrToInt(groupfieldindex.Strings[i]);
     client.Fields[index].AsVariant:=data.Fields[index].AsVariant;
    end;
    if client.GotoKey then
    begin
     grouped:=true;
     client.edit;
     try
      for i:=0 to data.fieldcount-1 do
      begin
       index:=groupfieldindex.IndexOf(IntToStr(i));
       if index<0 then
       begin
        if Not data.Fields[i].IsNull then
        begin
         if client.Fields[i].IsNull then
          client.Fields[i].AsVariant:=data.Fields[i].AsVariant
         else
          client.Fields[i].AsVariant:=client.Fields[i].AsVariant+data.Fields[i].AsVariant;
        end;
       end;
      end;
      client.post;
     except
      client.cancel;
      raise;
     end;
    end;
   end;
   if not grouped then
   begin
    client.Append;
    try
     for i:=0 to data.fieldcount-1 do
     begin
      client.Fields[i].AsVariant:=data.Fields[i].AsVariant;
     end;
     client.post;
    except
     client.cancel;
     raise;
    end;
   end;
   data.Next;
  end;
 finally
  groupfields.free;
  groupfieldindex.free;
 end;
end;

procedure TRpDatabaseInfoItem.DefineProperties(Filer:TFiler);
begin
 inherited;

 Filer.DefineProperty('ADOConnectionString',ReadAdoConnectionString,WriteAdoConnectionString,True);
end;

procedure TRpDatabaseInfoItem.ReadAdoConnectionString(Reader:TReader);
begin
 FAdoConnectionString:=ReadWideString(Reader);
end;

procedure TRpDatabaseInfoItem.WriteAdoConnectionString(Writer:TWriter);
begin
 WriteWideString(Writer, FAdoConnectionString);
end;

procedure TRpDatabaseInfoList.SaveToFile(ainifile:String);
var
 i:integer;
 inif:TMemInifile;
 concount:integer;
 aitem:TRpDatabaseInfoItem;
 conname:String;
begin
 inif:=TMemInifile.Create(ainifile);
 try
  concount:=Count;
  inif.WriteInteger('REPMAN_CONNECTIONS','COUNT',concount);
  for i:=0 to concount-1 do
  begin
   aitem:=Items[i];
   conname:='REPMAN_CONNECTION'+IntToStr(i);
   inif.WriteString(conname,'NAME',aitem.FAlias);
   inif.WriteString(conname,'ADOSTRING',aitem.ADOConnectionString);
   inif.WriteBool(conname,'LOADPARAMS',aitem.LoadParams);
   inif.WriteBool(conname,'LOADDRIVERPARAMS',aitem.LoadDriverParams);
   inif.WriteBool(conname,'LOGINPROMPT',aitem.LoginPrompt);
   inif.WriteInteger(conname,'DRIVER',Integer(aitem.Driver));
   inif.WriteString(conname,'REPORTTABLE',aitem.ReportTable);
   inif.WriteString(conname,'REPORTFIELD',aitem.ReportField);
   inif.WriteString(conname,'REPORTSEARCHFIELD',aitem.ReportSearchField);
   inif.WriteString(conname,'REPORTGROUPSTABLE',aitem.ReportGroupsTable);
  end;
  inif.UpdateFile;
 finally
  inif.free;
 end;
end;

procedure TRpDatabaseInfoList.LoadFromFile(ainifile:String);
var
 i:integer;
 inif:TMemInifile;
 concount:integer;
 aitem:TRpDatabaseInfoItem;
 aname:String;
 conname:String;
begin
 inif:=TMemInifile.Create(ainifile);
 try
  Clear;
  concount:=inif.ReadInteger('REPMAN_CONNECTIONS','COUNT',0);
  for i:=0 to concount-1 do
  begin
   conname:='REPMAN_CONNECTION'+IntToStr(i);
   aname:=inif.ReadString(conname,'NAME','CONNECTION'+IntToStr(i));
   aitem:=Add(aname);
   aitem.ADOConnectionString:=inif.ReadString(conname,'ADOSTRING',aitem.ADOConnectionString);
   aitem.LoadParams:=inif.ReadBool(conname,'LOADPARAMS',aitem.LoadParams);
   aitem.LoadDriverParams:=inif.ReadBool(conname,'LOADDRIVERPARAMS',aitem.LoadDriverParams);
   aitem.LoginPrompt:=inif.ReadBool(conname,'LOGINPROMPT',aitem.LoginPrompt);
   aitem.Driver:=TRpDbDriver(inif.ReadInteger(conname,'DRIVER',Integer(aitem.Driver)));
   aitem.ReportTable:=inif.ReadString(conname,'REPORTTABLE',aitem.ReportTable);
   aitem.ReportField:=inif.ReadString(conname,'REPORTFIELD',aitem.ReportField);
   aitem.ReportSearchField:=inif.ReadString(conname,'REPORTSEARCHFIELD',aitem.ReportSearchField);
   aitem.ReportGroupsTable:=inif.ReadString(conname,'REPORTGROUPSTABLE',aitem.ReportGroupsTable);
  end;
 finally
  inif.free;
 end;
end;

procedure TRpDatabaseInfoItem.CreateLibrary(reporttable,reportfield,reportsearchfield,groupstable:String);
var
 astring:String;
begin
 // Creates the library
 astring:='CREATE TABLE '+reporttable+' ('+reportsearchfield+' VARCHAR(50) NOT NULL,'+
  reportfield+' BLOB,REPORT_GROUP INTEGER,USER_FLAG INTEGER,PRIMARY KEY ('+reportsearchfield+'))';
 OpenDatasetFromSQL(astring,nil,true);
 astring:='CREATE TABLE REPMAN_GROUPS (GROUP_CODE INTEGER NOT NULL,'+
  'GROUP_NAME VARCHAR(50),PARENT_GROUP INTEGER NOT NULL,'+
  'PRIMARY KEY (GROUP_CODE))';
 OpenDatasetFromSQL(astring,nil,true);
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



