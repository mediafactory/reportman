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
 DB,rpparams,Inifiles,rptypes,DBClient,
 IBQuery,IBDatabase,rpdataset,
{$IFDEF MSWINDOWS}
  dbtables,adodb,
{$ENDIF}
 SqlConst;

{$IFDEF LINUX}
const
 DBXDRIVERFILENAME='dbxdrivers';
 DBXCONFIGFILENAME='dbxconnections';
{$ENDIF}
type
 TRpDbDriver=(rpdatadbexpress,rpdatamybase,rpdataibx,
  rpdatabde,rpdataado);


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
   FADOConnectionString:widestring;
   FIBDatabase:TIBDatabase;
{$IFDEF MSWINDOWS}
   FADOConnection:TADOConnection;
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
   property ADOConnectionString:widestring read FADOConnectionString write FADOConnectionString;
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
   FCachedDataset:TRpDataset;
   FSQLInternalQuery:TDataset;
   FMyBaseFilename:string;
   FMyBaseIndexFields:string;
   connecting:boolean;
   FCached:Boolean;
   procedure SetDatabaseAlias(Value:string);
   procedure SetAlias(Value:string);
   procedure SetDataSource(Value:string);
   procedure SetSQL(Value:widestring);
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
 if Assigned(FIBDatabase) then
 begin
  FIBDatabase.DefaultTransaction.Free;
  FIBDatabase.Free;
 end;
{$IFDEF MSWINDOWS}
 FBDEDatabase.free;
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
{$IFDEF LINUX}
 if (FDriver in [rpdatabde..rpdataado]) then
  Raise Exception.Create(SRpDriverNotSupported);
{$ENDIF}

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
  rpdataibx:
   begin
     if Not Assigned(FIBDatabase) then
     begin
      FIBDatabase:=TIBDatabase.Create(nil);
      FIBDatabase.DefaultTransaction:=TIBTransaction.Create(nil);
      FIBDatabase.DefaultTransaction.DefaultDatabase:=FIBDatabase;
      FIBDatabase.DefaultTransaction.Params.Add('concurrency');
      FIBDatabase.DefaultTransaction.Params.Add('nowait');
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
   end;
  rpdatamybase:
   begin
    // Nothing to do
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
  rpdataado:
   begin
{$IFDEF MSWINDOWS}
    if Not Assigned(FADOConnection) then
     FADOConnection:=TADOConnection.Create(nil);
    if FADOConnection.ConnectionString=ADOConnectionString then
    begin
     if FADOConnection.Connected then
      exit;
    end
    else
    begin
     FADOConnection.Connected:=false;
    end;
    FADOConnection.Mode:=cmRead;
    FADOConnection.ConnectionString:=ADOConnectionString;
    FADOConnection.LoginPrompt:=LoginPrompt;
    FADOConnection.Connected:=true;
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
 if Assigned(FIBDatabase) then
 begin
  FIBDatabase.Connected:=False;
 end;
{$IFDEF MSWINDOWS}
 if Assigned(FBDEDatabase) then
  FBDEDatabase.Connected:=false;
 if Assigned(FADOConnection) then
  FADOConnection.Connected:=false;
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
   begin
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
   if FDataset<>FSQLInternalQuery then
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
   databaseinfo.items[index].Connect;


   if not assigned(FSQLInternalQuery) then
   begin
    case databaseinfo.items[index].FDriver of
     rpdatadbexpress:
      begin
       FSQLInternalQuery:=TSQLQuery.Create(nil);
      end;
     rpdataibx:
      begin
       FSQLInternalQuery:=TIBQuery.Create(nil);
      end;
     rpdatamybase:
      begin
       FSQLInternalQuery:=TClientDataset.Create(nil);
      end;
     rpdatabde:
      begin
{$IFDEF MSWINDOWS}
       FSQLInternalQuery:=TQuery.Create(nil);
{$ENDIF}
      end;
     rpdataado:
      begin
{$IFDEF MSWINDOWS}
       FSQLInternalQuery:=TADOQuery.Create(nil);
{$ENDIF}
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
        FSQLInternalQuery:=TSQLQuery.Create(nil);
       end;
      end;
     rpdataibx:
      begin
       if Not (FSQLInternalQuery is TIBQuery) then
       begin
        FSQLInternalQuery.Free;
        FSQLInternalQuery:=nil;
        FSQLInternalQuery:=TIBQuery.Create(nil);
       end;
      end;
     rpdatabde:
      begin
{$IFDEF MSWINDOWS}
       if Not (FSQLInternalQuery is TQuery) then
       begin
        FSQLInternalQuery.Free;
        FSQLInternalQuery:=nil;
        FSQLInternalQuery:=TQuery.Create(nil);
       end;
{$ENDIF}
      end;
     rpdataado:
      begin
{$IFDEF MSWINDOWS}
       if Not (FSQLInternalQuery is TADOQuery) then
       begin
        FSQLInternalQuery.Free;
        FSQLInternalQuery:=nil;
        FSQLInternalQuery:=TADOQuery.Create(nil);
       end;
{$ENDIF}
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
    rpdataibx:
     begin
      TIBQuery(FSQLInternalQuery).Database:=
       databaseinfo.items[index].FIBDatabase;
      TIBQuery(FSQLInternalQuery).SQL.Text:=SQL;
      if Assigned(TIBQuery(FSQLInternalQuery).Database) then
      begin
       TIBQuery(FSQLInternalQuery).Transaction:=
        TIBQuery(FSQLInternalQuery).Database.DefaultTransaction;
      end;
      TIBQuery(FSQLInternalQuery).UniDirectional:=true;
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
{$IFDEF MSWINDOWS}
      TQuery(FSQLInternalQuery).DatabaseName:=databaseinfo[index].FBDEAlias;
      TQuery(FSQLInternalQuery).SQL.Text:=SQL;
      TQuery(FSQLInternalQUery).UniDirectional:=True;
{$ENDIF}
     end;
    rpdataado:
     begin
{$IFDEF MSWINDOWS}
      TADOQuery(FSQLInternalQuery).Connection:=databaseinfo[index].FADOConnection;
      TADOQuery(FSQLInternalQuery).SQL.Text:=SQL;
      TADOQuery(FSQLInternalQuery).CursorType:=ctOpenForwardOnly;
      TADOQuery(FSQLInternalQuery).CursorLocation:=clUseServer;
{$ENDIF}
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
       if datainfosource.cached then
        TSQLQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.CachedDataset
       else
        TSQLQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.Dataset;
      end;
     rpdataibx:
      begin
       if Not Assigned(TIBQuery(FSQLInternalQuery).DataSource) then
        TIBQuery(FSQLInternalQuery).DataSource:=TDataSource.Create(nil);
       if datainfosource.cached then
        TIBQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.CachedDataset
       else
        TIBQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.Dataset;
      end;
     rpdatabde:
      begin
{$IFDEF MSWINDOWS}
       if Not Assigned(TQuery(FSQLInternalQuery).DataSource) then
        TQuery(FSQLInternalQuery).DataSource:=TDataSource.Create(nil);
       if datainfosource.cached then
        TQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.CachedDataset
       else
        TQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.Dataset;
{$ENDIF}
      end;
     rpdataado:
      begin
{$IFDEF MSWINDOWS}
       if Not Assigned(TADOQuery(FSQLInternalQuery).DataSource) then
        TADOQuery(FSQLInternalQuery).DataSource:=TDataSource.Create(nil);
       if datainfosource.cached then
        TADOQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.CachedDataset
       else
        TADOQuery(FSQLInternalQuery).DataSource.DataSet:=datainfosource.Dataset;
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
     case databaseinfo.items[index].Driver of
      rpdatadbexpress:
       begin
        TSQLQuery(FSQLInternalQuery).ParamByName(param.Name).DataType:=
         ParamTypeToDataType(param.ParamType);
        TSQLQuery(FSQLInternalQuery).ParamByName(param.Name).Value:=param.Value;
       end;
      rpdataibx:
       begin
        TIBQuery(FSQLInternalQuery).ParamByName(param.Name).DataType:=
         ParamTypeToDataType(param.ParamType);
        TIBQuery(FSQLInternalQuery).ParamByName(param.Name).Value:=param.Value;
       end;
      rpdatabde:
       begin
{$IFDEF MSWINDOWS}
        TQuery(FSQLInternalQuery).ParamByName(param.Name).DataType:=
         ParamTypeToDataType(param.ParamType);
        TQuery(FSQLInternalQuery).ParamByName(param.Name).Value:=param.Value;
{$ENDIF}
       end;
      rpdataado:
       begin
{$IFDEF MSWINDOWS}
        TADOQuery(FSQLInternalQuery).Parameters.ParamByName(param.Name).DataType:=
         ParamTypeToDataType(param.ParamType);
        TADOQuery(FSQLInternalQuery).Parameters.ParamByName(param.Name).Value:=param.Value;
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
  FDataset.Active:=false;
  FCachedDataset.DoClose;
 end;
end;

constructor TRpDatainfoitem.Create(Collection:TCollection);
begin
 inherited Create(Collection);

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
