{
  Kylix / Delphi open source DbExpress driver for ODBC
  Version 1.03, 6 December 2001 

  Copyright (c) 2001 Edward Benson

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}
unit DbxOpenOdbc;

interface

uses
  OdbcApi,
  DBXpress,
  Classes;

{
  Kylix / Delphi DbExpress driver for ODBC Level 3.

  Normally SqlExpress loads the driver DLL dynamically (ie dbxoodbc.dll),
  according to TSQLConnection.LibraryName.

  Alternatively if you add this unit to a USES anywhere in your project,
  the driver will be statically linked into your program and SqlExpress will
  ignore TSQLConnection.LibraryName. (This magic is achieved by the call to
  SqlExpr.RegisterDbXpressLib in the initialization section of this unit, but
  it only works in Windows - in Kylix SqlExpr.RegisterDbXpressLib is not defined)

  The project source for DLL version is 'dbxoodbc.dpr', which just
  USES this module and EXPORTS the getSQLDriverODBC function.

  ------------------------------------------------------------------------------

  Steps to manually install new DBExpress driver
  (This is not really necessary - it just makes it easier to change drivers, by
  selecting TSQLConnection.DriverName and TSQLConnection.ConnectionName drop-down)

  1. Retrieve DbExpress INI file names from registry
     '\Software\Borland\DBExpress' Value 'Driver Registry File'
     (The default ini file names will be:
      C:\Program Files\Common Files\Borland Shared\DBExpress\dbxdrivers.ini)
      C:\Program Files\Common Files\Borland Shared\DBExpress\dbxconnections.ini)

  2. In dbxdrivers.ini file

  a. Add line to '[INSTALLEDDRIVERS]' section
       drivername=1 (eg 'Dbx Open Odbc Driver=1') (The driver name can be any name you like)

  b. Add new [drivername] section ie [Dbx Open Odbc Driver], with following 3 lines
       LibraryName=dbxoodbc.dll        (Not case-sensitive in Windows)
       GetDriverFunc=getSQLDriverODBC  (NB This is case-sensitive, even in Windows)
       VendorLib=ODBC32.DLL            (Value not actually used by this driver - ODBC32.DLL is hard-coded)

  3. In dbxconnections.ini file

  a. Add '[ConnectionName]' section with following 4 lines
       DriverName=Dbx Open Odbc Driver (or whatever you called it in dbxdrivers.ini)
       Database=ODBC DSN name, or ODBC connection string, or ? for Driver Prompt
       User_Name=user name (this can be ommitted)
       Password=password (this can be ommitted)

  b. You can have as many different [ConnectionName] sections as you like, so
     you can pre-configure several ODBC connection settings.

  If you skip the install, you must set TSQLConnection properties
  before you connect:
    .LibaryName = dbxoodbc.dll (must be fully qualified if not on the search path)
    .GetDriverFunc = getSQLDriverODBC (NB this is case-sensitive)
    .VendorLib = ODBC32.DLL
    .Params - As described dbxconnections.ini above - Database is required, others optional
}

{
  Note on Error checking:

  In this driver almost every function that implements the various
  DBXpress interfaces is guarded by "try / except".

  If an error occurs, an EDbxError exception of one of the following sub-types is rasied:

    EDbxOdbcError: An error was returned from an ODBC function call
    EDbxInvalidCall: The function or its parameters are not valid
    EDbxNotSupported: The function or its parameters are valid but not yet supported by this driver
    EDbxInternalError: Some other error occurred with this module

  Control then goes to the Except routine in the relevant interface functuon.

  If EDbxNotSupported can happen, this is always checked first, and the
  function returns DBXERR_NOTSUPPORTED, so the caller can take action as necessary.

  Otherwise the EDbxError handler gets invoked (because EDbxError is the parent
  class of the other 3 exception types). The handler retrieves the Exception message,
  saves it in an instance variable (occasionally some other info is also saved),
  and then the function returns MaxReservedStaticErrors + 1.
  The caller can get the error text (via GetErrorMessage/Len) and raise its own
  error or ignore, as appropriate.

  The reason for raising and trapping EDbxError, rather than simply returning
  non SQL_SUCCESS code, is that the calling programs may not always check
  the function return code, it may check for error code but retrieve the error
  message from the wrong interface. So by raising exception at the point of
  error, the IDE halts on the exception during source debugging, and this makes
  it much easier to trace errors.
}

{
Change History

Beta 26 Oct 2001
-------------------------
- First public release

Version 1.01, 28 Nov 2001
-------------------------
- Fix bug in TSqlCursorMetaDataIndexes
- Support Interbase ODBC Driver
- Support MySql ODBC Driver (ODBC level 2)

Version 1.02, 05 Dec 2001
-------------------------
- Fix bug in TSqlCursorOdbc.getBcd to cater for comma decimal separator


Version 1.03, 06 Dec 2001
-------------------------
- Change to support Kylix
}

{ getSQLDriverODBC is the starting point for everything else... }

function getSQLDriverODBC(sVendorLib : PChar; sResourceFile : PChar; out Obj): SQLResult; stdcall;

exports getSQLDriverODBC;

type
  TOdbcDriverType = (eOdbcDriverTypeUnspecified,
   eOdbcDriverTypeGupta, eOdbcDriverTypeMsSqlServer, EOdbcDriverTypeIbmDb2,
   eOdbcDriverTypeAccess, eOdbcDriverTypeMySql);
{ TSqlDriverOdbc implements ISQLDriver }

  TSqlDriverOdbc = class(TInterfacedObject, ISQLDriver)
  private
    fErrorLines: TStringList;
    fSQLCallbackEvent: TSQLCallbackEvent;
    fDbxOptionDrvCallBackInfo: Longint;
    fDrvBlobSizeLimitK: integer;
    fOdbcErrorLines: TStringList;
    fhEnv: SQLHENV;
    fNativeErrorCode: SQLINTEGER;
    fSqlStateChars: TSqlState; // 5 chars long + null terminator
    fSqlState: PSqlState;
    fDbxOptionDrvRestrict: LongWord;
    procedure AllocHCon(out HCon: SQLHDBC);
    procedure AllocHEnv;
    procedure FreeHCon(HCon: SQLHDBC);
    procedure FreeHEnv;
    procedure RetrieveOdbcErrorInfo(CheckCode: SQLRETURN;
     HandleType: Smallint; Handle: SQLHANDLE);
    procedure OdbcCheck(
      CheckCode: SQLRETURN;
      OdbcFunctionName: string;
      HandleType: Smallint;
      Handle: SQLHANDLE);
  protected
    { begin ISQLDriver methods }
    function getSQLConnection(
      out pConn: ISQLConnection
      ): SQLResult; stdcall;
    function SetOption(
      eDOption: TSQLDriverOption;
      PropValue: LongInt
      ): SQLResult; stdcall;
    function GetOption(
      eDOption: TSQLDriverOption;
      PropValue: Pointer;
      MaxLength: SmallInt;
      out Length: SmallInt
      ): SQLResult; stdcall;
    { end ISQLDriver methods }
  public
    constructor Create;
    destructor Destroy; override;
  end;


{ TSqlConnectionOdbc implements ISQLConnection }

  TSqlConnectionOdbc = class(TInterfacedObject, ISQLConnection)
  private
    fConnectionErrorLines: TStringList;
    fOwnerDbxDriver: TSqlDriverOdbc;
    fDbxCallBack: TSQLCallBackEvent;
    fDbxCallBackInfo: Integer;
    fConnected: boolean;
    fConnBlobSizeLimitK: Integer;
// Private fields below are specific to ODBC
    fhCon: SQLHDBC;
    fWantQuotedTableName: boolean;
    fOdbcConnectString: string;
    fOdbcConnectStringHidePassword: string;
    fOdbcReturnedConnectString: pAnsiChar;
    fOdbcMaxColumnNameLen: SQLUSMALLINT;
    fOdbcMaxCatalogNameLen: SQLUSMALLINT;
    fOdbcMaxSchemaNameLen: SQLUSMALLINT;
    fOdbcMaxTableNameLen: SQLUSMALLINT;
    fOdbcMaxIdentifierLen: SQLUSMALLINT;
    fOdbcDriverName: string;
    fOdbcDriverType: TOdbcDriverType;
    fOdbcDriverLevel: integer; // 2 or 3
    fInTransaction: boolean;
    fSupportsCatalog: boolean;
    fSupportsSQLSTATISTICS: boolean;
    fSupportsSQLPRIMARYKEYS: boolean;
    fSupportsSchemaDML: boolean;
    fSupportsSchemaProc: boolean;
    fSupportsCatalogDML: boolean;
    fSupportsCatalogProc: boolean;
    fGetDataAnyOrder: boolean;
    fCurrentCatalog: pAnsiChar;
    fQuoteChar: AnsiChar;
    fAutoCommitMode: SQLUINTEGER;
//    fCurrentSchema: string; // This is no ODBC API call to get this!
    procedure AllocHStmt(out HStmt: SQLHSTMT);
    procedure FreeHStmt(HStmt: SQLHSTMT);
    function GetMetaDataOption(
      eDOption: TSQLMetaDataOption;
      PropValue: Pointer;
      MaxLength: SmallInt;
      out Length: SmallInt
      ): SQLResult;
    procedure OdbcCheck(OdbcCode: SQLRETURN; OdbcFunctionName: string);
    function RetrieveDriverName: SQLResult;
    procedure GetCurrentCatalog;
    procedure TransactionCheck;
  protected
    { begin ISQLConnection methods }
    function connect(
      ServerName: PChar;
      UserName: PChar;
      Password: PChar
      ): SQLResult; stdcall;
    function disconnect: SQLResult; stdcall;
    function getSQLCommand(
      out pComm: ISQLCommand
      ): SQLResult; stdcall;
    function getSQLMetaData(
      out pMetaData: ISQLMetaData
      ): SQLResult; stdcall;
    function SetOption(
      eConnectOption: TSQLConnectionOption;
      lValue: LongInt
      ): SQLResult; stdcall;
    function GetOption(
      eDOption: TSQLConnectionOption
      ; PropValue: Pointer;
      MaxLength: SmallInt;
      out Length: SmallInt
      ): SQLResult; stdcall;
    function beginTransaction(
      TranID: LongWord
      ): SQLResult; stdcall;
    function commit(
      TranID: LongWord
      ): SQLResult; stdcall;
    function rollback(
      TranID: LongWord
      ): SQLResult; stdcall;
    function getErrorMessage(
      Error: PChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: SmallInt
      ): SQLResult; stdcall;
    { end ISQLConnection methods }
  public
    constructor Create(OwnerDbxDriver: TSqlDriverOdbc);
    destructor Destroy; override;
  end;


{ TSqlCommandOdbc implements ISQLCommand }

  TSqlCommandOdbc = class(TInterfacedObject, ISQLCommand)
  private
    fCommandErrorLines: TStringList;
    fOwnerDbxConnection: TSqlConnectionOdbc;
    fOwnerDbxDriver: TSqlDriverOdbc;
    fCommandBlobSizeLimitK: integer;
    fSql: string;  // fSQL is saved in prepare / executeImmediate
// Private fields below are specific to ODBC
    fhStmt: SQLHSTMT;
    fStmtFreed: boolean;
    fOdbcParamList: TList;
    procedure OdbcCheck(OdbcCode: SQLRETURN; OdbcFunctionName: string);
  protected
    { begin ISQLCommand methods }
    function SetOption(
      eSqlCommandOption: TSQLCommandOption;
      ulValue: Integer
      ): SQLResult; stdcall;
    function GetOption(
      eSqlCommandOption: TSQLCommandOption;
      var pValue: Integer;
      MaxLength: SmallInt;
      out Length: SmallInt
      ): SQLResult; stdcall;
    function setParameter(
      ulParameter: Word ;
      ulChildPos: Word ;
      eParamType: TSTMTParamType ;
      uLogType: Word;
      uSubType: Word;
      iPrecision: Integer;
      iScale: Integer;
      Length: LongWord ;
      pBuffer: Pointer;
      bIsNull: Integer
      ): SQLResult; stdcall;
    function getParameter(
      ParameterNumber: Word;
      ulChildPos: Word;
      Value: Pointer;
      Length: Integer;
      var IsBlank: Integer
      ): SQLResult; stdcall;
    function prepare(
      SQL: PChar;
      ParamCount: Word
      ): SQLResult; stdcall;
    function execute(
      var Cursor: ISQLCursor
      ): SQLResult; stdcall;
    function executeImmediate(
      SQL: PChar;
      var Cursor: ISQLCursor
      ): SQLResult; stdcall;
    function getNextCursor(
      var Cursor: ISQLCursor
      ): SQLResult; stdcall;
    function getRowsAffected(
      var Rows: LongWord
      ): SQLResult; stdcall;
    function close: SQLResult; stdcall;
    function getErrorMessage(
      Error: PChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: SmallInt
      ): SQLResult; stdcall;
    { end ISQLCommand methods }
  public
    constructor Create(OwnerDbxConnection: TSqlConnectionOdbc);
    destructor Destroy; override;
    property hOdbcStmt: SQLHSTMT read fhStmt;
  end;


{ TSQLMetaDataOdbc implements ISQLMetaData }

  TSQLMetaDataOdbc = class(TInterfacedObject, ISQLMetaData)
  private
    fOwnerDbxConnection: TSqlConnectionOdbc;
    fMetaDataErrorLines: TStringList;
  protected
    { begin ISQLMetaData methods }
    function SetOption(
      eDOption: TSQLMetaDataOption;
      PropValue: LongInt
      ): SQLResult; stdcall;
    function GetOption(
      eDOption: TSQLMetaDataOption;
      PropValue: Pointer;
      MaxLength: SmallInt;
      out Length: SmallInt
      ): SQLResult; stdcall;
    function getObjectList(
      eObjType: TSQLObjectType;
      out Cursor: ISQLCursor
      ): SQLResult; stdcall;
    function getTables(
      TableName: PChar;
      TableType: LongWord;
      out Cursor: ISQLCursor
      ): SQLResult; stdcall;
    function getProcedures(
      ProcedureName: PChar;
      ProcType: LongWord;
      out Cursor: ISQLCursor
      ): SQLResult; stdcall;
    function getColumns(
      TableName: PChar;
      ColumnName: PChar;
      ColType: LongWord;
      Out Cursor: ISQLCursor
      ): SQLResult; stdcall;
    function getProcedureParams(
      ProcName: PChar;
      ParamName: PChar;
      out Cursor: ISQLCursor
      ): SQLResult; stdcall;
    function getIndices(
      TableName: PChar;
      IndexType: LongWord;
      out Cursor: ISQLCursor
      ): SQLResult; stdcall;
    function getErrorMessage(
      Error: PChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen:
      SmallInt
      ): SQLResult; stdcall;
    { end ISQLMetaData methods }
  public
    constructor Create(OwnerDbxConnection: TSqlConnectionOdbc);
    destructor Destroy; override;
  end;


{ TSqlCursorOdbc implements ISQLCursor }

  TSqlCursorOdbc = class(TInterfacedObject, ISQLCursor)
  private
    fCursorErrorLines: TStringList;
    fOwnerCommand: TSqlCommandOdbc;
    fOwnerDbxConnection: TSqlConnectionOdbc;
    fOwnerDbxDriver: TSqlDriverOdbc;
    fRowNo: integer;
// Private fields below are specific to ODBC
    fhStmt: SQLHSTMT;
    fOdbcNumCols: SQLSMALLINT;
    fOdbcBindList: TList;
    procedure BindResultSet;
    procedure OdbcCheck(OdbcCode: SQLRETURN; OdbcFunctionName: string);
    procedure FetchLongData(OdbcColNo: SQLUSMALLINT);
    procedure FetchLateBoundData(OdbcColNo: SQLUSMALLINT);
  protected
    { begin ISQLCusror methods }
    function SetOption(
      eOption: TSQLCursorOption;
      PropValue: LongInt
      ): SQLResult; stdcall;
    function GetOption(
      eOption: TSQLCursorOption;
      PropValue: Pointer;
      MaxLength: SmallInt;
      out Length: SmallInt
      ): SQLResult; stdcall;
   function getErrorMessage(
     Error: PChar
     ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: SmallInt
      ): SQLResult; stdcall;
    function getColumnCount(
      var pColumns: Word
      ): SQLResult; stdcall;
    function getColumnNameLength(
       ColumnNumber: Word;
       var pLen: Word): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PChar
      ): SQLResult; stdcall;
    function getColumnType(
      ColumnNumber: Word;
      var puType: Word;
      var puSubType: Word
      ): SQLResult; stdcall;
    function getColumnLength(
      ColumnNumber: Word;
      var pLength: LongWord
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: SmallInt
      ): SQLResult; stdcall;
    function getColumnScale(
      ColumnNumber: Word;
      var piScale: SmallInt
      ): SQLResult; stdcall;
    function isNullable(
      ColumnNumber: Word;
      var Nullable: LongBool
      ): SQLResult; stdcall;
    function isAutoIncrement(
      ColumnNumber: Word;
      var AutoIncr: LongBool
      ): SQLResult; stdcall;
    function isReadOnly(
      ColumnNumber: Word;
      var ReadOnly: LongBool
      ): SQLResult; stdcall;
    function isSearchable(
      ColumnNumber: Word;
      var Searchable: LongBool
      ): SQLResult; stdcall;
    function isBlobSizeExact(
      ColumnNumber: Word;
      var IsExact: LongBool
      ): SQLResult; stdcall;
    function next: SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getShort(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getDouble(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBcd(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getTimeStamp(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getTime(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getDate(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBytes(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBlobSize(
      ColumnNumber: Word;
      var Length: LongWord;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBlob(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool;
      Length: LongWord): SQLResult; stdcall;
    { end ISQLCusror methods }
  public
    constructor Create(OwnerCommand: TSqlCommandOdbc);
    destructor Destroy; override;
  end;


  TSqlCursorMetaDataColumns = class; // forward declaration
  TSqlCursorMetaDataTables = class; // forward declaration
  TMetaIndexColumn = class; // forward declaration


{ TMetaTable - represents 1 row returned by ISQLMetaData.GetTables }

  TMetaTable = class(TObject)
  private
    fCat: pAnsiChar;
    fSchema: pAnsiChar;
    fTableName: pAnsiChar;
    fQualifiedTableName: pAnsiChar;
    fTableType: integer;

    fPrimaryKeyColumn1: TMetaIndexColumn;
    fIndexColumnList: TList;
  public
    constructor Create(
      SqlConnectionOdbc: TSqlConnectionOdbc;
      Cat: pAnsiChar;
      Schema: pAnsiChar;
      TableName: pAnsiChar;
      TableType: integer);
    destructor Destroy; override;
  end;


{ TMetaColumn - represents 1 row returned by ISQLMetaData.GetColumns }

  TMetaColumn = class(TObject)
  private
    fMetaTable: TMetaTable;
    fColumnName: pAnsiChar;
    fOrdinalPosition: smallint;
    fLength: integer;
    fTypeName: pAnsiChar;
    fPrecision: integer;
    fDecimalScale: smallint;
    fDbxType: smallint;
    fDbxSubType: smallint;
    fDbxNullable: smallint;
    fDbxColumnType: smallint;
  public
    constructor Create(
      ColumnName: pAnsiChar;
      OrdinalPosition: smallint;
      TypeName: pAnsiChar);
    destructor Destroy; override;
  end;


{ TMetaIndexColumn - represents 1 row returned by ISQLMetaData.GetIndices }

  TMetaIndexColumn = class(TObject)
  private
    fMetaTable: TMetaTable;
    fIndexName: pAnsiChar;
    fIndexColumnName: pAnsiChar;
//    fTypeName: pAnsiChar;
    fColumnPosition: smallint;
    fIndexType: smallint;
    fSortOrder: char;
    fFilter: pAnsiChar;
  public
    constructor Create(
      MetaTable: TMetaTable;
      IndexName: pAnsiChar;
      IndexColumnName: pAnsiChar);
    destructor Destroy; override;
  end;


{ TMetaProcedure - represents 1 row returned by ISQLMetaData.GetProcedures }

  TMetaProcedure = class(TObject)
  private
    fCat: pAnsiChar;
    fSchema: pAnsiChar;
    fProcName: pAnsiChar;
    fProcType: integer;
  public
    constructor Create(
      Cat: pAnsiChar;
      Schema: pAnsiChar;
      ProcName: pAnsiChar;
      ProcType: integer);
    destructor Destroy; override;
  end;


{ TMetaProcedureParam - represents 1 row returned by ISQLMetaData.GetProcedureParams }

  TMetaProcedureParam = class(TObject)
  private
    fMetaProcedure: TMetaProcedure;
    fParamName: pAnsiChar;
    fDataTypeName: pAnsiChar;
    fParamType: DBXpress.TSTMTParamType;
    fDataType: smallint;
    fDataSubtype: smallint;
    fPrecision: integer;
    fScale: smallint;
    fLength: integer;
    fNullable: smallint;
  public
    constructor Create(ParamName: pAnsiChar);
    destructor Destroy; override;
  end;


{ TColumnNames / TColumnTypes used by TSqlCursorMetaData}

  TColumnNames = array [0..MaxListSize] of string;
  TColumnTypes = array [0..MaxListSize] of word;
  PColumnNames = ^TColumnNames;
  PColumnTypes = ^TColumnTypes;


{ TSqlCursorMetaData - parent for all the MetaData cursor classes}

  TSqlCursorMetaData = class(TInterfacedObject, ISQLCursor)
  private
    fSqlCursorErrorMsg: TStringList;

    fOwnerMetaData: TSqlMetaDataOdbc;
    fSqlConnectionOdbc: TSqlConnectionOdbc;
    fSqlDriverOdbc: TSqlDriverOdbc;

    fhStmt: SQLHSTMT;

    fRowNo: integer;
    fColumnCount: integer;
    fColumnNames: PColumnNames;
    fColumnTypes: PColumnTypes;
    fMetaCatalogName: pChar;
    fMetaSchemaName: pChar;
    fMetaTableName: pChar;

    procedure OdbcCheck(OdbcCode: SQLRETURN; OdbcFunctionName: string);
    procedure ParseTableName(TableName: pChar);
    procedure DescribeAllocBindString(ColumnNo: SQLUSMALLINT; var BindString: pAnsiChar; var BindInd: SQLINTEGER);
    procedure BindInteger(ColumnNo: SQLUSMALLINT; var BindInteger: Integer;
      BindInd: PSQLINTEGER);
    procedure BindSmallint(ColumnNo: SQLUSMALLINT; var BindSmallint: Smallint;
      PBindInd: PSQLINTEGER);
  protected
    { begin ISQLCusror methods }
    function SetOption(
      eOption: TSQLCursorOption;
      PropValue: LongInt
      ): SQLResult; stdcall;
    function GetOption(
      eOption: TSQLCursorOption;
      PropValue: Pointer;
      MaxLength: SmallInt;
      out Length: SmallInt
      ): SQLResult; stdcall;
   function getErrorMessage(
     Error: PChar
     ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: SmallInt
      ): SQLResult; stdcall;
    function getColumnCount(
      var pColumns: Word
      ): SQLResult; stdcall;
    function getColumnNameLength(
       ColumnNumber: Word;
       var pLen: Word): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PChar
      ): SQLResult; stdcall;
    function getColumnType(
      ColumnNumber: Word;
      var puType: Word;
      var puSubType: Word
      ): SQLResult; stdcall;
    function  getColumnLength(
      ColumnNumber: Word;
      var pLength: LongWord
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: SmallInt
      ): SQLResult; stdcall;
    function getColumnScale(
      ColumnNumber: Word;
      var piScale: SmallInt
      ): SQLResult; stdcall;
    function isNullable(
      ColumnNumber: Word;
      var Nullable: LongBool
      ): SQLResult; stdcall;
    function isAutoIncrement(
      ColumnNumber: Word;
      var AutoIncr: LongBool
      ): SQLResult; stdcall;
    function isReadOnly(
      ColumnNumber: Word;
      var ReadOnly: LongBool
      ): SQLResult; stdcall;
    function isSearchable(
      ColumnNumber: Word;
      var Searchable: LongBool
      ): SQLResult; stdcall;
    function isBlobSizeExact(
      ColumnNumber: Word;
      var IsExact: LongBool
      ): SQLResult; stdcall;
    function next: SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getShort(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getDouble(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBcd(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getTimeStamp(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getTime(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getDate(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBytes(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBlobSize(
      ColumnNumber: Word;
      var Length: LongWord;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getBlob(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool;
      Length: LongWord): SQLResult; stdcall;
    { end ISQLCusror methods }
  public
    constructor Create(
      OwnerSqlMetaData: TSqlMetaDataOdbc);
    destructor Destroy; override;
  end;


{ TSqlCursorMetaDataTables - implements cursor returned by ISQLMetaData.GetTables }

  TSqlCursorMetaDataTables = class(TSQLCursorMetaData, ISQLCursor)
  private
    fTableList: TList;
    fMetaTableCurrent: TMetaTable;
    fCatLenMax: integer;
    fSchemaLenMax: integer;
    fQualifiedTableLenMax: integer;
    procedure FetchTables(SearchTableName: PChar;
      SearchTableType: LongWord);
  protected
    function  getColumnLength(
      ColumnNumber: Word;
      var pLength: LongWord
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: SmallInt
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function next: SQLResult; stdcall;
  public
    constructor Create(
      OwnerMetaData: TSQLMetaDataOdbc);
    destructor Destroy; override;
  end;


{ TSqlCursorMetaDataColumns - implements cursor returned by ISQLMetaData.GetColumns }

  TSqlCursorMetaDataColumns = class(TSQLCursorMetaData, ISQLCursor)
  private
    fTableList: TList;
    fColumnList: TList;

    fMetaTableCurrent: TMetaTable;
    fMetaColumnCurrent: TMetaColumn;

    fCatLenMax: integer;
    fSchemaLenMax: integer;
    fTableLenMax: integer;
    fColumnLenMax: integer;
    fTypeNameLenMax: integer;
    procedure FetchColumns(SearchTableName, SearchColumnName: PChar;
      SearchColType: LongWord);
  protected
    function  getColumnLength(
      ColumnNumber: Word;
      var pLength: LongWord
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: SmallInt
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getShort(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function next: SQLResult; stdcall;
  public
    constructor Create(
      OwnerMetaData: TSQLMetaDataOdbc);
    destructor Destroy; override;
  end;


{ TSqlCursorMetaDataIndexes - implements cursor returned by ISQLMetaData.GetIndices }

  TSqlCursorMetaDataIndexes = class(TSQLCursorMetaData, ISQLCursor)
  private
    fIndexList: TList;
    fTableList: TList;
    fCurrentIndexColumn: TMetaIndexColumn;

    fCatLenMax: integer;
    fSchemaLenMax: integer;
    fTableLenMax: integer;
    fIndexNameLenMax: integer;
    fIndexColumnNameLenMax: integer;
    fPkNameLenMax: integer;
    fFilterLenMax: integer;
    procedure FetchIndexes(SearchTableName: PChar;
      SearchIndexType: LongWord);
  protected
    function  getColumnLength(
      ColumnNumber: Word;
      var pLength: LongWord
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: SmallInt
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getShort(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
   function next: SQLResult; stdcall;
  public
    constructor Create(
      OwnerMetaData: TSQLMetaDataOdbc);
    destructor Destroy; override;
  end;


{ TSqlCursorMetaDataProcedures - implements cursor returned by ISQLMetaData.GetProcedures }

  TSqlCursorMetaDataProcedures = class(TSQLCursorMetaData, ISQLCursor)
  private
    fProcList: TList;
    fCatLenMax: integer;
    fSchemaLenMax: integer;
    fProcLenMax: integer;
    fMetaProcedureCurrent: TMetaProcedure;
    procedure FetchProcedures(ProcedureName: PChar; ProcType: LongWord);
  protected
    function  getColumnLength(
      ColumnNumber: Word;
      var pLength: LongWord
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: SmallInt
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function next: SQLResult; stdcall;
  public
    constructor Create(OwnerMetaData: TSQLMetaDataOdbc);
    destructor Destroy; override;
  end;


{ TSqlCursorMetaDataProcedureParams - implements cursor returned by ISQLMetaData.GetProcedureParams }

  TSqlCursorMetaDataProcedureParams = class(TSQLCursorMetaData, ISQLCursor)
  private
    fProcList: TList;
    fProcColumnList: TList;

    fCatLenMax: integer;
    fSchemaLenMax: integer;
    fProcNameLenMax: integer;
    fParamNameLenMax: integer;
    fDataTypeNameLenMax: integer;

    fMetaProcedureParamCurrent: TMetaProcedureParam;
    procedure FetchProcedureParams(SearchProcedureName,
      SearchParamName: PChar);
  protected
    function  getColumnLength(
      ColumnNumber: Word;
      var pLength: LongWord
      ): SQLResult; stdcall;
    function getColumnPrecision(
      ColumnNumber: Word;
      var piPrecision: SmallInt
      ): SQLResult; stdcall;
    function getLong(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getShort(
      ColumnNumber: Word;
      Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function next: SQLResult; stdcall;
  public
    constructor Create(OwnerMetaData: TSQLMetaDataOdbc);
    destructor Destroy; override;
  end;

  TOdbcBindCol = class(TObject)
  private
    fColName: pAnsiChar;
    fColNameSize: SQLSMALLINT;
    fSqlType: SQLSMALLINT;
    fColSize: SQLUINTEGER;
    fColScale: SQLSMALLINT;
    fNullable: SQLSMALLINT;
    fColValueSize: integer;
    fDbxType: Word;
    fDbxSubType: Word;
    fOdbcHostVarType: SQLSMALLINT;
    fOdbcHostVarSize: SQLUINTEGER;
    fOdbcHostVarAddress: pointer;
    fOdbcLateBound: boolean;
    // pointer to value
    fpBuffer: pointer;
    // value directly stored in this structure
    fValue: packed record
    case SmallInt of
      SQL_C_CHAR:      (OdbcColValueString:     array[0..255] of char);
      SQL_C_LONG:      (OdbcColValueInteger:    Longint);
      SQL_C_SHORT:     (OdbcColValueShort:      Smallint);
      SQL_C_DOUBLE:    (OdbcColValueDouble:     Double);
      SQL_C_DATE:      (OdbcColValueDate:       TSqlDateStruct);
      SQL_C_TIME:      (OdbcColValueTime:       TSqlTimeStruct);
      SQL_C_TIMESTAMP: (OdbcColValueTimeStamp:  TOdbcTimeStamp);
      SQL_C_BIT:       (OdbcColValueBit:        Byte);
      SQL_C_SBIGINT:   (OdbcParamValueInt64:    Int64);
      end;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TOdbcBindParam = class(TObject)
  private
    fOdbcParamCType:    SQLSMALLINT;
    fOdbcParamSqlType:  SQLSMALLINT;
    fOdbcParamCbColDef: SQLUINTEGER;
    fOdbcParamIbScale:  SQLSMALLINT;
    fOdbcParamLenOrInd: SQLINTEGER;
    fBuffer: pointer;
    fValue: packed record
    case SmallInt of
      SQL_C_CHAR:      (OdbcParamValueString:     array[0..255] of char);
      SQL_C_LONG:      (OdbcParamValueInteger:    Longint);
      SQL_C_SHORT:     (OdbcParamValueShort:      Smallint);
      SQL_C_DOUBLE:    (OdbcParamValueDouble:     Double);
      SQL_C_DATE:      (OdbcParamValueDate:       TSqlDateStruct);
      SQL_C_TIME:      (OdbcParamValueTime:       TSqlTimeStruct);
      SQL_C_TIMESTAMP: (OdbcParamValueTimeStamp:  TOdbcTimeStamp);
      SQL_C_BIT:       (OdbcParamValueBit:        Byte);
      SQL_C_SBIGINT:   (OdbcParamValueInt64:      Int64);
    end;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, // The only reason this is needed is to supply Window handle for SqlDriverConect
  SqlExpr, // This is needed only for "RegisterDbXpressLib" for static linking of driver (Windows only)
{$ENDIF}
  SysUtils,
  FmtBcd,
  SqlTimst;

type
  EDbxOdbcWarning = class(Exception);

  EDbxError = class(Exception);      // The 4 Exceptions below descendents of this
  EDbxOdbcError = class(EDbxError);     // Odbc returned error result code
  EDbxNotSupported = class(EDbxError);  // Feature not yet implemented
  EDbxInvalidCall = class(EDbxError);   // Invalid function call or function parameter
  EDbxInternalError = class(EDbxError); // Other error

{ Public function getSQLDriverODBC is the starting point for everything else... }

function getSQLDriverODBC(sVendorLib : PChar; sResourceFile : PChar; out Obj): SQLResult; stdcall;
begin
try
  if not LoadOdbcDriverManager(sVendorLib) then
    raise EDbxError.Create('Unable to load specified Odbc Driver manager DLL: ''' + sVendorLib + '''');
  ISQLDriver(Obj) := TSqlDriverOdbc.Create;
  Result := DBXpress.SQL_SUCCESS;
except
on EDbxError do
  Result := DBXpress.MaxReservedStaticErrors + 1;
end;
end;

{ Private utility functions... }

procedure MaxSet(var x: integer; n: integer);
  begin
  if x < n then
    x := n;
  end;
  
function strCompNil(const Str1, Str2 : PChar): Integer;
  begin
  if (Str1 = nil) or (str2 = nil) then
    strCompNil := 0
  else
    strCompNil := strComp(str1, str2);
  end;

procedure OdbcDataTypeToDbxType(aSqlType: smallint; var DbxType: smallint; var DbxSubType: smallint);
  begin
  DbxSubType := 0;
  case aSqlType of
    SQL_INTEGER:
      DbxType := fldINT32;
    SQL_BIGINT:
// DbExpress does NOT currently support INT64
//      DbxType := fldINT64;
      DbxType := fldINT32;
    SQL_SMALLINT, SQL_TINYINT:
      DbxType := fldINT16;
    SQL_BIT:
      DbxType:= fldBOOL;
    SQL_NUMERIC, SQL_DECIMAL:
      DbxType := fldBCD;
    SQL_DOUBLE, SQL_FLOAT, SQL_REAL:
      DbxType := fldFLOAT;
    SQL_CHAR, SQL_WCHAR, SQL_GUID:
      begin
      DbxType := fldZSTRING;
      DbxSubType:= fldstFIXED;
      end;
    SQL_VARCHAR, SQL_WVARCHAR:
      DbxType := fldZSTRING;
    SQL_BINARY, SQL_VARBINARY:
      DbxType := fldBYTES;
    SQL_TYPE_DATE:
      DbxType:= fldDATE;
    SQL_TYPE_TIME:
      DbxType := fldTIME;
    SQL_TYPE_TIMESTAMP, SQL_DATETIME, SQL_TIMESTAMP:
      DbxType := fldDATETIME;
    SQL_LONGVARCHAR, SQL_WLONGVARCHAR:
      begin
      DbxType:= fldBLOB;
      DbxSubType:= fldstMEMO;
      end;
    SQL_LONGVARBINARY:
      begin
      DbxType:= fldBLOB;
      DbxSubType:= fldstBINARY;
      end;
    else
      begin
      DbxType:= fldUNKNOWN;
      raise EDbxInternalError.Create('Unsupported ODBC data type ' + IntToStr(aSqlType));
      end;
    end;
  end;

{ TSqlDriverOdbc }

constructor TSqlDriverOdbc.Create;
begin
  inherited Create;
  fOdbcErrorLines := TStringList.Create;
  fErrorLines := TStringList.Create;
  fSqlStateChars := '00000' + #0;
  fSqlState := @fSqlStateChars;
  AllocHEnv;
end;

destructor TSqlDriverOdbc.Destroy;
begin
  FreeHEnv;
  fErrorLines.Free;
  fOdbcErrorLines.Free;
  inherited;
end;

procedure TSqlDriverOdbc.OdbcCheck(
 CheckCode: SQLRETURN;
 OdbcFunctionName: string;
 HandleType: Smallint;
 Handle: SQLHANDLE);
begin
  case CheckCode of
  OdbcApi.SQL_SUCCESS:
    exit;
  OdbcApi.SQL_SUCCESS_WITH_INFO:
    begin
    try
    fOdbcErrorLines.Clear;
    fOdbcErrorLines.Add('SQL_SUCCESS_WITH_INFO returned from ODBC function ' + OdbcFunctionName);
    RetrieveOdbcErrorInfo(CheckCode, HandleType, Handle);
    raise EDbxODBCWarning.Create(fOdbcErrorLines.Text);
    except
      on EDbxOdbcWarning do
        fOdbcErrorLines.Clear; // Clear the error - warning only
    end
    end;
  OdbcApi.SQL_NO_DATA:
    begin
    fOdbcErrorLines.Clear;
    fOdbcErrorLines.Add('Unexpected end of data returned from ODBC function: ' + OdbcFunctionName);
    raise EDbxODBCError.Create(fOdbcErrorLines.Text);
    end;
  else
    begin
    fOdbcErrorLines.Clear;
    fOdbcErrorLines.Add('Error returned from ODBC function ' + OdbcFunctionName);
    RetrieveOdbcErrorInfo(CheckCode, HandleType, Handle);
    raise EDbxOdbcError.Create(fOdbcErrorLines.Text);
    end;
  end;
end;

procedure TSqlDriverOdbc.RetrieveOdbcErrorInfo(
 CheckCode: SQLRETURN;
 HandleType: Smallint;
 Handle: SQLHANDLE);

var
  CheckCodeText: string;
  GetDiagRetCode: SQLRETURN;
  GetDiagRecNumber: smallint;
  SqlStateChars: TSqlState; // 5 chars long + null terminator
  SqlState: PSqlState;
  NativeError: SQLINTEGER;
  pMessageText: PAnsiChar;
  BufferLengthRet: SQLSMALLINT;

begin

  fNativeErrorCode := 0;
  fSqlState := '00000' + #0;

  case CheckCode of
    OdbcApi.SQL_SUCCESS:   CheckCodeText := 'SQL_SUCCESS';
    SQL_SUCCESS_WITH_INFO: CheckCodeText := 'SQL_SUCCESS_WITH_INFO';
    SQL_NO_DATA:           CheckCodeText := 'SQL_NO_DATA';
    SQL_ERROR:             CheckCodeText := 'SQL_ERROR';
    SQL_INVALID_HANDLE:    CheckCodeText := 'SQL_INVALID_HANDLE';
    SQL_STILL_EXECUTING:   CheckCodeText := 'SQL_STILL_EXECUTING';
    SQL_NEED_DATA:         CheckCodeText := 'SQL_NEED_DATA';
  else                     CheckCodeText := 'Unknown Error code';
  end;

  fOdbcErrorLines.Add('ODBC Return Code: ' +
   IntToStr(CheckCode) + ': ' + CheckCodeText);

  pMessageText := AllocMem(SQL_MAX_MESSAGE_LENGTH + 2);

  SqlState := @SqlStateChars;
  GetDiagRecNumber := 1;
  GetDiagRetCode := SQLGetDiagRec(HandleType, Handle, GetDiagRecNumber,
    Sqlstate, NativeError, pMessageText, SQL_MAX_MESSAGE_LENGTH, BufferLengthRet);

  if GetDiagRetCode = 0 then
    begin
    // The most significant SqlState is always the FIRST record:
    fSqlStateChars := SqlStateChars;
    end
  else
    fOdbcErrorLines.Add('No ODBC diagnostic info available');

  while (GetDiagRetCode = 0) do
    begin
    fOdbcErrorLines.Add('');
    fOdbcErrorLines.Add('ODBC SqlState:        ' + Sqlstate);

    if (NativeError <> 0) then
      begin
      fOdbcErrorLines.Add('Native Error Code:    ' + IntToStr(NativeError));
      if (fNativeErrorCode <> 0) then
        fNativeErrorCode := NativeError;
      end;

    fOdbcErrorLines.Add(pMessageText);

    inc(GetDiagRecNumber);
    GetDiagRetCode := SQLGetDiagRec(HandleType, Handle, GetDiagRecNumber,
      Sqlstate, NativeError, pMessageText, SQL_MAX_MESSAGE_LENGTH, BufferLengthRet);
    end;

  FreeMem(pMessageText);
end;

procedure TSqlDriverOdbc.AllocHCon(out HCon: SQLHDBC);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
  OdbcRetcode := SQLAllocHandle(SQL_HANDLE_DBC, fhEnv, HCon);
  OdbcCheck(OdbcRetcode, 'SQLAllocHandle(SQL_HANDLE_DBC)', SQL_HANDLE_ENV, fhEnv);
end;

procedure TSqlDriverOdbc.FreeHCon(HCon: SQLHDBC);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
  OdbcRetCode := SQLFreeHandle(SQL_HANDLE_DBC, HCon);
  OdbcCheck(OdbcRetCode, 'SQLFreeHandle(SQL_HANDLE_DBC)', SQL_HANDLE_DBC, HCon);
end;

procedure TSqlDriverOdbc.AllocHEnv;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
  OdbcRetCode := SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, fhEnv);
  OdbcCheck(OdbcRetCode, 'SQLAllocHandle(SQL_HANDLE_ENV)', SQL_HANDLE_ENV, fhEnv);

  // This specifies ODBC version 3 (called before SQLConnect)
  OdbcRetCode := SQLSetEnvAttr(fhEnv, SQL_ATTR_ODBC_VERSION, pointer(SQL_OV_ODBC3), 0);
  OdbcCheck(OdbcRetCode, 'SQLSetEnvAttr(SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3)', SQL_HANDLE_ENV, fhEnv);
end;

procedure TSqlDriverOdbc.FreeHEnv;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
  OdbcRetCode := SQLFreeHandle(SQL_HANDLE_ENV, fhEnv);
  OdbcCheck(OdbcRetCode, 'SQLFreeHandle(SQL_HANDLE_ENV)', SQL_HANDLE_ENV, fhEnv);
end;

function TSqlDriverOdbc.GetOption(eDOption: TSQLDriverOption;
  PropValue: Pointer; MaxLength: SmallInt;
  out Length: SmallInt): SQLResult;
begin
try
  case eDOption of
    eDrvBlobSize:
      LongInt(PropValue) := fDrvBlobSizeLimitK;
    eDrvCallBack:
      { TODO : IMPLEMENT TRACING SUPPORT - Just save trace callback function for now }
      TSQLCallbackEvent(PropValue) := fSQLCallbackEvent;
    eDrvCallBackInfo:
      LongInt(PropValue) := fDbxOptionDrvCallBackInfo;
    eDrvRestrict:
      LongWord(PropValue) := fDbxOptionDrvRestrict;
    else
      raise EDbxInvalidCall.Create('Invalid option passed to TSqlDriverOdbc.GetOption');
  end;
  Result := SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlDriverOdbc.getSQLConnection(
  out pConn: ISQLConnection
  ): SQLResult;
begin
try
  pConn := TSqlConnectionOdbc.Create(self);
  Result := SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlDriverOdbc.SetOption(
  eDOption: TSQLDriverOption;
  PropValue: LongInt
  ): SQLResult;
begin
try
  case eDOption of
   eDrvBlobSize:
      fDrvBlobSizeLimitK := PropValue;
   eDrvCallBack:
     fSQLCallbackEvent := TSQLCallbackEvent(PropValue);
   eDrvCallBackInfo:
     fDbxOptionDrvCallBackInfo := PropValue;
   eDrvRestrict:
     fDbxOptionDrvRestrict := LongWord(PropValue);
   else
     raise EDbxInvalidCall.Create('Invalid option passed to TSqlDriverOdbc.SetOption');
  end;
  Result := SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;


{ TSqlConnectionOdbc }

constructor TSqlConnectionOdbc.Create(OwnerDbxDriver: TSqlDriverOdbc);
begin
  inherited Create;
  fConnectionErrorLines := TStringList.Create;
  fConnected := false;
  fOwnerDbxDriver := OwnerDbxDriver;
  fOwnerDbxDriver.AllocHCon(fhCon);
  fWantQuotedTableName := true;
  fConnBlobSizeLimitK := fOwnerDbxDriver.fDrvBlobSizeLimitK;
end;

destructor TSqlConnectionOdbc.Destroy;
begin
  disconnect;
  fOwnerDbxDriver.FreeHCon(fhCon);
  fConnectionErrorLines.Free;
  inherited;
end;

procedure TSqlConnectionOdbc.AllocHStmt(out HStmt: SQLHSTMT);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
  OdbcRetCode := SQLAllocHandle(SQL_HANDLE_STMT, fhCon, HStmt);
  fOwnerDbxDriver.OdbcCheck(OdbcRetCode, 'SQLAllocHandle(SQL_HANDLE_STMT)',
  SQL_HANDLE_STMT, fhCon);
end;

procedure TSqlConnectionOdbc.FreeHStmt(HStmt: SQLHSTMT);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
  OdbcRetCode := SQLFreeHandle(SQL_HANDLE_STMT, HStmt);
  OdbcCheck(OdbcRetCode, 'SQLFreeHandle(SQL_HANDLE_STMT)');
end;

procedure TSqlConnectionOdbc.OdbcCheck(OdbcCode: SQLRETURN; OdbcFunctionName: string);
begin
  fOwnerDbxDriver.OdbcCheck(OdbcCode, OdbcFunctionName, SQL_HANDLE_DBC, fhCon);
end;

procedure TSqlConnectionOdbc.GetCurrentCatalog;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  aCurrentCatalogLen: SQLINTEGER;
begin
  if fSupportsCatalog then
    begin
    if fCurrentCatalog = nil then
      fCurrentCatalog := AllocMem(fOdbcMaxCatalogNameLen + 1);
    OdbcRetCode := SQLGetConnectAttr(
      fhCon,
      SQL_ATTR_CURRENT_CATALOG,
      fCurrentCatalog,
      fOdbcMaxCatalogNameLen,
      @aCurrentCatalogLen);
    OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(SQL_ATTR_CURRENT_CATALOG)');
    end;
end;

function TSqlConnectionOdbc.beginTransaction(
  TranID: LongWord): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
{
  Transactions in ODBC are not explicitly initiated.
  But we must make sure we are in Manual Commit Mode.
  Also, if a statement is executed after the transation has been committed,
  without another call to beginTransaction, we must go back to Auto Commit mode
  (see procedure TransactionCheck)
}
try
  if (fAutoCommitMode = SQL_AUTOCOMMIT_ON) then
    begin
    OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT, pointer(smallint(SQL_AUTOCOMMIT_OFF)), 0);
    OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF)');
    fAutoCommitMode := SQL_AUTOCOMMIT_OFF;
    end;
  fInTransaction := true;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fConnectionErrorLines.Add(E.Message);
    fConnectionErrorLines.Add('Connection string: ' + fOdbcConnectStringHidePassword);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlConnectionOdbc.commit(TranID: LongWord): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
try
  OdbcRetCode := SQLEndTran(SQL_HANDLE_DBC, fhCon, SQL_COMMIT);
  OdbcCheck(OdbcRetCode, 'SQLEndTran');
  fInTransaction := false;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fConnectionErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlConnectionOdbc.connect(
  ServerName: PChar;
  UserName: PChar;
  Password: PChar
  ): SQLResult;

// ServerName is either the ODBC DSN name (must be already set up in ODBC admin)
// or it is the complete ODBC connect string (to allow complete flexibility)
// In this case, the UserName and Password are passed only if they are not blank
// (Because you might want to specifiy the UID and PWD in the FileDsn, for example)
//
// Example using DSN name
//   ServerName: 'ODBCDSN'
//   UserName:   'USER'
//   Password:   'SECRET'
//
// Examples using ODBC connect string:
//   ServerName: 'DSN=Example;UID=USER;PWD=SECRET'
//   ServerName: 'DSN=Example;DB=MyDB;HOSTNAME=MyHost;TIMEOUT=10;UID=USER;PWD=SECRET''
//   ServerName: 'FILEDSN=FileDsnExample'
//   ServerName: 'DRIVER=Microsoft Access Driver (*.mdb);DBQ=c:\work\odbctest\odbctest.mdb'
//

function HidePassword(ConnectString: string): string;
  var
    p: integer;
  begin
  p := Pos('PWD=', UpperCase(ConnectString));
  if p > 0 then
    p := p + 4
  else
    begin
    p := Pos('PASSWORD=', UpperCase(ConnectString));
    if p > 0 then
      p := p + 9
    end;

  if (p > 0) then
    begin
    Result := Copy(ConnectString, 1, p-1) + '***';
    while (p <= length(ConnectString)) and (ConnectString[p] <> ';') do
      inc(p);
    Result := Result + Copy(ConnectString, p, Length(ConnectString) - p + 1);
    end
  else
    Result := ConnectString;
  end;

const
  OdbcReturnedConnectStringMax = 1024;

// IBM DB2 extensions to ODBC API
  SQL_LONGDATA_COMPAT   = 1253;
  SQL_LD_COMPAT_NO      = 0;
  SQL_LD_COMPAT_YES     = 1;

var
  OdbcRetCode: OdbcApi.SQLRETURN;
{$ifdef MSWINDOWS} 
  ParentWindowHandle: HWND; 
{$else} 
  ParentWindowHandle: integer; 
{$endif} 
  cbConnStrOut: SQLSMALLINT;
  FunctionSupported: SQLUSMALLINT;
  aBuffer: array[0..1] of char;
  StringLength: SQLSMALLINT;
  p: integer;
  Len: smallint;
  aOdbcSchemaUsage: SQLUINTEGER;
  aOdbcCatalogUsage: SQLUINTEGER;
  aOdbcGetDataExtensions:SQLUINTEGER;
begin
  Result := DBXpress.SQL_SUCCESS;
  if fConnected then exit;

try
  if Pos('=', ServerName) = 0 then
    begin
    // No '=' in connect string: its a normal Connect string
    if ((strLen(ServerName) = 0) or (ServerName[0] = '?')) then
    else
      fOdbcConnectString := 'DSN=' + ServerName + ';';
    if (UserName[0] <> #0) then
      fOdbcConnectString := fOdbcConnectString + 'UID=' + UserName + ';';
    fOdbcConnectStringHidePassword := fOdbcConnectString;
    if (UserName[0] <> #0) then
      begin
      fOdbcConnectString := fOdbcConnectString + 'PWD=' + Password + ';';
      fOdbcConnectStringHidePassword := fOdbcConnectStringHidePassword + 'PWD=***;';
      end;
    end
  else
    begin
    // '=' in connect string: its a Custom ODBC Connect string
    fOdbcConnectString := ServerName;

    // Check to see if User Id already specified in connect string -
    if (Pos('UID=', UpperCase(ServerName)) = 0) and
       (Pos('USERID=', UpperCase(ServerName)) = 0) and
       (UserName[0] <> #0) then
      fOdbcConnectString := fOdbcConnectString + ';UID=' + UserName;

    // Check to see if Password already specified in connect string -
    if (Pos('PWD=', UpperCase(ServerName)) = 0) and
       (Pos('PASSWORD=', UpperCase(ServerName)) = 0) and
       (Password[0] <> #0) then
      fOdbcConnectString := fOdbcConnectString + ';PWD=' + Password;

    fOdbcConnectStringHidePassword := HidePassword(fOdbcConnectString);

    end;

  fOdbcReturnedConnectString := AllocMem(OdbcReturnedConnectStringMax);

{$IFDEF MSWINDOWS}
   ParentWindowHandle := Windows.GetActiveWindow;
{$ELSE}
   ParentWindowHandle := 0;
{$ENDIF}

  if ((strLen(ServerName) = 0) or (ServerName[0] = '?')) then
    begin
    OdbcRetCode := SQLDriverConnect(
     fhCon,
     ParentWindowHandle,
     pAnsiChar(fOdbcConnectString), SQL_NTS,
     fOdbcReturnedConnectString, OdbcReturnedConnectStringMax, cbConnStrOut,
  //   SQL_DRIVER_NOPROMPT);
  //   SQL_DRIVER_PROMPT);
     SQL_DRIVER_COMPLETE_REQUIRED);
  //   SQL_DRIVER_COMPLETE);
    if (OdbcRetCode = OdbcApi.SQL_NO_DATA) then
      begin
      Result := DBXpress.DBXERR_INVALIDUSRPASS;
      exit;  // User Clicked Cancel
      end;
    OdbcCheck(OdbcRetCode, 'SQLDriverConnect (Driver Complete Required)');
    fOdbcConnectString := fOdbcReturnedConnectString;
    fOdbcConnectStringHidePassword := HidePassword(fOdbcReturnedConnectString);
    fConnected := true;
    end
  else
    begin
    OdbcRetCode := SQLDriverConnect(
     fhCon,
     ParentWindowHandle,
     pAnsiChar(fOdbcConnectString), SQL_NTS,
     fOdbcReturnedConnectString, OdbcReturnedConnectStringMax, cbConnStrOut,
     SQL_DRIVER_NOPROMPT);
  //   SQL_DRIVER_PROMPT);
  //   SQL_DRIVER_COMPLETE_REQUIRED);
  //   SQL_DRIVER_COMPLETE);
    OdbcCheck(OdbcRetCode, 'SQLDriverConnect (NoPrompt)');
    fConnected := true;
    end;

  fOdbcDriverLevel := 3; // Assume its a level 3 driver
  ReallocMem(fOdbcReturnedConnectString, cbConnStrOut+1);

  RetrieveDriverName;

{  OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_METADATA_ID, pointer(SQL_TRUE), 0);
}
  if (fOdbcDriverType = eOdbcDriverTypeMySql) then
    fSupportsCatalog := false
  else
    begin
    OdbcRetCode := SQLGetInfoString(fhCon, SQL_CATALOG_NAME, @aBuffer, sizeof(aBuffer), StringLength);
    OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_CATALOG_NAME)');
    fSupportsCatalog := (aBuffer[0] = 'Y');
    end;

  // IBM DB2 has driver-specific longdata type, but setting this option makes it ODBC compatible:
  if self.fOdbcDriverType = EOdbcDriverTypeIbmDb2 then
    begin
    OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_LONGDATA_COMPAT, SQLPOINTER(SQL_LD_COMPAT_YES), 0);
    OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(SQL_LONGDATA_COMPAT)');
    end;

  OdbcRetCode := SQLGetInfoInt(fhCon, SQL_SCHEMA_USAGE, aOdbcSchemaUsage,
   SizeOf(aOdbcSchemaUsage), nil);
  if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
    aOdbcSchemaUsage := 0;
  fSupportsSchemaDML := ((aOdbcSchemaUsage and SQL_SU_DML_STATEMENTS) <> 0);
  fSupportsSchemaProc := ((aOdbcSchemaUsage and SQL_SU_PROCEDURE_INVOCATION) <> 0);

  OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_COLUMN_NAME_LEN, fOdbcMaxColumnNameLen,
   SizeOf(fOdbcMaxColumnNameLen), nil);
  if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
    fOdbcMaxColumnNameLen := 64;
  OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_TABLE_NAME_LEN, fOdbcMaxTableNameLen,
   SizeOf(fOdbcMaxTableNameLen), nil);
  if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
    fOdbcMaxTableNameLen := 128;
  if fSupportsCatalog then
    begin
    OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_CATALOG_NAME_LEN, fOdbcMaxCatalogNameLen,
     SizeOf(fOdbcMaxCatalogNameLen), nil);
    if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
      fSupportsCatalog := false;

    OdbcRetCode := SQLGetInfoInt(fhCon, SQL_CATALOG_USAGE, aOdbcCatalogUsage,
     SizeOf(aOdbcCatalogUsage), nil);
    if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
      aOdbcCatalogUsage := 0;
    fSupportsCatalogDML := ((aOdbcCatalogUsage and SQL_CU_DML_STATEMENTS ) <> 0);
    fSupportsCatalogProc := ((aOdbcCatalogUsage and SQL_CU_PROCEDURE_INVOCATION) <> 0);
    end;

  OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_SCHEMA_NAME_LEN, fOdbcMaxSchemaNameLen,
   SizeOf(fOdbcMaxSchemaNameLen), nil);
  if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
    fOdbcMaxSchemaNameLen := 0;
  OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_IDENTIFIER_LEN, fOdbcMaxIdentifierLen,
   SizeOf(fOdbcMaxIdentifierLen), nil);
  if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
    fOdbcMaxSchemaNameLen := 128;

  OdbcRetCode := SQLGetFunctions(fhCon, SQL_API_SQLSTATISTICS, FunctionSupported);
  OdbcCheck(OdbcRetCode, 'SQLGetFunctions(SQL_API_SQLSTATISTICS)');
  fSupportsSQLSTATISTICS := (FunctionSupported = OdbcApi.SQL_TRUE);
  OdbcRetCode := SQLGetFunctions(fhCon, SQL_API_SQLPRIMARYKEYS, FunctionSupported);
  OdbcCheck(OdbcRetCode, 'SQLGetFunctions(SQL_API_SQLPRIMARYKEYS)');
  fSupportsSQLPRIMARYKEYS := (FunctionSupported = OdbcApi.SQL_TRUE);

  OdbcRetCode := SQLGetInfoInt(fhCon, SQL_CATALOG_USAGE, aOdbcCatalogUsage,
  SizeOf(aOdbcCatalogUsage), nil);
  if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
    aOdbcCatalogUsage := 0;

  OdbcRetCode := SQLGetInfoInt(fhCon, SQL_GETDATA_EXTENSIONS, aOdbcGetDataExtensions,
   SizeOf(aOdbcSchemaUsage), nil);
  OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_GETDATA_EXTENSIONS');
  fGetDataAnyOrder := ((aOdbcGetDataExtensions and SQL_GD_ANY_COLUMN) <> 0);

  GetMetaDataOption(eMetaObjectQuoteChar, @fQuoteChar, 1, Len);

  OdbcRetCode := SQLGetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT, @fAutoCommitMode, 0, nil);
  OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(SQL_ATTR_AUTOCOMMIT)');

  Result := DBXpress.SQL_SUCCESS;

except
  on E: EDbxError do
    begin
    fConnectionErrorLines.Add(E.Message);
    fConnectionErrorLines.Add('Connection string: ' + fOdbcConnectStringHidePassword);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlConnectionOdbc.disconnect: SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
try
  if NOT fConnected then
    begin
    Result := SQL_SUCCESS;
    exit;
    end;
  OdbcRetCode := SQLDisconnect(fhCon);
  OdbcCheck(OdbcRetCode, 'SQLDisconnect');
  fConnected := false;
  FreeMem(fOdbcReturnedConnectString);
  fOdbcDriverName := '';
  fOdbcDriverType := eOdbcDriverTypeUnspecified;
  if (fCurrentCatalog <> nil) then
    begin
    FreeMem(fCurrentCatalog);
    fCurrentCatalog := nil
    end;
  Result := SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fConnectionErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlConnectionOdbc.getErrorMessage(Error: PChar): SQLResult;
begin
  StrCopy(Error, PChar(fConnectionErrorLines.Text));
  fConnectionErrorLines.Clear;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlConnectionOdbc.getErrorMessageLen(
  out ErrorLen: SmallInt): SQLResult;
begin
  ErrorLen := Length(fConnectionErrorLines.Text);
  Result := DBXpress.SQL_SUCCESS;
end;

function TSQLConnectionOdbc.GetMetaDataOption(eDOption: TSQLMetaDataOption;
  PropValue: Pointer; MaxLength: SmallInt;
  out Length: SmallInt): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  GetInfoStringBuffer: array[0..1] of AnsiChar;
  GetInfoSmallInt: SQLUSMALLINT;
  ConnectAttrLength: SQLINTEGER;
  MaxIdentifierLen: SQLUSMALLINT;
  MaxColumnNameLen: SQLUSMALLINT;
  MaxTableNameLen: SQLUSMALLINT;
  MaxObjectNameLen: integer;
  BatchSupport: SQLUINTEGER;
begin

// Note on calls to SQLGetInfo -
//
// ODBC API specification states that where returned value is of type SQLUSMALLINT,
// the driver ignores the length parameter (ie assumes length of 2)
// However, Centura driver REQUIRES length parameter, even for SQLUSMALLINT value;
// If ommitted, Centura driver returns SQL_SUCCESS_WITH_INFO - Data Truncated,
// and does not return the data.
// So I have had to code the length parameter for all SQLGetInfo calls.
// Never mind, compliant ODBC driver will just ignore the length parameter...

try
  case eDOption of
    eMetaCatalogName: // Dbx Read/Write
      begin
      OdbcRetCode := SQLGetConnectAttr(fhCon, SQL_ATTR_CURRENT_CATALOG,
       PAnsiChar(PropValue), MaxLength, @ConnectAttrLength);
      OdbcCheck (OdbcRetCode, 'SQLGetConnectAttr(SQL_ATTR_CURRENT_CATALOG)');
      Length := ConnectAttrLength;
      end;
    eMetaSchemaName: // Dbx Read/Write
      begin
      // There is no ODBC function to get this
      Char(PropValue^) := #0;
      Length := 0;
      end;
    eMetaDatabaseName: // Readonly
      begin
      OdbcRetCode := SQLGetConnectAttr(fhCon, SQL_DATABASE_NAME,
       PAnsiChar(PropValue), MaxLength, @ConnectAttrLength);
      OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(fhCon, SQL_DATABASE_NAME)');
      Length := ConnectAttrLength;
      end;
    eMetaDatabaseVersion: // Readonly
      begin
      OdbcRetCode := SQLGetConnectAttr(fhCon, SQL_DBMS_VER,
       PAnsiChar(PropValue), MaxLength, @ConnectAttrLength);
      OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(fhCon, SQL_DBMS_VER)');
      Length := ConnectAttrLength;
      end;
    eMetaTransactionIsoLevel: // Readonly
      begin
      end;
    eMetaSupportsTransaction: // Readonly
      begin
      // Transaction support

{
 ODBC Transaction support info values...

 SQL_TC_NONE = Transactions not supported. (ODBC 1.0)

 SQL_TC_DML = Transactions can only contain Data Manipulation Language
 (DML) statements (SELECT, INSERT, UPDATE, DELETE).
 Data Definition Language (DDL) statements encountered in a transaction
 cause an error. (ODBC 1.0)

 SQL_TC_DDL_COMMIT = Transactions can only contain DML statements.
 DDL statements (CREATE TABLE, DROP INDEX, and so on) encountered in a transaction
 cause the transaction to be committed. (ODBC 2.0)

 SQL_TC_DDL_IGNORE = Transactions can only contain DML statements.
 DDL statements encountered in a transaction are ignored. (ODBC 2.0)

 SQL_TC_ALL = Transactions can contain DDL statements and DML statements in any order.
 (ODBC 1.0)

 Mapping to DbExpress transaction support is based on DML support (ie SELECT, INSERT etc)
}
      OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_TXN_CAPABLE, GetInfoSmallInt,
       SizeOf(GetInfoSmallInt), nil);
      OdbcCheck(OdbcRetCode, 'SQLGetInfo(fhCon, SQL_TXN_CAPABLE)');
      case GetInfoSmallInt of
        SQL_TC_NONE        : boolean(PropValue^) := false;
        SQL_TC_DML         : boolean(PropValue^) := true;
        SQL_TC_DDL_COMMIT  : boolean(PropValue^) := true;
        SQL_TC_DDL_IGNORE  : boolean(PropValue^) := true;
        SQL_TC_ALL         : boolean(PropValue^) := true;
        end;
      end;
    eMetaMaxObjectNameLength: // Readonly
      begin
      OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_IDENTIFIER_LEN, MaxIdentifierLen,
       SizeOf(GetInfoSmallInt), nil);
      OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_MAX_IDENTIFIER_LEN)');
      integer(PropValue^) := GetInfoSmallInt;

      OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_COLUMN_NAME_LEN, MaxColumnNameLen,
       SizeOf(GetInfoSmallInt), nil);
      OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_MAX_COLUMN_NAME_LEN)');

      OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_TABLE_NAME_LEN, MaxTableNameLen,
       SizeOf(GetInfoSmallInt), nil);
      OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_MAX_TABLE_NAME_LEN)');

      MaxObjectNameLen := MaxIdentifierLen;
      if MaxColumnNameLen < MaxObjectNameLen then
        MaxObjectNameLen := MaxColumnNameLen;
      if MaxTableNameLen < MaxObjectNameLen then
        MaxTableNameLen := MaxColumnNameLen;
      integer(PropValue^) := MaxColumnNameLen;

      end;
    eMetaMaxColumnsInTable: // Readonly
      begin
      OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_COLUMNS_IN_TABLE, GetInfoSmallInt,
       SizeOf(GetInfoSmallInt), nil);
      OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_MAX_COLUMNS_IN_TABLE)');
      integer(PropValue^) := GetInfoSmallInt;
      end;
    eMetaMaxColumnsInSelect: // Readonly
      begin
      OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_COLUMNS_IN_SELECT, GetInfoSmallInt,
       SizeOf(GetInfoSmallInt), nil);
      OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_MAX_COLUMNS_IN_SELECT)');
      integer(PropValue^) := GetInfoSmallInt;
      end;
    eMetaMaxRowSize: // Readonly
      begin
      OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_ROW_SIZE, GetInfoSmallInt,
       SizeOf(GetInfoSmallInt), nil);
      OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_MAX_ROW_SIZE)');
      integer(PropValue^) := GetInfoSmallInt;
      end;
    eMetaMaxSQLLength: // Readonly
      begin
      OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_STATEMENT_LEN, GetInfoSmallInt,
       SizeOf(GetInfoSmallInt), nil);
      OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_MAX_STATEMENT_LEN)');
      integer(PropValue^) := GetInfoSmallInt;
      end;
    eMetaObjectQuoteChar: // Readonly
      begin
      if (MaxLength = 1) then
        begin
        OdbcRetCode := SQLGetInfoString(fhCon, SQL_IDENTIFIER_QUOTE_CHAR,
         @GetInfoStringBuffer, SizeOf(GetInfoStringBuffer), Length);
        OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_IDENTIFIER_QUOTE_CHAR)');
        AnsiChar(PropValue^) := GetInfoStringBuffer[0];
        Length := 1;
        end
      else
        begin
        OdbcRetCode := SQLGetInfoString(fhCon, SQL_IDENTIFIER_QUOTE_CHAR,
         PropValue, MaxLength, Length);
        OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_IDENTIFIER_QUOTE_CHAR)');
        end;
      end;
    eMetaSQLEscapeChar: // Readonly
      begin
      if (MaxLength = 1) then
        begin
        OdbcRetCode := SQLGetInfoString(fhCon, SQL_SEARCH_PATTERN_ESCAPE,
         @GetInfoStringBuffer, SizeOf(GetInfoStringBuffer), Length);
        OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_SEARCH_PATTERN_ESCAPE)');
        AnsiChar(PropValue^) := GetInfoStringBuffer[0];
        Length := 1;
        end
      else
        begin
        OdbcRetCode := SQLGetInfoString(fhCon, SQL_SEARCH_PATTERN_ESCAPE,
         PropValue, MaxLength, Length);
        OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_SEARCH_PATTERN_ESCAPE)');
        end;
      end;
    eMetaProcSupportsCursor: // Readonly
    // whether stored procedures can return a cursor
    // If ODBC driver indicates support for Stored Procedures,
    // it is assumed that they may return result sets (ie Cursors)
      begin
      OdbcRetCode := SQLGetInfoString(fhCon, SQL_PROCEDURES,
       @GetInfoStringBuffer, SizeOf(GetInfoStringBuffer), Length);
      OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_PROCEDURES)');
      if GetInfoStringBuffer[0] = 'Y' then
        boolean(PropValue^) := true
      else
        boolean(PropValue^) := false;
      end;
    eMetaProcSupportsCursors: // Readonly
    // whether stored procedures can return multiple cursors
      begin
      OdbcRetCode := SQLGetInfoInt(fhCon, SQL_BATCH_SUPPORT, BatchSupport,
        SizeOf(GetInfoSmallInt), nil);
      OdbcCheck(OdbcRetCode, 'SQLGetInfo(SQL_BATCH_SUPPORT)');

      if ((BatchSupport and SQL_BS_SELECT_PROC) <> 0) then
      // This indicates that the driver supports batches of procedures
      // that can have result-set generating statements
        boolean(PropValue^) := true
      else
        boolean(PropValue^) := false;

      end;
    eMetaSupportsTransactions: // Readonly
      begin
      // Nested transactions - Not supported by ODBC
      // (N.B. Non-nested transaction support is eMetaSupportsTransaction)
      boolean(PropValue^) := false;
      end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxNotSupported do
    Result := DBXERR_NOTSUPPORTED;
  on E: EDbxError do
    begin
    fConnectionErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlConnectionOdbc.GetOption(
  eDOption: TSQLConnectionOption;
  PropValue: Pointer;
  MaxLength: SmallInt;
  out Length: SmallInt
  ): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  AttrVal: SQLUINTEGER;
  SmallintAttrVal: SQLUSMALLINT;
  ConnectAttrLength: SQLUINTEGER;
begin
try
  case eDOption of
    eConnAutoCommit:
      begin
      OdbcRetCode := SQLGetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT, @AttrVal, 0, nil);
      OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(SQL_ATTR_AUTOCOMMIT)');
      boolean(PropValue^) := (AttrVal = SQL_AUTOCOMMIT_OFF);
      end;
    eConnBlockingMode:
      // We do not support Asynchronous statement execution
      // From ODBC API:
      // "On multithread operating systems, applications should execute functions on
      // separate threads, rather than executing them asynchronously on the same thread.
      // Drivers that operate only on multithread operating systems
      // do not need to support asynchronous execution."
      boolean(PropValue^) := false;
    eConnBlobSize:
      begin
      // "For drivers that don’t provide the available blob size before fetching, this
      // specifies the number of kilobytes of BLOB data that is fetched for BLOB fields.
      // This overrides any value specified at the driver level using eDrvBlobSize."
      integer(PropValue^) := fConnBlobSizeLimitK;
      end;
    eConnRoleName:
      // String that specifies the role to use when establishing a connection. (Interbase only)
      raise EDbxNotSupported.Create('TSqlDriverOdbc.GetOption(eConnRoleName) not supported - Applies to Interbase only');
    eConnWaitOnLocks:
      // Boolean that indicates whether application should wait until a locked
      // resource is free rather than raise an exception. (Interbase only)
      raise EDbxNotSupported.Create('TSqlDriverOdbc.GetOption(eConnWaitOnLocks) not supported - Applies to Interbase only');
    eConnCommitRetain:
      // Cursors dropped after commit
      raise EDbxNotSupported.Create('TSqlDriverOdbc.GetOption(eConnCommitRetain) not supported - Applies to Interbase only');
    eConnTxnIsoLevel:
      begin
      OdbcRetCode := SQLGetConnectAttr(fhCon, SQL_ATTR_TXN_ISOLATION, @AttrVal, 0, nil);
      OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(SQL_ATTR_TXN_ISOLATION)');
      if (AttrVal and SQL_TXN_SERIALIZABLE) <> 0 then
      // Transactions are serializable.
      // Serializable transactions do not allow dirty reads, nonrepeatable reads, or phantoms.
         TTransIsolationLevel(PropValue^) := xilREPEATABLEREAD
      else if (AttrVal and SQL_TXN_REPEATABLE_READ) <> 0 then
      // Dirty reads and nonrepeatable reads are not possible. Phantoms are possible
         TTransIsolationLevel(PropValue^) := xilREPEATABLEREAD
      else if (AttrVal and SQL_TXN_READ_COMMITTED) <> 0 then
      // Dirty reads are not possible. Nonrepeatable reads and phantoms are possible
         TTransIsolationLevel(PropValue^) := xilREADCOMMITTED
      else if (AttrVal and SQL_TXN_READ_UNCOMMITTED) <> 0 then
      // Dirty reads, nonrepeatable reads, and phantoms are possible.
         TTransIsolationLevel(PropValue^) := xilDIRTYREAD
      end;
    eConnNativeHandle:
    // The native SQL connection handle (Read-only)
      SQLHDBC(PropValue^) := fhCon;
    eConnServerVersion:
      begin
      OdbcRetCode := SQLGetConnectAttr(fhCon, SQL_DBMS_VER, PropValue, MaxLength, @ConnectAttrLength);
      OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(fhCon, SQL_DBMS_VER)');
      Length := ConnectAttrLength;
      end;
    eConnCallBack:
      TSQLCallBackEvent(PropValue^) := fDbxCallBack;
    eConnCallBackInfo:
       integer(PropValue^) := fDbxCallBackInfo;
    eConnHostName:
      raise EDbxNotSupported.Create('TSqlDriverOdbc.GetOption(eConnHostName) not supported - applies to MySql only');
    eConnDatabaseName: // Readonly
      begin
      OdbcRetCode := SQLGetConnectAttr(fhCon, SQL_DATABASE_NAME, PropValue, MaxLength, @ConnectAttrLength);
      OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(fhCon, SQL_DATABASE_NAME)');
      Length := ConnectAttrLength;
      end;
    eConnObjectMode:
    // Boolean value to enable or disable object fields in Oracle8 tables
      raise EDbxNotSupported.Create('TSqlDriverOdbc.GetOption(eConnObjectMode) not supported - applies to Oracle only');
    eConnMaxActiveConnection:
      begin
      if not fConnected then
        begin
        try
        // We cannot determine this setting until after we have connected
        // Normally we should raise an exception and return error code,
        // but unfortunately SqlExpress calls this option BEFORE connecting
        // so we'll just raise a WARNING, set return value of 1
        // (ie assume only 1 concurrent connection), and Success code
          Smallint(PropValue^) := 1;
          raise EDbxOdbcWarning.Create(
           'TSqlConnectionOdbc.GetOption(eConnMaxActiveConnection) called, but not connected');
        except
          on EDbxOdbcWarning do ;
        end;
        end
      else
        begin
        // The maximum number of active commands that can be executed by a single connection. Read-only.
        OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_CONCURRENT_ACTIVITIES,
         SmallintAttrVal, 2, nil);
        OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(SQL_MAX_CONCURRENT_ACTIVITIES)');
        Smallint(PropValue^) := SmallintAttrVal;
        end;
      end;
    eConnServerCharSet:
      begin
      OdbcRetCode := SQLGetInfoString(fhCon, SQL_COLLATION_SEQ, PropValue, MaxLength, Length);
      OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(SQL_COLLATION_SEQ)');
      end;
    eConnSqlDialect:
      // Interbase only
      raise EDbxNotSupported.Create('TSqlDriverOdbc.GetOption(eConnSqlDialect) not supported - applies to Interbase only');
    else
      raise EDbxInvalidCall.Create('Invalid option passed to TSqlDriverOdbc.GetOption: ' + IntToStr(Ord(eDOption)));
  end;
  Result := SQL_SUCCESS;
except
  on E: EDbxNotSupported do
    Result := DBXERR_NOTSUPPORTED;
  on E: EDbxError do
    begin
    fConnectionErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlConnectionOdbc.RetrieveDriverName: SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  Buffer: array[0..100] of AnsiChar;
  BufLen: SQLSMALLINT;
begin
  OdbcRetCode := SQLGetInfoString(fhCon, SQL_DRIVER_NAME, Buffer, SizeOf(Buffer), BufLen);
  OdbcCheck(OdbcRetCode, 'SQLGetInfo(fhCon, SQL_DRIVER_NAME)');
  fOdbcDriverName := Buffer;

  strUpper(buffer);
  if StrLComp(buffer, 'C2GUP', 5) = 0 then
    fOdbcDriverType := eOdbcDriverTypeGupta
  else if StrLComp(buffer, 'SQLSRV', 6) = 0 then
    fOdbcDriverType := eOdbcDriverTypeMsSqlServer
  else if StrLComp(buffer, 'DB2CLI', 6) = 0 then
    fOdbcDriverType := eOdbcDriverTypeIbmDb2
  else if StrLComp(buffer, 'ODBCJT', 6) = 0 then
    fOdbcDriverType := eOdbcDriverTypeAccess
  else if StrLComp(buffer, 'MYODBC', 6) = 0 then
    begin
    fOdbcDriverType := eOdbcDriverTypeMySql;
    fOdbcDriverLevel := 2; // MySql is Level 2
    end
  else
    fOdbcDriverType := eOdbcDriverTypeUnspecified;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlConnectionOdbc.getSQLCommand(
  out pComm: ISQLCommand): SQLResult;
begin
try
// Cannot get command object until we have successfully connected
  if not fConnected then
    raise EDbxInvalidCall.Create('getSQLCommand called but not yet connected');
  pComm := TSqlCommandOdbc.Create(self);
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    pComm := nil;
    fConnectionErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlConnectionOdbc.getSQLMetaData(
  out pMetaData: ISQLMetaData): SQLResult;
begin
try
// Cannot get metadata object until we have successfully connected
  if not fConnected then
    raise EDbxInvalidCall.Create('getSQLMetaData called but not yet connected');
  pMetaData := TSqlMetaDataOdbc.Create(self);
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    pMetaData := nil;
    fConnectionErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlConnectionOdbc.rollback(TranID: LongWord): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
try
  OdbcRetCode := SQLEndTran(SQL_HANDLE_DBC, fhCon, SQL_ROLLBACK);
  OdbcCheck(OdbcRetCode, 'SQLEndTran');
  Result := DBXpress.SQL_SUCCESS;
  fInTransaction := false;
except
  on E: EDbxError do
    begin
    fConnectionErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlConnectionOdbc.SetOption(
  eConnectOption: TSQLConnectionOption; lValue: Integer): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  AttrValue: SQLUINTEGER;
begin
try
  case eConnectOption of
    eConnAutoCommit:
      begin
      if lValue = 0 then
        AttrValue := SQL_AUTOCOMMIT_OFF
      else
        AttrValue :=   SQL_AUTOCOMMIT_ON;
      fAutoCommitMode := AttrValue;
      OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT, pointer(AttrValue), 0);
      OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(SQL_ATTR_AUTOCOMMIT)');
      end;
    eConnBlockingMode:
      // Asynchronous support
      raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnBlockingMode) not valid (Read-only)');
    eConnBlobSize:
      begin
      // "For drivers that don’t provide the available blob size before fetching, this
      // specifies the number of kilobytes of BLOB data that is fetched for BLOB fields."
      fConnBlobSizeLimitK := lValue;
      end;
    eConnRoleName:
      begin
      // String that specifies the role to use when establishing a connection. (Interbase only)
      raise EDbxNotSupported.Create('TSqlDriverOdbc.SetOption(eConnRoleName) not supported - Applies to Interbase only');
      end;
    eConnWaitOnLocks:
      begin
      // Boolean that indicates whether application should wait until a locked
      // resource is free rather than raise an exception. (Interbase only)
      raise EDbxNotSupported.Create('TSqlDriverOdbc.SetOption(eConnWaitOnLocks) not supported - Applies to Interbase only');
      end;
    eConnCommitRetain:
      begin
      // Cursors dropped after commit
      raise EDbxNotSupported.Create('TSqlDriverOdbc.SetOption(eConnCommitRetain) not supported - Applies to Interbase only');
      end;
    eConnTxnIsoLevel:
      begin
      case TTransIsolationLevel(lValue) of
      // Note that ODBC defines an even higher level of isolation, viz, SQL_TXN_SERIALIZABLE;
      // In this mode, Phantoms are not possible. (See ODBC spec).
      xilREPEATABLEREAD:
      // Dirty reads and nonrepeatable reads are not possible. Phantoms are possible
        AttrValue := SQL_TXN_REPEATABLE_READ;
      xilREADCOMMITTED:
      // Dirty reads are not possible. Nonrepeatable reads and phantoms are possible
        AttrValue := SQL_TXN_READ_COMMITTED;
      xilDIRTYREAD:
      // Dirty reads, nonrepeatable reads, and phantoms are possible.
        AttrValue := SQL_TXN_READ_COMMITTED;
      else
        raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnTxnIsoLevel) invalid isolation value: ' + IntToStr(lValue));
      end;
      OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_TXN_ISOLATION, pointer(AttrValue), 0);
      OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(SQL_ATTR_TXN_ISOLATION)');
      end;
    eConnNativeHandle:
    // The native SQL connection handle (Read-only)
      raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnNativeHandle) not valid (Read-only)');
    eConnServerVersion:
    // The server version (Read-only)
      raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnServerVersion) not valid (Read-only)');
    eConnCallback:
      fDbxCallBack := TSQLCallBackEvent(lValue);
    eConnCallBackInfo:
      fDbxCallBackInfo := lValue;
    eConnHostName:
      raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnHostName) not valid (Read-only)');
    eConnDatabaseName: // Readonly
      raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnDatabaseName) not valid (Read-only)');
    eConnObjectMode:
    // Boolean value to enable or disable object fields in Oracle8 tables
      raise EDbxNotSupported.Create('TSqlDriverOdbc.SetOption(eConnObjectMode) not supported - applies to Oracle only');
    eConnMaxActiveConnection:
      raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnMaxActiveConnection) not valid (Read-only)');
    eConnServerCharSet:
      raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnServerCharSet) not valid (Read-only)');
    eConnSqlDialect:
      // Interbase only
      raise EDbxNotSupported.Create('TSqlDriverOdbc.SetOption(eConnSqlDialect) not supported - applies to Interbase only');
    else
      raise EDbxInvalidCall.Create('Invalid option passed to TSqlDriverOdbc.SetOption: ' + IntToStr(Ord(eConnectOption)));
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fConnectionErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

procedure TSqlConnectionOdbc.TransactionCheck;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
  if (fInTransaction) then
    exit; // It's OK - already in a transaction
  if (fAutoCommitMode = SQL_AUTOCOMMIT_ON) then
    exit; // It's OK - already in AutoCommit mode

  OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT, pointer(SQLUINTEGER(SQL_AUTOCOMMIT_ON)), 0);
  OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_ON)');
  fAutoCommitMode := SQL_AUTOCOMMIT_ON;
end;

{ TSqlCommandOdbc }

constructor TSqlCommandOdbc.Create(
  OwnerDbxConnection: TSqlConnectionOdbc);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
  inherited Create;
  fCommandErrorLines := TStringList.Create;
  fOwnerDbxConnection := OwnerDbxConnection;
  fOwnerDbxDriver := fOwnerDbxConnection.fOwnerDbxDriver;
  fCommandBlobSizeLimitK := fOwnerDbxConnection.fConnBlobSizeLimitK;
  OdbcRetCode := SQLAllocHandle(SQL_HANDLE_STMT, FOwnerDbxConnection.fhCon, fhStmt);
  OdbcCheck(OdbcRetCode, 'SQLAllocHandle(SQL_HANDLE_STMT)');
end;

destructor TSqlCommandOdbc.Destroy;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  i: integer;
begin
  OdbcRetCode := SQLFreeStmt (fhStmt, SQL_CLOSE);
  OdbcCheck(OdbcRetCode, 'SQLFreeStmt(SQL_CLOSE)');

  OdbcRetCode := SQLFreeStmt (fhStmt, SQL_UNBIND);
  OdbcCheck(OdbcRetCode, 'SQLFreeStmt(SQL_UNBIND)');

  OdbcRetCode := SQLFreeStmt (fhStmt, SQL_RESET_PARAMS);
  OdbcCheck(OdbcRetCode, 'SQLFreeStmt(SQL_RESET_PARAMS)');

  OdbcRetCode := SQLFreeHandle(SQL_HANDLE_STMT, fhStmt);
  OdbcCheck(OdbcRetCode, 'SQLFreeHandle(SQL_HANDLE_STMT)');

  if (fOdbcParamList <> nil) then
    begin
    for i := fOdbcParamList.Count - 1 downto 0 do
      TOdbcBindParam(fOdbcParamList[i]).Free;
    fOdbcParamList.Free;
    end;

  fCommandErrorLines.Free;
  inherited;
end;

procedure TSqlCommandOdbc.OdbcCheck(OdbcCode: SQLRETURN; OdbcFunctionName: string);
begin
  fOwnerDbxDriver.OdbcCheck(OdbcCode, OdbcFunctionName, SQL_HANDLE_STMT, fhStmt);
end;

function TSqlCommandOdbc.close: SQLResult;
begin
  Result := DBXpress.SQL_SUCCESS;
end;


function TSqlCommandOdbc.execute(var Cursor: ISQLCursor): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  OdbcNumCols: SQLSMALLINT;
begin
try
  fStmtFreed := false;
  fOwnerDbxConnection.TransactionCheck;

  OdbcRetCode := SQLExecute(fhstmt);
  if (OdbcRetCode <> OdbcApi.SQL_NO_DATA) then
    OdbcCheck(OdbcRetCode, 'SQLExecute');

// Get no of columns:
  OdbcRetCode := SQLNumResultCols(fhstmt, OdbcNumCols);
  OdbcCheck(OdbcRetCode, 'SQLNumResultCols in TSqlCommandOdbc.execute');

  if (OdbcNumCols = 0) then
    Cursor := nil
  else
    Cursor := TSqlCursorOdbc.Create(self);

  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    Cursor := nil;
    fCommandErrorLines.Add(E.Message);
    fCommandErrorLines.Add('');
    fCommandErrorLines.Add(fSql);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCommandOdbc.executeImmediate(SQL: PChar;
  var Cursor: ISQLCursor): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  OdbcNumCols: SQLSMALLINT;
begin
try
  fStmtFreed := false;
  fSql := fSQL;
  fOwnerDbxConnection.TransactionCheck;
  OdbcRetCode := SQLExecDirect(fhstmt, SQL, SQL_NTS);
// Some ODBC drivers return SQL_NO_DATA if update/delete statement did not update/delete any rows
  if (OdbcRetCode <> OdbcApi.SQL_NO_DATA) then
    OdbcCheck(OdbcRetCode, 'SQLExecDirect');

// Get no of columns:
  OdbcRetCode := SQLNumResultCols(fhstmt, OdbcNumCols);
  OdbcCheck(OdbcRetCode, 'SQLNumResultCols in TSqlCommandOdbc.executeImmediate');

  if (OdbcNumCols = 0) then
    Cursor := nil
  else
    Cursor := TSqlCursorOdbc.Create(self);

  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    Cursor := nil;
    fCommandErrorLines.Add(E.Message);
    fCommandErrorLines.Add('');
    fCommandErrorLines.Add(Sql);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCommandOdbc.getErrorMessage(Error: PChar): SQLResult;
begin
  StrCopy(Error, PChar(fCommandErrorLines.Text));
  fCommandErrorLines.Clear;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCommandOdbc.getErrorMessageLen(
  out ErrorLen: SmallInt): SQLResult;
begin
  ErrorLen := Length(fCommandErrorLines.Text);
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCommandOdbc.getNextCursor(var Cursor: ISQLCursor): SQLResult;
{ TODO : getNextCursor - THIS HAS NOT BEEN TESTED }
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  OdbcNumCols: SQLSMALLINT;
begin
try
  OdbcRetCode := SQLMoreResults(fhstmt);
  if (OdbcRetCode = OdbcApi.SQL_NO_DATA) then
    begin
    Cursor := nil;
    Result := DBXpress.SQL_SUCCESS;
    exit;
    end;
  OdbcCheck(OdbcRetCode, 'SQLMoreResults');

// Code below is the same as for Execute...

// Get number of columns:
  OdbcRetCode := SQLNumResultCols(fhstmt, OdbcNumCols);
  OdbcCheck(OdbcRetCode, 'SQLNumResultCols in TSqlCommandOdbc.execute');

  if (OdbcNumCols = 0) then
    Cursor := nil
  else
    Cursor := TSqlCursorOdbc.Create(self);

  Result := DBXpress.SQL_SUCCESS;

except
  on E: EDbxError do
    begin
    Cursor := nil;
    fCommandErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCommandOdbc.GetOption(eSqlCommandOption: TSQLCommandOption;
  var pValue: Integer; MaxLength: SmallInt;
  out Length: SmallInt): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  ValueLength: SQLSMALLINT;
begin
try
  case eSqlCommandOption of
    eCommRowsetSize:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommRowsetSize) not yet implemented');
    eCommBlobSize:
      pValue := fCommandBlobSizeLimitK;
    eCommBlockRead:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommBlockRead) not yet implemented');
    eCommBlockWrite:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommBlockWrite) not yet implemented');
    eCommParamCount:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommParamCount) not yet implemented');
    eCommNativeHandle:
      begin
      pValue := Integer(fhStmt);
      Length := SizeOf(Integer);
      end;
    eCommCursorName:
      begin
      OdbcRetCode := SQLGetCursorName(fhStmt, pointer(pValue), MaxLength, ValueLength);
      OdbcCheck(OdbcRetCode, 'SQLGetCursorName in TSqlCommandOdbc.GetOption');
      Length := ValueLength;
      end;
    eCommStoredProc:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommStoredProc) not yet implemented');
    eCommSQLDialect: // INTERBASE ONLY
      raise EDbxInvalidCall.Create('TSqlCommandOdbc.GetOption(eCommSQLDialect) valid only for Interbase');
    eCommTransactionID:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommTransactionID) not yet implemented');
  else
    raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption - Invalid option ' + IntToStr(Ord(eSqlCommandOption)));
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on EDbxNotSupported do
    begin
    Length := 0;
    pValue := 0;
    Result := DBXERR_NOTSUPPORTED;
    end;
  on EDbxInvalidCall do
    begin
    Length := 0;
    pValue := 0;
    Result := DBXERR_INVALIDPARAM;
    end;
  on E: EDbxError do
    begin
    Length := 0;
    pValue := 0;
    fCommandErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCommandOdbc.getParameter(ParameterNumber, ulChildPos: Word;
  Value: Pointer; Length: Integer; var IsBlank: Integer): SQLResult;
{ TODO : getParameter - THIS HAS NOT BEEN TESTED }
var
  aOdbcBindParam: TOdbcBindParam;
begin
try
  if ParameterNumber > fOdbcParamList.Count then
    raise EDbxInvalidCall.Create('TSqlConnectionOdbc.getParameter - ParameterNumber exceeds parameter count');
  aOdbcBindParam := TOdbcBindParam(fOdbcParamList.Items[ParameterNumber-1]);
  if aOdbcBindParam.fOdbcParamLenOrInd = OdbcApi.SQL_NULL_DATA then
    begin
    IsBlank := 1;
    end
  else
    begin
    IsBlank := 0;
    Move(aOdbcBindParam.fValue, Value^, Length);
    end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fCommandErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCommandOdbc.getRowsAffected(var Rows: LongWord): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  OdbcRowsAffected: SQLINTEGER;
begin

// This is a workaround because SqlExpress calls getRowsAffected after Close!
  if fStmtFreed then
    begin
    Rows := 0;
    Result := DBXpress.SQL_SUCCESS;
    exit;
    end;

try
  OdbcRetCode := SQLRowCount(fhStmt, OdbcRowsAffected);
  OdbcCheck(OdbcRetCode, 'SQLRowCount in TSqlCommandOdbc.getRowsAffected');
  if OdbcRowsAffected < 0 then
    Rows := 0
  else
    Rows := OdbcRowsAffected;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    Rows := 0;
    fCommandErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCommandOdbc.prepare(SQL: PChar; ParamCount: Word): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  i: integer;
begin
try
  fStmtFreed := false;
  fSql := SQL;
  fOwnerDbxConnection.TransactionCheck;
  OdbcRetCode := SQLPrepare(fhstmt, SQL, SQL_NTS);
  OdbcCheck(OdbcRetCode, 'SQLPrepare');

  if (fOdbcParamList <> nil) then
    begin
    for i := fOdbcParamList.Count - 1 downto 0 do
      TOdbcBindParam(fOdbcParamList[i]).Free;
    fOdbcParamList.Free;
    fOdbcParamList := nil;
    end;

  if ParamCount > 0 then
    begin
    fOdbcParamList := TList.Create;
    fOdbcParamList.Count := ParamCount;
    for i := 0 to ParamCount - 1 do
      fOdbcParamList[i] := TOdbcBindParam.Create;
    end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fCommandErrorLines.Add(E.Message);
    fCommandErrorLines.Add(SQL);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCommandOdbc.SetOption(eSqlCommandOption: TSQLCommandOption;
  ulValue: Integer): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
try
  case eSqlCommandOption of
    eCommRowsetSize:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.SetOption(eCommRowsetSize) not yet implemented');
    eCommBlobSize:
      fCommandBlobSizeLimitK := ulValue;
    eCommBlockRead:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.SetOption(eCommBlockRead) not yet implemented');
    eCommBlockWrite:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.SetOption(eCommBlockWrite) not yet implemented');
    eCommParamCount:
      raise EDbxInvalidCall.Create('TSqlCommandOdbc.SetOption(eCommParamCount) not valid (Read-only)');
    eCommNativeHandle:
      raise EDbxInvalidCall.Create('TSqlCommandOdbc.SetOption(eCommNativeHandle) not valid (Read-only)');
    eCommCursorName:
      begin
      OdbcRetCode := SQLSetCursorName(fhStmt, pointer(ulValue), SQL_NTS);
      OdbcCheck(OdbcRetCode, 'SQLSetCursorName');
      end;
    eCommStoredProc:;
//      raise EDbxNotSupported.Create('TSqlCommandOdbc.SetOption(eCommStoredProc) not yet implemented');
    eCommSQLDialect:
      raise EDbxInvalidCall.Create('TSqlCommandOdbc.SetOption(eCommStoredProc) not valid for this DBExpress driver (Interbase only)');
    eCommTransactionID:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.SetOption(eCommTransactionID) not yet implemented');
  else
    raise EDbxInvalidCall.Create('TSqlCommandOdbc.SetOption - Invalid option ' + IntToStr(Ord(eSqlCommandOption)));
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxNotSupported do
    begin
    Result := DBXERR_NOTSUPPORTED;
    end;
  on E: EDbxError do
    begin
    fCommandErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCommandOdbc.setParameter(
  ulParameter,
  ulChildPos: Word;
  eParamType: TSTMTParamType;
  uLogType,
  uSubType: Word;
  iPrecision,
  iScale: Integer;
  Length: LongWord;
  pBuffer: Pointer;
  bIsNull: Integer
  ): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  aDays: integer;
  aYear, aMonth, aDay: word;
  aSeconds: integer;
  s: string;
  aOdbcBindParam: TOdbcBindParam;
  pData: pointer;

begin
try
  if (eParamType <> paramIN) then
  { TODO : OUTPUT parameters }
    raise EDbxNotSupported.Create('TSqlCommandOdbc.setParameter - Non-input parameters not yet supoorted');

  aOdbcBindParam := TOdbcBindParam(fOdbcParamList.Items[ulParameter-1]);
  with aOdbcBindParam do
  begin
  fOdbcParamLenOrInd := 0;
  fOdbcParamIbScale := 0;
  if (fBuffer <> nil) then
    begin
    FreeMem(fBuffer);
    fBuffer := nil;
    end;
  pData := @fValue;  // pointer to the Data Value
  if (bIsNull <> 0) then
    fOdbcParamLenOrInd := OdbcApi.SQL_NULL_DATA;
  case uLogType of
  fldZSTRING:
    begin
    fOdbcParamCType := SQL_C_CHAR;
    fOdbcParamSqlType := SQL_VARCHAR;
    fOdbcParamCbColDef := Length;
    if (bIsNull = 0) then
      begin
      fOdbcParamLenOrInd := SQL_NTS;
      StrCopy(fValue.OdbcParamValueString, pBuffer);
      end
    else
      begin
      if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeMsSqlServer) then
      // MsSqlServer driver insists on non-zero length, even for NULL values
        fOdbcParamCbColDef := 1;
      end;
    end;
  fldINT32:
    begin
    fOdbcParamCType := SQL_C_LONG;
    fOdbcParamSqlType := SQL_INTEGER;
    fOdbcParamCbColDef := SizeOf(Integer);
    if (bIsNull = 0) then
      begin
      fOdbcParamLenOrInd := SizeOf(Integer);
      fValue.OdbcParamValueInteger := Integer(pBuffer^);
      end;
    end;
  fldINT16:
    begin
    fOdbcParamCType := SQL_C_SHORT;
    fOdbcParamSqlType := SQL_SMALLINT;
    fOdbcParamCbColDef := SizeOf(SmallInt);
    if (bIsNull = 0) then
      begin
      fOdbcParamLenOrInd := SizeOf(SmallInt);
      fValue.OdbcParamValueShort := Smallint(pBuffer^);
      end;
    end;
  fldINT64:
    begin
    fOdbcParamCType := SQL_C_SBIGINT;
    fOdbcParamSqlType := SQL_BIGINT;
    fOdbcParamCbColDef := SizeOf(Int64);
    if (bIsNull = 0) then
      begin
      fOdbcParamLenOrInd := SizeOf(Int64);
      fValue.OdbcParamValueInteger := Int64(pBuffer^);
      end;
    end;
  fldFLOAT: // 64-bit floating point
    begin
    fOdbcParamCType := SQL_C_DOUBLE;
    fOdbcParamSqlType := SQL_DOUBLE;
    fOdbcParamCbColDef := SizeOf(Double);
    if (bIsNull = 0) then
      begin
      fOdbcParamLenOrInd := SizeOf(SQL_DOUBLE);
      fValue.OdbcParamValueDouble := Double(pBuffer^);
      end;
    end;
  fldDATE:
    begin
    fOdbcParamCType := SQL_C_DATE;
    fOdbcParamSqlType := SQL_DATE;
    fOdbcParamCbColDef := SizeOf(TSqlDateStruct);
    if (bIsNull = 0) then
      begin
      fOdbcParamLenOrInd := SizeOf(TSqlDateStruct);
      aDays := integer(pBuffer^) - DateDelta; // Days between 1/1/0001 and 12/31/1899 = 693594, ie (1899 * 365) (normal days) + 460 (leap days) - 1 (correction for being last day of 1899)
      // leap days between 0001 and 1899 = 460, ie 1896/4 - 14 (because 14 years weren't leap years: 100,200,300, 500,600,700, 900,1000,1100, 1300,1400,1500, 1700,1800)
      DecodeDate(aDays, aYear, aMonth, aDay);
      fValue.OdbcParamValueDate.year  := aYear;
      fValue.OdbcParamValueDate.month := aMonth;
      fValue.OdbcParamValueDate.day   := aDay;
      end;
    end;
  fldTIME:
    begin
    fOdbcParamCType := SQL_C_TIME;
    fOdbcParamSqlType := SQL_TIME;
    fOdbcParamCbColDef := SizeOf(TSqlTimeStruct);
    if (bIsNull = 0) then
      begin
      // Value is time in Microseconds
      aSeconds := LongWord(pBuffer^) DIV 1000;
      fOdbcParamLenOrInd := SizeOf(TSqlTimeStruct);
      fValue.OdbcParamValueTime.hour  := aSeconds div 3600;
      fValue.OdbcParamValueTime.minute := (aSeconds div 60) mod 60;
      fValue.OdbcParamValueTime.second   := aSeconds mod 60;
      end;
    end;
  fldDATETIME:
    begin
    fOdbcParamCType := SQL_C_TIMESTAMP;
    fOdbcParamSqlType := SQL_TIMESTAMP;
    fOdbcParamCbColDef := 26;
    fOdbcParamIbScale := 6;
    if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeMsSqlServer) then
      begin
      // Workaround SqlServer driver - it only allows max scale of 3
      fOdbcParamCbColDef := 23;
      fOdbcParamIbScale := 3;
      end;
    if (bIsNull = 0) then
      begin
      fOdbcParamLenOrInd := SizeOf(SQL_TIMESTAMP_STRUCT);
      fValue.OdbcParamValueTimeStamp.year     := TSQLTimeStamp(pBuffer^).Year;
      fValue.OdbcParamValueTimeStamp.month    := TSQLTimeStamp(pBuffer^).Month;
      fValue.OdbcParamValueTimeStamp.day      := TSQLTimeStamp(pBuffer^).Day;
      fValue.OdbcParamValueTimeStamp.Hour     := TSQLTimeStamp(pBuffer^).Hour;
      fValue.OdbcParamValueTimeStamp.Minute   := TSQLTimeStamp(pBuffer^).Minute;
      fValue.OdbcParamValueTimeStamp.Second   := TSQLTimeStamp(pBuffer^).Second;
      // Odbc in nanoseconds; DbExpress in milliseconds; so multiply by 1 million
      fValue.OdbcParamValueTimeStamp.Fraction := TSQLTimeStamp(pBuffer^).Fractions * 1000000;
      end;
    end;
  fldBCD:
    begin
    fOdbcParamCType := SQL_C_CHAR;
    fOdbcParamSqlType := SQL_DECIMAL;
    if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeAccess) then
      fOdbcParamSqlType := SQL_NUMERIC;  // MS ACCESS driver does not allow SQL_DECIMAL
    fOdbcParamCbColDef := iPrecision;
    fOdbcParamIbScale := iScale;
    if (bIsNull = 0) then
      begin
      fOdbcParamLenOrInd := SQL_NTS;
      s := BcdToStr(TBcd(pBuffer^));
      StrCopy(fValue.OdbcParamValueString, pChar(s));
      end
    else
      begin
      if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeMsSqlServer) then
      // MsSqlServer driver insists on non-zero length, even for NULL values
        fOdbcParamCbColDef := 1;
      end;
    end;
  fldBOOL:
    begin
    fOdbcParamCType := SQL_C_BIT;
    fOdbcParamSqlType := SQL_BIT;  // MS ACCESS driver does not allow SQL_DECIMAL
    fOdbcParamCbColDef := 1;
    if (bIsNull = 0) then
      begin
      fOdbcParamLenOrInd := 1;
      if Byte(pBuffer^) = 0 then
        fValue.OdbcParamValueBit := 0
      else
        fValue.OdbcParamValueBit := 1;
      end;
    end;
  fldBLOB:
    begin
    case uSubType of
    fldstBINARY:
      begin
      fOdbcParamCType := SQL_C_BINARY;
      fOdbcParamSqlType := SQL_LONGVARBINARY;
      end;
    fldstMEMO:
      begin
      fOdbcParamCType := SQL_C_CHAR;
      fOdbcParamSqlType := SQL_LONGVARCHAR;
      end;
    else
      begin
      raise EDbxNotSupported.Create('TSqlCommandOdbc.setParameter - This data sub-type not yet supoorted');
      end;
    end;
    if (bIsNull = 0) then
      begin
      fOdbcParamLenOrInd := Length;
      fOdbcParamCbColDef := Length;
      fBuffer := AllocMem(Length);
      Move(pBuffer^, fBuffer^, Length);
      end
    else
      begin
      // MsSqlServer driver insists on non-zero length, even for NULL values
      if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeMsSqlServer) then
        fOdbcParamCbColDef := 1;
      fBuffer := nil;
      end;
    pData := fBuffer;  // pointer to the Data Value
    end;
  else
    begin
    raise EDbxNotSupported.Create('TSqlCommandOdbc.setParameter - This data type not yet supoorted');
    end;
  end;

  OdbcRetCode := SQLBindParameter(
   fhStmt, // Odbc statement handle
   ulParameter, // Parameter number, starting at 1
   SQL_PARAM_INPUT, // Parameter InputOutputType
   fOdbcParamCType, // 'C' data type of paremeter - Sets SQL_DESC_TYPE of APD (application parameter descriptor)
   fOdbcParamSqlType, // 'Sql' data type of paremeter - Sets SQL_DESC_TYPE of IPD (implementation parameter descriptor)
   fOdbcParamCbColDef, fOdbcParamIbScale,
   pData,  // pointer to the Data Value
   0, @fOdbcParamLenOrInd); // Second to last argument applies to Output (or Input/Output) parameters only
(*  if Length(ArgValue) = 0 then
    gRetcode := SQLBindParameter(fhStmt, ulParameter, SQL_PARAM_INPUT,
     SQL_C_CHAR, SQL_VARCHAR, SQL_NTS, 0, pChar(fParamValue[ParamIndex]), 0, nil)
  else
// Workaround for Centura SqlBase: SQL_NTS locks up if param length = field size, so use actual size instead
    gRetcode := SQLBindParameter(fhStmt, ulParameter, SQL_PARAM_INPUT,
     SQL_C_CHAR, SQL_VARCHAR, Length(ArgValue) {SQL_NTS}, 0, pChar(fParamValue[ParamIndex]), 0, nil);*)
//  OdbcRetCode := SQLBindParameter(fhStmt, 1, SQL_PARAM_INPUT,
//   SQL_C_CHAR, SQL_VARCHAR, 2, 0, pBuffer, 0, nil);
  OdbcCheck (OdbcRetCode, 'SQLBindParameter');
  Result := DBXpress.SQL_SUCCESS;
  end;
except
  on EDbxNotSupported do
    begin
    Result := DBXERR_NOTSUPPORTED;
    end;
  on E: EDbxError do
    begin
    fCommandErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

{ TSQLMetaDataOdbc }

constructor TSQLMetaDataOdbc.Create(OwnerDbxConnection: TSqlConnectionOdbc);
begin
  inherited Create;
  fMetaDataErrorLines := TStringList.Create;
  fOwnerDbxConnection := OwnerDbxConnection;
end;

destructor TSQLMetaDataOdbc.Destroy;
begin
  fMetaDataErrorLines.Free;
  inherited;
end;

function TSQLMetaDataOdbc.getColumns(
  TableName,
  ColumnName: PChar;
  ColType: LongWord;
  out Cursor: ISQLCursor
  ): SQLResult;
var
  aCursor: TSqlCursorMetaDataColumns;
begin
  aCursor := TSqlCursorMetaDataColumns.Create(self);
try
  aCursor.FetchColumns(TableName, ColumnName, ColType);
  Cursor := aCursor;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    aCursor.Free;
    Cursor := nil;
    fMetaDataErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSQLMetaDataOdbc.getErrorMessage(Error: PChar): SQLResult;
begin
  StrCopy(Error, PChar(fMetaDataErrorLines.Text));
  fMetaDataErrorLines.Clear;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSQLMetaDataOdbc.getErrorMessageLen(
  out ErrorLen: SmallInt): SQLResult;
begin
  ErrorLen := Length(fMetaDataErrorLines.Text);
  Result := DBXpress.SQL_SUCCESS;
end;

function TSQLMetaDataOdbc.getIndices(TableName: PChar; IndexType: LongWord;
  out Cursor: ISQLCursor): SQLResult;
var
  aCursor: TSqlCursorMetaDataIndexes;
begin
  aCursor := TSqlCursorMetaDataIndexes.Create(self);
try
  if fOwnerDbxConnection.fSupportsSQLSTATISTICS then
    aCursor.FetchIndexes(TableName, IndexType);
  Cursor := aCursor;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    aCursor.Free;
    Cursor := nil;
    fMetaDataErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSQLMetaDataOdbc.getObjectList(eObjType: TSQLObjectType;
  out Cursor: ISQLCursor): SQLResult;
begin
try
  raise EDbxNotSupported.Create('TSQLMetaDataOdbc.getObjectList - not yet supported');
except
  on E: EDbxNotSupported do
    begin
    Result := DBXERR_NOTSUPPORTED;
    end;
end;
end;

function TSQLMetaDataOdbc.GetOption(eDOption: TSQLMetaDataOption;
  PropValue: Pointer; MaxLength: SmallInt;
  out Length: SmallInt): SQLResult;
begin
try
  Result := fOwnerDbxConnection.GetMetaDataOption(eDOption, PropValue,
    MaxLength, Length);
except
  on E: EDbxNotSupported do
    begin
    Length := 0;
    Result := DBXERR_NOTSUPPORTED;
    end;
  on E: EDbxError do
    begin
    Length := 0;
    fMetaDataErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSQLMetaDataOdbc.getProcedureParams(ProcName, ParamName: PChar;
  out Cursor: ISQLCursor): SQLResult;
var
  aCursor: TSqlCursorMetaDataProcedureParams;
begin
  aCursor := TSqlCursorMetaDataProcedureParams.Create(self);
try
  aCursor.FetchProcedureParams(ProcName, ParamName);
  Cursor := aCursor;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    aCursor.Free;
    Cursor := nil;
    fMetaDataErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSQLMetaDataOdbc.getProcedures(
  ProcedureName: PChar;
  ProcType: LongWord;
  out Cursor: ISQLCursor
  ): SQLResult;
var
  aCursor: TSqlCursorMetaDataProcedures;
begin
  aCursor := TSqlCursorMetaDataProcedures.Create(self);
try
  aCursor.FetchProcedures(ProcedureName, ProcType);
  Cursor := aCursor;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    aCursor.Free;
    Cursor := nil;
    fMetaDataErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSQLMetaDataOdbc.getTables(TableName: PChar; TableType: LongWord;
  out Cursor: ISQLCursor): SQLResult;
var
  aCursor: TSqlCursorMetaDataTables;
begin
  aCursor := TSqlCursorMetaDataTables.Create(self);
try
  aCursor.FetchTables(TableName, TableType);
  Cursor := aCursor;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    aCursor.Free;
    Cursor := nil;
    fMetaDataErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSQLMetaDataOdbc.SetOption(
  eDOption: TSQLMetaDataOption;
  PropValue: Integer): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
begin
try
  case eDOption of
    eMetaCatalogName:
      begin
      OdbcRetCode := SQLSetConnectAttr(fOwnerDbxConnection.fhCon,
       SQL_ATTR_CURRENT_CATALOG, PAnsiChar(PropValue), SQL_NTS);
      fOwnerDbxConnection.OdbcCheck (OdbcRetCode, 'SQLSetConnectAttr(SQL_ATTR_CURRENT_CATALOG)');
      end;
    eMetaSchemaName:
      begin
      {
      SQLExpress calls this option to set SCHEMA name to login USERNAME immediately after connecting.
      This is behaviour is undesirable. Probabably a bug.
      (We return fully qualified table names where appropriate,
      eg UserId 'ED' may want to view table 'SYSSQL.SYSTABLES').
      So we just IGNORE this option.
      }
      end;
    eMetaDatabaseName: // Read-only
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaDatabaseName) not valid (Read-only)');
    eMetaDatabaseVersion: // Read-only
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaDatabaseVersion) not valid (Read-only)');
    eMetaTransactionIsoLevel: // (Read-only:
      // use the options of SQLConnection to set the transaction isolation level)
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaTransactionIsoLevel) not valid (Read-only) (Use options of ISQLConnection instead)');
    eMetaSupportsTransaction: // Read-only
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaSupportsTransaction) not valid (Read-only)');
    eMetaMaxObjectNameLength: // Read-only
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaMaxObjectNameLength) not valid (Read-only)');
    eMetaMaxColumnsInTable: // Read-only
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaMaxColumnsInTable) not valid (Read-only)');
    eMetaMaxColumnsInSelect: // Read-only
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaMaxColumnsInSelect) not valid (Read-only)');
    eMetaMaxRowSize: // Read-only
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaMaxRowSize) not valid (Read-only)');
    eMetaMaxSQLLength: // Read-only
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaMaxSQLLength) not valid (Read-only)');
    eMetaObjectQuoteChar: // Read-only
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaObjectQuoteChar) not valid (Read-only)');
    eMetaSQLEscapeChar: // Read-only
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaSQLEscapeChar) not valid (Read-only)');
    eMetaProcSupportsCursor: // Read-only
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaProcSupportsCursor) not valid (Read-only)');
    eMetaProcSupportsCursors: // Read-only
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaProcSupportsCursors) not valid (Read-only)');
    eMetaSupportsTransactions: // Read-only
      raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaSupportsTransactions) not valid (Read-only)');
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxNotSupported do
    Result := DBXERR_NOTSUPPORTED;
  on E: EDbxInvalidCall do
    Result := DBXERR_INVALIDPARAM;
  on E: EDbxError do
    begin
    fMetaDataErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;


{ TSqlCursorOdbc }

constructor TSqlCursorOdbc.Create(OwnerCommand: TSqlCommandOdbc);

begin
  inherited create;

  fCursorErrorLines := TStringList.Create;
  fOwnerCommand := OwnerCommand;
  fOwnerDbxConnection := OwnerCommand.fOwnerDbxConnection;
  fOwnerDbxDriver := fOwnerDbxConnection.fOwnerDbxDriver;
  fhStmt := OwnerCommand.fhStmt;

  BindResultSet;
end;

destructor TSqlCursorOdbc.Destroy;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  i: integer;
begin
  fOwnerCommand.fStmtFreed := true;
  OdbcRetCode := SQLCloseCursor(fhstmt);
  OdbcCheck(OdbcRetCode, 'SQLCloseCursor');

  if fCursorErrorLines.Count > 0 then
  // Error lines still pending! Pass to calling connection
    fOwnerDbxConnection.fConnectionErrorLines.Assign(fCursorErrorLines);
  fCursorErrorLines.Free;
  if fOdbcBindList <> nil then
    begin
    for i := fOdbcBindList.Count - 1 downto 0 do
      TOdbcBindCol(fOdbcBindList[i]).Free;
    fOdbcBindList.Free;
    end;
  inherited;
end;

procedure TSqlCursorOdbc.OdbcCheck(OdbcCode: SQLRETURN; OdbcFunctionName: string);
begin
  fOwnerDbxDriver.OdbcCheck(OdbcCode, OdbcFunctionName, SQL_HANDLE_STMT, fhStmt);
end;

procedure TSqlCursorOdbc.BindResultSet;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  aOdbcBindCol: TOdbcBindCol;
  colno: integer;
  ColNameTemp: pAnsiChar;
  IntAttribute: SQLINTEGER;
  OdbcLateBoundFound: boolean;
begin
  ColNameTemp := nil;
try
  ColNameTemp := AllocMem(fOwnerDbxConnection.fOdbcMaxColumnNameLen + 1);
// Get no of columns:
  OdbcRetCode := SQLNumResultCols(fhStmt, fOdbcNumCols);
  OdbcCheck(OdbcRetCode, 'SQLNumResultCols');
// Set up bind columns:
  if (fOdbcBindList <> nil) then
    begin
    for colno := fOdbcBindList.Count - 1 downto 0 do
      TOdbcBindCol(fOdbcBindList[colno]).Free;
    fOdbcBindList.Free;
    end;
  fOdbcBindList := TList.Create;
  fOdbcBindList.Count := fOdbcNumCols;

// Describe each column...
  for colno := 1 to fOdbcNumCols do
  begin
  aOdbcBindCol := TOdbcBindCol.Create;
  fOdbcBindList.Items[colNo-1] := aOdbcBindCol;
  with aOdbcBindCol do
    begin
    OdbcRetCode := SQLDescribeCol(
      fhStmt, colno,
      ColNameTemp, fOwnerDbxConnection.fOdbcMaxColumnNameLen, fColNameSize,
      fSqlType, fColSize, fColScale, fNullable);
    OdbcCheck(OdbcRetCode, 'SQLDescribeCol');
    fColName := AllocMem(fColNameSize + 1);
    strLCopy(fColName, ColNameTemp, fColNameSize);

    fOdbcHostVarAddress := @fValue;
    fDbxSubType:= 0;
    case fSqlType of
      SQL_INTEGER:
        begin
        fDbxType := fldINT32;
        fOdbcHostVarType := SQL_C_LONG;
        fOdbcHostVarSize := SizeOf(fValue.OdbcColValueInteger);

        // Check to see if field is an AUTO-INCREMENTING value
        OdbcRetCode := SQLColAttributeInt (fhStmt, colno, SQL_DESC_AUTO_UNIQUE_VALUE,
         nil, 0, nil, IntAttribute);
        OdbcCheck(OdbcRetCode, 'SQLColAttribute(SQL_DESC_AUTO_UNIQUE_VALUE)');
        if (IntAttribute = SQL_TRUE) then
          fDbxSubType:= fldstAUTOINC;
        end;
      SQL_BIGINT:
        begin
// DbExpress does not currently support Int64 - use Int32 instead!
// Re-instate next 3 statements when DbExpress does support int64 }
{
        fDbxType := fldINT64;
        fOdbcHostVarType := SQL_C_SBIGINT;
        fOdbcHostVarSize := SizeOf(fValue.OdbcParamValueInt64);
}
        fDbxType := fldINT32;
        fOdbcHostVarType := SQL_C_LONG;
        fOdbcHostVarSize := SizeOf(fValue.OdbcColValueInteger);
        end;
      SQL_SMALLINT, SQL_TINYINT:
        begin
        fDbxType := fldINT16;
        fOdbcHostVarType := SQL_C_SHORT;
        fOdbcHostVarSize := SizeOf(fValue.OdbcColValueShort);
        end;
      SQL_NUMERIC, SQL_DECIMAL:
        begin
        fDbxType := fldBCD;
        fOdbcHostVarType := SQL_C_CHAR; // Odbc prefers to return BCD as string
        fOdbcHostVarSize := fColSize + 3; // add 3 to number of digits: sign, decimal point, null terminator
        end;
      SQL_DOUBLE, SQL_FLOAT, SQL_REAL:
        begin
        fDbxType := fldFLOAT;
        fOdbcHostVarType := SQL_C_DOUBLE;
        fOdbcHostVarSize := SizeOf(fValue.OdbcColValueDouble);
        end;
      SQL_CHAR, SQL_VARCHAR, SQL_WCHAR, SQL_WVARCHAR, SQL_GUID:
        begin
        fDbxType := fldZSTRING;
        fOdbcHostVarType := SQL_C_CHAR;
        fOdbcHostVarSize := fColSize + 1;   // Add 1 for null terminator
        case fSqlType of
        SQL_CHAR, SQL_WCHAR: // Fixed length field
          fDbxSubType:= fldstFIXED;
        end;
        if fOdbcHostVarSize > 256 then
          begin
          // too big for buffer  - allocate memory for value
          fpBuffer := AllocMem(fOdbcHostVarSize);
          fOdbcHostVarAddress := fpBuffer;
          end
        end;
      SQL_BINARY, SQL_VARBINARY:
        begin
        fDbxType := fldBYTES;
        fOdbcHostVarType := SQL_C_BINARY;
        fOdbcHostVarSize := fColSize;
        if fColSize > 256 then
          begin
          // too big for buffer  - allocate memory for value
          fpBuffer := AllocMem(fColSize);
          fOdbcHostVarAddress := fpBuffer;
          end
        end;
      SQL_TYPE_DATE:
        begin
        fDbxType:= fldDATE;
        fOdbcHostVarType := SQL_C_DATE;
        fOdbcHostVarSize := SizeOf(fValue.OdbcColValueDate);
        end;
      SQL_TYPE_TIME:
        begin
        fDbxType := fldTIME;
        fOdbcHostVarType := SQL_C_TIME;
        fOdbcHostVarSize := SizeOf(fValue.OdbcColValueTime);
        end;
      SQL_TYPE_TIMESTAMP, SQL_DATETIME, SQL_TIMESTAMP:
         begin
        fDbxType := fldDATETIME;
        fOdbcHostVarType := SQL_C_TIMESTAMP;
        fOdbcHostVarSize := SizeOf(fValue.OdbcColValueTimeStamp);
        end;
      SQL_BIT:
        begin
        fDbxType:= fldBOOL;
        fOdbcHostVarType := SQL_C_BIT;
        fOdbcHostVarSize := SizeOf(fValue.OdbcColValueBit);
        end;
      SQL_LONGVARCHAR, SQL_WLONGVARCHAR:
        begin
        fDbxType:= fldBLOB;
        fDbxSubType:= fldstMEMO;
        fOdbcHostVarType := SQL_C_CHAR;
        if fOwnerCommand.fCommandBlobSizeLimitK > 0 then
// If BLOBSIZELIMIT specified, we early bind, just like normal column
// Otherwise get size and column data AFTER every row fetch, using SqlGetData
          begin
          fOdbcHostVarSize := fOwnerCommand.fCommandBlobSizeLimitK * 1024;
          fpBuffer := AllocMem(fOdbcHostVarSize);
          fOdbcHostVarAddress := fpBuffer;
          end
        else
          fOdbcLateBound := true;
        end;
      SQL_LONGVARBINARY:
        begin
        fDbxType:= fldBLOB;
        fDbxSubType:= fldstBINARY;
        fOdbcHostVarType := SQL_C_BINARY;
// We cannot Bind a BLOB - Determine size AFTER every row fetch
// We igmore BlobSizeLimit, because binary data (Images etc) cannot normally be truncated
        fOdbcLateBound := true;
        end;
      else
        begin
        raise EDbxOdbcError.Create(
          'ODBC function "SQLDescribeCol" returned unknown data type' + #13#10 +
          'Data type code= ' + inttostr(fSqlType) + #13#10 +
          'Column name=' + ColNameTemp);
        end;
      end; //case fSqlType of
      if fOdbcLateBound then
        OdbcLateBoundFound := true
      else
        begin
        if (OdbcLateBoundFound and (not fOwnerDbxConnection.fGetDataAnyOrder)) then
          fOdbcLateBound := true
        else
          begin
          OdbcRetCode := SQLBindCol(
            fhStmt, colno, fOdbcHostVarType,
            fOdbcHostVarAddress, fOdbcHostVarSize,
            @fColValueSize);
          OdbcCheck(OdbcRetCode, 'SQLBindCol');
          end;
        end;
    end;  // with
    end;  // for each column
finally
  FreeMem(ColNameTemp);
end;
end;

procedure TSqlCursorOdbc.FetchLateBoundData(OdbcColNo: SQLUSMALLINT);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  aOdbcBindCol: TOdbcBindCol;
begin
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[OdbcColNo-1]);
  with aOdbcBindCol do
    begin
    OdbcRetCode := SQLGetData(
     fhStmt, OdbcColNo, fOdbcHostVarType,
     fOdbcHostVarAddress, fOdbcHostVarSize, @fColValueSize);
    OdbcCheck(OdbcRetCode, 'SQLGetData');
    end;
end;

procedure TSqlCursorOdbc.FetchLongData(OdbcColNo: SQLUSMALLINT);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  aOdbcBindCol: TOdbcBindCol;
  BlobChunkSize: integer;
  CurrentBlobSize, PreviousBlobSize: integer;
  CurrentFetchPointer: pChar;
begin
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[OdbcColNo-1]);
  BlobChunkSize := 256;
  PreviousBlobSize := 0;
  with aOdbcBindCol do
    begin
    if (fpBuffer = nil) then
      fpBuffer := AllocMem(BlobChunkSize);
    CurrentBlobSize := BlobChunkSize;
    case aOdbcBindCol.fDbxSubType of
    fldstMEMO:
      begin
      {  Code below is very tricky
         Call SQLGetData to get first chunk of the BLOB
         If MORE blob data to get, Odbc returns SQL_SUCCESS_WITH_INFO, SQLSTATE 01004 (String truncated)
         Keep calling SqlGetData for each part of the blob, reallocating more
         memory for the BLOB data on each successive call.
         Odbc always
         }
      OdbcRetCode := SQLGetData(
       fhStmt, OdbcColNo, SQL_C_CHAR,
       fpBuffer, BlobChunkSize, @fColValueSize);
      while (OdbcRetCode = SQL_SUCCESS_WITH_INFO) do
        begin
        PreviousBlobSize := CurrentBlobSize;
        inc(CurrentBlobSize, BlobChunkSize);
        ReallocMem(fpBuffer, CurrentBlobSize);
        CurrentFetchPointer := fpBuffer;
        CurrentFetchPointer := CurrentFetchPointer + PreviousBlobSize
          - 1; // -1 to overwrite null termination char of previous chunk
        OdbcRetCode := SQLGetData(
          fhStmt, OdbcColNo, SQL_C_CHAR,
          CurrentFetchPointer,
          BlobChunkSize + 1, // Chunk size is +1 because we overwrite previous null terminator
          @fColValueSize);
        BlobChunkSize := BlobChunkSize * 2;  // Make ChunkSize bigger to avoid too many loop repetiontions
        end;
      end;
    fldstBINARY:
      begin
      {  Code for BINARY same as above, except we don't have NULL termination char  }
      OdbcRetCode := SQLGetData(
       fhStmt, OdbcColNo, SQL_C_BINARY,
       fpBuffer, BlobChunkSize, @fColValueSize);
      while (OdbcRetCode = SQL_SUCCESS_WITH_INFO) do
        begin
        PreviousBlobSize := CurrentBlobSize;
        inc(CurrentBlobSize, BlobChunkSize);
        ReallocMem(fpBuffer, CurrentBlobSize);
        CurrentFetchPointer := fpBuffer;
        CurrentFetchPointer := CurrentFetchPointer + PreviousBlobSize;
        OdbcRetCode := SQLGetData(
          fhStmt, OdbcColNo, SQL_C_BINARY,
          CurrentFetchPointer,
          BlobChunkSize,
          @fColValueSize);
        BlobChunkSize := BlobChunkSize * 2;  // Make ChunkSize bigger to avoid too many loop repetiontions
        end;
      end;
    else
      raise EDbxInternalError.Create('FetchLongData called for invalid Dbx Sub Type');
    end;

    fColValueSize := PreviousBlobSize + fColValueSize;
    OdbcCheck(OdbcRetCode, 'SQLGetData');
    end;
end;

function TSqlCursorOdbc.getBcd(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
var
  i: integer;
  c: char;
  n: byte;
  d: integer;
  Places: integer;
  DecimalPointFound: boolean;
begin
with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
  begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    if (fColValueSize = OdbcApi.SQL_NULL_DATA)
    or (fColValueSize = OdbcApi.SQL_NO_TOTAL) then
      begin
      IsBlank := true;
      Result := DBXpress.SQL_SUCCESS;
      exit;
      end;
    IsBlank := false;
    d := 0;  // Number of digits
    PBcd(Value).SignSpecialPlaces := 0; // Sign: 0=+; anything else =-
    DecimalPointFound := false;
    Places := 0;
    i := 0;
    if fValue.OdbcColValueString[0] = '-' then
      begin
      PBcd(Value).SignSpecialPlaces := $80; // Sign: 0=+; anything else =-
      i := 1;
      end;
    c := fValue.OdbcColValueString[i];
    while (c <> #0) do
      begin
      if (c = '.') or (c = ',') then
        DecimalPointFound := true
      else
        begin
        n := byte(c) - byte('0');
        if not odd(d) then
          PBcd(Value).Fraction[d DIV 2] := n SHL 4
        else
          inc(PBcd(Value).Fraction[d DIV 2], n); // Array of nibbles
        inc(d);
        if DecimalPointFound then
          inc(places);
        end;
      inc(i);
      c := fValue.OdbcColValueString[i];
      end;
    PBcd(Value).Precision := d;  // Number of digits
    inc(PBcd(Value).SignSpecialPlaces, places);
    Result := DBXpress.SQL_SUCCESS;
  end;
end;

function TSqlCursorOdbc.getBlob(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool; Length: LongWord): SQLResult;
begin
try
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
    begin
    if (fColValueSize = OdbcApi.SQL_NULL_DATA)
    or (fColValueSize = OdbcApi.SQL_NO_TOTAL) then
      begin
      IsBlank := true;
      Result := DBXpress.SQL_SUCCESS;
      exit;
      end;
    IsBlank := false;
    Move(fpBuffer^, Value^, Length);
    Result := DBXpress.SQL_SUCCESS;
    end;
except
  on E: EDbxError do
    begin
    pointer(Value^) := nil;
    IsBlank := true;
    fCursorErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorOdbc.getBlobSize(ColumnNumber: Word;
  var Length: LongWord; var IsBlank: LongBool): SQLResult;
begin
try
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
    begin

    if (fDbxType <> fldBLOB) then
      raise EDbxInvalidCall.Create('TSqlCursorOdbc.getBlobSize but field is not BLOB - column ' + IntToStr(ColumnNumber));

    if fOdbcLateBound then
      FetchLongData(ColumnNumber);

    if (fColValueSize = OdbcApi.SQL_NULL_DATA) then
      begin
      IsBlank := true;
      Length := 0;
      Result := DBXpress.SQL_SUCCESS;
      exit;
      end;

    IsBlank := false;
    Length := fColValueSize;

    Result := DBXpress.SQL_SUCCESS;

    end;
except
  on E: EDbxError do
    begin
    Length := 0;
    IsBlank := true;
    fCursorErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorOdbc.getBytes(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
    begin
    if (fColValueSize = OdbcApi.SQL_NULL_DATA)
    or (fColValueSize = OdbcApi.SQL_NO_TOTAL) then
      begin
      IsBlank := true;
      Result := DBXpress.SQL_SUCCESS;
      exit;
      end;
    IsBlank := false;
    if fpBuffer <> nil then
      Move(fpBuffer^, Value^, fColValueSize)
    else
      Move(fValue, Value^, fColValueSize);
    Result := DBXpress.SQL_SUCCESS;
    end;
end;

function TSqlCursorOdbc.getColumnCount(var pColumns: Word): SQLResult;
begin
  pColumns := fOdbcNumCols;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorOdbc.getColumnLength(ColumnNumber: Word;
  var pLength: LongWord): SQLResult;
begin
try
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
    begin
    case fDbxType of
    fldINT32: { 32 bit signed number }
      pLength:= 4;
    fldINT16: { 16 bit signed number }
      pLength:= 2;
    fldINT64: { 64 bit signed number }
      pLength:= 8;
    fldBCD:
      pLength := (fColSize + 1) DIV 2 + 2;
    fldFLOAT: { 64 bit floating point }
      pLength:= 8;
    fldZSTRING: { Null terminated string }
      pLength:= fColSize;
    fldDATE: { Date (32 bit) }
      pLength:= 4;
    fldTIME: { Time (32 bit) }
      pLength:= 4;
    fldDATETIME:
      pLength:= 8;
    fldBOOL:
      pLength:= 2;
    fldBYTES:
      pLength:= fColSize;
    fldBLOB:
      pLength:= fColSize;
    else
      raise EDbxNotSupported.Create('TSqlCursorOdbc.getColumnLength - not yet supported for this type - column ' + IntToStr(ColumnNumber));
    end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    pLength := 0;
    fCursorErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorOdbc.getColumnName(ColumnNumber: Word;
  pColumnName: PChar): SQLResult;
begin
  StrCopy(pColumnName, TOdbcBindCol(fOdbcBindList[ColumnNumber-1]).fColName);
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorOdbc.getColumnNameLength(ColumnNumber: Word;
  var pLen: Word): SQLResult;
begin
  pLen := strLen(TOdbcBindCol(fOdbcBindList[ColumnNumber-1]).fColName);
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorOdbc.getColumnPrecision(ColumnNumber: Word;
  var piPrecision: SmallInt): SQLResult;
begin
try
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
  case fDbxType of
    fldZSTRING:
      piPrecision := fColSize;
    fldBCD:
      piPrecision := fColSize;
    fldBytes:
      piPrecision := fColSize;
    fldVarBytes:
      piPrecision := fColSize;
    else
// DBXpress help says "Do not call getColumnPrecision for any other column type."
// But the donkey SqlExpress calls for EVERY column, so we cannot raise error
      piPrecision := 0;
//      raise EDbxNotSupported.Create('TSqlCursorOdbc.getColumnPrecision - not yet supported for data type - column ' + IntToStr(ColumnNumber));
    end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    piPrecision := 0;
    fCursorErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorOdbc.getColumnScale(ColumnNumber: Word;
  var piScale: SmallInt): SQLResult;
begin
try
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
  case fDbxType of
    fldBCD:
      piScale:= fColScale;
    else
// getColumnScale is should only be called for fldBCD, fldADT, or fldArray
// But SqlExpress calls it for EVERY column, so we cannot raise error...
//      raise EDbxNotSupported.Create('TSqlCursorOdbc.getColumnScale - not yet supported for data type - column ' + IntToStr(ColumnNumber));
      piScale:= 0;
    end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    piScale := 0;
    fCursorErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorOdbc.getColumnType(ColumnNumber: Word; var puType,
  puSubType: Word): SQLResult;
begin
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
    begin
    puType:= fDbxType;
    puSubType:= fDbxSubType;
    end;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorOdbc.getDate(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
    begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    if (fColValueSize = OdbcApi.SQL_NULL_DATA)
    or (fColValueSize = OdbcApi.SQL_NO_TOTAL) then
      begin
      IsBlank := true;
      Result := DBXpress.SQL_SUCCESS;
      exit;
      end;
    IsBlank := false;
    integer(Value^) := ((365 * 1900) + 94) + trunc(EncodeDate(
      fValue.OdbcColValueDate.year,
      fValue.OdbcColValueDate.month,
      fValue.OdbcColValueDate.day));
    Result := DBXpress.SQL_SUCCESS;
    end;
end;

function TSqlCursorOdbc.getDouble(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
    begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    if (fColValueSize = OdbcApi.SQL_NULL_DATA)
    or (fColValueSize = OdbcApi.SQL_NO_TOTAL) then
      begin
      IsBlank := true;
      Result := DBXpress.SQL_SUCCESS;
      exit;
      end;
    IsBlank := false;
    Double(Value^) := fValue.OdbcColValueDouble;
    IsBlank := false;
    Result := DBXpress.SQL_SUCCESS;
    end;
end;

function TSqlCursorOdbc.getErrorMessage(Error: PChar): SQLResult;
begin
  StrCopy(Error, PChar(fCursorErrorLines.Text));
  fCursorErrorLines.Clear;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorOdbc.getErrorMessageLen(
  out ErrorLen: SmallInt): SQLResult;
begin
  ErrorLen := Length(fCursorErrorLines.Text);
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorOdbc.getLong(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
    begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    if (fColValueSize = OdbcApi.SQL_NULL_DATA)
    or (fColValueSize = OdbcApi.SQL_NO_TOTAL) then
      begin
      IsBlank := true;
      Result := DBXpress.SQL_SUCCESS;
      exit;
      end;
    IsBlank := false;
    Integer(Value^) := fValue.OdbcColValueInteger;
    end;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorOdbc.GetOption(eOption: TSQLCursorOption;
  PropValue: Pointer; MaxLength: SmallInt;
  out Length: SmallInt): SQLResult;
begin
try
  raise EDbxNotSupported.Create('TSqlCursorOdbc.GetOption - not yet supported');
except
  on E: EDbxError do
    begin
    fCursorErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorOdbc.getShort(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
    begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    if (fColValueSize = OdbcApi.SQL_NULL_DATA)
    or (fColValueSize = OdbcApi.SQL_NO_TOTAL) then
      begin
      IsBlank := true;
      Result := DBXpress.SQL_SUCCESS;
      exit;
      end;
    IsBlank := false;
    Smallint(Value^) := fValue.OdbcColValueShort;
    Result := DBXpress.SQL_SUCCESS;
    end;
end;

function TSqlCursorOdbc.getString(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
    begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    if (fColValueSize = OdbcApi.SQL_NULL_DATA)
    or (fColValueSize = OdbcApi.SQL_NO_TOTAL) then
      begin
      IsBlank := true;
      Result := DBXpress.SQL_SUCCESS;
      exit;
      end;
    IsBlank := false;
    Move(fOdbcHostVarAddress^, Value^, fColValueSize + 1);
    Result := DBXpress.SQL_SUCCESS;
    end;
end;

function TSqlCursorOdbc.getTime(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
    begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    if (fColValueSize = OdbcApi.SQL_NULL_DATA)
    or (fColValueSize = OdbcApi.SQL_NO_TOTAL) then
      begin
      IsBlank := true;
      Result := DBXpress.SQL_SUCCESS;
      exit;
      end;
    IsBlank := false;

    // Returned value is time, in Microseconds
    LongWord(Value^) :=
     ((fValue.OdbcColValueTime.hour * 60 * 60) +
      (fValue.OdbcColValueTime.minute * 60) +
       fValue.OdbcColValueTime.second) * 1000;

    Result := DBXpress.SQL_SUCCESS;
    end;
end;

function TSqlCursorOdbc.getTimeStamp(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
    begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    if (fColValueSize = OdbcApi.SQL_NULL_DATA)
    or (fColValueSize = OdbcApi.SQL_NO_TOTAL) then
      begin
      IsBlank := true;
      Result := DBXpress.SQL_SUCCESS;
      exit;
      end;
    IsBlank := false;
    PSQLTimeStamp(Value).Year      := fValue.OdbcColValueTimeStamp.year;
    PSQLTimeStamp(Value).Month     := fValue.OdbcColValueTimeStamp.month;
    PSQLTimeStamp(Value).Day       := fValue.OdbcColValueTimeStamp.day;
    PSQLTimeStamp(Value).Hour      := fValue.OdbcColValueTimeStamp.hour;
    PSQLTimeStamp(Value).Minute    := fValue.OdbcColValueTimeStamp.minute;
    PSQLTimeStamp(Value).Second    := fValue.OdbcColValueTimeStamp.second;
    // Odbc returns nanoseconds; DbExpress expects milliseconds; so divide by 1 million
    PSQLTimeStamp(Value).Fractions := fValue.OdbcColValueTimeStamp.fraction DIV 1000000;
    end;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorOdbc.isAutoIncrement(ColumnNumber: Word;
  var AutoIncr: LongBool): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  IntAttribute: integer;
begin
try
  OdbcRetCode := SQLColAttributeInt (fhStmt, ColumnNumber-1, SQL_DESC_AUTO_UNIQUE_VALUE,
    nil, 0, nil, IntAttribute);
  OdbcCheck(OdbcRetCode, 'SQLColAttribute');
  AutoIncr := (IntAttribute = SQL_TRUE);
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fCursorErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorOdbc.isBlobSizeExact(ColumnNumber: Word;
  var IsExact: LongBool): SQLResult;
begin
  IsExact := true;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorOdbc.isNullable(ColumnNumber: Word;
  var Nullable: LongBool): SQLResult;
begin
  case TOdbcBindCol(fOdbcBindList[ColumnNumber-1]).fNullable of
  SQL_NULLABLE:
    Nullable := true;
  SQL_NO_NULLS:
    Nullable := false;
  SQL_NULLABLE_UNKNOWN:
    Nullable := true;
  end;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorOdbc.isReadOnly(ColumnNumber: Word;
  var ReadOnly: LongBool): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  IntAttribute: integer;
begin
try
  OdbcRetCode := SQLColAttributeInt (fhStmt, ColumnNumber-1, SQL_DESC_UPDATABLE,
    nil, 0, nil, IntAttribute);
  OdbcCheck(OdbcRetCode, 'SQLColAttribute(SQL_DESC_UPDATABLE)');
  ReadOnly := (IntAttribute = SQL_ATTR_READONLY);
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fCursorErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorOdbc.isSearchable(ColumnNumber: Word;
  var Searchable: LongBool): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  IntAttribute: integer;
begin
try
  OdbcRetCode := SQLColAttributeInt (fhStmt, ColumnNumber-1, SQL_DESC_SEARCHABLE,
    nil, 0, nil, IntAttribute);
  OdbcCheck(OdbcRetCode, 'SQLColAttribute(isSearchable)');
  Searchable := (IntAttribute <> SQL_PRED_NONE );
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fCursorErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorOdbc.next: SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  aOdbcBindCol: TOdbcBindCol;
begin
try
  inc(fRowNo);
  OdbcRetCode := SQLFetch(fhStmt);
  aOdbcBindCol := fOdbcBindList[0];

  case OdbcRetCode of
  OdbcApi.SQL_SUCCESS:
    Result := DBXpress.SQL_SUCCESS;
  OdbcApi.SQL_SUCCESS_WITH_INFO: // EOdbcWarning raised (warninbg only)
    Result := DBXpress.SQL_SUCCESS;
  OdbcApi.SQL_NO_DATA:
    Result := DBXERR_EOF;
  else
    Result := MaxReservedStaticErrors + 1; // Dummy to prevent compiler warning
    OdbcCheck(OdbcRetCode, 'SQLFetch');
  end;

except
  on E: EDbxError do
    begin
    fCursorErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorOdbc.SetOption(eOption: TSQLCursorOption;
  PropValue: Integer): SQLResult;
begin
try
  raise EDbxNotSupported.Create('TSqlCursorOdbc.SetOption - not yet supported');
except
  on E: EDbxError do
    begin
    fCursorErrorLines.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

{ TSqlCursorMetaData }

constructor TSqlCursorMetaData.Create(
  OwnerSqlMetaData: TSqlMetaDataOdbc);
begin
  inherited Create;
  fSqlCursorErrorMsg := TStringList.Create;
  fOwnerMetaData := OwnerSqlMetaData;
  fSqlConnectionOdbc := fOwnerMetaData.fOwnerDbxConnection;
  fSqlDriverOdbc := fSqlConnectionOdbc.fOwnerDbxDriver
end;

destructor TSqlCursorMetaData.Destroy;
begin
  fSqlCursorErrorMsg.Free;

  if fMetaCatalogName <> nil then
    FreeMem(fMetaCatalogName);
  if fMetaSchemaName <> nil then
    FreeMem(fMetaSchemaName);
  if fMetaTableName <> nil then
    FreeMem(fMetaTableName);
  inherited;
end;

procedure TSqlCursorMetaData.ParseTableName(TableName: pChar);
var
  dot1, dot2: pChar;
  C_start, C_end, S_start, S_end, T_start, T_end: integer;
  QuoteChar: AnsiChar;
begin
  QuoteChar := fSqlConnectionOdbc.fQuoteChar;
  if fMetaCatalogName <> nil then
    begin
    FreeMem(fMetaCatalogName);
    fMetaCatalogName := nil;
    end;
  if fMetaSchemaName <> nil then
    begin
    FreeMem(fMetaSchemaName);
    fMetaSchemaName := nil;
    end;
  if fMetaTableName <> nil then
    begin
    FreeMem(fMetaTableName);
    fMetaTableName := nil;
    end;

  if (TableName[0] = #0) then exit; // nothing

  dot1 := strpos(TableName, '.');

  C_start := 0;
  C_end := 0;

  S_start := 0;
  S_end := 0;

  T_start := 0;
  T_end := strLen(TableName) - 1;

  if dot1 <> nil then
    begin
    dot2 := strpos(dot1+1, '.');
    if (dot2 = nil) then
      begin
      S_end   := dot1 - TableName - 1;
      T_start := dot1 - TableName + 1;
      end
    else
      begin
      C_end   := dot1 - TableName - 1;
      S_start := dot1 - TableName + 1;
      S_end   := dot2 - TableName - 1;
      T_start := dot2 - TableName + 1;
      end;
    end;

  if (C_end <> 0) then
    begin
    if (TableName[C_start] = QuoteChar) and (TableName[C_end] = QuoteChar) then
      begin
      inc(C_start);
      dec(C_end);
      end;
    fMetaCatalogName := AllocMem(C_end - C_Start + 2);
    StrLCopy(fMetaCatalogName, @TableName[C_start], C_end - C_start + 1);
    end;
  if (S_end <> 0) then
    begin
    if (TableName[S_start] = QuoteChar) and (TableName[S_end] = QuoteChar) then
      begin
      inc(S_start);
      dec(S_end);
      end;
    fMetaSchemaName := AllocMem(S_end - S_Start + 2);
    StrLCopy(fMetaSchemaName, @TableName[S_start], S_end - S_start + 1);
    end;

  if (TableName[T_start] = QuoteChar) and (TableName[T_end] = QuoteChar) then
    begin
    inc(T_start);
    dec(T_end);
    end;
  fMetaTableName := AllocMem(T_end - T_Start + 2);
  StrLCopy(fMetaTableName, @TableName[T_start], T_end - T_start + 1);

end;

procedure TSqlCursorMetaData.DescribeAllocBindString(ColumnNo: SQLUSMALLINT;
  var BindString: pAnsiChar; var BindInd: SQLINTEGER);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  cbColName: SQLSMALLINT;
  szColNameTemp: array[0..255] of char;
  aSqlType: SQLSMALLINT;
  aScale: SQLSMALLINT;
  aNullable: SQLSMALLINT;
  aColSize: SQLUINTEGER;
begin
  OdbcRetCode := SQLDescribeCol(
    fhStmt, ColumnNo, szColNameTemp, 255, cbColName,
    aSqlType, aColSize, aScale, aNullable);
  OdbcCheck(OdbcRetCode, 'SQLDescribeCol');
  BindString := AllocMem(aColSize + 1);
  OdbcRetCode := SQLBindCol(fhStmt, ColumnNo, SQL_C_CHAR, BindString, aColSize + 1, @BindInd);
  OdbcCheck(OdbcRetCode, 'SQLBindCol');
end;


procedure TSqlCursorMetaData.BindSmallint(ColumnNo: SQLUSMALLINT;
  var BindSmallint: Smallint; PBindInd: PSQLINTEGER);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  cbColName: SQLSMALLINT;
  szColNameTemp: array[0..255] of char;
  aSqlType: SQLSMALLINT;
  aScale: SQLSMALLINT;
  aNullable: SQLSMALLINT;
  aColSize: SQLUINTEGER;
begin
  OdbcRetCode := SQLDescribeCol(
    fhStmt, ColumnNo, szColNameTemp, 255, cbColName,
    aSqlType, aColSize, aScale, aNullable);
  OdbcCheck(OdbcRetCode, 'SQLDescribeCol');

  if (aSqlType <> SQL_C_SHORT) then
    raise EDbxInternalError.Create(
      'BindSmallint called for non Smallint column no ' + IntToStr(ColumnNo) + ' - ' + szColNameTemp);
  if (PBindInd = nil) and (aNullable <> OdbcApi.SQL_NO_NULLS) then
    raise EDbxInternalError.Create(
      'BindInteger without indicator var for nullable column ' + IntToStr(ColumnNo) + ' - ' + szColNameTemp);
  OdbcRetCode := SQLBindCol(
   fhStmt, ColumnNo, SQL_C_SHORT, @BindSmallint, Sizeof(Smallint), PBindInd);
  OdbcCheck(OdbcRetCode, 'SQLBindCol');
end;

procedure TSqlCursorMetaData.BindInteger(ColumnNo: SQLUSMALLINT;
  var BindInteger: Integer; BindInd: PSQLINTEGER);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  cbColName: SQLSMALLINT;
  szColNameTemp: array[0..255] of char;
  aSqlType: SQLSMALLINT;
  aScale: SQLSMALLINT;
  aNullable: SQLSMALLINT;
  aColSize: SQLUINTEGER;
begin
  OdbcRetCode := SQLDescribeCol(
    fhStmt, ColumnNo, szColNameTemp, 255, cbColName,
    aSqlType, aColSize, aScale, aNullable);
  OdbcCheck(OdbcRetCode, 'SQLDescribeCol');

  if (aSqlType <> SQL_C_LONG) then
    raise EDbxInternalError.Create('BindInteger called for non Integer column no ' + IntToStr(ColumnNo) + ' - ' + szColNameTemp);
  if (BindInd = nil) and (aNullable <> OdbcApi.SQL_NO_NULLS) then
    raise EDbxInternalError.Create('BindInteger without indicator var for nullable column ' + IntToStr(ColumnNo) + ' - ' + szColNameTemp);
  OdbcRetCode := SQLBindCol(
   fhStmt, ColumnNo, SQL_C_LONG, @BindInteger, Sizeof(Integer), BindInd);
  OdbcCheck(OdbcRetCode, 'SQLBindCol');
end;

function TSqlCursorMetaData.getBcd(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create('getBcd - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.getBlob(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool; Length: LongWord): SQLResult;
begin
try
  raise EDbxInternalError.Create('getBlob - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.getBlobSize(ColumnNumber: Word;
  var Length: LongWord; var IsBlank: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create('getBlobSize - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.getBytes(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create('getBytes - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.getColumnCount(var pColumns: Word): SQLResult;
begin
  pColumns:= fColumnCount;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorMetaData.getColumnLength(ColumnNumber: Word;
  var pLength: LongWord): SQLResult;
begin
try
  raise EDbxInternalError.Create('getColumnLength - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.getColumnName(ColumnNumber: Word;
  pColumnName: PChar): SQLResult;
begin
  StrCopy(pColumnName, PChar(fColumnNames [ColumnNumber-1]));
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorMetaData.getColumnNameLength(ColumnNumber: Word;
  var pLen: Word): SQLResult;
begin
  pLen := Length(fColumnNames [ColumnNumber-1]);
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorMetaData.getColumnPrecision(ColumnNumber: Word;
  var piPrecision: SmallInt): SQLResult;
begin
try
  raise EDbxInternalError.Create('getColumnPrecision - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.getColumnScale(ColumnNumber: Word;
  var piScale: SmallInt): SQLResult;
begin
  piScale := 0;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorMetaData.getColumnType(ColumnNumber: Word; var puType,
  puSubType: Word): SQLResult;
begin
  puType := fColumnTypes [ColumnNumber-1];
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorMetaData.getDate(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create('getDate - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.getDouble(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create('getDouble - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.getErrorMessage(Error: PChar): SQLResult;
begin
  StrCopy(Error, PChar(fSqlCursorErrorMsg.Text));
  fSqlCursorErrorMsg.Clear;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorMetaData.getErrorMessageLen(
  out ErrorLen: SmallInt): SQLResult;
begin
  ErrorLen := Length(fSqlCursorErrorMsg.Text);
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorMetaData.getLong(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create('getLong - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.GetOption(eOption: TSQLCursorOption;
  PropValue: Pointer; MaxLength: SmallInt;
  out Length: SmallInt): SQLResult;
begin
try
  raise EDbxInternalError.Create('GetOption - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.getShort(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create('getShort - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.getString(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create('getString - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.getTime(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create('getTime - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.getTimeStamp(ColumnNumber: Word;
  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create('getTimeStamp - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.isAutoIncrement(ColumnNumber: Word;
  var AutoIncr: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create('isAutoIncrement - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.isBlobSizeExact(ColumnNumber: Word;
  var IsExact: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create('isBlobSizeExact - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.isNullable(ColumnNumber: Word;
  var Nullable: LongBool): SQLResult;
begin
  Nullable := False;
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorMetaData.isReadOnly(ColumnNumber: Word;
  var ReadOnly: LongBool): SQLResult;
begin
  ReadOnly := True;  // Cannot update metadata directly
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorMetaData.isSearchable(ColumnNumber: Word;
  var Searchable: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create('isSearchable - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaData.next: SQLResult;
begin
  inc(fRowNo);
  Result := DBXpress.SQL_SUCCESS;
end;

function TSqlCursorMetaData.SetOption(eOption: TSQLCursorOption;
  PropValue: Integer): SQLResult;
begin
try
  raise EDbxInternalError.Create('SetOption - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;


procedure TSqlCursorMetaData.OdbcCheck(OdbcCode: SQLRETURN;
  OdbcFunctionName: string);
begin
  fSqlDriverOdbc.OdbcCheck(OdbcCode, OdbcFunctionName, SQL_HANDLE_STMT, fhStmt);
end;

{ TOdbcBindCol }

constructor TOdbcBindCol.Create;
begin
  inherited;
end;

destructor TOdbcBindCol.Destroy;
begin
  FreeMem(fColName);
  FreeMem(fpBuffer);
  inherited;
end;

{ TOdbcBindParam }

constructor TOdbcBindParam.Create;
begin
  inherited
end;

destructor TOdbcBindParam.Destroy;
begin
  if (fBuffer <> nil) then
    FreeMem(fBuffer);
  inherited;
end;

{ TMetaTable }

constructor TMetaTable.Create(
  SqlConnectionOdbc: TSqlConnectionOdbc;
  Cat: pAnsiChar;
  Schema: pAnsiChar;
  TableName: pAnsiChar;
  TableType: integer);
var
  aCatLen: integer;
  aSchemaLen: integer;
  aTableLen: integer;

  WantCatalog: boolean;
  WantSchema: boolean;
  QuoteChar: AnsiChar;

begin
  aCatLen := 0;
  QuoteChar := SqlConnectionOdbc.fQuoteChar;

  if (Cat <> nil) then
    begin
    aCatLen := strLen(Cat);
    fCat := AllocMem(aCatLen + 1);
    StrCopy(fCat, Cat);
    end;
  aSchemaLen := 0;
  if (Schema <> nil) then
    begin
    aSchemaLen := strLen(Schema);
    if (aSchemaLen <> 0) then
      begin
      fSchema := AllocMem(aSchemaLen + 1);
      StrCopy(fSchema, Schema);
      end;
    end;

  aTableLen := strLen(TableName);
  fTableName := AllocMem(aTableLen + 1);
  StrCopy(fTableName, TableName);

  WantCatalog := true;
  WantSchema := true;

{  if (TableType = eSQLSynonym) then
    begin
    WantCatalog := false;
    WantSchema := false;
    end; }

  if (aCatLen = 0) or  (not SqlConnectionOdbc.fSupportsSchemaDML) then
    WantCatalog := false
  else
    if (strCompNil(SqlConnectionOdbc.fCurrentCatalog,Cat) = 0) then
      WantCatalog := false;

  if (aSchemaLen = 0) or (not SqlConnectionOdbc.fSupportsSchemaDML) then
    WantSchema := false;

  if WantCatalog then
    if WantSchema then
      begin
      // 3-part name (Catalog.Schema.Table)
      if SqlConnectionOdbc.fWantQuotedTableName then
        begin
        fQualifiedTableName := AllocMem(aTableLen + aSchemaLen + aCatLen + 3 + 6);
        StrCopy(fQualifiedTableName, pChar(QuoteChar + string(Cat) + QuoteChar
          + '.' + QuoteChar + string(Schema) + QuoteChar
          + '.' + QuoteChar + string(TableName) + QuoteChar));
        end
      else
        begin
        fQualifiedTableName := AllocMem(aTableLen + aSchemaLen + aCatLen + 3);
        StrCopy(fQualifiedTableName, pChar(string(Cat) + '.' + string(Schema) + '.' + string(TableName)));
        end
      end
    else
      begin
      // 3-part name, but schema omitted (Catalog..Schema)
      if SqlConnectionOdbc.fWantQuotedTableName then
        begin
        fQualifiedTableName := AllocMem(aTableLen + aCatLen + 3 + 4);
        StrCopy(fQualifiedTableName, pChar(QuoteChar + string(Cat) + QuoteChar +
         '..' + QuoteChar + string(TableName) + QuoteChar));
        end
      else
        begin
        fQualifiedTableName := AllocMem(aTableLen + aCatLen + 3);
        StrCopy(fQualifiedTableName, pChar(string(Cat) + '..' + string(TableName)));
        end;
      end
  else
    if WantSchema then
      begin
      // 2-part name (Schema.Table)
      if SqlConnectionOdbc.fWantQuotedTableName then
        begin
        fQualifiedTableName := AllocMem(aTableLen + aSchemaLen + 2 + 4);
        StrCopy(fQualifiedTableName, pChar(QuoteChar + string(Schema) + QuoteChar
         + '.' + QuoteChar + string(TableName) + QuoteChar));
        end
      else
        begin
        fQualifiedTableName := AllocMem(aTableLen + aSchemaLen + 2);
        StrCopy(fQualifiedTableName, pChar(string(Schema) + '.' + string(TableName)));
        end;
      end
    else
      begin
      // 1-part name (Table only)
      if SqlConnectionOdbc.fWantQuotedTableName then
        begin
        fQualifiedTableName := AllocMem(aTableLen + 1 + 2);
        StrCopy(fQualifiedTableName, pChar(QuoteChar + string(TableName) + QuoteChar));
        end
      else
        begin
        fQualifiedTableName := AllocMem(aTableLen + 1);
        StrCopy(fQualifiedTableName, TableName);
        end;
      end;

  fTableType := TableType;
end;

destructor TMetaTable.Destroy;
begin
  FreeMem(fCat);
  FreeMem(fSchema);
  FreeMem(fTableName);
  FreeMem(fQualifiedTableName);
  fIndexColumnList.Free;
  inherited;
end;

{ TMetaColumn }

constructor TMetaColumn.Create(
  ColumnName: pAnsiChar;
  OrdinalPosition: smallint;
  TypeName: pAnsiChar);
var
  aLen: integer;
begin
  aLen := strLen(ColumnName);
  fColumnName := AllocMem(aLen + 1);
  StrCopy(fColumnName, ColumnName);

  aLen := strLen(TypeName);
  fTypeName := AllocMem(aLen + 1);
  StrCopy(fTypeName, TypeName);

  fOrdinalPosition := OrdinalPosition;
end;

destructor TMetaColumn.Destroy;
begin
  FreeMem(fColumnName);
  FreeMem(fTypeName);
  inherited;
end;

{ TMetaIndexColumn }

constructor TMetaIndexColumn.Create(MetaTable: TMetaTable; IndexName, IndexColumnName: pAnsiChar);
var
  aLen: integer;
begin
  fMetaTable:= MetaTable;

  aLen := strLen(IndexName);
  fIndexName := AllocMem(aLen + 1);
  StrCopy(fIndexName, IndexName);

  aLen := strLen(IndexColumnName);
  fIndexColumnName := AllocMem(aLen + 1);
  StrCopy(fIndexColumnName, IndexColumnName);
end;

destructor TMetaIndexColumn.Destroy;
begin
  FreeMem(fIndexName);
  FreeMem(fIndexColumnName);
  FreeMem(fFilter);
  inherited;
end;

{ TMetaProcedure }

constructor TMetaProcedure.Create(Cat, Schema, ProcName: pAnsiChar;
  ProcType: integer);
var
  aLen: integer;
begin
  if (Cat <> nil) then
    begin
    aLen := strLen(Cat);
    fCat := AllocMem(aLen + 1);
    StrCopy(fCat, Cat);
    end;
  if (Schema <> nil) then
    begin
    aLen := strLen(Schema);
    fSchema := AllocMem(aLen + 1);
    StrCopy(fSchema, Schema);
    end;
  aLen := strLen(ProcName);
  fProcName := AllocMem(aLen + 1);
  StrCopy(fProcName, ProcName);
  fProcType := ProcType;
end;

destructor TMetaProcedure.Destroy;
begin
  FreeMem(fCat);
  FreeMem(fSchema);
  FreeMem(fProcName);
  inherited;
end;

{ TMetaProcedureParam }

constructor TMetaProcedureParam.Create(ParamName: pAnsiChar);
var
  aLen: integer;
begin
  aLen := strLen(ParamName);
  fParamName := AllocMem(aLen + 1);
  StrCopy(fParamName, ParamName);
end;

destructor TMetaProcedureParam.Destroy;
begin
  FreeMem(fParamName);
  FreeMem(fDataTypeName);
  inherited;
end;

{ TSqlCursorTables }
{
 Dbx returned cursor columns
  1.RECNO         fldINT32    A record number that uniquely identifies each record.
  2.CATALOG_NAME  fldZSTRING  The name of the catalog (database) that contains the table.
  3.SCHEMA_NAME   fldZSTRING  The name of the schema that identifies the owner of the table.
  4.TABLE_NAME    fldZSTRING  The name of the table.
  5.TABLE_TYPE    fldINT32    An eSQLTableType value (C++) or table type constant (Object Pascal) that indicates the type of table.

 ODBC result set columns
  1.TABLE_CAT     Varchar     Catalog name; NULL if not applicable to the data source
  2.TABLE_SCHEM   Varchar     Schema name; NULL if not applicable to the data source.
  3.TABLE_NAME    Varchar     Table name
  4.TABLE_TYPE    Varchar     Table type name eg TABLE, VIEW, SYNONYM, ALIAS etc
  5.REMARKS       Varchar     A description of the table
}

const
  TableColumnNames: array [1..5] of string =
    ('RECNO', 'CATALOG_NAME', 'SCHEMA_NAME', 'TABLE_NAME', 'TABLE_TYPE');
  TableColumnTypes: array [1..5] of word =
    (fldINT32, fldZSTRING, fldZSTRING, fldZSTRING, fldINT32);
  TableColumnCount = length(TableColumnNames);

constructor TSqlCursorMetaDataTables.Create(
  OwnerMetaData: TSQLMetaDataOdbc);
begin
  inherited Create(OwnerMetaData);

  fColumnCount := TableColumnCount;
  fColumnNames := @TableColumnNames;
  fColumnTypes := @TableColumnTypes;
end;

destructor TSqlCursorMetaDataTables.Destroy;
var
  i: integer;
begin
  if Assigned(fTableList) then
    begin
    for i := fTableList.Count - 1 downto 0 do
      TMetaTable(fTableList[i]).Free;
    fTableList.Free;
    end;
  inherited;
end;

procedure TSqlCursorMetaDataTables.FetchTables(
  SearchTableName: PChar;
  SearchTableType: LongWord);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  TableTypes: string;
  sTableTypes: pChar;

  Cat: pAnsiChar;
  Schema: pAnsiChar;
  TableName: pAnsiChar;
  OdbcTableType: pAnsiChar;
  DbxTableType: integer;

  cbCat: SQLINTEGER;
  cbSchema: SQLINTEGER;
  cbTableName: SQLINTEGER;
  cbOdbcTableType: SQLINTEGER;

  aMetaTable: TMetaTable;
  i: integer;
begin
  Cat := nil;
  Schema := nil;
  TableName := nil;
  OdbcTableType := nil;

try
  fSqlConnectionOdbc.AllocHStmt(fhStmt);

  TableTypes := '';
  if (SearchTableType and eSQLTable) <> 0 then
    TableTypes := 'TABLE, ';
  if (SearchTableType and eSQLView) <> 0 then
    TableTypes := TableTypes + 'VIEW, ';
  if (SearchTableType and eSQLSystemTable) <> 0 then
    TableTypes := TableTypes + 'SYSTEM TABLE, ';
  if (SearchTableType and eSQLSynonym) <> 0 then
    TableTypes := TableTypes + 'SYNONYM, ';
  if (SearchTableType and eSQLTempTable) <> 0 then
    TableTypes := TableTypes + 'GLOBAL TEMPORARY, ';
  if (SearchTableType and eSQLLocal) <> 0 then
    TableTypes := TableTypes + 'LOCAL TEMPORARY, ';
  if TableTypes = '' then
    sTableTypes := nil
  else
    begin
    TableTypes := Copy(TableTypes, 1, Length(TableTypes) - 2); // remove trailing comma
    sTableTypes := pAnsiChar(TableTypes);
    end;

  fSqlConnectionOdbc.GetCurrentCatalog;
  
  OdbcRetCode := SQLTables(fhStmt,
  nil, 0, // all catalogs
  nil, 0, // all schemas
  SearchTableName, SQL_NTS, // Table name match pattern
  sTableTypes, SQL_NTS); // Table types

  OdbcCheck(OdbcRetCode, 'SQLTables');

  if fSqlConnectionOdbc.fSupportsCatalog then
    DescribeAllocBindString(1, Cat, cbCat);
  DescribeAllocBindString(2, Schema, cbSchema);
  DescribeAllocBindString(3, TableName, cbTableName);
  DescribeAllocBindString(4, OdbcTableType, cbOdbcTableType);

  fTableList:= TList.Create;

  OdbcRetCode := SQLFetch(fhStmt);

  while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
    begin
    OdbcCheck(OdbcRetCode, 'SQLFetch');

    if (OdbcTableType = 'TABLE') then
      DbxTableType := eSQLTable
    else if (OdbcTableType = 'VIEW') then
      DbxTableType := eSQLView
    else if (OdbcTableType = 'SYNONYM') or
            (OdbcTableType = 'ALIAS') then
      // in IBM DB2, Alias is evivalent to Synonym
      DbxTableType := eSQLSynonym
    else if (OdbcTableType = 'SYSTEM TABLE') then
      DbxTableType := eSQLSystemTable
    else if (OdbcTableType = 'GLOBAL TEMPORARY') then
      DbxTableType := eSQLTempTable
    else if (OdbcTableType = 'LOCAL TEMPORARY') then
      DbxTableType := eSQLLocal
    else
      // Database-specific table type - assume its a table
      DbxTableType := eSQLTable;

    aMetaTable := TMetaTable.Create(fSqlConnectionOdbc, Cat, Schema, TableName, DbxTableType);
    fTableList.Add(aMetaTable);

    OdbcRetCode := SQLFetch(fhStmt);
    end;

  fCatLenMax := 0;
  fSchemaLenMax := 0;
  fQualifiedTableLenMax := 1;

  for i := 0 to fTableList.Count - 1 do
    begin
    aMetaTable := TMetaTable(fTableList.Items[i]);
    if aMetaTable.fCat <> nil then
      MaxSet(fCatLenMax, strLen(aMetaTable.fCat));
    if aMetaTable.fSchema <> nil then
      MaxSet(fCatLenMax, strLen(aMetaTable.fSchema));
    MaxSet(fQualifiedTableLenMax, strLen(aMetaTable.fQualifiedTableName));
    end;

finally
  FreeMem(Cat);
  FreeMem(Schema);
  FreeMem(TableName);
  FreeMem(OdbcTableType);

  if fhStmt <> nil then
    fSqlConnectionOdbc.FreeHStmt(fhStmt);
end;

end;

function TSqlCursorMetaDataTables.getColumnLength(ColumnNumber: Word;
  var pLength: LongWord): SQLResult;
begin
try
  case ColumnNumber of
    1:  // RECNO
      pLength := SizeOf(Integer);
    2:  // CATALOG_NAME
      pLength := fCatLenMax;
    3:  // SCHEMA_NAME
      pLength := fSchemaLenMax;
    4:  // TABLE_NAME
      pLength := fQualifiedTableLenMax;
    5: // TABLE_TYPE
      pLength := SizeOf(Integer);
  else
    begin
    raise EDbxInvalidCall.Create(
      'TSqlCursorMetaDataTables.getColumnLength invalid column no: ' + IntToStr(ColumnNumber));
    end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataTables.getColumnPrecision(ColumnNumber: Word;
  var piPrecision: SmallInt): SQLResult;
var
  Length: LongWord;
begin
  Result := getColumnLength(ColumnNumber, Length);
  piPrecision := Length;
end;

function TSqlCursorMetaDataTables.getLong(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  case ColumnNumber of
    1: // RECNO
      begin
      integer(Value^) := fRowNo;
      IsBlank := False;
      end;
    5: // TABLE_TYPE
      begin
      integer(Value^) := fMetaTableCurrent.fTableType;
      IsBlank := False;
      end;
    else
      begin
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataTables.getLong not valid for column ' + IntToStr(ColumnNumber));
      end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataTables.getString(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  case ColumnNumber of
    2:  // CATALOG_NAME
      begin
      if (fMetaTableCurrent.fCat = nil) then
        IsBlank := true
      else
        begin
        StrCopy(Value, pChar(fMetaTableCurrent.fCat));
        IsBlank := False;
        end;
      end;
    3:  // SCHEMA_NAME
      begin
      if (fMetaTableCurrent.fSchema = nil) then
        IsBlank := true
      else
        begin
        StrCopy(Value, pChar(fMetaTableCurrent.fSchema));
        IsBlank := False;
        end;
      end;
    4:  // TABLE_NAME
      begin
      StrCopy(Value, pChar(fMetaTableCurrent.fQualifiedTableName));
      IsBlank := False;
      end;
    else
      begin
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataTables.getString not valid for column ' + IntToStr(ColumnNumber));
      end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataTables.next: SQLResult;
begin
  inc(fRowNo);
  if fRowNo > fTableList.Count then
    begin
    Result := DBXERR_EOF;
    exit;
    end;
  fMetaTableCurrent := fTableList[fRowNo-1];
  Result := DBXpress.SQL_SUCCESS;
end;

{ TSqlCursorColumns }

{
1.  RECNO            fldINT32    A record number that uniquely identifies each record.
2.  CATALOG_NAME     fldZSTRING  The name of the catalog (database) that contains the table.
3.  SCHEMA_NAME      fldZSTRING  The name of the schema that identifies the owner of the table.
4.  TABLE_NAME       fldZSTRING  The name of the table in which the column appears.
5.  COLUMN_NAME      fldZSTRING  The name of the field (column).
6.  COLUMN_POSITION  fldINT16    The position of the column in its table.
7.  COLUMN_TYPE      fldINT32    An eSQLColType value (C++) or column type constant (Object Pascal) that indicates the type of field.
8.  COLUMN_DATATYPE  fldINT16    The logical data type for the field.
9.  COLUMN_TYPENAME  fldZSTRING  A string describing the datatype.
      This is the same information as contained in COLUMN_DATATYPE and COLUMN_SUBTYPE, but in a form used in some DDL statements.
10. COLUMN_SUBTYPE   fldINT16    The logical data subtype for the field.
11. COLUMN_PRECISION fldINT32    The size of the field type (number of characters in a string, bytes in a bytes field, significant digits in a BCD value, members of an ADT field, and so on)
12. COLUMN_SCALE     fldINT16    The number of digits to the right of the decimal on BCD values, or descendants on ADT and array fields.
13. COLUMN_LENGTH    fldINT32    The number of bytes required to store field values.
14. COLUMN_NULLABLE  fldINT16    If the field requires a value, nonzero if it can be blank.

ODBC result set columns
1.  TABLE_CAT         Varchar            Catalog name; NULL if not applicable to the data source
2.  TABLE_SCHEM       Varchar            Schema name; NULL if not applicable to the data source.
3.  TABLE_NAME        Varchar            Table name
4.  COLUMN_NAME       Varchar not NULL   Column name. Empty string for a column that does not have a name
5.  DATA_TYPE         Smallint not NULL  SQL data type
6.  TYPE_NAME         Varchar not NULL   Data source – dependent data type name
7.  COLUMN_SIZE       Integer            Column Size
     If DATA_TYPE is SQL_CHAR or SQL_VARCHAR, then this column contains the maximum length in characters of the column
     For datetime data types, this is the total number of characters required to display the value when converted to characters.
     For numeric data types, this is either the total number of digits
     or the total number of bits allowed in the column, according to the NUM_PREC_RADIX column
8.  BUFFER_LENGTH     Integer            The length in bytes of data transferred on SqlFetch etc if SQL_C_DEFAULT is specified
9.  DECIMAL_DIGITS    Smallint           The total number of significant digits to the right of the decimal point
10. NUM_PREC_RADIX    Smallint           For numeric data types, either 10 or 2.
11. NULLABLE          Smallint not NULL  SQL_NO_NULLS / SQL_NULLABLE / SQL_NULLABLE_UNKNOWN
12. REMARKS           Varchar            A description of the column
13. COLUMN_DEF        Varchar            The default value of the column
14. SQL_DATA_TYPE     Smallint not NULL  SQL data type,
     This column is the same as the DATA_TYPE column, with the exception of
     datetime and interval data types.
     This column returns the nonconcise data type (such as SQL_DATETIME or SQL_INTERVAL),
     rather than the concise data type (such as SQL_TYPE_DATE or SQL_INTERVAL_YEAR_TO_MONTH)
15. SQL_DATETIME_SUB  Smallint           The subtype code for datetime and interval data types. For other data types, this column returns a NULL.
16. CHAR_OCTET_LENGTH Integer            The maximum length in bytes of a character or binary data type column.
17. ORDINAL_POSITION  Integer not NULL   The ordinal position of the column in the table
18. IS_NULLABLE       Varchar            'NO' if the column does not include NULLs / 'YES' if the column could include NULLs / zero-length string if nullability is unknown.
}
const
  ColumnColumnNames: array [1..14] of string =
    ('RECNO', 'CATALOG_NAME', 'SCHEMA_NAME', 'TABLE_NAME', 'COLUMN_NAME',
     'COLUMN_POSITION', 'COLUMN_TYPE', 'COLUMN_DATATYPE', 'COLUMN_TYPENAME', 'COLUMN_SUBTYPE',
     'COLUMN_PRECISION', 'COLUMN_SCALE', 'COLUMN_LENGTH', 'COLUMN_NULLABLE');
  ColumnColumnTypes: array [1..14] of word =
    (fldINT32, fldZSTRING, fldZSTRING, fldZSTRING, fldZSTRING,
     fldINT16, fldINT32, fldINT16, fldZSTRING, fldINT16,
     fldINT32, fldINT16, fldINT32, fldINT16);
  ColumnColumnCount = length(ColumnColumnNames);

constructor TSqlCursorMetaDataColumns.Create(
  OwnerMetaData: TSQLMetaDataOdbc);
begin
  inherited Create(OwnerMetaData);
  fColumnCount := ColumnColumnCount;
  fColumnNames := @ColumnColumnNames;
  fColumnTypes := @ColumnColumnTypes;
end;

destructor TSqlCursorMetaDataColumns.Destroy;
var
  i: integer;
begin
  if Assigned(fTableList) then
    begin
    for i := fTableList.Count - 1 downto 0 do
      TMetaTable(fTableList[i]).Free;
    fTableList.Free;
    end;
  if Assigned(fColumnList) then
    begin
    for i := fColumnList.Count - 1 downto 0 do
      TMetaColumn(fColumnList[i]).Free;
    fColumnList.Free;
    end;
  inherited;
end;

procedure TSqlCursorMetaDataColumns.FetchColumns(
  SearchTableName,
  SearchColumnName: PChar;
  SearchColType: LongWord);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  Cat: pAnsiChar;
  Schema: pAnsiChar;
  TableName: pAnsiChar;
  ColumnName: pAnsiChar;
  TypeName: pAnsiChar;
  DefaultValue: pAnsiChar;
  OrdinalPosition: integer;
  OdbcDataType: smallint;
  Nullable: smallint;
  OdbcColumnSize: integer;
  DecimalScale: smallint;
  OdbcRadix: smallint;
  OdbcColumnBufferLength: integer;

  cbCat: integer;
  cbSchema: integer;
  cbTableName: integer;
  cbColumnName: integer;
  cbTypeName: integer;
  cbDefaultValue: integer;
  cbDecimalScale: integer; // allow for NULL values
  cbOdbcDataType: integer;
  cbOdbcColumnSize: integer;
  cbOdbcRadix: integer;
  cbNullable: integer;
  cbOrdinalPosition: integer;
  cbOdbcColumnBufferLength: integer;

  i: integer;
  aMetaTable: TMetaTable;
  aMetaColumn: TMetaColumn;
  bTableFound: boolean;

begin
  Cat := nil;
  Schema := nil;
  TableName := nil;
  ColumnName := nil;
  TypeName := nil;

try
  fSqlConnectionOdbc.AllocHStmt(fhStmt);
  ParseTableName(SearchTableName);

  if (SearchColumnName <> nil) then
    if (SearchColumnName[0] = #0) then
      SearchColumnName := nil;
  OdbcRetCode := SQLColumns(fhStmt,
   fMetaCatalogName, SQL_NTS, // Catalog
   fMetaSchemaName, SQL_NTS, // Schema
   fMetaTableName, SQL_NTS, // Table name match pattern
   SearchColumnName, SQL_NTS); // Column name match pattern

  OdbcCheck(OdbcRetCode, 'SQLColumns');

  if fSqlConnectionOdbc.fSupportsCatalog then
    DescribeAllocBindString(1, Cat, cbCat);
  if (fSqlConnectionOdbc.fOdbcMaxSchemaNameLen > 0) then
    DescribeAllocBindString(2, Schema, cbSchema);
  DescribeAllocBindString(3, TableName, cbTableName);
  DescribeAllocBindString(4, ColumnName, cbColumnName);
  BindSmallint(5, OdbcDataType, @cbOdbcDataType);
  DescribeAllocBindString(6, TypeName, cbTypeName);
  BindInteger(7, OdbcColumnSize, @cbOdbcColumnSize);
  BindInteger(8, OdbcColumnBufferLength, @cbOdbcColumnBufferLength);
  BindSmallint(9, DecimalScale, @cbDecimalScale);
  BindSmallint(10, OdbcRadix, @cbOdbcRadix);
  BindSmallint(11, Nullable, @cbNullable);
  // Level 2 Drivers do not support Oridinal Position
  if (fSqlConnectionOdbc.fOdbcDriverLevel = 2) then
    begin
    DefaultValue := nil;
    OrdinalPosition := 0;
    end
  else
    begin
    DescribeAllocBindString(13, DefaultValue, cbDefaultValue);
    BindInteger(17, OrdinalPosition, nil);
    end;
  fTableList:= TList.Create;
  fColumnList:= TList.Create;

  OdbcRetCode := SQLFetch(fhStmt);

  while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
    begin

    OdbcCheck(OdbcRetCode, 'SQLFetch');

    bTableFound := false;
    aMetaTable := nil;  // suppress compiler warning
    for i := 0 to fTableList.Count - 1 do
      begin
      aMetaTable := fTableList.Items[i];
      if (strCompNil(aMetaTable.fCat, Cat) = 0)
      and (strCompNil(aMetaTable.fSchema, Schema) = 0)
      and (strCompNil(aMetaTable.fTableName, TableName) = 0) then
        begin
        bTableFound := true;
        break;
        end;
      end;
    if not bTableFound then
      begin
      aMetaTable := TMetaTable.Create(fSqlConnectionOdbc, Cat, Schema, TableName, eSQLTable);
      fTableList.Add(aMetaTable);
      end;

    aMetaColumn := TMetaColumn.Create(ColumnName, OrdinalPosition, TypeName);
    fColumnList.Add(aMetaColumn);
    aMetaColumn.fMetaTable := aMetaTable;
    if (cbOdbcColumnBufferLength = OdbcAPi.SQL_NULL_DATA) then
      aMetaColumn.fLength := Low(integer) // this indicates null data
    else
      aMetaColumn.fLength := OdbcColumnBufferLength;

    if cbDecimalScale = OdbcAPi.SQL_NULL_DATA then
      aMetaColumn.fDecimalScale := Low(smallint) // this indicates null data
    else
      aMetaColumn.fDecimalScale := DecimalScale;
    if cbOdbcColumnSize = OdbcAPi.SQL_NULL_DATA then
      aMetaColumn.fPrecision := Low(smallint)  // this indicates null data
    else
      begin
      if (cbOdbcRadix <> OdbcAPi.SQL_NULL_DATA) and (OdbcRadix = 2) then
      // if RADIX = 2, Odbc column size is number of BITs; Decimal Digits is log10(2) * BITS = 0.30103 * No of BITS
        aMetaColumn.fPrecision := ((OdbcColumnSize * 3) div 10) + 1
      else
        aMetaColumn.fPrecision := OdbcColumnSize
      end;
    case Nullable of
    SQL_NULLABLE:
      aMetaColumn.fDbxNullable := 1;  // it can be null
    SQL_NO_NULLS:
      aMetaColumn.fDbxNullable := 0;  // null not allowed
    SQL_NULLABLE_UNKNOWN:
      aMetaColumn.fDbxNullable := 1;  // Odbc doesn't know - assume it might contain nulls
    end;
    OdbcDataTypeToDbxType(OdbcDataType, aMetaColumn.fDbxType, aMetaColumn.fDbxSubType);

{ Dbx Column type is combination of following flags
eSQLRowId	Row Id number.
eSQLRowVersion	Version number.
eSQLAutoIncr	Auto-incrementing field (server generates value).
eSQLDefault	Field with a default value. (server can generate value)

eSQLRowId      - This can be determined by Odbc call SQLSpecialColumns SQL_BEST_ROWID
eSQLRowVersion - This can be determined by Odbc call SQLSpecialColumns SQL_ROWVER
eSQLAutoIncr   - Odbc does not have facility to determine this until actual result set
eSQLDefault    - Odbc will retrun the defaulkt value
}
    if (cbDefaultValue <> OdbcAPi.SQL_NULL_DATA) then
      aMetaColumn.fDbxColumnType := aMetaColumn.fDbxColumnType + eSQLDefault;
    OdbcRetCode := SQLFetch(fhStmt);
    end;

  OdbcRetCode := SQLCloseCursor(fhstmt);
  OdbcCheck(OdbcRetCode, 'CloseCursor');
  OdbcRetCode := SQLFreeStmt (fhStmt, SQL_UNBIND);
  OdbcCheck(OdbcRetCode, 'SQLFreeStmt - SQL_UNBIND');

// Next block of code to determine eSQLRowId and eSQLRowVersion
{// But there's no point, DbExpress does not use this information

// This is to determine eSQLRowId
  OdbcRetCode := SQLSpecialColumns(fhStmt,
   SQL_BEST_ROWID,
   fMetaCatalogName, SQL_NTS, // Catalog
   fMetaSchemaName, SQL_NTS, // Schema
   fMetaTableName, SQL_NTS, // Table name match pattern
   SQL_SCOPE_TRANSACTION, // Minimum required scope of the rowid
   SQL_NULLABLE); // Return even if column can be null
  OdbcCheck(OdbcRetCode, 'SQLSpecialColumns');

  DescribeAllocBindString(2, ColumnName, cbColumnName);
  BindSmallint(3, OdbcDataType, @cbOdbcDataType);
  DescribeAllocBindString(4, TypeName, cbTypeName);
  BindInteger(5, OdbcColumnSize, @cbOdbcColumnSize);

  OdbcRetCode := SQLFetch(fhStmt);

  while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
    begin
    OdbcCheck(OdbcRetCode, 'SQLFetch');

    for i := 0 to fColumnList.Count - 1 do
      begin
      aMetaColumn := TMetaColumn(fColumnList.Items[i]);
      if StrComp(aMetaColumn.fColumnName, ColumnName) = 0 then
        aMetaColumn.fDbxColumnType := aMetaColumn.fDbxColumnType + eSQLRowId;
      end;

    OdbcRetCode := SQLFetch(fhStmt);
    end;

  OdbcRetCode := SQLCloseCursor(fhstmt);
  OdbcCheck(OdbcRetCode, 'CloseCursor');
  OdbcRetCode := SQLFreeStmt (fhStmt, SQL_UNBIND);
  OdbcCheck(OdbcRetCode, 'SQLFreeStmt - SQL_UNBIND');

// This is to determine eSQLRowVersion
  OdbcRetCode := SQLSpecialColumns(fhStmt,
   SQL_ROWVER,
   fMetaCatalogName, SQL_NTS, // Catalog
   fMetaSchemaName, SQL_NTS, // Schema
   fMetaTableName, SQL_NTS, // Table name match pattern
   0, // Does not apply to SQL_ROWVER
   SQL_NULLABLE); // Return even if column can be null
  OdbcCheck(OdbcRetCode, 'SQLSpecialColumns');

  DescribeAllocBindString(2, ColumnName, cbColumnName);
  BindSmallint(3, OdbcDataType, @cbOdbcDataType);
  DescribeAllocBindString(4, TypeName, cbTypeName);
  BindInteger(5, OdbcColumnSize, @cbOdbcColumnSize);

  OdbcRetCode := SQLFetch(fhStmt);

  while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
    begin
    OdbcCheck(OdbcRetCode, 'SQLFetch');

    for i := 0 to fColumnList.Count - 1 do
      begin
      aMetaColumn := TMetaColumn(fColumnList.Items[i]);
      if StrComp(aMetaColumn.fColumnName, ColumnName) = 0 then
        aMetaColumn.fDbxColumnType := aMetaColumn.fDbxColumnType + eSQLRowVersion;
      end;

    OdbcRetCode := SQLFetch(fhStmt);
    end;

  OdbcRetCode := SQLCloseCursor(fhstmt);
  OdbcCheck(OdbcRetCode, 'CloseCursor');
  OdbcRetCode := SQLFreeStmt (fhStmt, SQL_UNBIND);
  OdbcCheck(OdbcRetCode, 'SQLFreeStmt - SQL_UNBIND');
//}

  fCatLenMax := 0;
  fSchemaLenMax := 0;
  fTableLenMax := 1;
  fColumnLenMax := 1;

  for i := 0 to fTableList.Count - 1 do
    begin
    aMetaTable := TMetaTable(fTableList.Items[i]);
    if aMetaTable.fCat <> nil then
      MaxSet(fCatLenMax, strLen(aMetaTable.fCat));
    if aMetaTable.fSchema <> nil then
      MaxSet(fCatLenMax, strLen(aMetaTable.fSchema));
    MaxSet(fTableLenMax, strLen(aMetaTable.fTableName));
    end;

  for i := 0 to fColumnList.Count - 1 do
    begin
    aMetaColumn := TMetaColumn(fColumnList.Items[i]);
    MaxSet(fColumnLenMax, strLen(aMetaColumn.fColumnName));
    MaxSet(fTypeNameLenMax, strLen(aMetaColumn.fTypeName));
    end;

finally
  FreeMem(Cat);
  FreeMem(Schema);
  FreeMem(TableName);
  FreeMem(ColumnName);
  FreeMem(TypeName);
  FreeMem(DefaultValue);

  if fhStmt <> nil then
    fSqlConnectionOdbc.FreeHStmt(fhStmt);
end;
end;


function TSqlCursorMetaDataColumns.getColumnLength(ColumnNumber: Word;
  var pLength: LongWord): SQLResult;
begin
try
  case ColumnNumber of
    1:  // RECNO
      pLength := SizeOf(Integer);
    2:  // CATALOG_NAME
      pLength := fCatLenMax;
    3:  // SCHEMA_NAME
      pLength := fSchemaLenMax;
    4:  // TABLE_NAME
      pLength := fTableLenMax;
    5: // COLUMN_NAME
      pLength := fColumnLenMax;
    6: // COLUMN_POSITION   fldINT16
      pLength := SizeOf(Smallint);
    7: // COLUMN_TYPE       fldINT32
      pLength := SizeOf(LongInt);
    8: // COLUMN_DATATYPE   fldINT16
      pLength := SizeOf(Smallint);
    9: // COLUMN_TYPENAME
      pLength := fTypeNameLenMax;
    10: // COLUMN_SUBTYPE   fldINT16
      pLength := SizeOf(Smallint);
    11: // COLUMN_PRECISION fldINT32
      pLength := SizeOf(LongInt);
    12: // COLUMN_SCALE     fldINT16
      pLength := SizeOf(Smallint);
    13: // COLUMN_LENGTH    fldINT32
      pLength := SizeOf(LongInt);
    14: // COLUMN_NULLABLE  fldINT16
      pLength := SizeOf(Smallint);
  else
    begin
    raise EDbxInvalidCall.Create(
      'TSqlCursorMetaDataColumns.getColumnLength invalid column no: ' + IntToStr(ColumnNumber));
    end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataColumns.getColumnPrecision(ColumnNumber: Word;
  var piPrecision: SmallInt): SQLResult;
var
  Length: LongWord;
begin
  Result := getColumnLength(ColumnNumber, Length);
  piPrecision := Length;
end;

function TSqlCursorMetaDataColumns.getLong(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  case ColumnNumber of
    1: // RECNO
      begin
      integer(Value^) := fRowNo;
      IsBlank := False;
      end;
    7:  // COLUMN_TYPE      fldINT32
      begin
      IsBlank := false;
      integer(Value^) := fMetaColumnCurrent.fDbxColumnType
      end;
    11: // COLUMN_PRECISION  fldINT32
      begin
      if fMetaColumnCurrent.fPrecision = low(integer) then
        begin
        IsBlank := true;
        integer(Value^) := 0;
        end
      else
        begin
        IsBlank := false;
        integer(Value^) := fMetaColumnCurrent.fPrecision;
        end;
      end;
    13: // COLUMN_LENGTH    fldINT32
      begin
      if fMetaColumnCurrent.fLength = low(integer) then
        begin
        IsBlank := true;
        integer(Value^) := 0;
        end
      else
        begin
        IsBlank := false;
        integer(Value^) := fMetaColumnCurrent.fLength;
        end;
      end;
    else
      begin
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataColumns.getLong not valid for column ' + IntToStr(ColumnNumber));
      end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataColumns.getShort(ColumnNumber: Word;
  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
try
  case ColumnNumber of
    6:  // COLUMN_POSITION    fldINT16
      begin
      smallint(Value^) := fMetaColumnCurrent.fOrdinalPosition;
      IsBlank := False;
      end;
    8:  // COLUMN_DATATYPE  fldINT16
      begin
      smallint(Value^) := fMetaColumnCurrent.fDbxType;
      IsBlank := False;
      end;
    10: // COLUMN_SUBTYPE   fldINT16
      begin
      smallint(Value^) := fMetaColumnCurrent.fDbxSubType;
      IsBlank := False;
      end;
    12: // COLUMN_SCALE     fldINT16
      begin
      if fMetaColumnCurrent.fDecimalScale = low(smallint) then
        begin
        IsBlank := true;
        smallint(Value^) := 0;
        end
      else
        begin
        IsBlank := false;
        smallint(Value^) := fMetaColumnCurrent.fDecimalScale;
        end;
      end;
    14: // COLUMN_NULLABLE  fldINT16
      begin
        IsBlank := false;
        smallint(Value^) := fMetaColumnCurrent.fDbxNullable;
      end;
    else
      begin
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataColumns.getShort not valid for column ' + IntToStr(ColumnNumber));
      end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataColumns.getString(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  case ColumnNumber of
    2:  // CATALOG_NAME
      begin
      if (fMetaTableCurrent.fCat = nil) then
        IsBlank := true
      else
        begin
        StrCopy(Value, pChar(fMetaTableCurrent.fCat));
        IsBlank := False;
        end;
      end;
    3:  // SCHEMA_NAME
      begin
      if (fMetaTableCurrent.fSchema = nil) then
        IsBlank := true
      else
        begin
        StrCopy(Value, pChar(fMetaTableCurrent.fSchema));
        IsBlank := False;
        end;
      end;
    4:  // TABLE_NAME
      begin
      StrCopy(Value, pChar(fMetaTableCurrent.fTableName));
      IsBlank := False;
      end;
    5:  // COLUMN_NAME      fldZSTRING
      begin
      StrCopy(Value, pChar(fMetaColumnCurrent.fColumnName));
      IsBlank := False;
      end;
    9:   // COLUMN_TYPENAME  fldZSTRING
      begin
      StrCopy(Value, pChar(fMetaColumnCurrent.fTypeName));
      IsBlank := False;
      end;
    else
      begin
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataColumns.getString not valid for column ' + IntToStr(ColumnNumber));
      end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataColumns.next: SQLResult;
begin
  inc(fRowNo);
  if fRowNo > fColumnList.Count then
    begin
    Result := DBXERR_EOF;
    exit;
    end;
  Result := DBXpress.SQL_SUCCESS;
  fMetaColumnCurrent := fColumnList.Items[fRowNo-1];
  fMetaTableCurrent := fMetaColumnCurrent.fMetaTable;
end;


{ TSqlCursorMetaDataIndexes }

const
  IndexColumnNames: array [1..11] of string =
    ('RECNO', 'CATALOG_NAME', 'SCHEMA_NAME', 'TABLE_NAME', 'INDEX_NAME',
     'PKEY_NAME', 'COLUMN_NAME', 'COLUMN_POSITION', 'INDEX_TYPE', 'SORT_ORDER',
     'FILTER');
  IndexColumnTypes: array [1..11] of word =
    (fldINT32, fldZSTRING, fldZSTRING, fldZSTRING, fldZSTRING,
     fldZSTRING, fldZSTRING, fldINT16, fldINT16, fldZSTRING,
     fldZSTRING);
  IndexColumnCount = length(IndexColumnNames);


{
1.RECNO	          fldINT32    A record number that uniquely identifies each record.
2.CATALOG_NAME    fldZSTRING  The name of the catalog (database) that contains the index.
3.SCHEMA_NAME     fldZSTRING  The name of the schema that identifies the owner of the index.
4.TABLE_NAME      fldZSTRING  The name of the table for which the index is defined.
5.INDEX_NAME      fldZSTRING  The name of the index.
6.PKEY_NAME       fldZSTRING  The name of the primary key.
7.COLUMN_NAME	  fldZSTRING  The name of the column (field) in the index.
8.COLUMN_POSITION fldINT16    The position of this field in the index.
9.INDEX_TYPE	  fldINT16    An eSQLIndexType value (C++) or index type constant (Object Pascal) that indicates any special properties of the index.
10.SORT_ORDER	  fldZSTRING  Indicates whether the index sorts on this field in ascending (a) or descending (d) order.
11.FILTER         fldZSTRING  A string that gives a filter condition limiting indexed records.

ODBC SqlStatistics Result set columns:

1. TABLE_CAT        Varchar         Catalog name of the table to which the statistic or index applies; NULL if not applicable to the data source.
2. TABLE_SCHEM      Varchar         Schema name of the table to which the statistic or index applies; NULL if not applicable to the data source.
3. TABLE_NAME       VarcharnotNULL  Table name of the table to which the statistic or index applies.
4. NON_UNIQUE       Smallint        Indicates whether the index prohibits duplicate values:
     SQL_TRUE if the index values can be nonunique.
     SQL_FALSE if the index values must be unique.
     NULL is returned if TYPE is SQL_TABLE_STAT.
5. INDEX_QUALIFIER  Varchar         The identifier that is used to qualify the index name doing a DROP INDEX;
     NULL is returned if an index qualifier is not supported by the data source or if TYPE is SQL_TABLE_STAT.
     If a non-null value is returned in this column, it must be used to qualify the index name on a DROP INDEX statement;
     otherwise the TABLE_SCHEM should be used to qualify the index name.
6. INDEX_NAME       Varchar         Index name; NULL is returned if TYPE is SQL_TABLE_STAT.
7. TYPE             SmallintnotNULL Type of information being returned:
     SQL_TABLE_STAT indicates a statistic for the table (in the CARDINALITY or PAGES column).
     SQL_INDEX_BTREE indicates a B-Tree index.
     SQL_INDEX_CLUSTERED indicates a clustered index.
     SQL_INDEX_CONTENT indicates a content index.
     SQL_INDEX_HASHED indicates a hashed index.
     SQL_INDEX_OTHER indicates another type of index.
8. ORDINAL_POSITION Smallint        Column sequence number in index (starting with 1); NULL is returned if TYPE is SQL_TABLE_STAT.
9. COLUMN_NAME      Varchar         Column name.
     If the column is based on an expression, such as SALARY + BENEFITS, the expression is returned;
     if the expression cannot be determined, an empty string is returned.
     NULL is returned if TYPE is SQL_TABLE_STAT.
10.ASC_OR_DESC      Char(1)         Sort sequence for the column;
     'A' for ascending; 'D' for descending;
     NULL is returned if column sort sequence is not supported by the data source or if TYPE is SQL_TABLE_STAT.
11.CARDINALITY      Integer         Cardinality of table or index;
     number of rows in table if TYPE is SQL_TABLE_STAT;
     number of unique values in the index if TYPE is not SQL_TABLE_STAT;
     NULL is returned if the value is not available from the data source.
12.PAGES            Integer         Number of pages used to store the index or table;
     number of pages for the table if TYPE is SQL_TABLE_STAT;
     number of pages for the index if TYPE is not SQL_TABLE_STAT;
     NULL is returned if the value is not available from the data source, or if not applicable to the data source.
13.FILTER_CONDITION Varchar         If the index is a filtered index,
     this is the filter condition, such as SALARY > 30000;
     if the filter condition cannot be determined, this is an empty string.
     NULL if the index is not a filtered index, it cannot be determined whether the index is a filtered index, or TYPE is SQL_TABLE_STAT.


ODBC SqlPrimaryKeys Result set columns:

1. TABLE_CAT   Varchar            Primary key table catalog name;
    NULL if not applicable to the data source.
    If a driver supports catalogs for some tables but not for others,
    such as when the driver retrieves data from different DBMSs,
    it returns an empty string ('') for those tables that do not have catalogs.
2. TABLE_SCHEM Varchar
    Primary key table schema name;
    NULL if not applicable to the data source.
    If a driver supports schemas for some tables but not for others,
    such as when the driver retrieves data from different DBMSs,
    it returns an empty string ('') for those tables that do not have schemas.
3. TABLE_NAME  Varchar not NULL   Primary key table name.
4. COLUMN_NAME Varchar not NULL   Primary key column name.
    The driver returns an empty string for a column that does not have a name.
5. KEY_SEQ     Smallint not NULL  Column sequence number in key (starting with 1).
6. PK_NAME     Varchar            Primary key name. NULL if not applicable to the data source.

     }

constructor TSqlCursorMetaDataIndexes.Create(
  OwnerMetaData: TSQLMetaDataOdbc);
begin
  inherited Create(OwnerMetaData);

  fColumnCount := IndexColumnCount;
  fColumnNames := @IndexColumnNames;
  fColumnTypes := @IndexColumnTypes;

  fTableList:= TList.Create;
  fIndexList:= TList.Create;
end;

destructor TSqlCursorMetaDataIndexes.Destroy;
var
  i: integer;
begin
  for i := fTableList.Count - 1 downto 0 do
    TMetaTable(fTableList[i]).Free;
  fTableList.Free;
  for i := fIndexList.Count - 1 downto 0 do
    TMetaIndexColumn(fIndexList[i]).Free;
  fIndexList.Free;
  inherited;
end;

procedure TSqlCursorMetaDataIndexes.FetchIndexes(
  SearchTableName: PChar;
  SearchIndexType: LongWord);
var
  OdbcRetCode: OdbcApi.SQLRETURN;

  Cat: pAnsiChar;
  Schema: pAnsiChar;
  TableName: pAnsiChar;
  OdbcTableType: pAnsiChar;

  OdbcPkName: pAnsiChar;
  OdbcPkColumnName: pAnsiChar;

  IndexName: pAnsiChar;
  IndexColumnName: pAnsiChar;
  IndexFilter: pAnsiChar;
  IndexColumnPosition: Smallint;
  AscDesc: array[0..1] of char;

  cbCat: integer;
  cbSchema: integer;
  cbTableName: integer;
  cbOdbcTableType: integer;

  cbOdbcPkColumnName: integer;
  cbOdbcPkName: integer;

  cbIndexName: integer;
  cbIndexColumnName: integer;
  cbIndexFilter: integer;
  cbOdbcNonUnique: integer;
  cbAscDesc: integer;
  cbIndexColumnPosition: Smallint;

  OdbcIndexType: Smallint;
  OdbcNonUnique: Smallint;

  i: integer;
  aMetaTable: TMetaTable;
  aMetaIndexColumn: TMetaIndexColumn;
begin
  Cat := nil;
  Schema := nil;
  TableName := nil;
  OdbcTableType := nil;

  OdbcPkName := nil;
  OdbcPkColumnName := nil;

  IndexName := nil;
  IndexColumnName := nil;
  IndexFilter := nil;

 try
  fSqlConnectionOdbc.AllocHStmt(fhStmt);

  ParseTableName(SearchTableName);

  if (SearchIndexType = eSQLPrimaryKey)
  or (SearchIndexType = eSQLUnique)
  or (SearchIndexType = eSQLPrimaryKey + eSQLUnique) then
    OdbcIndexType := OdbcApi.SQL_INDEX_UNIQUE
  else
    OdbcIndexType := OdbcApi.SQL_INDEX_ALL;
{
// Accoring to DBXpress help, ISqlMetaDate.GetIndices allows for SEARCH PATTERN
// As Odbc Index function don't allow for search pattern, we have to get all
// matching tables first, then call Odbc Index functions for EACH table found.

// NOW COMMENTED OUT - DBXpress HELP IS WRONG
// Table names containing underscore (Odbc single char wildcard) fuck it up

  OdbcRetCode := SQLTables(fhStmt,
  fMetaCatalogName, SQL_NTS, // Catalog name
  fMetaSchemaName, SQL_NTS, // Schema name
  fMetaTableName, SQL_NTS, // Table name match pattern
  nil, SQL_NTS); // Table types

  OdbcCheck(OdbcRetCode, 'SQLTables');

  if fSqlConnectionOdbc.fSupportsCatalog then
    DescribeAllocBindString(1, Cat, cbCat);
  DescribeAllocBindString(2, Schema, cbSchema);
  DescribeAllocBindString(3, TableName, cbTableName);
  DescribeAllocBindString(4, OdbcTableType, cbOdbcTableType);

  OdbcRetCode := SQLFetch(fhStmt);

// -----------------------------------------------
// This is to find the TABLES that match search parameters...
  while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
    begin

    OdbcCheck(OdbcRetCode, 'SQLFetch');

    aMetaTable := TMetaTable.Create(fSqlConnectionOdbc, Cat, Schema, TableName, eSQLTable);
    fTableList.Add(aMetaTable);
    OdbcRetCode := SQLFetch(fhStmt);
    end;
  OdbcRetCode := SQLCloseCursor(fhstmt);
  OdbcCheck(OdbcRetCode, 'CloseCursor');
  OdbcRetCode := SQLFreeStmt (fhStmt, SQL_UNBIND);
  OdbcCheck(OdbcRetCode, 'SQLFreeStmt - SQL_UNBIND');
//}
  aMetaTable := TMetaTable.Create(fSqlConnectionOdbc,
   fMetaCatalogName, fMetaSchemaName, fMetaTableName, eSQLTable);
  fTableList.Add(aMetaTable);
// -----------------------------------------------
  for i := 0 to fTableList.Count - 1 do
    begin
    aMetaTable := TMetaTable(fTableList.Items[i]);

// -----------------------------------------------
// This is to find the PRIMARY KEY of the table...
    if fSqlConnectionOdbc.fSupportsSQLPRIMARYKEYS then
      begin
      OdbcRetCode := SQLPrimaryKeys(fhStmt,
       aMetaTable.fCat, SQL_NTS, // Catalog name (match pattern not allowed)
       aMetaTable.fSchema, SQL_NTS,  // Schema name (match pattern not allowed)
       aMetaTable.fTableName,  SQL_NTS); // Table name (match pattern not allowed)
      OdbcCheck(OdbcRetCode, 'SQLPrimaryKeys');

      DescribeAllocBindString(4, OdbcPkColumnName, cbOdbcPkColumnName);
      BindSmallInt(5, IndexColumnPosition, @cbIndexColumnPosition);
      if (fSqlConnectionOdbc.fOdbcDriverType = eOdbcDriverTypeMySql) then
        begin
        // Work around bug in MySql Driver - It incorrectluy returns length ZERO for column 6
        OdbcPkName := AllocMem(129);
        OdbcRetCode := SQLBindCol(fhStmt, 6, SQL_C_CHAR, OdbcPkName, 129, @cbOdbcPkName);
        OdbcCheck(OdbcRetCode, 'SQLBindCol');
        end
      else
        DescribeAllocBindString(6, OdbcPkName, cbOdbcPkName);

      OdbcRetCode := SQLFetch(fhStmt);
//      if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
//        OdbcPkName[0] := #0;
//      aMetaTable.fPkName := OdbcPkName;

// Get the PRIMARY KEY index columns(s)
      while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
        begin

        OdbcCheck(OdbcRetCode, 'SQLFetch');

        if (OdbcPkName = nil) or (OdbcPkName[0] = #0) then
          aMetaIndexColumn := TMetaIndexColumn.Create(aMetaTable, '[primary key - unnamed]', OdbcPkColumnName)
        else
          aMetaIndexColumn := TMetaIndexColumn.Create(aMetaTable, OdbcPkName, OdbcPkColumnName);
        if (aMetaTable.fIndexColumnList = nil) then
          aMetaTable.fIndexColumnList := TList.Create;
        if aMetaTable.fPrimaryKeyColumn1 = nil then
          aMetaTable.fPrimaryKeyColumn1 := aMetaIndexColumn;

        aMetaTable.fIndexColumnList.Add(aMetaIndexColumn);
        fIndexList.Add(aMetaIndexColumn);

        aMetaIndexColumn.fColumnPosition := IndexColumnPosition;
        // Assume Primary key is unique, ascending, no filter
        aMetaIndexColumn.fIndexType := eSQLPrimaryKey + eSQLUnique;
        aMetaIndexColumn.fSortOrder := 'a';
        aMetaIndexColumn.fFilter := nil;

        OdbcRetCode := SQLFetch(fhStmt);
        end;

      OdbcRetCode := SQLCloseCursor(fhstmt);
      OdbcCheck(OdbcRetCode, 'CloseCursor');
      OdbcRetCode := SQLFreeStmt (fhStmt, SQL_UNBIND);
      OdbcCheck(OdbcRetCode, 'SQLFreeStmt - SQL_UNBIND');
      end;
// -----------------------------------------------

// -----------------------------------------------
// Get INDEX columns...
      OdbcRetCode := SQLStatistics(fhStmt,
       aMetaTable.fCat, SQL_NTS, // Catalog name (match pattern not allowed)
       aMetaTable.fSchema, SQL_NTS,  // Schema name (match pattern not allowed)
       aMetaTable.fTableName, SQL_NTS,  // Table name (match pattern not allowed)
       OdbcIndexType, // Type of Index to return
       0);            // Reserved
      OdbcCheck(OdbcRetCode, 'SQLStatistics');

      DescribeAllocBindString(6, IndexName, cbIndexName);
      DescribeAllocBindString(9, IndexColumnName, cbIndexColumnName);
      BindSmallInt(4, OdbcNonUnique, @cbOdbcNonUnique);
      BindSmallInt(7, OdbcIndexType, nil);
      BindSmallInt(8, IndexColumnPosition, @cbIndexColumnPosition);
      OdbcRetCode := SQLBindCol(fhStmt, 10, SQL_C_CHAR,
        @AscDesc, SizeOf(AscDesc), @cbAscDesc);
      OdbcCheck(OdbcRetCode, 'SQLBindCol');
      DescribeAllocBindString(13, IndexFilter, cbIndexFilter);

      OdbcRetCode := SQLFetch(fhStmt);
      while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
        begin
        OdbcCheck(OdbcRetCode, 'SQLFetch');

        if (OdbcIndexType <> OdbcApi.SQL_TABLE_STAT) then  // Ignore table statistics
          begin

          if (aMetaTable.fPrimaryKeyColumn1 <> nil) and
             (AnsiStrComp(IndexName, aMetaTable.fPrimaryKeyColumn1.fIndexName) = 0) then
// This is the Primary index - Index column already loaded
          else
            begin
            aMetaIndexColumn := TMetaIndexColumn.Create(aMetaTable, IndexName, IndexColumnName);
            if (aMetaTable.fIndexColumnList = nil) then
              aMetaTable.fIndexColumnList := TList.Create;
            fIndexList.Add(aMetaIndexColumn);
            aMetaTable.fIndexColumnList.Add(aMetaIndexColumn);

            aMetaIndexColumn.fColumnPosition := IndexColumnPosition;

            aMetaIndexColumn.fIndexType := eSQLNonUnique;
            if (cbOdbcNonUnique <> OdbcApi.SQL_NULL_DATA) and (OdbcNonUnique = SQL_FALSE) then
              aMetaIndexColumn.fIndexType := eSQLUnique;

            if AscDesc[0] = 'D' then
              aMetaIndexColumn.fSortOrder := 'd'
            else
              aMetaIndexColumn.fSortOrder := 'a';

            if cbIndexFilter > 0 then
              begin
              aMetaIndexColumn.fFilter := AllocMem(cbIndexFilter);
              StrCopy(aMetaIndexColumn.fFilter, IndexFilter);
              end;

            end;
          end;
        OdbcRetCode := SQLFetch(fhStmt);
        end;

      OdbcRetCode := SQLCloseCursor(fhstmt);
      OdbcCheck(OdbcRetCode, 'CloseCursor');
      OdbcRetCode := SQLFreeStmt (fhStmt, SQL_UNBIND);
      OdbcCheck(OdbcRetCode, 'SQLFreeStmt - SQL_UNBIND');
      end;

  fCatLenMax := 0;
  fSchemaLenMax := 0;
  fTableLenMax := 1;
  fIndexNameLenMax := 1;
  fIndexColumnNameLenMax := 1;
  fPkNameLenMax := 0;
  fFilterLenMax := 0;

  for i := 0 to fTableList.Count - 1 do
    begin
    aMetaTable := TMetaTable(fTableList.Items[i]);
    if aMetaTable.fCat <> nil then
      MaxSet(fCatLenMax, strLen(aMetaTable.fCat));
    if aMetaTable.fSchema <> nil then
      MaxSet(fSchemaLenMax, strLen(aMetaTable.fSchema));
    MaxSet(fTableLenMax, strLen(aMetaTable.fTableName));
    end;

  for i := 0 to fIndexList.Count - 1 do
    begin
    aMetaIndexColumn := TMetaIndexColumn(fIndexList.Items[i]);
    MaxSet(fIndexNameLenMax, strLen(aMetaIndexColumn.fIndexName));
    MaxSet(fIndexColumnNameLenMax, strLen(aMetaIndexColumn.fIndexColumnName));
    if aMetaIndexColumn.fFilter <> nil then
      MaxSet(fFilterLenMax, strLen(aMetaIndexColumn.fFilter));
    end;

finally
  FreeMem(Cat);
  FreeMem(Schema);
  FreeMem(TableName);
  FreeMem(OdbcTableType);
  FreeMem(OdbcPkName);
  FreeMem(OdbcPkColumnName);
  FreeMem(IndexFilter);
  FreeMem(IndexName);
  FreeMem(IndexColumnName);

  if fhStmt <> nil then
    fSqlConnectionOdbc.FreeHStmt(fhStmt);
end;
end;

function TSqlCursorMetaDataIndexes.getColumnLength(ColumnNumber: Word;
  var pLength: LongWord): SQLResult;
begin
try
  case ColumnNumber of
    1:  // RECNO
      pLength := SizeOf(Integer);
    2:  // CATALOG_NAME
      pLength := fCatLenMax;
    3:  // SCHEMA_NAME
      pLength := fSchemaLenMax;
    4:  // TABLE_NAME
      pLength := fTableLenMax;
    5:   // INDEX_NAME      fldZSTRING
      pLength := fIndexNameLenMax;
    6:   // PKEY_NAME       fldZSTRING
      pLength := fPkNameLenMax;
    7:   // COLUMN_NAME     fldZSTRING
      pLength := fIndexColumnNameLenMax;
    8:    // COLUMN_POSITION fldINT16
      pLength := SizeOf(Smallint);
    9:    // INDEX_TYPE	     fldINT16
      pLength := SizeOf(Smallint);
    10:    // SORT_ORDER     fldZSTRING
      pLength := 1;
    11:    // FILTER         fldZSTRING
      pLength := fFilterLenMax;
  else
    begin
    raise EDbxInvalidCall.Create(
      'TSqlCursorMetaDataIndexes.getColumnLength invalid column ' + IntToStr(ColumnNumber));
    end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataIndexes.getColumnPrecision(ColumnNumber: Word;
  var piPrecision: SmallInt): SQLResult;
var
  Length: LongWord;
begin
  Result := getColumnLength(ColumnNumber, Length);
  piPrecision := Length;
end;

function TSqlCursorMetaDataIndexes.getLong(ColumnNumber: Word;
  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
try
  case ColumnNumber of
    1:   // RECNO
      begin
      integer(Value^) := fRowNo;
      IsBlank := False;
      end;
    else
      begin
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataIndexes.getLong not valid for column ' + IntToStr(ColumnNumber));
      end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataIndexes.getShort(ColumnNumber: Word;
  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
try
  case ColumnNumber of
    8:    // COLUMN_POSITION fldINT16
      begin
      SmallInt(Value^) := fCurrentIndexColumn.fColumnPosition;
      IsBlank := False;
      end;
    9:    // INDEX_TYPE	     fldINT16
      begin
      SmallInt(Value^) := fCurrentIndexColumn.fIndexType;
      IsBlank := False;
      end;
    else
      begin
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataIndexes.getLong not valid for column ' + IntToStr(ColumnNumber));
      end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataIndexes.getString(ColumnNumber: Word;
  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
try
  case ColumnNumber of
    1:;   // RECNO	     fldINT32
    2:  // CATALOG_NAME
      begin
      if fSqlConnectionOdbc.fSupportsCatalog then
        begin
        StrCopy(Value, pChar(fCurrentIndexColumn.fMetaTable.fCat));
        IsBlank := False;
        end
      else
        IsBlank := True;
      end;
    3:  // SCHEMA_NAME
      begin
      if fSchemaLenMax = 0 then
        IsBlank := true
      else
        begin
        StrCopy(Value, pChar(fCurrentIndexColumn.fMetaTable.fSchema));
        IsBlank := False;
        end;
      end;
    4:  // TABLE_NAME
      begin
      StrCopy(Value, pChar(fCurrentIndexColumn.fMetaTable.fTableName));
      IsBlank := False;
      end;
    5:  // INDEX_NAME      fldZSTRING
      begin
      StrCopy(Value, pChar(fCurrentIndexColumn.fIndexName));
      IsBlank := False;
      end;
    6:   // PKEY_NAME       fldZSTRING
      begin
      if (fCurrentIndexColumn.fMetaTable.fPrimaryKeyColumn1 <> nil) then
        begin
        IsBlank := False;
        StrCopy(Value, fCurrentIndexColumn.fMetaTable.fPrimaryKeyColumn1.fIndexName);
        end
      else
        IsBlank := True;
      end;
    7:    // COLUMN_NAME     fldZSTRING
      begin
      StrCopy(Value, pChar(fCurrentIndexColumn.fIndexColumnName));
      IsBlank := False;
      end;
    8:;   // COLUMN_POSITION fldINT16
    9:;   // INDEX_TYPE	     fldINT16
    10:   // SORT_ORDER     fldZSTRING
      begin
      pChar(Value)[0] := fCurrentIndexColumn.fSortOrder;
      pChar(Value)[1] := #0;
      IsBlank := False;
      end;
    11:    // FILTER         fldZSTRING
      begin
      if fFilterLenMax = 0 then
        IsBlank := true
      else
        begin
        StrCopy(Value, pChar(fCurrentIndexColumn.fFilter));
        IsBlank := False;
        end;
      end;
    else
      begin
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataIndexes.getLong not valid for column ' + IntToStr(ColumnNumber));
      end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;


function TSqlCursorMetaDataIndexes.next: SQLResult;
begin
  inc(fRowNo);
  if fRowNo <= fIndexList.Count then
    begin
    fCurrentIndexColumn := fIndexList[fRowNo-1];
    Result := DBXpress.SQL_SUCCESS;
    end
  else
    Result := DBXERR_EOF;
end;

{ TSqlCursorProcedures }
{
Dbx returned cursor columns
 1.RECNO	 fldINT32	A record number that uniquely identifies each record.
 2.CATALOG_NAME  fldZSTRING	The name of the catalog (database) that contains the stored procedure.
 3.SCHEMA_NAME   fldZSTRING	The name of the schema that identifies the owner of the stored procedure.
 4.PROC_NAME	 fldZSTRING	The name of the stored procedure.
 5.PROC_TYPE	 fldINT32	An eSQLProcType value (C++) or stored procedure type constant (Object Pascal) that indicates the type of stored procedure.
 6.IN_PARAMS	 fldINT16	The number of input parameters.
 7.OUT_PARAMS    fldINT16	The number of output parameters.

ODBC result set columns from SQLProcedures
 1.PROCEDURE_CAT     Varchar     Catalog name; NULL if not applicable to the data source
 2.PROCEDURE_SCHEM   Varchar     Schema name; NULL if not applicable to the data source.
 3.PROCEDURE_NAME    Varchar not null    Procedure identifier
 4.NUM_INPUT_PARAMS  N/A         Reserved for future use
 5.NUM_OUTPUT_PARAMS N/A         Reserved for future use
 6.NUM_RESULT_SETS   N/A         Reserved for future use
 7.REMARKS           Varchar     A description of the procedure
 8.PROCEDURE_TYPE    Smallint    Defines the procedure type:
    SQL_PT_UNKNOWN:   It cannot be determined whether the procedure returns a value.
    SQL_PT_PROCEDURE: The returned object is a procedure; that is, it does not have a return value.
    SQL_PT_FUNCTION:  The returned object is a function; that is, it has a return value.
}

const
  ProcedureColumnNames: array [1..7] of string =
    ('RECNO', 'CATALOG_NAME', 'SCHEMA_NAME', 'PROC_NAME', 'PROC_TYPE', 'IN_PARAMS', 'OUT_PARAMS');
  ProcedureColumnTypes: array [1..7] of word =
    (fldINT32, fldZSTRING, fldZSTRING, fldZSTRING, fldINT32, fldINT16, fldINT16);
  ProcedureColumnCount = length(ProcedureColumnNames);

constructor TSqlCursorMetaDataProcedures.Create(
  OwnerMetaData: TSQLMetaDataOdbc);
begin
  inherited Create(OwnerMetaData);
  fColumnCount := ProcedureColumnCount;
  fColumnNames := @ProcedureColumnNames;
  fColumnTypes := @ProcedureColumnTypes;
end;

destructor TSqlCursorMetaDataProcedures.Destroy;
var
  i: integer;
begin
  if Assigned(fProcList) then
    begin
    for i := fProcList.Count - 1 downto 0 do
      TMetaProcedure(fProcList[i]).Free;
    fProcList.Free;
    end;
  inherited;
end;

procedure TSqlCursorMetaDataProcedures.FetchProcedures(
  ProcedureName: PChar;
  ProcType: LongWord);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  aMetaProcedure: TMetaProcedure;

  Cat: pAnsiChar;
  Schema: pAnsiChar;
  ProcName: pAnsiChar;
  OdbcProcType: SmallInt;

  cbCat: SQLINTEGER;
  cbSchema: SQLINTEGER;
  cbProcName: SQLINTEGER;
  cbOdbcProcType: SQLINTEGER;

begin
  Cat := nil;
  Schema := nil;
  ProcName := nil;

try
  fSqlConnectionOdbc.AllocHStmt(fhStmt);

{  ProcType is a combination of flags:
    eSQLProcedure, eSQLFunction, eSQLPackage, eSQLSysProcedure
   But ODBC always returns all procedures }

  OdbcRetCode := SQLProcedures(fhStmt,
  nil, 0, // all catalogs
  nil, 0, // all schemas
  ProcedureName, SQL_NTS); // Procedure name match pattern
  OdbcCheck(OdbcRetCode, 'SQLProcedures');

  if fSqlConnectionOdbc.fSupportsCatalog then
    DescribeAllocBindString(1, Cat, cbCat);
  DescribeAllocBindString(2, Schema, cbSchema);
  DescribeAllocBindString(3, ProcName, cbProcName);
  BindSmallint(8, OdbcProcType, @cbOdbcProcType);

  fCatLenMax := 0;
  fSchemaLenMax := 0;
  fProcLenMax := 1;
  fProcList:= TList.Create;
  OdbcRetCode := SQLFetch(fhStmt);

  while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
    begin

    OdbcCheck(OdbcRetCode, 'SQLFetch');

    aMetaProcedure := TMetaProcedure.Create(Cat, Schema, ProcName, OdbcProcType);
    fProcList.Add(aMetaProcedure);

    if Cat <> nil then
      MaxSet(fCatLenMax, strLen(Cat));
    if Schema <> nil then
      MaxSet(fCatLenMax, strLen(Schema));
    MaxSet(fProcLenMax, strLen(ProcName));

    OdbcRetCode := SQLFetch(fhStmt);
    end;

finally
  FreeMem(Cat);
  FreeMem(Schema);
  FreeMem(ProcName);

  if fhStmt <> nil then
    fSqlConnectionOdbc.FreeHStmt(fhStmt);
end;

end;

function TSqlCursorMetaDataProcedures.getColumnLength(ColumnNumber: Word;
  var pLength: LongWord): SQLResult;
begin
try
  case ColumnNumber of
    1:  // RECNO
      pLength := SizeOf(Integer);
    2:  // CATALOG_NAME
      pLength := fCatLenMax;
    3:  // SCHEMA_NAME
      pLength := fSchemaLenMax;
    4:  // PROCEDURE_NAME
      pLength := fProcLenMax;
    5: // PROC_TYPE
      pLength := SizeOf(Integer);
    6: // IN_PARAMS
      pLength := SizeOf(SmallInt);
    7: // OUT_PARAMS
      pLength := SizeOf(SmallInt);
  else
    begin
    raise EDbxInvalidCall.Create(
      'TSqlCursorMetaDataProcedures.getColumnLength invalid column no: ' + IntToStr(ColumnNumber));
    end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataProcedures.getColumnPrecision(ColumnNumber: Word;
  var piPrecision: SmallInt): SQLResult;
var
  Length: LongWord;
begin
  Result := getColumnLength(ColumnNumber, Length);
  piPrecision := Length;
end;

function TSqlCursorMetaDataProcedures.getLong(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  case ColumnNumber of
    1:   // RECNO
      begin
      integer(Value^) := fRowNo;
      IsBlank := False;
      end;
    5:   // PROC_TYPE
      begin
       { TODO : CHECK FOR PROCEDURE TYPE - Assume Procedure for now }
      integer(Value^) := eSQLProcedure;
      IsBlank := False;
      end;
  else
    begin
    raise EDbxInvalidCall.Create(
      'TSqlCursorMetaDataProcedures.getLong invalid column no: ' + IntToStr(ColumnNumber));
    end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataProcedures.getString(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  case ColumnNumber of
    2:  // CATALOG_NAME
      begin
      if (fMetaProcedureCurrent.fCat = nil) then
        IsBlank := true
      else
        begin
        StrCopy(Value, pChar(fMetaProcedureCurrent.fCat));
        IsBlank := False;
        end;
      end;
    3:  // SCHEMA_NAME
      begin
      if (fMetaProcedureCurrent.fSchema = nil) then
        IsBlank := true
      else
        begin
        StrCopy(Value, pChar(fMetaProcedureCurrent.fSchema));
        IsBlank := False;
        end;
      end;
    4:  // PROCEDURE_NAME
      begin
      StrCopy(Value, pChar(fMetaProcedureCurrent.fProcName));
      IsBlank := False;
      end;
  else
    begin
    raise EDbxInvalidCall.Create(
      'TSqlCursorMetaDataProcedures.getString invalid column no: ' + IntToStr(ColumnNumber));
    end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataProcedures.next: SQLResult;
begin
  inc(fRowNo);
  if fRowNo <= fProcList.Count then
    begin
    fMetaProcedureCurrent := fProcList[fRowNo-1];
    Result := DBXpress.SQL_SUCCESS;
    end
  else
    Result := DBXERR_EOF;
end;

{ TSqlCursorMetaDataProcedureParams }

{
Dbx returned cursor columns
 1.  RECNO	        fldINT32	A record number that uniquely identifies each record.
 2.  CATALOG_NAME	fldZSTRING	The name of the catalog (database) that contains the stored procedure.
 3.  SCHEMA_NAME	fldZSTRING	The name of the schema that identifies the owner of the stored procedure.
 4.  PROC_NAME	        fldZSTRING	The name of the procedure in which the parameter appears.
 5.  PARAM_NAME	        fldZSTRING	The name of the parameter.
 6.  PARAM_TYPE	        fldINT16	A STMTParamType value that indicates whether the parameter is used for input, output, or result.
 7.  PARAM_DATATYPE	fldINT16	The logical data type for the parameter.
 8.  PARAM_SUBTYPE	fldINT16	The logical data subtype for the parameter.
 9.  PARAM_TYPENAME	fldZSTRING	A string describing the datatype. This is the same information as contained in PARAM_DATATYPE and PARAM_SUBTYPE, but in a form used in some DDL statements.
 10. PARAM_PRECISION	fldINT32	The size of the parameter type (number of characters in a string, bytes in a bytes field, significant digits in a BCD value, members of an ADT, and so on)
 11. PARAM_SCALE	fldINT16	The number of digits to the right of the decimal on BCD values, or descendants on ADT and array values.
 12. PARAM_LENGTH	fldINT32	The number of bytes required to store parameter values.
 13. PARAM_NULLABLE	fldINT16	0 if the parameter requires a value, nonzero if it can be blank.

ODBC result set columns from SQLProcedureColumns
 1. PROCEDURE_CAT        Varchar           Procedure catalog name; NULL if not applicable to the data source.
 2. PROCEDURE_SCHEM      Varchar           Procedure schema name; NULL if not applicable to the data source.
 3. PROCEDURE_NAME       Varchar not NULL  Procedure name. An empty string is returned for a procedure that does not have a name.
 4. COLUMN_NAME          Varchar not NULL  Procedure column name. The driver returns an empty string for a procedure column that does not have a name.
 5. COLUMN_TYPE          Smallint not NULL Defines the procedure column as a parameter or a result set column:
     SQL_PARAM_TYPE_UNKNOWN: The procedure column is a parameter whose type is unknown
     SQL_PARAM_INPUT:        The procedure column is an input parameter
     SQL_PARAM_INPUT_OUTPUT: The procedure column is an input/output parameter
     SQL_PARAM_OUTPUT:       The procedure column is an output parameter
     SQL_RETURN_VALUE:       The procedure column is the return value of the procedure
     SQL_RESULT_COL:         The procedure column is a result set column
 6. DATA_TYPE            Smallint not NULL SQL data type. This can be an ODBC SQL data type or a driver-specific SQL data type.
      For datetime and interval data types, this column returns the concise data types (for example, SQL_TYPE_TIME or SQL_INTERVAL_YEAR_TO_MONTH)
 7. TYPE_NAME            Varchar not NULL  Data source – dependent data type name
 8. COLUMN_SIZE          Integer           The column size of the procedure column on the data source.
      NULL is returned for data types where column size is not applicable.
      For more information concerning precision, see 'Column Size, Decimal Digits, Transfer Octet Length, and Display Size,' in Appendix D, 'Data Types.'
 9. BUFFER_LENGTH        Integer           The length in bytes of data transferred on an SQLGetData or SQLFetch operation if SQL_C_DEFAULT is specified.
      For numeric data, this size may be different than the size of the data stored on the data source.
      For more information concerning precision, see 'Column Size, Decimal Digits, Transfer Octet Length, and Display Size,' in Appendix D, 'Data Types.'
 10.DECIMAL_DIGITS       Smallint          The decimal digits of the procedure column on the data source.
      NULL is returned for data types where decimal digits is not applicable.
      For more information concerning decimal digits, see 'Column Size, Decimal Digits, Transfer Octet Length, and Display Size,' in Appendix D, 'Data Types.'
 11.NUM_PREC_RADIX       Smallint          For numeric data types, either 10 or 2.
      If it is 10, the values in COLUMN_SIZE and DECIMAL_DIGITS give the number of decimal digits allowed for the column.
      For example, a DECIMAL(12,5) column would return a NUM_PREC_RADIX of 10, a COLUMN_SIZE of 12, and a DECIMAL_DIGITS of 5;
      a FLOAT column could return a NUM_PREC_RADIX of 10, a COLUMN_SIZE of 15 and a DECIMAL_DIGITS of NULL.
      If it is 2, the values in COLUMN_SIZE and DECIMAL_DIGITS give the number of bits allowed in the column.
      For example, a FLOAT column could return a NUM_PREC_RADIX of 2, a COLUMN_SIZE of 53, and a DECIMAL_DIGITS of NULL.
      NULL is returned for data types where NUM_PREC_RADIX is not applicable.
 12.NULLABLE             Smallint not NULL Whether the procedure column accepts a NULL value:
     SQL_NO_NULLS: The procedure column does not accept NULL values.
     SQL_NULLABLE: The procedure column accepts NULL values.
     SQL_NULLABLE_UNKNOWN: It is not known if the procedure column accepts NULL values.
 13.REMARKS              Varchar           A description of the procedure column.
 14.COLUMN_DEF           Varchar           The default value of the column.
     If NULL was specified as the default value, then this column is the word NULL, not enclosed in quotation marks.
     If the default value cannot be represented without truncation, then this column contains TRUNCATED, with no enclosing single quotation marks.
     If no default value was specified, then this column is NULL.
     The value of COLUMN_DEF can be used in generating a new column definition, except when it contains the value TRUNCATED.
 15.SQL_DATA_TYPE        Smallint not NULL The value of the SQL data type as it appears in the SQL_DESC_TYPE field of the descriptor.
      This column is the same as the DATA_TYPE column, except for datetime and interval data types.
      For datetime and interval data types, the SQL_DATA_TYPE field in the result set will return SQL_INTERVAL or SQL_DATETIME,
      and the SQL_DATETIME_SUB field will return the subcode for the specific interval or datetime data type (see Appendix D, “Data Types”).
 16.SQL_DATETIME_SUB     Smallint          The subtype code for datetime and interval data types. For other data types, this column returns a NULL.
 17.CHAR_OCTET_LENGTH    Integer           The maximum length in bytes of a character or binary data type column.
      For all other data types, this column returns a NULL.
 18.ORDINAL_POSITION     Integer not NULL  For input and output parameters,
     the ordinal position of the parameter in the procedure definition
     (in increasing parameter order, starting at 1).
     For a return value (if any), 0 is returned.
     For result-set columns, the ordinal position of the column in the result set,
     with the first column in the result set being number 1.
     If there are multiple result sets, column ordinal positions are returned in a driver-specific manner.
 19.IS_NULLABLE          Varchar           'NO' if the column does not include NULLs.
                                           'YES' if the column can include NULLs.
      This column returns a zero-length string if nullability is unknown.
      ISO rules are followed to determine nullability. An ISO SQL – compliant DBMS cannot return an empty string.
      The value returned for this column is different from the value returned for the NULLABLE column.
      (See the description of the NULLABLE column.)
}

const
  ProcedureParamColumnNames: array [1..13] of string =
   ('RECNO',      'CATALOG_NAME',   'SCHEMA_NAME',   'PROC_NAME',      'PARAM_NAME',
    'PARAM_TYPE', 'PARAM_DATATYPE', 'PARAM_SUBTYPE', 'PARAM_TYPENAME', 'PARAM_PRECISION',
    'PARAM_SCALE', 'PARAM_LENGTH', 'PARAM_NULLABLE');
  ProcedureParamColumnTypes: array [1..13] of word =
    (fldINT32, fldZSTRING, fldZSTRING, fldZSTRING, fldZSTRING,
     fldINT16, fldINT16,   fldINT16,   fldZSTRING, fldINT32,
     fldINT16, fldINT32,   fldINT16);
  ProcedureParamColumnCount = length(ProcedureParamColumnNames);

constructor TSqlCursorMetaDataProcedureParams.Create(
  OwnerMetaData: TSQLMetaDataOdbc);
begin
  inherited Create(OwnerMetaData);

  fColumnCount := ProcedureParamColumnCount;
  fColumnNames := @ProcedureParamColumnNames;
  fColumnTypes := @ProcedureParamColumnTypes;
end;

destructor TSqlCursorMetaDataProcedureParams.Destroy;
var
  i: integer;
begin
  if Assigned(fProcColumnList) then
    begin
    for i := fProcColumnList.Count - 1 downto 0 do
      TMetaProcedureParam(fProcColumnList[i]).Free;
    fProcColumnList.Free;
    end;
  inherited;
end;

procedure TSqlCursorMetaDataProcedureParams.FetchProcedureParams(
  SearchProcedureName: PChar;
  SearchParamName: PChar);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  Cat: pChar;
  Schema: pChar;
  ProcName: pChar;
  ProcColumnName: pChar;
  TypeName: pChar;
  OrdinalPosition: integer;
  ColumnType: SmallInt;
  OdbcDataType: SmallInt;
  Scale: SmallInt;
  OdbcNullable: SmallInt;

  cbCat: integer;
  cbSchema: integer;
  cbProcName: integer;
  cbProcColumnName: integer;
  cbTypeName: integer;
  cbScale: integer;
  cbColumnType: integer;
  cbOdbcDataType: integer;
  cbOrdinalPosition: integer;

  DbxDataType: smallint;
  DbxDataSubType: smallint;
  i: integer;
  aMetaProcedure: TMetaProcedure;
  aMetaProcedureParam: TMetaProcedureParam;

begin
  Cat := nil;
  Schema := nil;
  ProcName := nil;
  ProcColumnName := nil;
  TypeName := nil;
try
  fSqlConnectionOdbc.AllocHStmt(fhStmt);

  if (SearchParamName <> nil) then
    if (SearchParamName[0] = #0) then
      SearchParamName := nil;

  ParseTableName(SearchProcedureName);

  OdbcRetCode := SQLProcedureColumns(fhStmt,
   fMetaCatalogName, SQL_NTS, // Catalog name
   fMetaSchemaName, SQL_NTS, // Schema name
   fMetaTableName, SQL_NTS,  // Procedure name match pattern
   SearchParamName, SQL_NTS); // Column name match pattern
  OdbcCheck(OdbcRetCode, 'SQLProcedureColumns');

  if fSqlConnectionOdbc.fSupportsCatalog then
    DescribeAllocBindString(1, Cat, cbCat);
  if (fSqlConnectionOdbc.fOdbcMaxSchemaNameLen > 0) then
    DescribeAllocBindString(2, Schema, cbSchema);
  DescribeAllocBindString(3, ProcName, cbProcName);
  DescribeAllocBindString(4, ProcColumnName, cbProcColumnName);
  BindSmallInt(5, ColumnType, @cbColumnType);
  BindSmallInt(6, OdbcDataType, @cbOdbcDataType);
  DescribeAllocBindString(7, TypeName, cbTypeName);
  BindSmallInt(10, Scale, @cbScale);
  BindSmallInt(12, OdbcNullable, nil); // NULLABLE
  BindInteger(18, OrdinalPosition, @cbOrdinalPosition);

  fProcList:= TList.Create;
  fProcColumnList:= TList.Create;

  OdbcRetCode := SQLFetch(fhStmt);

  while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
    begin

    OdbcCheck(OdbcRetCode, 'SQLFetch');

    if (ColumnType <> SQL_RESULT_COL) then
      begin
      aMetaProcedure := TMetaProcedure.Create(Cat, Schema, ProcName, 0);
      fProcList.Add(aMetaProcedure);
      aMetaProcedureParam := TMetaProcedureParam.Create(ProcColumnName);
      fProcColumnList.Add(aMetaProcedureParam);
      case ColumnType of
        SQL_PARAM_TYPE_UNKNOWN:
          aMetaProcedureParam.fParamType := DBXpress.paramUNKNOWN;
        SQL_PARAM_INPUT:
          aMetaProcedureParam.fParamType := DBXpress.paramIN;
        SQL_PARAM_INPUT_OUTPUT:
          aMetaProcedureParam.fParamType := DBXpress.paramINOUT;
        SQL_PARAM_OUTPUT:
          aMetaProcedureParam.fParamType := DBXpress.paramOUT;
        SQL_RETURN_VALUE:
          aMetaProcedureParam.fParamType := DBXpress.paramRET;
        SQL_RESULT_COL: ; // Already discarded
        end;
      OdbcDataTypeToDbxType(OdbcDataType, DbxDataType, DbxDataSubType);
      aMetaProcedureParam.fDataType := DbxDataType;
      aMetaProcedureParam.fDataSubtype := DbxDataSubType;
      aMetaProcedureParam.fDataTypeName := AllocMem(strLen(TypeName) + 1);
      StrCopy(TypeName, aMetaProcedureParam.fDataTypeName);
      if (OdbcNullable <> SQL_NULLABLE) then
         aMetaProcedureParam.fNullable := 0 // Requires a value
      else
         aMetaProcedureParam.fNullable := 1; // Does not require a value
      end;
    OdbcRetCode := SQLFetch(fhStmt);
    end;

  fCatLenMax := 0;
  fSchemaLenMax := 0;
  fProcNameLenMax := 1;
  fParamNameLenMax := 1;
  fDataTypeNameLenMax := 1;

  for i := 0 to fProcList.Count - 1 do
    begin
    aMetaProcedure := TMetaProcedure(fProcList.Items[i]);
    if aMetaProcedure.fCat <> nil then
      MaxSet(fCatLenMax, strLen(aMetaProcedure.fCat));
    if aMetaProcedure.fSchema <> nil then
      MaxSet(fSchemaLenMax, strLen(aMetaProcedure.fSchema));
    MaxSet(fProcNameLenMax, strLen(aMetaProcedure.fProcName));
    end;

  for i := 0 to fProcColumnList.Count - 1 do
    begin
    aMetaProcedureParam := TMetaProcedureParam(fProcColumnList.Items[i]);
    MaxSet(fParamNameLenMax, strLen(aMetaProcedureParam.fParamName));
    MaxSet(fDataTypeNameLenMax, strLen(aMetaProcedureParam.fDataTypeName));
    end;

finally
  FreeMem(Cat);
  FreeMem(Schema);
  FreeMem(ProcName);
  FreeMem(ProcColumnName);
  FreeMem(TypeName);

  if fhStmt <> nil then
    fSqlConnectionOdbc.FreeHStmt(fhStmt);
end;
end;

function TSqlCursorMetaDataProcedureParams.getColumnLength(
  ColumnNumber: Word; var pLength: LongWord): SQLResult;
begin
try
  case ColumnNumber of
    1:    // RECNO
      pLength := SizeOf(integer);
    2:  // CATALOG_NAME
      pLength := fCatLenMax;
    3:  // SCHEMA_NAME
      pLength := fSchemaLenMax;
    4:  // PROCEDURE_NAME
      pLength := fProcNameLenMax;
    5:   // PARAM_NAME
      pLength := fParamNameLenMax;
    6:   // PARAM_TYPE
      pLength := SizeOf(Smallint);
    7:   // PARAM_DATATYPE
      pLength := SizeOf(Smallint);
    8:   // PARAM_SUBTYPE
      pLength := SizeOf(Smallint);
    9:   // PARAM_TYPENAME
      pLength := fDataTypeNameLenMax;
    10:  // PARAM_PRECISION
      pLength := SizeOf(integer);
    11:  // PARAM_SCALE
      pLength := SizeOf(Smallint);
    12:  // PARAM_LENGTH
      pLength := SizeOf(integer);
    13:  // PARAM_NULLABLE
      pLength := SizeOf(Smallint);
  else
    begin
    raise EDbxInvalidCall.Create(
      'TSqlCursorMetaDataProcedureParams.getColumnLength invalid column no: ' + IntToStr(ColumnNumber));
    end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataProcedureParams.getColumnPrecision(
  ColumnNumber: Word; var piPrecision: SmallInt): SQLResult;
var
  Length: LongWord;
begin
  Result := getColumnLength(ColumnNumber, Length);
  piPrecision := Length;
end;

function TSqlCursorMetaDataProcedureParams.getLong(ColumnNumber: Word;
  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
try
  case ColumnNumber of
    1:   // RECNO
      begin
      integer(Value^) := fRowNo;
      IsBlank := False;
      end;
    10:  // PARAM_PRECISION
      begin
      integer(Value^) := fMetaProcedureParamCurrent.fPrecision;
      IsBlank := False;
      end;
    12:  // PARAM_LENGTH
      begin
      integer(Value^) := fMetaProcedureParamCurrent.fLength;
      IsBlank := False;
      end;
  else
    begin
    raise EDbxInvalidCall.Create(
      'TSqlCursorMetaDataProcedures.getLong invalid column no: ' + IntToStr(ColumnNumber));
    end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataProcedureParams.getShort(ColumnNumber: Word;
  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
try
  case ColumnNumber of
    6:   // PARAM_TYPE
      begin
      smallint(Value^) := smallInt(fMetaProcedureParamCurrent.fParamType);
      IsBlank := False;
      end;
    7:   // PARAM_DATATYPE
      begin
      smallint(Value^) := fMetaProcedureParamCurrent.fDataType;
      IsBlank := False;
      end;
    8:   // PARAM_SUBTYPE
      begin
      smallint(Value^) := fMetaProcedureParamCurrent.fDataSubType;
      IsBlank := False;
      end;
    11:  // PARAM_SCALE
      begin
      smallint(Value^) := fMetaProcedureParamCurrent.fScale;
      IsBlank := False;
      end;
    13:  // PARAM_NULLABLE
      begin
      smallint(Value^) := fMetaProcedureParamCurrent.fNullable;
      IsBlank := False;
      end;
  else
    begin
    raise EDbxInvalidCall.Create(
      'TSqlCursorMetaDataProcedures.getShort invalid column no: ' + IntToStr(ColumnNumber));
    end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataProcedureParams.getString(ColumnNumber: Word;
  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
try
  case ColumnNumber of
    2:   // CATALOG_NAME
      begin
      StrCopy(Value, fMetaProcedureParamCurrent.fMetaProcedure.fCat);
      IsBlank := False;
      end;
    3:   // SCHEMA_NAME
      begin
      StrCopy(Value, fMetaProcedureParamCurrent.fMetaProcedure.fSchema);
      IsBlank := False;
      end;
    4:   // PROC_NAME
      begin
      StrCopy(Value, fMetaProcedureParamCurrent.fMetaProcedure.fProcName);
      IsBlank := False;
      end;
    5:   // PARAM_NAME
      begin
      StrCopy(Value, fMetaProcedureParamCurrent.fParamName);
      IsBlank := False;
      end;
    9:   // PARAM_TYPENAME
      begin
      StrCopy(Value, fMetaProcedureParamCurrent.fDataTypeName);
      IsBlank := False;
      end;
  else
    begin
    raise EDbxInvalidCall.Create(
      'TSqlCursorMetaDataProcedures.getString invalid column no: ' + IntToStr(ColumnNumber));
    end;
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;

function TSqlCursorMetaDataProcedureParams.next: SQLResult;
begin
  inc(fRowNo);
  if fRowNo <= fProcColumnList.Count then
    begin
    fMetaProcedureParamCurrent := fProcColumnList[fRowNo-1];
    Result := DBXpress.SQL_SUCCESS;
    end
  else
    Result := DBXERR_EOF;
end;

initialization
{$IFDEF MSWINDOWS}
// This allows option of static linking the DbExpress driver into your app
SqlExpr.RegisterDbXpressLib(@getSQLDriverODBC);
{$ENDIF}
end.
