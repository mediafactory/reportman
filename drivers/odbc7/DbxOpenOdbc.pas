{
  Kylix / Delphi open source DbExpress driver for ODBC
  Version 2.041, 2003-01-15

  Copyright (c) 2001, 2002 Edward Benson

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}
unit DbxOpenOdbc;

{+2.01 Compiler options}
// Vadim V.Lopushansky:
// Compiler options

{.$DEFINE _DEBUG_}

{$IFDEF _DEBUG_}
  // - Debugger options
 {$O-,D+}
{$ELSE}
  // - Release options:
 {$O+,D-}
{$ENDIF}

// Desirable Compiler options

{$B-,J+}

{$IFDEF VER140} // Delphi 6 only }
  {$DEFINE _D6_}
{$ELSE}
    {$DEFINE _D7UP_}// Delphi 7 or more ( and Kylix )
    {$IFDEF VER150} // Delphi 7 only
      {$DEFINE _D7_}//
    {$ENDIF}
{$ENDIF}
{/+2.01 /Compiler options}

{$DEFINE _RegExprParser_}

interface

uses
  DbxOpenOdbcInterface,
  OdbcApi,
  DBXpress,
{$ifdef _RegExprParser_}
  DbxObjectParser,
{$endif}
  Classes;

{
  Kylix / Delphi DbExpress driver for ODBC Version 3 drivers
  (Also works with ODBC Version 2).

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
  the function return code, or it may check for error code but retrieve the error
  message from the wrong interface. So by raising exception at the point of
  error, the IDE halts on the exception during source debugging, and this makes
  it much easier to trace errors.
}

{
Change History

Beta, 2001-10-26 [ Edward Benson ]
----------------------------------
+ First public release

Version 1.01, 2001-11-28  [ Edward Benson ]
-------------------------------------------
+ Fix bug in TSqlCursorMetaDataIndexes
+ Support Interbase 6 Easysoft ODBC Driver
+ Support MySql ODBC Driver (ODBC level 2)

Version 1.02, 2001-12-05 [ Edward Benson ]
------------------------------------------
+ Fix bug in TSqlCursorOdbc.getBcd to cater for comma decimal separator

Version 1.03, 2001-12-06 [ Edward Benson ]
------------------------------------------
+ Change to support Kylix
  (fixes posted by Ivan Francolin Martinez)

Version 1.04, 2002-01-22 [ Edward Benson ] (Not released to public)
------------------------------------------
+ Internally clone connection for databases that only support 1 statement
  handle per connection, such as MsSqlServer
  (maintain internal connection cache for such databases, until disconnected)
+ Work around MySql bug - odbc driver incorrectly reports that
  it supports transactions when it doesn't
+ More changes to support Kylix (in OdbcApi.pas)
  (fixes posted and tested by Ivan Francolin Martinez)
+ Allow for blank column names (returned by Informix stored procedures)
  (fix posted and tested by Bulent Erdemir)

Version 1.05, 2002-06-09 [ Edward Benson ] (Not released to public)
------------------------------------------
+ Change to support TIMESTAMP parameters
  (fix posted and tested by Michael Schwarzl)
+ Work around to support multiple GetBlob calls for MS SqlServer
  (fix posted and tested by Michael Schwarzl)
+ Work around for Delphi 6.02 -
  SqlExpress now calls ISqlCommand.SetOption(RowSetSize) for all drivers
+ Fix TSqlCursorOdbc functions: isReadOnly, isAutoIncrement, isSearchable
  Were incorrectly using ColNo-1 (ie 0-based) - ODBC column indexes are 1-based
  (Confusing, because the bind array (fOdbcBindList) is 0-based)
+ eOdbcDriverTypeAccess renamed eOdbcDriverTypeMsJet
  (MsJet driver works for other databases, not just Access)

Version 1.06, 2002-11-01 [ Edward Benson ] (Prepare for Vadim's changes)
------------------------------------------
+ Reformatted comments and code, so diff shows up changes for 2.00
}

{+2.01 WhatNews}
(*
Version 2.01, 2002-11-01 (Vadim Lopushansky)
------------------------

Edward> + below means I have included Vadim's change,
Edward> - means I have not

  + Change to support Delphi7. See block: {$IFDEF _D7UP_}.
  + Change to support INFORMIX (tested on version IDS 7.31 TD3).
  + Change to support ThinkSQL (tested on version 0.4.07 beta. http://thinksql.com/).
  + Change to detect database types for Multiplatform DataDirect ODBC Drivers
    (http://www.datadirect-technologies.com)
  - Change to detect database type method TSqlConnectionOdbc.RetrieveDriverName.
    For detecting usage specific RDMS query.
    Edward> I have not included this:
    Edward> I think it is better to use SQLGetInfoString(SQL_DBMS_NAME) instead
  + Change to remapping Int64 to BCD
    (optional. Connection parameter: "Database"="...;MapInt64ToBcd=1"
    or "Custom String"="...;MapInt64ToBcd=1")
  + Change to remapping small BCD to native
    (optional. Connection parameter: "Database"="...;MapSmallBcdToNative=1"
    or "Custom String"="...;MapSmallBcdToNative=1"))
    Is problem in editing controls when native type length is more then BCD data type length.
    For editing you mast usage controls with format string...
  + Change for addition of possibility of disconnecting of support of the metadata.
    Is used in case of availability of errors in the ODBC driver.
    For disconnecting the metadata it is necessary to add to connection line Metadata=0
    (Connection parameter: "Database"="...;Metadata=0"  or "Custom String"="...;Metadata=1").
  + Change to updating BCD values when DecimalSeparator <> '.'
  + Change to reading of PK_INDEX from metadata (Calculating fPkNameLenMax).
    For an example look: ($DELPHI$)\Demos\Db\DbxExplorer\dbxexplorer.dpr
    (Read PKEY_NAME error).
    All metadata fields returned length more 0.
  + Change in %Metadata%.getColumnLength:
    Adapting to calculate visible of columns in SqlExpr.pas type.
    For an example look: ($DELPHI$)\Demos\Db\DbxExplorer\dbxexplorer.dpr
    (Read procedure parameters position error).
  + Change to remove warnings and hints.
  + Change to Access Violation code
    (When returned column precision from LongWord to Smallint type when
    precission is more High(smallint), ... )
  + Change to setting metadata position for bad odbc driver
    (Read of columns information with the "Easysoft Interbase ODBC Driver"
     version 1.00.01.67 on example "dbxexplorer.dpr").
  + Changes when QuoteChar=' '.
    In this situation QuoteChar must be empty (''). (MSSQL, Informix,...)
    ( Edward> ???Ed>Vad/All: But I think MSSQL uses doublequote char ("), not blank. )
  + Change to support Trim of Fixed Char when connection parameter "Trim Char" is True
    or when connection parameter: Database=...;TrimChar=1.
    The mode 1 - allows to work in the mode compatible
    with the "BDE" mode for "FixedChar" of strings.
    Mode 0 - is default - the strings of fixed width are not truncating.
  + New SchemaFilter parameter in login parameter "Custom String".
    This parameter allows to filter the metadata of the only current scheme.
    By default filtering is on for: Oracle.
    If it does not settle - disable filtering through the parameter of connection:
    Custom String=...;SchemaFilter=0
  + Change to autodetect ODBC driver level mode 2.
  + Change to autodetect SupportsCatalog Options.
    Warning: Some of the driver is illconditioned work with this option.
    For example do not return an error at installation of a unknown of the catalog.
    From behind it the procedure of installation of the catalog was received
    cumbersome and depending from database.
    But you have possibility of load shedding of support of the catalog.
    Read further about parameter of Catalog...
  + The possibility is supplemented to disable support of the
    'Catalog option. Database=...;Catalog=0
  + Change to increase of speed of blob fetching.
    Database or "Custom String" parameter "BlobChunkSize".
    In Bytes. Define size blob buffer for loop-fetching.
    The size of a cache can be synchronized with a size of a cache assigned in the ODBC driver.
  + Vadim> ???Vad>All: Change to support Odbc driver attribute SQL_ATTR_PACKET_SIZE.
    Edward> Very good! Although I think it is not advisable to ever change this,
    Edward> Borland has seen fit to add it as an option, so we should implement it.
    For support this attribute you must define value it parameter in
    "Custom String" (Delphi 7) or "Database" (Delphi 6, 7)
    "Custom String"="...;ConPacketSize=8192";
    Database="..;ConPacketSize=8192"
    ConPacketSize should not be less than 4096. The upper range is defined by the driver.
  - Database or "Custom String" parameter "DriverLevel"
    user defined ODBC driver level mode.
    Edward> Now removed - we now auto-detect driver level
  - Change in ParseTableName for parsing in informix ...
    Warning: Probably and for other database servers it is necessary to change
    in view of their format of the job(definition) of a full name of the table.
  + Ignoring of exceptions for want of indexes.
  + It is possible at call to the tables from other spaces (catalog, servers, references).
    (Edward> what do you mean?)
  + The possibility of the external definition of parameters of the driver
    is supplemented (Catalog,TrimChar,BlobChankSize).
      Examples:
        Delphi 7:
          Custom String=;Catalog=0;TrimChar=1;MapInt64ToBcd=1;SchemaFilter=1;
           Metadata=0;MapSmallBcdToNative=0;BlobChunkSize=32768
        Delphi 6 (also will work for Delphi 7):
          Database=DSN=DBDEMOS;UID=anonymous;PWD=unknown;
           TrimChar=1;BlobChunkSize=32768;ConPacketSize=3072
  - Change in "SqlExpr.pas" (Delphi 6,7):
     Change for reading of metadata when ODBC driver supported
     only one sql statement (MSSQL...).
     Change for support of the "connection string with prompt" '?'
     when need clone connection.
     For more detail look then file WhatNews.Txt.
    (Edward> I have done this by internally cloning the connection to handle this case)
  + All changes are included in the block:
      //                                   {+ver Optional description}
      //                                   ... new or changed code
      //                                   {/+ver /Optional description}

*)
{/+2.01 /WhatNews}

{+2.02 WhatNews}
(*
Version 2.02, 2002-11-04 [ Vadim V.Lopushansky pult@ukr.net ]
------------------------

      All changes are concluded in the block:
      {+2.02 Optional Description}
       ... new or changed code
      {/+2.02 /Optional Description}

+ added suported INTERVAL types as Fixed Char
  (look SQL_INTERVAL_YEAR or SQL_INTERVAL_MINUTE_TO_SECOND )
+ added optiong for ignoring of uknknown field types
  (look coNoIgnoreUnknownFieldType and IgnoreUnknownType )
  Connectin parameter: Database=...;IgnoreUnknownType=1
  or parameter "Custom String"="...;IgnoreUnknownType=1"
  Default is False(0) except informix. For informix=True(1)
+ Set default isolation to DirtyRead (look SQL_TXN_READ_UNCOMMITTED) (???)
- ??? Set default CURSOR BEHAVIOR to PRESERVE MODE. !!! Has failed !!!
(look SQL_CURSOR_COMMIT_BEHAVIOR) (???)
+ detect RDBMS types ( you can analyze RDBMS name, major and minor version, and client version )
*)
{/+2.02 /WhatNews}

{
Version 2.03, 2002-11-20 [ Edward Benson ]
------------------------
{
+ Split ISqlConnectionOdbc out to new module, DbxOpenOdbcInterface.
  This allows you to call the new methods of ISqlConnection,
  but without having to statically link in this module.
  (See QueryInterface comments in DbxOpenOdbcInterface on how to do this).


{+2.01 Polite request}
(*
Edward> ???Ed>All: Polite request to contributors
Edward> Please try to keep line lengths within a reasonable width, to avoid
Edward> having to scroll right. My monitor is only 1024x768.
Edward> (I have also cleaned up some of my own offending code in the regard)
Edward>
Edward> Another thing. I have a way I like to format my code, the main things are:
Edward>   - indentation of 2 for each nesting level (this helps with line width)
Edward>   - compound statements left-aligned with the enclosing begin / end
Edward>     (except the outer level, because Borland auto-complete does it differently),
Edward>   - if then else formatted like this:
Edward>     if condition then
Edward>       statement // < I prefer to see this on a new line, indented 2
Edward>     else if condition then
Edward>       statement // < I prefer to see this on a new line
Edward>     else        // < I prefer to see this on a new line, aligned with 'if'
Edward>       statement // < I prefer to see this on a new line
*)
{/+2.01 /Polite request}

{+2.04 WhatNews}
(*
Version 2.04, 2002-12-19 [ Vadim V.Lopushansky pult@ukr.net ]
------------------------
+ Regular Expression Parser for Decode/Encode different DBMS object name format.
  Usage of this capability is adjusted(regulated) in parameter: {$define _RegExprParser_}
  It option can be turned off.

  If for your DBMS the off-gauge format of the definition of a full name of the object of DBMS,
  to you is necessary to describe the template of this format in the file "DbxObjectParser.pas".
  For debugging your template you can take advantage of an example from "RegExprParser.zip".

+ The capability of mapping of text fields into memo field is added as it is made in BDE:
  the fields with lengthy more than 256 characters are imaged on BlobMemo
  (optional. Connection parameter: "Database"="...;MapCharAsBDE=1"
    or "Custom String"="...;MapCharAsBDE=1")
*)
{/+2.04 /WhatNews}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{ getSQLDriverODBC is the starting point for everything else... }

function getSQLDriverODBC(sVendorLib : PChar; sResourceFile : PChar; out Obj): SQLResult; stdcall;

exports getSQLDriverODBC;

{+2.01}
const
   cBlobChunkSizeDefault = 40960;
   cBlobChunkSizeLimit   = 1024 * 100;

type
  TSqlConnectionOdbc = class;
{/+2.01}
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
    fDbxOptionDrvRestrict: LongWord;
    procedure AllocHCon(out HCon: SQLHDBC);
    procedure AllocHEnv;
    procedure FreeHCon(HCon: SQLHDBC);
    procedure FreeHEnv;
    procedure RetrieveOdbcErrorInfo(CheckCode: SQLRETURN;
     HandleType: Smallint; Handle: SQLHANDLE; Connection: TSqlCOnnectionOdbc);
    procedure OdbcCheck(
      CheckCode: SQLRETURN;
      const OdbcFunctionName: string;
      HandleType: Smallint;
      Handle: SQLHANDLE;
      Connection: TSqlConnectionOdbc = nil);
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
    procedure Drivers(DriverList: TStrings);
  end;

   {+2.01 New custom connection options}
   //Vadim V.Lopushansky:

  PConnectionStatement = ^TConnectionStatement;
  TConnectionStatement = record
    fhCon: SQLHDBC;
    fhStmt: SQLHDBC;
    end;

   TConnectionOption = (coNoSupportsMetadata, // Connection parameter: Database=...;Metadata=0
      coSupportsMetadata,   // or parameter "Custom String"="...;Metadata=1"
      // Default is True(1)

      coNoSupportsSchemaFilter, // Connection parameter: Database=...;SchemaFilter=0
      coSupportsSchemaFilter,   // or parameter "Custom String"="...;SchemaFilter=1"
      // Default is False(0)

      coNoTrimChar,  // Support eConnTrimChar: Trim of Fixed Char when connection parameter
      coTrimChar,
      // "Trim Char" is True or when connection parameter: Database=...;TrimChar=1 or parameter "Custom String"="...;TrimChar=0"
      // Default is False(0)

      coNoMapInt64ToBcd, // Connection parameter: Database=...;MapInt64ToBcd=0
      coMapInt64ToBcd,   // or parameter "Custom String"="...;MapInt64ToBcd=1"
      // Default is False(0)

      coNoMapSmallBcdToNative,  // Connection parameter: Database=...;MapSmallBcdToNative=0
      coMapSmallBcdToNative,    // or parameter "Custom String"="...;MapSmallBcdToNative=1"
      // Default is False(0)

      coNoSupportsCatalog,    // Connection parameter: Database=...;Catalog=0
      coSupportsCatalog,       // or parameter "Custom String"="...;Catalog=1"
      // Default is True(1) or is specified for DriverType

      {+2.02}
      coNoIgnoreUnknownFieldType, // Connectin parameter: Database=...;IgnoreUnknownType=1
      coIgnoreUnknownFieldType,   // or parameter "Custom String"="...;IgnoreUnknownType=1"
      // Default is False(0) except informix. For informix=True(1)
      {/+2.02}

      {+2.04}
      coNoMapCharAsBDE, // Connectin parameter: Database=...;MapCharAsBDE=1
      coMapCharAsBDE    // or parameter "Custom String"="...;MapCharAsBDE=1"
      // Default is False(0)
      {/+2.04}
      );

   TConnectionOptions = set of TConnectionOption;

   {/+2.01 /New custom connection options}

{ TSqlConnectionOdbc implements ISQLConnection }

  TSqlConnectionOdbc = class(TInterfacedObject, ISQLConnection, ISqlConnectionOdbc)
  private
    fConnectionErrorLines: TStringList;
    fOwnerDbxDriver: TSqlDriverOdbc;
    fDbxCallBack: TSQLCallBackEvent;
    fDbxCallBackInfo: Integer;
    fConnected: boolean;
    fConnBlobSizeLimitK: Integer;
// Private fields below are specific to ODBC
    fhCon: SQLHDBC;
    fStatementPerConnection: SQLUSMALLINT;
    // Connection + Statement cache, for databases that support
    // only 1 statement per connection (eg MS SqlServer) :
    fConnectionStatementList: TList;
    fWantQuotedTableName: boolean;
    fOdbcConnectString: string;
    fOdbcConnectStringHidePassword: string;
    fOdbcReturnedConnectString: pAnsiChar;
    fOdbcMaxColumnNameLen: SQLUSMALLINT;
    fOdbcMaxCatalogNameLen: SQLUSMALLINT;
    fOdbcMaxSchemaNameLen: SQLUSMALLINT;
    fOdbcMaxTableNameLen: SQLUSMALLINT;
    fOdbcMaxIdentifierLen: SQLUSMALLINT;
    fDbmsName: string;
    fDbmsType: TDbmsType;
    fDbmsVersionString: string;
    fDbmsVersionMajor: integer;
    fOdbcDriverName: string;
    fOdbcDriverType: TOdbcDriverType;
    fOdbcDriverVersionString: string;
    fOdbcDriverVersionMajor: integer;
    fOdbcDriverLevel: integer; // 2 or 3
    fInTransaction: boolean;
    fSupportsCatalog: boolean;
    fSupportsSQLSTATISTICS: boolean;
    fSupportsSQLPRIMARYKEYS: boolean;
    fSupportsSchemaDML: boolean;
    fSupportsSchemaProc: boolean;
    fSupportsCatalogDML: boolean;
    fSupportsCatalogProc: boolean;
    fGetDataAnyColumn: boolean;
    fCurrentCatalog: pAnsiChar;
    fQuoteChar: AnsiChar;
    fAutoCommitMode: SQLUINTEGER;
    fSupportsTransaction: boolean;
    {+2.01}
    //Vadim V.Lopushansky:
    fCurrentSchema: string; // This is no ODBC API call to get this! // defined by option: fSupportsSchemaFilter
    fConnectionOptions: TConnectionOptions;
    fBlobChunkSize: Integer;
    {$IFDEF _D7UP_}
    //Vadim> ???Vad>All:
    //Vadim>  for support SetOption(eConnQualifiedName)
    //Vadim>  and  getOption(eConnCatalogName...
    //Vadim>  getOption(eConnSchemaName...
    //Vadim>  getOption(eConnObjectName
    //Edward> I do not have Delphi 7, and this stuff is tricky.
    //Edward> But I have kept your change.
    fQualifiedName: string;
    {$ENDIF}
    {/+2.01}
//    fCurrentSchema: string; // This is no ODBC API call to get this!
    {+2.03 Ability to retrieve Error info}
    fNativeErrorCode: SQLINTEGER;
    fSqlStateChars: TSqlState; // 5 chars long + null terminator
    {/+2.03 /Ability to retrieve Error info}
    {+2.03 Bypass SetCatalog call}
    fDbxCatalog: string;
    fDbmsVersion: string;
    {$ifdef _RegExprParser_}
    fObjectNameParser:TObjectNameParser;
    {$endif}
    {/+2.03 /Bypass SetCatalog call}
    procedure AllocHStmt(out HStmt: SQLHSTMT);
    procedure CheckTransactionSupport;
    procedure CloneOdbcConnection(out HCon: SQLHDBC);
    procedure FreeHStmt(HStmt: SQLHSTMT);
    function GetMetaDataOption(
      eDOption: TSQLMetaDataOption;
      PropValue: Pointer;
      MaxLength: SmallInt;
      out Length: SmallInt
      ): SQLResult;
    procedure OdbcCheck(OdbcCode: SQLRETURN; const OdbcFunctionName: string);
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
    { begin ISQLConnectionOdbc methods }
    function GetDbmsName: string;
    function GetDbmsType: TDbmsType;
    function GetDbmsVersionString: string;
    function GetDbmsVersionMajor: integer;
    function GetLastOdbcSqlState: pchar;
    procedure GetOdbcConnectStrings(ConnectStringList: TStrings);
    function GetOdbcDriverName: string;
    function GetOdbcDriverType: TOdbcDriverType;
    function GetOdbcDriverVersionString: string;
    function GetOdbcDriverVersionMajor: integer;
    { end ISQLConnectionOdbc methods }
  public
    constructor Create(OwnerDbxDriver: TSqlDriverOdbc);
    destructor Destroy; override;
    { begin additional public methods/props }

    property DbmsName: string read fDbmsName;
    property DbmsType: TDbmsType read fDbmsType;
    property DbmsVersionMajor: integer read fDbmsVersionMajor;
    property DbmsVersionString: string read fDbmsVersion;
    property LastOdbcSqlState: pchar read GetLastOdbcSqlState;
    property OdbcConnectString: string read fOdbcConnectString;
    property OdbcDriverName: string read fOdbcDriverName;
    property OdbcDriverType: TOdbcDriverType read fOdbcDriverType;
    property OdbcDriverVersionMajor: integer read fOdbcDriverVersionMajor;
    property OdbcDriverVersionString: string read fOdbcDriverVersionString;

    { end additional public methods/props }
  end;


{ TSqlCommandOdbc implements ISQLCommand }

  TSqlCommandOdbc = class(TInterfacedObject, ISQLCommand)
  private
    fCommandErrorLines: TStringList;
    fOwnerDbxConnection: TSqlConnectionOdbc;
    fOwnerDbxDriver: TSqlDriverOdbc;
    fCommandBlobSizeLimitK: integer;
    fCommandRowSetSize: integer; // New for Delphi 6.02, but not yet used
    fSql: string;  // fSQL is saved in prepare / executeImmediate
// Private fields below are specific to ODBC
    fhStmt: SQLHSTMT;
    fStmtFreed: boolean;
    fOdbcParamList: TList;
    {+2.01}
    //Vadim V.Lopushansky:
    fTrimChar: Boolean;
    {/+2.01}
    fExecutedOk: boolean;
    procedure OdbcCheck(OdbcCode: SQLRETURN; const OdbcFunctionName: string);
  protected
    { begin ISQLCommand methods }
    function SetOption(
      eSqlCommandOption: TSQLCommandOption;
      ulValue: Integer
      ): SQLResult; stdcall;
    function GetOption(
      eSqlCommandOption: TSQLCommandOption;
      {+2.01}
      // Borland changed GetOption function prototype between Delphi V6 and V7}
      {$IFDEF _D7UP_}
      PropValue: Pointer;
      {$ELSE}
      var pValue: Integer;
      {$ENDIF}
      {/+2.01}
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
    procedure OdbcCheck(OdbcCode: SQLRETURN; const OdbcFunctionName: string);
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
    {+2.01}
    //Vadim V.Lopushansky: add support of PARAM_POSITION
    fPosition: SmallInt;
    {/+2.01}
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

    procedure OdbcCheck(OdbcCode: SQLRETURN; const OdbcFunctionName: string);
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
    fBlobFetched: boolean;
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

const
  OdbcReturnedConnectStringMax = 1024;

// IBM DB2 extensions to ODBC API
  SQL_LONGDATA_COMPAT   = 1253;
  SQL_LD_COMPAT_NO      = 0;
  SQL_LD_COMPAT_YES     = 1;

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
 const OdbcFunctionName: string;
 HandleType: Smallint;
 Handle: SQLHANDLE;
 Connection: TSqlConnectionOdbc = nil);
begin
  case CheckCode of
  OdbcApi.SQL_SUCCESS:
    exit;
  OdbcApi.SQL_SUCCESS_WITH_INFO:
    begin
    try
    if (OdbcFunctionName = 'SQLDriverConnect (NoPrompt)') then
      exit;
    if (OdbcFunctionName = 'CloneOdbcConnection - SQLDriverConnect (NoPrompt)') then
      exit;
    fOdbcErrorLines.Clear;
    fOdbcErrorLines.Add('SQL_SUCCESS_WITH_INFO returned from ODBC function ' + OdbcFunctionName);
    RetrieveOdbcErrorInfo(CheckCode, HandleType, Handle, Connection);
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
    RetrieveOdbcErrorInfo(CheckCode, HandleType, Handle, Connection);
    raise EDbxOdbcError.Create(fOdbcErrorLines.Text);
    end;
  end;
end;

procedure TSqlDriverOdbc.RetrieveOdbcErrorInfo(
 CheckCode: SQLRETURN;
 HandleType: Smallint;
 Handle: SQLHANDLE;
 Connection: TSqlCOnnectionOdbc);

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
//  fSqlState := '00000' + #0;
  fSqlStateChars := '00000' + #0;

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
    if Connection <> nil then
      begin
      Connection.fSqlStateChars := SqlStateChars;
      Connection.fNativeErrorCode := NativeError;
      end
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

procedure TSqlDriverOdbc.Drivers(DriverList: TStrings);
  const
   DriverDescLengthMax = 255;
   DriverAttributesLengthMax = 4000;
  var
    OdbcRetCode: OdbcApi.SQLRETURN;
    sDriverDescBuffer: pchar;
    sDriverAttributesBuffer: pchar;
    aDriverDescLength: SQLSMALLINT;
    aDriverAttributesLength: SQLSMALLINT;
  begin

  sDriverDescBuffer := AllocMem(DriverDescLengthMax);
  sDriverAttributesBuffer := AllocMem(DriverAttributesLengthMax);
  DriverList.Clear;
  try
  OdbcRetCode := SQLDrivers(fhEnv, SQL_FETCH_FIRST,
    sDriverDescBuffer, DriverDescLengthMax, aDriverDescLength,
    sDriverAttributesBuffer, DriverAttributesLengthMax, aDriverAttributesLength);
    if (OdbcRetCode <> SQL_NO_DATA) then
      OdbcCheck(OdbcRetCode, 'SQLDrivers(SQL_FETCH_FIRST)', SQL_HANDLE_ENV, fhEnv);

  while OdbcRetCode = 0 do
    begin
    DriverList.Add(sDriverDescBuffer);
    OdbcRetCode := SQLDrivers(fhEnv, SQL_FETCH_NEXT,
      sDriverDescBuffer, DriverDescLengthMax, aDriverDescLength,
      sDriverAttributesBuffer, DriverAttributesLengthMax, aDriverAttributesLength);
    if (OdbcRetCode <> SQL_NO_DATA) then
      OdbcCheck(OdbcRetCode, 'SQLDrivers(SQL_FETCH_NEXT)', SQL_HANDLE_ENV, fhEnv);
    end;
  finally
  FreeMem(sDriverAttributesBuffer);
  FreeMem(sDriverDescBuffer);
  end;
  end;

{ TSqlConnectionOdbc }

constructor TSqlConnectionOdbc.Create(OwnerDbxDriver: TSqlDriverOdbc);
begin
  inherited Create;
  fNativeErrorCode := 0;
  fSqlStateChars := '00000' + #0;
  fConnectionErrorLines := TStringList.Create;
  fConnected := false;
  fOwnerDbxDriver := OwnerDbxDriver;
  fOwnerDbxDriver.AllocHCon(fhCon);
  fWantQuotedTableName := true;
  fConnBlobSizeLimitK := fOwnerDbxDriver.fDrvBlobSizeLimitK;
  {+2.01}// default values: (restore when disconnected)
  fOdbcDriverLevel     := 3; // Assume its a level 3 driver
  fBlobChunkSize       := cBlobChunkSizeDefault;
  {/+2.01}
end;

destructor TSqlConnectionOdbc.Destroy;
begin
  disconnect;
  {$ifdef _RegExprParser_}
   FreeAndNil(fObjectNameParser);
  {$endif}
  fConnectionErrorLines.Free;
  inherited;
end;

procedure TSqlConnectionOdbc.CloneOdbcConnection(out HCon: SQLHDBC);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  cbConnStrOut: SQLSMALLINT;
  aTempOdbcReturnedConnectString: PChar;
begin
  fOwnerDbxDriver.AllocHCon(HCon);
  aTempOdbcReturnedConnectString := AllocMem(OdbcReturnedConnectStringMax);
  OdbcRetCode := SQLDriverConnect(
   HCon, 0,
   pAnsiChar(fOdbcReturnedConnectString), SQL_NTS,
   aTempOdbcReturnedConnectString, OdbcReturnedConnectStringMax, cbConnStrOut,
   SQL_DRIVER_NOPROMPT);
  fOwnerDbxDriver.OdbcCheck(OdbcRetCode, 'CloneOdbcConnection - SQLDriverConnect (NoPrompt)',
   SQL_HANDLE_DBC, hCon);
  FreeMem(aTempOdbcReturnedConnectString);
end;

procedure TSqlConnectionOdbc.AllocHStmt(out HStmt: SQLHSTMT);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  aConnectionStatement: PConnectionStatement;

  function FindFreeConnection: PConnectionStatement;
    var
      i: integer;
    begin
    for i := 0 to (fConnectionStatementList.Count - 1) do
      begin
      Result := fConnectionStatementList[i];
      if (Result.fhCon <> SQL_NULL_HANDLE) and (Result.fhStmt = SQL_NULL_HANDLE) then
        exit;
      end;
    Result := nil;  // not found
    end;

begin
  if (fStatementPerConnection <> 0) then
    begin
    aConnectionStatement := FindFreeConnection;
    if (aConnectionStatement = nil) then
      begin
      New(aConnectionStatement);
      self.CloneOdbcConnection(aConnectionStatement^.fhCon);
      aConnectionStatement^.fhStmt := SQL_NULL_HANDLE;
      fConnectionStatementList.Add(aConnectionStatement);
      end;
    OdbcRetCode := SQLAllocHandle(SQL_HANDLE_STMT, aConnectionStatement^.fhCon, HStmt);
    fOwnerDbxDriver.OdbcCheck(OdbcRetCode, 'SQLAllocHandle(SQL_HANDLE_STMT)',
     SQL_HANDLE_STMT, aConnectionStatement^.fhCon);
    aConnectionStatement^.fhStmt := HStmt;
    end
  else
    begin
    OdbcRetCode := SQLAllocHandle(SQL_HANDLE_STMT, fhCon, HStmt);
    fOwnerDbxDriver.OdbcCheck(OdbcRetCode, 'SQLAllocHandle(SQL_HANDLE_STMT)',
     SQL_HANDLE_STMT, fhCon);
    end;
end;

procedure TSqlConnectionOdbc.FreeHStmt(HStmt: SQLHSTMT);
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  i: integer;
  iConnectionStatement: PConnectionStatement;
begin
  if (fStatementPerConnection <> 0) then
    begin
    for i := 0 to (fConnectionStatementList.Count - 1) do
      begin
      iConnectionStatement := fConnectionStatementList[i];
      if (iConnectionStatement^.fhStmt = HStmt) then
        begin
        OdbcRetCode := SQLFreeHandle(SQL_HANDLE_STMT, HStmt);
        fOwnerDbxDriver.OdbcCheck(OdbcRetCode, 'SQLFreeHandle(SQL_HANDLE_STMT)',
         SQL_HANDLE_STMT, HStmt);
        // Indicate that connection is free to be re-used...
        iConnectionStatement^.fhStmt := SQL_NULL_HANDLE;
        exit;
        end;
      end;
// if we reach here, the statement handle was not found in the list
    raise EDbxInternalError.Create('TSqlConnectionOdbc.FreeHStmt - Statement handle was not found in list');
    end
  else
    begin
    OdbcRetCode := SQLFreeHandle(SQL_HANDLE_STMT, HStmt);
    OdbcCheck(OdbcRetCode, 'SQLFreeHandle(SQL_HANDLE_STMT)');
    end;
end;

procedure TSqlConnectionOdbc.OdbcCheck(OdbcCode: SQLRETURN; const OdbcFunctionName: string);
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
      {+2.01 Workaround for bad old ODBC drivers}
      //Vadim V.Lopushansky :
      //Vadim> ???Vad>All: For old ODBC Drivers (example: INTERSOLV Inc ODBC Drivers 1997 )
      //Edward> This looks OK, but I cannot test as I do not have such old drivers.
      //Edward> (But really, no one should be using drivers 5 years old!)
      if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
       begin
         fSupportsCatalog := False;
         FreeMem(fCurrentCatalog);
         fCurrentCatalog := nil
       end;
      //ORIGINAL CODE:
      //OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(SQL_ATTR_CURRENT_CATALOG)');
      {/+2.01 /Workaround for bad old ODBC drivers}
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
{+2.01}
(*
//Edward> ???Ed>Vad/All: I don't think this should be done here - I have commented it out
// Replace <;;> to <;>
  Result := StringReplace(Result, ';;', ';', [rfReplaceAll, rfIgnoreCase]);
//*)
{/+2.01}
  end;

  {+2.01 New function to parse custom options}
  // Vadim V.Lopushansky: parse advanced connection string Boolean options:
  function GetOptionValue(var ConnectString: string; const OptionName: string;
    HideOption: Boolean = False; TrimResult: Boolean = True): string;
   var
     pLeft, pRight: Integer;
     sLeft, sVal: string;
     R: Boolean;
    begin
    pLeft := Pos(OptionName + '=', UpperCase(ConnectString));
    R     := pLeft > 0;
    if (R) then
      begin
      sLeft  := Copy(ConnectString, 1, pLeft - 1);
      pLeft  := pLeft + Length(OptionName) + 1;//skip OptionsName=
      pRight := pLeft;
      while (pRight <= Length(ConnectString)) and (ConnectString[pRight] <> ';') do Inc(pRight);
        sVal := Copy(ConnectString, pLeft, pRight - pLeft);
        if HideOption then ConnectString :=
         StringReplace(sLeft + Copy(ConnectString, pRight, Length(ConnectString) - pRight + 1),
         ';;', ';', [rfReplaceAll, rfIgnoreCase]
         );
      if TrimResult then
        Result := Trim(sVal)
      else
        Result := sVal;
    end
      else Result := '';
  end;
  {/+2.01 /New function to parse custom options}

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
  {+2.01 New custom options}
  tmpS: string;
  // Cache ConnectionOptions from Database property in following variables:
  sSupportsMetadata: string;
  sMapInt64ToBcd: string;
  sMapSmallBcdToNative: string;
  sMapCharAsBDE :string;
  sTrimChar: string;
  sSupportsSchemaFilter: string;
  sSupportsCatalog: string;
  sUserName: string;
  {+2.02}
  sIgnoreUnknownFieldType: string;
  {/+2.02}
  sBlobChunkSize: string;
  vBlobChunkSize: Integer;
  fUserConnectionOptions: TConnectionOptions;
  vConPacketSize: Integer;
  {/+2.01 /New custom options}
  Len: smallint;
  aOdbcSchemaUsage: SQLUINTEGER;
  aOdbcCatalogUsage: SQLUINTEGER;
  aOdbcGetDataExtensions:SQLUINTEGER;
  {+2.02}
  //GetInfoSmallInt: SQLUSMALLINT;
  {/+2.02}
  aConnectionStatement: PConnectionStatement;

  {+2.01}
  // Edward> ???Ed>Vad/All: - I don't know why Vadim uses both coTrue and coFalse
  procedure MergeOption(coFalse, coTrue: TConnectionOption);
  begin
    if not (coFalse in fUserConnectionOptions)
        or (coTrue in fUserConnectionOptions) then
      begin
      if (coFalse in fConnectionOptions) then
        System.Include(fUserConnectionOptions, coFalse)
      else if (coTrue in fConnectionOptions) then
        System.Include(fUserConnectionOptions, coTrue);
      end;
    end;
  {/+2.01}
begin
  Result := DBXpress.SQL_SUCCESS;
  if fConnected then exit;

  {+2.01}
  //Vadim V.Lopushansky
  sSupportsMetadata := '';
  sMapInt64ToBcd    := '';
  sMapSmallBcdToNative := '';
  sMapCharAsBDE    := '';
  sTrimChar        := '';
  sSupportsSchemaFilter := '';
  sSupportsCatalog := '';
  sBlobChunkSize   := '';
  vBlobChunkSize   := fBlobChunkSize;
  vConPacketSize   := 0;
  {/+2.01}

try
  {+2.03}
  // SqlExpr calls SetCatalog for the server name after connect,
  // so save server name to enable check for this case and bypass the call
  fDbxCatalog := ServerName;
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

    {+2.01 Parse custom options}
    // Vadim V.Lopushansky: Parse ConnectionOptions in Database property string

    //Metadata:
    {Edward> ???Ed>Vad/All: - Why does Vadim have both INCLUDE coSupportXXX and EXCLUDE coNoSupportXXX}
    sSupportsMetadata := GetOptionValue(fOdbcConnectString, 'METADATA', True);
    if Length(sSupportsMetadata) = 1 then
      begin
      case sSupportsMetadata[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coSupportsMetadata);
        System.Exclude(fConnectionOptions, coNoSupportsMetadata);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoSupportsMetadata);
        System.Exclude(fConnectionOptions, coSupportsMetadata);
        end;
      else
         sSupportsMetadata := '';
      end;
      end;

    //MapInt64ToBcd:
    sMapInt64ToBcd := GetOptionValue(fOdbcConnectString, 'MAPINT64TOBCD', True);
    if Length(sMapInt64ToBcd) = 1 then
      begin
      case sMapInt64ToBcd[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coMapInt64ToBcd);
        System.Exclude(fConnectionOptions, coNoMapInt64ToBcd);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoMapInt64ToBcd);
        System.Exclude(fConnectionOptions, coMapInt64ToBcd);
        end;
      else
        sMapInt64ToBcd := '';
      end;
      end;

    //MapSmallBcdToNative:
    sMapSmallBcdToNative := GetOptionValue(fOdbcConnectString, 'MAPSMALLBCDTONATIVE', True);
    if Length(sMapSmallBcdToNative) = 1 then
      begin
      case sMapSmallBcdToNative[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coMapSmallBcdToNative);
        System.Exclude(fConnectionOptions, coNoMapSmallBcdToNative);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoMapSmallBcdToNative);
        System.Exclude(fConnectionOptions, coMapSmallBcdToNative);
        end;
      else
        sMapSmallBcdToNative := '';
      end;
      end;

    //MapCharAsBDE:
    sMapCharAsBDE := GetOptionValue(fOdbcConnectString, 'MAPCHARASBDE', True);
    if Length(sMapCharAsBDE) = 1 then
      begin
      case sMapCharAsBDE[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coMapCharAsBDE);
        System.Exclude(fConnectionOptions, coNoMapCharAsBDE);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoMapCharAsBDE);
        System.Exclude(fConnectionOptions, coMapCharAsBDE);
        end;
      else
        sMapCharAsBDE := '';
      end;
      end;

    //TrimChar:
    sTrimChar := GetOptionValue(fOdbcConnectString, 'TRIMCHAR', True);
    if Length(sTrimChar) = 1 then
      begin
      case sTrimChar[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coTrimChar);
        System.Exclude(fConnectionOptions, coNoTrimChar);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoTrimChar);
        System.Exclude(fConnectionOptions, coTrimChar);
        end;
      else
        sTrimChar := '';
      end;
      end;

    //SchemaFilter:
    sSupportsSchemaFilter := GetOptionValue(fOdbcConnectString, 'SCHEMAFILTER', True);
    if Length(sSupportsSchemaFilter) = 1 then
      begin
      case sSupportsSchemaFilter[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coSupportsSchemaFilter);
        System.Exclude(fConnectionOptions, coNoSupportsSchemaFilter);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoSupportsSchemaFilter);
        System.Exclude(fConnectionOptions, coSupportsSchemaFilter);
        end;
      else
        sSupportsSchemaFilter := '';
      end;
      end;

    //Catalog:
    sSupportsCatalog := GetOptionValue(fOdbcConnectString, 'CATALOG', True);
    if Length(sSupportsCatalog) = 1 then
      begin
      case sSupportsCatalog[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coSupportsCatalog);
        System.Exclude(fConnectionOptions, coNoSupportsCatalog);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoSupportsCatalog);
        System.Exclude(fConnectionOptions, coSupportsCatalog);
        end;
      else
        sSupportsCatalog := '';
      end;
      end;

    //BlobChunkSize:
    sBlobChunkSize := GetOptionValue(fOdbcConnectString, 'BLOBCHUKSIZE', True);
    if Length(sBlobChunkSize) > 0 then
      begin
      vBlobChunkSize := StrToIntDef(sBlobChunkSize, - 1);
      if vBlobChunkSize < 0 then
        sBlobChunkSize := ''
      else
        begin
        if vBlobChunkSize < 256 then
          vBlobChunkSize := 256
        else if vBlobChunkSize > cBlobChunkSizeLimit then
          vBlobChunkSize := cBlobChunkSizeLimit;
        sBlobChunkSize := IntToStr(fBlobChunkSize);
        end;
      end;

    {+2.02 New IgnoreUnknownFieldType option}
    sIgnoreUnknownFieldType := GetOptionValue(fOdbcConnectString, 'IGNOREUNKNOWNTYPE', True);
    if Length(sIgnoreUnknownFieldType) = 1 then
      case sIgnoreUnknownFieldType[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coIgnoreUnknownFieldType);
        System.Exclude(fConnectionOptions, coNoIgnoreUnknownFieldType);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoIgnoreUnknownFieldType);
        System.Exclude(fConnectionOptions, coIgnoreUnknownFieldType);
        end;
      else
        sIgnoreUnknownFieldType := '';
      end;
    {/+2.02 /New IgnoreUnknownFieldType option}

    //ConPacketSize:
    vConPacketSize := StrToIntDef(GetOptionValue(fOdbcConnectString, 'CONPACKETSIZE', True), 0);
    if (vConPacketSize >= 4096) then
      begin
      OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_PACKET_SIZE, Pointer(vConPacketSize), 0);
      if OdbcRetCode <> OdbcApi.SQL_SUCCESS then
        vConPacketSize := 0;
      end
    else
      vConPacketSize := 0;
    {/+2.01 /Parse custom options}

{+2.01 Vadim's mystic code (commented for now)}
(*
//Edward> ???Ed>Vad/All: - I do not understand what Vadim is trying to do here;
//Edward> commented out for now!

//Vadim V.Lopushansky: Change to: Correction of check of the rights in ConnectionString

         // Check to see if User Id already specified in connect string
         if (UserName[0] <> #0) then
          begin
            //User Name:
            if (Pos('UID=', UpperCase(ServerName)) <> 0) then
             begin
               // Hide option:
               GetOptionValue(fOdbcConnectString, 'UID', True, False);
               // Define new value for this option
               fOdbcConnectString := fOdbcConnectString + ';UID=' + UserName;
             end
            else if (Pos('USERID=', UpperCase(ServerName)) <> 0) then
             begin
               GetOptionValue(fOdbcConnectString, 'USERID', True, False);
               fOdbcConnectString := fOdbcConnectString + ';USERID=' + UserName;
             end
            else fOdbcConnectString := fOdbcConnectString + ';UID=' + UserName;
            //Password:
            // Check to see if Password already specified in connect string
            if (Password[0] <> #0) then
             begin
               if (Pos('PWD=', UpperCase(ServerName)) <> 0) then
                begin
                  GetOptionValue(fOdbcConnectString, 'PWD', True, False);
                  fOdbcConnectString := fOdbcConnectString + ';PWD=' + Password;
                end
               else if (Pos('PASSWORD=', UpperCase(ServerName)) <> 0) then
                begin
                  GetOptionValue(fOdbcConnectString, 'PASSWORD', True, False);
                  fOdbcConnectString := fOdbcConnectString + ';PASSWORD=' + Password;
                end
               else fOdbcConnectString := fOdbcConnectString + ';PWD=' + Password;
             end
            else
             begin //Hide password:
               // INFORMIX: allow undefined password
               GetOptionValue(fOdbcConnectString, 'PWD', True, False);
               GetOptionValue(fOdbcConnectString, 'PASSWORD', True, False);
             end;
          end
         else
          begin
            // Do not set password for unknown user
            // Clearing empty password value:
            if (Pos('PWD=', UpperCase(ServerName)) <> 0) then
             begin
               tmpS := GetOptionValue(fOdbcConnectString, 'PWD', True, False);
               if Length(tmpS) > 0
                 then fOdbcConnectString := fOdbcConnectString + ';PWD=' + tmpS;
             end
            else if (Pos('PASSWORD=', UpperCase(ServerName)) <> 0) then
             begin
               tmpS := GetOptionValue(fOdbcConnectString, 'PASSWORD', True, False);
               if Length(tmpS) > 0
                 then fOdbcConnectString := fOdbcConnectString + ';PASSWORD=' + tmpS;
             end
          end;

//*)
{/+2.01 /Vadim's mystic code (commented for now)}

    // Check to see if User Id already specified in connect string -
    // If not already specified in connect string,
    // we use UserName passed in the Connect function call (if non-blank)
    if (Pos('UID=', UpperCase(ServerName)) = 0) and
       (Pos('USERID=', UpperCase(ServerName)) = 0) and
       (UserName[0] <> #0) then
      fOdbcConnectString := fOdbcConnectString + ';UID=' + UserName;

    // Check to see if Password already specified in connect string -
    // If not already specified in connect string,
    // we use Password passed in the Connect function call (if non-blank)
    if (Pos('PWD=', UpperCase(ServerName)) = 0) and
       (Pos('PASSWORD=', UpperCase(ServerName)) = 0) and
       (Password[0] <> #0) then
      fOdbcConnectString := fOdbcConnectString + ';PWD=' + Password;


{+2.01}
//Vadim V.Lopushansky:  Deleting superfluous <;;>
(*
//Edward> ???Ed>Vad Why change ;; to ;
//Edward> ???Ed>Vad Trim final ; is unnecessary
//Edward> Changes commented for now

         if (Length(fOdbcConnectString) > 0) then
          begin
            // replace <;;> to <;>
            fOdbcConnectString := StringReplace(fOdbcConnectString, ';;', ';', [rfReplaceAll, rfIgnoreCase]);
            // trim last <;>
            if (fOdbcConnectString[Length(fOdbcConnectString)] = ';')
              then SetLength(fOdbcConnectString, Length(fOdbcConnectString) - 1);
          end;
//*)
{/+2.01}

    fOdbcConnectStringHidePassword := HidePassword(fOdbcConnectString);

    end;

  fOdbcReturnedConnectString := AllocMem(OdbcReturnedConnectStringMax);

{$IFDEF MSWINDOWS}
{+2.01}
//Vadim> ???Vad>Ed/All: If process is not NT service (need checked)
//Edward> When doing SQLDriverConnect, the Driver manager and/or Driver may display a
//Edward> dialog box to prompt user for additional connect parameters.
//Edward> So SQLDriverConnect has a Window Handle Parameter to use as the parent.
//Edward> In Windows I pass the Active Window handle for this parameter,
//Edward> but in Kylix, I do not know the equivalent call, so I just pass 0.
{/+2.01}
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

  {+2.01}
  // Vadim V.Lopushansky:
  // for support cloning connection when returning database connection string
  // Edward> ???Ed>Vad: Can we remove this now we are internally cloning connection

  if Length(sSupportsMetadata) = 1 then
    fOdbcConnectString := fOdbcConnectString + ';Metadata=' + sSupportsMetadata;

  if Length(sMapInt64ToBcd) = 1 then
    fOdbcConnectString := fOdbcConnectString + ';MapInt64ToBcd=' + sMapInt64ToBcd;

  if Length(sMapSmallBcdToNative) = 1 then
    fOdbcConnectString := fOdbcConnectString + ';MapSmallBcdToNative=' + sMapSmallBcdToNative;

  if Length(sMapCharAsBDE) = 1 then
    fOdbcConnectString := fOdbcConnectString + ';MapCharAsBDE=' + sMapCharAsBDE;

  if Length(sTrimChar) = 1 then
    fOdbcConnectString := fOdbcConnectString + ';TrimChar=' + sTrimChar;

  if Length(sSupportsSchemaFilter) = 1 then
    fOdbcConnectString := fOdbcConnectString + ';SchemaFilter=' + sSupportsSchemaFilter;

  if Length(sSupportsCatalog) = 1 then
    fOdbcConnectString := fOdbcConnectString + ';Catalog=' + sSupportsCatalog;

  if Length(sBlobChunkSize) > 0 then
    begin
    fOdbcConnectString := fOdbcConnectString + ';BlobChunkSize=' + sBlobChunkSize;
    fBlobChunkSize     := vBlobChunkSize;
    end;

{+2.02}
       // Vadim> ConPacketSize:
       // Vadim> ???Vad>Vad/All: I do not know when it is necessary to install
       // Vadim> this option: up to or after connection
//       if (vConPacketSize >= 4096) then
//        begin
//          OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_PACKET_SIZE, Pointer(vConPacketSize), 0);
//          OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr( SQL_ATTR_PACKET_SIZE )');
//          if OdbcRetCode <> OdbcApi.SQL_SUCCESS
//            then vConPacketSize := 0;
//        end;
  if vConPacketSize >= 4096 then
    fOdbcConnectString := fOdbcConnectString + ';ConPacketSize=' + IntToStr(vConPacketSize);
{/+2.02}

{/+2.01}

  ReallocMem(fOdbcReturnedConnectString, cbConnStrOut+1);

{+2.01}
  //Vadim V.Lopushansky: We save of user's set-up before installation of
  // customizations defined by the driver
  fUserConnectionOptions := fConnectionOptions;
{/+2.01}

  RetrieveDriverName;

{+2.01}
  //Vadim V.Lopushansky:
  //We unite of set-up of the user to customizations of the driver
  // Set-up of the user have the greater priority before customizations defined automatically
  MergeOption(coNoSupportsMetadata, coSupportsMetadata);
  MergeOption(coNoSupportsSchemaFilter, coSupportsSchemaFilter);
  MergeOption(coNoTrimChar, coTrimChar);
  MergeOption(coNoMapInt64ToBcd, coMapInt64ToBcd);
  MergeOption(coNoMapSmallBcdToNative, coMapSmallBcdToNative);
  MergeOption(coNoMapCharAsBDE, coMapCharAsBDE);
  MergeOption(coNoSupportsCatalog, coSupportsCatalog);
  {+2.02}
  MergeOption(coNoIgnoreUnknownFieldType, coIgnoreUnknownFieldType);
  {/+2.02}

  fConnectionOptions := fUserConnectionOptions;

  //Vadim V.Lopushansky:

  // if Undefined SupportsMetadata option then define by default
  if not
     ((coSupportsMetadata in fConnectionOptions) or
     (coNoSupportsMetadata in fConnectionOptions)) then
    System.Include(fConnectionOptions, coSupportsMetadata);

  // Parsing default(current) SchemaName. It is equal logoon UserName
  tmpS      := StrPas(fOdbcReturnedConnectString);
  sUserName := GetOptionValue(tmpS, 'UID', True, False);
  if Length(sUserName) = 0 then
    sUserName := GetOptionValue(tmpS, 'USERID');
  fCurrentSchema := sUserName;

  // Prepare other ConnectionOptions:
  if not (coSupportsSchemaFilter in fConnectionOptions) then
    System.Include(fConnectionOptions, coNoSupportsSchemaFilter);

  if not (coTrimChar in fConnectionOptions) then
    System.Include(fConnectionOptions, coNoTrimChar);

  if not (coMapInt64ToBcd in fConnectionOptions) then
    System.Include(fConnectionOptions, coNoMapInt64ToBcd);

  if not (coMapSmallBcdToNative in fConnectionOptions) then
    System.Include(fConnectionOptions, coNoMapSmallBcdToNative);

  if not (coMapCharAsBDE in fConnectionOptions) then
    System.Include(fConnectionOptions, coNoMapCharAsBDE);

{/+2.01}

{  OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_METADATA_ID, pointer(SQL_TRUE), 0);
}

{+2.01}
  //Vadim V.Lopushansky:
  // Edward> ???Ed>Vad> Why do we need both coSupportsCatalog and coNoSupportsCatalog
  if (coSupportsCatalog in fConnectionOptions) or (not (coNoSupportsCatalog in fConnectionOptions)) then
    begin
    OdbcRetCode      := SQLGetInfoString(fhCon, SQL_CATALOG_NAME, @aBuffer, SizeOf(aBuffer), StringLength);
    fSupportsCatalog := (OdbcRetCode = OdbcApi.SQL_SUCCESS) and (aBuffer[0] = 'Y')
    end
  else
    fSupportsCatalog := False;

  if fSupportsCatalog then
    begin
    System.Include(fConnectionOptions, coSupportsCatalog);
    System.Exclude(fConnectionOptions, coNoSupportsCatalog);
    end
  else
    begin
    System.Include(fConnectionOptions, coNoSupportsCatalog);
    System.Exclude(fConnectionOptions, coSupportsCatalog);
    end;
{/+2.01}

  // IBM DB2 has driver-specific longdata type, but setting this option makes it ODBC compatible:
  if self.fOdbcDriverType = eOdbcDriverTypeIbmDb2 then
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
    fOdbcMaxIdentifierLen := 128;

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
  {+2.01}
  // Vadim> ???Vad>Ed: Why not SQL_GD_ANY_ORDER ?
  // Edward> Yes you right - I have renamed fGetDataAnyOrder to fGetDataAnyColumn
  {/+2.01}
(*

SQL_GD_ANY_COLUMN = SQLGetData can be called for any unbound column,
including those before the last bound column.
Note that the columns must be called in order of ascending column number
unless SQL_GD_ANY_ORDER is also returned.

SQL_GD_ANY_ORDER = SQLGetData can be called for unbound columns in any order.
Note that SQLGetData can be called only for columns after the last bound column
unless SQL_GD_ANY_COLUMN is also returned.
*)
  fGetDataAnyColumn := ((aOdbcGetDataExtensions and SQL_GD_ANY_COLUMN) <> 0);

  GetMetaDataOption(eMetaObjectQuoteChar, @fQuoteChar, 1, Len);
{+2.04}
  {$ifdef _RegExprParser_}
   FreeAndNil(fObjectNameParser);
   fObjectNameParser := TObjectNameParser.Create( DbmsObjectNameTemplateInfo[fDbmsType], fQuoteChar );
  {$endif}
{/+2.04}
{+2.03}
{Comment out code added in 2.02}
(*
  {+2.02}
  // Vadim> ???Vad>Vad/All:
  // set default connection parameters:
  OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_TXN_CAPABLE, GetInfoSmallInt, SizeOf(GetInfoSmallInt), nil);
  if (OdbcRetCode = OdbcApi.SQL_SUCCESS) then
    begin
    if GetInfoSmallInt <> SQL_TC_NONE then
      begin
      // if driver supports transactions
      //OdbcRetCode :=
// Edward> Ed>Vad> Line below commented; AUTO_COMMIT_ON is the ODBC and the DBExpress default mode
      // SQLSetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT, SQLPOINTER(SQL_AUTOCOMMIT_OFF), 0);
      //OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(SQL_ATTR_AUTOCOMMIT)');
      // default transaction type
      //OdbcRetCode :=

// Edward>???Ed>Vad> I do not like READ_UNCOMMITTED as the default, so I have commented this
{      SQLSetConnectAttr( fhCon, SQL_ATTR_TXN_ISOLATION,
       SQLPOINTER(
       SQL_TXN_READ_UNCOMMITTED  // DirtyRead
       //SQL_TXN_READ_COMMITTED  // ReadCommited
       ), 0); //}
      //OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(SQL_ATTR_TXN_ISOLATION)');

      { SET CURSOR BEHAVIOR to PRESERVE MODE:
      // Vadim> ???Vad>Vad/All:
     OdbcRetCode :=
      SQLSetConnectAttr(fhCon, SQL_CURSOR_COMMIT_BEHAVIOR, SQLPOINTER(SQL_CB_PRESERVE), 0);
//   OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(SQL_CURSOR_COMMIT_BEHAVIOR, SQL_CB_PRESERVE)');
     OdbcRetCode :=
      SQLSetConnectAttr(fhCon, SQL_CURSOR_ROLLBACK_BEHAVIOR, SQLPOINTER(SQL_CB_PRESERVE), 0);
//   OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(SQL_CURSOR_ROLLBACK_BEHAVIOR, SQL_CB_PRESERVE)');
      {}
      end;
    end;
    {/+2.02}
*) {/+2.03}
  OdbcRetCode := SQLGetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT, @fAutoCommitMode, 0, nil);
  OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(SQL_ATTR_AUTOCOMMIT)');

  CheckTransactionSupport;

// Get max no of statements per connection.
// If necessary, we will internally clone connection for databases that
// only support 1 statement handle per connection, such as MsSqlServer
  OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_MAX_CONCURRENT_ACTIVITIES,
     fStatementPerConnection, 2, nil);
  OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(SQL_MAX_CONCURRENT_ACTIVITIES)');
  if (fStatementPerConnection <> 0) then
    begin
// Create the Connection + Statement cache, for databases that support
// only 1 statement per connection
    fConnectionStatementList := TList.Create;
    New(aConnectionStatement);
    aConnectionStatement^.fhCon := fhCon;
    aConnectionStatement^.fhStmt := SQL_NULL_HANDLE;
    fConnectionStatementList.Add(aConnectionStatement);
    end;

  Result := DBXpress.SQL_SUCCESS;

except
  on E: EDbxError do
    begin
    fConnectionErrorLines.Add(E.Message);
    fConnectionErrorLines.Add('Connection string: ' + fOdbcConnectStringHidePassword);
    Result := MaxReservedStaticErrors + 1;
    {+2.01}
    //Vadim V.Lopushansky: autodisconnect when exception
    // Edward> Thanks, Vadim - that gets rid of 'function sequence error' when
    // attempting to reconnect after bad call
    if fConnected then disconnect;
    {/+2.01}
    end;
end;
end;

function TSqlConnectionOdbc.disconnect: SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  i: integer;
  iConnectionStatement: PConnectionStatement;
begin
try
  if (fStatementPerConnection <> 0) then
    begin
    if (fConnectionStatementList <> nil) then
      begin
      for i := (fConnectionStatementList.Count - 1) downto 0 do
        begin
        iConnectionStatement :=  fConnectionStatementList[i];
        if fConnected then
          begin
          OdbcRetCode := SQLDisconnect(iConnectionStatement.fhCon);
          fOwnerDbxDriver.OdbcCheck(OdbcRetCode, 'SQLDisconnect',
            SQL_HANDLE_DBC, iConnectionStatement.fhCon);
          end;
        fOwnerDbxDriver.FreeHCon(iConnectionStatement.fhCon);
        iConnectionStatement.fhCon := SQL_NULL_HANDLE;
        fConnectionStatementList.Delete(i);
        Dispose(iConnectionStatement);
        end;
      fConnectionStatementList.Free;
      fConnectionStatementList := nil;
      fhCon := SQL_NULL_HANDLE;
      end
    end
  else
    begin
    if (fhCon <> SQL_NULL_HANDLE) then
      begin
      if fConnected then
        begin
        {+2.03}
        {if fInTransaction then
          begin
          OdbcRetCode := SQLEndTran(SQL_HANDLE_DBC, fhCon, SQL_COMMIT);
          OdbcCheck(OdbcRetCode, 'SQLEndTran');
          end; }
        {/+2.03}
        OdbcRetCode := SQLDisconnect(fhCon);
        OdbcCheck(OdbcRetCode, 'SQLDisconnect');
        end;
      fOwnerDbxDriver.FreeHCon(fhCon);
      fhCon := SQL_NULL_HANDLE;
      end;
    end;
  fConnected := false;
  {$ifdef _RegExprParser_}
    FreeAndNil(fObjectNameParser);
  {$endif}
  if (fOdbcReturnedConnectString <> nil) then
    begin
    FreeMem(fOdbcReturnedConnectString);
    fOdbcReturnedConnectString := nil;
    end;
  fOdbcDriverName := '';
  fOdbcDriverType := eOdbcDriverTypeUnspecified;
  if (fCurrentCatalog <> nil) then
    begin
    FreeMem(fCurrentCatalog);
    fCurrentCatalog := nil
    end;
  {+2.01}
  //Vadim V.Lopushansky: Clear extended parameters:
  fConnectionOptions := [];
  fBlobChunkSize     := cBlobChunkSizeDefault;
  fOdbcDriverLevel   := 3;
  {/+2.01}
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

procedure TSqlConnectionOdbc.CheckTransactionSupport;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  GetInfoSmallInt: SQLUSMALLINT;
begin
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
  {+2.02}
  fSupportsTransaction := GetInfoSmallInt <> SQL_TC_NONE;
// Edward> ???Ed>Vad> Vadim's code is more concise, but the point of
// Edward> the original code is to be absolutely explicit on how
// Edward> ODBC isolation levels map to dbExpress isolation level
  {
  case GetInfoSmallInt of
    SQL_TC_NONE        : fSupportsTransaction := false;
    SQL_TC_DML         : fSupportsTransaction := true;
    SQL_TC_DDL_COMMIT  : fSupportsTransaction := true;
    SQL_TC_DDL_IGNORE  : fSupportsTransaction := true;
    SQL_TC_ALL         : fSupportsTransaction := true;
    end;
   }
  {/+2.02}
// Workaund MySql bug - MySql ODBC driver can INCORRECTLY report that it
// supports transactions, so we test it to make sure..
  if ((fOdbcDriverType = eOdbcDriverTypeMySql) and fSupportsTransaction) then
    begin
    OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT, pointer(smallint(SQL_AUTOCOMMIT_OFF)), 0);
    if OdbcRetCode = -1 then
      fSupportsTransaction := false;
    OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT, pointer(smallint(SQL_AUTOCOMMIT_ON)), 0);
    OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(fhCon, SQL_ATTR_AUTOCOMMIT)');
    end;
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
// If omitted, Centura driver returns SQL_SUCCESS_WITH_INFO - Data Truncated,
// and does not return the data.
// So I have had to code the length parameter for all SQLGetInfo calls.
// Never mind, compliant ODBC driver will just ignore the length parameter...

try
  case eDOption of
    eMetaCatalogName: // Dbx Read/Write
      begin
      // {+2.03}
      // Do not return cached catalog name, could be changed, eg. by Sql statement USE catalogname
      GetCurrentCatalog;
      if fCurrentCatalog = nil then
        Length := 0
      else
        Length := StrLen(fCurrentCatalog);
      if (Length = 0) then
        PChar(PropValue)[0] := #0
      else
        begin
        if (MaxLength >= Length) then
          move(PChar(fCurrentCatalog)[0], PChar(PropValue)[0], Length)
        else
          raise EDbxInvalidCall.Create(
           'TSqlConnectionOdbc.GetOption(eMetaCatalogName) MaxLength too small. ' +
           'MaxLength=' + inttostr(MaxLength) +
           ', CurrentCatalog=' + fCurrentCatalog);
        end;
      end;
    eMetaSchemaName: // Dbx Read/Write
      begin
      // There is no ODBC function to get this
      {+2.01}
      //old code:
      //Char(PropValue^) := #0;
      //Length := 0;
      Length := System.Length(fCurrentSchema);
      if Length <= MaxLength then
        AnsiChar(PropValue^) := PChar(fCurrentSchema)^
      else
        raise EDbxInvalidCall.Create(
        'TSQLConnectionOdbc.GetMetaDataOption(eMetaSchemaName) MaxLength parameter is smaller than length of CurrentSchema');
      {/+2.01}
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
      boolean(PropValue^) := fSupportsTransaction;
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
        {+2.01}
        //Vadim V.Lopushansky: never used ' ' QuoteChar
        // Edward> ???Ed>Ed: This looks good, but I have not tested it.
        // Edward> Which ODBC driver(s) return ' ' here?
        // Edward> OK, I see from WhatNews, it is the DataDirect driver for MS SqlServer.
        // Edward> I tested with the MS SqlServer driver, and I get doublequote (").
        if GetInfoStringBuffer[0] = ' ' then
          begin
          fWantQuotedTableName := False;
          Length := 0;
          end
        else
          begin
          AnsiChar(PropValue^) := GetInfoStringBuffer[0];
          Length := 1;
          end
        {/+2.01}
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
    {+2.01}
    {$IFDEF _D7UP_}
    // Vadim> ???Vad>Ed/All:
    // Edward> I do not have Delphi 7 - I don't know either!
    eMetaPackageName:
      begin
      Char(PropValue^) := #0;
      Length           := 0;
      end;
    {$ENDIF}
    {/+2.01}
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
  ConnectAttrLength: SQLUINTEGER;
  {+2.02}
  {$IFDEF _D7UP_}
  SmallintAttrVal: SQLUSMALLINT;
  {$ENDIF}
  {/+2.02}
  {$ifdef _RegExprParser_}
  procedure GetQuotedQualifiedName;
   var sQuotedObjectName :string;
  begin
    sQuotedObjectName := // This is right for multi-part names
      fObjectNameParser.GetQuotedObjectName(fQualifiedName);
    Length := System.Length(sQuotedObjectName);
    if (System.Length(sQuotedObjectName) > 0) and
       (MaxLength >= Length) then
      move(PChar(sQuotedObjectName)[0], PChar(PropValue)[0], Length)
    else
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnObjectName) Length og fQualifiedName string is incorrect');
  end;
  {$endif}
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
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnRoleName) not supported - Applies to Interbase only');
    eConnWaitOnLocks:
      // Boolean that indicates whether application should wait until a locked
      // resource is free rather than raise an exception. (Interbase only)
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnWaitOnLocks) not supported - Applies to Interbase only');
    eConnCommitRetain:
      // Cursors dropped after commit
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnCommitRetain) not supported - Applies to Interbase only');
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
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnHostName) not supported - applies to MySql only');
    eConnDatabaseName: // Readonly
      begin
      OdbcRetCode := SQLGetConnectAttr(fhCon, SQL_DATABASE_NAME, PropValue, MaxLength, @ConnectAttrLength);
      OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(fhCon, SQL_DATABASE_NAME)');
      Length := ConnectAttrLength;
      end;
    eConnObjectMode:
    // Boolean value to enable or disable object fields in Oracle8 tables
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnObjectMode) not supported - applies to Oracle only');
    {+2.01}
    {$IFDEF _D7UP_}
    eConnMaxActiveComm:
    {$ELSE}
    eConnMaxActiveConnection:
    {$ENDIF}
    {/+2.01}
      begin
// The maximum number of active commands that can be executed by a single connection. Read-only.
//
// If database does not support multiple statements, we internally clone
// connection, so return 0 to DbExpress (unlimited statements per connection)
      Smallint(PropValue^) := 0;

// Old code below commented out, v1.04:
{
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
      //}
      end;
    eConnServerCharSet:
      begin
      OdbcRetCode := SQLGetInfoString(fhCon, SQL_COLLATION_SEQ, PropValue, MaxLength, Length);
      OdbcCheck(OdbcRetCode, 'SQLGetConnectAttr(SQL_COLLATION_SEQ)');
      end;
    eConnSqlDialect:
      // Interbase only
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnSqlDialect) not supported - applies to Interbase only');
    {+2.01 New Delphi 7 options}
    {$IFDEF _D7UP_}
    eConnRollbackRetain:
      begin
      Pointer(PropValue^) := nil;
      //:raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnRollbackRetain) not supported');
      end;
    eConnObjectQuoteChar:
      begin
      PAnsiChar(PropValue)^ := fQuoteChar;
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnObjectQuoteChar) not supported');
      end;
    eConnConnectionName:
      begin
      if (System.Length(fOdbcConnectString) > 0) and
         (System.Length(fOdbcConnectString) <= MaxLength) then
         begin
         Length := System.Length(fOdbcConnectString);
         move(PChar(fOdbcConnectString)[0], PChar(PropValue)[0], Length);
         end
       else
         begin
         raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnConnectionName) connection string is long');
         end;
       //raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnConnectionName) not supported');
       end;
    eConnOSAuthentication:
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnOSAuthentication) not supported');
    eConnSupportsTransaction:
      {+2.02}
      //raise EDbxNotSupported.Create('TSqlDriverOdbc.GetOption(eConnSupportsTransaction) not supported');
      begin
      OdbcRetCode := SQLGetInfoSmallint(fhCon, SQL_TXN_CAPABLE, SmallintAttrVal, SizeOf(SmallintAttrVal), nil);
      OdbcCheck(OdbcRetCode, 'SQLGetInfo(fhCon, SQL_TXN_CAPABLE)');
      Boolean(PropValue^) := SmallintAttrVal <> SQL_TC_NONE;
      end;
      {/+2.02}
    eConnMultipleTransaction:
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnMultipleTransaction) not supported');
    eConnServerPort:
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnServerPort) not supported');
    eConnOnLine:
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnOnLine) not supported');
    eConnTrimChar:
      Boolean(PropValue^) := coTrimChar in fConnectionOptions;
    eConnQualifiedName:
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnQualifiedName) not supported');
      begin
        // Vadim > ???Vad>All: for SQLLite:
        // Edward > I do not have SQLLite - I don't know either
        if (System.Length(fQualifiedName) > 0) and
         (MaxLength >= System.Length(fQualifiedName)) then
        begin
          Length := System.Length(fQualifiedName);
          move(PChar(fQualifiedName)[0], PChar(PropValue)[0], Length);
          end
        else
          begin
          raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnObjectName) Length og fQualifiedName string is incorrect');
          end;
      end;
    eConnCatalogName:
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnCatalogName) not supported');
      // {+2.03}
      // Do not cache catalog name, could be changed, eg. by Sql statement USE catalogname
      begin
      GetCurrentCatalog;
      if fCurrentCatalog = nil then
        Length := 0
      else
        Length := StrLen(fCurrentCatalog);
      if (Length = 0) then
        PChar(PropValue)[0] := #0
      else
        begin
        if (MaxLength >= Length) then
          move(PChar(fCurrentCatalog)[0], PChar(PropValue)[0], Length)
        else
          raise EDbxInvalidCall.Create(
           'TSqlConnectionOdbc.GetOption(eConnCatalogName) MaxLength too small. ' +
           'MaxLength=' + inttostr(MaxLength) +
           ', CurrentCatalog=' + fCurrentCatalog);
        end;
      // {/+2.03}
      end;
    eConnSchemaName:
    //raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnSchemaName) not supported');
      begin
      Length := System.Length(fCurrentSchema);
      if (Length > 0) and
         (MaxLength >= Length) then
        begin
        move(PChar(fCurrentSchema)[0], PChar(PropValue)[0], Length);
        end
      else
        begin
        if (MaxLength >= Length) and (coSupportsSchemaFilter in fConnectionOptions) then
          raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnSchemaName) CurrentSchema name is very long');
         end;
        end;
    eConnObjectName:
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnObjectName) not supported');
       begin
         if (System.Length(fQualifiedName) > 0) and
            (MaxLength >= System.Length(fQualifiedName)) then
          begin
            Length := System.Length(fQualifiedName);
            move(PChar(fQualifiedName)[0], PChar(PropValue)[0], Length);
          end
         else
          begin
            if (MaxLength >= System.Length(fQualifiedName)) then
               raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnObjectName) Length fQualifiedName string is incorrect');
          end;
       end;
    eConnQuotedObjectName:
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnQuotedObjectName) not supported');
      begin
      {$ifdef _RegExprParser_}
        // This is right for multi-part names
        GetQuotedQualifiedName;
      {$else}
        //Vadim> ???Vad>All: for SQLLite:
        //Edward> I don't know eithier
        //Edward> ???Ed>Vad/All: This does not look right for multi-part names
        //Edward> (eg master.dbo.tablename)
        //Edward> but I have kept Vadim's change for now
        Length := System.Length(fQualifiedName);
        if fWantQuotedTableName then
          Length := Length + 2;
        if (System.Length(fQualifiedName) > 0) and
           (MaxLength >= Length) then
        begin
          if fWantQuotedTableName and
             (fQualifiedName[1] <> fQuoteChar) then
             // doublequote chars: ObjectName = "schema"."table" (SQL Server)
            move(PChar(fQuoteChar + fQualifiedName + fQuoteChar)[0], PChar(PropValue)[0], Length)
          else
            move(PChar(fQualifiedName)[0], PChar(PropValue)[0], Length);
        end
        else
        begin
          raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnObjectName) fQualifiedName string is very long');
        end;
      {$endif}
      end;
    eConnCustomInfo:
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnCustomInfo) not supported');
    eConnTimeOut:
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.GetOption(eConnTimeOut) not supported');
    {$ENDIF}
{/+2.01 /New Delphi 7 options}
    else
      raise EDbxInvalidCall.Create('Invalid option passed to TSqlConnectionOdbc.GetOption: ' + IntToStr(Ord(eDOption)));
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


{+2.02 DBMS autodetection}
(*
// Edward> ???Ed>Vad: Vadim, I have commented the code for database type autodetection
// Edward> I can see you put a lot of work into it, but I think it was a bad idea.
// Edward> ???Ed>Vad: Better to use SQLGetInfoString(SQL_DBMS_NAME) instead
// Edward> Now I have your version of 2002-11-04, I will include your changes.

// Edward> ???Ed>Vad: Commented database auto-detection

//Vadim V.Lopushansky:  for database type autodetection
const  // database specific queries which return 0 records:
   c_sql_autodetection: array[TOdbcDriverType] of PChar = (
      // UNSPECIFIED
      nil
      // GUPTA
      , nil
      // MS SQL Server
      ,
      'select sf.groupid+sc.colid, null f02 from sysfiles sf,syscomments sc where 1=0 union select 1, USER_NAME(2)+SYSTEM_USER from sysobjects where id=1 and 1=0 union select null, @@TRANCOUNT+@@ROWCOUNT+@@IDENTITY+@@ERROR from sysobjects where id=1'
      // IBM DB2
      ,
      'SELECT NPAGES, CARD FROM SYSCAT.TABLES WHERE 1=0 AND TABNAME = TABLE_NAME (''X1'',''HEDGES'') AND TABSCHEMA = TABLE_SCHEMA (''X1'',''HEDGES'')'
      // ACCESS
      , 'SELECT DATA, ID FROM `MSysAccessObjects` WHERE 1=0'
      // My SQL
      ,'select u.Shutdown_priv,u.Reload_priv from user u, columns_priv p where 1=0 and Timestamp is null'
      // INFORMIX
      ,
      'select min(case when t.tabid = 0 then 1 else dbinfo(''sessionid'') end) from systables t, outer(syssynonyms s) where 1=0 and t.tabid = s.tabid'
      // SQL Lite
      , nil
      // Think SQL: The "Catalog" does not supported, as in Delphi for it allocate the small buffer (max 255 symbols)
      ,
      'SELECT T."AUTHORIZATION" || T."ISOLATION_LEVEL" AS FLD1 FROM INFORMATION_SCHEMA.ACTIVE_TRANSACTIONS T WHERE 1=0 AND T."TRANSACTION"=0 UNION SELECT T."COUNTER" || T."SEQUENCE_CATALOG" FROM INFORMATION_SCHEMA.SEQUENCES T WHERE 1=0'
      // ORACLE:
      ,
      'SELECT UID FROM DUAL D, ALL_TABLES T WHERE 1=0 AND T.TABLE_NAME (+) = SUBSTR(TO_CHAR(SYSDATE), 1, 1) UNION SELECT 1 AS FUID FROM DUAL WHERE 1=0'
      // INTERBASE
      //        ,'/*InterBase*/ SELECT COUNT(RDB$SECURITY_CLASS) from RDB$DATABASE WHERE 1=0'
      );
//*)

// Prepare and execute query
// Edward> ???Ed>Vad
function sql_prepared(Con: TSqlConnectionOdbc; SQL: PChar): Boolean;// check query is correct
var
   fhStmt: SQLHSTMT;
   OdbcRetCode: OdbcApi.SQLRETURN;
begin
   try
      OdbcRetCode := SQLAllocHandle(SQL_HANDLE_STMT, Con.fhCon, fhStmt);
      Con.OdbcCheck(OdbcRetCode, 'SQLAllocHandle(SQL_HANDLE_STMT)');
      // Prepare Query
      OdbcRetCode := SQLPrepare(fhStmt, SQL, SQL_NTS);
      Result      := OdbcRetCode = OdbcApi.SQL_SUCCESS;
      if Result then
       begin
         // It is necessary to test execution. MSAccess after prepare db2 query don't return error.
         OdbcRetCode := SQLExecute(fhStmt);
         Result      := OdbcRetCode = OdbcApi.SQL_SUCCESS;
       end;
   finally
      OdbcRetCode := SQLFreeStmt(fhStmt, SQL_UNBIND);
      Con.OdbcCheck(OdbcRetCode, 'SQLFreeStmt(SQL_UNBIND)');

      OdbcRetCode := SQLFreeHandle(SQL_HANDLE_STMT, fhStmt);
      Con.OdbcCheck(OdbcRetCode, 'SQLFreeHandle(SQL_HANDLE_STMT)');
    end;
end;
//*)
{/+2.02 /DBMS autodetection}

function TSqlConnectionOdbc.RetrieveDriverName: SQLResult;

var
  OdbcRetCode: OdbcApi.SQLRETURN;
  Buffer: array[0..100] of AnsiChar;
  uDbmsName: string;
  BufLen: SQLSMALLINT;
{+2.02 Get DBMS info}
  VersionMinor, VersionRelease: integer;

  procedure VersionStringToNumeric(const VersionString: string;
    var VersionMajor, VersionMinor, VersionRelease: integer);
  const
    cDigits = ['0'..'9'];
  var
    c: char;
    NextNumberFound: boolean;
    sVer: array[1..3] of string;
    VerIndex: integer;
  var
    i: integer;
  begin
  VerIndex := 0;
  NextNumberFound := false;

  for i := 1 to Length(VersionString) do
    begin
    c := VersionString[i];
    if c in cDigits then
      begin
      if not NextNumberFound then
        begin
        NextNumberFound := true;
        inc(VerIndex);
        if VerIndex > High(sVer) then break;
        end;
      sVer[VerIndex] := sVer[VerIndex] + c;
      end
    else
      NextNumberFound := false;
    end;

  if sVer[1] <> '' then
    VersionMajor := StrToIntDef(sVer[1], -1);
  if sVer[2] <> '' then
    VersionMinor := StrToIntDef(sVer[2], -1);
  if sVer[3] <> '' then
    VersionRelease := StrToIntDef(sVer[3], -1);
  end;
{/+2.02 /Get DBMS info}

begin
{+2.02 Get DBMS info}
  FillChar(Buffer[0], SizeOf(Buffer), #0);
  OdbcRetCode := SQLGetInfoString(fhCon, SQL_DBMS_NAME, Buffer, SizeOf(Buffer), BufLen);
  OdbcCheck(OdbcRetCode, 'SQLGetInfo(fhCon, SQL_DBMS_NAME)');
  fDbmsName := Buffer;
  uDbmsName := UpperCase(fDbmsName);
  // RDBMS NAME
  if      uDbmsName = 'SQLBASE'   then fDbmsType := eDbmsTypeGupta
  else if uDbmsName = 'MICROSOFT SQL SERVER' then fDbmsType := eDbmsTypeMsSqlServer
  else if uDbmsName = 'IBMDB2'    then fDbmsType := eDbmsTypeIbmDb2
  else if uDbmsName = 'MYSQL'     then fDbmsType := eDbmsTypeMySql
// JET databases
  else if uDbmsName = 'ACCESS'    then fDbmsType := eDbmsTypeMsAccess
  else if uDbmsName = 'EXCEL'     then fDbmsType := eDbmsTypeExcel
  else if uDbmsName = 'TEXT'      then fDbmsType := eDbmsTypeText
  else if uDbmsName = 'DBASE'     then fDbmsType := eDbmsTypeDBase
  else if uDbmsName = 'PARADOX'   then fDbmsType := eDbmsTypeParadox
// Ohter databases, not fully tested
  else if uDbmsName = 'ORACLE'    then fDbmsType := eDbmsTypeOracle
  else if uDbmsName = 'INFORMIX'  then fDbmsType := eDbmsTypeInformix
  else if uDbmsName = 'INTERBASE' then fDbmsType := eDbmsTypeInterbase
// Ohter databases, not tested at all
  else if uDbmsName = '???SYBASE'   then fDbmsType := eDbmsTypeSybase
  else if uDbmsName = '???SQLLITE'  then fDbmsType := eDbmsTypeSQLLite
  else if uDbmsName = '???THINKSQL' then fDbmsType := eDbmsTypeThinkSQL
  else if uDbmsName = '???SAPDB'    then fDbmsType := eDbmsTypeSAPDB

  else fDbmsType := eDbmsTypeUnspecified;

  // RDBMS VERSION
  FillChar(Buffer[0], SizeOf(Buffer), #0);
  OdbcRetCode := SQLGetInfoString(fhCon, SQL_DBMS_VER, Buffer, SizeOf(Buffer), BufLen);
  OdbcCheck(OdbcRetCode, 'SQLGetInfo(fhCon, SQL_DBMS_VER)');
  fDbmsVersionString := Buffer;

  VersionStringToNumeric(fDbmsVersionString, fDbmsVersionMajor, VersionMinor, VersionRelease);

  // ODBC DRIVER VERSION
  FillChar(Buffer[0], SizeOf(Buffer), #0);
  OdbcRetCode := SQLGetInfoString(fhCon, SQL_DRIVER_VER, Buffer, SizeOf(Buffer), BufLen);
  OdbcCheck(OdbcRetCode, 'SQLGetInfo(fhCon, SQL_DRIVER_VER)');
  fOdbcDriverVersionString := Buffer;

  VersionStringToNumeric(fOdbcDriverVersionString, fOdbcDriverVersionMajor, VersionMinor, VersionRelease);

// ODBC DRIVER NAME:
  FillChar(Buffer[0], SizeOf(Buffer), #0);
  OdbcRetCode := SQLGetInfoString(fhCon, SQL_DRIVER_NAME, Buffer, SizeOf(Buffer), BufLen);
  OdbcCheck(OdbcRetCode, 'SQLGetInfo(fhCon, SQL_DRIVER_NAME)');
  fOdbcDriverName := Buffer;

  strUpper(Buffer);

{
// Oroginal code
  if (StrLComp(Buffer, 'IV', 2) = 0) then
    fOdbcDriverType := eOdbcDriverTypeDataDirect
  else if (StrLComp(Buffer, 'NTL', 3) = 0) then
    fOdbcDriverType := eOdbcDriverTypeDataDirect
  else if StrLComp(buffer, 'C2GUP', 5) = 0 then
    fOdbcDriverType := eOdbcDriverTypeGupta
  else if StrLComp(buffer, 'SQLSRV', 6) = 0 then
    fOdbcDriverType := eOdbcDriverTypeMsSqlServer
  else if StrLComp(buffer, 'DB2CLI', 6) = 0 then
    fOdbcDriverType := eOdbcDriverTypeIbmDb2
  else if StrLComp(buffer, 'ODBCJT', 6) = 0 then
    fOdbcDriverType := eOdbcDriverTypeMsJet
  else if StrLComp(buffer, 'MYODBC', 6) = 0 then
    begin
    fOdbcDriverType := eOdbcDriverTypeMySql;
    fOdbcDriverLevel := 2; // MySql is Level 2
    end
  else if StrLComp(buffer, 'IVINF', 5) = 0 then
    fOdbcDriverType := eOdbcDriverTypeInformix
  else if StrLComp(buffer, 'IB', 2) = 0 then
    fOdbcDriverType := eOdbcDriverTypeInterbase
  else
    fOdbcDriverType := eOdbcDriverTypeUnspecified;
}


  // SQL Base:
  if (StrLComp(Buffer, 'C2GUP', 5) = 0) or
     (StrLComp(Buffer, 'IVGUP', 5) = 0)  // DataDirect SQLBase ODBC Driver
  then fOdbcDriverType := eOdbcDriverTypeGupta

  // SQL Server:
  else if (StrLComp(Buffer, 'SQLSRV', 6) = 0) // SQL Server Microsoft Corporation ODBC Driver
       or (StrLComp(Buffer, 'IVSS', 4) = 0)   // DataDirect SQL Server ODBC Driver
       or (StrLComp(Buffer, 'IVMSSS', 6) = 0) // DataDirect SQL Server Wire Protocol ODBC Driver
       or// extended comparing
       ((StrLComp(Buffer, 'NTL', 3) = 0) and (Buffer[5] = 'M')) // OpenLink Lite for MS-SQL Server (32 Bit) ODBC Driver
  then fOdbcDriverType := eOdbcDriverTypeMsSqlServer

  // IBM DB2:
  else if (StrLComp(Buffer, 'DB2CLI', 6) = 0) // IBM DB2 ODBC DRIVER
       or (StrLComp(Buffer, 'IVDB2', 5) = 0)  // DataDirect DB2 Wire Protocol ODBC Driver
  then fOdbcDriverType := eOdbcDriverTypeIbmDb2

   // Microsoft desktop databases:
  else if StrLComp(Buffer, 'ODBCJT', 6) = 0
  //(Microsoft Paradox Driver, Microsoft dBASE Driver, ...).
  then fOdbcDriverType := eOdbcDriverTypeMsJet
    // This driver does not allow SQL_DECIMAL.
    // It driverType usagheb for detecting this situation.
    // Edward> ???Ed>Vad: What does it mean?

  // My SQL ODBC Version 3 Driver:
  else if StrLComp(Buffer, 'MYODBC3', 7) = 0
  then fOdbcDriverType := eOdbcDriverTypeMySql3

  // My SQL:
  else if StrLComp(Buffer, 'MYODBC', 6) = 0
  then fOdbcDriverType := eOdbcDriverTypeMySql

  // INFORMIX:
  else if (StrLComp(Buffer, 'ICLI', 4) = 0)   // "INFORMIX 3.32 32 BIT" ODBC Driver
       or (StrLComp(Buffer, 'IVINF', 5) = 0)  // DataDirect Informix ODBC Driver
       or (StrLComp(Buffer, 'IVIFCL', 6) = 0) // DataDirect Informix Wire Protocol ODBC Driver
       or (StrLComp(Buffer, 'PDINF', 5) = 0)  // INTERSOLV Inc ODBC Driver (1997. Now is DataDirect)
       or// extended comparing
       ((StrLComp(Buffer, 'NTL', 3) = 0) and (Buffer[5] = 'I')) // OpenLink Lite for Informix 7 (32 Bit) ODBC Driver
  then fOdbcDriverType := eOdbcDriverTypeInformix

  // SQL Lite:
  else if StrLComp(Buffer, 'SQLITEODBC', 10) = 0
  then fOdbcDriverType := eOdbcDriverTypeSQLLite

  // INTERBASE:
  else if StrLComp(Buffer, 'IB6ODBC', 7) = 0 // Easysoft ODBC Driver
  then fOdbcDriverLevel := 2

  // Think SQL:
  else if StrLComp(Buffer, 'THINKSQL', 8) = 0 // ThinkSQL ODBC Driver
  then fOdbcDriverType := eOdbcDriverTypeThinkSQL

  // ORACLE:
  else if (StrLComp(Buffer, 'SQORA', 5) = 0)  // Oracle ODBC Driver
       or (StrLComp(Buffer, 'MSORCL', 6) = 0) // Microsoft ODBC for Oracle
       or (StrLComp(Buffer, 'IVOR', 4) = 0)   // DataDirect Oracle ODBC Driver
       or (StrLComp(Buffer, 'IVORA', 5) = 0)  // DataDirect Oracle Wire Protocol ODBC Driver
  then fOdbcDriverType := eOdbcDriverTypeOracle

  else if (StrLComp(Buffer, 'INOLE', 5) = 0)  // MERANT ODBC-OLE DB Adapter Driver
  then fOdbcDriverType := eOdbcDriverTypeMerantOle

  // SYBASE:
  {
     ( StrLComp(buffer, 'IVASE', 5) = 0 )  // DataDirect SybaseWire Protocol ODBC Driver
  }
  // BTRIEVE:
  {
  ( StrLComp(buffer, 'IVBTR', 5) = 0 )  // DataDirect Btrieve (*.dta) ODBC Driver
  }
  // PROGRESS:
  {
  ( StrLComp(buffer, 'IVPRO', 5) = 0 )  // DataDirect Progress ODBC Driver
  }
  // OTHER:
  {
  Microsoft Visual FoxPro Driver (*.dbf)    'VFPODBC'
  DataDirect dBASE File (*.dbf)             'IVDBF'
  DataDirect FoxPro 3.0 database (*.dbc)    'IVDBF'
  DataDirect Excel Workbook (*.xls)         'IVXLWB'
  DataDirect Paradox File (*.db)            'IVDP'
  DataDirect Text File (*.*)                'IVTXT'
  DatDirect XML                             'IVXML'
  SQLLite                                   'SQLITEODBC'
  ThinkSQL                                  'THINKSQL'
  }
  else fOdbcDriverType := eOdbcDriverTypeUnspecified;

  // Initialize Server specific parameters
  case fOdbcDriverType of
  eOdbcDriverTypeMsSqlServer:
    begin
    // DataDirect SQL Server ODBC Driver (Contains an error of installation of the unknown catalog)
    if (StrLComp(Buffer, 'IVSS', 4) = 0) then
      System.Include(fConnectionOptions, coNoSupportsCatalog);
    end;
  eOdbcDriverTypeMsJet:
    begin
    // Edward> ???Ed>Vad: I have commented your line below? Jet can support "catalog" (ie directory)
//    System.Include(fConnectionOptions, coNoSupportsCatalog);
    end;
  eOdbcDriverTypeMySql3: ; // New MySql Driver - Odbc Version 3!
  eOdbcDriverTypeMySql:
    begin
    fOdbcDriverLevel := 2; // MySql is Level 2
    System.Include(fConnectionOptions, coNoSupportsCatalog);
    end;
  eOdbcDriverTypeInformix:
    begin
    fWantQuotedTableName := False;
    //if ( StrLComp(buffer, 'PDINF', 5) = 0 )  // INTERSOLV Inc ODBC Driver (1997. Now is DataDirect)
    System.Include(fConnectionOptions, coNoSupportsCatalog); // INFORMIX supports operation with
    // the catalog, but usage of this option is inconvenient for the developers and there is no
    // large sense  by work with INFORMIX. If you want to work with the catalog, comment out this line.
    System.Include(fConnectionOptions, coIgnoreUnknownFieldType);
    end;
  eOdbcDriverTypeSQLLite:
    begin
    fOdbcDriverLevel := 2; // SqlLite is Level 2
    System.Include(fConnectionOptions, coNoSupportsCatalog);
    end;
  eOdbcDriverTypeThinkSQL:
    begin
    System.Include(fConnectionOptions, coNoSupportsCatalog);
    end;
  eOdbcDriverTypeOracle:
    begin
    // If the user has not defined value of an option "SchemaFilter" then we shall define a desired value:
    if not
      ((coSupportsSchemaFilter in fConnectionOptions) or
      (coNoSupportsSchemaFilter in fConnectionOptions)) then
      System.Include(fConnectionOptions, coSupportsSchemaFilter);
    end;
    end;
{/+2.03 /Get DBMS and DRIVER info}
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

{+2.01}
  //Vadim V.Lopushansky:
  {$IFDEF _D7UP_}
  function GetOptionValue(const OptionsString, OptionName: string; OneChar: Boolean = True): String;
  var
    pLeft, pRight: Integer;
    R: Boolean;
    begin
    pLeft := Pos(OptionName + '=', UpperCase(OptionsString));
    R     := pLeft > 0;
    if (R) then
      begin
      pLeft  := pLeft + System.Length(OptionName) + 1;//skip OptionsName=
      pRight := pLeft;
      while (pRight <= System.Length(OptionsString)) and (OptionsString[pRight] <> ';') do Inc(pRight);
      if OneChar then Result := (Trim(Copy(OptionsString, pLeft, pRight - pLeft)) + ' ')[1]
      else Result := Trim(Copy(OptionsString, pLeft, pRight - pLeft));
      end
    else
      begin
      if OneChar then Result := ' '
      else Result := '';
    end;
  end;
  {$ENDIF}
{/+2.01}
{+2.03}
  function isConnectionString(const ConnectionString, CatalogName:String):Boolean;
    begin
    Result := (CompareText(ConnectionString, CatalogName) = 0)
            or(CompareText(ConnectionString, 'DSN='+CatalogName) = 0);
    end;
{/+2.03}

var
  OdbcRetCode: OdbcApi.SQLRETURN;
  AttrValue: SQLUINTEGER;
  {+2.02}
  {$IFDEF _D7UP_}
  //vCurrentCatalog: pchar;
  //aCurrentCatalogLen: SQLINTEGER;
  {$ENDIF}
  {/+2.02}
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
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnRoleName) not supported - Applies to Interbase only');
      end;
    eConnWaitOnLocks:
      begin
      // Boolean that indicates whether application should wait until a locked
      // resource is free rather than raise an exception. (Interbase only)
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnWaitOnLocks) not supported - Applies to Interbase only');
      end;
    eConnCommitRetain:
      begin
      // Cursors dropped after commit
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnCommitRetain) not supported - Applies to Interbase only');
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
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnObjectMode) not supported - applies to Oracle only');
    {+2.01}
    {$IFDEF _D7UP_}
    eConnMaxActiveComm:
    {$ELSE}
    eConnMaxActiveConnection:
    {$ENDIF}
    {/+2.01}
      raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnMaxActiveConnection) not valid (Read-only)');
    eConnServerCharSet:
      raise EDbxInvalidCall.Create('TSqlConnectionOdbc.SetOption(eConnServerCharSet) not valid (Read-only)');
    eConnSqlDialect:
      // Interbase only
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnSqlDialect) not supported - applies to Interbase only');
    {+2.01 New options for Delphi 7}
    {$IFDEF _D7UP_}
    eConnRollbackRetain:
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnRollbackRetain) not supported');
    eConnObjectQuoteChar:
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnObjectQuoteChar) not valid (Read-only)');
    eConnConnectionName:
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnConnectionName) not valid (Read-only');
    eConnOSAuthentication:
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnOSAuthentication) not supported');
    eConnSupportsTransaction:
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnSupportsTransaction) not supported');
    eConnMultipleTransaction:
      raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnMultipleTransaction) not supported');
    eConnServerPort:
       raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnServerPort) not supported');
    eConnOnLine:;
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnOnLine) not supported');
    eConnTrimChar:
      begin
      if Boolean(lValue) then
        System.Include(fConnectionOptions, coTrimChar)
       else
         System.Exclude(fConnectionOptions, coTrimChar);
      end;
    eConnQualifiedName:
      //Vadim> ???Vad>All:
      //Edward> I don't know either. But I have kept your change.
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnQualifiedName) not supported');
      begin
        fQualifiedName := StrPas(PChar(lValue));
      // error in D7:
{
//Edward> ???Ed>Ed: - I do not understand what Vadim is saying here

In Delphi7: Not full analysing when FSchemaName = ''. File "SqlExpr.pas":
function TCustomSQLDataSet.GetQueryFromType: string;
   ...
           begin
             //+ new line:
             if Self.FSchemaName <> '' then
             //+. ^^^^^^^^^^^^^^^^^^^^^^^^^
             STableName := AddQuoteCharToObjectName(Self, FSchemaName + '.' + FNativeCommand,
                      FSQLConnection.QuoteChar)
             //+ new line:
             else
             STableName := AddQuoteCharToObjectName(Self, FNativeCommand, FSQLConnection.QuoteChar)
             //+. ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
             ;
             Result := SSelectStarFrom + STableName;
           end;
       end;
     ctStoredProc:
}     // The padding code for the analysis of this situation:
      if (Length(fQualifiedName) > 0) and (fQualifiedName[1] = '.') then
        fQualifiedName := Copy(fQualifiedName, 2, Length(fQualifiedName) - 1);
      end;
    eConnCatalogName:
      begin
      {+2.03}
      // Vadim> Error if NewCatalog=Currentcatalog (informix, mssql)
      // Edward> ???Ed>Vad I still don't think code below is correct
      if fSupportsCatalog and
         // catalog name <> connection string
         (not isConnectionString( fOdbcConnectString, StrPas(PAnsiChar(lValue)))) then
        begin
        GetCurrentCatalog;
        // catalog name <> current catalog
        if fSupportsCatalog and
           (StrCompNil(PAnsiChar(fCurrentCatalog), PAnsiChar(lValue) ) <> 0) then
          begin
          OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_CURRENT_CATALOG, PAnsiChar(lValue), SQL_NTS);
          OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(SQL_ATTR_CURRENT_CATALOG)');
          end;
        end;
      {/+2.03}
(*
      //Edward> Ed>Vad:  - I don't know what Vadim is doing here, and I don't think it is right,
      //Vadim> Vad>Ed: - Replaced by code above
      if fSupportsCatalog
      and
      // catalog don't equal "DSN Name" contained in SqlExp.pas:GetInternalConnection.FParams.Values[DATABASENAME_KEY]
       ((StrPos(strUpper(PChar(lValue)), 'DSN=') = nil)
      and
       (StrPos(strUpper(PChar(lValue)), 'DRIVER=') = nil)
       {$IFDEF WINDOWS}
      and (StrPos(PChar(lValue), ':\') <> nil) // part path to local file
       {$ENDIF}
       )
      then
        begin
        if fCurrentCatalog = nil then
          begin
          GetCurrentCatalog;
          if not fSupportsCatalog then
            begin
            Result := SQL_SUCCESS;
            exit;
            end;
          if strCompNil(strUpper(fCurrentCatalog), strUpper(PAnsiChar(lValue))) = 0 then
            begin
            Result := SQL_SUCCESS;
            exit;
            end;
          end
        else
          begin
          vCurrentCatalog := AllocMem(fOdbcMaxCatalogNameLen + 1);
          OdbcRetCode     := SQLGetConnectAttr(fhCon,
           SQL_ATTR_CURRENT_CATALOG,
           vCurrentCatalog,
           fOdbcMaxCatalogNameLen, @aCurrentCatalogLen);
           if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
             begin
             FreeMem(vCurrentCatalog);
             Result := SQL_SUCCESS;
             exit;
             end;
           if strCompNil(strUpper(vCurrentCatalog), strUpper(PAnsiChar(lValue))) = 0 then
             begin
             FreeMem(vCurrentCatalog);
             Result := SQL_SUCCESS;
             exit;
             end;
           FreeMem(vCurrentCatalog);
           end;
        // set new catalog
        OdbcRetCode := SQLSetConnectAttr(fhCon,
         SQL_ATTR_CURRENT_CATALOG, PAnsiChar(lValue), SQL_NTS);
        if (OdbcRetCode = DBXpress.SQL_SUCCESS) then
          begin
          // Definition of a correctness of installation of the catalog using the server - dependent queries
          case fOdbcDriverType of
          eOdbcDriverTypeInformix:
            begin
            if not sql_prepared(Self,
            PAnsiChar('select * from ' + PAnsiChar(lValue) + ':systables where 1=0')
             ) then
               OdbcRetCode := DBXpress.SQL_ERROR;
            end;
          end;
          if (OdbcRetCode <> DBXpress.SQL_SUCCESS) then
          // restore catalog
            OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_CURRENT_CATALOG, fCurrentCatalog, SQL_NTS);
          end
        else
          if Assigned(fCurrentCatalog) then
            // restore catalog
            OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_CURRENT_CATALOG, fCurrentCatalog, SQL_NTS);
          OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(SQL_ATTR_CURRENT_CATALOG)');
        end;
*)
      end;
    eConnSchemaName:;
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnSchemaName) not supported');
    eConnObjectName:;
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnObjectName) not supported');
    eConnQuotedObjectName:;
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnQuotedObjectName) not supported');
    eConnCustomInfo:
      //raise EDbxNotSupported.Create('TSqlConnectionOdbc.SetOption(eConnCustomInfo) not supported');
      begin
      //Metadata
      case GetOptionValue(PChar(lValue), 'METADATA')[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coSupportsMetadata);
        System.Exclude(fConnectionOptions, coNoSupportsMetadata);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoSupportsMetadata);
        System.Exclude(fConnectionOptions, coSupportsMetadata);
        end;
        end;
      //MapIn64ToBcd
      case GetOptionValue(PChar(lValue), 'MAPINT64TOBCD')[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coMapInt64ToBcd);
        System.Exclude(fConnectionOptions, coNoMapInt64ToBcd);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoMapInt64ToBcd);
        System.Exclude(fConnectionOptions, coMapInt64ToBcd);
        end;
        end;
      //MapSmallBcdToNative
      case GetOptionValue(PChar(lValue), 'MAPSMALLBCDTONATIVE')[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coMapSmallBcdToNative);
        System.Exclude(fConnectionOptions, coNoMapSmallBcdToNative);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoMapSmallBcdToNative);
        System.Exclude(fConnectionOptions, coMapSmallBcdToNative);
        end;
        end;
      //MapCharAsBDE
      case GetOptionValue(PChar(lValue), 'MAPCHARASBDE')[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coMapCharAsBDE);
        System.Exclude(fConnectionOptions, coNoMapCharAsBDE);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoMapCharAsBDE);
        System.Exclude(fConnectionOptions, coMapCharAsBDE);
        end;
        end;
      //TrimChar
      case GetOptionValue(PChar(lValue), 'TRIMCHAR')[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coTrimChar);
        System.Exclude(fConnectionOptions, coNoTrimChar);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoTrimChar);
        System.Exclude(fConnectionOptions, coTrimChar);
        end;
        end;
      //SchemaFilter
      case GetOptionValue(PChar(lValue), 'SCHEMAFILTER')[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coSupportsSchemaFilter);
        System.Exclude(fConnectionOptions, coNoSupportsSchemaFilter);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoSupportsSchemaFilter);
        System.Exclude(fConnectionOptions, coSupportsSchemaFilter);
        end;
        end;
      {+2.02 New option IgnoreUnknownFieldType}
      //IgnoreUnknownFieldType
      case GetOptionValue(PChar(lValue), 'IGNOREUNKNOWNTYPE')[1] of
      '1':
        begin
        System.Include(fConnectionOptions, coIgnoreUnknownFieldType);
        System.Exclude(fConnectionOptions, coNoIgnoreUnknownFieldType);
        end;
      '0':
        begin
        System.Include(fConnectionOptions, coNoIgnoreUnknownFieldType);
        System.Exclude(fConnectionOptions, coIgnoreUnknownFieldType);
        end;
        end;
      {/+2.02 /New option IgnoreUnknownFieldType}
      //BlobChunkSize:
      fBlobChunkSize := StrToIntDef(GetOptionValue(PChar(lValue), 'BLOBCHUNKSIZE', False), cBlobChunkSizeDefault);
      if fBlobChunkSize < 256 then
        fBlobChunkSize := 256
      else if fBlobChunkSize > cBlobChunkSizeLimit then
        fBlobChunkSize := cBlobChunkSizeLimit;

      //ConPacketSize:
      AttrValue := StrToIntDef(GetOptionValue(PChar(lValue), 'CONPACKETSIZE', False), 0);
      if (AttrValue >= 4096) then
        SQLSetConnectAttr(fhCon, SQL_ATTR_PACKET_SIZE, Pointer(AttrValue), 0);
      end;
    eConnTimeOut:
    //Vadim> ???Vad>All: not tested
      begin
      if lValue <= 0 then
        AttrValue := SQL_LOGIN_TIMEOUT_DEFAULT
      else
        AttrValue := lValue;
      OdbcRetCode := SQLSetConnectAttr(fhCon, SQL_ATTR_LOGIN_TIMEOUT, Pointer(AttrValue), 0);
      OdbcCheck(OdbcRetCode, 'SQLSetConnectAttr(SQL_ATTR_LOGIN_TIMEOUT)');
      end;
    {$ENDIF}
    {/+2.01 /New options for Delphi 7}
    else
      raise EDbxInvalidCall.Create('Invalid option passed to TSqlConnectionOdbc.SetOption: ' + IntToStr(Ord(eConnectOption)));
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

function TSqlConnectionOdbc.GetDbmsType: TDbmsType;
begin
  Result := fDbmsType;
end;

function TSqlConnectionOdbc.GetOdbcDriverType: TOdbcDriverType;
begin
  Result := fOdbcDriverType;
end;

procedure TSqlConnectionOdbc.GetOdbcConnectStrings(ConnectStringList: TStrings);
var
  i: integer;
  s: string;
begin
  if ConnectStringList = nil then
    ConnectStringList := TStringList.Create;
  s := '';
  for i := 1 to Length(fOdbcConnectString) do
    begin
    s := s + fOdbcConnectString[i];
    if fOdbcConnectString[i] = ';' then
      begin
      ConnectStringList.Add(s);
      s := '';
      end;
    end;
  if s <> '' then
      ConnectStringList.Add(s);
end;

function TSqlConnectionOdbc.GetLastOdbcSqlState: pchar;
begin
  Result := @fSqlStateChars;
end;

function TSqlConnectionOdbc.GetDbmsName: string;
begin
  Result := fDbmsName;
end;

function TSqlConnectionOdbc.GetDbmsVersionMajor: integer;
begin
  Result := fDbmsVersionMajor;
end;

function TSqlConnectionOdbc.GetDbmsVersionString: string;
begin
  Result := fDbmsVersionString;
end;

function TSqlConnectionOdbc.GetOdbcDriverName: string;
begin
  Result := fOdbcDriverName;
end;

function TSqlConnectionOdbc.GetOdbcDriverVersionMajor: integer;
begin
  Result := fOdbcDriverVersionMajor;
end;

function TSqlConnectionOdbc.GetOdbcDriverVersionString: string;
begin
  Result := fOdbcDriverVersionString;
end;

{ TSqlCommandOdbc }

constructor TSqlCommandOdbc.Create(
  OwnerDbxConnection: TSqlConnectionOdbc);
begin
  inherited Create;
  fCommandErrorLines := TStringList.Create;
  fOwnerDbxConnection := OwnerDbxConnection;
  fOwnerDbxDriver := fOwnerDbxConnection.fOwnerDbxDriver;
  fCommandBlobSizeLimitK := fOwnerDbxConnection.fConnBlobSizeLimitK;
  fOwnerDbxConnection.AllocHStmt(fhStmt);
  {+2.01}
  {$IFDEF _D7UP_}
  //Vadim V.Lopushansky: Support Trim of Fixed Char when connection parameter "Trim Char" is True
  fTrimChar := coTrimChar in fOwnerDbxConnection.fConnectionOptions;
  {$ENDIF}
  {/+2.01}
end;

destructor TSqlCommandOdbc.Destroy;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  i: integer;
begin
   {+2.01}
   if assigned(fhStmt) then
     begin
   {/+2.01}

     OdbcRetCode := SQLFreeStmt (fhStmt, SQL_CLOSE);
     OdbcCheck(OdbcRetCode, 'SQLFreeStmt(SQL_CLOSE)');

     OdbcRetCode := SQLFreeStmt (fhStmt, SQL_UNBIND);
     OdbcCheck(OdbcRetCode, 'SQLFreeStmt(SQL_UNBIND)');

     OdbcRetCode := SQLFreeStmt (fhStmt, SQL_RESET_PARAMS);
     OdbcCheck(OdbcRetCode, 'SQLFreeStmt(SQL_RESET_PARAMS)');

     fOwnerDbxConnection.FreeHStmt(fhStmt);
   {+2.01}
     end;
   {+2.01}

  if (fOdbcParamList <> nil) then
    begin
    for i := fOdbcParamList.Count - 1 downto 0 do
      TOdbcBindParam(fOdbcParamList[i]).Free;
    fOdbcParamList.Free;
    end;

  fCommandErrorLines.Free;
  inherited;
end;

procedure TSqlCommandOdbc.OdbcCheck(OdbcCode: SQLRETURN; const OdbcFunctionName: string);
begin
  fOwnerDbxDriver.OdbcCheck(OdbcCode, OdbcFunctionName, SQL_HANDLE_STMT, fhStmt, fOwnerDbxConnection);
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
  fExecutedOk := false;
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

  fExecutedOk := true;
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
  fExecutedOk := false;
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

  fExecutedOk := true;
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
  {+2.01}
  {$IFDEF _D7UP_}
  PropValue: Pointer;
  {$ELSE}
  var pValue: Integer;
  {$ENDIF}
  {/+2.01}
  MaxLength: SmallInt; out Length: SmallInt): SQLResult;
var
  OdbcRetCode: OdbcApi.SQLRETURN;
  ValueLength: SQLSMALLINT;
begin
try
  case eSqlCommandOption of
    eCommRowsetSize:
      begin
// New for Delphi 6.02
      {$IFDEF _D7UP_}
      integer(PropValue^) := fCommandRowSetSize;
      {$ELSE}
      pValue := fCommandRowSetSize;
      {$ENDIF}
//      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommRowsetSize) not yet implemented');
      end;
    eCommBlobSize:
      {+2.01}
      {$IFDEF _D7UP_}
      integer(PropValue^) := fCommandBlobSizeLimitK;
      {$ELSE}
      pValue := fCommandBlobSizeLimitK;
      {$ENDIF}
      {/+2.01}
    eCommBlockRead:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommBlockRead) not yet implemented');
    eCommBlockWrite:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommBlockWrite) not yet implemented');
    eCommParamCount:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommParamCount) not yet implemented');
    eCommNativeHandle:
      begin
      {+2.01}
      {$IFDEF _D7UP_}
      Integer(PropValue^) := Integer(fhStmt);
      {$ELSE}
      pValue := Integer(fhStmt);
      {$ENDIF}
      {/+2.01}
      Length := SizeOf(Integer);
      end;
    eCommCursorName:
      begin
      {+2.01}
      {$IFDEF _D7UP_}
      OdbcRetCode := SQLGetCursorName(fhStmt, PropValue, MaxLength, ValueLength);
      {$ELSE}
      OdbcRetCode := SQLGetCursorName(fhStmt, Pointer(pValue), MaxLength, ValueLength);
      {$ENDIF}
      {/+2.01}
      OdbcCheck(OdbcRetCode, 'SQLGetCursorName in TSqlCommandOdbc.GetOption');
      Length := ValueLength;
      end;
    eCommStoredProc:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommStoredProc) not yet implemented');
    eCommSQLDialect: // INTERBASE ONLY
      raise EDbxInvalidCall.Create('TSqlCommandOdbc.GetOption(eCommSQLDialect) valid only for Interbase');
    eCommTransactionID:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommTransactionID) not yet implemented');
    {+2.01}
    {$IFDEF _D7UP_}
    eCommPackageName:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommPackageName) not yet implemented');
    eCommTrimChar:
      Boolean(PropValue^) := fTrimChar;
    eCommQualifiedName:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommQualifiedName) not yet implemented');
    eCommCatalogName:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommCatalogName) not yet implemented');
    eCommSchemaName:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommSchemaName) not yet implemented');
    eCommObjectName:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommObjectName) not yet implemented');
    eCommQuotedObjectName:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption(eCommQuotedObjectName) not yet implemented');
    {$ENDIF}
    {/+2.01}
  else
    raise EDbxNotSupported.Create('TSqlCommandOdbc.GetOption - Invalid option ' + IntToStr(Ord(eSqlCommandOption)));
  end;
  Result := DBXpress.SQL_SUCCESS;
except
  on EDbxNotSupported do
    begin
    Length := 0;
    {+2.01}
    {$IFNDEF _D7UP_}
    pValue := 0;
    {$ENDIF}
    {/+2.01}
    Result := DBXERR_NOTSUPPORTED;
    end;
  on EDbxInvalidCall do
    begin
    Length := 0;
    {+2.01}
    {$IFNDEF _D7UP_}
    pValue := 0;
    {$ENDIF}
    {/+2.01}
    Result := DBXERR_INVALIDPARAM;
    end;
  on E: EDbxError do
    begin
    Length := 0;
    {+2.01}
    {$IFNDEF _D7UP_}
    pValue := 0;
    {$ENDIF}
    {/+2.01}
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

// This is another workaround because SqlExpress calls getRowsAffected after Error!
  if not fExecutedOk then
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
  fExecutedOk := false;
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
// Delphi 6.02 workaround - RowSetSize now set for all drivers
      fCommandRowSetSize := ulValue;
//      raise EDbxNotSupported.Create('TSqlCommandOdbc.SetOption(eCommRowsetSize) not yet implemented');
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
    {+2.01}
    {$IFDEF _D7UP_}
    eCommPackageName:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.SetOption(eCommPackageName) not yet implemented');
    eCommTrimChar:
      fTrimChar := Boolean(ulValue);
    eCommQualifiedName:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.SetOption(eCommQualifiedName) not yet implemented');
    eCommCatalogName:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.SetOption(eCommCatalogName) not yet implemented');
    eCommSchemaName:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.SetOption(eCommSchemaName) not yet implemented');
    eCommObjectName:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.SetOption(eCommObjectName) not yet implemented');
    eCommQuotedObjectName:
      raise EDbxNotSupported.Create('TSqlCommandOdbc.SetOption(eCommQuotedObjectName) not yet implemented');
    {$ENDIF}
    {/+2.01}
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
    raise EDbxNotSupported.Create(
    'TSqlCommandOdbc.setParameter - Non-input parameters not yet supoorted');

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
      aDays := integer(pBuffer^) - DateDelta;
      // DateDelta: Days between 1/1/0001 and 12/31/1899 = 693594,
      // ie (1899 * 365) (normal days) + 460 (leap days) - 1
      //(-1: correction for being last day of 1899)
      // leap days between 0001 and 1899 = 460, ie 1896/4 - 14
      // (-14: because 14 years weren't leap years:
      // 100,200,300, 500,600,700, 900,1000,1100, 1300,1400,1500, 1700,1800)
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
  fldDATETIME,
  fldTIMESTAMP: // fldTIMESTAMP added by Michael Schwarzl, to support MS SqlServer 2000
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
    if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeMsJet) then
      fOdbcParamSqlType := SQL_NUMERIC;  // MS ACCESS driver does not allow SQL_DECIMAL
    fOdbcParamCbColDef := iPrecision;
    fOdbcParamIbScale := iScale;
    if (bIsNull = 0) then
      begin
      fOdbcParamLenOrInd := SQL_NTS;
      s := BcdToStr(TBcd(pBuffer^));
      {+2.01}// Vadim V.Lopushansky: changing to updating BCD values when DecimalSeparator <> '.'
      // Edward> Yes, I think ODBC parameters ALWAYS use '.' as decimal separator
      if (iScale > 0) and (DecimalSeparator <> '.') then
        s := StringReplace(s, DecimalSeparator, '.', [rfIgnoreCase]);
      {/+2.01}
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
      raise EDbxNotSupported.Create(
      'TSqlCommandOdbc.setParameter - This data sub-type not yet supoorted');
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
  {+2.01}//Vadim V.Lopushansky:
  if coSupportsMetadata in fOwnerDbxConnection.fConnectionOptions then
  {/+2.01}
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
  {+2.01}//Vadim V.Lopushansky:
  if coSupportsMetadata in fOwnerDbxConnection.fConnectionOptions then
  {/+2.01}
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
  raise EDbxNotSupported.Create(
  'TSQLMetaDataOdbc.getObjectList - not yet supported');
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
  {+2.01}//Vadim V.Lopushansky:
  if coSupportsMetadata in fOwnerDbxConnection.fConnectionOptions then
  {/+2.01}
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
  {+2.01}//Vadim V.Lopushansky:
  if coSupportsMetadata in fOwnerDbxConnection.fConnectionOptions then
  {/+2.01}
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
//var
  //OdbcRetCode: OdbcApi.SQLRETURN;
begin
try
  case eDOption of
    eMetaCatalogName:
//Edward> Code below commented
//Edward> MS SqlServer returns error, "Invalid cursor state"
//Edward> I don't know why DbExpress needs to call this anyway
      begin
{
      if fOwnerDbxConnection.fDbxCatalog <> PAnsiChar(PropValue) then
        begin
        fOwnerDbxConnection.fDbxCatalog := PAnsiChar(PropValue);
        if fOwnerDbxConnection.fSupportsCatalog then
          begin
          OdbcRetCode := SQLSetConnectAttr(fOwnerDbxConnection.fhCon,
           SQL_ATTR_CURRENT_CATALOG, PAnsiChar(PropValue), SQL_NTS);
          fOwnerDbxConnection.OdbcCheck (OdbcRetCode, 'SQLSetConnectAttr(SQL_ATTR_CURRENT_CATALOG)');
          end;
        end;
//}
{/-2.03}
{+2.01}
(*
// Edward> ???Ed>Vad/All: I have commented out Vadim's change for now
// Edward> It looks over-complicated to me (I like to keep things simple),
// Edward> and I think it needs to be carefully checked.
               //Vadim V.Lopushansky: in delphi use bad method for identification the default catalog name
               // Vadim> ???Vad>All
               // OLD CODE:
               //fOwnerDbxConnection.OdbcCheck (OdbcRetCode, 'SQLSetConnectAttr(SQL_ATTR_CURRENT_CATALOG)');
               //NEW CODE:
               if fOwnerDbxConnection.fSupportsCatalog
                  and
                  // catalog don't equal "DSN Name" contained in SqlExp.pas:GetInternalConnection.FParams.Values[DATABASENAME_KEY]
                  ((StrPos(strUpper(PAnsiChar(PropValue)), 'DSN=') = nil)
                  and
                  (StrPos(strUpper(PAnsiChar(PropValue)), 'DRIVER=') = nil)
                  {$IFDEF WINDOWS} and
                  (StrPos(PAnsiChar(PropValue), ':\') <> nil)// // part path to local file
                  {$ENDIF}
                  ) then
                begin
                  if fOwnerDbxConnection.fCurrentCatalog = nil then
                   begin
                     fOwnerDbxConnection.GetCurrentCatalog;
                     if not fOwnerDbxConnection.fSupportsCatalog then
                      begin
                        Result := SQL_SUCCESS;
                        exit;
                      end;
                     if strCompNil(strUpper(fOwnerDbxConnection.fCurrentCatalog), strUpper(PAnsiChar(PropValue))) = 0 then
                      begin
                        Result := SQL_SUCCESS;
                        exit;
                      end;
                   end
                  else
                   begin
                     vCurrentCatalog := AllocMem(fOwnerDbxConnection.fOdbcMaxCatalogNameLen + 1);
                     OdbcRetCode     := SQLGetConnectAttr(fOwnerDbxConnection.fhCon,
                        SQL_ATTR_CURRENT_CATALOG,
                        vCurrentCatalog,
                        fOwnerDbxConnection.fOdbcMaxCatalogNameLen, @aCurrentCatalogLen);
                     if (OdbcRetCode <> OdbcApi.SQL_SUCCESS) then
                      begin
                        FreeMem(vCurrentCatalog);
                        Result := SQL_SUCCESS;
                        exit;
                      end;
                     if strCompNil(strUpper(vCurrentCatalog), strUpper(PAnsiChar(PropValue))) = 0 then
                      begin
                        FreeMem(vCurrentCatalog);
                        Result := SQL_SUCCESS;
                        exit;
                      end;
                     FreeMem(vCurrentCatalog);
                   end;
                  // set new catalog
                  OdbcRetCode := SQLSetConnectAttr(fOwnerDbxConnection.fhCon,
                     SQL_ATTR_CURRENT_CATALOG, PAnsiChar(PropValue), SQL_NTS);
                  if (OdbcRetCode = DBXpress.SQL_SUCCESS) then
                   begin
                     // Definition of a correctness of installation of the catalog using the server - dependent queries
                     case fOwnerDbxConnection.fOdbcDriverType of
                        eOdbcDriverTypeInformix:
                           if not sql_prepared(fOwnerDbxConnection,
                              PAnsiChar('select * from ' + PAnsiChar(PropValue) + ':systables where 1=0')
                              ) then OdbcRetCode := DBXpress.SQL_ERROR;
                      end;

                     if (OdbcRetCode <> DBXpress.SQL_SUCCESS) then // restore catalog
                        //OdbcRetCode :=
                        SQLSetConnectAttr(fOwnerDbxConnection.fhCon,
                           SQL_ATTR_CURRENT_CATALOG, fOwnerDbxConnection.fCurrentCatalog, SQL_NTS);
                   end
                  else if Assigned(fOwnerDbxConnection.fCurrentCatalog) then // restore catalog
                     //OdbcRetCode :=
                     SQLSetConnectAttr(fOwnerDbxConnection.fhCon,
                        SQL_ATTR_CURRENT_CATALOG, fOwnerDbxConnection.fCurrentCatalog, SQL_NTS);
                  //fOwnerDbxConnection.OdbcCheck (OdbcRetCode, 'SQLSetConnectAttr(SQL_ATTR_CURRENT_CATALOG)');
                end;
//*)
{/+2.01}
      end;
    eMetaSchemaName:
      begin
      {+2.01}

      // Vadim> ???Vad>All: In Oracle very slowly are read Synonyms.
      // Edward> I don't have Oracle, so I do not know

      // Vadim> Cached Schema into fCurrentSchema
      // Vadim> It is not allowed to change the schema, since in SqlExpr.pas
      // Vadim> the errors are contained.
      // Edward> ???Ed>Vad: I don't understand, but I have kept your code
      // Edward> My original code did nothing here, reason in comment just below

      if (coSupportsSchemaFilter in fOwnerDbxConnection.fConnectionOptions)
       and (Length(fOwnerDbxConnection.fCurrentSchema) = 0)
       and (StrLen(PAnsiChar(PropValue)) > 0) then
        fOwnerDbxConnection.fCurrentSchema := StrPas(PAnsiChar(PropValue));
      {/+2.01}
      {
      SQLExpress calls this option to set SCHEMA name to login USERNAME immediately after connecting.
      This is behaviour is undesirable. Probably a bug.
      (We return fully qualified table names where appropriate,
      eg UserId 'ED' may want to view table 'SYSSQL.SYSTABLES').
      So we just IGNORE this option.
      }
      end;
    eMetaDatabaseName: // Read-only
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaDatabaseName) not valid (Read-only)');
    eMetaDatabaseVersion: // Read-only
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaDatabaseVersion) not valid (Read-only)');
    eMetaTransactionIsoLevel: // (Read-only:
      // use the options of SQLConnection to set the transaction isolation level)
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaTransactionIsoLevel) not valid (Read-only) (Use options of ISQLConnection instead)');
    eMetaSupportsTransaction: // Read-only
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaSupportsTransaction) not valid (Read-only)');
    eMetaMaxObjectNameLength: // Read-only
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaMaxObjectNameLength) not valid (Read-only)');
    eMetaMaxColumnsInTable: // Read-only
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaMaxColumnsInTable) not valid (Read-only)');
    eMetaMaxColumnsInSelect: // Read-only
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaMaxColumnsInSelect) not valid (Read-only)');
    eMetaMaxRowSize: // Read-only
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaMaxRowSize) not valid (Read-only)');
    eMetaMaxSQLLength: // Read-only
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaMaxSQLLength) not valid (Read-only)');
    eMetaObjectQuoteChar: // Read-only
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaObjectQuoteChar) not valid (Read-only)');
    eMetaSQLEscapeChar: // Read-only
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaSQLEscapeChar) not valid (Read-only)');
    eMetaProcSupportsCursor: // Read-only
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaProcSupportsCursor) not valid (Read-only)');
    eMetaProcSupportsCursors: // Read-only
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaProcSupportsCursors) not valid (Read-only)');
    eMetaSupportsTransactions: // Read-only
      raise EDbxInvalidCall.Create(
      'TSQLMetaDataOdbc.SetOption(eMetaSupportsTransactions) not valid (Read-only)');
    {+2.01}
    {$IFDEF _D7UP_}
    eMetaPackageName:;
    // Edward> ???Ed>Vad: Vadim, why line below commented out?
    //raise EDbxInvalidCall.Create('TSQLMetaDataOdbc.SetOption(eMetaPackageName) not valid (Read-only)');
    {$ENDIF}
    {/+2.01}
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
  {+2.01}
  if fhStmt <> nil then
    begin
  {/+2.01}
    OdbcRetCode := SQLCloseCursor(fhStmt);
    OdbcCheck(OdbcRetCode, 'SQLCloseCursor');
  {+2.01}
    end;
  {/+2.01}

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

procedure TSqlCursorOdbc.OdbcCheck(OdbcCode: SQLRETURN; const OdbcFunctionName: string);
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
  {+2.01}
  // Vadim> ???Vad>Ed: OdbcLateBoundFound not initialized
  // Edward> Sorry Vadim - you were working on an old version - I had already fixed this
  OdbcLateBoundFound: boolean;
  DefaultFieldName: string;
  {+2.03}
  LastColNo: integer;
  {/+2.03}
begin
  ColNameTemp := nil;
  OdbcLateBoundFound := false;
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
  {+2.03}
  LastColNo := 0;
  {/+2.03}

// Describe each column...
  for colno := 1 to fOdbcNumCols do
  begin
  aOdbcBindCol := TOdbcBindCol.Create;
  fOdbcBindList.Items[colNo-1] := aOdbcBindCol;

  with aOdbcBindCol do
    begin
    OdbcRetCode := SQLDescribeCol(
      fhStmt, colno,
      ColNameTemp, fOwnerDbxConnection.fOdbcMaxColumnNameLen+1, fColNameSize,
      fSqlType, fColSize, fColScale, fNullable);
    OdbcCheck(OdbcRetCode, 'SQLDescribeCol');
    if (fColNameSize = 0) then
    // Allow for blank column names (returned by Informix stored procedures),
    // blank column names are also returned by functions, eg Max(Col)
    // Added v1.4 2002-01-16, for Bulent Erdemir
    // (Similar fix also posted by Michael Schwarzl)
      begin
      DefaultFieldName := 'Column_' + inttostr(colno);
      fColNameSize := length(DefaultFieldName);
      fColName := AllocMem(fColNameSize + 1);
      strLCopy(fColname, pchar(DefaultFieldName), fColNameSize);
      end
    else
      begin
      fColName := AllocMem(fColNameSize + 1);
      strLCopy(fColName, ColNameTemp, fColNameSize);
      end;

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
        {+2.01}
        // SQLLite does not support this option
        // Old code:
        // OdbcCheck(OdbcRetCode, 'SQLColAttribute(SQL_DESC_AUTO_UNIQUE_VALUE)');
        // if (IntAttribute = SQL_TRUE) then
        //   fDbxSubType:= fldstAUTOINC;
        // New code:
        if (OdbcRetCode = OdbcApi.SQL_SUCCESS) and (IntAttribute = SQL_TRUE) then
          fDbxSubType := fldstAUTOINC;
        {/+2.01}
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

        {+2.01}//Vadim V.Lopushansky:
        // Vadim> ???Vad>All: For supporting int64 remapping it to fldBCD type
        // Edward> This is a good idea.
        // Edward> ???Ed>All: I think it should be the default option,
        // Edward> I think the Borland dbexpress drivers map int64 to BCD
        if not (coMapInt64ToBcd in fOwnerDbxConnection.fConnectionOptions) then
          begin
          // Default code:
          fDbxType         := fldINT32;
          fOdbcHostVarType := SQL_C_LONG;
          fOdbcHostVarSize := SizeOf(fValue.OdbcColValueInteger);
          end
        else
          begin
          // Remapping to BCD
          fDbxType         := fldBCD;
          fOdbcHostVarType := SQL_C_CHAR; // Odbc prefers to return BCD as string
          fColSize         := 18;
          fColScale        := 0;
          fOdbcHostVarSize := fColSize + 3; // add 3 to number of digits: sign, decimal point, null terminator
          end;
        {/+2.01}
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
        {+2.01 Workaround for bad MERANT driver}
        // Vadim> ???Vad>All: MERANT 2.10 ODBC-OLE DB Adapter Driver: Error: "BCD Everflow" on query:
        // Edward> ???Ed>Vad: Which underlying DBMS were you connecting to with this driver?
        // Edward> I have never heard of this ODBC-OLE DB Adapter driver
        // Edward> only the other way round!
        // Edward> ???Ed>Ed: We sould have another eOdbcDriverType for this driver
        //
        //   select first 1
        //     unit_price
        //   from
        //    stores7:stock
        //
        // Native ODBC:
        // aOdbcBindCol = ('unit_price', 10, 3, 6, 2, 1, 0, 8, 0, 1, 9, ...
        // Merant bad format:
        // aOdbcBindCol = ('unit_price', 10, 3, 6, 4, 1, 0, 8, 0, 1, 9, ...
        //           value: 250.000
        // INOLE
        if (fColSize - fColScale <= 2)
        // Vadim> ???Vad>All: for any driver?
        // Edward> OK. It does no harm to other drivers if ColSize is 1 bigger
        // Edward> to allow for this bug in Merant driver.

        // and // Detect "MERANT 2.10 ODBC-OLE DB Adapter Driver"
        // ( Pos('INOLE',UpperCase(fOwnerDbxConnection.fOdbcDriverName))=1 )
          then fColSize := fColSize + 1;
        {/+2.01 /Workaround for bad MERANT driver}
        fOdbcHostVarSize := fColSize + 3; // add 3 to number of digits: sign, decimal point, null terminator
        {+2.01 Workaround for bad INFORMIX behaviour}
        //INFORMIX:
        {
        fColScale mast be less or equal fColSize.
        "INFORMIX 3.32 32 BIT" ODBC Returned fColScale equal 255 in next example:
        1) script tables
        --------------------------------------------------
        create table tbl (custno FLOAT primary key);
        insert into tbl values (1);
        2) exexute next query in DbExpress TSQLQuery:
        --------------------------------------------------
        select custno+1 from tbl;
        --------------------------------------------------
        When executing returned error in SqlExpr.pas:
        "invalid field size."
        It is error in informix matadata.
        Example:
        1) create view v1_tbl (custno) as select custno+1 from tbl
        2) look matadata columns info for view "v1_tbl":  custno DECIMAL (17,255).
        It error handled in DataDirect ODBC driver.
        }
        // INFORMIX: Error-checking in the metadata about the datatype of columns in informix

        // Edward> ???Ed>All: Really, this bug should be fixed in Informix DBMS,
        // Edward> not with such ugly hacks in here. But I have kept Vadim's fix.
        if (fOwnerDbxConnection.fOdbcDriverType = eOdbcDriverTypeInformix)
        and (fColSize <= 18) and (fColScale = 255) then
          begin
          fDbxType         := fldFLOAT;
          fOdbcHostVarType := SQL_C_DOUBLE;
          fOdbcHostVarSize := SizeOf(fValue.OdbcColValueDouble);
          end;
        {/+2.01 /Workaround for bad INFORMIX behaviour}
        if (fColScale > fColSize) then
          raise EDbxOdbcError.Create(
            'ODBC function "SQLDescribeCol" returned Column Scale > Column Size' + #13#10
            + 'Column name=' + ColNameTemp
            + ' Scale=' + IntToStr(fColScale)
            + ' Size=' + IntToStr(fColSize));
        {+2.01 Option for BCD mapping}
        // Vadim V.Lopushansky:
        // Vadim > ???Vad>All: If BCD is small then remap it to native type:
        // Edward> Nice idea.
        if coMapSmallBcdToNative in fOwnerDbxConnection.fConnectionOptions then
          begin
          if (fColSize <= 4) and (fColScale = 0) then
            begin
            fDbxType         := fldINT16;
            fOdbcHostVarType := SQL_C_SHORT;
            fOdbcHostVarSize := SizeOf(fValue.OdbcColValueShort);
            end
          else if (fColSize <= 9) and (fColScale = 0) then
            begin
            fDbxType         := fldINT32;
            fOdbcHostVarType := SQL_C_LONG;
            fOdbcHostVarSize := SizeOf(fValue.OdbcColValueInteger);
            end
          else if (fColSize <= 10) then
            begin
            fDbxType         := fldFLOAT;
            fOdbcHostVarType := SQL_C_DOUBLE;
            fOdbcHostVarSize := SizeOf(fValue.OdbcColValueDouble);
            end
          end;
        {/+2.01 /Option for BCD mapping}
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

          {+2.03 INFORMIX lvarchar}
          { Vadim V.Lopushansky:
             Fixed when error for mapping INFORMIX lvarchar type over native ODBC.
             Example query: select amparam from sysindices
          }

          if (fColSize>2048) then // When fColSize = maxInt then fOdbcHostVarSize
          begin                   // less zero after type conversion
            if (fOwnerCommand.fCommandBlobSizeLimitK < 0) then
            begin
                fOdbcHostVarSize := fColSize;
                fDbxType:= fldBLOB;
                fDbxSubType:= fldstMEMO;
                fOdbcHostVarType := SQL_C_CHAR;
                fOdbcLateBound := true;
            end
            else
            begin
              { Vadim>???Vad>All if fColSize > 2 Gb ???
                 Informix native odbc supported fOdbcLateBound, but if not ?
                 DataDirect ODBC for this informix type return length 2048.
              }
              fColSize := 2048;
              fOdbcHostVarSize := fColSize + 1;
              fpBuffer := AllocMem(fOdbcHostVarSize);
              fOdbcHostVarAddress := fpBuffer;
            end;
          end
          else
          {/2.03 /INFORMIX lvarchar}
          if fOdbcHostVarSize > 256 then
          begin
            // too big for buffer  - allocate memory for value
            fpBuffer := AllocMem(fOdbcHostVarSize);
            fOdbcHostVarAddress := fpBuffer;
          end;
          {+2.04 MapCharAsBDE}
          if (fDbxType <> fldBLOB) and
             (fColSize>256) and (coMapCharAsBDE in fOwnerDbxConnection.fConnectionOptions) then
          begin
            fDbxType:= fldBLOB;
            fDbxSubType:= fldstMEMO;
            fOdbcHostVarType := SQL_C_CHAR;
          end;
          {/2.04 / MapCharAsBDE}
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

      {+2.02}
      SQL_INTERVAL_YEAR .. SQL_INTERVAL_MINUTE_TO_SECOND:
        begin
        fDbxType         := fldZSTRING;
        fOdbcHostVarType := SQL_C_CHAR;
        fOdbcHostVarSize := 28;
        fDbxSubType := fldstFIXED;
        end;
      {/+2.02}
      else
        begin
        {+2.03 IgnoreUnknownFieldType option}
        if (coIgnoreUnknownFieldType in fOwnerDbxConnection.fConnectionOptions)
           and
           (not ( (LastColNo=0) and (colNo=fOdbcNumCols) ) )// when in query only one unknown field type
        then
          begin
          fOdbcBindList.Items[colNo-1] := nil;
          aOdbcBindCol.Free;
          continue
          end
        else
        {/+2.03 /IgnoreUnknownFieldType option}
          raise EDbxOdbcError.Create(
            'ODBC function "SQLDescribeCol" returned unknown data type' + #13#10 +
           'Data type code= ' + IntToStr(fSqlType) + #13#10 +
           'Column name=' + ColNameTemp);
        end;

      end; //case fSqlType of

      {+2.03}
      fOdbcBindList.Items[colNo-1]   := nil; // colNo may be NEQ LastColNo
      fOdbcBindList.Items[LastColNo] := aOdbcBindCol;
      inc(LastColNo);
      {/+2.03}

      {+2.01}
      // Vadim V.Lopushansky:
      // Vadim> ???Vad>Ed: It is necessary to verify logic
      // Edward> For BLOB columns, it is often not possible to determine the
      // Edward> size of the data that will be returned. So for BLOBs,
      // Edward> (unless there is a Row Size limit) we have to determine the
      // Edward> size and bind to host vars after every row fetch (late binding)
      // Edward> Now, the next problem is that some ODBC drivers, eg MS SqlServer,
      // Edward> require that all early-bound columns must be before the
      // Edward> late bound columns, while some drivers allow late bound and
      // Edward> early bound in any order. This is indicated by SQL_GD_ANY_ORDER.
      // Edward> So, for databases where SQL_GD_ANY_ORDER is FALSE, all columns
      // Edward> following the first late-bound column found must also be late-bound.
      // Edward> This is not really a problem, and any column (not just BLOBs)
      // Edward> can be late-bound if desired, it is just a bit less efficient.

      if fOdbcLateBound then
        // Vadim> ???Vad>Ed: OdbcLateBoundFound not initialized
        //                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        // Edward> Yes, this was a benign bug in the public version, but I had
        // Edward> already found this and fixed it in my own unreleased version
        OdbcLateBoundFound := true
      else
        begin
        if (OdbcLateBoundFound and (not fOwnerDbxConnection.fGetDataAnyColumn)) then
          // Driver does not support early-bound after late-bound columns,
          // and we have already had a late bound column, so we force this
          // column to be late-bound, even though normally it would be early-bound.
          fOdbcLateBound := true
        else
          begin
          // Early bound
          OdbcRetCode := SQLBindCol(
            fhStmt, colno, fOdbcHostVarType,
            fOdbcHostVarAddress, fOdbcHostVarSize,
            @fColValueSize);
          OdbcCheck(OdbcRetCode, 'SQLBindCol');
          end;
        end;
    end;  // with
    end;  // for each column
    {+2.03}
    fOdbcBindList.Count := LastColNo;
    fOdbcNumCols := LastColNo;
    {/+2.03}
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
  {+2.01}
  //Vadim V.Lopushansky: optimize BlobChunkSize:
  //old:
  //BlobChunkSize := 256;
  //new:
  if (fOwnerDbxConnection.fBlobChunkSize < 256)
  or (fOwnerDbxConnection.fBlobChunkSize > cBlobChunkSizeLimit) then
    fOwnerDbxConnection.fBlobChunkSize := cBlobChunkSizeDefault;
  BlobChunkSize := fOwnerDbxConnection.fBlobChunkSize;
  if (aOdbcBindCol.fColSize > 256) and (aOdbcBindCol.fColSize < BlobChunkSize) then
    BlobChunkSize := aOdbcBindCol.fColSize;
  {/+2.01}
  PreviousBlobSize := 0;
  with aOdbcBindCol do
    begin
    if fBlobFetched then exit;;
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
      {+2.01}
      //Michael Schwarzl
      //-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
      // blob load behaviour
      // Michael Schwarzl 31.05.2002
      //-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
      // on SQL-Server connections multiple reading of the blob leads into a error message from ODBC
      // the data has been read correctly at this time and when cursor leaves position next read will
      // be successful. So when returncode is SQL_NO_DATA but data has been loaded (fColValueSize > 0)
      // reset SQL Result Csode
      //-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
      if (fColValueSize > 0) and (OdbcRetCode = SQL_NO_DATA) then
        OdbcRetCode := 0;
      //-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
      {/+2.01}
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
      {+2.01}
      //Michael Schwarzl
      //-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
      // blob load behaviour - see above
      // Michael Schwarzl 31.05.2002
      if (fColValueSize > 0) and (OdbcRetCode = SQL_NO_DATA) then
        OdbcRetCode := 0;
      //-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
      {/+2.01}
      end;
    else
      raise EDbxInternalError.Create('FetchLongData called for invalid Dbx Sub Type');
    end;

    fColValueSize := PreviousBlobSize + fColValueSize;
    OdbcCheck(OdbcRetCode, 'SQLGetData');
    fBlobFetched := true;
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
  {+2.01}
  // for debugging:
  // aOdbcBindCol:TOdbcBindCol;
  {/+2.01}
begin
  with TOdbcBindCol(fOdbcBindList[ColumnNumber-1]) do
  {+2.01}
  // for debuging:
  //aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber-1]);
  //with aOdbcBindCol do
  {/+2.01}
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
    if assigned(Value) then  // Workaround bug in TBlobField.GetIsNull
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
      raise EDbxInvalidCall.Create(
       'TSqlCursorOdbc.getBlobSize but field is not BLOB - column '
       + IntToStr(ColumnNumber));

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
      raise EDbxNotSupported.Create
       ('TSqlCursorOdbc.getColumnLength - not yet supported for this type - column '
       + IntToStr(ColumnNumber));
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
    {+2.01}
      //Vadim V.Lopushnasky: Problems with loss of accuracy at type conversion.
      begin
      if fColSize < High(SmallInt) then
        piPrecision := fColSize
      else
        // Edward> ???Ed>Vad: -1 or High(SmallInt)?
        piPrecision := High(SmallInt);//-1
      end;
    {/+2.01}
    else
// DBXpress help says "Do not call getColumnPrecision for any other column type."
// But the donkey SqlExpress calls for EVERY column, so we cannot raise error
      piPrecision := 0;
//      raise EDbxNotSupported.Create(
//       'TSqlCursorOdbc.getColumnPrecision - not yet supported for data type - column '
//       + IntToStr(ColumnNumber));
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
  {+2.01}
  var
    vLen: Integer;
  {/+2.01}
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
    {+2.01}
    // Vadim V.Lopushansky:
    // Support Trim of Fixed Char when connection parameter "Trim Char" is True
    // Edward> This is a very good idea
    if (fDbxSubType = fldstFIXED) and fOwnerCommand.fTrimChar then
      begin
      vLen := fColValueSize;
      while PChar(fOdbcHostVarAddress)[vLen] in [' ', #0] do
        Dec(vLen);
      Move(fOdbcHostVarAddress^, Value^, vLen + 1);
      PChar(Value)[vLen + 1] := #0;
      end
    else
    {/+2.01}
    //Default code:
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
  OdbcRetCode := SQLColAttributeInt (fhStmt, ColumnNumber, SQL_DESC_AUTO_UNIQUE_VALUE,
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
  {+2.01}
  (*
  {$IFDEF MSWINDOWS}
  OdbcRetCode := SQLColAttributeInt(fhStmt, ColumnNumber, SQL_DESC_UPDATABLE, nil, 0, nil, IntAttribute);
  {$ELSE}
  // Vadim> ???Vad>All: The check on the Linux is necessary
  // Edward> ???Ed>Vad: This cannot be right!
  // Edward> Surely Linux ODBC must follow the ODBC standard here,
  // Edward> which is 1-based column index (same as DbExpress).
  // Edward> If this is not the case, the bug is in Linux ODBC.
  // Edward> So I have commented your change.
  // Edward> I myself had a bug here in an earlier version (1.03 and before):
  // Edward> I was incorrectly using ColumnNumber-1,
  // Edward> and I think this is the reason for the subsequent confusion
  // Edward> (My bug only became a problem with Delphi 6 SP 2, as earlier
  // Edward> Delphi SqlExpress seems to ignore the result of this function)
  OdbcRetCode := SQLColAttributeInt(fhStmt, ColumnNumber - 1, SQL_DESC_UPDATABLE, nil, 0, nil, IntAttribute);
  {$ENDIF}
  *)
  {/+2.01}
  OdbcRetCode := SQLColAttributeInt (fhStmt, ColumnNumber, SQL_DESC_UPDATABLE,
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
  OdbcRetCode := SQLColAttributeInt (fhStmt, ColumnNumber, SQL_DESC_SEARCHABLE,
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
  i: integer;
begin
try
  inc(fRowNo);

//  if (fRowNo > fOwnerDbxConnection.fRowLimit) then
//    Result := DBXERR_EOF;

  OdbcRetCode := SQLFetch(fhStmt);

  for i := 0 to fOdbcBindList.Count - 1 do
    TOdbcBindCol(fOdbcBindList[i]).fBlobFetched := false;

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

{+2.01 RDBMS type specific parsing}
//Vadim V.Lopushansky: RDBMS type specific parsing
// Edward> ???Ed>Ed/All: I want to see if we can find a better algorithm for
// Edward> non-standard databases like Informix

procedure TSqlCursorMetaData.ParseTableName(TableName: PChar);

  procedure ClearMetaData;
  begin
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
  end;
{$ifdef _RegExprParser_}
 var
   CatalogName,SchemaName,ObjectName:string;
begin
  ClearMetaData;
  fSqlConnectionOdbc.fObjectNameParser.DecodeObjectFullName(
    StrPas(TableName), CatalogName, SchemaName, ObjectName);
  if Length(ObjectName)=0 then
    exit;
  // OBJECT:
  fMetaTableName := AllocMem(Length(ObjectName) + 1);
  StrLCopy(fMetaTableName, PChar(ObjectName), Length(ObjectName) + 1);
  // SCHEMA:
  if Length(SchemaName)>0 then
  begin
    fMetaSchemaName := AllocMem(Length(SchemaName) + 1);
    StrLCopy(fMetaSchemaName, PChar(SchemaName), Length(SchemaName) + 1);
  end;
  // CATALOG:
  if Length(CatalogName)>0 then
  begin
    fMetaCatalogName := AllocMem(Length(CatalogName) + 1);
    StrLCopy(fMetaCatalogName, PChar(CatalogName), Length(CatalogName) + 1);
  end;
end;
{$else}

  var
    QuoteChar: AnsiChar;

  procedure DefaultParseTableName(TableName: PChar);
  var
    dot1, dot2: PChar;
    C_start, C_end, S_start, S_end, T_start, T_end: Integer;
  begin
  dot1 := StrPos(TableName, '.');

  C_start := 0;
  C_end   := 0;

  S_start := 0;
  S_end   := 0;

  T_start := 0;
  T_end   := StrLen(TableName) - 1;

  if dot1 <> nil then
    begin
    dot2 := StrPos(dot1 + 1, '.');
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
      Inc(C_start);
      Dec(C_end);
      end;
    fMetaCatalogName := AllocMem(C_end - C_Start + 2);
    StrLCopy(fMetaCatalogName, @TableName[C_start], C_end - C_start + 1);
    end;
  if (S_end <> 0) then
    begin
    if (TableName[S_start] = QuoteChar) and (TableName[S_end] = QuoteChar) then
      begin
      Inc(S_start);
      Dec(S_end);
      end;
    fMetaSchemaName := AllocMem(S_end - S_Start + 2);
    StrLCopy(fMetaSchemaName, @TableName[S_start], S_end - S_start + 1);
    end;

  if (TableName[T_start] = QuoteChar) and (TableName[T_end] = QuoteChar) then
    begin
    Inc(T_start);
    Dec(T_end);
    end;
  fMetaTableName := AllocMem(T_end - T_Start + 2);
  StrLCopy(fMetaTableName, @TableName[T_start], T_end - T_start + 1);
  end;

  procedure InformixParseTableName(TableName: PChar);
  var
    vTable, vStr: String;
    p: Integer;
  begin
  // format:   "catalog:schema:table" or "catalog::schema.table"
  //     catalog = database@server or database
  //     schema  = user
  // example:  dbdemos@infserver1:informix.biolife
  vTable :=
   StringReplace(StrPas(TableName), '::', ':', [rfReplaceAll, rfIgnoreCase]);
  if Length(vTable) = 0 then Exit;
  //Catalog:
  p := pos(':', vTable);
  if p > 0 then
    begin
    vStr := Copy(vTable, 1, p - 1);
    if Length(vStr) > 0 then
      begin
      fMetaCatalogName := AllocMem(Length(vStr) + 1);
      StrLCopy(fMetaCatalogName, PChar(vStr), Length(vStr) + 1);
      end;
    vTable := Copy(vTable, p + 1, Length(vTable) - p);
    if Length(vTable) = 0 then
      begin
      ClearMetaData;
      Exit;
      end;
    end;
  //Schema:
  p := pos('.', vTable);
  if p > 0 then
    begin
    vStr := Copy(vTable, 1, p - 1);
    if Length(vStr) > 0 then
      begin
      fMetaSchemaName := AllocMem(Length(vStr) + 1);
      StrLCopy(fMetaSchemaName, PChar(vStr), Length(vStr) + 1);
      end;
    vTable := Copy(vTable, p + 1, Length(vTable) - p);
    if Length(vTable) = 0 then
      begin
      ClearMetaData;
      Exit;
      end;
    end;
  //Table:
  if Length(Trim(vTable)) = 0 then
    begin
    ClearMetaData;
    Exit;
    end;
  fMetaTableName := AllocMem(Length(vTable) + 1);
  StrLCopy(fMetaTableName, PChar(vTable), Length(vTable) + 1);
  end;

  begin
  QuoteChar := fSqlConnectionOdbc.fQuoteChar;

  ClearMetaData;

  if (TableName[0] = #0) then exit; // nothing

  if fSqlConnectionOdbc.fOdbcDriverType = eOdbcDriverTypeInformix then
    InformixParseTableName(TableName)
  else
    DefaultParseTableName(TableName);
end;
{$endif}//of: {$ifdef _RegExprParser_}
{/+2.01 /RDBMS type specific parsing}

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
    {+2.01}
    //Think SQL:
    // Vadim> ???Vad>All:
    // Edward> I do not have ThinkSQL, but if that's how it works, your fix is OK
    if (fSqlConnectionOdbc.fOdbcDriverType = eOdbcDriverTypeThinkSQL) and
    not (aSqlType in [SQL_INTEGER, SQL_NUMERIC]) then
    {/+2.01}
      raise EDbxInternalError.Create(
        'BindSmallint called for non Smallint column no '
        + IntToStr(ColumnNo) + ' - ' + szColNameTemp);
  if (PBindInd = nil) and (aNullable <> OdbcApi.SQL_NO_NULLS) then
    raise EDbxInternalError.Create(
      'BindInteger without indicator var for nullable column '
      + IntToStr(ColumnNo) + ' - ' + szColNameTemp);
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

  {+2.01}
  // INFORMIX: SQL_C_SHORT in INFORMIX
  // Edward> This is fine -
  // Edward> ???Ed>Ed: I thought I had already fixed this -
  // ORIGINAL CODE:
  // if (aSqlType <> SQL_C_LONG) then
  // NEW CODE:
  if not (aSqlType in [SQL_C_LONG, SQL_C_SHORT]) then
  {/+2.01}
    raise EDbxInternalError.Create
    ('BindInteger called for non Integer column no '
     + IntToStr(ColumnNo) + ' - ' + szColNameTemp);
  if (BindInd = nil) and (aNullable <> OdbcApi.SQL_NO_NULLS) then
    raise EDbxInternalError.Create
    ('BindInteger without indicator var for nullable column '
     + IntToStr(ColumnNo) + ' - ' + szColNameTemp);
  OdbcRetCode := SQLBindCol(
   fhStmt, ColumnNo, SQL_C_LONG, @BindInteger, Sizeof(Integer), BindInd);
  OdbcCheck(OdbcRetCode, 'SQLBindCol');
end;

function TSqlCursorMetaData.getBcd(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
try
  raise EDbxInternalError.Create
  ('getBcd - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('getBlob - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('getBlobSize - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('getBytes - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('getColumnLength - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('getColumnPrecision - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('getDate - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('getDouble - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('getLong - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('GetOption - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('getShort - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('getString - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('getTime - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('getTimeStamp - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('isAutoIncrement - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('isBlobSizeExact - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('isSearchable - Unimplemented method invoked on metadata cursor');
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
  raise EDbxInternalError.Create
  ('SetOption - Unimplemented method invoked on metadata cursor');
except
  on E: EDbxError do
    begin
    fSqlCursorErrorMsg.Add(E.Message);
    Result := MaxReservedStaticErrors + 1;
    end;
end;
end;


procedure TSqlCursorMetaData.OdbcCheck(OdbcCode: SQLRETURN;
  const OdbcFunctionName: string);
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
  {$ifdef _RegExprParser_}
   vCatalogName, vSchemaName, vObjectName :String;
  {$else}
   QuoteChar: AnsiChar;
  {$endif}
begin
  aCatLen := 0;
  {$ifndef _RegExprParser_}
  QuoteChar := SqlConnectionOdbc.fQuoteChar;
  {$endif}

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

  {+2.01}
  //INFORMIX: tablename without owner
  if SqlConnectionOdbc.fOdbcDriverType = eOdbcDriverTypeInformix then
  //Edward> ???Ed>Vad/All: I do not have Informix, so I don't know for sure if this is correct
    begin                    // INFORMIX supports operation with the catalog, but usage of this
      WantCatalog := False;  // option is inconvenient for the developers and there is no large
      WantSchema  := False;  // sense  by work with INFORMIX. If you want to work with the catalog,
    end;                     // comment out this block.
  {/+2.01}

  {$ifdef _RegExprParser_}

    if WantCatalog and Assigned(Cat) then
      vCatalogName := StrPas(Cat)
    else
      SetLength(vCatalogName,0);

    if WantSchema and Assigned(Schema) then
      vSchemaName := StrPas(Schema)
    else
      SetLength(vSchemaName,0);

    if Assigned(TableName) then
      vObjectName := StrPas(TableName)
    else
      SetLength(vObjectName,0);

    // The calculation of a full qualified name:
    vObjectName := SqlConnectionOdbc.fObjectNameParser.EncodeObjectFullName(
      vCatalogName, vSchemaName, vObjectName);

    if Length(vObjectName)>0 then
    begin
      fQualifiedTableName := AllocMem(Length(vObjectName)+1);
      StrLCopy(fQualifiedTableName, PChar(vObjectName), Length(vObjectName)+1);
    end
    else // The conversion was not successful:
      fQualifiedTableName := nil;

  {$else}
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
  {$endif}
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
    {+2.01}
    if Assigned(Cat) then
      StrCopy(fCat, Cat)
    else
      StrCopy(fCat, #0);
    {/+2.01}
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
  1. RECNO         fldINT32
       A record number that uniquely identifies each record.
  2. CATALOG_NAME  fldZSTRING
       The name of the catalog (database) that contains the table.
  3. SCHEMA_NAME   fldZSTRING
       The name of the schema that identifies the owner of the table.
  4. TABLE_NAME    fldZSTRING
       The name of the table.
  5. TABLE_TYPE    fldINT32
       An eSQLTableType value (C++) or table type constant (Object Pascal)
       that indicates the type of table.

 ODBC result set columns
  1. TABLE_CAT     Varchar
       Catalog name; NULL if not applicable to the data source
  2. TABLE_SCHEM   Varchar
       Schema name; NULL if not applicable to the data source.
  3. TABLE_NAME    Varchar
       Table name
  4. TABLE_TYPE    Varchar
       Table type name eg TABLE, VIEW, SYNONYM, ALIAS etc
  5. REMARKS       Varchar
       A description of the table
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

  {+2.01 Metadata CurrentSchema Filter}
  // Vadim V.Lopushansky: Set Metadata CurrentSchema Filter
  // Edward> ???Ed>Vad: ODBC V3 certainly has the capability to support this,
  // Edward> but I don't think any DbExpress application would ever want it.
  // Edward> ???Ed>All:
  // Edward> This is much more tricky than it looks at first.
  // Edward> ODBC V2 and V3 specifications differ in their behaviour here,
  // Edward> and different databases also behave differently.
  // Edward> Also, there is a particular problem if the real Schema name might
  // Edward> contain underscore character, which just happens to be the ODBC
  // Edward> wildcard character. In this case you should use an escape character,
  // Edward> but dbexpress cannot easily handle this,
  // Edward> The consistent handling of other metadata objects also needs to
  // Edward> be considered, and this requires investigation and careful thought.
  // Edward> As far as I remember, dbExpress "specificiation" (ha ha) is
  // Edward> inconsistent/unclear between the various metadata querying interfaces,
  // Edward> and it is not easily compatible with the ODBC specification (for
  // Edward> example, ODBC specification allows the catalog to be specified, but
  // Edward> dbexpress does not.
  // Edward> Really this is getting too complicated, and my feeling it is best
  // Edward> just to leave it out. But I have kept Vadim's code for now.
  if (coSupportsSchemaFilter in fSqlConnectionOdbc.fConnectionOptions) and
     (Length(fSqlConnectionOdbc.fCurrentSchema) > 0) and
     ((SearchTableType and eSQLSystemTable) = 0) then
    begin
    OdbcRetCode := SQLTables(fhStmt,
      nil, 0, // all catalogs
      PChar(fSqlConnectionOdbc.fCurrentSchema), Length(fSqlConnectionOdbc.fCurrentSchema), // all schemas
      SearchTableName, SQL_NTS, // Table name match pattern
      sTableTypes, SQL_NTS); // Table types
    end
  else
  {/+2.01 /Metadata CurrentSchema Filter}

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
    if Assigned(aMetaTable.fQualifiedTableName) then // If the conversion was successful:
      fTableList.Add(aMetaTable)
    else
      aMetaTable.Free;

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
      {+2.01}
      //Vadim V.Lopushansky:
      // Corrections for calculation schema maxLength
      // Edward - Yes, that was bug - I hadn't spotted that one!
      MaxSet(fSchemaLenMax, StrLen(aMetaTable.fSchema));
      {/+2.01}
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
      'TSqlCursorMetaDataTables.getColumnLength invalid column no: '
      + IntToStr(ColumnNumber));
    end;
  end;
  {+2.01}
  // Vadim V.Lopushansky:
  // add visible columns and SqlExpr.pas type identification.
  // If Length = 0 then field is Unbinding...
  // Edward> ???Ed>Vad: I do not understand, but I trust you are correct
  if (pLength = 0) and (TableColumnTypes[ColumnNumber] = fldZSTRING) then
    pLength := 1;
  {/+2.01}
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
        'TSqlCursorMetaDataTables.getLong not valid for column '
        + IntToStr(ColumnNumber));
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
        'TSqlCursorMetaDataTables.getString not valid for column '
        + IntToStr(ColumnNumber));
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
  {+2.01}
  if (fTableList = nil) or
     (fRowNo > fTableList.Count) then
  {/+2.01}
    begin
    Result := DBXERR_EOF;
    exit;
    end;
  fMetaTableCurrent := fTableList[fRowNo-1];
  Result := DBXpress.SQL_SUCCESS;
end;

{ TSqlCursorColumns }

{
1.  RECNO            fldINT32
      A record number that uniquely identifies each record.
2.  CATALOG_NAME     fldZSTRING
      The name of the catalog (database) that contains the table.
3.  SCHEMA_NAME      fldZSTRING
      The name of the schema that identifies the owner of the table.
4.  TABLE_NAME       fldZSTRING
      The name of the table in which the column appears.
5.  COLUMN_NAME      fldZSTRING
      The name of the field (column).
6.  COLUMN_POSITION  fldINT16
      The position of the column in its table.
7.  COLUMN_TYPE      fldINT32
      An eSQLColType value (C++) or column type constant (Object Pascal)
      that indicates the type of field.
8.  COLUMN_DATATYPE  fldINT16
      The logical data type for the field.
9.  COLUMN_TYPENAME  fldZSTRING
      A string describing the datatype.
      This is the same information as contained in COLUMN_DATATYPE
      and COLUMN_SUBTYPE, but in a form used in some DDL statements.
10. COLUMN_SUBTYPE   fldINT16
      The logical data subtype for the field.
11. COLUMN_PRECISION fldINT32
      The size of the field type (number of characters in a string, bytes in a
      bytes field, significant digits in a BCD value, members of an ADT field, and so on)
12. COLUMN_SCALE     fldINT16
      The number of digits to the right of the decimal on BCD values,
      or descendants on ADT and array fields.
13. COLUMN_LENGTH    fldINT32
      The number of bytes required to store field values.
14. COLUMN_NULLABLE  fldINT16
      If the field requires a value, nonzero if it can be blank.

ODBC result set columns
1.  TABLE_CAT         Varchar
      Catalog name; NULL if not applicable to the data source
2.  TABLE_SCHEM       Varchar
      Schema name; NULL if not applicable to the data source.
3.  TABLE_NAME        Varchar
      Table name
4.  COLUMN_NAME       Varchar not NULL
      Column name. Empty string for a column that does not have a name
5.  DATA_TYPE         Smallint not NULL
      SQL data type
6.  TYPE_NAME         Varchar not NULL
      Data source – dependent data type name
7.  COLUMN_SIZE       Integer
     Column Size
     If DATA_TYPE is SQL_CHAR or SQL_VARCHAR, then this column contains the
     maximum length in characters of the column
     For datetime data types, this is the total number of characters required
     to display the value when converted to characters.
     For numeric data types, this is either the total number of digits or the total
     number of bits allowed in the column, according to the NUM_PREC_RADIX column
8.  BUFFER_LENGTH     Integer
      The length in bytes of data transferred on SqlFetch etc if SQL_C_DEFAULT is specified
9.  DECIMAL_DIGITS    Smallint
      The total number of significant digits to the right of the decimal point
10. NUM_PREC_RADIX    Smallint
      For numeric data types, either 10 or 2.
11. NULLABLE          Smallint not NULL
      SQL_NO_NULLS / SQL_NULLABLE / SQL_NULLABLE_UNKNOWN
12. REMARKS           Varchar
      A description of the column
13. COLUMN_DEF        Varchar
      The default value of the column
14. SQL_DATA_TYPE     Smallint not NULL
     SQL data type,
     This column is the same as the DATA_TYPE column, with the exception of
     datetime and interval data types.
     This column returns the nonconcise data type (such as SQL_DATETIME or SQL_INTERVAL),
     rather than the concise data type (such as SQL_TYPE_DATE or SQL_INTERVAL_YEAR_TO_MONTH)
15. SQL_DATETIME_SUB  Smallint
      The subtype code for datetime and interval data types.
      For other data types, this column returns a NULL.
16. CHAR_OCTET_LENGTH Integer
      The maximum length in bytes of a character or binary data type column.
17. ORDINAL_POSITION  Integer not NULL
      The ordinal position of the column in the table
18. IS_NULLABLE       Varchar
      'NO' if the column does not include NULLs
      'YES' if the column could include NULLs
      zero-length string if nullability is unknown.
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
  cbOrdinalPosition: Integer;
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
  {+2.01}
  DefaultValue := nil;
  {/+2.01}

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
    {+2.01}
    // DefaultValue := nil;
    OrdinalPosition := 0;
    cbDefaultValue  := OdbcAPi.SQL_NULL_DATA;
    {/+2.01}
    end
  else
    begin
    {+2.01}
    //Vadim V.Lopushansky:
    // Automatically assign fOdbcDriverLevel mode to 2 when exception
    try
      DescribeAllocBindString(13, DefaultValue, cbDefaultValue);
      BindInteger(17, OrdinalPosition, @cbOrdinalPosition);
    except
      fSqlConnectionOdbc.fOdbcDriverLevel := 2;
      // Initialize as Level 2
      OrdinalPosition := 0;
      cbDefaultValue  := OdbcAPi.SQL_NULL_DATA;
      end;
    {/+2.01}
    end;
  fTableList:= TList.Create;
  fColumnList:= TList.Create;

  OdbcRetCode := SQLFetch(fhStmt);

  while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
    begin

    OdbcCheck(OdbcRetCode, 'SQLFetch');
    {+2.01}
    //Vadim V.Lopushansky:
    // The code for drivers which not supporting filter
    // (Easysoft IB6 ODBC Driver [ver:1.00.01.67] contain its error).
    // Edward> ???Ed>Vad/All: I think column name filter is a bad idea (see long
    // Edward> comment under TSqlCursorMetaDataTables.FetchTables).
    // Edward> ???Ed>Ed: I think the filter should also be removed from my code above.
    // Edward> But I have kept it all for now.
    if Assigned(SearchColumnName) then
      i := StrLen(SearchColumnName)
    else
      i := 0;
    if (i > 0) and ((i <> Integer(StrLen(ColumnName))) or
      (StrLComp(SearchColumnName, ColumnName, i) <> 0)) then
      begin
      OdbcRetCode := SQLFetch(fhStmt);
      continue;
      end;
    {/+2.01}

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
      // if RADIX = 2, Odbc column size is number of BITs;
      // Decimal Digits is log10(2) * BITS = 0.30103 * No of BITS
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
    {+2.01}
    // Vadim> ???Vad>All: OpenLink Lite for Informix 7 (32 Bit) ODBC Driver:
    // (aMetaColumn.fDbxType = 3 = BLOB )
    // Edward> I do not have Informix, I do not know
    // Vadim> Problems with loss of accuracy at type conversion.
    if aMetaColumn.fPrecision > High(SmallInt) then
      begin
      aMetaColumn.fPrecision := -1;
    // Edward> ???Ed>Vad/All: This does not look right!
    // Edward> But I do not understand exactly what you are trying to do
      if aMetaColumn.fLength > High(SmallInt) then
        aMetaColumn.fLength := High(Integer);
    end;
    {/+2.01}

{ Dbx Column type is combination of following flags
eSQLRowId         Row Id number.
eSQLRowVersion    Version number.
eSQLAutoIncr      Auto-incrementing field (server generates value).
eSQLDefault       Field with a default value. (server can generate value)

eSQLRowId      - This can be determined by Odbc call SQLSpecialColumns SQL_BEST_ROWID
eSQLRowVersion - This can be determined by Odbc call SQLSpecialColumns SQL_ROWVER
eSQLAutoIncr   - Odbc does not have facility to determine this until actual result set
eSQLDefault    - Odbc will return the defaulkt value
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
      {+2.01}
      //Vadim V.Lopushansky: Corrections for calculation schema maxLength
      MaxSet(fSchemaLenMax, StrLen(aMetaTable.fSchema));
      {/+2.01}
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

  if (fhStmt <> SQL_NULL_HANDLE) then
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
      'TSqlCursorMetaDataColumns.getColumnLength invalid column no: '
      + IntToStr(ColumnNumber));
    end;
  end;
  {+2.01}
  //Vadim V.Lopushansky:
  // if Length is Zero then columns will be hidden (unbinded).
  // Look SqlExpr.pas ...
  // Edward> ???Ed>Vad: I do not understand this, but I assume you know what you are doing
  if (pLength = 0) and (ColumnColumnTypes[ColumnNumber] = fldZSTRING) then
    pLength := 1;
  {/+2.01}
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
  {+2.01}
  //Vadim V.Lopushansky:
  // Problems with loss of accuracy at type conversion
  // Edward> I assume this is correct
  if Length < LongWord(High(SmallInt)) then
    piPrecision := Length
  else
    piPrecision := -1;
  {/+2.01}
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
        'TSqlCursorMetaDataColumns.getLong not valid for column '
        + IntToStr(ColumnNumber));
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
        'TSqlCursorMetaDataColumns.getShort not valid for column '
        + IntToStr(ColumnNumber));
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
        'TSqlCursorMetaDataColumns.getString not valid for column '
        + IntToStr(ColumnNumber));
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
  {+2.01}
  if (fColumnList = nil) or
   (fRowNo > fColumnList.Count) then
  {/+2.01}
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
1.  RECNO           fldINT32
      A record number that uniquely identifies each record.
2.  CATALOG_NAME    fldZSTRING
      The name of the catalog (database) that contains the index.
3.  SCHEMA_NAME     fldZSTRING
      The name of the schema that identifies the owner of the index.
4.  TABLE_NAME      fldZSTRING
      The name of the table for which the index is defined.
5.  INDEX_NAME      fldZSTRING
      The name of the index.
6.  PKEY_NAME       fldZSTRING
      The name of the primary key.
7.  COLUMN_NAME     fldZSTRING
      The name of the column (field) in the index.
8.  COLUMN_POSITION fldINT16
      The position of this field in the index.
9.  INDEX_TYPE      fldINT16
      An eSQLIndexType value (C++) or index type constant (Object Pascal) that
      indicates any special properties of the index.
10. SORT_ORDER      fldZSTRING
      Indicates whether the index sorts on this field in
      ascending (a) or descending (d) order.
11. FILTER          fldZSTRING
      A string that gives a filter condition limiting indexed records.

ODBC SqlStatistics Result set columns:

1.  TABLE_CAT        Varchar
      Catalog name of the table to which the statistic or index applies;
      NULL if not applicable to the data source.
2.  TABLE_SCHEM      Varchar
      Schema name of the table to which the statistic or index applies;
      NULL if not applicable to the data source.
3.  TABLE_NAME       Varchar not NULL
      Table name of the table to which the statistic or index applies.
4.  NON_UNIQUE       Smallint
      Indicates whether the index prohibits duplicate values:
      SQL_TRUE if the index values can be nonunique.
      SQL_FALSE if the index values must be unique.
      NULL is returned if TYPE is SQL_TABLE_STAT.
5.  INDEX_QUALIFIER  Varchar
      The identifier that is used to qualify the index name doing a DROP INDEX;
      NULL is returned if an index qualifier is not supported by the data source
      or if TYPE is SQL_TABLE_STAT.
      If a non-null value is returned in this column, it must be used to qualify
      the index name on a DROP INDEX statement; otherwise the TABLE_SCHEM
      should be used to qualify the index name.
6.  INDEX_NAME       Varchar
       Index name; NULL is returned if TYPE is SQL_TABLE_STAT.
7.  TYPE             Smallint not NULL
      Type of information being returned:
      SQL_TABLE_STAT indicates a statistic for the table (in the CARDINALITY or PAGES column).
      SQL_INDEX_BTREE indicates a B-Tree index.
      SQL_INDEX_CLUSTERED indicates a clustered index.
      SQL_INDEX_CONTENT indicates a content index.
      SQL_INDEX_HASHED indicates a hashed index.
      SQL_INDEX_OTHER indicates another type of index.
8.  ORDINAL_POSITION Smallint
      Column sequence number in index (starting with 1);
      NULL is returned if TYPE is SQL_TABLE_STAT.
9.  COLUMN_NAME      Varchar
      Column name.
      If the column is based on an expression, such as SALARY + BENEFITS,
      the expression is returned;
      if the expression cannot be determined, an empty string is returned.
      NULL is returned if TYPE is SQL_TABLE_STAT.
10. ASC_OR_DESC      Char(1)         Sort sequence for the column;
     'A' for ascending; 'D' for descending;
     NULL is returned if column sort sequence is not supported by the
     data source or if TYPE is SQL_TABLE_STAT.
11. CARDINALITY      Integer         Cardinality of table or index;
     number of rows in table if TYPE is SQL_TABLE_STAT;
     number of unique values in the index if TYPE is not SQL_TABLE_STAT;
     NULL is returned if the value is not available from the data source.
12. PAGES            Integer
     Number of pages used to store the index or table;
     number of pages for the table if TYPE is SQL_TABLE_STAT;
     number of pages for the index if TYPE is not SQL_TABLE_STAT;
     NULL is returned if the value is not available from the data source,
     or if not applicable to the data source.
13. FILTER_CONDITION Varchar
     If the index is a filtered index, this is the filter condition,
     such as SALARY > 30000;
     if the filter condition cannot be determined, this is an empty string.
     NULL if the index is not a filtered index, it cannot be determined whether
     the index is a filtered index, or TYPE is SQL_TABLE_STAT.


ODBC SqlPrimaryKeys Result set columns:

1.  TABLE_CAT   Varchar
      Primary key table catalog name;
      NULL if not applicable to the data source.
      If a driver supports catalogs for some tables but not for others,
      such as when the driver retrieves data from different DBMSs,
      it returns an empty string ('') for those tables that do not have catalogs.
2.  TABLE_SCHEM Varchar
      Primary key table schema name;
      NULL if not applicable to the data source.
      If a driver supports schemas for some tables but not for others,
      such as when the driver retrieves data from different DBMSs,
      it returns an empty string ('') for those tables that do not have schemas.
3.  TABLE_NAME  Varchar not NULL
      Primary key table name.
4.  COLUMN_NAME Varchar not NULL
      Primary key column name.
      The driver returns an empty string for a column that does not have a name.
5.  KEY_SEQ     Smallint not NULL  Column sequence number in key (starting with 1).
6.  PK_NAME     Varchar
      Primary key name. NULL if not applicable to the data source.
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

  OdbcPkName: pAnsiChar;
  OdbcPkColumnName: pAnsiChar;

  IndexName: pAnsiChar;
  IndexColumnName: pAnsiChar;
  IndexFilter: pAnsiChar;
  IndexColumnPosition: Smallint;
  AscDesc: array[0..1] of char;

{ Vars below were used for search pattern logic - now commented out
  Cat: pAnsiChar;
  Schema: pAnsiChar;
  TableName: pAnsiChar;
  OdbcTableType: pAnsiChar;

  cbCat: integer;
  cbSchema: integer;
  cbTableName: integer;
  cbOdbcTableType: integer;//}

  cbOdbcPkColumnName: integer;
  cbOdbcPkName: integer;

  cbIndexName: integer;
  cbIndexColumnName: integer;
  cbIndexFilter: integer;
  cbOdbcNonUnique: integer;
  cbAscDesc: integer;
  cbIndexColumnPosition: Smallint;
  cbOdbcIndexType: Integer;

  OdbcIndexType: Smallint;
  OdbcNonUnique: Smallint;

  i: integer;
  aMetaTable: TMetaTable;
  aMetaIndexColumn: TMetaIndexColumn;
begin

{ Vars below were used for search pattern logic - now commented out
  Cat := nil;
  Schema := nil;
  TableName := nil;
  OdbcTableType := nil;//}

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

    {+2.01}
    //Vadim V.Lopushansky: Corrections for calculation fPkNameLenMax
    //For an example look: ($DELPHI$)\Demos\Db\DbxExplorer\dbxexplorer.dpr
    fPkNameLenMax := 0;
    {/+2.01}

// -----------------------------------------------
// This is to find the PRIMARY KEY of the table...
    if fSqlConnectionOdbc.fSupportsSQLPRIMARYKEYS then
      begin
      OdbcRetCode := SQLPrimaryKeys(fhStmt,
       aMetaTable.fCat, SQL_NTS, // Catalog name (match pattern not allowed)
       aMetaTable.fSchema, SQL_NTS,  // Schema name (match pattern not allowed)
       aMetaTable.fTableName,  SQL_NTS); // Table name (match pattern not allowed)
      {+2.01}
      // INFORMIX: The error is possible at call to other database.
      // Example:  select username from sysmaster::informix.syssessions
      // OdbcCheck(OdbcRetCode, 'SQLPrimaryKeys');
      if OdbcRetCode = OdbcApi.SQL_SUCCESS then
        begin
        {/+2.01}
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
            {+2.01}
            //Vadim V.Lopushansky: Corrections for calculation fPkNameLenMax
            // Edward> Yes, that was a bug
            begin
            MaxSet(fPkNameLenMax, StrLen(OdbcPkColumnName));
            aMetaIndexColumn := TMetaIndexColumn.Create(aMetaTable, OdbcPkName, OdbcPkColumnName);
            end;
            {/+2.01}
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
        {+2.01}
        end;//of: if OdbcRetCode = OdbcApi.SQL_SUCCESS
        {/+2.01}
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

//      OdbcCheck(OdbcRetCode, 'SQLStatistics');
      if OdbcRetCode = OdbcApi.SQL_SUCCESS then
        begin
        DescribeAllocBindString(6, IndexName, cbIndexName);
        DescribeAllocBindString(9, IndexColumnName, cbIndexColumnName);
        BindSmallInt(4, OdbcNonUnique, @cbOdbcNonUnique);
        {+2.01}
        //BindSmallInt(7, OdbcIndexType, nil);
        BindSmallInt(7, OdbcIndexType, @cbOdbcIndexType);
        {/+2.01}
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
              if (cbOdbcNonUnique <> OdbcApi.SQL_NULL_DATA)
              and (OdbcNonUnique = SQL_FALSE) then
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
        {+2.01}
        end;//of: if OdbcRetCode = OdbcApi.SQL_SUCCESS
        {/+2.01}
      end;

  fCatLenMax := 0;
  fSchemaLenMax := 0;
  fTableLenMax := 1;
  fIndexNameLenMax := 1;
  fIndexColumnNameLenMax := 1;
  {+2.01}
  //Vadim V.Lopushansky:
  //Corrections for calculation fPkNameLenMax - comment out this line
  // fPkNameLenMax := 0;
  {/+2.01}
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
{ Vars below were used for search pattern logic - now commented out
  FreeMem(Cat);
  FreeMem(Schema);
  FreeMem(TableName);
  FreeMem(OdbcTableType);//}
  FreeMem(OdbcPkName);
  FreeMem(OdbcPkColumnName);
  FreeMem(IndexFilter);
  FreeMem(IndexName);
  FreeMem(IndexColumnName);

  if (fhStmt <> SQL_NULL_HANDLE) then
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
    9:    // INDEX_TYPE      fldINT16
      pLength := SizeOf(Smallint);
    10:    // SORT_ORDER     fldZSTRING
      pLength := 1;
    11:    // FILTER         fldZSTRING
      pLength := fFilterLenMax;
  else
    begin
    raise EDbxInvalidCall.Create(
      'TSqlCursorMetaDataIndexes.getColumnLength invalid column '
      + IntToStr(ColumnNumber));
    end;
  end;
  {+2.01}
  //Vadim V.Lopushansky:
  // if Length is Zero then columns will be hidden (unbinded). Look SqlExpr.pas ...
  // Edward> ???Ed>Vad: I do not understand this, but I assume you know what you are doing
  if (pLength = 0) and (IndexColumnTypes[ColumnNumber] = fldZSTRING) then
    pLength := 1;
  {/+2.01}
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
  {+2.01}
  //Vadim V.Lopushansky: Problems with loss of accuracy at type conversion
  // Edward> ???Ed>Vad: SqlIndexes should never return such a long column,
  // Edward> but this does no harm, so I leave it in
  if Length < LongWord(High(SmallInt)) then
    piPrecision := Length
  else
    piPrecision := -1;
  {/+2.01}
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
        'TSqlCursorMetaDataIndexes.getLong not valid for column '
        + IntToStr(ColumnNumber));
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
    9:    // INDEX_TYPE      fldINT16
      begin
      SmallInt(Value^) := fCurrentIndexColumn.fIndexType;
      IsBlank := False;
      end;
    else
      begin
      raise EDbxInvalidCall.Create(
        'TSqlCursorMetaDataIndexes.getLong not valid for column '
         + IntToStr(ColumnNumber));
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
    1:; // RECNO           fldINT32
    2:  // CATALOG_NAME
      begin
        IsBlank := (not fSqlConnectionOdbc.fSupportsCatalog) or
                   (fCurrentIndexColumn.fMetaTable.fCat = nil);
        if not IsBlank then
          StrCopy(Value, pChar(fCurrentIndexColumn.fMetaTable.fCat));
      end;
    3:  // SCHEMA_NAME
      begin
        IsBlank := (fSchemaLenMax = 0) or (fCurrentIndexColumn.fMetaTable.fSchema=nil);
        if not IsBlank then
          StrCopy(Value, pChar(fCurrentIndexColumn.fMetaTable.fSchema));
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
    9:;   // INDEX_TYPE      fldINT16
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
        'TSqlCursorMetaDataIndexes.getLong not valid for column '
        + IntToStr(ColumnNumber));
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
  {+2.01}
  if Assigned(fIndexList) and (fRowNo <= fIndexList.Count) then
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
 1. RECNO         fldINT32
      A record number that uniquely identifies each record.
 2. CATALOG_NAME  fldZSTRING
      The name of the catalog (database) that contains the stored procedure.
 3. SCHEMA_NAME   fldZSTRING
      The name of the schema that identifies the owner of the stored procedure.
 4. PROC_NAME     fldZSTRING
      The name of the stored procedure.
 5. PROC_TYPE     fldINT32
      An eSQLProcType value (C++) or stored procedure type constant (Object Pascal)
      that indicates the type of stored procedure.
 6. IN_PARAMS     fldINT16
      The number of input parameters.
 7. OUT_PARAMS    fldINT16
      The number of output parameters.

ODBC result set columns from SQLProcedures
 1. PROCEDURE_CAT     Varchar
      Catalog name; NULL if not applicable to the data source
 2. PROCEDURE_SCHEM   Varchar
      Schema name; NULL if not applicable to the data source.
 3. PROCEDURE_NAME    Varchar not null
      Procedure identifier
 4. NUM_INPUT_PARAMS  N/A         Reserved for future use
 5. NUM_OUTPUT_PARAMS N/A         Reserved for future use
 6. NUM_RESULT_SETS   N/A         Reserved for future use
 7. REMARKS           Varchar
      A description of the procedure
 8. PROCEDURE_TYPE    Smallint    Defines the procedure type:
      SQL_PT_UNKNOWN:   It cannot be determined whether the procedure returns a value.
      SQL_PT_PROCEDURE: The returned object is a procedure;
       that is, it does not have a return value.
      SQL_PT_FUNCTION:  The returned object is a function;
       that is, it has a return value.
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

  {+2.01}
  //Vadim V.Lopushansky:
  // Set Metadata CurrentSchema Filter
  // Edward> Again, I don't think any real dbxpress application will use
  // Edward> schema filter, but I leave this code sa it is harmless
  if (coSupportsSchemaFilter in fSqlConnectionOdbc.fConnectionOptions) and
     (Length(fSqlConnectionOdbc.fCurrentSchema) > 0) then
   begin
     OdbcRetCode := SQLProcedures(fhStmt,
        nil, 0, // all catalogs
        PChar(fSqlConnectionOdbc.fCurrentSchema),
        Length(fSqlConnectionOdbc.fCurrentSchema), // current schemas
        ProcedureName, SQL_NTS); // Procedure name match pattern
   end
  else
  {/+2.01}

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
      {+2.01}
      //Vadim V.Lopushansky: Corrections for calculation schema maxLength
      MaxSet(fSchemaLenMax, StrLen(Schema));
      {/+2.01}
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
      'TSqlCursorMetaDataProcedures.getColumnLength invalid column no: '
      + IntToStr(ColumnNumber));
    end;
  end;
  {+2.01}
  //Vadim V.Lopushansky: if Length is Zero then columns will be hidden (unbinded). Look SqlExpr.pas ...
  // Edward> ???Ed>Vad: I still don't understand, but it looks like you do
  if (pLength = 0) and (ProcedureColumnTypes[ColumnNumber] = fldZSTRING) then
    pLength := 1;
  {/+2.01}
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
  {+2.01}
  //Vadim V.Lopushansky: Problems with loss of accuracy at type conversion
  // Edward> ???Ed>Vad: No column from SqlProcedures should ever be this big,
  // Edward> but it does no harm, and it is consistent with other changes
  if Length < LongWord(High(SmallInt)) then
    piPrecision := Length
  else
    piPrecision := -1;
   {/+2.01}
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
      'TSqlCursorMetaDataProcedures.getLong invalid column no: '
      + IntToStr(ColumnNumber));
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

function TSqlCursorMetaDataProcedures.getString(ColumnNumber: Word;
  Value: Pointer; var IsBlank: LongBool): SQLResult;
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
      'TSqlCursorMetaDataProcedures.getString invalid column no: '
      + IntToStr(ColumnNumber));
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
  {+2.01}
  if (fProcList <> nil) and
   (fRowNo <= fProcList.Count) then
  {/+2.01}
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
 1.  RECNO          fldINT32
       A record number that uniquely identifies each record.
 2.  CATALOG_NAME      fldZSTRING
       The name of the catalog (database) that contains the stored procedure.
 3.  SCHEMA_NAME       fldZSTRING
       The name of the schema that identifies the owner of the stored procedure.
 4.  PROC_NAME         fldZSTRING
       The name of the procedure in which the parameter appears.
 5.  PARAM_NAME        fldZSTRING
       The name of the parameter.
 6.  PARAM_TYPE        fldINT16
       A STMTParamType value that indicates whether the parameter is used
       for input, output, or result.
 7.  PARAM_DATATYPE    fldINT16
       The logical data type for the parameter.
 8.  PARAM_SUBTYPE     fldINT16
       The logical data subtype for the parameter.
 9.  PARAM_TYPENAME    fldZSTRING
      A string describing the datatype.
      This is the same information as contained in PARAM_DATATYPE
      and PARAM_SUBTYPE, but in a form used in some DDL statements.
 10. PARAM_PRECISION   fldINT32
      The size of the parameter type
      (number of characters in a string, bytes in a bytes field,
      significant digits in a BCD value, members of an ADT, and so on)
 11. PARAM_SCALE       fldINT16
       The number of digits to the right of the decimal on BCD values,
       or descendants on ADT and array values.
 12. PARAM_LENGTH      fldINT32
       The number of bytes required to store parameter values.
 13. PARAM_NULLABLE    fldINT16
       0 if the parameter requires a value, nonzero if it can be blank.
 {+2.01}
 { Vadim V.Lopushansky: add support parameter position.
   For an example look: ($DELPHI$)\Demos\Db\DbxExplorer\dbxexplorer.dpr (Read PARAM_POSITION error).
 14. PARAM_POSITION    fldINT16
       The position of the param in its procedure.
 {/+2.01}
{
ODBC result set columns from SQLProcedureColumns
 1.  PROCEDURE_CAT        Varchar
       Procedure catalog name; NULL if not applicable to the data source.
 2.  PROCEDURE_SCHEM      Varchar
       Procedure schema name; NULL if not applicable to the data source.
 3.  PROCEDURE_NAME       Varchar not NULL
       Procedure name. An empty string is returned for a procedure
       that does not have a name.
 4.  COLUMN_NAME          Varchar not NULL
       Procedure column name. The driver returns an empty string for
       a procedure column that does not have a name.
 5.  COLUMN_TYPE          Smallint not NULL
       Defines the procedure column as a parameter or a result set column:
       SQL_PARAM_TYPE_UNKNOWN: The procedure column is a parameter whose type is unknown
       SQL_PARAM_INPUT:        The procedure column is an input parameter
       SQL_PARAM_INPUT_OUTPUT: The procedure column is an input/output parameter
       SQL_PARAM_OUTPUT:       The procedure column is an output parameter
       SQL_RETURN_VALUE:       The procedure column is the return value of the procedure
       SQL_RESULT_COL:         The procedure column is a result set column
 6.  DATA_TYPE            Smallint not NULL
       SQL data type. This can be an ODBC SQL data type or a driver-specific SQL data type.
       For datetime and interval data types, this column returns the concise
       data types (for example, SQL_TYPE_TIME or SQL_INTERVAL_YEAR_TO_MONTH)
 7.  TYPE_NAME            Varchar not NULL
       Data source – dependent data type name
 8.  COLUMN_SIZE          Integer
       The column size of the procedure column on the data source.
       NULL is returned for data types where column size is not applicable.
       For more information concerning precision, see 'Column Size, Decimal
       Digits, Transfer Octet Length, and Display Size,' in Appendix D, 'Data Types.'
 9.  BUFFER_LENGTH        Integer
      The length in bytes of data transferred on an SQLGetData or SQLFetch
      operation if SQL_C_DEFAULT is specified.
      For numeric data, this size may be different than the size of the data
      stored on the data source.
      For more information concerning precision, see 'Column Size, Decimal
      Digits, Transfer Octet Length, and Display Size,' in Appendix D, 'Data Types.'
 10. DECIMAL_DIGITS       Smallint
      The decimal digits of the procedure column on the data source.
      NULL is returned for data types where decimal digits is not applicable.
      For more information concerning decimal digits, see 'Column Size, Decimal
      Digits, Transfer Octet Length, and Display Size,' in Appendix D, 'Data Types.'
 11. NUM_PREC_RADIX       Smallint
      For numeric data types, either 10 or 2.
      If it is 10, the values in COLUMN_SIZE and DECIMAL_DIGITS give the number
      of decimal digits allowed for the column.
      For example, a DECIMAL(12,5) column would return a NUM_PREC_RADIX of 10,
      a COLUMN_SIZE of 12, and a DECIMAL_DIGITS of 5;
      a FLOAT column could return a NUM_PREC_RADIX of 10, a COLUMN_SIZE of 15
      and a DECIMAL_DIGITS of NULL.
      If it is 2, the values in COLUMN_SIZE and DECIMAL_DIGITS give the number
      of bits allowed in the column.
      For example, a FLOAT column could return a NUM_PREC_RADIX of 2,
      a COLUMN_SIZE of 53, and a DECIMAL_DIGITS of NULL.
      NULL is returned for data types where NUM_PREC_RADIX is not applicable.
 12.NULLABLE             Smallint not NULL
     Whether the procedure column accepts a NULL value:
     SQL_NO_NULLS: The procedure column does not accept NULL values.
     SQL_NULLABLE: The procedure column accepts NULL values.
     SQL_NULLABLE_UNKNOWN: It is not known if the procedure column accepts NULL values.
 13.REMARKS              Varchar
      A description of the procedure column.
 14.COLUMN_DEF           Varchar
     The default value of the column.
     If NULL was specified as the default value, then this column is
     the word NULL, not enclosed in quotation marks.
     If the default value cannot be represented without truncation, then this
     column contains TRUNCATED, with no enclosing single quotation marks.
     If no default value was specified, then this column is NULL.
     The value of COLUMN_DEF can be used in generating a new column definition,
     except when it contains the value TRUNCATED.
 15.SQL_DATA_TYPE        Smallint not NULL
      The value of the SQL data type as it appears in the SQL_DESC_TYPE field
      of the descriptor.
      This column is the same as the DATA_TYPE column, except for datetime and
      interval data types.
      For datetime and interval data types, the SQL_DATA_TYPE field in the
      result set will return SQL_INTERVAL or SQL_DATETIME,
      and the SQL_DATETIME_SUB field will return the subcode for the
      specific interval or datetime data type (see Appendix D, “Data Types”).
 16.SQL_DATETIME_SUB     Smallint
      The subtype code for datetime and interval data types.
      For other data types, this column returns a NULL.
 17.CHAR_OCTET_LENGTH    Integer
      The maximum length in bytes of a character or binary data type column.
      For all other data types, this column returns a NULL.
 18.ORDINAL_POSITION     Integer not NULL
     For input and output parameters, the ordinal position of the parameter
     in the procedure definition (in increasing parameter order, starting at 1).
     For a return value (if any), 0 is returned.
     For result-set columns, the ordinal position of the column in the result set,
     with the first column in the result set being number 1.
     If there are multiple result sets, column ordinal positions are returned in
     a driver-specific manner.
 19.IS_NULLABLE          Varchar
      'NO' if the column does not include NULLs.
      'YES' if the column can include NULLs.
      This column returns a zero-length string if nullability is unknown.
      ISO rules are followed to determine nullability.
      An ISO SQL – compliant DBMS cannot return an empty string.
      The value returned for this column is different from the value returned
      for the NULLABLE column. (See the description of the NULLABLE column.)
}

const
  {+2.01}
  //Vadim V.Lopushansky: The support of PARAM_POSITION is supplemented
  ProcedureParamColumnNames: array [1..14] of string =
   ('RECNO',      'CATALOG_NAME',   'SCHEMA_NAME',   'PROC_NAME',      'PARAM_NAME',
    'PARAM_TYPE', 'PARAM_DATATYPE', 'PARAM_SUBTYPE', 'PARAM_TYPENAME', 'PARAM_PRECISION',
    'PARAM_SCALE', 'PARAM_LENGTH', 'PARAM_NULLABLE', 'PARAM_POSITION');
  ProcedureParamColumnTypes: array [1..14] of word =
    (fldINT32, fldZSTRING, fldZSTRING, fldZSTRING, fldZSTRING,
     fldINT16, fldINT16,   fldINT16,   fldZSTRING, fldINT32,
     fldINT16, fldINT32,   fldINT16, fldINT16);
   {/+2.01}
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
  {+2.01}
  v_DECIMAL_DIGITS: SmallInt;
  cbv_DECIMAL_DIGITS: Integer;
  v_NUM_PREC_RADIX: Smallint;
  cbv_NUM_PREC_RADIX: Integer;
  v_COLUMN_SIZE: Integer;
  cbv_COLUMN_SIZE: Integer;
  v_CHAR_OCTET_LENGTH: Integer;
  cbv_CHAR_OCTET_LENGTH: Integer;
  v_BUFFER_LENGTH: Integer;
  cbv_BUFFER_LENGTH: Integer;
  {/+2.01}
  OdbcNullable: SmallInt;

  cbCat: integer;
  cbSchema: integer;
  cbProcName: integer;
  cbProcColumnName: integer;
  cbTypeName: integer;
  {+2.01}
  //cbScale: integer;
  {/+2.01}
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
  {+2.01}//Vadim V.Lopushansky: Reading of the information about types of parameters
  //BindSmallInt(10, Scale, @cbScale);
  BindInteger(8, v_COLUMN_SIZE, @cbv_COLUMN_SIZE);
  BindInteger(9, v_BUFFER_LENGTH, @cbv_BUFFER_LENGTH);
  BindSmallInt(10, v_DECIMAL_DIGITS, @cbv_DECIMAL_DIGITS);
  BindSmallInt(11, v_NUM_PREC_RADIX, @cbv_NUM_PREC_RADIX);
  BindInteger(17, v_CHAR_OCTET_LENGTH, @cbv_CHAR_OCTET_LENGTH);
  v_DECIMAL_DIGITS    := 0;
  v_NUM_PREC_RADIX    := 0;
  v_COLUMN_SIZE       := 0;
  v_CHAR_OCTET_LENGTH := 0;
  {/+2.01}
  BindSmallInt(12, OdbcNullable, nil); // NULLABLE
  BindInteger(18, OrdinalPosition, @cbOrdinalPosition);

  fProcList:= TList.Create;
  fProcColumnList:= TList.Create;

  OdbcRetCode := SQLFetch(fhStmt);

  while (OdbcRetCode <> ODBCapi.SQL_NO_DATA) do
    begin

    OdbcCheck(OdbcRetCode, 'SQLFetch');
    {+2.01}
    //Vadim V.Lopushansky: The code for drivers which not supporting filter
    // (Easysoft IB6 ODBC Driver [ver:1.00.01.67] contain its error).
    // Edward> Again, I don't think a real dbxpress application will use filter,
    // Edward> but I leave the code, as it is correct
    if Assigned(SearchParamName) then
      i := StrLen(SearchParamName)
    else
      i := 0;
    if (i > 0) and ((i <> Integer(StrLen(ProcColumnName))) or
      (StrLComp(SearchParamName, ProcColumnName, i) <> 0)) then
      begin
      OdbcRetCode := SQLFetch(fhStmt);
      continue;
      end;
    {/+2.01}

    if (ColumnType <> SQL_RESULT_COL) then
      begin
      aMetaProcedure := TMetaProcedure.Create(Cat, Schema, ProcName, 0);
      fProcList.Add(aMetaProcedure);
      aMetaProcedureParam := TMetaProcedureParam.Create(ProcColumnName);
      fProcColumnList.Add(aMetaProcedureParam);
      {+2.01}
      //Vadim V.Lopushansky: Correction to reference from ProcedureParam to Procedure
      aMetaProcedureParam.fMetaProcedure := aMetaProcedure;
      {/+2.01}
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
        {+2.01}
        //Vadim V.Lopushansky: Calculating metadata:
        if (cbv_BUFFER_LENGTH = OdbcAPi.SQL_NULL_DATA) then
          aMetaProcedureParam.fLength := Low(Integer) // this indicates null data
        else
          aMetaProcedureParam.fLength := v_BUFFER_LENGTH;

        if cbv_DECIMAL_DIGITS = OdbcAPi.SQL_NULL_DATA then
          aMetaProcedureParam.fScale := Low(SmallInt) // this indicates null data
        else
          aMetaProcedureParam.fScale := v_DECIMAL_DIGITS;

        if cbv_COLUMN_SIZE = OdbcAPi.SQL_NULL_DATA then
          aMetaProcedureParam.fPrecision := Low(Integer)  // this indicates null data
        else
          begin
          if (cbv_NUM_PREC_RADIX <> OdbcAPi.SQL_NULL_DATA) and (v_NUM_PREC_RADIX = 2) then
            aMetaProcedureParam.fPrecision := ((v_COLUMN_SIZE * 3) div 10) + 1
          else
            aMetaProcedureParam.fPrecision := v_COLUMN_SIZE
          end;
        {/+2.01}
      OdbcDataTypeToDbxType(OdbcDataType, DbxDataType, DbxDataSubType);
      aMetaProcedureParam.fDataType := DbxDataType;
      aMetaProcedureParam.fDataSubtype := DbxDataSubType;
      aMetaProcedureParam.fDataTypeName := AllocMem(strLen(TypeName) + 1);
      {+2.01}
      //ORIGINAL CODE:
      //StrCopy(TypeName, aMetaProcedureParam.fDataTypeName);
      StrCopy(aMetaProcedureParam.fDataTypeName, TypeName);
      {/+2.01}
      if (OdbcNullable <> SQL_NULLABLE) then
         aMetaProcedureParam.fNullable := 0 // Requires a value
      else
         aMetaProcedureParam.fNullable := 1; // Does not require a value
      {+2.01}
      //Vadim V.Lopushansky: add support of PARAM_POSITION
      aMetaProcedureParam.fPosition := OrdinalPosition;
      {/+2.01}
      end;
    {+2.01}
    v_DECIMAL_DIGITS    := 0;
    v_NUM_PREC_RADIX    := 0;
    v_COLUMN_SIZE       := 0;
    v_CHAR_OCTET_LENGTH := 0;
    v_BUFFER_LENGTH     := 0;
    {/+2.01}
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

  if (fhStmt <> SQL_NULL_HANDLE) then
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
    {+2.01}//Vadim V.Lopushansky: add support of PARAM_POSITION
    14:  // PARAM_POSITION
      pLength := SizeOf(SmallInt);
    {/+2.01}
  else
    begin
    raise EDbxInvalidCall.Create(
      'TSqlCursorMetaDataProcedureParams.getColumnLength invalid column no: '
      + IntToStr(ColumnNumber));
    end;
  end;
  {+2.01}
  //Vadim V.Lopushansky: if Length is Zero then columns will be hidden (unbinded). Look SqlExpr.pas ...
  // Edward> Same thing again. But you look like you know what you are doing, so I have kept it.
  if (pLength = 0) and (ProcedureParamColumnTypes[ColumnNumber] = fldZSTRING) then
    pLength := 1;
  {/+2.01}
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
  {+2.01}
  //Vadim V.Lopushansky: Problems with loss of accuracy at type conversion
  // Edward> ???Ed>Vad: SqlProcedureColumns should never return such a long column
  if Length < LongWord(High(SmallInt)) then
    piPrecision := Length
  else
    piPrecision := -1;
  {/+2.01}
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
      'TSqlCursorMetaDataProcedures.getLong invalid column no: '
      + IntToStr(ColumnNumber));
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
    {+2.01}
    //Vadim V.Lopushansky: add support of PARAM_POSITION
    14:  // PARAM_POSITION
      begin
      SmallInt(Value^) := fMetaProcedureParamCurrent.fPosition;
      IsBlank          := False;
      end;
    {/+2.01}
  else
    begin
    raise EDbxInvalidCall.Create(
      'TSqlCursorMetaDataProcedures.getShort invalid column no: '
      + IntToStr(ColumnNumber));
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
      {+2.01}// AV if fCat=nil
      IsBlank := fMetaProcedureParamCurrent.fMetaProcedure.fCat = nil;
      if not IsBlank then
        StrCopy(Value, fMetaProcedureParamCurrent.fMetaProcedure.fCat);
      {/+2.01}
      end;
    3:   // SCHEMA_NAME
      begin
      {+2.01}// AV if fSchema=nil
      IsBlank := fMetaProcedureParamCurrent.fMetaProcedure.fSchema = nil;
      if not IsBlank then
        StrCopy(Value, fMetaProcedureParamCurrent.fMetaProcedure.fSchema);
      {/+2.01}
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
      'TSqlCursorMetaDataProcedures.getString invalid column no: '
      + IntToStr(ColumnNumber));
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
  {+2.01}
  if Assigned(fProcColumnList) and (fRowNo <= fProcColumnList.Count) then
  {/+2.01}
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
