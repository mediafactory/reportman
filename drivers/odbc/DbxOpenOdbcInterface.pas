{
  Kylix / Delphi open source DbExpress driver for ODBC
  Version 2.03, 2002-11-20

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
unit DbxOpenOdbcInterface;

interface

uses
  Classes;

type
  TOdbcDriverType = (eOdbcDriverTypeUnspecified,
   eOdbcDriverTypeGupta, eOdbcDriverTypeMsSqlServer, eOdbcDriverTypeIbmDb2,
   eOdbcDriverTypeMsJet,
   eOdbcDriverTypeMySql, eOdbcDriverTypeMySql3,
   eOdbcDriverTypeInterbase, eOdbcDriverTypeInformix,
   eOdbcDriverTypeOracle, eOdbcDriverTypeSybase,
   eOdbcDriverTypeSQLLite, eOdbcDriverTypeThinkSQL, eOdbcDriverTypeMerantOle
   );

  TDbmsType = (eDbmsTypeUnspecified,
   eDbmsTypeGupta, eDbmsTypeMsSqlServer, eDbmsTypeIbmDb2,
   eDbmsTypeMySql, eDbmsTypeMySqlMax,
   eDbmsTypeMsAccess, eDbmsTypeExcel, eDbmsTypeText, eDbmsTypeDBase, eDbmsTypeParadox,
   eDbmsTypeOracle, eDbmsTypeInterbase, eDbmsTypeInformix, eDbmsTypeSybase,
   eDbmsTypeSQLLite, eDbmsTypeThinkSQL, eDbmsTypeSapDb
   );


{ ISqlConnectionOdbc interface  }
{
// ISqlConnectionOdbc introduces additional methods on ISqlConnection.
// Here is an example of how you can access this interface:
procedure OdbcInterfaceExample1(Conn: SqlConnection; Memo: TMemo);
var
  aSqlConnectionInterface: ISqlConnection;
  aSqlConnectionOdbcInterface: ISqlConnectionOdbc;
  aResult: HResult;
begin
  aSqlConnectionInterface := Conn.SqlConnection;
  aResult := aSqlConnectionInterface.QueryInterface(ISqlConnectionOdbc, aSqlConnectionOdbcInterface);
  if aResult = S_OK then
    aSqlConnectionOdbcInterface.GetConnectStrings(Memo.Lines);
end;

// If you have statically linked DbxOpenOdbc into your program,
// so you know SqlConnection will be implemented by TSqlConnectionOdbc,
// you can use "as" in place of "QueryInterface"
procedure OdbcInterfaceExample2(Conn: SqlConnection; Memo: TMemo);
var
  aSqlConnectionInterface: ISqlConnection;
  aSqlConnectionOdbcInterface: ISqlConnectionOdbc;
begin
  aSqlConnectionInterface := Conn.SqlConnection;
  aSqlConnectionOdbcInterface := aSqlConnectionInterface as ISqlConnectionOdbc;
  aSqlConnectionOdbcInterface.GetConnectStrings(Memo.Lines);
end;
}

  ISqlConnectionOdbc = interface
    ['{136DD9D1-9B9C-4355-9AEF-959662CB095E}']
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
    end;

implementation

end.
