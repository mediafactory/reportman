{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpmdshfolder                                    }
{                                                       }
{                                                       }
{       An interface to user and system config files    }
{       to store information with a Windows 2000        }
{       compliant procedure (and LSB in Linux)          }
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

unit rpmdshfolder;

interface

{$I rpconf.inc}

uses
  SysUtils, Classes,
{$IFDEF LINUX}
  Libc,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFNDEF USEVARIANTS}
{$IFNDEF FPC}
  FileCtrl,
{$ENDIF}
{$ENDIF}
{$IFDEF DOTNETD}
  System.IO,
{$ENDIF}
  rpmdconsts;

{$IFDEF MSWINDOWS}
const
{$IFNDEF DOTNETD}
  {$EXTERNALSYM CSIDL_APPDATA}
  CSIDL_APPDATA              = $001A;  // Application Data, new for NT4
  {$EXTERNALSYM CSIDL_FLAG_CREATE}
  CSIDL_FLAG_CREATE          = $8000;  // new for Win2K, or this in to force
  {$EXTERNALSYM CSIDL_LOCAL_APPDATA}
  CSIDL_LOCAL_APPDATA        = $001C;  // non roaming,
  {$EXTERNALSYM CSIDL_COMMON_APPDATA}
  CSIDL_COMMON_APPDATA       = $0023;  // All Users\Application Data
{$ENDIF}

  shlwapi32 = 'shlwapi.dll';
  shfolder  = 'shfolder.dll';
{$ENDIF}
{$IFDEF LINUX}
const
 DIR_SEPARATOR='/';
{$ENDIF}
{$IFDEF MSWINDOWS}
 DIR_SEPARATOR='\';
{$ENDIF}


  function Obtainininameuserconfig (company, product, filename:string):string;
  function Obtainininamelocalconfig (company, product, filename:string):string;
  function Obtainininamecommonconfig (company, product, filename:string):string;
  function GetTheSystemDirectory:String;
{$IFDEF MSWINDOWS}
{$IFNDEF BUILDER4}
{$IFNDEF DOTNETD}
{$EXTERNALSYM SHGetFolderPath}
{$IFNDEF FPC}
function SHGetFolderPath(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PChar): HResult; stdcall;
function SHGetFolderPath; external shfolder name 'SHGetFolderPathA';
{$ENDIF}
{$IFDEF FPC}
function SHGetFolderPath(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PChar): HResult; stdcall; external shfolder name 'SHGetFolderPathA';
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$IFDEF BUILDER4}
{$IFNDEF DOTNETD}
var SHGetFolderPath:function (hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PChar): HResult; stdcall;
var HandleLib:THandle;
{$ENDIF}
{$ENDIF}

{$IFNDEF DOTNETD}
{$EXTERNALSYM PathAppend}
{$IFNDEF FPC}
function PathAppend(pszPath: PChar; pMore: PChar): BOOL; stdcall;
function PathAppend; external shlwapi32 name 'PathAppendA';
{$ENDIF}
{$IFDEF FPC}
function PathAppend(pszPath: PChar; pMore: PChar): BOOL; stdcall;external shlwapi32 name 'PathAppendA';
{$ENDIF}
{$ENDIF}
{$ENDIF}

implementation

uses rptypes;

{$IFNDEF DOTNETD}
function Obtainininameuserconfig(company,product,filename:string):string;
var
 szAppData:array [0..MAX_PATH] of char;
{$IFDEF LINUX}
 ap:PCHar;
{$ENDIF}
begin
{$IFDEF LINUX}
 ap:=getenv(Pchar('HOME'));
 if assigned(ap) then
 begin
  StrPCopy(szAppdata,ap);
  Result:=StrPas(szAppdata)+'/.'
 end
 else
  Result:='./.';
 if length(company)>0 then
  Result:=Result+company+'.';
 if length(product)>0 then
  Result:=Result+product+'.';
 Result:=Result+filename;
{$ENDIF}
{$IFDEF MSWINDOWS}
 if length(filename)<1 then
  Raise Exception.Create(SRpFileNameRequired);
 SHGetFolderPath(0, CSIDL_APPDATA or CSIDL_FLAG_CREATE, 0, 0, szAppData);
 if length(company)>0 then
 begin
  if not PathAppend(szAppdata,Pchar(company)) then
   RaiseLastOSError;
 end;
 if Length(product)>0 then
 begin
  if not PathAppend(szAppdata,Pchar(product)) then
   RaiseLastOSError;
 end;
 Result:=StrPas(szAppdata);
 if Not DirectoryExists(Result) then
 begin
{$IFDEF BUILDER4}
 ForceDirectories(Result);
{$ENDIF}
{$IFNDEF BUILDER4}
 try
  if not ForceDirectories(Result) then
   Result:='';
 except
   Result:='';
 end;
{$ENDIF}
 end;
 if not PathAppend(szAppdata,Pchar(filename+'.ini')) then
    RaiseLastOSError;
 Result:=StrPas(szAppdata);
{$ENDIF}
end;


function Obtainininamelocalconfig(company,product,filename:string):string;
{$IFDEF MSWINDOWS}
var
 szAppData:array [0..MAX_PATH] of char;
{$ENDIF}
begin
{$IFDEF LINUX}
 Result:=Obtainininameuserconfig(company,product+'etc',filename);
{$ENDIF}
{$IFDEF MSWINDOWS}
 if length(filename)<1 then
  Raise Exception.Create(SRpFileNameRequired);
 SHGetFolderPath(0, CSIDL_LOCAL_APPDATA or CSIDL_FLAG_CREATE, 0, 0, szAppData);

 if length(company)>0 then
 begin
  if not PathAppend(szAppdata,Pchar(company)) then
   RaiseLastOSError;
 end;
 if Length(product)>0 then
 begin
  if not PathAppend(szAppdata,Pchar(product)) then
    RaiseLastOSError;
 end;
 Result:=StrPas(szAppdata);
 if Not DirectoryExists(Result) then
 begin
{$IFDEF BUILDER4}
 ForceDirectories(Result);
{$ENDIF}
{$IFNDEF BUILDER4}
 try
  if not ForceDirectories(Result) then
   Result:='';
 except
   Result:='';
 end;
{$ENDIF}
 end;
 if not PathAppend(szAppdata,Pchar(filename+'.ini')) then
   RaiseLastOSError;
 Result:=StrPas(szAppdata);
{$ENDIF}
end;


function Obtainininamecommonconfig(company,product,filename:string):string;
{$IFDEF MSWINDOWS}
var
 szAppData:array [0..MAX_PATH] of char;
{$ENDIF}
begin
{$IFDEF LINUX}
 Result:='/etc/'+company+product+filename;
{$ENDIF}
{$IFDEF MSWINDOWS}
 if length(filename)<1 then
  Raise Exception.Create(SRpFileNameRequired);
 SHGetFolderPath(0, CSIDL_COMMON_APPDATA or CSIDL_FLAG_CREATE, 0, 0, szAppData);
 if length(company)>0 then
 begin
  if not PathAppend(szAppdata,Pchar(company)) then
    RaiseLastOSError;
 end;
 if Length(product)>0 then
 begin
  if not PathAppend(szAppdata,Pchar(product)) then
    RaiseLastOSError;
 end;
 Result:=StrPas(szAppdata);
 if Not DirectoryExists(Result) then
 begin
{$IFDEF BUILDER4}
 ForceDirectories(Result);
{$ENDIF}
{$IFNDEF BUILDER4}
 try
  if not ForceDirectories(Result) then
   Result:='';
 except
   Result:='';
 end;
{$ENDIF}
 end;
 if not PathAppend(szAppdata,Pchar(filename+'.ini')) then
    RaiseLastOSError;
 Result:=StrPas(szAppdata);
{$ENDIF}
end;

function GetTheSystemDirectory:String;
{$IFDEF MSWINDOWS}
var
 pbuf:PChar;
 asize:Integer;
{$ENDIF}
begin
{$IFDEF LINUX}
 Result:='/lib';
{$ENDIF}
{$IFDEF MSWINDOWS}
 pbuf:=AllocMem(MAX_PATH+1);
 try
  asize:=GetSystemDirectory(pbuf,MAX_PATH);
  if asize=0 then
   RaiseLastOsError;
  if asize>MAX_PATH then
  begin
   asize:=GetSystemDirectory(pbuf,MAX_PATH);
   if asize=0 then
    RaiseLastOsError;
  end;
  result:=StrPas(pbuf);
 finally
  FreeMem(pbuf);
 end;
{$ENDIF}
end;
{$ENDIF}

{$IFDEF DOTNETD}
function Obtainininameuserconfig(company,product,filename:string):string;
begin
 Result:=Environment.GetFolderPath (System.Environment.SpecialFolder.ApplicationData);
 if length(filename)<1 then
  Raise Exception.Create(SRpFileNameRequired);
 if length(company)>0 then
  Result:=System.IO.Path.Combine(Result,company);
 if Length(product)>0 then
 begin
  Result:=System.IO.Path.Combine(Result,product);
 end;
 if Not System.IO.Directory.Exists(Result) then
 begin
   System.IO.Directory.CreateDirectory(Result);
   if Not System.IO.Directory.Exists(Result) then
    Result:='';
 end;
 if Length(Result)>0 then
  Result:=System.IO.Path.Combine(Result,filename+'.ini');
end;


function Obtainininamelocalconfig(company,product,filename:string):string;
begin
 Result:=Environment.GetFolderPath (System.Environment.SpecialFolder.LocalApplicationData);
 if length(filename)<1 then
  Raise Exception.Create(SRpFileNameRequired);
 if length(company)>0 then
  Result:=System.IO.Path.Combine(Result,company);
 if Length(product)>0 then
 begin
  Result:=System.IO.Path.Combine(Result,product);
 end;
 if Not System.IO.Directory.Exists(Result) then
 begin
   System.IO.Directory.CreateDirectory(Result);
   if Not System.IO.Directory.Exists(Result) then
    Result:='';
 end;
 if Length(Result)>0 then
  Result:=System.IO.Path.Combine(Result,filename+'.ini');
end;


function Obtainininamecommonconfig(company,product,filename:string):string;
begin
 Result:=Environment.GetFolderPath (System.Environment.SpecialFolder.CommonApplicationData);
 if length(filename)<1 then
  Raise Exception.Create(SRpFileNameRequired);
 if length(company)>0 then
  Result:=System.IO.Path.Combine(Result,company);
 if Length(product)>0 then
 begin
  Result:=System.IO.Path.Combine(Result,product);
 end;
 if Not System.IO.Directory.Exists(Result) then
 begin
   System.IO.Directory.CreateDirectory(Result);
   if Not System.IO.Directory.Exists(Result) then
    Result:='';
 end;
 if Length(Result)>0 then
  Result:=System.IO.Path.Combine(Result,filename+'.ini');
end;

function GetTheSystemDirectory:String;
begin
 Result:=Environment.GetFolderPath (System.Environment.SpecialFolder.System);
end;

{$ENDIF}


{$IFDEF BUILDER4}
initialization

HandleLib:=LoadLibrary(shfolder);
if HandleLib=0 then
 RaiseLastOSError;
SHGetFolderPath:=GetProcAddress(HandleLib,PChar('SHGetFolderPathA'));
if Not Assigned(SHGetFolderPath) then
 RaiseLastOSError;

finalization

if HandleLib<>0 then
 FreeLibrary(HandleLib);
{$ENDIF}
end.











