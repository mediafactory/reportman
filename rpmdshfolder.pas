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
  FileCtrl,
{$ENDIF}
  rpmdconsts;

{$IFDEF MSWINDOWS}
const
  {$EXTERNALSYM CSIDL_APPDATA}
  CSIDL_APPDATA              = $001A;  // Application Data, new for NT4
  {$EXTERNALSYM CSIDL_FLAG_CREATE}
  CSIDL_FLAG_CREATE          = $8000;  // new for Win2K, or this in to force
  {$EXTERNALSYM CSIDL_LOCAL_APPDATA}
  CSIDL_LOCAL_APPDATA        = $001C;  // non roaming,
  {$EXTERNALSYM CSIDL_COMMON_APPDATA}
  CSIDL_COMMON_APPDATA       = $0023;  // All Users\Application Data

  shlwapi32 = 'shlwapi.dll';
  shfolder  = 'shfolder.dll';
{$ENDIF}


  function Obtainininameuserconfig(company,product,filename:string):string;
  function Obtainininamelocalconfig(company,product,filename:string):string;
  function Obtainininamecommonconfig(company,product,filename:string):string;

{$IFDEF MSWINDOWS}
{$EXTERNALSYM SHGetFolderPath}
function SHGetFolderPath(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PChar): HResult; stdcall;
function SHGetFolderPath; external shfolder name 'SHGetFolderPathA';
{$EXTERNALSYM PathAppend}
function PathAppend(pszPath: PChar; pMore: PChar): BOOL; stdcall;
function PathAppend; external shlwapi32 name 'PathAppendA';
{$ENDIF}

implementation



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
{$IFDEF USEVARIANTS}
     RaiseLastOSError;
{$ELSE}
     RaiseLastWin32Error;
{$ENDIF}
 end;
 if Length(product)>0 then
 begin
  if not PathAppend(szAppdata,Pchar(product)) then
{$IFDEF USEVARIANTS}
     RaiseLastOSError;
{$ELSE}
     RaiseLastWin32Error;
{$ENDIF}
 end;
 Result:=StrPas(szAppdata);
 if Not DirectoryExists(Result) then
 begin
  try
   if not CreateDirectory(Pchar(Result),nil) then
  except
   Result:='';
  end;
 end;
 if not PathAppend(szAppdata,Pchar(filename+'.ini')) then
{$IFDEF USEVARIANTS}
     RaiseLastOSError;
{$ELSE}
     RaiseLastWin32Error;
{$ENDIF}
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
 Result:='/etc/'+company+product+filename;
{$ENDIF}
{$IFDEF MSWINDOWS}
 if length(filename)<1 then
  Raise Exception.Create(SRpFileNameRequired);
 SHGetFolderPath(0, CSIDL_LOCAL_APPDATA or CSIDL_FLAG_CREATE, 0, 0, szAppData);

 if length(company)>0 then
 begin
  if not PathAppend(szAppdata,Pchar(company)) then
{$IFDEF USEVARIANTS}
     RaiseLastOSError;
{$ELSE}
     RaiseLastWin32Error;
{$ENDIF}
 end;
 if Length(product)>0 then
 begin
  if not PathAppend(szAppdata,Pchar(product)) then
{$IFDEF USEVARIANTS}
     RaiseLastOSError;
{$ELSE}
     RaiseLastWin32Error;
{$ENDIF}
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
{$IFDEF USEVARIANTS}
     RaiseLastOSError;
{$ELSE}
     RaiseLastWin32Error;
{$ENDIF}
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
{$IFDEF USEVARIANTS}
     RaiseLastOSError;
{$ELSE}
     RaiseLastWin32Error;
{$ENDIF}
 end;
 if Length(product)>0 then
 begin
  if not PathAppend(szAppdata,Pchar(product)) then
{$IFDEF USEVARIANTS}
     RaiseLastOSError;
{$ELSE}
     RaiseLastWin32Error;
{$ENDIF}
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
{$IFDEF USEVARIANTS}
     RaiseLastOSError;
{$ELSE}
     RaiseLastWin32Error;
{$ENDIF}
 Result:=StrPas(szAppdata);
{$ENDIF}
end;

initialization


end.




