{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpshfolder                                      }
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

unit rpshfolder;

interface


uses
  SysUtils, Classes,
{$IFDEF LINUX}
  Libc,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,shfolder,shLWApi,
{$ENDIF}
  rpconsts;


  function Obtainininameuserconfig(company,product,filename:string):string;
  function Obtainininamelocalconfig(company,product,filename:string):string;
  function Obtainininamecommonconfig(company,product,filename:string):string;

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
  if not CreateDirectory(Pchar(Result),nil) then
   Result:='';
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
 Result:='/etc/'+company+product+filename;
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
  if not ForceDirectories(Result) then
   Result:='';
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
  if not ForceDirectories(Result) then
   Result:='';
 end;
 if not PathAppend(szAppdata,Pchar(filename+'.ini')) then
  RaiseLastOSError;
 Result:=StrPas(szAppdata);
{$ENDIF}
end;

initialization


end.
