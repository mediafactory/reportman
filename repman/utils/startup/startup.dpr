{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       startup                                         }
{                                                       }
{                                                       }
{       This application starts a Borland application   }
{       setting library path and LC_NUMERIC (knownbug)  }
{       so it will work without the need of a startup   }
{       script. It replaces the startup script          }
{       It has .bin extension and assumes the executable}
{       has the same name without extension             }
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

program startup;

{$APPTYPE CONSOLE}

uses
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  SysUtils;

{$E bin}

{$IFDEF LINUX}
procedure SetEnvValue(varname:string;varvalue:string;addvalue:boolean);
var
 avalue:string;
begin
 if addvalue then
 begin
  avalue:=GetEnvironmentVariable(varname);
  if varname='LD_LIBRARY_PATH' then
  begin
   if Length(avalue)>0 then
    avalue:='/opt/kylixlibs'+':'+avalue
   else
    avalue:='/opt/kylixlibs';
  end;
  if Length(avalue)>0 then
   avalue:=varvalue+':'+avalue
  else
   avalue:=varvalue;
  varvalue:=avalue;
 end;
ls
 setenv(PChar(varname),PChar(varvalue),1);
end;

procedure CopyArgStr(var dest:PChar;source:string);
var
 len:Cardinal;
begin
 len:=Length(source)+1;
 GetMem(dest,len);
 StrLCopy(dest,PChar(source),len);
end;

procedure ExecuteApp(appname:string);
type
 parray=array[0..0] of PChar;
 pparray=^parray;
var
 argv_size:size_t;
 argv:pparray;
begin
 argv_size:=(ArgCount+1)*SizeOf(PChar);
 GetMem(argv,argv_size);
 memcpy(argv,ArgValues,argv_size);
 CopyArgStr(argv[0],appname);
 execv(PChar(appname),PPChar(argv));
end;
{$ENDIF}

{$IFDEF LINUX}
var
 fullpath:string;
 targetdir,targetapp:string;
{$ENDIF}
begin
  { TODO -oUser -cConsole Main : Insert code here }
{$IFDEF LINUX}
  // Extract the filename
  fullpath:=ExpandFileName(ParamStr(0));
  // Append to PATH variable
  targetdir:=ExtractFileDir(fullpath);
  targetapp:=ChangeFileExt(fullpath,'');
  SetEnvValue('LC_NUMERIC','en_US',false);
  SetEnvValue('KYLIX_DEFINEDENVLOCALES','Yes',false);
  SetEnvValue('KYLIX_DECIMAL_SEPARATOR',DecimalSeparator,false);
  SetEnvValue('KYLIX_THOUSAND_SEPARATOR',ThousandSeparator,false);
  SetEnvValue('KYLIX_DATE_SEPARATOR',DateSeparator,false);
  SetEnvValue('KYLIX_TIME_SEPARATOR',TimeSeparator,false);
  SetEnvValue('LD_LIBRARY_PATH',targetdir,true);
  SetEnvValue('PATH',targetdir,true);
  ExecuteApp(targetapp);
  // Not succefull
  Writeln(ErrOutput,'Error: Unable to execute '+targetapp);
  Writeln(ErrOutput,Format('code=%d msg=%s',[errno,strerror(errno)]));
{$ENDIF}
{$IFDEF MSWINDOWS}
  Writeln('This application is useful only in Linux');
{$ENDIF}
end.
