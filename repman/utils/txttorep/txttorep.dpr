{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       reptotxt                                        }
{                                                       }
{       Converts a text to report definition format     }
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

program txttorep;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
{$IFDEF MSWINDOWS}
  rpwriter in '..\..\..\rpwriter.pas',
  rpconsts in '..\..\..\rpconsts.pas';
{$ENDIF}

{$IFDEF LINUX}
  rpwriter in '../../../rpwriter.pas',
  rpconsts in '../../../rpconsts.pas';
{$ENDIF}

procedure PrintHelp;
begin
 Writeln(SRpTxtToRep1);
 Writeln(SRpTxtToRep2);
 Writeln(SRpTxtToRep3);
end;

begin
  { TODO -oUser -cConsole Main : Insert code here }
  if ParamCount<>2 then
   PrintHelp
  else
   PlainTextToFileReport(ParamStr(1),ParamStr(2));
end.
