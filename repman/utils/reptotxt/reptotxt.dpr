{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       reptotxt                                        }
{                                                       }
{       Converts a report definition to text            }
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

program reptotxt;

{$APPTYPE CONSOLE}

uses
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
 Writeln(SRpRepToTxt1);
 Writeln(SRpRepToTxt2);
 Writeln(SRpRepToTxt3);
end;

begin
  { TODO -oUser -cConsole Main : Insert code here }
  if ParamCount<>2 then
   PrintHelp
  else
   FileReportToPlainText(ParamStr(1),ParamStr(2));
end.
