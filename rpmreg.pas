{*******************************************************}
{                                                       }
{       Rpmreg                                          }
{                                                       }
{       Units that registers the reportmanager engine   }
{       into the Delphi component palette               }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{*******************************************************}

unit rpmreg;

interface

{$I rpconf.inc}

uses
  Classes,
  rpparser,rpeval,rpreport,rppdfreport,
{$IFDEF USEVCL}
  rpvclreport,
{$ENDIF}
  rpevalfunc,rpalias,rptypeval,rplastsav;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Reportman', [TRpEvaluator]);
  RegisterComponents('Reportman', [TRpAlias]);
  RegisterComponents('Reportman', [TRpLastUsedStrings]);
  RegisterComponents('Reportman', [TPDFReport]);
{$IFDEF USEVCL}
  RegisterComponents('Reportman', [TVCLReport]);
{$ENDIF}
end;

end.
