{*******************************************************}
{                                                       }
{       Rpmreg                                          }
{                                                       }
{       Units that registers the reportmanager engine   }
{       into the Delphi component palette               }
{                                                       }
{       Copyright (c) 1994-2003 Toni Martir             }
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
  rpparser,rpeval,rpreport,rppdfreport,rptranslator,
{$IFDEF USEVCL}
  rpvclreport,
{$ENDIF}
{$IFNDEF USEVARIANTS}
{$IFNDEF BUILDER4}
  rprulervcl,rpmdesignervcl,
{$ENDIF}
  DsgnIntf,rpeditalias,rpdatainfo,
{$ENDIF}
  rpevalfunc,rpalias,rptypeval,rplastsav;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Reportman', [TRpEvaluator]);
  RegisterComponents('Reportman', [TRpAlias]);
  RegisterComponents('Reportman', [TRpLastUsedStrings]);
  RegisterComponents('Reportman', [TRpTranslator]);
  RegisterComponents('Reportman', [TPDFReport]);
{$IFDEF USEVCL}
  RegisterComponents('Reportman', [TVCLReport]);
  RegisterPropertyEditor(TypeInfo(TRpAliasList),TRpAlias,'',TRpAliasPropEditor);
  RegisterPropertyEditor(TypeInfo(TRpDatabaseInfoList),TRpAlias,'',TRpConnectionPropEditor);
{$ENDIF}
{$IFNDEF USEVARIANTS}
{$IFNDEF BUILDER4}
  RegisterComponents('Reportman', [TRpRulerVCL]);
  RegisterComponents('Reportman', [TRpDesignerVCL]);
{$ENDIF}
{$ENDIF}
end;

end.
