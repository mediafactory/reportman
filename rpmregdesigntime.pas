{*******************************************************}
{                                                       }
{       Rpmregdesigntime                                }
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

unit rpmregdesigntime;

interface

{$I rpconf.inc}

uses
  Classes,rpcompobase,rpdatainfo,rpeval,
 {$IFNDEF USEVARIANTS}
   DsgnIntf,
 {$ENDIF}
 {$IFNDEF DOTNETDBUGS}
  rpmdesigneditors,
  rpeditalias,rpwebmetaclient,
 {$ENDIF}
 {$IFDEF USEVARIANTS}
  {$IFNDEF DOTNETDBUGS}
    DesignIntf,
  {$ENDIF}
 {$ENDIF}
  rpalias;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Reportman', [TRpWebMetaPrint]);
  RegisterPropertyEditor(TypeInfo(String),TCBaseReport,'ConnectionName',TRpReportLibNamePropEditor);
  RegisterPropertyEditor(TypeInfo(String),TCBaseReport,'ReportName',TRpReportNamePropEditor);
  RegisterPropertyEditor(TypeInfo(TRpDatabaseInfoList),TRpAlias,'',TRpConnectionPropEditor);
  RegisterPropertyEditor(TypeInfo(String),TRpCustomEvaluator,'Expression',TRpExpressionPropEditor);
  RegisterComponentEditor(TCBaseReport,TRpBaseComponentEditor);
  RegisterComponentEditor(TRpCustomEvaluator,TRpEvalComponentEditor);
{$IFNDEF USEVARIANTS}
  RegisterPropertyEditor(TypeInfo(TRpAliasList),TRpAlias,'',TRpAliasPropEditor);
{$ENDIF}
end;

end.
