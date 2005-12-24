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
  Classes,rpcompobase,
  rpparser,rpeval,rpreport,rppdfreport,rptranslator,
  rpevalfunc,rptypes,rpdatainfo,rpalias,rptypeval,
{$IFNDEF USEVARIANTS}
  rpvclreport,rpmaskedit,rppreviewcontrol,
  {$IFNDEF BUILDER4}
   rprulervcl,rpmdesignervcl,rpdbgridvcl,
  {$ENDIF}
   DsgnIntf,
{$ENDIF}
  rplastsav;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Reportman', [TRpEvaluator]);
  RegisterComponents('Reportman', [TRpAlias]);
  RegisterComponents('Reportman', [TRpLastUsedStrings]);
  RegisterComponents('Reportman', [TRpTranslator]);
  RegisterComponents('Reportman', [TPDFReport]);
{$IFNDEF USEVARIANTS}
  RegisterComponents('Reportman', [TVCLReport]);
  RegisterComponents('Reportman', [TRpPreviewControl]);
  RegisterComponents('Reportman', [TRpMaskEdit]);
  {$IFNDEF BUILDER4}
   RegisterComponents('Reportman', [TRpRulerVCL]);
   RegisterComponents('Reportman', [TRpDesignerVCL]);
   RegisterComponents('Reportman', [TRpGrid]);
  {$ENDIF}
{$ENDIF}


end;

end.
