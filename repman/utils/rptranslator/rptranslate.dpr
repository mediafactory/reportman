{*******************************************************}
{                                                       }
{       RpTranslate application                         }
{                                                       }
{       To easy translate strings at                    }
{       runtime                                         }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{*******************************************************}

program rptranslate;

uses
  Forms,
  umain in 'umain.pas' {FMain},
  rptranslator in '..\..\..\rptranslator.pas',
  uflanginfo in 'uflanginfo.pas' {FLangInfo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
