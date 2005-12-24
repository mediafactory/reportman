{*******************************************************}
{                                                       }
{       Rpregvcl                                        }
{                                                       }
{       Units that registers the reportmanager engine   }
{       vcl version Delphi 6 component palette          }
{                                                       }
{       Copyright (c) 1994-2003 Toni Martir             }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{*******************************************************}


unit rpregvcl;

interface

uses
  Classes,rpvclreport,rpexpredlgvcl,rpmaskedit,rpdbgridvcl,rppreviewcontrol;


procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('Reportman', [TVCLReport]);
  RegisterComponents('Reportman', [TRpExpreDialogVCL]);
  RegisterComponents('Reportman', [TRpMaskEdit]);
  RegisterComponents('Reportman', [TRpGrid]);
  RegisterComponents('Reportman', [TRpPreviewControl]);
  // TRpActiveXReport is a Wrapper to generate the ActiveX version
  // with Delphi 6 Active X Control Wizard
//  RegisterComponents('Reportman', [TRpActiveXReport]);
end;

end.
