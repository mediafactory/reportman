{*******************************************************}
{                                                       }
{       Rpmregdesignvcl                                 }
{                                                       }
{       Units that registers the Report Manager Designer}
{       into the Delphi component palette               }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{*******************************************************}

unit rpmregdesignvcl;

interface

uses Classes,rpmdesignervcl;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('Reportman', [TRpDesignerVCL]);
end;

end.