{*******************************************************}
{                                                       }
{       Rpregvcl                                        }
{                                                       }
{       Units that registers the reportmanager engine   }
{       vcl version Delphi 6 component palette          }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{*******************************************************}


unit rpregvcl;

interface

uses
  Classes,rpvclreport;


procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('Reportman', [TVCLReport]);
end;

end.
