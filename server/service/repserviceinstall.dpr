{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       repserviceinstall                               }
{       Installation of Report Manager Service          }
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

program repserviceinstall;

uses
  Forms,
  uinstall in 'uinstall.pas' {FStartService},
  rpmdconsts in '..\..\rpmdconsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Report Manager Service Install';
  Application.CreateForm(TFStartService, FStartService);
  Application.Run;
end.
