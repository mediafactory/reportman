{*******************************************************}
{                                                       }
{       Report Manager Service Main project             }
{                                                       }
{       repserverservice                                }
{                                                       }
{       To install service run:                         }
{           repserverservice /INSTALL                   }
{       To uninstall service run:                       }
{           repserverservice /UNINSTALL                 }
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

program repserverservice;

uses
  SvcMgr,
  urepservice in 'urepservice.pas' {ReportService: TService},
  urepserver in '..\app\urepserver.pas' {modserver: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TReportService, ReportService);
  Application.Run;
end.