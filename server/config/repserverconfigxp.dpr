{*******************************************************}
{                                                       }
{       Report Manager Server configuration             }
{                                                       }
{       repserverconfigxp                               }
{                                                       }
{       Main project to build repserverconfig           }
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

program repserverconfigxp;

{$I rpconf.inc}

uses
  Forms,
  mainfvcl in 'mainfvcl.pas' {FMainVCL},
  unewuservcl in 'unewuservcl.pas' {FNewUserVCL},
  unewaliasvcl in 'unewaliasvcl.pas' {FNewAliasVCL},
  midaslib,
  rpmdrepclient in '..\..\rpmdrepclient.pas' {modclient: TDataModule},
  ureptreevcl in 'ureptreevcl.pas' {FReportTreeVCL};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMainVCL, FMainVCL);
  Application.Run;
end.
