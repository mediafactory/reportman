program repserverconfig;

{$I rpconf.inc}

uses
  QForms,
  mainf in 'mainf.pas' {FMain},
  unewuser in 'unewuser.pas' {FNewUser},
  unewalias in 'unewalias.pas' {FNewAlias},
{$IFDEF MSWINDOWS}
  rpmdrepclient in '..\..\rpmdrepclient.pas' {modclient: TDataModule},
{$ENDIF}
{$IFDEF LINUX}
  rpmdrepclient in '../../rpmdrepclient.pas' {modclient: TDataModule},
{$ENDIF}
  ureptree in 'ureptree.pas' {FReportTree};


{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
