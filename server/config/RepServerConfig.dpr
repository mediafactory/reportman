program RepServerConfig;

uses
  QForms,
  mainf in 'mainf.pas' {FMain},
  rpmdrepclient in '..\..\rpmdrepclient.pas' {modclient: TDataModule},
  unewuser in 'unewuser.pas' {FNewUser},
  unewalias in 'unewalias.pas' {FNewAlias},
  ureptree in 'ureptree.pas' {FReportTree};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
