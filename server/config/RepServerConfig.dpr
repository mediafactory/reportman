program RepServerConfig;

uses
  QForms,
  mainf in 'mainf.pas' {FMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
