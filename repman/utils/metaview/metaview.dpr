program metaview;

uses
  QForms,
  fmetaview in 'fmetaview.pas' {FMeta},
  rpmetafile in '../../../rpmetafile.pas',
  rpqtdriver in '../../../rpqtdriver.pas',
  rpconsts in '../../../rpconsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMeta, FMeta);
  Application.Run;
end.
