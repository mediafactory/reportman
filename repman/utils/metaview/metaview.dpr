program metaview;

uses
  QForms,
  fmetaview in 'fmetaview.pas' {FMeta},
{$IFDEF MSWINDOWS}
  rpmetafile in '..\..\..\rpmetafile.pas',
  rpqtdriver in '..\..\..\rpqtdriver.pas',
  rpconsts in '..\..\..\rpconsts.pas';
{$ENDIF}
{$IFDEF LINUX}
  rpmetafile in '../../../rpmetafile.pas',
  rpqtdriver in '../../../rpqtdriver.pas',
  rpconsts in '../../../rpconsts.pas';
{$ENDIF}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Report manager metafile report viewer';
  Application.CreateForm(TFMeta, FMeta);
  Application.Run;
end.
