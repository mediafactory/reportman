program metaview;

{%File '..\..\..\rpconf.inc'}

uses
  QForms,
  fmetaview in 'fmetaview.pas' {FMeta},
{$IFDEF MSWINDOWS}
  rpmetafile in '..\..\..\rpmetafile.pas',
  rpprintdia in '..\..\..\rpprintdia.pas' {FRpPrintDialog},
  rpqtdriver in '..\..\..\rpqtdriver.pas',
  rppdfdriver in '..\..\..\rppdfdriver.pas',
  rpconsts in '..\..\..\rpconsts.pas';
{$ENDIF}
{$IFDEF LINUX}
  rpmetafile in '../../../rpmetafile.pas',
  rpprintdia in '../../../rpprintdia.pas' {FRpPrintDialog},
  rppdfdriver in '../../../rppdfdriver.pas',
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
