program metaview;

{%File '..\..\..\rpconf.inc'}

uses
  QForms,
{$IFDEF MSWINDOWS}
  fmetaview in 'fmetaview.pas' {FMeta},
  rpmetafile in '..\..\..\rpmetafile.pas',
  rpprintdia in '..\..\..\rpprintdia.pas' {FRpPrintDialog},
  rpqtdriver in '..\..\..\rpqtdriver.pas',
  rppdfdriver in '..\..\..\rppdfdriver.pas',
  rpmdconsts in '..\..\..\rpmdconsts.pas',
  rpmdrepclient in '..\..\..\rpmdrepclient.pas' {modclient: TDataModule},
  rpmdprotocol in '..\..\..\rpmdprotocol.pas',
  rpmdclitree in '..\..\..\rpmdclitree.pas' {FRpCliTree: TFrame};
{$ENDIF}
{$IFDEF LINUX}
  rpmetafile in '../../../rpmetafile.pas',
  rpprintdia in '../../../rpprintdia.pas' {FRpPrintDialog},
  rppdfdriver in '../../../rppdfdriver.pas',
  rpqtdriver in '../../../rpqtdriver.pas',
  rpmdconsts in '../../../rpmdconsts.pas';
  rpmdrepclient in '../../../rpmdrepclient.pas' {modclient: TDataModule},
  rpmdprotocol in '../../../rpmdprotocol.pas';
  rpmdclitree in '../../../rpmdclitree.pas' {FRpCliTree: TFrame};
{$ENDIF}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Report manager metafile report viewer';
  Application.CreateForm(TFMeta, FMeta);
  Application.Run;
end.
