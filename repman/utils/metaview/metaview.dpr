program metaview;

{%File '..\..\..\rpconf.inc'}

uses
  QForms,
{$IFDEF MSWINDOWS}
  fmetaview in '..\..\..\fmetaview.pas' {FRpMeta},
  rpmetafile in '..\..\..\rpmetafile.pas',
  rpprintdia in '..\..\..\rpprintdia.pas' {FRpPrintDialog},
  rpqtdriver in '..\..\..\rpqtdriver.pas' {TFRpQtProgress},
  rppdfdriver in '..\..\..\rppdfdriver.pas',
  rpmdconsts in '..\..\..\rpmdconsts.pas',
  rpmdrepclient in '..\..\..\rpmdrepclient.pas' {modclient: TDataModule},
  rpmdprotocol in '..\..\..\rpmdprotocol.pas',
  rpmdclitree in '..\..\..\rpmdclitree.pas' {FRpCliTree: TFrame},
  rpgraphutils in '..\..\..\rpgraphutils.pas' {FRpMessageDlg},
  rpgdidriver in '..\..\..\rpgdidriver.pas' {FRpVCLProgress},
  rpmdfabout in '..\..\..\rpmdfabout.pas' {FRpAboutBox},
  rprfparams in '..\..\..\rprfparams.pas' {FRpRunTimeParams},
  rpmdprintconfig in '..\..\..\rpmdprintconfig.pas' {FRpPrinterConfig};
{$ENDIF}
{$IFDEF LINUX}
  fmetaview in '../../../fmetaview.pas' {FRpMeta},
  rpmetafile in '../../../rpmetafile.pas',
  rpprintdia in '../../../rpprintdia.pas' {FRpPrintDialog},
  rppdfdriver in '../../../rppdfdriver.pas',
  rpqtdriver in '../../../rpqtdriver.pas' {TFRpQtProgress},
  rpmdconsts in '../../../rpmdconsts.pas',
  rpmdrepclient in '../../../rpmdrepclient.pas' {modclient: TDataModule},
  rpmdprotocol in '../../../rpmdprotocol.pas',
  rpmdclitree in '../../../rpmdclitree.pas' {FRpCliTree: TFrame},
  rpgraphutils in '../../../rpgraphutils.pas' {FRpMessageDlg},
  rpmdfabout in '../../../rpmdfabout.pas' {FRpAboutBox},
  rprfparams in '../../../rprfparams.pas' {FRpRunTimeParams},
  rpmdprintconfig in '../../../rpmdprintconfig.pas' {FRpPrinterConfig};
{$ENDIF}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Report manager metafile report viewer';
  Application.CreateForm(TFRpMeta, FRpMeta);
  Application.Run;
end.
