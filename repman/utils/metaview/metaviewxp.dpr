program metaviewxp;

{%File '..\..\..\rpconf.inc'}

uses
  Forms,
  fmetaviewvcl in 'fmetaviewvcl.pas' {FMetaVCL},
  rpmetafile in '..\..\..\rpmetafile.pas',
  rppdfdriver in '..\..\..\rppdfdriver.pas',
  rpmdconsts in '..\..\..\rpmdconsts.pas',
  rpmdrepclient in '..\..\..\rpmdrepclient.pas' {modclient: TDataModule},
  rpmdprotocol in '..\..\..\rpmdprotocol.pas',
  rpmdclitreevcl in '..\..\..\rpmdclitreevcl.pas' {FRpCliTreeVCL: TFrame},
  rpmdfaboutvcl in '..\..\..\rpmdfaboutvcl.pas' {FRpAboutBoxVCL},
  rprfvparams in '..\..\..\rprfvparams.pas' {FRpRTParams},
  rpvgraphutils in '..\..\..\rpvgraphutils.pas',
  rpgdidriver in '..\..\..\rpgdidriver.pas' {FRpVCLProgress},
  rpmdprintconfigvcl in '..\..\..\rpmdprintconfigvcl.pas' {FRpPrinterConfigVCL};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Report manager metafile report viewer';
  Application.CreateForm(TFMetaVCL, FMetaVCL);
  Application.Run;
end.
