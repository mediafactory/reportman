program metaviewxp;

{%File '..\..\..\rpconf.inc'}

uses
  Forms,
  fmetaviewvcl in 'fmetaviewvcl.pas' {FMetaVCL},
  rpmetafile in '..\..\..\rpmetafile.pas',
  rpgdidriver in '..\..\..\rpgdidriver.pas',
  rppdfdriver in '..\..\..\rppdfdriver.pas',
  rpmdconsts in '..\..\..\rpmdconsts.pas',
  rpmdrepclient in '..\..\..\rpmdrepclient.pas' {modclient: TDataModule},
  rpmdprotocol in '..\..\..\rpmdprotocol.pas',
  rpmdclitreevcl in '..\..\..\rpmdclitreevcl.pas' {FRpCliTreeVCL: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Report manager metafile report viewer';
  Application.CreateForm(TFMetaVCL, FMetaVCL);
  Application.Run;
end.
