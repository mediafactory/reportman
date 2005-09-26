program metaviewxp;

{%File '..\..\..\rpconf.inc'}

uses
  Graphics,
  Forms,
  rpfmainmetaviewvcl in '..\..\..\rpfmainmetaviewvcl.pas' {FRpMainMetaVCL},
  rpmetafile in '..\..\..\rpmetafile.pas',
  rppdfdriver in '..\..\..\rppdfdriver.pas',
  rpmdconsts in '..\..\..\rpmdconsts.pas',
  rpmdrepclient in '..\..\..\rpmdrepclient.pas',
  rpmdprotocol in '..\..\..\rpmdprotocol.pas',
  rpmdclitreevcl in '..\..\..\rpmdclitreevcl.pas' {FRpCliTreeVCL: TFrame},
  rpmdfaboutvcl in '..\..\..\rpmdfaboutvcl.pas' {FRpAboutBoxVCL},
  rprfvparams in '..\..\..\rprfvparams.pas' {FRpRTParams},
  rpvgraphutils in '..\..\..\rpvgraphutils.pas',
  rpgdidriver in '..\..\..\rpgdidriver.pas' {FRpVCLProgress},
  rpmdprintconfigvcl in '..\..\..\rpmdprintconfigvcl.pas' {FRpPrinterConfigVCL},
  rpfmetaviewvcl in '..\..\..\rpfmetaviewvcl.pas' {FRpMetaVCL: TFrame},
  rpexceldriver in '..\..\..\rpexceldriver.pas' {FRpExcelProgress};

{$R *.res}
var
 meta:TRpMEtafileReport;

begin
  meta:=TRpMetafileReport.Create(nil);
  meta.AsyncReading:=true;
  meta.LoadFromFile('z:\testcinta.rpmf');
  PreviewMetafile(meta,nil,false,false);
  with Application do
  begin
   Title:=SRpRepMetafile;
  end;
  Graphics.DefFontData.Name:=Screen.IconFont.Name;
  Application.Initialize;
  Application.CreateForm(TFRpMainMetaVCL, FRpMainMetaVCL);
  FRpMainMetaVCL.Font.Assign(Screen.IconFont);
  FRpMainMetaVCL.MFrame.CreateClitree;
  FRpMainMetaVCL.browsecommandline:=true;
  Application.Run;
end.

