{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       Repmand                                         }
{       Report manager designer executable              }
{       allow to design, print, preview reports         }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

program repmand;

{%ToDo 'repmand.todo'}

uses
  QForms,
  fmain in 'fmain.pas' {FMainf},
  frpstruc in 'frpstruc.pas' {FRpStructure: TFrame},
  fdesign in 'fdesign.pas' {FDesignFrame: TFrame},
  rpshfolder in 'rpshfolder.pas',
  fdatainfo in 'fdatainfo.pas' {FDatainfoconfig},
  fsampledata in 'fsampledata.pas' {FShowSampledata},
  rpobinsint in 'rpobinsint.pas',
  fdrawint in 'fdrawint.pas' {FDrawInterface},
  rpobjinsp in 'rpobjinsp.pas' {FObjInsp: TFrame},
  frpgrid in 'frpgrid.pas' {FRpGridOptions},
  flabelint in 'flabelint.pas' {FLabelInterface},
  fabout in 'fabout.pas' {FAboutBox},
{$IFDEF MSWINDOWS}
  ShLwApi in 'ShLwApi.pas',
  ShFolder in 'ShFolder.pas',
  midaslib,
  rpreport in '..\rpreport.pas',
  rpsubreport in '..\rpsubreport.pas',
  rpconsts in '..\rpconsts.pas',
  rppagesetup in '..\rppagesetup.pas' {FPageSetup},
  rpmunits in '..\rpmunits.pas',
  rptypes in '..\rptypes.pas',
  rpsection in '..\rpsection.pas',
  rpsecutil in '..\rpsecutil.pas',
  rplastsav in '..\rplastsav.pas',
  rpprintitem in '..\rpprintitem.pas',
  rpparser in '..\rpparser.pas',
  rpevalfunc in '..\rpevalfunc.pas',
  rpeval in '..\rpeval.pas',
  rpalias in '..\rpalias.pas',
  rpexpredlg in '..\rpexpredlg.pas' {FrpExpredialog},
  rpgdidriver in '..\rpgdidriver.pas',
  rpmetafile in '..\rpmetafile.pas',
  rpqtdriver in '..\rpqtdriver.pas' {FRpQtProgress},
  rpruler in '..\rpruler.pas',
  rptypeval in '..\rptypeval.pas',
  rpwriter in '..\rpwriter.pas',
  rpdatainfo in '..\rpdatainfo.pas',
  rpparams in '..\rpparams.pas',
  fsectionint in 'fsectionint.pas' {FSectionProps},
  rpfparams in '..\rpfparams.pas' {FRpParams},
  rpdbxconfig in '..\rpdbxconfig.pas' {FDBXConfig},
  rpgraphutils in '..\rpgraphutils.pas' {FRpGraphProgres},
  rplabelitem in '..\rplabelitem.pas',
  rpdrawitem in '..\rpdrawitem.pas',
  rpzlib in '..\rpzlib.pas',
  rppreview in '..\rppreview.pas' {FRpPreview},
  adler in '..\adler.pas',
  infblock in '..\infblock.pas',
  infcodes in '..\infcodes.pas',
  inffast in '..\inffast.pas',
  inftrees in '..\inftrees.pas',
  infutil in '..\infutil.pas',
  trees in '..\trees.pas',
  zdeflate in '..\zdeflate.pas',
  zinflate in '..\zinflate.pas',
  zlib in '..\zlib.pas',
  zutil in '..\zutil.pas',
  rpclxreport in '..\rpclxreport.pas';
{$ENDIF}

{$IFDEF LINUX}
  rpreport in '../rpreport.pas',
  rpsubreport in '../rpsubreport.pas',
  rpconsts in '../rpconsts.pas',
  rppagesetup in '../rppagesetup.pas' {FPageSetup},
  rpmunits in '../rpmunits.pas',
  rptypes in '../rptypes.pas',
  rpsection in '../rpsection.pas',
  rpsecutil in '../rpsecutil.pas',
  rplastsav in '../rplastsav.pas',
  rpprintitem in '../rpprintitem.pas',
  rpparser in '../rpparser.pas',
  rpevalfunc in '../rpevalfunc.pas',
  rpeval in '../rpeval.pas',
  rpalias in '../rpalias.pas',
  rpexpredlg in '../rpexpredlg.pas' {FrpExpredialog},
  rpmetafile in '../rpmetafile.pas',
  rpqtdriver in '../rpqtdriver.pas' {FQtProgress},
  rpruler in '../rpruler.pas',
  rptypeval in '../rptypeval.pas',
  rpwriter in '../rpwriter.pas',
  rpdatainfo in '../rpdatainfo.pas',
  rpparams in '../rpparams.pas',
  rpfparams in '../rpfparams.pas' {FRpParams},
  rpdbxconfig in '../rpdbxconfig.pas' {FDBXConfig},
  rpgraphutils in '../rpgraphutils.pas' {FRpGraphProgres},
  rplabelitem in '../rplabelitem.pas',
  rpdrawitem in '../rpdrawitem.pas',
  rpzlib in '../rpzlib.pas',
  rppreview in '../rppreview.pas' {FRpPreview},
  adler in '../adler.pas',
  infblock in '../infblock.pas',
  infcodes in '../infcodes.pas',
  inffast in '../inffast.pas',
  inftrees in '../inftrees.pas',
  infutil in '../infutil.pas',
  trees in '../trees.pas',
  zdeflate in '../zdeflate.pas',
  zinflate in '../zinflate.pas',
  zlib in '../zlib.pas',
  zutil in '../zutil.pas',
  rpclxreport in '../rpclxreport.pas';
{$ENDIF}



{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Report manager designer';
  Application.CreateForm(TFMainf, FMainf);
  Application.Run;
end.
