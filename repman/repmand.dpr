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

{$IFDEF MSWINDOWS}
{%File '..\rpconf.inc'}
{$ENDIF}

{$IFDEF LINUX}
{%File '../rpconf.inc'}
{$ENDIF}

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
  fhelpform in 'fhelpform.pas' {FHelpf},
{$IFDEF MSWINDOWS}
  ShLwApi in 'ShLwApi.pas',
  ShFolder in 'ShFolder.pas',
  midaslib,Crtl,
  rppdfreport in '..\rppdfreport.pas',
  rpvclreport in '..\rpvclreport.pas',
  rpvgraphutils in '..\rpvgraphutils.pas',
  rpgdidriver in '..\rpgdidriver.pas',
  rpvpreview in '..\rpvpreview.pas',
  rprfvparams in '..\rprfvparams.pas',
  rpgdifonts in '..\rpgdifonts.pas',

  rpreport in '..\rpreport.pas',
  rpsubreport in '..\rpsubreport.pas',
  rpconsts in '..\rpconsts.pas',
  rppagesetup in '..\rppagesetup.pas' {FPageSetup},
  rpmunits in '..\rpmunits.pas',
  rptypes in '..\rptypes.pas',
  rpdataset in '..\rpdataset.pas',
  rpsection in '..\rpsection.pas',
  rpsecutil in '..\rpsecutil.pas',
  rplastsav in '..\rplastsav.pas',
  rpprintitem in '..\rpprintitem.pas',
  rpparser in '..\rpparser.pas',
  rpevalfunc in '..\rpevalfunc.pas',
  rpeval in '..\rpeval.pas',
  rpalias in '..\rpalias.pas',
  rpexpredlg in '..\rpexpredlg.pas' {FrpExpredialog},
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
  rpprintdia in '..\rpprintdia.pas' {FRpPrintDialog},
  rplabelitem in '..\rplabelitem.pas',
  rpdrawitem in '..\rpdrawitem.pas',
  rpzlib in '..\rpzlib.pas',
  rppreview in '..\rppreview.pas' {FRpPreview},
  rprfparams in '..\rprfparams.pas' {FRpRunTimeParams},
  rpzlibadler in '..\rpzlibadler.pas',
  rpzlibinfblock in '..\rpzlibinfblock.pas',
  rpzlibinfcodes in '..\rpzlibinfcodes.pas',
  rpzlibinffast in '..\rpzlibinffast.pas',
  rpzlibinftrees in '..\rpzlibinftrees.pas',
  rpzlibinfutil in '..\rpzlibinfutil.pas',
  rpzlibtrees in '..\rpzlibtrees.pas',
  rpzlibzdeflate in '..\rpzlibzdeflate.pas',
  rpzlibzinflate in '..\rpzlibzinflate.pas',
  rpzlibzlib in '..\rpzlibzlib.pas',
  rpzlibzutil in '..\rpzlibzutil.pas',
  rpclxreport in '..\rpclxreport.pas',
  rppdffile in '..\rppdffile.pas',
  rppdfdriver in '..\rppdfdriver.pas';
{$ENDIF}

{$IFDEF LINUX}
  rpclxreport in '../rppdfreport.pas',
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
  rpdataset in '../rpdataset.pas',
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
  rprfparams in '../rprfparams.pas' {FRpRunTimeParams},
  rpzlibadler in '../rpzlibadler.pas',
  rpzlibinfblock in '../rpzlibinfblock.pas',
  rpzlibinfcodes in '../rpzlibinfcodes.pas',
  rpzlibinffast in '../rpzlibinffast.pas',
  rpzlibinftrees in '../rpzlibinftrees.pas',
  rpzlibinfutil in '../rpzlibinfutil.pas',
  rpzlibtrees in '../rpzlibtrees.pas',
  rpzlibzdeflate in '../rpzlibzdeflate.pas',
  rpzlibzinflate in '../rpzlibzinflate.pas',
  rpzlibzlib in '../rpzlibzlib.pas',
  rpzlibzutil in '../rpzlibzutil.pas',
  rpclxreport in '../rpclxreport.pas',
  rppdfdriver in '../rppdfdriver.pas',
  rppdffile in '../rppdffile.pas';
{$ENDIF}



{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMainf, FMainf);
  Application.Run;
end.
