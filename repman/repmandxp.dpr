{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       repmandxp                                       }
{       Main form of report manager designer            }
{       Used by a subreport                             }
{                                                       }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{*******************************************************}

program repmandxp;

{$I rpconf.inc}

uses
  Forms,
{$IFDEF USEVARIANTS}
  midaslib,
{$ENDIF}
  rpmdfmainvcl in '..\rpmdfmainvcl.pas' {FRpMainFVCL},
  rpmdfdesignvcl in '..\rpmdfdesignvcl.pas' {FRpDesignFrameVCL: TFrame},
  rpmdfaboutvcl in '..\rpmdfaboutvcl.pas' {FRpAboutBoxVCL},
  rpmdfstrucvcl in '..\rpmdfstrucvcl.pas' {FRpStructureVCL: TFrame},
  rpmdobjinspvcl in '..\rpmdobjinspvcl.pas' {FRpObjInspVCL: TFrame},
  rppagesetupvcl in '..\rppagesetupvcl.pas' {FRpPageSetupVCL},
  rpfparamsvcl in '..\rpfparamsvcl.pas' {FRpParamsVCL},
  rpgraphutilsvcl in '..\rpgraphutilsvcl.pas' {FRpMessageDlgVCL},
  rpexpredlgvcl in '..\rpexpredlgvcl.pas' {FRpExpredialogVCL},
  rprfvparams in '..\rprfvparams.pas' {FRpRTParams},
  rpmdfsectionintvcl in '..\rpmdfsectionintvcl.pas',
  rpactivexreport in '..\rpactivexreport.pas',
  rpalias in '..\rpalias.pas',
  rpcompobase in '..\rpcompobase.pas',
  rpdatainfo in '..\rpdatainfo.pas',
  rpdataset in '..\rpdataset.pas',
  rpdbxconfigvcl in '..\rpdbxconfigvcl.pas' {FRpDBXConfigVCL},
  rpdrawitem in '..\rpdrawitem.pas',
  rpeval in '..\rpeval.pas',
  rpevalfunc in '..\rpevalfunc.pas',
  rpgdidriver in '..\rpgdidriver.pas' {FRpVCLProgress},
{$IFDEF USEEXCEL}
  rpexceldriver in '..\rpexceldriver.pas' {FRpExcelProgress},
{$ENDIF}
  rpgdifonts in '..\rpgdifonts.pas',
  rplabelitem in '..\rplabelitem.pas',
  rplastsav in '..\rplastsav.pas',
  rpmdbarcode in '..\rpmdbarcode.pas',
  rpmdchart in '..\rpmdchart.pas',
  rpmdconsts in '..\rpmdconsts.pas',
  rpmdfbarcodeintvcl in '..\rpmdfbarcodeintvcl.pas',
  rpmdfchartintvcl in '..\rpmdfchartintvcl.pas',
  rpmdfdatainfovcl in '..\rpmdfdatainfovcl.pas' {FRpDatainfoconfigVCL},
  rpmdfdrawintvcl in '..\rpmdfdrawintvcl.pas',
  rpmdfgridvcl in '..\rpmdfgridvcl.pas' {FRpGridOptionsVCL},
  rpmdflabelintvcl in '..\rpmdflabelintvcl.pas',
  rpmdfsampledatavcl in '..\rpmdfsampledatavcl.pas' {FRpShowSampledataVCL},
  rpmdobinsintvcl in '..\rpmdobinsintvcl.pas',
  rpmdprintconfigvcl in '..\rpmdprintconfigvcl.pas' {FRpPrinterConfigVCL},
  rpmdshfolder in '..\rpmdshfolder.pas',
  rpmetafile in '..\rpmetafile.pas',
  rpmreg in '..\rpmreg.pas',
  rpmunits in '..\rpmunits.pas',
  rpparams in '..\rpparams.pas',
  rpparser in '..\rpparser.pas',
  rppdfdriver in '..\rppdfdriver.pas',
  rppdffile in '..\rppdffile.pas',
  rppdfreport in '..\rppdfreport.pas',
  rpprintitem in '..\rpprintitem.pas',
  rpregvcl in '..\rpregvcl.pas',
  rpreport in '..\rpreport.pas',
  rprulervcl in '..\rprulervcl.pas',
  rpsection in '..\rpsection.pas',
  rpsecutil in '..\rpsecutil.pas',
  rpsubreport in '..\rpsubreport.pas',
  rptranslator in '..\rptranslator.pas',
  rptypes in '..\rptypes.pas',
  rptypeval in '..\rptypeval.pas',
  rpvclreport in '..\rpvclreport.pas',
  rpvgraphutils in '..\rpvgraphutils.pas',
  rpvpreview in '..\rpvpreview.pas' {FRpVPreview},
  rpwriter in '..\rpwriter.pas',
  rpmdfconnectionvcl in '..\rpmdfconnectionvcl.pas' {FRpConnectionVCL: TFrame},
  rpmdfwizardvcl in '..\rpmdfwizardvcl.pas' {FRpWizardVCL},
  rpmdfextsecvcl in '..\rpmdfextsecvcl.pas' {FRpExtSectionVCL},
  rpmdfdatasetsvcl in '..\rpmdfdatasetsvcl.pas' {FRpDatasetsVCL: TFrame},
  rpfmetaviewvcl in '..\rpfmetaviewvcl.pas' {FRpMetaVCL},
  rpfmainmetaviewvcl in '..\rpfmainmetaviewvcl.pas' {FRpMainMetaVCL},
  rpmdsysinfo in '..\rpmdsysinfo.pas' {FRpSysInfo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFRpMainFVCL, FRpMainFVCL);
  FRpMainFVCL.BrowseCommandLine:=true;
  Application.Run;
end.
