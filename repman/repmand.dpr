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
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}

program repmand;

uses
  QForms,
  fmain in 'fmain.pas' {FMainf},
  frpstruc in 'frpstruc.pas' {FRpStructure: TFrame},
  fdesign in 'fdesign.pas' {FDesignFrame: TFrame},
  rpshfolder in 'rpshfolder.pas',
  freportgroup in 'freportgroup.pas' {FGroup},
  fdatainfo in 'fdatainfo.pas' {FDatainfoconfig},
  fsampledata in 'fsampledata.pas' {FShowSampledata},
  rpobinsint in 'rpobinsint.pas',
  rpobjinsp in 'rpobjinsp.pas' {FObjInsp: TFrame},
{$IFDEF MSWINDOWS}
  ShLwApi in 'ShLwApi.pas',
  ShFolder in 'ShFolder.pas',
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
  rpqtdriver in '..\rpqtdriver.pas',
  rpruler in '..\rpruler.pas',
  rptypeval in '..\rptypeval.pas',
  rpwriter in '..\rpwriter.pas',
  rpdatainfo in '..\rpdatainfo.pas',
  rpparams in '..\rpparams.pas',
  fsectionint in 'fsectionint.pas' {FSectionProps},
  rpfparams in '..\rpfparams.pas' {FRpParams},
  rpdbxconfig in '..\rpdbxconfig.pas' {FDBXConfig},
  rpgraphutils in '..\rpgraphutils.pas' {FRpGraphProgres};
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
  rpqtdriver in '../rpqtdriver.pas',
  rpruler in '../rpruler.pas',
  rptypeval in '../rptypeval.pas',
  rpwriter in '../rpwriter.pas',
  rpdatainfo in '../rpdatainfo.pas',
  rpparams in '../rpparams.pas',
  rpfparams in '../rpfparams.pas' {FRpParams},
  rpdbxconfig in '../rpdbxconfig.pas' {FDBXConfig},
  rpgraphutils in '../rpgraphutils.pas' {FRpGraphProgres};
{$ENDIF}



{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Report manager designer';
  Application.CreateForm(TFMainf, FMainf);
  Application.Run;
end.
