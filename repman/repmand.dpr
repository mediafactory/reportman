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
{%File '..\rpconf.inc'}

uses
  QForms,
{$IFDEF MSWINDOWS}
  midaslib,
  rpmdfmain in '..\rpmdfmain.pas' {FRpMainF},
  rpmdfstruc in '..\rpmdfstruc.pas' {FRpStructure: TFrame},
  rpmdfdesign in '..\rpmdfdesign.pas' {FRpDesignFrame: TFrame},
  rpmdshfolder in '..\rpmdshfolder.pas',
  rpmdfdatainfo in '..\rpmdfdatainfo.pas' {FRpDatainfoconfig},
  rpmdfsampledata in '..\rpmdfsampledata.pas' {FRpShowSampledata},
  rpmdobinsint in '..\rpmdobinsint.pas',
  rpmdfdrawint in '..\rpmdfdrawint.pas',
  rpmdobjinsp in '..\rpmdobjinsp.pas' {FRpObjInsp: TFrame},
  rpmdfgrid in '..\rpmdfgrid.pas' {FRpGridOptions},
  rpmdflabelint in '..\rpmdflabelint.pas',
  rpmdfbarcodeint in '..\rpmdfbarcodeint.pas',
  rpmdfabout in '..\rpmdfabout.pas' {FRpAboutBox},
  rpmdfhelpform in '..\rpmdfhelpform.pas' {FRpHelpForm},
  rppdfreport in '..\rppdfreport.pas',
  rpvclreport in '..\rpvclreport.pas',
  rpvgraphutils in '..\rpvgraphutils.pas',
  rpgdidriver in '..\rpgdidriver.pas',
  rpvpreview in '..\rpvpreview.pas',
  rprfvparams in '..\rprfvparams.pas',
  rpgdifonts in '..\rpgdifonts.pas',
  rpreport in '..\rpreport.pas',
  rpsubreport in '..\rpsubreport.pas',
  rpmdconsts in '..\rpmdconsts.pas',
  rppagesetup in '..\rppagesetup.pas' {FRpPageSetup},
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
  rpmdfsectionint in '..\rpmdfsectionint.pas',
  rpfparams in '..\rpfparams.pas' {FRpParams},
  rpdbxconfig in '..\rpdbxconfig.pas' {FRpDBXConfig},
  rpgraphutils in '..\rpgraphutils.pas' {FRpGraphProgres},
  rpprintdia in '..\rpprintdia.pas' {FRpPrintDialog},
  rplabelitem in '..\rplabelitem.pas',
  rpmdbarcode in '..\rpmdbarcode.pas',
  rpdrawitem in '..\rpdrawitem.pas',
  rpmzlib in '..\rpmzlib.pas',
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
  rpcompobase in '..\rpcompobase.pas',
  rpclxreport in '..\rpclxreport.pas',
  rppdffile in '..\rppdffile.pas',
  rppdfdriver in '..\rppdfdriver.pas',
  rpmdesigner in '..\rpmdesigner.pas';
{$ENDIF}


{$IFDEF LINUX}
  rpmdfmain in '../rpmdfmain.pas' {FRpMainF},
  rpmdfstruc in '../rpmdfstruc.pas' {FRpStructure: TFrame},
  rpmdfdesign in '../rpmdfdesign.pas' {FRpDesignFrame: TFrame},
  rpmdshfolder in '../rpmdshfolder.pas',
  rpmdfdatainfo in '../rpmdfdatainfo.pas' {FRpDatainfoconfig},
  rpmdfsampledata in '../rpmdfsampledata.pas' {FRpShowSampledata},
  rpmdobinsint in '../rpmdobinsint.pas',
  rpmdfdrawint in '../rpmdfdrawint.pas',
  rpmdobjinsp in '../rpmdobjinsp.pas' {FRpObjInsp: TFrame},
  rpmdfgrid in '../rpmdfgrid.pas' {FRpGridOptions},
  rpmdflabelint in '../rpmdflabelint.pas',
  rpmdfbarcodeint in '../rpmdfbarcodeint.pas',
  rpmdfabout in '../rpmdfabout.pas' {FRpAboutBox},
  rpmdfhelpform in '../rpmdfhelpform.pas' {FRpHelpForm},
  rppdfreport in '../rppdfreport.pas',
  rpreport in '../rpreport.pas',
  rpsubreport in '../rpsubreport.pas',
  rpmdconsts in '../rpmdconsts.pas',
  rppagesetup in '../rppagesetup.pas' {FRpPageSetup},
  rpmunits in '../rpmunits.pas',
  rptypes in '../rptypes.pas',
  rpdataset in '../rpdataset.pas',
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
  rpqtdriver in '../rpqtdriver.pas' {FRpQtProgress},
  rpruler in '../rpruler.pas',
  rptypeval in '../rptypeval.pas',
  rpwriter in '../rpwriter.pas',
  rpdatainfo in '../rpdatainfo.pas',
  rpparams in '../rpparams.pas',
  rpmdfsectionint in '../rpmdfsectionint.pas',
  rpfparams in '../rpfparams.pas' {FRpParams},
  rpdbxconfig in '../rpdbxconfig.pas' {FRpDBXConfig},
  rpgraphutils in '../rpgraphutils.pas' {FRpGraphProgres},
  rpprintdia in '../rpprintdia.pas' {FRpPrintDialog},
  rplabelitem in '../rplabelitem.pas',
  rpmdbarcode in '../rpmdbarcode.pas',
  rpdrawitem in '../rpdrawitem.pas',
  rpmzlib in '../rpmzlib.pas',
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
  rpcompobase in '../rpcompobase.pas',
  rpclxreport in '../rpclxreport.pas',
  rppdffile in '../rppdffile.pas',
  rppdfdriver in '../rppdfdriver.pas',
  rpmdesigner in '../rpmdesigner.pas';
{$ENDIF}



{$R *.res}


var
  FRpMainF: TFRpMainF;

begin
  Application.Initialize;
  Application.CreateForm(TFRpMainF, FRpMainF);
  Application.Run;
end.
