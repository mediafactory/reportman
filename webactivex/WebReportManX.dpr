library WebReportManX;

uses
  ComServ,XPMan,
  WebReportManX_TLB in 'WebReportManX_TLB.pas',
  rpwebreportx in 'rpwebreportx.pas' {WebReportMan: TActiveForm} {WebReportMan: CoClass},
  rpwebmetaclient in '..\rpwebmetaclient.pas',
  rpfmainmetaviewvcl in '..\rpfmainmetaviewvcl.pas' {FRpMainMetaVCL},
  rpfmetaviewvcl in '..\rpfmetaviewvcl.pas' {FRpMetaVCL: TFrame},
  rptranslator in '..\rptranslator.pas',
  rptypes in '..\rptypes.pas',
  rpmdfaboutvcl in '..\rpmdfaboutvcl.pas' {FRpAboutBoxVCL},
  rpmzlib in '..\rpmzlib.pas',
  rpmetafile in '..\rpmetafile.pas';

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
