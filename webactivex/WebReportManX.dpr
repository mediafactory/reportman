library WebReportManX;

uses
  ComServ,
  WebReportManX_TLB in 'WebReportManX_TLB.pas',
  rpwebreportx in 'rpwebreportx.pas' {WebReportMan: TActiveForm} {WebReportMan: CoClass},
  rpwebmetaclient in '..\rpwebmetaclient.pas',
  rpfmainmetaviewvcl in '..\rpfmainmetaviewvcl.pas' {FRpMainMetaVCL},
  rpfmetaviewvcl in '..\rpfmetaviewvcl.pas' {FRpMetaVCL: TFrame};

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
