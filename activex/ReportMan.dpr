library ReportMan;

uses
  ComServ,
  ReportMan_TLB in 'ReportMan_TLB.pas',
  rpaxreportimp in 'rpaxreportimp.pas' {ReportManX: CoClass},
  rpfaboutx in 'rpfaboutx.pas' {ReportManXAbout};

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
