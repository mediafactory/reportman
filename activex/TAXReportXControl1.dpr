library TAXReportXControl1;

uses
  ComServ,
  TAXReportXControl1_TLB in 'TAXReportXControl1_TLB.pas',
  rpaxreportimp in 'rpaxreportimp.pas' {TAXReport: CoClass},
  rpfaboutx in 'rpfaboutx.pas' {TAXReportAbout};

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
