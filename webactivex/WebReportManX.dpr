library WebReportManX;

uses
  ComServ,
  WebReportManX_TLB in 'WebReportManX_TLB.pas',
  rpwebreportx in 'rpwebreportx.pas' {WebReportMan: TActiveForm} {WebReportMan: CoClass};

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
