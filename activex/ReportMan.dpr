library ReportMan;

{$I rpconf.inc}

uses
  ComServ,
{$IFDEF USEVARIANTS}
  MidasLib,
{$ENDIF}
  Reportman_TLB in 'Reportman_TLB.pas',
  rpaxreportimp in 'rpaxreportimp.pas' {ReportManX: CoClass},
  aboutrpax in 'aboutrpax.pas' {ReportManXAbout},
  rpactivexreport in '..\rpactivexreport.pas';

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
