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
  rpdllutil in '..\rpdllutil.pas',
  rpdllutilqt in '..\rpdllutilqt.pas',
  rpactivexreport in '..\rpactivexreport.pas';

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer,
  rp_open,
  rp_close,
  rp_execute,
  rp_lasterror,
  rp_print,
  rp_preview;

{$R *.TLB}

{$R *.RES}

begin
end.
