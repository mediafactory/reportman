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
  rpactivexreport in '..\rpactivexreport.pas',
  rpaxreportreport in 'rpaxreportreport.pas' {ReportReport: CoClass},
  rpaxreportparameters in 'rpaxreportparameters.pas' {ReportParameters: CoClass},
  rpaxreportparam in 'rpaxreportparam.pas' {ReportParam: CoClass};

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
  rp_preview,
  rp_executeremote,
  rp_printremote,
  rp_previewremote,
  rp_setparamvalue,
  rp_setparamvaluevar,
  rp_getparamname,
  rp_getparamcount,
  rp_setadoconnectionstring;

{$R *.TLB}

{$R *.RES}

begin
end.
