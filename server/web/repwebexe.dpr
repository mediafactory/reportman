program repwebexe;

{$APPTYPE CONSOLE}

{$I rpconf.inc}

uses
  WebBroker,
  CGIApp,
{$IFDEF MSWINDOWS}
{$IFDEF USEVARIANTS}
  midaslib,
{$ENDIF}
{$ENDIF}
  rpwebmodule in 'rpwebmodule.pas' {repwebmod: TWebModule},
  rpwebpages in 'rpwebpages.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Trepwebmod, repwebmod);
  Application.Run;
end.

