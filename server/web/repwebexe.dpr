program repwebexe;

{$APPTYPE CONSOLE}

uses
  WebBroker,
  CGIApp,
  rpwebmodule in 'rpwebmodule.pas' {repwebmod: TWebModule},
  rpwebpages in 'rpwebpages.pas',
  rpdatainfo in '../../rpdatainfo.pas',
  rpmdconsts in '../../rpmdconsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Trepwebmod, repwebmod);
  Application.Run;
end.
