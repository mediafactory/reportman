program Project1;

uses
  QForms,
  Unit1 in 'Unit1.pas' {Form1},
  rptypes in '..\..\rptypes.pas',
  rpgraphutils in '..\..\rpgraphutils.pas' {FRpMessageDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
