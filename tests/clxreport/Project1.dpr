program Project1;

uses
  QForms,
  midaslib,
  Unit1 in 'Unit1.pas' {Form1},
  rpclxreport in '..\..\rpclxreport.pas',
  rpalias in '..\..\rpalias.pas',
  rpreport in '..\..\rpreport.pas',
  rpdatainfo in '..\..\rpdatainfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
