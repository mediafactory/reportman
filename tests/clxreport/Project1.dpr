program Project1;

uses
  QForms,
  Unit1 in 'Unit1.pas' {Form1},
{$IFDEF MSWINDOWS}
  midaslib,
  rpclxreport in '..\..\rpclxreport.pas',
  rpalias in '..\..\rpalias.pas',
  rpreport in '..\..\rpreport.pas',
  rpdatainfo in '..\..\rpdatainfo.pas',
  rpqtdriver in '..\..\rpqtdriver.pas' {FRpQtProgress},
  rpcompobase in '..\..\rpcompobase.pas',
  rppdfreport in '..\..\rppdfreport.pas',
  rpmdesigner in '..\..\rpmdesigner.pas';

{$ENDIF}

{$IFDEF LINUX}
  rpclxreport in '../../rpclxreport.pas',
  rpalias in '../../rpalias.pas',
  rpreport in '../../rpreport.pas',
  rpdatainfo in '../../rpdatainfo.pas',
  rpcompobase in '../../rpcompobase.pas',
  rppdfreport in '../../rppdfreport.pas',
  rpmdesigner in '../../rpmdesigner.pas';
{$ENDIF}


{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
