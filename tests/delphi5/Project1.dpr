program Project1;

{%File '..\..\rpconf.inc'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  rpvclreport in '..\..\rpvclreport.pas',
  rprfvparams in '..\..\rprfvparams.pas' {FRpRunTimeParams},
  rpconsts in '..\..\rpconsts.pas',
  rpvpreview in '..\..\rpvpreview.pas' {FRpPreview},
  rpgdidriver in '..\..\rpgdidriver.pas' {FRpQtProgress},
  rpvgraphutils in '..\..\rpvgraphutils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
