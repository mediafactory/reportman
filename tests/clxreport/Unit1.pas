unit Unit1;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, rpclxreport,  DB,rpvclreport;


type
  TForm1 = class(TForm)
    CLXReport1: TCLXReport;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    ETest: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.Button1Click(Sender: TObject);
begin

 CLXReport1.Execute;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 CLXReport1.SaveToPDF(ETest.text);
end;

end.
