unit Unit1;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, rpclxreport,  DB, IBDatabase, IBCustomDataSet, IBQuery,
  rpalias;

type
  TForm1 = class(TForm)
    CLXReport1: TCLXReport;
    Button1: TButton;
    IBDatabase1: TIBDatabase;
    IBQuery1: TIBQuery;
    IBTransaction1: TIBTransaction;
    RpAlias1: TRpAlias;
    procedure Button1Click(Sender: TObject);
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

end.
