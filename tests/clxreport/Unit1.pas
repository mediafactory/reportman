unit Unit1;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, rpclxreport,  DB,
{$IFDEF MSWINDOWS}
  rpvclreport,
{$ENDIF}
  rpcompobase, rpmdesigner;


type
  TForm1 = class(TForm)
    CLXReport1: TCLXReport;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    ETest: TEdit;
    RpDesigner1: TRpDesigner;
    BDesign: TButton;
    EReportName: TEdit;
    Label2: TLabel;
    BPrint: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BDesignClick(Sender: TObject);
    procedure BPrintClick(Sender: TObject);
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
 CLXReport1.Filename:=EReportName.Text;
 CLXReport1.Preview:=True;
 CLXReport1.Execute;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 CLXReport1.SaveToPDF(ETest.text,false);
end;

procedure TForm1.BDesignClick(Sender: TObject);
begin
 RpDesigner1.Filename:=EReportName.Text;
 RpDesigner1.Execute;
end;

procedure TForm1.BPrintClick(Sender: TObject);
begin
 CLXReport1.Filename:=EReportName.Text;
 CLXReport1.Preview:=False;
 CLXReport1.Execute;
end;

end.
