unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,rpwebmetaclient, StdCtrls;

const
 TestString='http://localhost/cgi-bin/repwebexe.exe/execute?reportname=%5Csample6&aliasname=TEST2&username=Admin&password=&ParamDETAIL=True&ParamFIRSTORDER=1000&ParamLASTORDER=1010&METAFILE=1';

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
 rpwebmetaclient.PrintHttpReport(Edit1.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 Edit1.Text:=TestString;
end;

end.
