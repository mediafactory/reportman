unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, rpcompobase, rpvclreport, DB, DBTables;

type
  TForm1 = class(TForm)
    Button1: TButton;
    VCLReport1: TVCLReport;
    Label1: TLabel;
    EReportname: TEdit;
    Table1: TTable;
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
 VCLReport1.Filename:=EReportname.Text;
 VCLReport1.Execute;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 EReportName.Text:='c:\prog\toni\cvsroot\reportman\reportman\repman\repsamples\sample5.rep';

end;

end.
