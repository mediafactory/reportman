unit Unit1;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls,rpgraphutils, QComCtrls;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    Button1: TButton;
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
 FillTreeView('c:\prog\toni\cvsroot\reportman\reportman',TreeView1.Items,TreeView1.TopItem,'*.rep');
end;

end.
