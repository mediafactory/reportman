unit fabout;

interface

uses SysUtils, Classes, QGraphics, QForms, 
  QButtons, QExtCtrls, QControls, QStdCtrls;

type
  TFAboutBox = class(TForm)
    OKBtn: TButton;
    LReport: TLabel;
    Label1: TLabel;
    LName: TLabel;
    Label2: TLabel;
    LEmail: TLabel;
    Label3: TLabel;
    Image1: TImage;
    LVersion: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure ShowAbout;

implementation

procedure ShowAbout;
var dia:TFAboutBox;
begin
 dia:=TFAboutBox.Create(Application);
 try
  dia.ShowModal;
 finally
  dia.free;
 end;
end;


{$R *.xfm}

procedure TFAboutBox.FormCreate(Sender: TObject);
begin
 LReport.Font.Size:=20;
 LReport.Font.Style:=[fsBold];
 LName.Font.Style:=[fsBold];
 LVersion.Font.Size:=16;
 LEmail.Font.Style:=[fsBold];
end;

end.
