unit umain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,rptwaincomp, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, StdCtrls, XPMan, Menus;

type
  TForm1 = class(TForm)
    XPManifest1: TXPManifest;
    MainMenu1: TMainMenu;
    Archivo1: TMenuItem;
    Empezar1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Empezar1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
   awebtwain:TRpTwainWeb;

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 awebtwain:=TRpTwainWeb.Create(Self);
 awebtwain.Align:=alClient;
 awebtwain.Parent:=self;
 awebtwain.BufferSize:=4096;
 awebtwain.ImageFormat:=2;
 awebtwain.GifDither:=1;
 awebtwain.JPegQuality:=100;
// awebtwain.GifColorReduction:=4;
 awebtwain.CompletedUrlPath:='http://reportman.dyndns.org/';
 awebtwain.UrlPath:='http://reportman.dyndns.org/upload/test4.gif';
end;

procedure TForm1.Empezar1Click(Sender: TObject);
begin
 awebtwain.Execute;
end;

end.
