unit rppreview;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls,rpreport,rpmetafile,rpprintrep, QComCtrls,rpqtdriver, QExtCtrls;

type
  TFRpPreview = class(TForm)
    BToolBar: TToolBar;
    ImageContainer: TScrollBox;
    AImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure AppIdle(Sender:TObject;var done:boolean);
  public
    { Public declarations }
    report:TRpReport;
    metafile:TrpMetafileReport;
    qtdriver:TRpQtDriver;
    bitmap:TBitmap;
  end;


procedure ShowPreview(report:TRpReport);

implementation

{$R *.xfm}

procedure ShowPreview(report:TRpReport);
var
 dia:TFRpPreview;
begin
 dia:=TFRpPreview.Create(Application);
 try
  dia.report:=report;
  Application.OnIdle:=dia.AppIdle;
  dia.ShowModal;
 finally
  dia.Free;
 end;
end;

procedure TFRpPreview.AppIdle(Sender:TObject;var done:boolean);
begin
 Application.OnIdle:=nil;
 done:=false;
 PrintReport(report,metafile);
 metafile.DrawPage(qtdriver);
 if Assigned(qtdriver.bitmap) then
 begin
  AImage.Width:=qtdriver.bitmap.Width;
  AImage.Height:=qtdriver.bitmap.Height;
  AImage.Picture.Bitmap.Assign(qtdriver.bitmap);
  AImage.Invalidate;
 end;
end;

procedure TFRpPreview.FormCreate(Sender: TObject);
begin
 metafile:=TrpMetafileReport.Create(Self);
 qtdriver:=TRpQtDriver.Create;
// qtdriver.toprinter:=true;
 bitmap:=TBitmap.Create;
 bitmap.PixelFormat:=pf32bit;
 AImage.Picture.Bitmap:=bitmap;
end;

procedure TFRpPreview.FormDestroy(Sender: TObject);
begin
 bitmap.free;
end;

end.
