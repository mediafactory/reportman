unit rppreview;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls,rpreport,rpmetafile, QComCtrls,rpqtdriver, QExtCtrls,
  QActnList, QImgList,QPrinters;

type
  TFRpPreview = class(TForm)
    BToolBar: TToolBar;
    ImageContainer: TScrollBox;
    AImage: TImage;
    ImageList1: TImageList;
    ActionList1: TActionList;
    AFirst: TAction;
    APrevious: TAction;
    ANext: TAction;
    ALast: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    EPageNum: TEdit;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    APrint: TAction;
    ToolButton6: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AFirstExecute(Sender: TObject);
    procedure ANextExecute(Sender: TObject);
    procedure APreviousExecute(Sender: TObject);
    procedure ALastExecute(Sender: TObject);
    procedure EPageNumKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure APrintExecute(Sender: TObject);
  private
    { Private declarations }
    procedure AppIdle(Sender:TObject;var done:boolean);
  public
    { Public declarations }
    pagenum:integer;
    report:TRpReport;
    qtdriver:TRpQtDriver;
    aqtdriver:IRpPrintDriver;
    bitmap:TBitmap;
    procedure PrintPage;
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

procedure TFRpPreview.PrintPage;
begin
 EPageNum.Text:='0';
 if report.Metafile.PageCount>=pagenum then
 begin
  report.Metafile.CurrentPage:=pagenum-1;
 end
 else
 begin
  while report.Metafile.PageCount<pagenum do
  begin
   if report.LastPage then
    break;
   if Not report.PrintNextPage then
    break;
  end;
  if report.Metafile.PageCount<pagenum then
   pagenum:=report.Metafile.PageCount;
  report.Metafile.CurrentPage:=pagenum-1;
 end;
 report.metafile.DrawPage(qtdriver);
 if Assigned(qtdriver.bitmap) then
 begin
  AImage.Width:=qtdriver.bitmap.Width;
  AImage.Height:=qtdriver.bitmap.Height;
  AImage.Picture.Bitmap.Assign(qtdriver.bitmap);
  AImage.Invalidate;
 end;
 EPageNum.Text:=IntToStr(PageNum);
end;

procedure TFRpPreview.AppIdle(Sender:TObject;var done:boolean);
begin
 Application.OnIdle:=nil;
 done:=false;
 report.BeginPrint;
 pagenum:=1;
 PrintPage;
end;

procedure TFRpPreview.FormCreate(Sender: TObject);
begin
 qtdriver:=TRpQtDriver.Create;
 aqtdriver:=qtdriver;
// qtdriver.toprinter:=true;
 bitmap:=TBitmap.Create;
 bitmap.PixelFormat:=pf32bit;
 AImage.Picture.Bitmap:=bitmap;
end;

procedure TFRpPreview.FormDestroy(Sender: TObject);
begin
 report.EndPrint;
 bitmap.free;
end;

procedure TFRpPreview.AFirstExecute(Sender: TObject);
begin
 pagenum:=1;
 PrintPage;
end;

procedure TFRpPreview.ANextExecute(Sender: TObject);
begin
 inc(pagenum);
 PrintPage;
end;

procedure TFRpPreview.APreviousExecute(Sender: TObject);
begin
 dec(pagenum);
 if pagenum<1 then
  pagenum:=1;
 PrintPage;
end;

procedure TFRpPreview.ALastExecute(Sender: TObject);
begin
 pagenum:=MaxInt;
 PrintPage;
end;

procedure TFRpPreview.EPageNumKeyPress(Sender: TObject; var Key: Char);
begin
 if Key=chr(13) then
 begin
  pagenum:=StrToInt(EPageNum.Text);
  PrintPage;
 end;
end;

procedure TFRpPreview.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 qtdriver:=nil;
end;

procedure TFRpPreview.APrintExecute(Sender: TObject);
begin
 while not report.LastPage do
 begin
  report.PrintNextPage;
 end;
 // Prints the report
 Printer.ExecuteSetup;
 qtdriver.toprinter:=true;
 try
  report.Metafile.DrawAll(QtDriver);
 finally
  qtdriver.toprinter:=false;
 end;
end;

end.
