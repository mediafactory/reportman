unit fmetaview;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls,rpmetafile, QComCtrls,rpqtdriver, QExtCtrls,
  QActnList, QImgList,QPrinters,Qt;

type
  TFMeta = class(TForm)
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
    ASave: TAction;
    SaveDialog1: TSaveDialog;
    ToolButton7: TToolButton;
    OpenDialog1: TOpenDialog;
    AOpen: TAction;
    ToolButton8: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AFirstExecute(Sender: TObject);
    procedure ANextExecute(Sender: TObject);
    procedure APreviousExecute(Sender: TObject);
    procedure ALastExecute(Sender: TObject);
    procedure EPageNumKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure APrintExecute(Sender: TObject);
    procedure ASaveExecute(Sender: TObject);
    procedure AOpenExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    pagenum:integer;
    metafile:TRpMetafileReport;
    qtdriver:TRpQtDriver;
    aqtdriver:IRpPrintDriver;
    bitmap:TBitmap;
    procedure PrintPage;
  end;

var
 FMeta:TFMeta;

implementation

{$R *.xfm}


procedure TFMeta.PrintPage;
begin
 EPageNum.Text:='0';
 if pagenum>Metafile.PageCount then
 begin
  pagenum:=Metafile.PageCount;
 end;
 Metafile.CurrentPage:=pagenum-1;
 metafile.DrawPage(qtdriver);
 if Assigned(qtdriver.bitmap) then
 begin
  AImage.Width:=qtdriver.bitmap.Width;
  AImage.Height:=qtdriver.bitmap.Height;
  AImage.Picture.Bitmap.Assign(qtdriver.bitmap);
  AImage.Invalidate;
 end;
 pagenum:=Metafile.CurrentPage+1;
 EPageNum.Text:=IntToStr(PageNum);
end;

procedure TFMeta.FormCreate(Sender: TObject);
begin
 APrevious.ShortCut:=Key_PageUp;
 ANext.ShortCut:=Key_PageDown;
 AFirst.ShortCut:=Key_Home;
 ALast.ShortCut:=Key_End;
 qtdriver:=TRpQtDriver.Create;
 aqtdriver:=qtdriver;
// qtdriver.toprinter:=true;
 bitmap:=TBitmap.Create;
 bitmap.PixelFormat:=pf32bit;
 AImage.Picture.Bitmap:=bitmap;
 metafile:=TrpMetafileReport.Create(Self);
end;

procedure TFMeta.FormDestroy(Sender: TObject);
begin
 bitmap.free;
end;

procedure TFMeta.AFirstExecute(Sender: TObject);
begin
 pagenum:=1;
 PrintPage;
end;

procedure TFMeta.ANextExecute(Sender: TObject);
begin
 inc(pagenum);
 PrintPage;
end;

procedure TFMeta.APreviousExecute(Sender: TObject);
begin
 dec(pagenum);
 if pagenum<1 then
  pagenum:=1;
 PrintPage;
end;

procedure TFMeta.ALastExecute(Sender: TObject);
begin
 pagenum:=MaxInt;
 PrintPage;
end;

procedure TFMeta.EPageNumKeyPress(Sender: TObject; var Key: Char);
begin
 if Key=chr(13) then
 begin
  pagenum:=StrToInt(EPageNum.Text);
  PrintPage;
 end;
end;

procedure TFMeta.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 qtdriver:=nil;
end;

procedure TFMeta.APrintExecute(Sender: TObject);
begin
 // Prints the report
 PrintMetafile(metafile,opendialog1.FileName,true);
end;

procedure TFMeta.ASaveExecute(Sender: TObject);
begin
 // Saves the metafile
 if SaveDialog1.Execute then
 begin
  Metafile.SaveToFile(SaveDialog1.Filename);
 end;
end;

procedure TFMeta.AOpenExecute(Sender: TObject);
begin
 if OpenDialog1.Execute then
 begin
  metafile.LoadFromFile(OpenDialog1.Filename);
  ASave.Enabled:=True;
  APrint.Enabled:=True;
  AFirst.Enabled:=True;
  APrevious.Enabled:=True;
  ANext.Enabled:=True;
  ALast.Enabled:=True;
  pagenum:=1;
  PrintPage;
 end;
end;

procedure TFMeta.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
 increment:integer;
begin
 if (ssShift in Shift) then
  increment:=1
 else
  increment:=ImageContainer.VertScrollBar.Increment;
 if Key=Key_Down then
 begin
  if ImageContainer.VertScrollBar.Position+increment>ImageContainer.VertScrollBar.Range-ImageContainer.Height then
   ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Range-ImageContainer.Height+increment
  else
   ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Position+Increment;
 end;
 if Key=Key_Up then
 begin
  ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Position-Increment;
 end;
 if Key=Key_Right then
 begin
  if ImageContainer.HorzScrollBar.Position+increment>ImageContainer.HorzScrollBar.Range-ImageContainer.Width then
   ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Range-ImageContainer.Width+increment
  else
   ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Position+Increment;
 end;
 if Key=Key_Left then
 begin
  ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Position-Increment;
 end;
end;

end.
