{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rppreview                                        }
{       Preview the report                              }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rppreview;

interface

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  windows,
{$ENDIF}
  Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls,rpreport,rpmetafile, QComCtrls,
  rpqtdriver, QExtCtrls,
  QActnList, QImgList,QPrinters,rpconsts,Qt;

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
    ASave: TAction;
    SaveDialog1: TSaveDialog;
    ToolButton7: TToolButton;
    ACancel: TAction;
    BCancel: TButton;
    PBar: TProgressBar;
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
    procedure BCancelClick(Sender: TObject);
    procedure ACancelExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    cancelled:boolean;
    procedure AppIdle(Sender:TObject;var done:boolean);
    procedure RepProgress(Sender:TRpReport;var docancel:boolean);
    procedure MetProgress(Sender:TRpMetafileReport;Position,Size:int64;page:integer);
    procedure DisableControls(enablebar:boolean);
    procedure EnableControls;
  public
    { Public declarations }
    pagenum:integer;
    report:TRpReport;
    qtdriver:TRpQtDriver;
    aqtdriver:IRpPrintDriver;
    bitmap:TBitmap;
    procedure PrintPage;
  end;


procedure ShowPreview(report:TRpReport;caption:string);

implementation

{$R *.xfm}

procedure ShowPreview(report:TRpReport;caption:string);
var
 dia:TFRpPreview;
 oldprogres:TRpProgressEvent;
begin
 dia:=TFRpPreview.Create(Application);
 try
  dia.caption:=caption;
  oldprogres:=report.OnProgress;
  try
   dia.report:=report;
   report.OnProgress:=dia.RepProgress;
   Application.OnIdle:=dia.AppIdle;
   dia.ShowModal;
  finally
   report.OnProgress:=oldprogres;
  end;
 finally
  dia.Free;
 end;
end;

procedure TFRpPreview.PrintPage;
begin
 try
  if report.Metafile.PageCount>=pagenum then
  begin
   report.Metafile.CurrentPage:=pagenum-1;
  end
  else
  begin
   if Not report.LastPage then
   begin
    cancelled:=false;
    DisableControls(false);
    try
     while report.Metafile.PageCount<pagenum do
     begin
      // Canbe canceled
      if report.PrintNextPage then
       break;
     end;
    finally
     EnableControls;
    end;
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
 except
  EPageNum.Text:='0';
  raise;
 end;
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
 if CalcReportWidthProgress(report) then
  PrintMetafile(report.Metafile,caption,true);
end;

procedure TFRpPreview.ASaveExecute(Sender: TObject);
var
 oldonprogress:TRpMetafileStreamProgres;
begin
 // Saves the metafile
 if SaveDialog1.Execute then
 begin
  ALast.Execute;
  oldonprogress:=report.Metafile.OnProgress;
  try
   report.Metafile.OnProgress:=MetProgress;
   DisableControls(true);
   try
    report.Metafile.SaveToFile(SaveDialog1.Filename);
   finally
    EnableControls;
   end;
  finally
   report.Metafile.OnProgress:=oldonprogress;
  end;
 end;
end;

procedure TFRpPreview.RepProgress(Sender:TRpReport;var docancel:boolean);
begin
 BCancel.Caption:=IntToStr(Sender.CurrentSubReportIndex)+' '+SRpPage+':'+
  FormatFloat('####,####',report.PageNum)+':'
  +FormatFloat('####,####',report.RecordCount)+'-'+SRpCancel;
{$IFDEF MSWINDOWS}
 if ((GetKeyState(VK_ESCAPE) AND $80)>0) then
  cancelled:=true;
{$ENDIF}
 Application.ProcessMessages;
 if cancelled then
  docancel:=true;
end;

procedure TFRpPreview.BCancelClick(Sender: TObject);
begin
 cancelled:=true;
end;

procedure TFRpPreview.ACancelExecute(Sender: TObject);
begin
 cancelled:=true;
end;

procedure TFRpPreview.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
 if Not ANext.Enabled then
 begin
  cancelled:=true;
 end;
end;

procedure TFRpPreview.FormKeyDown(Sender: TObject; var Key: Word;
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

procedure TFRpPreview.DisableControls(enablebar:boolean);
begin
 BCancel.Visible:=true;
 AFirst.Enabled:=false;
 ALast.Enabled:=false;
 APrint.Enabled:=false;
 ANext.Enabled:=false;
 APrevious.Enabled:=false;
 EPageNum.Enabled:=false;
 ASave.Enabled:=false;
 PBar.Position:=0;
 PBar.Visible:=enablebar;
end;

procedure TFRpPreview.EnableControls;
begin
 BCancel.Visible:=false;
 AFirst.Enabled:=true;
 ALast.Enabled:=true;
 APrint.Enabled:=true;
 ANext.Enabled:=true;
 APrevious.Enabled:=true;
 EPageNum.Enabled:=true;
 ASave.Enabled:=true;
 PBar.Visible:=false;
end;


procedure TFRpPreview.MetProgress(Sender:TRpMetafileReport;Position,Size:int64;page:integer);
begin
 BCancel.Caption:=SRpPage+':'+FormatFloat('####,#####',page)+
  ' -'+FormatFloat('######,####',Position div 1024)+SRpKbytes+' '+SrpCancel;
 if Position=size then
 begin
  PBar.Position:=page;
  PBar.Max:=Sender.PageCount;
 end
 else
 begin
  PBar.Position:=Position;
  PBar.Max:=Size;
 end;
 Application.ProcessMessages;
 if ((GetKeyState(VK_ESCAPE) AND $80)>0) then
  cancelled:=true;
 if cancelled then
  Raise Exception.Create(SRpOperationAborted);
end;


end.
