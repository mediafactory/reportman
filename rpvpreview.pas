{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpvpreview                                      }
{       VCL Preview the report                          }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2003 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

// Implement page size and showprint dialog and font style

unit rpvpreview;

interface

{$I rpconf.inc}

uses
  SysUtils,
  windows,
{$IFDEF USEVARIANTS}
  Types,
{$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,rpbasereport,rpreport,rpmetafile, ComCtrls,rphtmldriver,
  rpgdidriver, ExtCtrls,Menus,rptypes,rpexceldriver,rptextdriver,
  ActnList, ImgList,Printers,rpmdconsts, ToolWin;

type
  TFRpVPreview = class(TForm)
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
    APrint: TAction;
    ToolButton6: TToolButton;
    ASave: TAction;
    SaveDialog1: TSaveDialog;
    ToolButton7: TToolButton;
    ACancel: TAction;
    BCancel: TButton;
    PBar: TProgressBar;
    AExit: TAction;
    BExit: TToolButton;
    ToolButton8: TToolButton;
    AParams: TAction;
    AScale100: TAction;
    AScaleWide: TAction;
    AScaleFull: TAction;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    AScaleLess: TAction;
    AScaleMore: TAction;
    ToolButton15: TToolButton;
    ToolButton5: TToolButton;
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
    procedure AExitExecute(Sender: TObject);
    procedure AParamsExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure AScale100Execute(Sender: TObject);
    procedure AScaleWideExecute(Sender: TObject);
    procedure AScaleFullExecute(Sender: TObject);
    procedure AScaleLessExecute(Sender: TObject);
    procedure AScaleMoreExecute(Sender: TObject);
    procedure AImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }
    cancelled:boolean;
    printed:boolean;
    enableparams:boolean;
    procedure AppIdle(Sender:TObject;var done:boolean);
    procedure RepProgress(Sender:TRpBaseReport;var docancel:boolean);
    procedure MetProgress(Sender:TRpMetafileReport;Position,Size:int64;page:integer);
    procedure DisableControls(enablebar:boolean);
    procedure EnableControls;
    procedure PlaceImagePosition;
  public
    { Public declarations }
    pagenum:integer;
    report:TRpReport;
    gdidriver:TRpgdidriver;
    agdidriver:IRpPrintDriver;
    bitmap:TBitmap;
    procedure PrintPage;
  end;


function ShowPreview(report:TRpReport;caption:string):boolean;

implementation

uses rprfvparams, rppdfdriver;

{$R *.dfm}

function ShowPreview(report:TRpReport;caption:string):boolean;
var
 dia:TFRpVPreview;
 oldprogres:TRpProgressEvent;
 hasparams:boolean;
 i:integer;
begin
 dia:=TFRpVPreview.Create(Application);
 try
  dia.caption:=caption;
  oldprogres:=report.OnProgress;
  try
   dia.report:=report;
   hasparams:=false;
   for i:=0 to report.params.count-1 do
   begin
    if report.params.items[i].Visible then
    begin
     hasparams:=true;
     break;
    end;
   end;
   dia.AParams.Enabled:=hasparams;
   dia.enableparams:=hasparams;
   dia.AScale100.Checked:=False;
   dia.AScaleFull.Checked:=False;
   dia.AScaleWide.Checked:=False;
   case report.PreviewStyle of
    spNormal:
     begin
      dia.AScale100.Checked:=True;
      dia.gdidriver.PreviewStyle:=spNormal;
     end;
    spEntirePage:
     begin
      dia.AScaleFull.Checked:=True;
      dia.gdidriver.PreviewStyle:=spEntirePage;
     end
    else
      dia.AScaleWide.Checked:=True;
   end;
   if report.PreviewWindow=spwMaximized then
    dia.WindowState:=wsMaximized;
   report.OnProgress:=dia.RepProgress;
   Application.OnIdle:=dia.AppIdle;
   dia.ShowModal;
   Result:=dia.printed;
  finally
   report.OnProgress:=oldprogres;
  end;
 finally
  dia.Free;
 end;
end;

procedure TFRpVPreview.PrintPage;
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
      begin
       report.EndPrint;
       break;
      end;
     end;
    finally
     EnableControls;
    end;
   end;
   if report.Metafile.PageCount<pagenum then
    pagenum:=report.Metafile.PageCount;
   report.Metafile.CurrentPage:=pagenum-1;
  end;
  report.metafile.DrawPage(gdidriver);
  if Assigned(gdidriver.bitmap) then
  begin
   AImage.Width:=Round(gdidriver.bitmap.Width);
   AImage.Height:=Round(gdidriver.bitmap.Height);
   AImage.Picture.Bitmap.Assign(gdidriver.bitmap);
   AImage.Invalidate;
  end;
  EPageNum.Text:=IntToStr(PageNum);
 except
  EPageNum.Text:='0';
  raise;
 end;
end;

procedure TFRpVPreview.AppIdle(Sender:TObject;var done:boolean);
begin
 Application.OnIdle:=nil;
 done:=false;
 try
  DisableControls(false);
  report.OnProgress:=RepProgress;
  gdidriver.lockedpagesize:=false;
  if report.TwoPass then
  begin
   if Not CalcReportWidthProgress(report) then
    Abort;
  end
  else
  begin
   report.BeginPrint(gdidriver);
  end;
  if report.PrinterFonts=rppfontsalways then
   gdidriver.devicefonts:=true
  else
   gdidriver.devicefonts:=false;
  pagenum:=1;
  gdidriver.NewDocument(report.Metafile,1,false);
  gdidriver.lockedpagesize:=true;
  PrintPage;
  PlaceImagePosition;
  printed:=true;
  EnableControls;
 except
  on E:Exception do
  begin
   Close;
   Raise;
  end;
 end;
end;

procedure TFRpVPreview.FormCreate(Sender: TObject);
begin
 SaveDialog1.Filter:=SRpRepMetafile+'|*.rpmf|'+
   SRpPDFFile+'|*.pdf|'+
   SRpPDFFileUn+'|*.pdf|'+
   SRpExcelFile+'|*.xls|'+
   SRpPlainFile+'|*.txt|'+
   SRpBitmapFile+'|*.bmp|'+
   SRpBitmapFileMono+'|*.bmp|'+
   SRpHtmlFile+'|*.html';
{$IFNDEF DOTNETD}
  SaveDialog1.Filter:=SaveDialog1.Filter+
    '|'+SRpExeMetafile+'|*.exe';
{$ENDIF}
 APrevious.ShortCut:=ShortCut(VK_PRIOR, []);
 ANext.ShortCut:=ShortCut(VK_NEXT, []);
 AFirst.ShortCut:=ShortCut(VK_HOME, []);
 ALast.ShortCut:=ShortCut(VK_END, []);
 gdidriver:=TRpgdidriver.Create;
 agdidriver:=gdidriver;
 bitmap:=TBitmap.Create;
 {$IFNDEF DOTNETDBUGS}
   bitmap.PixelFormat:=pf32bit;
 {$ENDIF}
 {$IFDEF DOTNETDBUGS}
//   bitmap.PixelFormat:=pf24bit;
//   bitmap.HandleType:=bmDIB;
 {$ENDIF}
 AImage.Picture.Bitmap:=bitmap;

 Caption:=TranslateStr(215,Caption);
 SaveDialog1.Title:=TranslateStr(216,SaveDialog1.Title);
 ACancel.Caption:=TranslateStr(94,ACancel.Caption);
 ACancel.Hint:=TranslateStr(218,ACancel.Hint);
 APrint.Caption:=TranslateStr(52,APrint.Caption);
 APrint.Hint:=TranslateStr(53,APrint.Hint);
 ASave.Caption:=TranslateStr(46,ASave.Caption);
 ASave.Hint:=TranslateStr(217,ASave.Hint);
 AExit.Caption:=TranslateStr(44,AExit.Caption);
 AExit.Hint:=TranslateStr(219,AExit.Hint);
 AParams.Caption:=TranslateStr(135,Aparams.Caption);
 AParams.Hint:=TranslateStr(136,Aparams.Hint);
 AFirst.Caption:=TranslateStr(220,AFirst.Caption);
 AFirst.Hint:=TranslateStr(221,AFirst.Hint);
 APrevious.Caption:=TranslateStr(222,APrevious.Caption);
 APrevious.Hint:=TranslateStr(223,APrevious.Hint);
 ANext.Caption:=TranslateStr(224,ANext.Caption);
 ANext.Hint:=TranslateStr(225,ANext.Hint);
 ALast.Caption:=TranslateStr(226,ALast.Caption);
 ALast.Hint:=TranslateStr(227,ALast.Hint);
 AScale100.Caption:=TranslateStr(228,AScale100.Caption);
 AScale100.Hint:=TranslateStr(229,AScale100.Hint);
 AScaleWide.Caption:=TranslateStr(230,AScaleWide.Caption);
 AScaleWide.Hint:=TranslateStr(231,AScaleWide.Hint);
 AScaleFull.Caption:=TranslateStr(232,AScaleFull.Caption);
 AScaleFull.Hint:=TranslateStr(233,AScaleFull.Hint);
 AScaleLess.Caption:=TranslateStr(234,AScaleLess.Caption);
 AScaleLess.Hint:=TranslateStr(235,AScaleLess.Hint);
 AScaleMore.Caption:=TranslateStr(236,AScaleMore.Caption);
 AScaleMore.Hint:=TranslateStr(237,AScaleMore.Hint);
end;

procedure TFRpVPreview.FormDestroy(Sender: TObject);
begin
 report.EndPrint;
 bitmap.free;
 gdidriver.free;
end;

procedure TFRpVPreview.AFirstExecute(Sender: TObject);
begin
 pagenum:=1;
 PrintPage;
end;

procedure TFRpVPreview.ANextExecute(Sender: TObject);
begin
 inc(pagenum);
 PrintPage;
end;

procedure TFRpVPreview.APreviousExecute(Sender: TObject);
begin
 dec(pagenum);
 if pagenum<1 then
  pagenum:=1;
 PrintPage;
end;

procedure TFRpVPreview.ALastExecute(Sender: TObject);
begin
 pagenum:=MaxInt;
 PrintPage;
end;

procedure TFRpVPreview.EPageNumKeyPress(Sender: TObject; var Key: Char);
begin
 if Key=chr(13) then
 begin
  pagenum:=StrToInt(EPageNum.Text);
  PrintPage;
 end;
end;

procedure TFRpVPreview.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 gdidriver:=nil;
end;

procedure TFRpVPreview.APrintExecute(Sender: TObject);
var
 adone:boolean;
 allpages,collate:boolean;
 frompage,topage,copies:integer;
begin
 allpages:=true;
 collate:=report.CollateCopies;
 frompage:=1; topage:=999999;
 copies:=report.Copies;
 if Not DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
  exit;
 ALastExecute(Self);
 PrintMetafile(report.Metafile,Caption,true,allpages,frompage,topage,copies,
 collate,false,report.Metafile.PrinterSelect);
  // report.EndPrint;
// PrintReport(report,Caption,true,allpages,frompage,topage,copies,collate);
 AppIdle(Self,adone);
end;

procedure TFRpVPreview.ASaveExecute(Sender: TObject);
var
 oldonprogress:TRpMetafileStreamProgres;
 adone:boolean;
 abitmap:TBitmap;
begin
 // Saves the metafile
 if SaveDialog1.Execute then
 begin
  oldonprogress:=report.Metafile.OnProgress;
  try
   report.Metafile.OnProgress:=MetProgress;
   DisableControls(true);
   try
    case SaveDialog1.FilterIndex of
     1:
     begin
      ALastExecute(Self);
      report.Metafile.SaveToFile(SaveDialog1.Filename)
     end;
     2,3:
      begin
       ALastExecute(Self);
       SaveMetafileToPDF(report.Metafile,SaveDialog1.FileName,SaveDialog1.FilterIndex=2);
//    report.endprint
//      ExportReportToPDF(report,SaveDialog1.Filename,true,true,1,9999999,
//       true,SaveDialog1.Filename,SaveDialog1.FilterIndex=2);
       AppIdle(Self,adone);
      end;
     4:
      begin
       ALastExecute(Self);
       ExportMetafileToExcel(report.Metafile,SaveDialog1.FileName,
        true,false,true,1,9999);
       AppIdle(Self,adone);
      end;
     6,7:
      begin
       ALastExecute(Self);
       abitmap:=MetafileToBitmap(report.Metafile,true,SaveDialog1.FilterIndex=7);
       try
        if assigned(abitmap) then
         abitmap.SaveToFile(SaveDialog1.FileName);
       finally
        abitmap.free;
       end;
      end;
     8:
      begin
       ALastExecute(Self);
       ExportMetafileToHtml(report.Metafile,Caption,SaveDialog1.FileName,
        true,true,1,9999);
       AppIdle(Self,adone);
      end;
{$IFNDEF DOTNETD}
     9:
      begin
       ALastExecute(Self);
       MetafileToExe(report.metafile,SaveDialog1.Filename);
      end;
{$ENDIF}
     else
     begin
      ALastExecute(Self);
      SaveMetafileToTextFile(report.Metafile,SaveDialog1.FileName);
      AppIdle(Self,adone);
     end;
    end;
   finally
    EnableControls;
   end;
  finally
   report.Metafile.OnProgress:=oldonprogress;
  end;
 end;
end;

procedure TFRpVPreview.RepProgress(Sender:TRpBaseReport;var docancel:boolean);
begin
 BCancel.Caption:=IntToStr(Sender.CurrentSubReportIndex)+' '+SRpPage+':'+
  FormatFloat('####,####',report.PageNum)+':'
  +FormatFloat('####,####',report.RecordCount)+'-'+SRpCancel;
{$IFDEF MSWINDOWS}
 if ((GetAsyncKeyState(VK_ESCAPE) AND $8000)<>0) then
  cancelled:=true;
{$ENDIF}
 Application.ProcessMessages;
 if cancelled then
  docancel:=true;
end;

procedure TFRpVPreview.BCancelClick(Sender: TObject);
begin
 cancelled:=true;
end;

procedure TFRpVPreview.ACancelExecute(Sender: TObject);
begin
 cancelled:=true;
end;

procedure TFRpVPreview.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
 if Not ANext.Enabled then
 begin
  cancelled:=true;
 end;
end;

procedure TFRpVPreview.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
 increment:integer;
begin
 if (ssShift in Shift) then
  increment:=1
 else
  increment:=ImageContainer.VertScrollBar.Increment;
 if Key=VK_DOWN then
 begin
  if ImageContainer.VertScrollBar.Position+increment>ImageContainer.VertScrollBar.Range-ImageContainer.Height then
   ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Range-ImageContainer.Height+increment
  else
   ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Position+Increment;
 end;
 if Key=VK_UP then
 begin
  ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Position-Increment;
 end;
 if Key=VK_RIGHT then
 begin
  if ImageContainer.HorzScrollBar.Position+increment>ImageContainer.HorzScrollBar.Range-ImageContainer.Width then
   ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Range-ImageContainer.Width+increment
  else
   ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Position+Increment;
 end;
 if Key=VK_LEFT then
 begin
  ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Position-Increment;
 end;
 if Key=VK_SPACE then
 begin
  AImageMouseDown(Self,mbLeft,[],0,0);
  Key:=0;
 end;
end;

procedure TFRpVPreview.DisableControls(enablebar:boolean);
begin
 BCancel.Left:=BExit.Left+BExit.Width;
 BCancel.Visible:=true;
 AScale100.Enabled:=false;
 AScaleFull.Enabled:=false;
 AScaleWide.Enabled:=false;
 AScaleLess.Enabled:=False;
 AScaleMore.Enabled:=False;
 AFirst.Enabled:=false;
 ALast.Enabled:=false;
 APrint.Enabled:=false;
 ANext.Enabled:=false;
 APrevious.Enabled:=false;
 EPageNum.Enabled:=false;
 ASave.Enabled:=false;
 AParams.Enabled:=false;
 PBar.Position:=0;
 PBar.Visible:=enablebar;
 AExit.Enabled:=false;
end;

procedure TFRpVPreview.EnableControls;
begin
 BCancel.Visible:=false;
 AScale100.Enabled:=true;
 AScaleFull.Enabled:=true;
 AScaleWide.Enabled:=true;
 AScaleLess.Enabled:=true;
 AScaleMore.Enabled:=true;
 AFirst.Enabled:=true;
 ALast.Enabled:=true;
 APrint.Enabled:=true;
 ANext.Enabled:=true;
 APrevious.Enabled:=true;
 EPageNum.Enabled:=true;
 ASave.Enabled:=true;
 AParams.Enabled:=enableparams;
 PBar.Visible:=false;
 AExit.Enabled:=true;
end;


procedure TFRpVPreview.MetProgress(Sender:TRpMetafileReport;Position,Size:int64;page:integer);
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
{$IFDEF MSWINDOWS}
 if ((GetAsyncKeyState(VK_ESCAPE) AND $8000)<>0) then
  cancelled:=true;
{$ENDIF}
 if cancelled then
  Raise Exception.Create(SRpOperationAborted);
end;


procedure TFRpVPreview.AExitExecute(Sender: TObject);
begin
 Close;
end;

procedure TFRpVPreview.AParamsExecute(Sender: TObject);
var
 adone:boolean;
begin
 if ShowUserParams(report.params) then
 begin
  // Reexecutes the report
  AppIdle(Self,adone);
 end;
end;

procedure TFRpVPreview.PlaceImagePosition;
var
 AWidth:integeR;
 Aheight:integer;
begin
 AWidth:=ImageContainer.Width-GetSystemMetrics(SM_CYHSCROLL);
 AHeight:=ImageContainer.Height-GetSystemMetrics(SM_CXHSCROLL);

 if AImage.Width>AWidth then
  AImage.Left:=-ImageContainer.HorzScrollBar.Position
 else
  AImage.Left:=((AWidth-AImage.Width) div 2)-ImageContainer.HorzScrollBar.Position;
 if AImage.Height>AHeight then
  AImage.Top:=-ImageContainer.VertScrollBar.Position
 else
  AImage.Top:=((AHeight-AImage.Height) div 2)-ImageContainer.VertScrollBar.Position;
end;


procedure TFRpVPreview.FormResize(Sender: TObject);
begin
 if BCancel.Visible then
  exit;
 // Sets the driver widths and redraw accordingly
 AScaleFull.Checked:=false;
 AScaleWide.Checked:=false;
 AScale100.Checked:=false;
 if Assigned(gdidriver) then
 begin
  gdidriver.clientwidth:=ImageContainer.Width;
  gdidriver.clientHeight:=ImageContainer.Height;
  case gdidriver.PreviewStyle of
   spWide:
    AScaleWide.Checked:=True;
   spEntirePage:
    AScaleFull.Checked:=True;
   spNormal:
    AScale100.Checked:=True;
  end;
  if pagenum>=1 then
   PrintPage;
  if pagenum>=1 then
   PlaceImagePosition;
 end;
end;

procedure TFRpVPreview.AScale100Execute(Sender: TObject);
begin
 gdidriver.PreviewStyle:=spNormal;
 FormResize(Self);
end;

procedure TFRpVPreview.AScaleWideExecute(Sender: TObject);
begin
 gdidriver.PreviewStyle:=spWide;
 FormResize(Self);
end;

procedure TFRpVPreview.AScaleFullExecute(Sender: TObject);
begin
 gdidriver.PreviewStyle:=spEntirePage;
 FormResize(Self);
end;

procedure TFRpVPreview.AScaleLessExecute(Sender: TObject);
begin
 gdidriver.PreviewStyle:=spCustom;
 gdidriver.Scale:=gdidriver.scale-0.10;
 FormResize(Self);
end;

procedure TFRpVPreview.AScaleMoreExecute(Sender: TObject);
begin
 gdidriver.PreviewStyle:=spCustom;
 gdidriver.Scale:=gdidriver.scale+0.10;
 FormResize(Self);
end;

procedure TFRpVPreview.AImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 relx:Extended;
 rely:Extended;
 posx,migx:Extended;
 posy,migy:Extended;
 punt:Tpoint;
begin
 // When clic in image scale to 100% and scroll to the
 // clicked section
 if gdidriver.PreviewStyle=spEntirePage then
 begin
  punt.X:=X;
  punt.y:=Y;
  relx:=punt.X;
  rely:=punt.Y;
  relx:=relx/AImage.Width;
  rely:=rely/AImage.Height;
  AScale100.Execute;
  // looks the limit
  posx:=ImageContainer.HorzScrollBar.Range*relx;
  posy:=ImageContainer.VertScrollBar.Range*rely;
  // To the center
  Migx:=PosX-(ImageContainer.ClientWidth div 2);
  Migy:=PosY-(ImageContainer.ClientHeight div 2);

  ImageContainer.HorzScrollBar.Position:=Trunc(migX);
  ImageContainer.VertScrollBar.Position:=Trunc(MigY);
 end
 else
  AScaleFull.Execute;
end;

procedure TFRpVPreview.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
 if (ssCtrl in Shift) then
  ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Position+GetWheelInc(Shift)
 else
  ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Position+GetWheelInc(Shift);
 Handled:=true;
end;

procedure TFRpVPreview.FormMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
 if (ssCtrl in Shift) then
  ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Position-GetWheelInc(Shift)
 else
  ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Position-GetWheelInc(Shift);
 Handled:=true;
end;

end.
