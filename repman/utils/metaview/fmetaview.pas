{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Metaview                                        }
{       TFMetaView                                      }
{       A form to view, print and export                }
{        report metafiles                               }
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

unit fmetaview;

interface

{$I rpconf.inc}

uses
  SysUtils,Inifiles,
{$IFDEF MSWINDOWS}
  Windows,Dialogs,rpgdidriver,
{$ENDIF}
  Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls,rpmetafile, QComCtrls,rpqtdriver, QExtCtrls,rpmdclitree,
  QActnList, QImgList,QPrinters,Qt,rpmdconsts,rptypes, QMenus,
  rpmdfabout,QTypes,QStyle,rpmdshfolder,rpmdprintconfig,
  rpmdfhelpform;

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
    APrint: TAction;
    ToolButton6: TToolButton;
    ASave: TAction;
    ToolButton7: TToolButton;
    OpenDialog1: TOpenDialog;
    AOpen: TAction;
    ToolButton8: TToolButton;
    BCancel: TButton;
    PBar: TProgressBar;
    AExit: TAction;
    AScale100: TAction;
    AScaleWide: TAction;
    AScaleFull: TAction;
    AScaleLess: TAction;
    AScaleMore: TAction;
    ToolButton10: TToolButton;
    SaveDialog1: TSaveDialog;
    ToolButton5: TToolButton;
    ToolButton9: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Page1: TMenuItem;
    Firstpage1: TMenuItem;
    Print1: TMenuItem;
    Nextpage1: TMenuItem;
    Lastpage1: TMenuItem;
    View1: TMenuItem;
    Normalscale1: TMenuItem;
    Normalscale2: TMenuItem;
    Scaletowindow1: TMenuItem;
    N2: TMenuItem;
    Print2: TMenuItem;
    ACancel: TAction;
    Splitter1: TSplitter;
    MHelp: TMenuItem;
    AAbout: TAction;
    AAbout1: TMenuItem;
    AViewConnect: TAction;
    ReportConnection1: TMenuItem;
    MPreferences: TMenuItem;
    MQtStyle: TMenuItem;
    AStatusBar: TAction;
    BStatus: TStatusBar;
    ASystemPrintDialog: TAction;
    Windows1: TMenuItem;
    Motif1: TMenuItem;
    MotifPlus1: TMenuItem;
    CDE1: TMenuItem;
    QtSGI1: TMenuItem;
    Platinum1: TMenuItem;
    MQtDefault: TMenuItem;
    ADriverQt: TAction;
    ADriverGDI: TAction;
    MDriverSelect: TMenuItem;
    WindowsGDIDriver1: TMenuItem;
    QtDriver1: TMenuItem;
    ADocumentation: TAction;
    StatusBar1: TMenuItem;
    QtSystemPrintDialog1: TMenuItem;
    Documentation1: TMenuItem;
    APrintSetup: TAction;
    PrinterSetup1: TMenuItem;
    MSelectPrinter: TMenuItem;
    MSelPrinter0: TMenuItem;
    MSelPrinter1: TMenuItem;
    MSelPrinter2: TMenuItem;
    MSelPrinter3: TMenuItem;
    MSelPrinter4: TMenuItem;
    MSelPrinter5: TMenuItem;
    MSelPrinter6: TMenuItem;
    MSelPrinter7: TMenuItem;
    MSelPrinter8: TMenuItem;
    MSelPrinter9: TMenuItem;
    MSelPrinter10: TMenuItem;
    MSelPrinter11: TMenuItem;
    MSelPrinter12: TMenuItem;
    MSelPrinter13: TMenuItem;
    MSelPrinter14: TMenuItem;
    MSelPrinter15: TMenuItem;
    APrintersConfiguration: TAction;
    PrintersConfiguration1: TMenuItem;
    AAsyncExec: TAction;
    Asynchronousexecution1: TMenuItem;
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
    procedure AExitExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure AImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AScale100Execute(Sender: TObject);
    procedure AScaleWideExecute(Sender: TObject);
    procedure AScaleFullExecute(Sender: TObject);
    procedure AScaleMoreExecute(Sender: TObject);
    procedure AScaleLessExecute(Sender: TObject);
    procedure ACancelExecute(Sender: TObject);
    procedure AAboutExecute(Sender: TObject);
    procedure AViewConnectExecute(Sender: TObject);
    procedure AStatusBarExecute(Sender: TObject);
    procedure Windows1Click(Sender: TObject);
    procedure ADocumentationExecute(Sender: TObject);
    procedure ASystemPrintDialogExecute(Sender: TObject);
    procedure ADriverQtExecute(Sender: TObject);
    procedure ADriverGDIExecute(Sender: TObject);
    procedure APrintSetupExecute(Sender: TObject);
    procedure APrintersConfigurationExecute(Sender: TObject);
    procedure MSelPrinter0Click(Sender: TObject);
    procedure AAsyncExecExecute(Sender: TObject);
  private
    { Private declarations }
    fhelp:TFRpHelpform;
    cancelled:boolean;
    clitree:TFRpCliTree;
    oldonHint:TNotifyEvent;
    AppStyle:TDefaultStyle;
    configfile:string;
    procedure MetProgress(Sender:TRpMetafileReport;Position,Size:int64;page:integer);
    procedure EnableButtons;
    procedure DisableButtons;
    procedure PlaceImagePosition;
    procedure ExecuteServer(Sender:TObject);
    procedure AppHint(Sender:TObject);
    procedure LoadConfig;
    procedure SaveConfig;
    procedure UpdateStyle;
    procedure ShowHelp(AURL:string);
    procedure UpdatePrintSel;
  public
    { Public declarations }
    pagenum:integer;
    metafile:TRpMetafileReport;
    qtdriver:TRpQtDriver;
    printerindex:TRpPrinterSelect;
    aqtdriver:IRpPrintDriver;
    bitmap:TBitmap;
    procedure PrintPage;
  end;

var
 FMeta:TFMeta;

implementation

uses rpprintdia,rppdfdriver;

{$R *.xfm}


procedure TFMeta.PrintPage;
var
 rPageSizeQt:TPageSizeQt;
begin
 EPageNum.Text:='0';
 if pagenum>Metafile.PageCount then
 begin
  pagenum:=Metafile.PageCount;
 end;
 if Metafile.PageSize<0 then
 begin
  rpagesizeqt.Custom:=True;
  rPageSizeQt.CustomWidth:=metafile.CustomX;
  rPageSizeQt.CustomHeight:=metafile.CustomY;
 end
 else
 begin
  rpagesizeqt.Indexqt:=metafile.PageSize;
  rpagesizeqt.Custom:=False;
 end;
 qtdriver.SetPagesize(rpagesizeqt);
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
 MSelectPrinter.Caption:=TranslateStr(741,MSelectPrinter.Caption);
 MSelPrinter0.Caption:=SRpDefaultPrinter;
 MSelPrinter1.Caption:=SRpReportPrinter;
 MSelPrinter2.Caption:=SRpTicketPrinter;
 MSelPrinter3.Caption:=SRpGraphicprinter;
 MSelPrinter4.Caption:=SRpCharacterprinter;
 MSelPrinter5.Caption:=SRpReportPrinter2;
 MSelPrinter6.Caption:=SRpTicketPrinter2;
 MSelPrinter7.Caption:=SRpUserPrinter1;
 MSelPrinter8.Caption:=SRpUserPrinter2;
 MSelPrinter9.Caption:=SRpUserPrinter3;
 MSelPrinter10.Caption:=SRpUserPrinter4;
 MSelPrinter11.Caption:=SRpUserPrinter5;
 MSelPrinter12.Caption:=SRpUserPrinter6;
 MSelPrinter13.Caption:=SRpUserPrinter7;
 MSelPrinter14.Caption:=SRpUserPrinter8;
 MSelPrinter15.Caption:=SRpUserPrinter9;

{$IFDEF MSWINDOWS}
  // Visible driver selection
  MDriverSelect.Visible:=true;
{$ENDIF}
 configfile:=Obtainininameuserconfig('','','repmand');
{$IFDEF VCLFILEFILTERS}
 SaveDialog1.Filter:=SRpRepMetafile+'|*.rpmf|'+
   SRpPDFFile+'|*.pdf|'+
   SRpPDFFileUn+'|*.pdf';
 OpenDialog1.Filter:=SRpRepMetafile+'|*.rpmf';
{$ENDIF}
{$IFNDEF VCLFILEFILTERS}
 SaveDialog1.Filter:=SRpRepMetafile+' (*.rpmf)|'+
   SRpPDFFile+' (*.pdf)|'+
   SRpPDFFileUn+' (*.pdf)';
 OpenDialog1.Filter:=SRpRepMetafile+' (*.rpmf)';
{$ENDIF}
 AppStyle:=dsSystemDefault;
 clitree:=TFRpCliTree.Create(Self);
 clitree.Align:=alLeft;
 clitree.Parent:=Self;
 clitree.OnExecuteServer:=ExecuteServer;
 MHelp.Caption:=TranslateStr(6,MHelp.Caption);
 AAbout.Caption:=TranslateStr(58,AAbout.Caption);
 AAbout.Hint:=TranslateStr(59,AABout.Hint);
 ImageContainer.Align:=alClient;
 Caption:=SRpRepMetafile;
 SaveDialog1.Title:=TranslateStr(216,SaveDialog1.Title);
 ACancel.Caption:=TranslateStr(94,ACancel.Caption);
 ACancel.Hint:=TranslateStr(218,ACancel.Hint);
 APrint.Caption:=TranslateStr(52,APrint.Caption);
 APrint.Hint:=TranslateStr(53,APrint.Hint);
 APrintersConfiguration.Caption:=TranslateStr(742,APrintersConfiguration.Caption);
 ASave.Caption:=TranslateStr(46,ASave.Caption);
 ASave.Hint:=TranslateStr(217,ASave.Hint);
 AExit.Caption:=TranslateStr(44,AExit.Caption);
 AExit.Hint:=TranslateStr(219,AExit.Hint);
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
 AViewConnect.Caption:=TranslateStr(781,AViewConnect.Caption);
 AViewConnect.Hint:=TranslateStr(781,AViewConnect.Hint);
 APrintSetup.Caption:=TranslateStr(56,APrintSetup.Caption);
 APrintSetup.Hint:=TranslateStr(57,APrintSetup.Hint);
 ADocumentation.Caption:=TranslateStr(60,ADocumentation.Caption);
 ADocumentation.Hint:=TranslateStr(61,ADocumentation.Hint);
 MDriverSelect.Caption:=TranslateStr(67,MDriverSelect.Caption);
 ADriverQt.Caption:=TranslateStr(68,ADriverQt.Caption);
 ADriverQt.Hint:=TranslateStr(69,ADriverQt.Hint);
 ADriverGDI.Caption:=TranslateStr(70,ADriverGDI.Caption);
 ADriverGDI.Hint:=TranslateStr(71,ADriverGDI.Hint);
 AAsyncExec.Caption:=TranslateStr(783,AASyncExec.Caption);
 AAsyncExec.Hint:=TranslateStr(784,AAsyncExec.Hint);


 File1.Caption:=TranslateStr(0,File1.Caption);
 Page1.Caption:=TranslateStr(269,Page1.Caption);
 View1.Caption:=TranslateStr(740,View1.Caption);
 OpenDialog1.Title:=File1.Caption;

 AOpen.Caption:=TranslateStr(42,AOpen.Caption);
 AOpen.Hint:=TranslateStr(739,AOpen.Hint);

 AStatusBar.Caption:=TranslateStr(76,AStatusBar.Caption);
 AStatusBar.Hint:=TranslateStr(77,AStatusBar.Hint);
 MPreferences.Caption:=TranslateStr(5,MPreferences.Caption);
 ASystemPrintDialog.Caption:=TranslateStr(72,ASystemPrintDialog.Caption);
 ASystemPrintDialog.Hint:=TranslateStr(73,ASystemPrintDialog.Hint);
 MQtStyle.Caption:=TranslateStr(78,MQtStyle.Caption);
 MQtStyle.Hint:=TranslateStr(79,MQtStyle.Hint);
 MQtDefault.Caption:=TranslateStr(80,MQtDefault.Caption);


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
 metafile.OnProgress:=MetProgress;

 // Activates OnHint
 oldonhint:=Application.OnHint;
 Application.OnHint:=AppHint;

 LoadConfig;
end;


procedure TFMeta.ShowHelp(AURL:string);
begin
 if Not Assigned(FHelp) then
  FHelp:=TFRpHelpform.Create(Application);
 FHelp.TextBrowser1.FileName:=AURL;
 FHelp.Show;
 if Length(FHelp.TextBrowser1.Text)<1 then
 begin
  FHelp.TextBrowser1.Text:=SRpDocNotInstalled+#10+
   SRpDocNotInstalled2+#10+
   SRpDocNotInstalled3+#10;
 end;
end;

procedure TFMeta.UpdateStyle;
var
 i:integer;
 aitem:TMenuItem;
begin
 Application.Style.DefaultStyle:=AppStyle;
 for i:=0 to MQtStyle.Count-1 do
 begin
  aitem:=MQtStyle.Items[i];
  aitem.Checked:=(aitem.Tag=Integer(Application.Style.DefaultStyle));
 end;
end;

procedure TFMeta.AppHint(Sender:TObject);
begin
 BStatus.Panels.Items[0].Text:=Application.Hint;
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
 cancelled:=true;
 qtdriver:=nil;
 SaveConfig;
end;

procedure TFMeta.APrintExecute(Sender: TObject);
var
 frompage,topage,copies:integer;
 allpages,collate:boolean;
 rpPageSize:TPageSizeQt;
begin
 // Prints the report
 frompage:=1;
 topage:=999999;
 allpages:=true;
 collate:=false;
 copies:=1;
 rpPageSize.Custom:=metafile.PageSize<0;
 rpPageSize.Indexqt:=metafile.PageSize;
 rpPageSize.CustomWidth:=metafile.CustomX;
 rpPageSize.CustomHeight:=metafile.CustomY;
{$IFDEF MSWINDOWS}
 if ADriverGDI.Checked then
 begin
  allpages:=true;
  frompage:=1; topage:=999999;
  copies:=1;
  rpgdidriver.PrinterSelection(printerindex);
  rpgdidriver.PageSizeSelection(rpPageSize);
  rpgdidriver.OrientationSelection(metafile.orientation);
  if rpgdidriver.DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
   rpgdidriver.PrintMetafile(metafile,opendialog1.FileName,true,allpages,
    frompage,topage,copies,collate,GetDeviceFontsOption(printerindex),printerindex);
  exit;
 end;
{$ENDIF}
 rpqtdriver.PrinterSelection(printerindex);
 rpqtdriver.PageSizeSelection(rpPageSize);
 rpqtdriver.OrientationSelection(metafile.orientation);
 if Not ASystemPrintDialog.Checked then
 begin
  if rpprintdia.DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
   rpqtdriver.PrintMetafile(metafile,opendialog1.FileName,true,allpages,
    frompage,topage,copies,collate,printerindex);
 end
 else
 begin
  if rpqtdriver.DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
   rpqtdriver.PrintMetafile(metafile,opendialog1.FileName,true,allpages,
    frompage,topage,copies,collate,printerindex);
 end;
end;

procedure TFMeta.ASaveExecute(Sender: TObject);
begin
 cancelled:=false;
 // Saves the metafile
 if SaveDialog1.Execute then
 begin
  DisableButtons;
  try
   Metafile.SaveToFile(SaveDialog1.Filename);
   if SaveDialog1.FilterIndex=1 then
   begin
    Metafile.SaveToFile(SaveDialog1.Filename)
   end
   else
    if SaveDialog1.FilterIndex in [2,3] then
    begin
     if SaveDialog1.FilterIndex=2 then
      SaveMetafileToPDF(metafile,SaveDialog1.filename,true)
     else
      SaveMetafileToPDF(metafile,SaveDialog1.filename,false);
    end;
 finally
   EnableButtons;
  end;
 end;
end;

procedure TFMeta.AOpenExecute(Sender: TObject);
begin
 DisableButtons;
 try
  cancelled:=false;
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
   FormResize(Self);
  end;
 finally
  EnableButtons;
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

procedure TFMeta.MetProgress(Sender:TRpMetafileReport;Position,Size:int64;page:integer);
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
 if ((GetKeyState(VK_ESCAPE) AND $80)>0) then
  cancelled:=true;
{$ENDIF}
 if cancelled then
  Raise Exception.Create(SRpOperationAborted);
end;



procedure TFMeta.EnableButtons;
begin
 AFirst.Enabled:=true;
 ANext.Enabled:=true;
 APrevious.Enabled:=true;
 ALast.Enabled:=true;
 ASave.Enabled:=true;
 AOpen.Enabled:=true;
 APrint.Enabled:=true;
 BCancel.Visible:=false;
 PBar.Visible:=false;
end;

procedure TFMeta.DisableButtons;
begin
 AFirst.Enabled:=false;
 ANext.Enabled:=false;
 APrevious.Enabled:=false;
 ALast.Enabled:=false;
 ASave.Enabled:=false;
 AOpen.Enabled:=false;
 AOpen.Enabled:=false;
 APrint.Enabled:=false;
 BCancel.Visible:=true;
 PBar.Position:=0;
 PBar.Visible:=true;
end;


procedure TFMeta.AExitExecute(Sender: TObject);
begin
 Close;
end;


procedure TFMeta.PlaceImagePosition;
var
 AWidth:integeR;
 Aheight:integer;
begin
 ImageContainer.HorzScrollBar.Position:=0;
 ImageContainer.VertScrollBar.Position:=0;
 AImage.Left:=0;
 AImage.Top:=0;
 ImageContainer.HorzScrollBar.Position:=0;
 ImageContainer.VertScrollBar.Position:=0;

 AWidth:=ImageContainer.Width-SCROLLBAR_VX;
 AHeight:=ImageContainer.Height-SCROLLBAR_HX;

 if AImage.Width>AWidth then
  AImage.Left:=0
 else
  AImage.Left:=(AWidth-AImage.Width) div 2;
 if AImage.Height>AHeight then
  AImage.Top:=0
 else
  AImage.Top:=(AHeight-AImage.Height) div 2;
 // A bug in the refresh in Windows
{$IFDEF MSWINDOWS}
 ImageContainer.Visible:=False;
 ImageContainer.Visible:=True;
 {$ENDIF}
end;

procedure TFMeta.FormResize(Sender: TObject);
begin
 // Sets the driver widths and redraw accordingly
 AScaleFull.Checked:=false;
 AScaleWide.Checked:=false;
 AScale100.Checked:=false;
 if Assigned(qtdriver) then
 begin
  qtdriver.clientwidth:=ImageContainer.Width;
  qtdriver.clientHeight:=ImageContainer.Height;
  case qtdriver.PreviewStyle of
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

procedure TFMeta.AScale100Execute(Sender: TObject);
begin
 qtdriver.PreviewStyle:=spNormal;
 FormResize(Self);
end;

procedure TFMeta.AScaleWideExecute(Sender: TObject);
begin
 qtdriver.PreviewStyle:=spWide;
 FormResize(Self);
end;

procedure TFMeta.AScaleFullExecute(Sender: TObject);
begin
 qtdriver.PreviewStyle:=spEntirePage;
 FormResize(Self);
end;

procedure TFMeta.AScaleLessExecute(Sender: TObject);
begin
 qtdriver.PreviewStyle:=spCustom;
 qtdriver.Scale:=qtdriver.scale-0.10;
 FormResize(Self);
end;

procedure TFMeta.AScaleMoreExecute(Sender: TObject);
begin
 qtdriver.PreviewStyle:=spCustom;
 qtdriver.Scale:=qtdriver.scale+0.10;
 FormResize(Self);
end;

procedure TFMeta.AImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
 relx:Extended;
 rely:Extended;
 posx,migx:Extended;
 posy,migy:Extended;
 punt:Tpoint;
begin
 // When clic in image scale to 100% and scroll to the
 // clicked section
 if qtdriver.PreviewStyle=spEntirePage then
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

procedure TFMeta.ACancelExecute(Sender: TObject);
begin
 cancelled:=true;
end;

procedure TFMeta.ExecuteServer(Sender:TObject);
begin
 metafile.LoadFromStream(clitree.Stream);
 ASave.Enabled:=True;
 APrint.Enabled:=True;
 AFirst.Enabled:=True;
 APrevious.Enabled:=True;
 ANext.Enabled:=True;
 ALast.Enabled:=True;
 pagenum:=1;
 PrintPage;
 FormResize(Self);
end;

procedure TFMeta.AAboutExecute(Sender: TObject);
begin
 ShowAbout;
end;

procedure TFMeta.AViewConnectExecute(Sender: TObject);
begin
 AViewConnect.Checked:=Not AViewConnect.Checked;
 clitree.Width:=clitree.Initialwidth;
 clitree.Visible:=AViewConnect.Checked;
 FormResize(Self);
end;

procedure TFMeta.AStatusBarExecute(Sender: TObject);
begin
 AStatusBar.Checked:=Not AStatusBar.Checked;
 BStatus.Visible:=AStatusBar.Checked;
 FormResize(Self);
end;


procedure TFMeta.Windows1Click(Sender: TObject);
begin
 // Sets the style
 AppStyle:=TDefaultStyle((Sender As TComponent).Tag);
 UpdateStyle;
end;

procedure TFMeta.LoadConfig;
var
 inif:TInifile;
begin
 inif:=TIniFile.Create(configfile);
 try
{$IFDEF MSWINDOWS}
  ADriverQT.Checked:=inif.ReadBool('Preferences','DriverQt',false);
{$ENDIF}
{$IFDEF LINUX}
  ADriverQT.Checked:=true;
{$ENDIF}
  AsystemPrintDialog.Checked:=inif.ReadBool('Preferences','SystemPrintDialog',True);
  BStatus.Visible:=inif.ReadBool('Preferences','StatusBar',True);
  AStatusBar.Checked:=BStatus.Visible;
  AViewConnect.Checked:=inif.ReadBool('Preferences','DiagConnect',True);
  clitree.Visible:=AViewConnect.Checked;
{$IFDEF LINUX}
  rpqtdriver.kylixprintbug:=false;
{$ENDIF}
  clitree.ComboHost.Text:=inif.ReadString('Preferences','Host','localhost');
  clitree.EUserName.Text:=inif.ReadString('Preferences','UserName','Admin');
  ADriverGDI.Checked:=Not ADriverQT.Checked;
  AAsyncExec.Checked:=inif.ReadBool('Preferences','AsyncExec',False);;
  clitree.asynchrohous:=AAsyncexec.Checked;
  AppStyle:=TDefaultStyle(inif.ReadInteger('Preferences','QtStyle',Integer(dsSystemDefault)));
  printerindex:=TRpPrinterSelect(inif.ReadInteger('Preferences','PrinterIndex',Integer(pRpDefaultPrinter)));
  UpdatePrintSel;
  UpdateStyle;
 finally
  inif.free;
 end;
end;

procedure TFMeta.SaveConfig;
var
 inif:TInifile;
begin
 inif:=TIniFile.Create(configfile);
 try
  inif.WriteBool('Preferences','DriverQT',ADriverQT.Checked);
  inif.WriteBool('Preferences','SystemPrintDialog',AsystemPrintDialog.Checked);
  inif.WriteBool('Preferences','StatusBar',BStatus.Visible);
  inif.WriteInteger('Preferences','QtStyle',Integer(AppStyle));
  inif.WriteInteger('Preferences','PrinterIndex',Integer(printerindex));
  inif.WriteString('Preferences','Host',clitree.ComboHost.Text);
  inif.WriteString('Preferences','UserName',clitree.EUserName.Text);
  inif.WriteBool('Preferences','AsyncExec',AAsyncExec.Checked);;
  inif.WriteBool('Preferences','DiagConnect',AViewConnect.Checked);
  inif.UpdateFile;
 finally
  inif.free;
 end;
end;

procedure TFMeta.ADocumentationExecute(Sender: TObject);
var
 aurl:string;
 Directorysep:string;
begin
 aurl:=ExtractFilePath(Application.Exename);
{$IFDEF MSWINDOWS}
 Directorysep:='\';
{$ENDIF}
{$IFDEF LINUX}
 Directorysep:='/';
{$ENDIF}
 aurl:=aurl+'doc'+Directorysep+
  Directorysep+'left.html';
 ShowHelp(aurl);
end;

procedure TFMeta.ASystemPrintDialogExecute(Sender: TObject);
begin
 ASystemPrintDialog.Checked:=Not ASystemPrintDialog.Checked;
end;

procedure TFMeta.ADriverQtExecute(Sender: TObject);
begin
 ADriverQT.Checked:=true;
 ADriverGDI.Checked:=false;
end;

procedure TFMeta.ADriverGDIExecute(Sender: TObject);
begin
 ADriverQT.Checked:=false;
 ADriverGDI.Checked:=true;
end;

procedure TFMeta.APrintSetupExecute(Sender: TObject);
{$IFDEF MSWINDOWS}
var
 psetup:TPrinterSetupDialog;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
 if ADriverGDI.Checked then
 begin
  psetup:=TPrinterSetupDialog.Create(nil);
  try
   psetup.execute;
  finally
   psetup.free;
  end;
  exit;
 end;
{$ENDIF}
 printer.ExecuteSetup;
end;

procedure TFMeta.UpdatePrintSel;
var
 i:integer;
begin
 for i:=0 to MSelectPrinter.Count-1 do
 begin
  MSelectPrinter.Items[i].Checked:=MSelectPrinter.Items[i].Tag=Integer(printerindex);
 end;
end;

procedure TFMeta.APrintersConfigurationExecute(Sender: TObject);
begin
 ShowPrintersConfiguration;
end;

procedure TFMeta.MSelPrinter0Click(Sender: TObject);
begin
 printerindex:=TRpPRinterSelect((Sender as TComponent).Tag);
 UpdatePrintSel;
end;

procedure TFMeta.AAsyncExecExecute(Sender: TObject);
begin
 AAsyncExec.Checked:=Not AAsyncExec.checked;
 clitree.asynchrohous:=AAsyncexec.Checked;
end;

end.
