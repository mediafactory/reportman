{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpfMetaview                                     }
{       TFRpMeta                                        }
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

unit rpfmetaview;

interface

{$I rpconf.inc}

uses
  SysUtils,Inifiles,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF VCLANDCLX}
  rpgdidriver,Dialogs,
{$ENDIF}
  Types, Classes, QGraphics, QControls, QForms,
  QStdCtrls,rpmetafile, QComCtrls,rpqtdriver, QExtCtrls,rpmdclitree,
  QActnList, QImgList,QPrinters,Qt,rpmdconsts,rptypes, QMenus,
  rpmdfabout,QTypes,QStyle,rpmdshfolder,rpmdprintconfig,rptextdriver,
  rphtmldriver,rpsvgdriver,rpcsvdriver,
  rpmdfhelpform, QDialogs,rpprintdia,rppdfdriver, QMask, rpmaskeditclx;

type
  TFRpMeta = class(TFrame)
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
    EPageNum: TRpCLXMaskEdit;
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
    BExit: TToolButton;
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
    BConfig: TToolButton;
    MPrintMenu: TPopupMenu;
    PrinterSetup2: TMenuItem;
    PrintersConfiguration2: TMenuItem;
    MSelectPrinter2: TMenuItem;
    MSelPrinter20: TMenuItem;
    MSelPrinter21: TMenuItem;
    MSelPrinter22: TMenuItem;
    MSelPrinter23: TMenuItem;
    MSelPrinter26: TMenuItem;
    MSelPrinter25: TMenuItem;
    MSelPrinter27: TMenuItem;
    MSelPrinter28: TMenuItem;
    MSelPrinter29: TMenuItem;
    MSelPrinter210: TMenuItem;
    MSelPrinter211: TMenuItem;
    MSelPrinter212: TMenuItem;
    MSelPrinter213: TMenuItem;
    MSelPrinter214: TMenuItem;
    MSelPrinter215: TMenuItem;
    MSelPrinter24: TMenuItem;
    AMailTo: TAction;
    Mailto1: TMenuItem;
    ToolButton10: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    procedure AFirstExecute(Sender: TObject);
    procedure ANextExecute(Sender: TObject);
    procedure APreviousExecute(Sender: TObject);
    procedure ALastExecute(Sender: TObject);
    procedure EPageNumKeyPress(Sender: TObject; var Key: Char);
    procedure APrintExecute(Sender: TObject);
    procedure ASaveExecute(Sender: TObject);
    procedure AOpenExecute(Sender: TObject);
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
    procedure FrameMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FrameMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure BConfigClick(Sender: TObject);
    procedure AMailToExecute(Sender: TObject);
  private
    { Private declarations }
    fhelp:TFRpHelpform;
    cancelled:boolean;
    oldonHint:TNotifyEvent;
    AppStyle:TDefaultStyle;
    configfile:string;
    faform:TForm;
    procedure SetForm(Value:TForm);
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
  public
    { Public declarations }
    clitree:TFRpCliTree;
    pagenum:integer;
    metafile:TRpMetafileReport;
    qtdriver:TRpQtDriver;
    printerindex:TRpPrinterSelect;
    aqtdriver:IRpPrintDriver;
    bitmap:TBitmap;
    setmenu:boolean;
    ShowPrintDialog:Boolean;
    procedure DoOpen(afilename:String);
    procedure UpdatePrintSel;
    property aform:TForm read faform write SetForm;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure PrintPage;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  end;

var
 FRpMeta:TFRpMeta;


implementation


{$R *.xfm}


procedure TFRpMeta.SetForm(Value:TForm);
begin
 faform:=Value;

 if assigned(faform) then
 begin
  faform.OnKeyDown:=FormKeyDown;
  if setmenu then
   faform.Menu:=MainMenu1;
 end;
end;

procedure TFRpMeta.PrintPage;
var
 rPageSizeQt:TPageSizeQt;
begin
 AAbout.Visible:=Metafile.PreviewAbout;
 ADocumentation.Visible:=Metafile.PreviewAbout;
 MHelp.Visible:=Metafile.PreviewAbout;
 AAsyncExec.Visible:=Metafile.PreviewAbout;
 AViewConnect.Visible:=Metafile.PreviewAbout;
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

constructor TFRpMeta.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 ShowPrintDialog:=true;
 setmenu:=true;
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

 MSelectPrinter2.Caption:=TranslateStr(741,MSelectPrinter.Caption);
 MSelPrinter20.Caption:=SRpDefaultPrinter;
 MSelPrinter21.Caption:=SRpReportPrinter;
 MSelPrinter22.Caption:=SRpTicketPrinter;
 MSelPrinter23.Caption:=SRpGraphicprinter;
 MSelPrinter24.Caption:=SRpCharacterprinter;
 MSelPrinter25.Caption:=SRpReportPrinter2;
 MSelPrinter26.Caption:=SRpTicketPrinter2;
 MSelPrinter27.Caption:=SRpUserPrinter1;
 MSelPrinter28.Caption:=SRpUserPrinter2;
 MSelPrinter29.Caption:=SRpUserPrinter3;
 MSelPrinter210.Caption:=SRpUserPrinter4;
 MSelPrinter211.Caption:=SRpUserPrinter5;
 MSelPrinter212.Caption:=SRpUserPrinter6;
 MSelPrinter213.Caption:=SRpUserPrinter7;
 MSelPrinter214.Caption:=SRpUserPrinter8;
 MSelPrinter215.Caption:=SRpUserPrinter9;
 BConfig.Hint:=TranslateStr(57,APrintSetup.Hint);

{$IFDEF VCLANDCLX}
  // Visible driver selection
  MDriverSelect.Visible:=true;
{$ENDIF}
 configfile:=Obtainininameuserconfig('','','repmand');
{$IFDEF VCLFILEFILTERS}
 SaveDialog1.Filter:=SRpRepMetafile+'|*.rpmf|'+
   SRpPDFFile+'|*.pdf|'+
   SRpPDFFileUn+'|*.pdf|'+
   SRpPlainFile+'|*.txt|'+
   SRpBitmapFile+'|*.bmp|'+
   SRpBitmapFileMono+'|*.bmp|'+
   SRpHtmlFile+'|*.html|'+
   SRpSVGFile+'|*.svg|'+
   SRpCSVFile+'|*.csv|'+
   SRpTXTProFile+'|*.txt';
{$IFDEF MSWINDOWS}
 SaveDialog1.Filter:=SaveDialog1.Filter+'|'+SRpExeMetafile+'|*.exe';
{$ENDIF}
 OpenDialog1.Filter:=SRpRepMetafile+'|*.rpmf';
{$ENDIF}
{$IFNDEF VCLFILEFILTERS}
 SaveDialog1.Filter:=SRpRepMetafile+' (*.rpmf)|'+
   SRpPDFFile+' (*.pdf)|'+
   SRpPDFFileUn+' (*.pdf)|'+
   SRpPlainFile+' (*.txt)|'+
   SRpBitmapFile+' (*.bmp)|'+
   SRpBitmapFileMono+' (*.bmp)|'+
   SRpHtmlFile+' (*.html)|'+
   SRpSVGFile+' (*.svg)|'+
   SRpCSVFile+' (*.csv)|'+
   SRpTXTProFile+' (*.txt)';
{$IFDEF MSWINDOWS}
  SaveDialog1.Filter:=SaveDialog1.Filter+'|'+SRpExeMetafile+' (*.exe)';
{$ENDIF}
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
 AMailTo.Caption:=TranslateStr(1230,AMailTo.Caption);
 AMailTo.Hint:=TranslateStr(1231,AMailTo.Hint);
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

 SaveDialog1.FilterIndex:=2;

 LoadConfig;
end;



procedure TFRpMeta.ShowHelp(AURL:string);
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

procedure TFRpMeta.UpdateStyle;
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

procedure TFRpMeta.AppHint(Sender:TObject);
begin
 BStatus.Panels.Items[0].Text:=Application.Hint;
end;


procedure TFRpMeta.AFirstExecute(Sender: TObject);
begin
 pagenum:=1;
 PrintPage;
end;

procedure TFRpMeta.ANextExecute(Sender: TObject);
begin
 inc(pagenum);
 PrintPage;
end;

procedure TFRpMeta.APreviousExecute(Sender: TObject);
begin
 dec(pagenum);
 if pagenum<1 then
  pagenum:=1;
 PrintPage;
end;

procedure TFRpMeta.ALastExecute(Sender: TObject);
begin
 pagenum:=MaxInt;
 PrintPage;
end;

procedure TFRpMeta.EPageNumKeyPress(Sender: TObject; var Key: Char);
begin
 if Key=chr(13) then
 begin
  pagenum:=StrToInt(EPageNum.Text);
  PrintPage;
 end;
end;

destructor TFRpMeta.Destroy;
begin
 cancelled:=true;
 qtdriver:=nil;
 SaveConfig;
 Application.OnHint:=oldonhint;
 bitmap.free;
 inherited Destroy;
end;

procedure TFRpMeta.APrintExecute(Sender: TObject);
var
 frompage,topage,copies:integer;
 allpages,collate:boolean;
 rpPageSize:TPageSizeQt;
 selectedok:boolean;
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
{$IFDEF VCLANDCLX}
 if ADriverGDI.Checked then
 begin
  allpages:=true;
  frompage:=1; topage:=999999;
  copies:=1;
  rpgdidriver.PrinterSelection(printerindex);
  rpgdidriver.PageSizeSelection(rpPageSize);
  rpgdidriver.OrientationSelection(metafile.orientation);
  selectedok:=true;
  if ShowPrintDialog then
   selectedok:=rpgdidriver.DoShowPrintDialog(allpages,frompage,topage,copies,collate);
  if selectedok then
   rpgdidriver.PrintMetafile(metafile,opendialog1.FileName,true,allpages,
    frompage,topage,copies,collate,GetDeviceFontsOption(printerindex),printerindex);
  exit;
 end;
{$ENDIF}
 rpqtdriver.PrinterSelection(printerindex);
 rpqtdriver.PageSizeSelection(rpPageSize);
 rpqtdriver.OrientationSelection(metafile.orientation);
 selectedok:=true;
 if ShowPrintDialog then
 begin
  if Not ASystemPrintDialog.Checked then
  begin
   selectedok:=rpprintdia.DoShowPrintDialog(allpages,frompage,topage,copies,collate);
  end
  else
  begin
   selectedok:=rpqtdriver.DoShowPrintDialog(allpages,frompage,topage,copies,collate);
  end;
 end;
 if selectedok then
  rpqtdriver.PrintMetafile(metafile,opendialog1.FileName,true,allpages,
    frompage,topage,copies,collate,printerindex);
end;

procedure TFRpMeta.ASaveExecute(Sender: TObject);
var
 abitmap:TBitmap;
begin
 cancelled:=false;
 // Saves the metafile
 if SaveDialog1.Execute then
 begin
  DisableButtons;
  try
   Metafile.SaveToFile(SaveDialog1.Filename);
   case SaveDialog1.FilterIndex of
    1:
     begin
      Metafile.SaveToFile(SaveDialog1.Filename)
     end;
    2,3:
     begin
      if SaveDialog1.FilterIndex=2 then
       SaveMetafileToPDF(metafile,SaveDialog1.filename,true,nil)
      else
       SaveMetafileToPDF(metafile,SaveDialog1.filename,false,nil);
     end;
    5,6:
     begin
      ALastExecute(Self);
      abitmap:=MetafileToBitmap(Metafile,true,SaveDialog1.FilterIndex=6);
      try
       if assigned(abitmap) then
        abitmap.SaveToFile(SaveDialog1.FileName);
      finally
       abitmap.free;
      end;
     end;
    7:
     begin
      ExportMetafileToHtml(Metafile,Caption,SaveDialog1.FileName,
       true,true,1,9999);
     end;
    8:
     begin
      ExportMetafileToSVG(Metafile,Caption,SaveDialog1.FileName,
       true,true,1,9999);
     end;
    9:
     begin
      ExportMetafileToCSV(metafile,SaveDialog1.Filename,true,true,
       1,9999);
     end;
    10:
     begin
      ExportMetafileToTextPro(metafile,SaveDialog1.Filename,true,true,
       1,9999);
     end;
{$IFDEF MSWINDOWS}
    11:
     begin
      MetafileToExe(metafile,SaveDialog1.Filename);
     end;
{$ENDIF}
    else
    begin
     // Plain text file
     ALastExecute(Self);
     SaveMetafileToTextFile(Metafile,SaveDialog1.FileName);
    end;
   end;
 finally
   EnableButtons;
  end;
 end;
end;

procedure TFRpMeta.DoOpen(afilename:String);
begin
 metafile.LoadFromFile(afilename);
 ASave.Enabled:=True;
 AMailTo.Enabled:=true;
 APrint.Enabled:=True;
 AFirst.Enabled:=True;
 APrevious.Enabled:=True;
 ANext.Enabled:=True;
 ALast.Enabled:=True;
 pagenum:=1;
 PrintPage;
 FormResize(Self);
end;

procedure TFRpMeta.AOpenExecute(Sender: TObject);
begin
 DisableButtons;
 try
  cancelled:=false;
  if OpenDialog1.Execute then
  begin
   DoOpen(OpenDialog1.Filename);
  end;
 finally
  EnableButtons;
 end;
end;

procedure TFRpMeta.FormKeyDown(Sender: TObject; var Key: Word;
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

procedure TFRpMeta.MetProgress(Sender:TRpMetafileReport;Position,Size:int64;page:integer);
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



procedure TFRpMeta.EnableButtons;
begin
 AFirst.Enabled:=true;
 ANext.Enabled:=true;
 APrevious.Enabled:=true;
 ALast.Enabled:=true;
 ASave.Enabled:=true;
 AMailTo.Enabled:=true;
 AOpen.Enabled:=true;
 APrint.Enabled:=true;
 BCancel.Visible:=false;
 PBar.Visible:=false;
end;

procedure TFRpMeta.DisableButtons;
begin
 AFirst.Enabled:=false;
 ANext.Enabled:=false;
 APrevious.Enabled:=false;
 ALast.Enabled:=false;
 ASave.Enabled:=false;
 AMailTo.Enabled:=false;
 AOpen.Enabled:=false;
 AOpen.Enabled:=false;
 APrint.Enabled:=false;
 BCancel.Visible:=true;
 PBar.Position:=0;
 PBar.Visible:=true;
end;


procedure TFRpMeta.AExitExecute(Sender: TObject);
begin
 if assigned(aform) then
  aform.Close;
end;


procedure TFRpMeta.PlaceImagePosition;
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

procedure TFRpMeta.FormResize(Sender: TObject);
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

procedure TFRpMeta.AScale100Execute(Sender: TObject);
begin
 qtdriver.PreviewStyle:=spNormal;
 FormResize(Self);
end;

procedure TFRpMeta.AScaleWideExecute(Sender: TObject);
begin
 qtdriver.PreviewStyle:=spWide;
 FormResize(Self);
end;

procedure TFRpMeta.AScaleFullExecute(Sender: TObject);
begin
 qtdriver.PreviewStyle:=spEntirePage;
 FormResize(Self);
end;

procedure TFRpMeta.AScaleLessExecute(Sender: TObject);
begin
 qtdriver.PreviewStyle:=spCustom;
 qtdriver.Scale:=qtdriver.scale-0.10;
 FormResize(Self);
end;

procedure TFRpMeta.AScaleMoreExecute(Sender: TObject);
begin
 qtdriver.PreviewStyle:=spCustom;
 qtdriver.Scale:=qtdriver.scale+0.10;
 FormResize(Self);
end;

procedure TFRpMeta.AImageMouseDown(Sender: TObject; Button: TMouseButton;
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

procedure TFRpMeta.ACancelExecute(Sender: TObject);
begin
 cancelled:=true;
end;

procedure TFRpMeta.ExecuteServer(Sender:TObject);
begin
 metafile.LoadFromStream(clitree.Stream);
 ASave.Enabled:=True;
 AMailTo.Enabled:=true;
 APrint.Enabled:=True;
 AFirst.Enabled:=True;
 APrevious.Enabled:=True;
 ANext.Enabled:=True;
 ALast.Enabled:=True;
 pagenum:=1;
 PrintPage;
 FormResize(Self);
end;

procedure TFRpMeta.AAboutExecute(Sender: TObject);
begin
 ShowAbout;
end;

procedure TFRpMeta.AViewConnectExecute(Sender: TObject);
begin
 AViewConnect.Checked:=Not AViewConnect.Checked;
 clitree.Width:=clitree.Initialwidth;
 clitree.Visible:=AViewConnect.Checked;
 FormResize(Self);
end;

procedure TFRpMeta.AStatusBarExecute(Sender: TObject);
begin
 AStatusBar.Checked:=Not AStatusBar.Checked;
 BStatus.Visible:=AStatusBar.Checked;
 FormResize(Self);
end;


procedure TFRpMeta.Windows1Click(Sender: TObject);
begin
 // Sets the style
 AppStyle:=TDefaultStyle((Sender As TComponent).Tag);
 UpdateStyle;
end;

procedure TFRpMeta.LoadConfig;
var
 inif:TInifile;
begin
 inif:=TIniFile.Create(configfile);
 try
{$IFDEF VCLANDCLX}
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

procedure TFRpMeta.SaveConfig;
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

procedure TFRpMeta.ADocumentationExecute(Sender: TObject);
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

procedure TFRpMeta.ASystemPrintDialogExecute(Sender: TObject);
begin
 ASystemPrintDialog.Checked:=Not ASystemPrintDialog.Checked;
end;

procedure TFRpMeta.ADriverQtExecute(Sender: TObject);
begin
 ADriverQT.Checked:=true;
 ADriverGDI.Checked:=false;
end;

procedure TFRpMeta.ADriverGDIExecute(Sender: TObject);
begin
 ADriverQT.Checked:=false;
 ADriverGDI.Checked:=true;
end;

procedure TFRpMeta.APrintSetupExecute(Sender: TObject);
{$IFDEF VCLANDCLX}
var
 psetup:TPrinterSetupDialog;
{$ENDIF}
begin
{$IFDEF VCLANDCLX}
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

procedure TFRpMeta.UpdatePrintSel;
var
 i:integer;
begin
 for i:=0 to MSelectPrinter.Count-1 do
 begin
  MSelectPrinter.Items[i].Checked:=MSelectPrinter.Items[i].Tag=Integer(printerindex);
 end;
 for i:=0 to MSelectPrinter2.Count-1 do
 begin
  MSelectPrinter2.Items[i].Checked:=MSelectPrinter2.Items[i].Tag=Integer(printerindex);
 end;
end;

procedure TFRpMeta.APrintersConfigurationExecute(Sender: TObject);
begin
 ShowPrintersConfiguration;
end;

procedure TFRpMeta.MSelPrinter0Click(Sender: TObject);
begin
 printerindex:=TRpPRinterSelect((Sender as TComponent).Tag);
 UpdatePrintSel;
end;

procedure TFRpMeta.AAsyncExecExecute(Sender: TObject);
begin
 AAsyncExec.Checked:=Not AAsyncExec.checked;
 clitree.asynchrohous:=AAsyncexec.Checked;
end;

procedure TFRpMeta.FrameMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
 if (ssCtrl in Shift) then
  ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Position+GetWheelInc(Shift)
 else
  ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Position+GetWheelInc(Shift);
 Handled:=true;
end;

procedure TFRpMeta.FrameMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
 if (ssCtrl in Shift) then
  ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Position-GetWheelInc(Shift)
 else
  ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Position-GetWheelInc(Shift);
 Handled:=true;
end;

procedure TFRpMeta.BConfigClick(Sender: TObject);
var
 apoint:TPoint;
begin
 apoint.X:=BConfig.Left;
 apoint.Y:=BConfig.Top+BConfig.Height;
 apoint:=BConfig.Parent.ClientToScreen(apoint);
 // SHows the printer menu
 MPrintMenu.Popup(apoint.X,apoint.Y);
end;

procedure TFRpMeta.AMailToExecute(Sender: TObject);
var
 afilename:String;
begin
 afilename:=ChangeFileExt(RpTempFileName,'.pdf');
 SaveMetafileToPDF(Metafile,afilename,true,nil);
 try
  rptypes.SendMail('',ExtractFileName(afilename),'',afilename);
 finally
  SysUtils.DeleteFile(afilename);
 end;
end;


end.
