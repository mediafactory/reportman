{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpMetaviewvcl                                   }
{       TFRpMetaVCL                                     }
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

unit rpfmetaviewvcl;

interface

{$I rpconf.inc}

uses
  SysUtils,Inifiles,
  Windows,Dialogs,rpgdidriver,ShellApi,rpgraphutilsvcl,rphtmldriver,
{$IFDEF USEVARIANTS}
  Types,Variants,
{$ENDIF}
  Classes, Graphics, Controls, Forms,
  StdCtrls,rpmetafile, ComCtrls,ExtCtrls,
{$IFDEF USEINDY}
  rpmdclitreevcl,
{$ENDIF}
  rpexceldriver,rptextdriver,
  ActnList, ImgList,Printers,rpmdconsts,rptypes, Menus,
  rpmdfaboutvcl,rpmdshfolder,rpmdprintconfigvcl,
  ToolWin, Mask, rpmaskedit;

type

  TFRpMetaVCL = class(TFrame)
    BToolBar: TToolBar;
    ImageContainer: TScrollBox;
    AImage: TImage;
    ActionList1: TActionList;
    AFirst: TAction;
    APrevious: TAction;
    ANext: TAction;
    ALast: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    EPageNum: TRpMaskEdit;
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
    AStatusBar: TAction;
    BStatus: TStatusBar;
    ADocumentation: TAction;
    StatusBar1: TMenuItem;
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
    ImageList1: TImageList;
    BConfig: TToolButton;
    MPrintMenu: TPopupMenu;
    PrinterSetup2: TMenuItem;
    PrintersConfiguration2: TMenuItem;
    MSelectPrinter2: TMenuItem;
    MSelPrinter20: TMenuItem;
    MSelPrinter21: TMenuItem;
    MSelPrinter22: TMenuItem;
    MSelPrinter23: TMenuItem;
    MSelPrinter24: TMenuItem;
    MSelPrinter25: TMenuItem;
    MSelPrinter26: TMenuItem;
    MSelPrinter27: TMenuItem;
    MSelPrinter28: TMenuItem;
    MSelPrinter29: TMenuItem;
    MSelPrinter210: TMenuItem;
    MSelPrinter211: TMenuItem;
    MSelPrinter212: TMenuItem;
    MSelPrinter213: TMenuItem;
    MSelPrinter214: TMenuItem;
    MSelPrinter215: TMenuItem;
    ToolButton10: TToolButton;
    AMailTo: TAction;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    procedure AFirstExecute(Sender: TObject);
    procedure ANextExecute(Sender: TObject);
    procedure APreviousExecute(Sender: TObject);
    procedure ALastExecute(Sender: TObject);
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
    procedure ADocumentationExecute(Sender: TObject);
    procedure APrintSetupExecute(Sender: TObject);
    procedure APrintersConfigurationExecute(Sender: TObject);
    procedure MSelPrinter0Click(Sender: TObject);
    procedure AAsyncExecExecute(Sender: TObject);
    procedure FrameMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FrameMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure BConfigClick(Sender: TObject);
    procedure EPageNumKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure AMailToExecute(Sender: TObject);
  private
    { Private declarations }
    cancelled:boolean;
    oldonHint:TNotifyEvent;
    configfile:string;
    faform:TWinControl;
    procedure SetForm(Value:TWinControl);
    procedure MetProgress(Sender:TRpMetafileReport;Position,Size:int64;page:integer);
    procedure EnableButtons;
    procedure DisableButtons;
    procedure PlaceImagePosition;
{$IFDEF USEINDY}
    procedure ExecuteServer(Sender:TObject);
{$ENDIF}
    procedure AppHint(Sender:TObject);
    procedure LoadConfig;
    procedure SaveConfig;
    procedure ShowHelp(AURL:string);
  public
    { Public declarations }
{$IFDEF USEINDY}
    clitree:TFRpCliTreeVCL;
{$ENDIF}
    ShowPrintDialog:Boolean;
    pagenum:integer;
    metafile:TRpMetafileReport;
    gdidriver:TRpGDIDriver;
    printerindex:TRpPrinterSelect;
    agdidriver:IRpPrintDriver;
    bitmap:TBitmap;
    procedure DoOpen(afilename:String);
    procedure UpdatePrintSel;
    property aform:TWinControl read faform write SetForm;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure CreateClitree;
    procedure PrintPage;
  end;

var
 FRpMetaVCL:TFRpMetaVCL;

implementation

uses rppdfdriver;

{$R *.dfm}

procedure TFRpMetaVCL.CreateClitree;
begin
{$IFDEF USEINDY}
 clitree:=TFRpCliTreeVCL.Create(Self);
 clitree.Align:=alLeft;
 clitree.Parent:=Self;
 clitree.OnExecuteServer:=ExecuteServer;
{$ENDIF}
 LoadConfig;
end;

procedure TFRpMetaVCL.SetForm(Value:TWinControl);
begin
 faform:=Value;
 if assigned(faform) then
 begin
  if (faform is TForm) then
  begin
   TForm(faform).Menu:=MainMenu1;
   TForm(faform).OnKeyDown:=FormKeyDown;
  end;
 end;
end;

procedure TFRpMetaVCL.PrintPage;
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
 try
  gdidriver.SetPagesize(rpagesizeqt);
 except
  On E:Exception do
  begin
   rpgraphutilsvcl.RpMessageBox(E.Message);
  end;
 end;
 Metafile.CurrentPage:=pagenum-1;
 metafile.DrawPage(gdidriver);
 if Assigned(gdidriver.bitmap) then
 begin
  AImage.Width:=gdidriver.bitmap.Width;
  AImage.Height:=gdidriver.bitmap.Height;
  AImage.Picture.Bitmap.Assign(gdidriver.bitmap);
  AImage.Invalidate;
 end;
 pagenum:=Metafile.CurrentPage+1;
 EPageNum.Text:=IntToStr(PageNum);
end;

constructor TFRpMetaVCL.Create(AOwner:TComponent);
{$IFDEF DOTNETD}
var
 i:integer;
{$ENDIF}
begin
 inherited Create(AOwner);

 ShowPrintDialog:=true;
 if AOwner is TWinControl then
  Parent:=TWinControl(AOwner);
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

 configfile:=Obtainininameuserconfig('','','repmand');
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


 APrevious.ShortCut:=VK_PRIOR;
 ANext.ShortCut:=VK_NEXT;
 AFirst.ShortCut:=VK_HOME;
 ALast.ShortCut:=VK_END;
 gdidriver:=TRpGDIDriver.Create;
 agdidriver:=gdidriver;
// qtdriver.toprinter:=true;
 bitmap:=TBitmap.Create;
 bitmap.PixelFormat:=pf32bit;
 AImage.Picture.Bitmap:=bitmap;
 metafile:=TrpMetafileReport.Create(Self);
 metafile.OnProgress:=MetProgress;

{$IFDEF DOTNETD}
 // Fix for .net
 for i:=0 to MSelectPrinter.Count-1 do
 begin
  MSelectPrinter.Items[i].Tag:=Variant(i);
 end;
 for i:=0 to MSelectPrinter2.Count-1 do
 begin
  MSelectPrinter2.Items[i].Tag:=Variant(i);
 end;
{$ENDIF}

 // Activates OnHint
 oldonhint:=Application.OnHint;
 Application.OnHint:=AppHint;
 SaveDialog1.FilterIndex:=2;


 LoadConfig;
end;


procedure TFRpMetaVCL.ShowHelp(AURL:string);
begin
 // Starts the default explorer
{$IFNDEF DOTNETD}
 ShellExecute(Self.handle,Pchar('open'),Pchar(AURL),nil,nil,SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF DOTNETD}
 ShellExecute(Self.Handle,'open',AURL,'','',SW_SHOWNORMAL);
{$ENDIF}
end;


procedure TFRpMetaVCL.AppHint(Sender:TObject);
begin
 if Not (csDestroying in ComponentState) then
  BStatus.Panels.Items[0].Text:=Application.Hint;
end;


procedure TFRpMetaVCL.AFirstExecute(Sender: TObject);
begin
 pagenum:=1;
 PrintPage;
end;

procedure TFRpMetaVCL.ANextExecute(Sender: TObject);
begin
 inc(pagenum);
 PrintPage;
end;

procedure TFRpMetaVCL.APreviousExecute(Sender: TObject);
begin
 dec(pagenum);
 if pagenum<1 then
  pagenum:=1;
 PrintPage;
end;

procedure TFRpMetaVCL.ALastExecute(Sender: TObject);
begin
 pagenum:=MaxInt;
 PrintPage;
end;


destructor TFRpMetaVCL.Destroy;
begin
 cancelled:=true;
 gdidriver:=nil;
 SaveConfig;
 Application.OnHint:=oldonhint;
 bitmap.free;

 inherited Destroy;
end;

procedure TFRpMetaVCL.APrintExecute(Sender: TObject);
var
 frompage,topage,copies:integer;
 allpages,collate:boolean;
 rpPageSize:TPageSizeQt;
 selectedok:Boolean;
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
end;

procedure TFRpMetaVCL.ASaveExecute(Sender: TObject);
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
       SaveMetafileToPDF(metafile,SaveDialog1.filename,true)
      else
       SaveMetafileToPDF(metafile,SaveDialog1.filename,false);
     end;
    4:
     begin
      ALastExecute(Self);
      ExportMetafileToExcel(Metafile,SaveDialog1.FileName,
       true,false,true,1,9999);
     end;
    6,7:
     begin
      ALastExecute(Self);
      abitmap:=MetafileToBitmap(Metafile,true,SaveDialog1.FilterIndex=7);
      try
       if assigned(abitmap) then
        abitmap.SaveToFile(SaveDialog1.FileName);
      finally
       abitmap.free;
      end;
     end;
     8:
      begin
       ExportMetafileToHtml(Metafile,Caption,SaveDialog1.FileName,
        true,true,1,9999);
      end;
{$IFNDEF DOTNETD}
     9:
      begin
       MetafileToExe(metafile,SaveDialog1.Filename);
      end;
{$ENDIF}
    else
    begin
     ALastExecute(Self);
     SaveMetafileToTextFile(Metafile,SaveDialog1.FileName);
    end;
   end;
 finally
   EnableButtons;
  end;
 end;
end;

procedure TFRpMetaVCL.DoOpen(afilename:String);
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

procedure TFRpMetaVCL.AOpenExecute(Sender: TObject);
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

procedure TFRpMetaVCL.FormKeyDown(Sender: TObject; var Key: Word;
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
end;

procedure TFRpMetaVCL.MetProgress(Sender:TRpMetafileReport;Position,Size:int64;page:integer);
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



procedure TFRpMetaVCL.EnableButtons;
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

procedure TFRpMetaVCL.DisableButtons;
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


procedure TFRpMetaVCL.AExitExecute(Sender: TObject);
begin
 if assigned(aform) then
 begin
  if (aform is TForm) then
   TForm(aform).Close;
 end;
end;


procedure TFRpMetaVCL.PlaceImagePosition;
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

procedure TFRpMetaVCL.FormResize(Sender: TObject);
begin
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

procedure TFRpMetaVCL.AScale100Execute(Sender: TObject);
begin
 gdidriver.PreviewStyle:=spNormal;
 FormResize(Self);
end;

procedure TFRpMetaVCL.AScaleWideExecute(Sender: TObject);
begin
 gdidriver.PreviewStyle:=spWide;
 FormResize(Self);
end;

procedure TFRpMetaVCL.AScaleFullExecute(Sender: TObject);
begin
 gdidriver.PreviewStyle:=spEntirePage;
 FormResize(Self);
end;

procedure TFRpMetaVCL.AScaleLessExecute(Sender: TObject);
begin
 gdidriver.PreviewStyle:=spCustom;
 gdidriver.Scale:=gdidriver.scale-0.10;
 FormResize(Self);
end;

procedure TFRpMetaVCL.AScaleMoreExecute(Sender: TObject);
begin
 gdidriver.PreviewStyle:=spCustom;
 gdidriver.Scale:=gdidriver.scale+0.10;
 FormResize(Self);
end;

procedure TFRpMetaVCL.AImageMouseDown(Sender: TObject; Button: TMouseButton;
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

procedure TFRpMetaVCL.ACancelExecute(Sender: TObject);
begin
 cancelled:=true;
end;

{$IFDEF USEINDY}
procedure TFRpMetaVCL.ExecuteServer(Sender:TObject);
begin
 if assigned(clitree) then
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
end;
{$ENDIF}

procedure TFRpMetaVCL.AAboutExecute(Sender: TObject);
begin
 ShowAbout;
end;

procedure TFRpMetaVCL.AViewConnectExecute(Sender: TObject);
begin
 AViewConnect.Checked:=Not AViewConnect.Checked;
{$IFDEF USEINDY}
 if assigned(clitree) then
 begin
  clitree.Width:=clitree.Initialwidth;
  clitree.Visible:=AViewConnect.Checked;
 end;
{$ENDIF}
 FormResize(Self);
end;

procedure TFRpMetaVCL.AStatusBarExecute(Sender: TObject);
begin
 AStatusBar.Checked:=Not AStatusBar.Checked;
 BStatus.Visible:=AStatusBar.Checked;
 FormResize(Self);
end;



procedure TFRpMetaVCL.LoadConfig;
var
 inif:TInifile;
begin
 inif:=TIniFile.Create(configfile);
 try
  BStatus.Visible:=inif.ReadBool('Preferences','StatusBar',True);
  AStatusBar.Checked:=BStatus.Visible;
  AViewConnect.Checked:=inif.ReadBool('Preferences','DiagConnect',True);
{$IFDEF USEINDY}
  if assigned(clitree) then
  begin
   clitree.Visible:=AViewConnect.Checked;
   clitree.ComboHost.Text:=inif.ReadString('Preferences','Host','localhost');
   clitree.EUserName.Text:=inif.ReadString('Preferences','UserName','Admin');
   clitree.asynchrohous:=AAsyncexec.Checked;
  end;
{$ENDIF}
  AAsyncExec.Checked:=inif.ReadBool('Preferences','AsyncExec',False);;
  printerindex:=TRpPrinterSelect(inif.ReadInteger('Preferences','PrinterIndex',Integer(pRpDefaultPrinter)));
  UpdatePrintSel;
 finally
  inif.free;
 end;
end;

procedure TFRpMetaVCL.SaveConfig;
var
 inif:TInifile;
begin
 inif:=TIniFile.Create(configfile);
 try
  inif.WriteBool('Preferences','StatusBar',BStatus.Visible);
  inif.WriteInteger('Preferences','PrinterIndex',Integer(printerindex));
{$IFDEF USEINDY}
  if assigned(clitree) then
  begin
   inif.WriteString('Preferences','Host',clitree.ComboHost.Text);
   inif.WriteString('Preferences','UserName',clitree.EUserName.Text);
  end;
{$ENDIF}
  inif.WriteBool('Preferences','AsyncExec',AAsyncExec.Checked);;
  inif.WriteBool('Preferences','DiagConnect',AViewConnect.Checked);
  inif.UpdateFile;
 finally
  inif.free;
 end;
end;

procedure TFRpMetaVCL.ADocumentationExecute(Sender: TObject);
var
 aurl:string;
 Directorysep:string;
begin
 aurl:=ExtractFilePath(Application.Exename);
 Directorysep:='\';
 aurl:=aurl+'doc'+Directorysep+
  'index.html';
 if FileExists(aurl) then
  ShowHelp(aurl)
 else
  ShowHelp('http://reportman.sourceforge.net');
end;


procedure TFRpMetaVCL.APrintSetupExecute(Sender: TObject);
var
 psetup:TPrinterSetupDialog;
begin
 psetup:=TPrinterSetupDialog.Create(nil);
 try
  psetup.execute;
 finally
  psetup.free;
 end;
end;

procedure TFRpMetaVCL.UpdatePrintSel;
var
 i:integer;
begin
 for i:=0 to MSelectPrinter.Count-1 do
 begin
  MSelectPrinter.Items[i].Checked:=Integer(MSelectPrinter.Items[i].Tag)=Integer(printerindex);
 end;
 for i:=0 to MSelectPrinter2.Count-1 do
 begin
  MSelectPrinter2.Items[i].Checked:=Integer(MSelectPrinter.Items[i].Tag)=Integer(printerindex);
 end;
end;

procedure TFRpMetaVCL.APrintersConfigurationExecute(Sender: TObject);
begin
 ShowPrintersConfiguration;
end;

procedure TFRpMetaVCL.MSelPrinter0Click(Sender: TObject);
begin
 printerindex:=TRpPRinterSelect((Sender as TComponent).Tag);
 UpdatePrintSel;
end;

procedure TFRpMetaVCL.AAsyncExecExecute(Sender: TObject);
begin
 AAsyncExec.Checked:=Not AAsyncExec.checked;
{$IFDEF USEINDY}
 if assigned(clitree) then
  clitree.asynchrohous:=AAsyncexec.Checked;
{$ENDIF}
end;

procedure TFRpMetaVCL.FrameMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
 if (ssCtrl in Shift) then
  ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Position+GetWheelInc(Shift)
 else
  ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Position+GetWheelInc(Shift);
 Handled:=true;
end;

procedure TFRpMetaVCL.FrameMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
 if (ssCtrl in Shift) then
  ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Position-GetWheelInc(Shift)
 else
  ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Position-GetWheelInc(Shift);
 Handled:=true;
end;

procedure TFRpMetaVCL.BConfigClick(Sender: TObject);
var
 apoint:TPoint;
begin
 apoint.X:=BConfig.Left;
 apoint.Y:=BConfig.Top+BConfig.Height;
 apoint:=BConfig.Parent.ClientToScreen(apoint);
 // SHows the printer menu
 MPrintMenu.Popup(apoint.X,apoint.Y);
end;

procedure TFRpMetaVCL.EPageNumKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key=VK_RETURN then
 begin
  pagenum:=StrToInt(EPageNum.Text);
  PrintPage;
 end;
end;

procedure TFRpMetaVCL.AMailToExecute(Sender: TObject);
var
 afilename:String;
begin
 afilename:=ChangeFileExt(RpTempFileName,'.pdf');
 SaveMetafileToPDF(Metafile,afilename,true);
 try
  rptypes.SendMail('',ExtractFileName(afilename),'',afilename);
 finally
  SysUtils.DeleteFile(afilename);
 end;
end;

end.
