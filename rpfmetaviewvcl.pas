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
  Windows,Dialogs,rpgdidriver,ShellApi,rpgraphutilsvcl,
{$IFDEF USEVARIANTS}
  Types,
{$ENDIF}
  Classes, Graphics, Controls, Forms,
  StdCtrls,rpmetafile, ComCtrls,ExtCtrls,
{$IFNDEF FORWEBAX}
  rpmdclitreevcl,
{$ENDIF}
  ActnList, ImgList,Printers,rpmdconsts,rptypes, Menus,
  rpmdfaboutvcl,rpmdshfolder,rpmdprintconfigvcl,
  ToolWin;

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
    ToolButton10: TToolButton;
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
    procedure ADocumentationExecute(Sender: TObject);
    procedure APrintSetupExecute(Sender: TObject);
    procedure APrintersConfigurationExecute(Sender: TObject);
    procedure MSelPrinter0Click(Sender: TObject);
    procedure AAsyncExecExecute(Sender: TObject);
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
    procedure ExecuteServer(Sender:TObject);
    procedure AppHint(Sender:TObject);
    procedure LoadConfig;
    procedure SaveConfig;
    procedure ShowHelp(AURL:string);
    procedure UpdatePrintSel;
  public
    { Public declarations }
{$IFNDEF FORWEBAX}
    clitree:TFRpCliTreeVCL;
{$ENDIF}
    pagenum:integer;
    metafile:TRpMetafileReport;
    gdidriver:TRpGDIDriver;
    printerindex:TRpPrinterSelect;
    agdidriver:IRpPrintDriver;
    bitmap:TBitmap;
    property aform:TWinControl read faform write SetForm;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure PrintPage;
  end;

var
 FRpMetaVCL:TFRpMetaVCL;

implementation

uses rppdfdriver;

{$R *.dfm}

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
 BExit.Visible:=Not Assigned(faform);
 Exit1.Visible:=BExit.Visible;
end;

procedure TFRpMetaVCL.PrintPage;
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
begin
 inherited Create(AOwner);
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

 configfile:=Obtainininameuserconfig('','','repmand');
 SaveDialog1.Filter:=SRpRepMetafile+'|*.rpmf|'+
   SRpPDFFile+'|*.pdf|'+
   SRpPDFFileUn+'|*.pdf';
{$IFNDEF FORWEBAX}
 clitree:=TFRpCliTreeVCL.Create(Self);
 clitree.Align:=alLeft;
 clitree.Parent:=Self;
 clitree.OnExecuteServer:=ExecuteServer;
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

 // Activates OnHint
 oldonhint:=Application.OnHint;
 Application.OnHint:=AppHint;

 LoadConfig;
end;


procedure TFRpMetaVCL.ShowHelp(AURL:string);
begin
 // Starts the default explorer
 ShellExecute(Self.handle,Pchar('open'),Pchar(AURL),nil,nil,SW_SHOWNORMAL);
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

procedure TFRpMetaVCL.EPageNumKeyPress(Sender: TObject; var Key: Char);
begin
 if Key=chr(13) then
 begin
  pagenum:=StrToInt(EPageNum.Text);
  PrintPage;
 end;
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
 if rpgdidriver.DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
  rpgdidriver.PrintMetafile(metafile,opendialog1.FileName,true,allpages,
   frompage,topage,copies,collate,GetDeviceFontsOption(printerindex),printerindex);
end;

procedure TFRpMetaVCL.ASaveExecute(Sender: TObject);
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

procedure TFRpMetaVCL.AOpenExecute(Sender: TObject);
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
 if ((GetKeyState(VK_ESCAPE) AND $80)>0) then
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

procedure TFRpMetaVCL.ExecuteServer(Sender:TObject);
begin
{$IFNDEF FORWEBAX}
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
{$ENDIF}
end;

procedure TFRpMetaVCL.AAboutExecute(Sender: TObject);
begin
 ShowAbout;
end;

procedure TFRpMetaVCL.AViewConnectExecute(Sender: TObject);
begin
 AViewConnect.Checked:=Not AViewConnect.Checked;
{$IFNDEF FORWEBAX}
 clitree.Width:=clitree.Initialwidth;
 clitree.Visible:=AViewConnect.Checked;
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
{$IFNDEF FORWEBAX}
  clitree.Visible:=AViewConnect.Checked;
  clitree.ComboHost.Text:=inif.ReadString('Preferences','Host','localhost');
  clitree.EUserName.Text:=inif.ReadString('Preferences','UserName','Admin');
  clitree.asynchrohous:=AAsyncexec.Checked;
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
{$IFNDEF FORWEBAX}
  inif.WriteString('Preferences','Host',clitree.ComboHost.Text);
  inif.WriteString('Preferences','UserName',clitree.EUserName.Text);
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
  MSelectPrinter.Items[i].Checked:=MSelectPrinter.Items[i].Tag=Integer(printerindex);
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
{$IFNDEF FORWEBAX}
 clitree.asynchrohous:=AAsyncexec.Checked;
{$ENDIF}
end;

end.