{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdfmainvcl                                    }
{       Main form of report manager designer            }
{       Used by a subreport                             }
{                                                       }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{*******************************************************}

unit rpmdfmainvcl;

interface

{$I rpconf.inc}

uses
  SysUtils,
  Types, Classes,
  rpgdidriver,rpvpreview,rprfvparams,windows,
  Controls, Forms,
  StdCtrls, ComCtrls, ActnList, ImgList, Menus,ExtCtrls,
  Clipbrd,Printers,Consts, Dialogs,ShellApi,
  rpreport,rpmdshfolder,
  rpmdfdesignvcl,rpmdfaboutvcl,rppagesetupvcl,
  rpmdobjinspvcl, rplastsav, ToolWin,rpmdfdatainfovcl,rpmdfsectionintvcl,
  rpmdfgridvcl,rpmdfstrucvcl,rpmdobinsintvcl,rpfparamsvcl,
  rpmdconsts,rptypes, rpsubreport,
  IniFiles,
  rpsection,rpprintitem,
  DB,rpmunits,rpgraphutilsvcl;

const
  // File name in menu width
  C_FILENAME_WIDTH=40;
type
  TFRpMainFVCL = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    ActionList1: TActionList;
    iconlist: TImageList;
    ToolBar1: TToolBar;
    ANew: TAction;
    AOpen: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    New1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    AExit: TAction;
    Exit1: TMenuItem;
    ToolButton3: TToolButton;
    ASave: TAction;
    BSave: TToolButton;
    N2: TMenuItem;
    Save1: TMenuItem;
    SaveDialog1: TSaveDialog;
    ASaveas: TAction;
    Saveas1: TMenuItem;
    APageSetup: TAction;
    Pagesetup1: TMenuItem;
    N3: TMenuItem;
    OpenDialog1: TOpenDialog;
    mainscrollbox: TScrollBox;
    leftpanel: TPanel;
    Splitter1: TSplitter;
    Lastusedfiles: TRpLastUsedStrings;
    MReport: TMenuItem;
    ANewPageHeader: TAction;
    Newpageheader1: TMenuItem;
    MAdd: TMenuItem;
    ANewPageFooter: TAction;
    ANewGroup: TAction;
    Pagefooter1: TMenuItem;
    Groupheaderandfooter1: TMenuItem;
    ANewSubreport: TAction;
    Subreport1: TMenuItem;
    ADeleteSelection: TAction;
    ADeleteSelection1: TMenuItem;
    ANewDetail: TAction;
    Detail1: TMenuItem;
    ADataConfig: TAction;
    Dataaccessconfiguration1: TMenuItem;
    AParams: TAction;
    Parameters1: TMenuItem;
    APrint: TAction;
    APreview: TAction;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    BLabel: TToolButton;
    BArrow: TToolButton;
    BExpression: TToolButton;
    BShape: TToolButton;
    BImage: TToolButton;
    BChart: TToolButton;
    MDisplay: TMenuItem;
    AGridOptions: TAction;
    Grid1: TMenuItem;
    Splitter2: TSplitter;
    MEdit: TMenuItem;
    PrintPreview1: TMenuItem;
    Print1: TMenuItem;
    ACut: TAction;
    ACopy: TAction;
    APaste: TAction;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    APaste1: TMenuItem;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    AAbout: TAction;
    MHelp: TMenuItem;
    ReportManager1: TMenuItem;
    MFields: TPopupMenu;
    est1: TMenuItem;
    ext21: TMenuItem;
    ADocumentation: TAction;
    utorial1: TMenuItem;
    APrintSetup: TAction;
    Printersetup1: TMenuItem;
    AUnitCms: TAction;
    AUnitsinchess: TAction;
    MPreferences: TMenuItem;
    MMeasurement: TMenuItem;
    Cms1: TMenuItem;
    Inchess1: TMenuItem;
    AUserParams: TAction;
    Userparameters1: TMenuItem;
    MDriverSelect: TMenuItem;
    MQtDriver: TMenuItem;
    MGDIDriver: TMenuItem;
    ADriverQT: TAction;
    ADriverGDI: TAction;
    ASystemPrintDialog: TAction;
    MSystemPrint: TMenuItem;
    AkylixPrintBug: TAction;
    MKylixPrintBug: TMenuItem;
    MQtStyle: TMenuItem;
    Windows1: TMenuItem;
    Motif1: TMenuItem;
    MotifPlus1: TMenuItem;
    CDE1: TMenuItem;
    QtSGI1: TMenuItem;
    Platinum1: TMenuItem;
    MQtDefault: TMenuItem;
    AHide: TAction;
    N4: TMenuItem;
    Hide1: TMenuItem;
    AShowAll: TAction;
    Showall1: TMenuItem;
    ASelectAll: TAction;
    ASelectAllText: TAction;
    N5: TMenuItem;
    MSelect: TMenuItem;
    ASelectAll1: TMenuItem;
    ASelectAllText1: TMenuItem;
    ToolButton14: TToolButton;
    ALeft: TAction;
    ToolButton15: TToolButton;
    ARight: TAction;
    AUp: TAction;
    ADown: TAction;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    MMove: TMenuItem;
    Left1: TMenuItem;
    Right1: TMenuItem;
    Up1: TMenuItem;
    Down1: TMenuItem;
    ToolButton19: TToolButton;
    AAlignLeft: TAction;
    AAlignRight: TAction;
    AAlignUp: TAction;
    AAlignDown: TAction;
    AAlignHorz: TAction;
    AAlignVert: TAction;
    MAlign: TMenuItem;
    Left2: TMenuItem;
    Right2: TMenuItem;
    Up2: TMenuItem;
    Down2: TMenuItem;
    Horizontalspace1: TMenuItem;
    Verticalspace1: TMenuItem;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    BBarcode: TToolButton;
    BStatus: TStatusBar;
    AStatusBar: TAction;
    Statusbar1: TMenuItem;
    procedure ANewExecute(Sender: TObject);
    procedure AExitExecute(Sender: TObject);
    procedure AOpenExecute(Sender: TObject);
    procedure ASaveExecute(Sender: TObject);
    procedure ASaveasExecute(Sender: TObject);
    procedure APageSetupExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ANewPageHeaderExecute(Sender: TObject);
    procedure ANewPageFooterExecute(Sender: TObject);
    procedure ANewGroupExecute(Sender: TObject);
    procedure ANewSubreportExecute(Sender: TObject);
    procedure ADeleteSelectionExecute(Sender: TObject);
    procedure ANewDetailExecute(Sender: TObject);
    procedure ADataConfigExecute(Sender: TObject);
    procedure AParamsExecute(Sender: TObject);
    procedure AGridOptionsExecute(Sender: TObject);
    procedure ACutExecute(Sender: TObject);
    procedure ACopyExecute(Sender: TObject);
    procedure APasteExecute(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure APreviewExecute(Sender: TObject);
    procedure AAboutExecute(Sender: TObject);
    procedure APrintExecute(Sender: TObject);
    procedure BExpressionMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShowHelp(AURL:string);
    procedure ADocumentationExecute(Sender: TObject);
    procedure AFeaturesExecute(Sender: TObject);
    procedure APrintSetupExecute(Sender: TObject);
    procedure AUnitCmsExecute(Sender: TObject);
    procedure AUnitsinchessExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure AUserParamsExecute(Sender: TObject);
    procedure ADriverQTExecute(Sender: TObject);
    procedure ADriverGDIExecute(Sender: TObject);
    procedure ASystemPrintDialogExecute(Sender: TObject);
    procedure AkylixPrintBugExecute(Sender: TObject);
    procedure AHideExecute(Sender: TObject);
    procedure AShowAllExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASelectAllExecute(Sender: TObject);
    procedure ASelectAllTextExecute(Sender: TObject);
    procedure ALeftExecute(Sender: TObject);
    procedure ARightExecute(Sender: TObject);
    procedure AUpExecute(Sender: TObject);
    procedure ADownExecute(Sender: TObject);
    procedure AAlignLeftExecute(Sender: TObject);
    procedure AAlignRightExecute(Sender: TObject);
    procedure AAlignUpExecute(Sender: TObject);
    procedure AAlignDownExecute(Sender: TObject);
    procedure AAlignHorzExecute(Sender: TObject);
    procedure AAlignVertExecute(Sender: TObject);
    procedure AStatusBarExecute(Sender: TObject);
  private
    { Private declarations }
    fdesignframe:TFRpDesignFrameVCL;
    fobjinsp:TFRpObjInspVCL;
    lastsaved:TMemoryStream;
    configfile:string;
    updatedmfields:boolean;
    oldonException:TExceptionEvent;
    oldonhint:TNotifyEvent;
    procedure FreeInterface;
    procedure CreateInterface;
    function checkmodified:boolean;
    procedure DoSave;
    procedure DoEnable;
    procedure DoDisable;
    function CheckSave:Boolean;
    procedure UpdateFileMenu;
    procedure OnFileClick(Sender:TObject);
    procedure DoOpen(newfilename:string;showopendialog:boolean);
    procedure OnReadError(Reader: TReader; const Message: string;
     var Handled: Boolean);
    procedure UpdateMFields;
    procedure MFieldsItemClick(Sender:TObject);
    procedure LoadConfig;
    procedure SaveConfig;
    procedure UpdateUnits;
    procedure CorrectScrollBoxes;
    procedure UpdateStyle;
    procedure MyExceptionHandler(Sender:TObject;E:Exception);
    procedure AppHint(Sender:TObject);
  public
    { Public declarations }
    report:TRpReport;
    filename:string;
    freportstructure:TFRpStructureVCL;
    procedure RefreshInterface(Sender: TObject);
    function GetExpressionText:string;
  end;

var
 FRpMainFVCL:TFRpMainFVCL;

implementation


{$R *.dfm}

// Check if it is saved and return true if all is ok
function TFRpMainFVCL.CheckSave:Boolean;
var
 res:TMessageButton;
begin
 Result:=true;
 if report=nil then
  exit;
 if Not ASave.Visible then
  exit;
 if report<>nil then
 begin
  if Not CheckModified then
   exit;
  res:=RpMessageBox(SRpReportChanged,SRpWarning,[smbYes,smbNo,smbCancel],
   smsWarning,smbYes,smbCancel);

  if (res=smbCancel) then
   Raise EAbort.Create(SRpSaveAborted);
  if res=smbNo then
   exit;
  ASaveExecute(Self);
 end;
end;

procedure TFRpMainFVCL.ANewExecute(Sender: TObject);
begin
 if Not checksave then
  exit;
 DoDisable;
 // Creates a new report
 report:=TRpReport.Create(Self);
 report.OnReadError:=OnReadError;
 report.CreateNew;
 filename:='';

 DoEnable;
 FormResize(Self);
end;

procedure TFRpMainFVCL.DoEnable;
begin
 // Save the report for seeing after if it's modified
 if Assigned(lastsaved) then
 begin
  lastsaved.free;
  lastsaved:=nil;
 end;
 lastsaved:=TMemorystream.create;
 report.SaveToStream(lastsaved);

 CreateInterface;
end;

procedure TFRpMainFVCL.DoDisable;
begin
 FreeInterface;
 report.free;
 report:=nil;
 // Save the report for seeing after if it's modified
 if Assigned(lastsaved) then
 begin
  lastsaved.free;
  lastsaved:=nil;
 end;
end;

procedure TFRpMainFVCL.AExitExecute(Sender: TObject);
begin
 Close;
end;

procedure TFRpMainFVCL.DoOpen(newfilename:string;showopendialog:boolean);
begin
 if Not checksave then
  exit;
 // Opens an existing report
 if Showopendialog then
 begin
  if Not OpenDialog1.execute then
   exit;
 end
 else
  OpenDialog1.Filename:=newfilename;
 FreeInterface;
 filename:='';
 report.free;
 report:=nil;
 DoDisable;
 // Creates a new report
 report:=TRpReport.Create(Self);
 try
  report.OnReadError:=OnReadError;
  report.LoadFromFile(OpenDialog1.FileName);
  filename:=OpenDialog1.FileName;
  Savedialog1.filename:=filename;
  LastUsedFiles.UseString(filename);
  UpdateFileMenu;
  DoEnable;
 except
  report.free;
  report:=nil;
  filename:='';
  raise;
 end;
end;

procedure TFRpMainFVCL.AOpenExecute(Sender: TObject);
begin
 DoOpen(filename,true);
 FormResize(Self);
end;

procedure TFRpMainFVCL.ASaveExecute(Sender: TObject);
begin
 Assert(report<>nil,'Called Save without a report assigned');
 // Saves the current report
 if Length(filename)>0 then
 begin
  DoSave;
 end
 else
 begin
  ASaveAsExecute(Self);
 end;
end;


procedure TFRpMainFVCL.FreeInterface;
begin
 // Frees the interface for the report
 ASave.Enabled:=false;
 ASaveas.Enabled:=false;
 MReport.Visible:=false;
 ANewPageHeader.Enabled:=false;
 ANewPageFooter.Enabled:=false;
 ANewGroup.Enabled:=false;
 ANewSubReport.Enabled:=false;
 ADeleteSelection.Enabled:=false;
 AnewDetail.Enabled:=false;
 ADataConfig.Enabled:=false;
 APreview.Enabled:=false;
 ACut.Enabled:=False;
 ACopy.Enabled:=FalsE;
 APaste.Enabled:=False;
 AShowAll.Enabled:=False;
 AHide.Enabled:=False;
 APrint.Enabled:=false;
 AGridOptions.Enabled:=false;
 MDisplay.Visible:=false;
 MEdit.Visible:=false;

 // Palette
 BArrow.Enabled:=false;
 BLabel.Enabled:=false;
 BExpression.Enabled:=false;
 BShape.Enabled:=false;
 BImage.Enabled:=false;
 BBarcode.Enabled:=false;
 BChart.Enabled:=false;
 BArrow.Down:=false;
 BLabel.Down:=false;
 BExpression.Down:=false;
 BShape.Down:=false;
 BImage.Down:=false;
 BBarcode.Down:=false;
 BChart.Down:=false;

 AParams.Enabled:=False;
 APageSetup.Enabled:=false;
 Caption:=SRpRepman;

 freportstructure.free;
 fdesignframe.free;
 fobjinsp.free;
 fobjinsp:=nil;
 fdesignframe:=nil;
 freportstructure:=nil;
 mainscrollbox.Visible:=false;
 updatedmfields:=false;
end;

procedure TFRpMainFVCL.CreateInterface;
begin
 // Creates an interface for the report
 ASave.Enabled:=true;
 ASaveas.Enabled:=True;
 APageSetup.Enabled:=True;
 MReport.Visible:=True;
 ANewPageHeader.Enabled:=True;
 ANewPageFooter.Enabled:=True;
 ANewGroup.Enabled:=true;
 ANewSubReport.Enabled:=True;
 ADeleteSelection.Enabled:=true;
 AnewDetail.Enabled:=true;
 ADataConfig.Enabled:=true;
 APreview.Enabled:=true;
 ACut.Enabled:=False;
 ACopy.Enabled:=FalsE;
 AHide.Enabled:=False;
 APaste.Enabled:=true;
 AShowAll.Enabled:=True;
 APrint.Enabled:=true;
 AGridOptions.Enabled:=true;
 MDisplay.Visible:=true;
 MEdit.Visible:=true;

 // Palette
 BArrow.Enabled:=true;
 BLabel.Enabled:=true;
 BExpression.Enabled:=true;
 BShape.Enabled:=true;
 BImage.Enabled:=true;
 BBarcode.Enabled:=true;
 BChart.Enabled:=true;
 BArrow.Down:=true;

 AParams.Enabled:=True;
 if length(filename)<1 then
  Caption:=SRpRepman+'-'+SRpUntitled
 else
  Caption:=SRpRepman+'-'+filename;
 // Create the report structure frame
 fobjinsp:=TFRpObjInspVCL.Create(Self);
 fobjinsp.Align:=alclient;
 fobjinsp.Parent:=leftpanel;
 freportstructure:=TFRpStructureVCL.Create(Self);
 freportstructure.Align:=alTop;
 freportstructure.Parent:=leftPanel;
 fdesignframe:=TFRpDesignFrameVCL.Create(Self);
 fobjinsp.DesignFrame:=fdesignframe;
 fdesignframe.Align:=alclient;
 fdesignframe.Parent:=MainScrollBox;
 fdesignframe.freportstructure:=freportstructure;
 freportstructure.designframe:=fdesignframe;
 splitter2.Top:=freportstructure.Height+10;

 fdesignframe.objinsp:=fobjinsp;
 freportstructure.objinsp:=fobjinsp;
 freportstructure.report:=report;
 fdesignframe.report:=report;

 mainscrollbox.Visible:=true;
end;

procedure TFRpMainFVCL.ASaveasExecute(Sender: TObject);
begin
 Assert(report<>nil,'Called Save without a report assigned');
 // Saves the report
 if SaveDialog1.Execute then
 begin
  DoSave;
 end
 else
  Raise EAbort.Create(SRpSaveAborted);
end;

procedure TFRpMainFVCL.DoSave;
begin
 Assert(report<>nil,'Called DoSave without a report assigned');

 report.SaveToFile(savedialog1.filename);

 // After saving update the lastsaved stream
 if assigned(lastsaved) then
 begin
  lastsaved.free;
  lastsaved:=nil;
 end;
 lastsaved:=TMemorystream.create;
 report.SaveToStream(lastsaved);

 filename:=savedialog1.filename;
 LastUsedFiles.UseString(filename);
 UpdateFileMenu;
 Caption:=SRpRepman+'-'+filename;
end;

procedure TFRpMainFVCL.APageSetupExecute(Sender: TObject);
begin
 Assert(report<>nil,'Called Page setup without a report assigned');

 ExecutePageSetup(report);
end;


// A report is known is modified by comparing the saving of
// the current report with the last saved report (lastsaved)
function TFRpMainFVCL.checkmodified:boolean;
var
 newsave:TMemoryStream;
begin
 Result:=true;
 if report=nil then
  exit;
 if Not Assigned(lastsaved) then
  exit;

 newsave:=TMemoryStream.create;
 try
  report.SaveToStream(newsave);
  if streamcompare(lastsaved,newsave) then
   result:=false;
 finally
  newsave.free;
 end;
end;

procedure TFRpMainFVCL.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 try
  canclose:=CheckSave;
 except
  canclose:=false;
 end;
end;

procedure TFRpMainFVCL.FormCreate(Sender: TObject);
begin
 // Inits Bools Arrays
 BoolToStr(True,True);
 ALeft.ShortCut:=ShortCut(VK_LEFT,[ssCtrl]);
 ARight.ShortCut:=ShortCut(VK_RIGHT,[ssCtrl]);
 AUp.ShortCut:=ShortCut(VK_UP,[ssCtrl]);
 ADown.ShortCut:=ShortCut(VK_DOWN,[ssCtrl]);
 MQtStyle.Visible:=false;
 MSystemPrint.Visible:=false;

 OpenDialog1.Filter := SRpRepFile+'|*.rep';
 SaveDialog1.Filter := SRpRepFile+'|*.rep|'+SRpAnyFile+'|*.*';
 // Sets on exception event
 oldonexception:=Application.OnException;
 Forms.Application.OnException:=MyExceptionHandler;


 Application.Title:=SRpRepman;
 configfile:=Obtainininameuserconfig('','','repmand');
  LastUsedFiles.CaseSensitive:=False;
  // Visible driver selection
 LastUsedFiles.LoadFromConfigFile(configfile);
 UpdateFileMenu;
 LoadConfig;
 // A bug in Kylix loading decimal sep, and thousand sep
 // Translate menus and actions
 File1.Caption:=TranslateStr(0,File1.Caption);
 Caption:=TranslateStr(1,Caption);
 MReport.Caption:=TranslateStr(2,MReport.Caption);
 MEdit.Caption:=TranslateStr(3,MEdit.Caption);
 MDisplay.Caption:=TranslateStr(4,MDisplay.Caption);
 AGridOptions.Caption:=TranslateStr(7,AGridOptions.Caption);
 AGridOptions.Hint:=TranslateStr(8,AGridOptions.Hint);
 ACut.Caption:=TranslateStr(9,ACut.Caption);
 ACopy.Caption:=TranslateStr(10,ACopy.Caption);
 APaste.Caption:=TranslateStr(11,APaste.Caption);
 ACut.Hint:=TranslateStr(12,ACut.Hint);
 ACopy.Hint:=TranslateStr(13,ACopy.Hint);
 APaste.Hint:=TranslateStr(14,APaste.Hint);
 AHide.Caption:=TranslateStr(15,AHide.Caption);
 AHide.Hint:=TranslateStr(16,AHide.Hint);
 AShowAll.Caption:=TranslateStr(17,AShowAll.Caption);
 AShowAll.Hint:=TranslateStr(18,AShowAll.Hint);
 ASelectAll.Caption:=TranslateStr(19,ASelectAll.Caption);
 ASelectAllText.Caption:=TranslateStr(117,ASelectAllText.Caption);
 ASelectAllText.Hint:=TranslateStr(118,ASelectAllText.Hint);
 ASelectAll.Hint:=TranslateStr(20,ASelectAll.Hint);
 MSelect.Caption:=TranslateStr(21,MSelect.Caption);
 MMove.Caption:=TranslateStr(22,MMove.Caption);
 ALeft.Caption:=TranslateStr(23,ALeft.Caption);
 ALeft.Hint:=TranslateStr(24,ALeft.Hint);
 ARight.Caption:=TranslateStr(25,ARight.Caption);
 ARight.Hint:=TranslateStr(26,ARight.Hint);
 AUp.Caption:=TranslateStr(27,AUp.Caption);
 AUp.Hint:=TranslateStr(28,AUp.Hint);
 ADown.Caption:=TranslateStr(29,ADown.Caption);
 ADown.Hint:=TranslateStr(30,ADown.Hint);
 MAlign.Caption:=TranslateStr(31,MAlign.Caption);
 AAlignLeft.Caption:=TranslateStr(23,AAlignLeft.Caption);
 AAlignRight.Caption:=TranslateStr(25,AAlignRight.Caption);
 AAlignUp.Caption:=TranslateStr(27,AAlignUp.Caption);
 AAlignDown.Caption:=TranslateStr(29,AAlignDown.Caption);
 AAlignLeft.Hint:=TranslateStr(32,AAlignLeft.Hint);
 AAlignRight.Hint:=TranslateStr(33,AAlignRight.Hint);
 AAlignUp.Hint:=TranslateStr(34,AAlignUp.Hint);
 AAlignDown.Hint:=TranslateStr(35,AAlignDown.Hint);
 AAlignHorz.Caption:=TranslateStr(36,AAlignHorz.Caption);
 AAlignHorz.Hint:=TranslateStr(37,AAlignHorz.Hint);
 AAlignVert.Caption:=TranslateStr(38,AAlignVert.Caption);
 AAlignVert.Hint:=TranslateStr(39,AAlignVert.Hint);
 ANew.Caption:=TranslateStr(40,ANew.Caption);
 ANew.Hint:=TranslateStr(41,ANew.Hint);
 AOpen.Caption:=TranslateStr(42,AOpen.Caption);
 AOpen.Hint:=TranslateStr(43,AOpen.Hint);
 AExit.Caption:=TranslateStr(44,AExit.Caption);
 AExit.Hint:=TranslateStr(45,AExit.Hint);
 ASave.Caption:=TranslateStr(46,ASave.Caption);
 ASave.Hint:=TranslateStr(47,ASave.Hint);
 ASaveas.Caption:=TranslateStr(48,ASaveas.Caption);
 ASaveas.Hint:=TranslateStr(49,ASaveas.Hint);
 APageSetup.Caption:=TranslateStr(50,APageSetup.Caption);
 APageSetup.Hint:=TranslateStr(51,APageSetup.Hint);
 APrint.Caption:=TranslateStr(52,APrint.Caption);
 APrint.Hint:=TranslateStr(53,APrint.Hint);
 APreview.Caption:=TranslateStr(54,APreview.Caption);
 APreview.Hint:=TranslateStr(55,APreview.Hint);
 APrintSetup.Caption:=TranslateStr(56,APrintSetup.Caption);
 APrintSetup.Hint:=TranslateStr(57,APrintSetup.Hint);
 MHelp.Caption:=TranslateStr(6,MHelp.Caption);
 AAbout.Caption:=TranslateStr(58,AAbout.Caption);
 AAbout.Hint:=TranslateStr(59,AABout.Hint);
 ADocumentation.Caption:=TranslateStr(60,ADocumentation.Caption);
 ADocumentation.Hint:=TranslateStr(61,ADocumentation.Hint);
 MMeasurement.Caption:=TranslateStr(62,MMeasurement.Caption);
 AUnitcms.Caption:=TranslateStr(63,AUnitcms.Caption);
 AUnitcms.Hint:=TranslateStr(64,AUnitcms.Hint);
 AUnitsinchess.Caption:=TranslateStr(65,AUnitsinchess.Caption);
 AUnitsinchess.Hint:=TranslateStr(66,AUnitsinchess.Hint);
 ADocumentation.Caption:=TranslateStr(60,ADocumentation.Caption);
 ADocumentation.Hint:=TranslateStr(61,ADocumentation.Hint);
 MDriverSelect.Caption:=TranslateStr(67,MDriverSelect.Caption);
 ADriverQt.Caption:=TranslateStr(68,ADriverQt.Caption);
 ADriverQt.Hint:=TranslateStr(69,ADriverQt.Hint);
 ADriverGDI.Caption:=TranslateStr(70,ADriverGDI.Caption);
 ADriverGDI.Hint:=TranslateStr(71,ADriverGDI.Hint);
 AKylixPrintBug.Caption:=TranslateStr(74,AKylixPrintBug.Caption);
 AKylixPrintBug.Hint:=TranslateStr(75,AKylixPrintBug.Hint);
 AStatusBar.Caption:=TranslateStr(76,AStatusBar.Caption);
 AStatusBar.Hint:=TranslateStr(77,AStatusBar.Hint);
 MPreferences.Caption:=TranslateStr(5,MPreferences.Caption);
 ASystemPrintDialog.Caption:=TranslateStr(72,ASystemPrintDialog.Caption);
 ASystemPrintDialog.Hint:=TranslateStr(73,ASystemPrintDialog.Hint);
 MQtStyle.Caption:=TranslateStr(78,MQtStyle.Caption);
 MQtStyle.Hint:=TranslateStr(79,MQtStyle.Hint);
 MQtDefault.Caption:=TranslateStr(80,MQtDefault.Caption);
 BArrow.Hint:=TranslateStr(81,BArrow.Hint);
 BLabel.Hint:=TranslateStr(82,BLabel.Hint);
 BExpression.Hint:=TranslateStr(83,BExpression.Hint);
 BShape.Hint:=TranslateStr(84,BShape.Hint);
 BImage.Hint:=TranslateStr(85,BImage.Hint);
 BBarCode.Hint:=TranslateStr(86,BBarCode.Hint);
 BChart.Hint:=TranslateStr(87,BChart.Hint);

 ANewPageHeader.Caption:=TranslateStr(119,ANewPageHeader.Caption);
 ANewPageHeader.Hint:=TranslateStr(120,ANewPageHeader.Hint);
 ANewPageFooter.Caption:=TranslateStr(121,ANewPageFooter.Caption);
 ANewPageFooter.Hint:=TranslateStr(122,ANewPageFooter.Hint);
 ANewGroup.Caption:=TranslateStr(123,ANewGroup.Caption);
 ANewGroup.Hint:=TranslateStr(124,ANewGroup.Hint);
 ANewSubReport.Caption:=TranslateStr(125,ANewSubreport.Caption);
 ANewSubReport.Hint:=TranslateStr(126,ANewSubreport.Hint);
 ADeleteSelection.Caption:=TranslateStr(127,ADeleteSelection.Caption);
 ADeleteSelection.Hint:=TranslateStr(128,ADeleteSelection.Hint);
 ANewDetail.Caption:=TranslateStr(129,ANewDetail.Caption);
 ANewDetail.Hint:=TranslateStr(130,ANewDetail.Hint);
 ADataConfig.Caption:=TranslateStr(131,ADataConfig.Caption);
 ADataConfig.Hint:=TranslateStr(132,ADataConfig.Hint);
 AParams.Caption:=TranslateStr(133,Aparams.Caption);
 AParams.Hint:=TranslateStr(134,Aparams.Hint);
 AUserParams.Caption:=TranslateStr(135,AUserparams.Caption);
 AUserParams.Hint:=TranslateStr(136,AUserparams.Hint);
 MAdd.Caption:=TranslateStr(149,MAdd.Caption);
 SaveDialog1.Title:=TranslateStr(213,SaveDialog1.Title);
 OpenDialog1.Title:=TranslateStr(214,OpenDialog1.Title);

 // Activates OnHint
 oldonhint:=Application.OnHint;
 Application.OnHint:=AppHint;
end;

procedure TFRpMainFVCL.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 LastUsedFiles.SaveToConfigFile(configfile);
 SaveConfig;
end;

procedure TFRpMainFVCL.UpdateFileMenu;
var
 exitindex:integer;
 alist:TStringlist;
 aitem:TmenuItem;
 i:integer;
begin
 // Search the last menu item
 exitindex:=0;
 While File1.Items[exitindex].Action<>AExit do
 begin
  inc(exitindex);
  if exitindex>=File1.Count then
   raise Exception.Create(SrpErrorProcesingFileMenu);
 end;
 // Remove all the last items
 inc(exitindex);
 While exitindex<(File1.count) do
  File1.Items[exitindex].Free;
 // Add the files and the on click
 alist:=TStringList.create;
 try
  Lastusedfiles.FillWidthShortNames(alist,C_FILENAME_WIDTH);
  // Creation of menu items
  aItem:=TMenuItem.Create(Self);
  File1.Add(aitem);
  aItem.Caption:='-';
  for i:=0 to alist.count-1 do
  begin
   aItem:=TMenuItem.Create(Self);
   File1.Add(aitem);
   aItem.Caption:=alist.strings[i];
   aItem.Tag:=i;
   aItem.OnClick:=OnFileClick;
  end;
 finally
  alist.free;
 end;
end;

procedure TFRpMainFVCL.OnFileClick(Sender:TObject);
var
 newfilename:string;
begin
 newfilename:=LastusedFiles.LastUsed.Strings[TComponent(Sender).tag];
 // Try to open the file
 DoOpen(newfilename,false);
end;


procedure TFRpMainFVCL.RefreshInterface(Sender: TObject);
begin
 FreeInterface;
 CreateInterface;
 FormResize(self);
end;

procedure TFRpMainFVCL.ANewPageHeaderExecute(Sender: TObject);
begin
 // Inserts a new page header
 Assert(report<>nil,'Called AddNew PageHeader without a report assigned');

 freportstructure.FindSelectedSubreport.AddPageHeader;

 RefreshInterface(Self);
end;

procedure TFRpMainFVCL.ANewPageFooterExecute(Sender: TObject);
begin
 // Inserts a new page footer
 Assert(report<>nil,'Called AddNewPageFooter without a report assigned');

 freportstructure.FindSelectedSubreport.AddPageFooter;

 RefreshInterface(Self);
end;

procedure TFRpMainFVCL.ANewGroupExecute(Sender: TObject);
var
 newgroupname:string;
begin
 // Inserts a new group header and footer
 Assert(report<>nil,'Called AddNewGroupout a report unassigned');

 newgroupname:=Uppercase(Trim(RpInputBox(SRpNewGroup,SRpSGroupName,'')));
 if length(newgroupname)>0 then
 begin
  freportstructure.FindSelectedSubreport.AddGroup(newgroupname);

  RefreshInterface(Self);
 end;
end;

procedure TFRpMainFVCL.ANewSubreportExecute(Sender: TObject);
begin
 // Inserts a new group header and footer
 Assert(report<>nil,'Called AddSubReport a report unassigned');

 report.AddSubReport;

 RefreshInterface(Self);
end;

procedure TFRpMainFVCL.ADeleteSelectionExecute(Sender: TObject);
begin
 // Deletes section
 Assert(report<>nil,'Called ADeleteSection a report unassigned');

 if RpMessageBox(SRpSureDeleteSection,SRpWarning,[smbok,smbcancel],smsWarning,smbCancel)=smbOk then
 begin
  freportstructure.DeleteSelectedNode;
  RefreshInterface(Self);
 end;
end;

procedure TFRpMainFVCL.ANewDetailExecute(Sender: TObject);
begin
 // Inserts a new group header and footer
 Assert(report<>nil,'Called ADeleteSection a report unassigned');

 freportstructure.FindSelectedSubreport.AddDetail;

 RefreshInterface(Self);
end;

procedure TFRpMainFVCL.ADataConfigExecute(Sender: TObject);
begin
 // Data info configuration dialog
 ShowDataConfig(report);
 fobjinsp.ClearMultiSelect;
 fdesignframe.UpdateSelection(true);
 updatedmfields:=false;
end;

procedure TFRpMainFVCL.AParamsExecute(Sender: TObject);
begin
 ShowParamDef(report.Params,report.DataInfo);
end;

procedure TFRpMainFVCL.AGridOptionsExecute(Sender: TObject);
begin
 fobjinsp.ClearMultiSelect;
 ModifyGridProperties(report);
 fdesignframe.UpdateSelection(true);
end;

procedure TFRpMainFVCL.ACutExecute(Sender: TObject);
var
 sectionintf:TRpSectionInterface;
 aitem:TRpSizePosInterface;
 alist:TStringList;
 i:integer;
begin
 // Delete current selection
 if fobjinsp.SelectedItems.Count<1 then
  exit;
 if (Not (fobjinsp.SelectedItems.Objects[0] is TRpSizePosInterface)) then
  exit;
 ACopy.Execute;
 alist:=TStringList.Create;
 try
  alist.Assign(fobjinsp.SelectedItems);
  fobjinsp.ClearMultiSelect;
  for i:=0 to alist.count-1 do
  begin
   aitem:=TRpSizePosInterface(alist.Objects[i]);
   sectionintf:=TRpSectionInterface(aitem.SectionInt);
   TRpSection(sectionintf.printitem).DeleteComponent(aitem.printitem);
  end;
 finally
  alist.free;
 end;
 fdesignframe.UpdateSelection(true);
end;

procedure TFRpMainFVCL.ACopyExecute(Sender: TObject);
var
 pitem:TRpCommonComponent;
 acompo:TComponent;
 i:integer;
begin
 if fobjinsp.SelectedItems.Count<1 then
  exit;
 if (Not (fobjinsp.SelectedItems.Objects[0] is TRpSizePosInterface)) then
  exit;
 acompo:=TRpReport.Create(nil);
 try
  acompo.Name:='TheOwner';
  for i:=0 to fobjinsp.SelectedItems.Count-1 do
  begin
   pitem:=TRpSizePosInterface(fobjinsp.SelectedItems.Objects[i]).printitem;
   report.Removecomponent(pitem);
   acompo.InsertComponent(pitem);
  end;
  Clipboard.SetComponent(acompo);
  for i:=0 to fobjinsp.SelectedItems.Count-1 do
  begin
   pitem:=TRpSizePosInterface(fobjinsp.SelectedItems.Objects[i]).printitem;
   acompo.RemoveComponent(pitem);
   report.Insertcomponent(pitem);
  end;
 finally
  acompo.free;
 end;
end;

procedure TFRpMainFVCL.APasteExecute(Sender: TObject);
var
 section:TRpSection;
 secint:TRpSectionInterface;
 compo:TComponent;
 acompo:TComponent;
 i:integer;
 alist:TList;
 pitem:TRpCommonPosComponent;
begin
 if fobjinsp.SelectedItems.Count<1 then
  exit;
 if (fobjinsp.SelectedItems.Objects[0] is TRpSectionInterface) then
 begin
  secint:=TRpSectionInterface(fobjinsp.CompItem);
 end
 else
 begin
  secint:=TRpSectionInterface(TRpSizePosInterface(fobjinsp.SelectedItems.Objects[0]).SectionInt);
 end;
 fobjinsp.ClearMultiSelect;
 section:=TrpSection(secint.printitem);
 acompo:=TRpReport.Create(nil);
 try
  acompo.Name:='AOwner';
  compo:=Clipboard.GetComponent(acompo,acompo);
  alist:=TList.Create;
  try
   for i:=0 to compo.ComponentCount-1 do
   begin
    alist.Add(compo.Components[i]);
   end;
   for i:=0 to alist.Count-1 do
   begin
    if (not (TObject(alist.items[i]) is TrpCommonPosComponent)) then
    begin
     Raise Exception.Create(SRpInvalidClipboardFormat+':'+TObject(alist.items[i]).ClassName);
    end;
    pitem:=TrpCommonPosComponent(alist.Items[i]);
    compo.RemoveComponent(pitem);
    pitem.Name:='';
    (section.Components.Add).Component:=pitem;
    report.InsertComponent(pitem);
    Generatenewname(pitem);
   end;
   fdesignframe.UpdateSelection(true);
  finally
   alist.Free;
  end;
 finally
  acompo.Free;
 end;
end;

procedure TFRpMainFVCL.Splitter1Moved(Sender: TObject);
var
 olditem:TRpSizeInterface;
begin
 // Assigns then objinsp
 fobjinsp.InvalidatePanels;
 olditem:=fobjinsp.CompItem;
 fobjinsp.AddCompItem(nil,true);
 fobjinsp.AddCompItem(olditem,true);
 // Correct scrollboxes
 CorrectScrollBoxes;

end;

procedure TFRpMainFVCL.OnReadError(Reader: TReader; const Message: string;
    var Handled: Boolean);
begin
 Handled:=RpMessageBox(SRpErrorReadingReport+#10+Message+#10+SRpIgnoreError,SRpWarning,[smbYes,smbNo],smsWarning,smbYes)=smbYes;
end;

procedure TFRpMainFVCL.APreviewExecute(Sender: TObject);
begin
 // Previews the report
 rpvpreview.ShowPreview(report,caption);
end;

procedure TFRpMainFVCL.AAboutExecute(Sender: TObject);
begin
 ShowAbout;
end;

procedure TFRpMainFVCL.APrintExecute(Sender: TObject);
var
 allpages,collate:boolean;
 frompage,topage,copies:integer;
begin
  allpages:=true;
  collate:=report.CollateCopies;
  frompage:=1; topage:=999999;
  copies:=report.Copies;
  rpgdidriver.PrinterSelection(report.PrinterSelect);
  if rpgdidriver.DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
   rpgdidriver.PrintReport(report,Caption,true,allpages,frompage,topage,copies,collate);
end;


procedure TFRpMainFVCL.MFieldsItemClick(Sender:TObject);
var
 i:integer;
begin
 for i:=0 to MFields.Items.Count-1 do
 begin
  if MFields.Items[i]=Sender then
   MFields.Items[i].Checked:=true
  else
   MFields.Items[i].Checked:=false;
 end;
end;

procedure TFRpMainFVCL.UpdateMFields;
var
 i,j:integer;
 alist:TStringList;
 datas:TDataset;
 alias:string;
 aitem:TMenuItem;
begin
 if updatedmfields then
  exit;
 updatedmfields:=true;
 MFields.Items.Clear;
 alist:=TStringList.Create;
 try
  for i:=0 to report.DataInfo.Count-1 do
  begin
   try
    alias:=report.DataInfo.Items[i].Alias;
    report.DataInfo.Items[i].Connect(report.DatabaseInfo,report.Params);
    datas:=report.DataInfo.Items[i].Dataset;
    for j:=0 to datas.FieldCount-1 do
    begin
     aitem:=TMenuItem.Create(MFields);
     aitem.Caption:=alias+'.'+datas.fields[j].FieldName;
     aitem.OnClick:=MFieldsItemClick;
     MFields.Items.Add(aitem);
    end;
   except

   end;
  end;
 finally
  alist.free;
 end;
end;

procedure TFRpMainFVCL.BExpressionMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
 apoint:TPoint;
begin
 // Popups Fields
 UpdateMFields;

 if MFields.Items.Count>0 then
 begin
  apoint.x:=BExpression.Left;
  apoint.Y:=BExpression.Top+BExpression.Height;
  apoint:=ClientToScreen(apoint);
  MFields.Popup(apoint.x,apoint.Y);
 end;
end;


function TFRpMainFVCL.GetExpressionText:string;
var
 i:integer;
begin
 Result:='2+2';
 for i:=0 to MFields.Items.Count-1 do
 begin
  if MFields.Items[i].Checked then
  begin
   Result:=MFields.Items[i].Caption;
   if Pos(' ',Trim(Result))>0 then
    Result:='['+Trim(Result)+']';
   break;
  end;
 end;
end;

procedure TFRpMainFVCL.ShowHelp(AURL:string);
begin
 // Starts the default explorer
 ShellExecute(Self.handle,Pchar('open'),Pchar(AURL),nil,nil,SW_SHOWNORMAL);
end;

procedure TFRpMainFVCL.ADocumentationExecute(Sender: TObject);
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

procedure TFRpMainFVCL.AFeaturesExecute(Sender: TObject);
var
 aurl:string;
 Directorysep:string;
begin
 aurl:=ExtractFilePath(Application.Exename);
 Directorysep:='\';
 aurl:=aurl+'doc'+Directorysep+'features.html';
 ShowHelp(aurl);
end;

procedure TFRpMainFVCL.APrintSetupExecute(Sender: TObject);
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

procedure TFRpMainFVCL.LoadConfig;
var
 inif:TInifile;
begin
 inif:=TIniFile.Create(configfile);
 try
  AUnitCms.Checked:=inif.ReadBool('Preferences','UnitCms',true);
  ADriverQt.Checked:=False;
  ADriverGDI.Checked:=True;
  AsystemPrintDialog.Checked:=True;
  BStatus.Visible:=inif.ReadBool('Preferences','StatusBar',True);
  AStatusBar.Checked:=BStatus.Visible;
  ADriverGDI.Checked:=Not ADriverQT.Checked;
  AUnitsinchess.Checked:=Not AUnitCms.Checked;
  UpdateStyle;
  UpdateUnits;
 finally
  inif.free;
 end;
end;

procedure TFRpMainFVCL.SaveConfig;
var
 inif:TInifile;
begin
 inif:=TIniFile.Create(configfile);
 try
  inif.WriteBool('Preferences','UnitCms',AUnitCms.Checked);
  inif.WriteBool('Preferences','DriverQT',ADriverQT.Checked);
  inif.WriteBool('Preferences','SystemPrintDialog',AsystemPrintDialog.Checked);
  inif.WriteBool('Preferences','StatusBar',BStatus.Visible);
  inif.WriteBool('Preferences','KylixPrintBug',AKylixPrintBug.Checked);
  inif.UpdateFile;
 finally
  inif.free;
 end;
end;


procedure TFRpMainFVCL.UpdateUnits;
begin
 if assigned(fobjinsp) then
  fobjinsp.ClearMultiSelect;
 if AUnitCms.Checked then
  rpmunits.defaultunit:=rpUnitcms
 else
  rpmunits.defaultunit:=rpUnitinchess;
 if assigned(fdesignframe) then
 begin
  fdesignframe.UpdateInterface;
  fdesignframe.UpdateSelection(true);
 end;
end;


procedure TFRpMainFVCL.AUnitCmsExecute(Sender: TObject);
begin
 AUnitCms.Checked:=true;
 AUnitsInchess.Checked:=false;
 UpdateUnits;
end;

procedure TFRpMainFVCL.AUnitsinchessExecute(Sender: TObject);
begin
 AUnitCms.Checked:=false;
 AUnitsInchess.Checked:=true;
 UpdateUnits;
end;

procedure TFRpMainFVCL.CorrectScrollBoxes;
begin
 if assigned(fdesignframe) then
 begin
  // A bug in aligments CLX Windows and Linux Still present in D7/K3
  // forced me to include this corrections
  // To reproduce the bug
  // Maximize, open a report, select text control and restore
  // the top ruler will disapear
//   fdesignframe.SectionScrollBox.HorzScrollBar.Position:=0;
//   fdesignframe.SectionScrollBox.VertScrollBar.Position:=0;
//   fdesignframe.SectionScrollBox.Align:=alnone;
//   fdesignframe.HorzScrollBar.Position:=0;
//   fdesignframe.VertScrollBar.Position:=0;
//   fdesignframe.SectionScrollBox.Align:=alClient;
//   HorzScrollBar.Position:=0;
//   VertScrollBar.Position:=0;
//   MainScrollBox.HorzScrollBar.Position:=0;
//   MainScrollBox.VertScrollBar.Position:=0;
//   LeftPanel.Left:=0;
//   Splitter1.Left:=20;
//   ToolBar1.Left:=0;
 end;
end;


procedure TFRpMainFVCL.FormResize(Sender: TObject);
begin
// The bug in form resize is not present in VCL
// if assigned(fdesignframe) then
// begin
//  fdesignframe.UpdateInterface;
//  CorrectScrollBoxes;
// end;
end;

procedure TFRpMainFVCL.AUserParamsExecute(Sender: TObject);
begin
  rprfvparams.ShowUserParams(report);
end;

procedure TFRpMainFVCL.ADriverQTExecute(Sender: TObject);
begin
 ADriverQT.Checked:=true;
 ADriverGDI.Checked:=false;
end;

procedure TFRpMainFVCL.ADriverGDIExecute(Sender: TObject);
begin
 ADriverGDI.Checked:=true;
 ADriverQT.Checked:=false;
end;

procedure TFRpMainFVCL.ASystemPrintDialogExecute(Sender: TObject);
begin
 ASystemPrintDialog.Checked:=Not ASystemPrintDialog.Checked;
end;

procedure TFRpMainFVCL.AkylixPrintBugExecute(Sender: TObject);
begin
 AKylixPrintBug.Checked:=Not AKylixPrintBug.Checked;
end;


procedure TFRpMainFVCL.UpdateStyle;
begin
end;


procedure TFRpMainFVCL.AHideExecute(Sender: TObject);
var
 i:integer;
begin
 if fobjinsp.SelectedItems.Count<1 then
  exit;
 if (Not (fobjinsp.SelectedItems.Objects[0] is TRpSizePosInterface)) then
  exit;
 for i:=0 to fobjinsp.SelectedItems.Count-1 do
 begin
  TRpSizePosInterface(fobjinsp.SelectedItems.Objects[i]).Visible:=false;
 end;
 fobjinsp.ClearMultiselect;
end;

procedure TFRpMainFVCL.AShowAllExecute(Sender: TObject);
begin
 // Shows all components
 fdesignframe.ShowAllHiden;
end;


procedure TFRpMainFVCL.MyExceptionHandler(Sender:TObject;E:Exception);
var
 compo:TComponent;
 subrep:TRpSubreport;
 sec,secsel:TRpSection;
 secint:TRpSectionInterface;
 printitem:TRpCommonComponent;
 i,j,k:integer;
 secintitem:TRpSizePosInterface;
begin
 // Looks the exception type
 if ((E is TRpReportException) And Assigned(fdesignframe) )then
 begin
  compo:=TRpReportException(E).Component;
  if compo is TRpSubReport then
  begin
   subrep:=TRpSubReport(compo);
   freportstructure.SelectDataItem(subrep);
  end
  else
  begin
   // If is a section
   if compo is TRpSection then
   begin
    sec:=TRpSection(compo);
    freportstructure.SelectDataItem(sec);
   end
   else
   begin
    if (compo is TRpCommonComponent) then
    begin
     printitem:=TRpCommonComponent(compo);
     secsel:=nil;
     for i:=0 to report.SubReports.Count-1 do
     begin
      subrep:=report.SubReports.Items[i].SubReport;
      for j:=0 to subrep.Sections.Count-1 do
      begin
       sec:=subrep.Sections.Items[j].Section;
       for k:=0 to sec.components.Count-1 do
       begin
        if printitem=sec.Components.Items[k].Component then
        begin
         secsel:=sec;
         break;
        end;
       end;
       if Assigned(secsel) then
        break;
      end;
      if Assigned(secsel) then
      begin
       freportstructure.SelectDataItem(secsel);
       if Assigned(fobjinsp.CompItem) then
        if (fobjinsp.CompItem is TRpSectionInterface) then
        begin
         secint:=TRpSectionInterface(fobjinsp.CompItem);
         for j:=0 to secint.childlist.Count-1 do
         begin
          secintitem:=TRpSizePosInterface(secint.Childlist.Items[j]);
          if secintitem.printitem=printitem then
          begin
           secintitem.DoSelect;
           fobjinsp.SelectProperty(TRpReportException(E).PropertyName);
           break;
          end;
         end;
        end;
      end;
     end;
    end;
   end;
  end;

 end;
 if assigned(oldonexception) then
 begin
  oldonexception(Sender,E);
 end
 else
  RpMessageBox(E.Message,SRpError,[smbok]);
end;


procedure TFRpMainFVCL.FormDestroy(Sender: TObject);
begin
 Application.OnException:=oldonexception;
 Application.OnHint:=oldonhint;
end;

procedure TFRpMainFVCL.ASelectAllExecute(Sender: TObject);
begin
 // Selects all objects of the report
 fobjinsp.SelectAllClass('TRpSizePosInterface');
end;


procedure TFRpMainFVCL.ASelectAllTextExecute(Sender: TObject);
begin
 // Selects all objects of the report
 fobjinsp.SelectAllClass('TRpGenTextInterface');
end;

procedure TFRpMainFVCL.ALeftExecute(Sender: TObject);
begin
 fobjinsp.MoveSelected(1,false);
end;

procedure TFRpMainFVCL.ARightExecute(Sender: TObject);
begin
 fobjinsp.MoveSelected(2,false);
end;

procedure TFRpMainFVCL.AUpExecute(Sender: TObject);
begin
 fobjinsp.MoveSelected(3,false);
end;

procedure TFRpMainFVCL.ADownExecute(Sender: TObject);
begin
 fobjinsp.MoveSelected(4,false);
end;

procedure TFRpMainFVCL.AAlignLeftExecute(Sender: TObject);
begin
 fobjinsp.AlignSelected(1);
end;

procedure TFRpMainFVCL.AAlignRightExecute(Sender: TObject);
begin
 fobjinsp.AlignSelected(2);
end;

procedure TFRpMainFVCL.AAlignUpExecute(Sender: TObject);
begin
 fobjinsp.AlignSelected(3);
end;

procedure TFRpMainFVCL.AAlignDownExecute(Sender: TObject);
begin
 fobjinsp.AlignSelected(4);
end;

procedure TFRpMainFVCL.AAlignHorzExecute(Sender: TObject);
begin
 fobjinsp.AlignSelected(5);
end;

procedure TFRpMainFVCL.AAlignVertExecute(Sender: TObject);
begin
 fobjinsp.AlignSelected(6);
end;

procedure TFRpMainFVCL.AppHint(Sender:TObject);
begin
 BStatus.Panels.Items[0].Text:=Application.Hint;
end;

procedure TFRpMainFVCL.AStatusBarExecute(Sender: TObject);
begin
 AStatusBar.Checked:=Not AStatusBar.Checked;
 BStatus.Visible:=ASTatusBar.Checked;
end;

initialization



end.