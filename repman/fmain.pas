{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       fmain                                           }
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

unit fmain;

interface

uses
  SysUtils,
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls, QActnList, QImgList, QMenus, QTypes,rpreport,
  rpconsts,rptypes, QExtCtrls,frpstruc, rplastsav,rpsubreport,
  rpobinsint,rpfparams,fdesign,rpobjinsp,fsectionint,IniFiles,
  rpsection,rpprintitem,QClipbrd,QPrinters,rpqtdriver, IBDatabase,
  DB,fhelpform,rpmunits;
const
  // File name in menu width
  C_FILENAME_WIDTH=40;
type
  TFMainf = class(TForm)
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
    About1: TMenuItem;
    ReportManager1: TMenuItem;
    MFields: TPopupMenu;
    est1: TMenuItem;
    ext21: TMenuItem;
    ATutorial: TAction;
    AFeatures: TAction;
    Features1: TMenuItem;
    utorial1: TMenuItem;
    APrintSetup: TAction;
    Printersetup1: TMenuItem;
    AUnitCms: TAction;
    AUnitsinchess: TAction;
    Measurement1: TMenuItem;
    Measurement2: TMenuItem;
    Cms1: TMenuItem;
    Inchess1: TMenuItem;
    AUserParams: TAction;
    Userparameters1: TMenuItem;
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
    procedure ATutorialExecute(Sender: TObject);
    procedure AFeaturesExecute(Sender: TObject);
    procedure APrintSetupExecute(Sender: TObject);
    procedure AUnitCmsExecute(Sender: TObject);
    procedure AUnitsinchessExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure AUserParamsExecute(Sender: TObject);
  private
    { Private declarations }
    fdesignframe:TFDesignFrame;
    fhelp:TFHelpf;
    fobjinsp:TFObjInsp;
    lastsaved:TMemoryStream;
    configfile:string;
    updatedmfields:boolean;
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
    procedure AppIdleRefreshInterface(Sender:TObject;var done:boolean);
    procedure RefreshInterface(Sender: TObject);
  public
    { Public declarations }
    report:TRpReport;
    filename:string;
    freportstructure:TFRpStructure;
    function GetExpressionText:string;
  end;

var
  FMainf: TFMainf;

implementation

uses rppagesetup, rpshfolder,  fdatainfo, frpgrid, rppreview, fabout,
  rpprintdia, rprfparams;

{$R *.xfm}

// Check if it is saved and return true if all is ok
function TFMainf.CheckSave:Boolean;
var
 res:TmodalResult;
begin
 Result:=true;
 if report=nil then
  exit;
 if report<>nil then
 begin
  if Not CheckModified then
   exit;
  res:=MessageDlg(SRpReportChanged,mtWarning,[mbYes,mbNo,mbCancel],0);
  if res=mrCancel then
   Raise EAbort.Create(SRpSaveAborted);
  if res=mrNo then
   exit;
  ASaveExecute(Self);
 end;
end;

procedure TFMainf.ANewExecute(Sender: TObject);
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

procedure TFMainF.DoEnable;
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

procedure TFMainF.DoDisable;
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

procedure TFMainf.AExitExecute(Sender: TObject);
begin
 Close;
end;

procedure TFMainf.DoOpen(newfilename:string;showopendialog:boolean);
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

procedure TFMainf.AOpenExecute(Sender: TObject);
begin
 DoOpen(filename,true);
 FormResize(Self);
end;

procedure TFMainf.ASaveExecute(Sender: TObject);
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


procedure TFMainF.FreeInterface;
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
 BChart.Enabled:=false;
 BArrow.Down:=false;
 BLabel.Down:=false;
 BExpression.Down:=false;
 BShape.Down:=false;
 BImage.Down:=false;
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

procedure TFMainF.CreateInterface;
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
 APaste.Enabled:=true;
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
 BChart.Enabled:=true;
 BArrow.Down:=true;

 AParams.Enabled:=True;
 if length(filename)<1 then
  Caption:=SRpRepman+'-'+SRpUntitled
 else
  Caption:=SRpRepman+'-'+filename;
 // Create the report structure frame
 fobjinsp:=TFObjInsp.Create(Self);
 fobjinsp.Parent:=leftpanel;
 freportstructure:=TFRpStructure.Create(Self);
 freportstructure.Align:=alTop;
 freportstructure.Parent:=leftPanel;
 fdesignframe:=TFDesignFrame.Create(Self);
 fobjinsp.DesignFrame:=fdesignframe;
 fdesignframe.Parent:=MainScrollBox;
 fdesignframe.freportstructure:=freportstructure;
 freportstructure.designframe:=fdesignframe;

 fdesignframe.objinsp:=fobjinsp;
 freportstructure.objinsp:=fobjinsp;
 freportstructure.report:=report;
 fdesignframe.report:=report;

 mainscrollbox.Visible:=true;
end;

procedure TFMainf.ASaveasExecute(Sender: TObject);
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

procedure TFMainF.DoSave;
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

procedure TFMainf.APageSetupExecute(Sender: TObject);
begin
 Assert(report<>nil,'Called Page setup without a report assigned');

 ExecutePageSetup(report);
end;


// A report is known is modified by comparing the saving of
// the current report with the last saved report (lastsaved)
function TFMainf.checkmodified:boolean;
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

procedure TFMainf.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 try
  canclose:=CheckSave;
 except
  canclose:=false;
 end;
end;

procedure TFMainf.FormCreate(Sender: TObject);
begin
 Application.Title:=SRpRepman;
 configfile:=Obtainininameuserconfig('','','repmand');
{$IFDEF MSWINDOWS}
  LastUsedFiles.CaseSensitive:=False;
{$ENDIF}
{$IFDEF LINUX}
  LastUsedFiles.CaseSensitive:=True;
{$ENDIF}
 LastUsedFiles.LoadFromConfigFile(configfile);
 UpdateFileMenu;
 LoadConfig;
end;

procedure TFMainf.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 LastUsedFiles.SaveToConfigFile(configfile);
 SaveConfig;
end;

procedure TFMainf.UpdateFileMenu;
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
  aItem:=TMenuItem.Create(FMainf);
  File1.Add(aitem);
  aItem.Caption:='-';
  for i:=0 to alist.count-1 do
  begin
   aItem:=TMenuItem.Create(FMainf);
   File1.Add(aitem);
   aItem.Caption:=alist.strings[i];
   aItem.Tag:=i;
   aItem.OnClick:=OnFileClick;
  end;
 finally
  alist.free;
 end;
end;

procedure TFMainf.OnFileClick(Sender:TObject);
var
 newfilename:string;
begin
 newfilename:=LastusedFiles.LastUsed.Strings[TComponent(Sender).tag];
 // Try to open the file
 DoOpen(newfilename,false);
end;

procedure TFMainf.AppIdleRefreshInterface(Sender:TObject;var done:boolean);
begin
 Application.OnIdle:=nil;
 done:=false;
 FreeInterface;
 CreateInterface;
 FormResize(self);
end;

procedure TFMainf.RefreshInterface(Sender: TObject);
begin
 Application.OnIdle:=AppIdleRefreshInterface;
end;

procedure TFMainf.ANewPageHeaderExecute(Sender: TObject);
begin
 // Inserts a new page header
 Assert(report<>nil,'Called AddNew PageHeader without a report assigned');

 freportstructure.FindSelectedSubreport.AddPageHeader;

 RefreshInterface(Self);
end;

procedure TFMainf.ANewPageFooterExecute(Sender: TObject);
begin
 // Inserts a new page footer
 Assert(report<>nil,'Called AddNewPageFooter without a report assigned');

 freportstructure.FindSelectedSubreport.AddPageFooter;

 RefreshInterface(Self);
end;

procedure TFMainf.ANewGroupExecute(Sender: TObject);
var
 newgroupname:string;
begin
 // Inserts a new group header and footer
 Assert(report<>nil,'Called AddNewGroupout a report unassigned');

 newgroupname:=Uppercase(Trim(InputBox(SRpNewGroup,SRpSGroupName,'')));
 if length(newgroupname)>0 then
 begin
  freportstructure.FindSelectedSubreport.AddGroup(newgroupname);

  RefreshInterface(Self);
 end;
end;

procedure TFMainf.ANewSubreportExecute(Sender: TObject);
begin
 // Inserts a new group header and footer
 Assert(report<>nil,'Called AddSubReport a report unassigned');

 report.AddSubReport;

 RefreshInterface(Self);
end;

procedure TFMainf.ADeleteSelectionExecute(Sender: TObject);
begin
 // Deletes section
 Assert(report<>nil,'Called ADeleteSection a report unassigned');

 freportstructure.DeleteSelectedNode;

 RefreshInterface(Self);
end;

procedure TFMainf.ANewDetailExecute(Sender: TObject);
begin
 // Inserts a new group header and footer
 Assert(report<>nil,'Called ADeleteSection a report unassigned');

 freportstructure.FindSelectedSubreport.AddDetail;

 RefreshInterface(Self);
end;

procedure TFMainf.ADataConfigExecute(Sender: TObject);
begin
 // Data info configuration dialog
 ShowDataConfig(report);
 fdesignframe.UpdateSelection(true);
 updatedmfields:=false;
end;

procedure TFMainf.AParamsExecute(Sender: TObject);
begin
 ShowParamDef(report.Params,report.DataInfo);
end;

procedure TFMainf.AGridOptionsExecute(Sender: TObject);
begin
 ModifyGridProperties(report);
 fdesignframe.UpdateSelection(true);
end;

procedure TFMainf.ACutExecute(Sender: TObject);
var
 sectionintf:TRpSectionInterface;
 pitem:TRpCommonComponent;
begin
 // Delete current selection
 if Not Assigned(fobjinsp.CompItem) then
  exit;
 pitem:=TRpSizePosInterface(fobjinsp.CompItem).printitem;
 sectionintf:=TrpSectionInterface(TRpSizePosInterface(fobjinsp.CompItem).sectionint);
 Clipboard.SetComponent(pitem);
 sectionintf.DoDeleteComponent(pitem);
 fobjinsp.CompItem:=nil;
 fdesignframe.UpdateSelection(true);
end;

procedure TFMainf.ACopyExecute(Sender: TObject);
var
 pitem:TRpCommonComponent;
begin
 // Delete current selection
 if Not Assigned(fobjinsp.CompItem) then
  exit;
 pitem:=TRpSizePosInterface(fobjinsp.CompItem).printitem;
 Clipboard.SetComponent(pitem);
end;

procedure TFMainf.APasteExecute(Sender: TObject);
var
 section:TRpSection;
 secint:TRpSectionInterface;
 compo:TComponent;
begin
// 'application/delphi.component'

 // Delete current selection
 if Not Assigned(fobjinsp.CompItem) then
  exit;
 if (fobjinsp.CompItem is TRpSectionInterface) then
 begin
  secint:=TRpSectionInterface(fobjinsp.CompItem);
 end
 else
 begin
  secint:=TRpSectionInterface(TRpSizePosInterface(fobjinsp.CompItem).SectionInt);
 end;
 section:=TrpSection(secint.printitem);
 compo:=Clipboard.GetComponent(report,report);
 if compo is TRpCommonPosComponent then
 begin
  compo.Name:='';
  Generatenewname(compo);
  (section.Components.Add).Component:=TRpCommonPosComponent(compo);
  fobjinsp.CompItem:=nil;
  fdesignframe.UpdateSelection(true);
 end
 else
 begin
  Raise Exception.Create(SRpInvalidClipboardFormat);
 end;
end;

procedure TFMainf.Splitter1Moved(Sender: TObject);
var
 olditem:TRpSizeInterface;
begin
 // Assigns then objinsp
 olditem:=fobjinsp.CompItem;
 fobjinsp.CompItem:=nil;
 fobjinsp.CompItem:=olditem;
 // Correct scrollboxes
 CorrectScrollBoxes;

end;

procedure TFMainf.OnReadError(Reader: TReader; const Message: string;
    var Handled: Boolean);
begin
 Handled:=MessageDlg(SRpErrorReadingReport,Message+#10+SRpIgnoreError,mtWarning,[mbYes,mbNo],0)=mrYes;
end;

procedure TFMainf.APreviewExecute(Sender: TObject);
begin
 // Previews the report
 ShowPreview(report,caption);
end;

procedure TFMainf.AAboutExecute(Sender: TObject);
begin
 ShowAbout;
end;

procedure TFMainf.APrintExecute(Sender: TObject);
var
 allpages,collate:boolean;
 frompage,topage,copies:integer;
begin
 allpages:=true;
 collate:=report.CollateCopies;
 frompage:=1; topage:=999999;
 copies:=report.Copies;
 if DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
  PrintReport(report,Caption,true,allpages,frompage,topage,copies,collate);
end;


procedure TFMainf.MFieldsItemClick(Sender:TObject);
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

procedure TFMainf.UpdateMFields;
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

procedure TFMainf.BExpressionMouseUp(Sender: TObject; Button: TMouseButton;
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


function TFMainf.GetExpressionText:string;
var
 i:integer;
begin
 Result:=SRpSampleExpression;
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

procedure TFMainf.ShowHelp(AURL:string);
begin
 if Not Assigned(FHelp) then
  FHelp:=TFHelpf.Create(Application);
 FHelp.TextBrowser1.FileName:=AURL;
 FHelp.Show;
end;

procedure TFMainf.ATutorialExecute(Sender: TObject);
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
 aurl:=aurl+'doc'+Directorysep+'tutorial'+
  Directorysep+'left.html';
 ShowHelp(aurl);
end;

procedure TFMainf.AFeaturesExecute(Sender: TObject);
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
 aurl:=aurl+'doc'+Directorysep+'features.html';
 ShowHelp(aurl);
end;

procedure TFMainf.APrintSetupExecute(Sender: TObject);
begin
 printer.ExecuteSetup;
end;

procedure TFMainf.LoadConfig;
var
 inif:TInifile;
begin
 inif:=TIniFile.Create(configfile);
 try
  AUnitCms.Checked:=inif.ReadBool('Preferences','UnitCms',true);
  AUnitsinchess.Checked:=Not AUnitCms.Checked;
  UpdateUnits;
 finally
  inif.free;
 end;
end;

procedure TFMainf.SaveConfig;
var
 inif:TInifile;
begin
 inif:=TIniFile.Create(configfile);
 try
  inif.WriteBool('Preferences','UnitCms',AUnitCms.Checked);
  inif.UpdateFile;
 finally
  inif.free;
 end;
end;


procedure TFMainf.UpdateUnits;
begin
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


procedure TFMainf.AUnitCmsExecute(Sender: TObject);
begin
 AUnitCms.Checked:=true;
 AUnitsInchess.Checked:=false;
 UpdateUnits;
end;

procedure TFMainf.AUnitsinchessExecute(Sender: TObject);
begin
 AUnitCms.Checked:=false;
 AUnitsInchess.Checked:=true;
 UpdateUnits;
end;

procedure TFMainf.CorrectScrollBoxes;
begin
 if assigned(fdesignframe) then
 begin
  // A bug in aligments CLX Windows and Linux
  // forced me to include this corrections
   fdesignframe.HorzScrollBar.Position:=0;
   fdesignframe.VertScrollBar.Position:=0;
   HorzScrollBar.Position:=0;
   VertScrollBar.Position:=0;
   MainScrollBox.HorzScrollBar.Position:=0;
   MainScrollBox.VertScrollBar.Position:=0;
   LeftPanel.Left:=0;
   Splitter1.Left:=20;
   ToolBar1.Left:=0;
 end;
end;


procedure TFMainf.FormResize(Sender: TObject);
begin
 if assigned(fdesignframe) then
 begin
  fdesignframe.UpdateInterface;
  CorrectScrollBoxes;
 end;
end;

procedure TFMainf.AUserParamsExecute(Sender: TObject);
begin
 ShowUserParams(report);
end;

initialization



end.
