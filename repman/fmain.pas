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
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}

unit fmain;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls, QActnList, QImgList, QMenus, QTypes,rpreport,
  rpconsts,rptypes, QExtCtrls,frpstruc, rplastsav,rpsubreport,rpfparams;

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
    ToolButton4: TToolButton;
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
    ANewReportHeader: TAction;
    MAdd: TMenuItem;
    Reportheader1: TMenuItem;
    ANewPageFooter: TAction;
    ANewReportFooter: TAction;
    ANewGroup: TAction;
    Pagefooter1: TMenuItem;
    Reportfooter1: TMenuItem;
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
    procedure ANewReportHeaderExecute(Sender: TObject);
    procedure ANewPageFooterExecute(Sender: TObject);
    procedure ANewReportFooterExecute(Sender: TObject);
    procedure ANewGroupExecute(Sender: TObject);
    procedure ANewSubreportExecute(Sender: TObject);
    procedure ADeleteSelectionExecute(Sender: TObject);
    procedure ANewDetailExecute(Sender: TObject);
    procedure ADataConfigExecute(Sender: TObject);
    procedure AParamsExecute(Sender: TObject);
  private
    { Private declarations }
    report:TRpReport;
    lastsaved:TMemoryStream;
    configfile:string;
    freportstructure:TFRpStructure;
    function checkmodified:boolean;
    procedure FreeInterface;
    procedure CreateInterface;
    procedure DoSave;
    procedure DoEnable;
    procedure DoDisable;
    function CheckSave:Boolean;
    procedure UpdateFileMenu;
    procedure OnFileClick(Sender:TObject);
    procedure DoOpen(newfilename:string;showopendialog:boolean);
  public
    { Public declarations }
    filename:string;
  end;

var
  FMainf: TFMainf;

implementation

uses rppagesetup, rpshfolder, freportgroup, fdatainfo;

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
 report.CreateNew;
 filename:='';

 DoEnable;
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
 report.free;
 report:=nil;
 DoDisable;
 // Creates a new report
 report:=TRpReport.Create(Self);
 try
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
 ANewReportHeader.Enabled:=false;
 ANewPageFooter.Enabled:=false;
 ANewReportFooter.Enabled:=false;
 ANewGroup.Enabled:=false;
 ANewSubReport.Enabled:=false;
 ADeleteSelection.Enabled:=false;
 AnewDetail.Enabled:=false;
 ADataConfig.Enabled:=false;
 AParams.Enabled:=False;
 APageSetup.Enabled:=false;
 filename:='';
 Caption:=SRpRepman;

 freportstructure.free;
 freportstructure:=nil;
 mainscrollbox.Visible:=false;
end;

procedure TFMainF.CreateInterface;
begin
 // Creates an interface for the report
 ASave.Enabled:=true;
 ASaveas.Enabled:=True;
 APageSetup.Enabled:=True;
 MReport.Visible:=True;
 ANewPageHeader.Enabled:=True;
 ANewReportHeader.Enabled:=True;
 ANewPageFooter.Enabled:=True;
 ANewReportFooter.Enabled:=True;
 ANewGroup.Enabled:=True;
 ANewSubReport.Enabled:=True;
 ADeleteSelection.Enabled:=true;
 AnewDetail.Enabled:=true;
 ADataConfig.Enabled:=true;
 AParams.Enabled:=True;
 if length(filename)<1 then
  Caption:=SRpRepman+'-'+SRpUntitled
 else
  Caption:=SRpRepman+'-'+filename;
 // Create the report structure frame
 freportstructure:=TFRpStructure.Create(Self);
 freportstructure.Align:=alTop;
 freportstructure.Report:=report;
 freportstructure.RView.Selected:=freportstructure.RView.Items.Item[0];
 freportstructure.RView.FullExpand;
 freportstructure.Parent:=leftPanel;
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
 Assert(report<>nil,'Called Pagesetup without a report assigned');

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
 configfile:=Obtainininameuserconfig('','','repmand');
{$IFDEF MSWINDOWS}
  LastUsedFiles.CaseSensitive:=False;
{$ENDIF}
{$IFDEF LINUX}
  LastUsedFiles.CaseSensitive:=True;
{$ENDIF}
 LastUsedFiles.LoadFromConfigFile(configfile);
 UpdateFileMenu;
end;

procedure TFMainf.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 LastUsedFiles.SaveToConfigFile(configfile);
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

procedure TFMainf.ANewPageHeaderExecute(Sender: TObject);
begin
 // Inserts a new page header
 Assert(report<>nil,'Called AddNew PageHeader without a report assigned');

 freportstructure.FindSelectedSubreport.AddPageHeader;
 FreeInterface;
 CreateInterface;
end;

procedure TFMainf.ANewReportHeaderExecute(Sender: TObject);
begin
 // Inserts a new page report header
 Assert(report<>nil,'Called AddNewReportHeader without a report assigned');

 freportstructure.FindSelectedSubreport.AddReportHeader;
 FreeInterface;
 CreateInterface;
end;

procedure TFMainf.ANewPageFooterExecute(Sender: TObject);
begin
 // Inserts a new page footer
 Assert(report<>nil,'Called AddNewPageFooter without a report assigned');

 freportstructure.FindSelectedSubreport.AddPageFooter;
 FreeInterface;
 CreateInterface;
end;

procedure TFMainf.ANewReportFooterExecute(Sender: TObject);
begin
 // Inserts a new report footer
 Assert(report<>nil,'Called AddNewReportFooter without a report assigned');

 freportstructure.FindSelectedSubreport.AddReportFooter;
 FreeInterface;
 CreateInterface;
end;

procedure TFMainf.ANewGroupExecute(Sender: TObject);
begin
 // Inserts a new group header and footer
 Assert(report<>nil,'Called AddNewGroupout a report unassigned');

 if AddGroup(freportstructure.FindSelectedSubreport) then
 begin
  FreeInterface;
  CreateInterface;
 end;
end;

procedure TFMainf.ANewSubreportExecute(Sender: TObject);
begin
 // Inserts a new group header and footer
 Assert(report<>nil,'Called AddSubReport a report unassigned');

 report.AddSubReport;

 FreeInterface;
 CreateInterface;
end;

procedure TFMainf.ADeleteSelectionExecute(Sender: TObject);
begin
 // Deletes section
 Assert(report<>nil,'Called ADeleteSection a report unassigned');

 freportstructure.DeleteSelectedNode;

 FreeInterface;
 CreateInterface;
end;

procedure TFMainf.ANewDetailExecute(Sender: TObject);
begin
 // Inserts a new group header and footer
 Assert(report<>nil,'Called ADeleteSection a report unassigned');

 freportstructure.FindSelectedSubreport.AddDetail;

 FreeInterface;
 CreateInterface;
end;

procedure TFMainf.ADataConfigExecute(Sender: TObject);
begin
 // Data info configuration dialog
 ShowDataConfig(report);
end;

procedure TFMainf.AParamsExecute(Sender: TObject);
begin
 ShowParamDef(report.Params,report.DataInfo);
end;

initialization

end.
