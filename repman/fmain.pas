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
  rpobinsint,rpfparams,fdesign,rpobjinsp,fsectionint,
{$IFNDEF PROFILE}  rpsection,rpprintitem,QClipbrd,QPrinters,rpqtdriver;{$ENDIF}
{$IFDEF PROFILE}  rpsection,rpprintitem,QClipbrd,QPrinters,rpqtdriver ,Proftimx;{$ENDIF}
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
    procedure APrintExecute(Sender: TObject);
    procedure AAboutExecute(Sender: TObject);
  private
    { Private declarations }
    fdesignframe:TFDesignFrame;
    fobjinsp:TFObjInsp;
    lastsaved:TMemoryStream;
    configfile:string;
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
  public
    { Public declarations }
    report:TRpReport;
    filename:string;
    freportstructure:TFRpStructure;
  end;

var
  FMainf: TFMainf;

implementation

uses rppagesetup, rpshfolder,  fdatainfo, frpgrid, rppreview, fabout;

{$R *.xfm}

// Check if it is saved and return true if all is ok
function TFMainf.CheckSave:Boolean;
var
 res:TmodalResult;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,64; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,64; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.ANewExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,65; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 if Not checksave then
  exit;
 DoDisable;
 // Creates a new report
 report:=TRpReport.Create(Self);
 report.OnReadError:=OnReadError;
 report.CreateNew;
 filename:='';

 DoEnable;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,65; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainF.DoEnable;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,66; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Save the report for seeing after if it's modified
 if Assigned(lastsaved) then
 begin
  lastsaved.free;
  lastsaved:=nil;
 end;
 lastsaved:=TMemorystream.create;
 report.SaveToStream(lastsaved);

 CreateInterface;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,66; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainF.DoDisable;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,67; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 FreeInterface;
 report.free;
 report:=nil;
 // Save the report for seeing after if it's modified
 if Assigned(lastsaved) then
 begin
  lastsaved.free;
  lastsaved:=nil;
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,67; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.AExitExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,68; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 Close;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,68; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.DoOpen(newfilename:string;showopendialog:boolean);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,69; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,69; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.AOpenExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,70; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 DoOpen(filename,true);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,70; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.ASaveExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,71; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,71; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


procedure TFMainF.FreeInterface;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,72; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,72; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainF.CreateInterface;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,73; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Creates an interface for the report
 ASave.Enabled:=true;
 ASaveas.Enabled:=True;
 APageSetup.Enabled:=True;
 MReport.Visible:=True;
 ANewPageHeader.Enabled:=True;
 ANewPageFooter.Enabled:=True;
// ANewGroup.Enabled:=true;
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,73; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.ASaveasExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,74; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 Assert(report<>nil,'Called Save without a report assigned');
 // Saves the report
 if SaveDialog1.Execute then
 begin
  DoSave;
 end
 else
  Raise EAbort.Create(SRpSaveAborted);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,74; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainF.DoSave;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,75; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,75; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.APageSetupExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,76; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 Assert(report<>nil,'Called Pagesetup without a report assigned');

 ExecutePageSetup(report);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,76; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


// A report is known is modified by comparing the saving of
// the current report with the last saved report (lastsaved)
function TFMainf.checkmodified:boolean;
var
 newsave:TMemoryStream;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,77; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,77; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,78; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 try
  canclose:=CheckSave;
 except
  canclose:=false;
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,78; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.FormCreate(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,79; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 configfile:=Obtainininameuserconfig('','','repmand');
{$IFDEF MSWINDOWS}
  LastUsedFiles.CaseSensitive:=False;
{$ENDIF}
{$IFDEF LINUX}
  LastUsedFiles.CaseSensitive:=True;
{$ENDIF}
 LastUsedFiles.LoadFromConfigFile(configfile);
 UpdateFileMenu;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,79; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.FormClose(Sender: TObject; var Action: TCloseAction);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,80; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 LastUsedFiles.SaveToConfigFile(configfile);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,80; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.UpdateFileMenu;
var
 exitindex:integer;
 alist:TStringlist;
 aitem:TmenuItem;
 i:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,81; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,81; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.OnFileClick(Sender:TObject);
var
 newfilename:string;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,82; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 newfilename:=LastusedFiles.LastUsed.Strings[TComponent(Sender).tag];
 // Try to open the file
 DoOpen(newfilename,false);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,82; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.ANewPageHeaderExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,83; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Inserts a new page header
 Assert(report<>nil,'Called AddNew PageHeader without a report assigned');

 freportstructure.FindSelectedSubreport.AddPageHeader;
 FreeInterface;
 CreateInterface;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,83; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.ANewPageFooterExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,84; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Inserts a new page footer
 Assert(report<>nil,'Called AddNewPageFooter without a report assigned');

 freportstructure.FindSelectedSubreport.AddPageFooter;
 FreeInterface;
 CreateInterface;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,84; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.ANewGroupExecute(Sender: TObject);
var
 newgroupname:string;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,85; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Inserts a new group header and footer
 Assert(report<>nil,'Called AddNewGroupout a report unassigned');

 newgroupname:=Uppercase(Trim(InputBox(SRpNewGroup,SRpSGroupName,'')));
 if length(newgroupname)>0 then
 begin
  freportstructure.FindSelectedSubreport.AddGroup(newgroupname);
  FreeInterface;
  CreateInterface;
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,85; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.ANewSubreportExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,86; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Inserts a new group header and footer
 Assert(report<>nil,'Called AddSubReport a report unassigned');

 report.AddSubReport;

 FreeInterface;
 CreateInterface;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,86; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.ADeleteSelectionExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,87; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Deletes section
 Assert(report<>nil,'Called ADeleteSection a report unassigned');

 freportstructure.DeleteSelectedNode;

 FreeInterface;
 CreateInterface;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,87; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.ANewDetailExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,88; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Inserts a new group header and footer
 Assert(report<>nil,'Called ADeleteSection a report unassigned');

 freportstructure.FindSelectedSubreport.AddDetail;

 FreeInterface;
 CreateInterface;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,88; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.ADataConfigExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,89; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Data info configuration dialog
 ShowDataConfig(report);
 fdesignframe.UpdateSelection(true);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,89; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.AParamsExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,90; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 ShowParamDef(report.Params,report.DataInfo);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,90; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.AGridOptionsExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,91; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 ModifyGridProperties(report);
 fdesignframe.UpdateSelection(true);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,91; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.ACutExecute(Sender: TObject);
var
 section:TRpSection;
 pitem:TRpCommonComponent;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,92; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Delete current selection
 if Not Assigned(fobjinsp.CompItem) then
  exit;
 pitem:=TRpSizePosInterface(fobjinsp.CompItem).printitem;
 section:=TRpSection(TRpSizePosInterface(fobjinsp.CompItem).SectionInt.printitem);
 Clipboard.SetComponent(pitem);
 section.DeleteComponent(pitem);
 fobjinsp.CompItem:=nil;
 fdesignframe.UpdateSelection(true);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,92; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.ACopyExecute(Sender: TObject);
var
 pitem:TRpCommonComponent;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,93; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Delete current selection
 if Not Assigned(fobjinsp.CompItem) then
  exit;
 pitem:=TRpSizePosInterface(fobjinsp.CompItem).printitem;
 Clipboard.SetComponent(pitem);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,93; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.APasteExecute(Sender: TObject);
var
 section:TRpSection;
 secint:TRpSectionInterface;
 compo:TComponent;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,94; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,94; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.Splitter1Moved(Sender: TObject);
var
 olditem:TRpSizeInterface;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,95; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Assigns then objinsp
 olditem:=fobjinsp.CompItem;
 fobjinsp.CompItem:=nil;
 fobjinsp.CompItem:=olditem;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,95; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.OnReadError(Reader: TReader; const Message: string;
    var Handled: Boolean);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,96; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 Handled:=MessageDlg(SRpErrorReadingReport,Message+#10+SRpIgnoreError,mtWarning,[mbYes,mbNo],0)=mrYes;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,96; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.APreviewExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,97; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // Previews the report
 ShowPreview(report,caption);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,97; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.APrintExecute(Sender: TObject);
{$IFDEF LINUX}
var
 theparams:array [0..3] of pchar;
 param1:string;
 param2:string;
 child:__pid_t;
{$ENDIF}
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,98; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 // A bug in Kylix 2 does not allow printing
 // when using dbexpress
{$IFDEF MSWINDOWS}
 if CalcReportWidthProgress(report) then
  PrintMetafile(report.Metafile,Caption,true);
{$ENDIF}
{$IFDEF LINUX}
 if CalcReportWidthProgress(report) then
 begin
  // Saves the metafile
  report.Metafile.SaveToFile('meta.rpmf');
  param1:='metaprint';
  param2:='meta.rpmf';
  theparams[0]:=Pchar(param1);
  theparams[1]:=Pchar(param2);
  theparams[2]:=nil;

  child:=fork;
  if child=-1 then
   Raise Exception.Create(SRpErrorFork);
  if child<>0 then
  begin
   execvp(theparams[0],PPChar(@theparams))
  end
 end;
{$ENDIF}
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,98; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFMainf.AAboutExecute(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,99; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 ShowAbout;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,99; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

initialization



end.
