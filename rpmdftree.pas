{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpmdftree                                       }
{                                                       }
{       Build a report tree from a dataset              }
{       or a directory                                  }
{                                                       }
{       You can assign events to OnDelete, OnNew        }
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

unit rpmdftree;

interface

{$I rpconf.inc}

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs,rpmdconsts, QActnList, QImgList, QComCtrls,rpgraphutils, DB,
  DBClient, QStdCtrls,QPrinters,
{$IFDEF LINUX}
  Libc,
{$ENDIF}
{$IFDEF MSWINDOWS}
  mmsystem,windows,
{$ENDIF}
  rpclxreport;

const PROGRESS_INTERVAL=500;

type
  TRpOnLoadReport=procedure (reportname:string;memstream:TMemoryStream) of object;

  TFRpDBTree = class(TFrame)
    BToolBar: TToolBar;
    imalist: TImageList;
    ActionList1: TActionList;
    ANew: TAction;
    ADelete: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ATree: TTreeView;
    DReports: TClientDataSet;
    DReportGroups: TClientDataSet;
    BCancel: TButton;
    ToolButton3: TToolButton;
    DReportGroupsGROUP_CODE: TIntegerField;
    DReportGroupsGROUP_NAME: TStringField;
    DReportGroupsPARENT_GROUP: TIntegerField;
    DReportGroupsTREE_NODE: TIntegerField;
    DReportGroups2: TClientDataSet;
    DReportsREPORT_NAME: TStringField;
    DReportsREPORT_GROUP: TIntegerField;
    DReportGroups2GROUP_CODE: TIntegerField;
    DReportGroups2GROUP_NAME: TStringField;
    DReportGroups2PARENT_GROUP: TIntegerField;
    DReportGroups2TREE_NODE: TIntegerField;
    APreview: TAction;
    APrint: TAction;
    AUserParams: TAction;
    APrintSetup: TAction;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    SaveDialog1: TSaveDialog;
    procedure ADeleteExecute(Sender: TObject);
    procedure APreviewExecute(Sender: TObject);
    procedure ATreeItemClick(Sender: TObject; Button: TMouseButton;
      Node: TTreeNode; const Pt: TPoint);
    procedure AUserParamsExecute(Sender: TObject);
    procedure APrintExecute(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure APrintSetupExecute(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
  private
    { Private declarations }
    docancel:boolean;
    lobjects:TList;
    report:TCLXReport;
    CurrentLoaded:String;
    FOnLoadReport:TRpOnLoadReport;
    counter:Integer;
{$IFDEF MSWINDOWS}
    mmfirst,mmlast:DWORD;
{$ENDIF}
{$IFDEF LINUX}
    milifirst,mililast:TDatetime;
{$ENDIF}
    difmilis:int64;
    procedure SaveDir(adir:String;anode:TTreeNode);
    procedure CheckCancel(acount:integer);
    procedure GenerateTree;
    procedure FillTreeForCurrentRecord(ANode:TTreeNode);
    procedure CheckLoaded;
    procedure DisableButtonsReport;
    procedure EnableButtonsReport;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure FillTree(groups:TDataset;reports:TDataset);
    property OnLoadReport:TRpOnLoadReport read FOnLoadReport
     write FOnLoadReport;
  end;

  TRpNodeInfo=class(Tobject)
   private
    Node:TTreeNode;
    ReportName:String;
    Group_Code:Integer;
    Parent_Group:integer;
   end;

implementation

{$R *.xfm}



constructor TFRpDBTree.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 lobjects:=TList.Create;
 ANew.Caption:=TranslateStr(790,ANew.Caption);
 ANew.Hint:=TranslateStr(791,ANew.Hint);
 ADelete.Caption:=TranslateStr(792,ADelete.Caption);
 ADelete.Hint:=TranslateStr(793,ADelete.Hint);
 APrint.Caption:=TranslateStr(52,APrint.Caption);
 APrint.Hint:=TranslateStr(53,APrint.Hint);
 APreview.Caption:=TranslateStr(54,APreview.Caption);
 APreview.Hint:=TranslateStr(55,APreview.Hint);
 APrintSetup.Caption:=TranslateStr(56,APrintSetup.Caption);
 APrintSetup.Hint:=TranslateStr(57,APrintSetup.Hint);
 AUserParams.Caption:=TranslateStr(135,AUserparams.Caption);
 AUserParams.Hint:=TranslateStr(136,AUserparams.Hint);
end;

destructor TFRpDBTree.Destroy;
var
 i:integer;
begin
 for i:=0 to lobjects.count-1 do
 begin
  TObject(lobjects[i]).free;
 end;
 lobjects.clear;
 lobjects.free;

 ATree.Items.Clear;

 inherited Destroy;
end;

procedure TFRpDBTree.FillTree(groups:TDataset;reports:TDataset);
var
 acount:integer;
 i:integer;
begin
 // Get the time
{$IFDEF MSWINDOWS}
 mmfirst:=TimeGetTime;
{$ENDIF}
{$IFDEF LINUX}
 milifirst:=now;
{$ENDIF}
 // Transport the dataset to the clientdatasets
 DReports.Close;
 DReportGroups.Close;
 DReports.CreateDataSet;
 DReportGroups.CreateDataset;
 DReportGroups2.Close;
 DReportGroups2.FieldDefs.Assign(DReportGroups.FieldDefs);
 DReportGroups2.IndexDefs.Assign(DReportGroups.IndexDefs);
 DReportGroups2.CreateDataSet;
 docancel:=false;
 acount:=0;
 BCancel.Visible:=True;
 try
  While Not groups.Eof do
  begin
   DReportGroups.Append;
   try
    DReportGroupsGROUP_CODE.Value:=groups.FieldByName('GROUP_CODE').Value;
    DReportGroupsGROUP_NAME.AsVariant:=groups.FieldByName('GROUP_NAME').AsVariant;
    DReportGroupsPARENT_GROUP.AsVariant:=groups.FieldByName('PARENT_GROUP').AsVariant;
    DReportGroups2.Append;
    try
     for i:=0 to DReportGroups2.Fields.Count-1 do
     begin
      DReportGroups2.Fields[i].AsVariant:=DReportGroups.Fields[i].AsVariant;
     end;
     DReportGroups2.Post;
    except
     DReportGroups2.Cancel;
     Raise;
    end;
    DReportGroups.Post;
   except
    DReportGroups.Cancel;
    Raise;
   end;
   inc(acount);
   CheckCancel(acount);
   groups.Next;
  end;

  acount:=0;
  While Not reports.Eof do
  begin
   DReports.Append;
   try
    DReportsREPORT_NAME.Value:=reports.FieldByName('REPORT_NAME').Value;
    DReportsREPORT_GROUP.AsVariant:=reports.FieldByName('REPORT_GROUP').AsVariant;
    DReports.Post;
   except
    DReports.Cancel;
    Raise;
   end;
   inc(acount);
   CheckCancel(acount);
   reports.Next;
  end;

  // Generates the tree view
  GenerateTree;
 finally
  BCancel.Visible:=False;
 end;
end;

procedure TFRpDBTree.FillTreeForCurrentRecord(ANode:TTreeNode);
var
 amark:TBookMark;
 agroup:Integer;
 NewNode,ANewNode:TTreeNode;
 NodeInfo:TRpNodeInfo;
begin
 agroup:=DReportGroups2GROUP_CODE.Value;
 if Not DReportGroups2.FindKey([agroup]) then
  Exit;
 While (DReportGroups2PARENT_GROUP.Value=agroup) do
 begin
  NewNode:=ATree.Items.AddChild(ANode,DReportGroups2GROUP_NAME.AsString);
  NewNode.ImageIndex:=2;
  NodeInfo:=TRpNodeInfo.Create;
  lobjects.Add(NodeInfo);
  NodeInfo.ReportName:='';
  NodeInfo.Group_Code:=DReportGroups2GROUP_CODE.AsInteger;
  NodeInfo.Parent_Group:=DReportGroups2PARENT_GROUP.AsInteger;
  NodeInfo.Node:=NewNode;
  NewNode.Data:=NodeInfo;

  amark:=DReportGroups2.GetBookmark;
  try
   FillTreeForCurrentRecord(NewNode);
   DReportGroups2.GotoBookmark(amark);
  finally
   DReportGroups2.FreeBookmark(amark);
  end;
  // Add the reports for this group
  if DReports.FindKey([DReportGroups2GROUP_CODE.Value]) then
  begin
   While Not DReports.Eof do
   begin
    if DReportsREPORT_GROUP.IsNull then
     break;
    if DReportsREPORT_GROUP.Value<>DReportGroups2GROUP_CODE.Value then
      break;
    ANewNode:=ATree.Items.AddChild(NewNode,DReportsREPORT_NAME.AsString);
    ANewNode.ImageIndex:=3;
    NodeInfo:=TRpNodeInfo.Create;
    lobjects.Add(NodeInfo);
    NodeInfo.ReportName:=DReportsREPORT_NAME.AsString;
    NodeInfo.Group_Code:=DReportsREPORT_GROUP.AsInteger;
    NodeInfo.Parent_Group:=0;
    NodeInfo.Node:=ANewNode;
    ANewNode.Data:=NodeInfo;
    DReports.Next;
   end;
  end;
  DReportGroups2.Next;
  if DReportGroups2.Eof then
   break;
  CheckCancel(0);
 end;
end;

procedure TFRpDBTree.GenerateTree;
var
 ANode:TTreeNode;
 ATopItem:TTreeNode;
 NodeInfo:TRpNodeInfo;
begin
 ATree.Items.BeginUpdate;
 try
  ATopItem:=ATree.TopItem;
  DReportGroups.First;
  While Not DReportGroups.Eof do
  begin
   if DReportGroupsPARENT_GROUP.Value=0 then
   begin
    ANode:=ATree.Items.AddChild(ATopItem,DReportGroupsGROUP_NAME.AsString);
    ANode.ImageIndex:=2;
    NodeInfo:=TRpNodeInfo.Create;
    lobjects.Add(NodeInfo);
    NodeInfo.ReportName:='';
    NodeInfo.Group_Code:=DReportGroupsGROUP_CODE.AsInteger;
    NodeInfo.Parent_Group:=0;
    NodeInfo.Node:=ANode;
    ANode.Data:=NodeInfo;
    DReportGroups2.Locate('GROUP_CODE',DReportGroups.FieldByName('GROUP_CODE').Value,[]);
    FillTreeForCurrentRecord(ANode);
    // Add the reports for this group
    if DReports.FindKey([DReportGroupsGROUP_CODE.Value]) then
    begin
     While Not DReports.Eof do
     begin
      if DReportsREPORT_GROUP.IsNull then
       break;
      if DReportsREPORT_GROUP.Value<>DReportGroupsGROUP_CODE.Value then
       break;
      ANode:=ATree.Items.AddChild(ANode,DReportsREPORT_NAME.AsString);
      ANode.ImageIndex:=3;
      NodeInfo:=TRpNodeInfo.Create;
      lobjects.Add(NodeInfo);
      NodeInfo.ReportName:=DReportsREPORT_NAME.AsString;
      NodeInfo.Group_Code:=DReportsREPORT_GROUP.AsInteger;
      NodeInfo.Parent_Group:=0;
      NodeInfo.Node:=ANode;
      ANode.Data:=NodeInfo;
      DReports.Next;
      CheckCancel(0);
     end;
    end;
   end;
   DReportGroups.Next;
  end;
  // Insert te reports without any group
  DReports.First;
  While Not DReports.Eof do
  begin
   if Not DReportsREPORT_GROUP.IsNull then
    if DReportsREPORT_GROUP.Value>=0 then
     break;
   DReports.Next;
   CHeckCancel(0);
  end;
 finally
  ATree.Items.EndUpdate;
 end;
end;

procedure TFRpDBTree.CheckCancel(acount:integer);
begin
{$IFDEF MSWINDOWS}
 mmlast:=TimeGetTime;
 difmilis:=(mmlast-mmfirst);
{$ENDIF}
{$IFDEF LINUX}
 mililast:=now;
 difmilis:=MillisecondsBetween(mililast,milifirst);
{$ENDIF}
 if difmilis>PROGRESS_INTERVAL then
 begin
  // Get the time
{$IFDEF MSWINDOWS}
  mmfirst:=TimeGetTime;
{$ENDIF}
{$IFDEF LINUX}
  milifirst:=now;
{$ENDIF}
  docancel:=false;
  BCancel.Caption:=FormatFloat('###,####',acount)+'-'+SRpCancel;
  Application.ProcessMessages;
  if docancel then
   Raise Exception.Create(SRpOperationAborted);
 end;
end;

procedure TFRpDBTree.ADeleteExecute(Sender: TObject);
begin
 if smbYes<>RpMessageBox(SRpSureDeleteSection,SRpWarning,[smbYes,smbCancel],smsWarning,smbYes) then
  exit;
 // This deletes the selected report
end;

procedure TFRpDBTree.APreviewExecute(Sender: TObject);
begin
 CheckLoaded;
 report.Preview:=true;
 report.Execute;
end;

procedure TFRpDBTree.CheckLoaded;
var
 ANode:TTreeNode;
 NodeInfo:TRpNodeInfo;
 memstream:TMemoryStream;
begin
 if Not Assigned(Report) then
  report:=TCLXReport.Create(Self);
 // See if selected node is a report
 if Not Assigned(ATree.Selected) then
  Raise Exception.Create(SRptReportnotfound);
 ANode:=ATree.Selected;
 NodeInfo:=TRpNodeInfo(ANode.Data);
 if Length(NodeInfo.ReportName)<1 then
  Raise Exception.Create(SRptReportnotfound);
 if CurrentLoaded=NodeInfo.ReportName then
  exit;
 if Not Assigned(FOnLoadReport) then
  Raise Exception.Create(SRptReportnotfound);
 memstream:=TMemoryStream.Create;
 try
  FOnLoadReport(NodeInfo.ReportName,memstream);
  memstream.Seek(0,soFromBeginning);
  CurrentLoaded:='';
  report.LoadFromStream(memstream);
  report.Report.SaveToFile('c:\Documents and Settings\Toni\Mis Documentos\LastOpened.rep');
  CurrentLoaded:=NodeInfo.ReportName;
 finally
  memstream.free;
 end;
end;

procedure TFRpDBTree.ATreeItemClick(Sender: TObject; Button: TMouseButton;
  Node: TTreeNode; const Pt: TPoint);
var
 ANode:TTreeNode;
 NodeInfo:TRpNodeInfo;
begin
 // See if selected node is a report
 if Not Assigned(ATree.Selected) then
 begin
  DisableButtonsReport;
  exit;
 end;
 ANode:=ATree.Selected;
 NodeInfo:=TRpNodeInfo(ANode.Data);
 if Not Assigned(Nodeinfo) then
 begin
  DisableButtonsReport;
  exit;
 end;
 if Length(NodeInfo.ReportName)<1 then
 begin
  DisableButtonsReport;
  exit;
 end;
 EnableButtonsReport;
end;

procedure TFRpDBTree.DisableButtonsReport;
begin
 APreview.Enabled:=False;
 APrint.Enabled:=False;
 AUserParams.Enabled:=False;
end;


procedure TFRpDBTree.EnableButtonsReport;
begin
 APreview.Enabled:=True;
 APrint.Enabled:=True;
 AUserParams.Enabled:=True;
end;


procedure TFRpDBTree.AUserParamsExecute(Sender: TObject);
begin
 CheckLoaded;
 report.ShowParams;
end;

procedure TFRpDBTree.APrintExecute(Sender: TObject);
begin
 CheckLoaded;
 report.Preview:=false;
 report.Execute;
end;

procedure TFRpDBTree.BCancelClick(Sender: TObject);
begin
 docancel:=true;
end;

procedure TFRpDBTree.APrintSetupExecute(Sender: TObject);
begin
 Printer.ExecuteSetup;
end;

procedure TFRpDBTree.SaveDir(adir:String;anode:TTreeNode);
var
 i:integer;
 newnode:TTreeNode;
 newdir:string;
 ainfo:TRpNodeInfo;
 memstream:TMemoryStream;
begin
 // Save folders of de node
 for i:=0 to anode.Count-1 do
 begin
  newnode:=anode.Item[i];
  ainfo:=TRpNodeInfo(newnode.data);
  if Length(ainfo.ReportName)<1 then
  begin
   // Creates the dir
   newdir:=adir+'\'+String(newnode.Text);
   newdir:=StringReplace(newdir,'/','-',[rfReplaceAll]);
   CreateDir(newdir);
   SaveDir(newdir,newnode);
  end
  else
  begin
   if Not Assigned(FOnLoadReport) then
    Raise Exception.Create(SRptReportnotfound);
   memstream:=TMemoryStream.Create;
   try
    FOnLoadReport(ainfo.ReportName,memstream);
    memstream.Seek(0,soFromBeginning);
    memstream.SaveToFile(adir+'\'+ainfo.ReportName+'.rep');
   finally
    memstream.free;
   end;
  end;
  inc(counter);
  CheckCancel(counter);
 end;
end;

procedure TFRpDBTree.ToolButton9Click(Sender: TObject);
var
 aDir:String;
 newnode:TTreeNode;
 i:integer;
 newdir:string;
 ainfo:TRpNodeInfo;
 memstream:TMemoryStream;
begin
 // Exports database reports to filenames and directories
 if Not SaveDialog1.Execute then
  exit;
 adir:=ExtractFilePath(SaveDialog1.Filename);
 if Length(adir)<1 then
  exit;
 if adir[Length(adir)]='\' then
  adir:=Copy(adir,1,Length(adir)-1);
 // Get the time
{$IFDEF MSWINDOWS}
 mmfirst:=TimeGetTime;
{$ENDIF}
{$IFDEF LINUX}
 milifirst:=now;
{$ENDIF}
 counter:=0;
 BCancel.Visible:=True;
 try
  for i:=0 to ATree.Items.Count-1 do
  begin
   newnode:=Atree.Items.Item[i];
   if Assigned(newnode.Parent) then
    continue;
   ainfo:=TRpNodeInfo(newnode.data);
   if Length(ainfo.ReportName)<1 then
   begin
    // Creates the dir
//    newdir:=adir+'\'+String(newnode.Text);
//    newdir:=StringReplace(newdir,'/','-',[rfReplaceAll]);
//    CreateDir(newdir);
    SaveDir(adir,newnode);
   end
   else
   begin
    if Not Assigned(FOnLoadReport) then
     Raise Exception.Create(SRptReportnotfound);
    memstream:=TMemoryStream.Create;
    try
     FOnLoadReport(ainfo.ReportName,memstream);
     memstream.Seek(0,soFromBeginning);
     memstream.SaveToFile(adir+'\'+ainfo.ReportName+'.rep');
    finally
     memstream.free;
    end;
   end;
   inc(counter);
   CheckCancel(counter);
  end;
 finally
  BCancel.Visible:=False;
 end;
end;

end.