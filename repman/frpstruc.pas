{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       fRpstruc                                        }
{       Shows the report structure and allow to alter it}
{                                                       }
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

unit frpstruc;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QComCtrls,rpreport,rpsubreport,rpconsts, QMenus, QTypes,
  rpsection,rpobjinsp,rpprintitem, QActnList, QImgList, QButtons, QExtCtrls;

type
  TFRpStructure = class(TFrame)
    RView: TTreeView;
    Panel1: TPanel;
    BUp: TSpeedButton;
    BDown: TSpeedButton;
    ActionList1: TActionList;
    ImageList1: TImageList;
    AUp: TAction;
    ADown: TAction;
    procedure Expand1Click(Sender: TObject);
    procedure RViewClick(Sender: TObject);
    procedure AUpExecute(Sender: TObject);
    procedure ADownExecute(Sender: TObject);
  private
    { Private declarations }
    FReport:TRpReport;
    FObjInsp:TFObjInsp;
    procedure SetReport(Value:TRpReport);
    procedure CreateInterface;
  public
    { Public declarations }
    designframe:TControl;
    function FindSelectedSubreport:TRpSubreport;
    function FindSelectedObject:TObject;
    procedure DeleteSelectedNode;
    property Report:TRpReport read FReport write SetReport;
    property ObjInsp:TFObjInsp read FObjInsp write FObjInsp;
    procedure SelectDataItem(data:TObject);
  end;

implementation

{$R *.xfm}

uses fdesign, fmain;

procedure TFRpStructure.SetReport(Value:TRpReport);
begin
 FReport:=Value;
 if Not Assigned(FReport) then
  exit;
 // Creates the interface
 CreateInterface;
 RView.Selected:=RView.Items.Item[0];
 RView.FullExpand;
end;

function TFRpStructure.FindSelectedSubreport:TRpSubreport;
var
 selectednode:TTreeNode;
begin
 Result:=nil;
 selectednode:=RView.Selected;
 if Not Assigned(selectednode) then
  Raise Exception.Create(SRPNoSelectedSubreport);
 Assert(selectednode.data<>nil,'Node without data assertion error');
 if (TObject(selectednode.data) is TRpSubReport) then
 begin
  Result:=TRpSubReport(selectednode.data);
  exit;
 end;
 selectednode:=selectednode.Parent;
 Assert(selectednode.data<>nil,'Expected subreport');
 if (TObject(selectednode.data) is TRpSubReport) then
 begin
  Result:=TRpSubReport(selectednode.data);
  exit;
 end;
 Assert(selectednode.data<>nil,'Expected subreport');
end;


function TFRpStructure.FindSelectedObject:TObject;
var
 selectednode:TTreeNode;
begin
 selectednode:=RView.Selected;
 if Not Assigned(selectednode) then
  Raise Exception.Create(SRPNoSelectedSubreport);
 Assert(selectednode.data<>nil,'Node without data assertion error');
 Assert(selectednode.data<>nil,'Expected data with a value');
 Result:=TObject(selectednode.data);
end;

procedure TFRpStructure.CreateInterface;
var
 anew:TTreeNode;
 i,j:integer;
 subr:TRpSubreport;
 child:TTreeNode;
begin
 RView.Items.Clear;

 // Adds the items
 for i:=0 to Report.SubReports.Count-1 do
 begin
  subr:=Report.SubReports.Items[i].SubReport;
  anew:=RView.Items.Add(nil,SRpSubReport);
  anew.data:=Report.SubReports.Items[i].SubReport;
  for j:=0 to subr.Sections.Count-1 do
  begin
   child:=RView.Items.AddChild(anew,subr.Sections.Items[j].Section.SectionCaption);
   child.data:=subr.Sections.Items[j].Section;
  end;
 end;
end;


procedure TFRpStructure.Expand1Click(Sender: TObject);
begin
 RView.FullExpand;
end;

procedure TFRpStructure.DeleteSelectedNode;
var
 secorsub:TObject;
 selsubreport:TRpSubReport;
begin
 secorsub:=FindSelectedObject;
 if (secorsub is TRpSubReport) then
  freport.DeleteSubreport(TRpSubReport(secorsub))
 else
 begin
  if (Not (secorsub is TRpSection)) then
   Raise Exception.Create(SRPNoSelectedSection);
  selsubreport:=FindSelectedSubreport;
  selsubreport.FreeSection(TRpSection(secorsub));
 end;
end;

procedure TFRpStructure.RViewClick(Sender: TObject);
begin
 TFDesignFrame(designframe).UpdateSelection(false);
 if (FindSelectedObject is TRpSubReport) then
 begin
  AUp.Enabled:=True;
  ADown.Enabled:=True;
 end
 else
 begin
  AUp.Enabled:=False;
  ADown.Enabled:=False;
 end;
end;

function FindDataInTree(nodes:TTreeNodes;data:TObject):TTreeNode;
var
 i:integer;
begin
 Result:=nil;
 i:=0;
 while i<nodes.Count do
 begin
  if nodes.item[i].data=Data then
  begin
   Result:=nodes.item[i];
   break;
  end;
  inc(i);
 end;
end;

procedure TFRpStructure.SelectDataItem(data:TObject);
var
 anode:TTreeNode;
begin
 anode:=FindDataInTree(RView.Items,data);
 if Assigned(anode) then
 begin
  RView.Selected:=anode;
  RViewClick(Self);
 end;
end;


procedure TFRpStructure.AUpExecute(Sender: TObject);
var
 subrep:TRpSubreport;
 arep:TRpSubReport;
 changesubrep:integer;
 i:integer;
begin
 // Goes up
 if (FindSelectedObject is TRpSubReport) then
 begin
  subrep:=TRpSubReport(FindSelectedObject);
  i:=0;
  changesubrep:=-1;
  while i<report.SubReports.Count do
  begin
   if report.SubReports.Items[i].SubReport=subrep then
   begin
    if changesubrep<0 then
     break;
    arep:=report.SubReports.Items[changesubrep].SubReport;
    report.SubReports.Items[changesubrep].SubReport:=subrep;
    report.SubReports.Items[i].SubReport:=arep;
    SetReport(FReport);
    break;
   end;
   changesubrep:=i;
   inc(i);
  end;
 end;
end;

procedure TFRpStructure.ADownExecute(Sender: TObject);
var
 subrep:TRpSubreport;
 arep:TRpSubReport;
 changesubrep:integer;
 i:integer;
begin
 // Goes down
 if (FindSelectedObject is TRpSubReport) then
 begin
  subrep:=TRpSubReport(FindSelectedObject);
  i:=0;
  changesubrep:=-1;
  while i<report.SubReports.Count do
  begin
   if report.SubReports.Items[i].SubReport=subrep then
   begin
    changesubrep:=i;
   end
   else
   begin
    if changesubrep>=0 then
    begin
     arep:=report.SubReports.Items[i].SubReport;
     report.SubReports.Items[i].SubReport:=subrep;
     report.SubReports.Items[changesubrep].SubReport:=arep;
     SetReport(FReport);
     break;
    end;
   end;
   inc(i);
  end;
 end;
end;

end.
