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
    Panel1: TToolBar;
    ActionList1: TActionList;
    ImageList1: TImageList;
    AUp: TAction;
    ADown: TAction;
    BNew: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ADelete: TAction;
    PopupMenu1: TPopupMenu;
    Detail1: TMenuItem;
    Pageheader1: TMenuItem;
    Pagefooter1: TMenuItem;
    Groupheaderandfooter1: TMenuItem;
    Subreport1: TMenuItem;
    procedure Expand1Click(Sender: TObject);
    procedure RViewClick(Sender: TObject);
    procedure AUpExecute(Sender: TObject);
    procedure ADownExecute(Sender: TObject);
    procedure ADeleteExecute(Sender: TObject);
    procedure BNewClick(Sender: TObject);
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
 if Assigned(designframe) then
 begin
  TFDesignFrame(designframe).UpdateSelection(true);
 end;
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
var
 aobject:TObject;
begin
 TFDesignFrame(designframe).UpdateSelection(false);
 aobject:=FindSelectedObject;
 if Assigned(aobject) then
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
 aobject:TObject;
 changesubrep:integer;
 asection:TRpSection;
 asec:TRpSection;
 i,index:integer;
 asectype:TRpSectionType;
 firstdetail,lastdetail:integer;
begin
 // Goes up
 aobject:=FindSelectedObject;
 if (aobject is TRpSubReport) then
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
 end
 else
 begin
  if (aobject is TRpSection) then
  begin
   subrep:=FindSelectedSubreport;
   asection:=TRpSection(aobject);
   // It can be a detail,pheader,pfooter
   if asection.SectionType in [rpsecdetail,rpsecpheader,rpsecpfooter] then
   begin
    asectype:=asection.SectionType;
    firstdetail:=subrep.FirstSectionThatIs(asectype);
    lastdetail:=subrep.LastSectionThatIs(asectype);
    if firstdetail=lastdetail then
     exit;
    index:=-1;
    for i:=firstdetail to lastdetail do
    begin
     if subrep.Sections[i].Section=asection then
     begin
      index:=i;
      break;
     end;
    end;
    if index>=0 then
    begin
     if index>firstdetail then
     begin
      asec:=subrep.Sections[index-1].Section;
      subrep.Sections[index-1].Section:=subrep.Sections[index].Section;
      subrep.Sections[index].Section:=asec;
      SetReport(FReport);
      SelectDataItem(asection);
     end;
    end;
   end
   else
   begin
    if asection.SectionType=rpsecgfooter then
    begin
     if subrep.GroupCount<2 then
      exit;
     index:=-1;
     lastdetail:=subrep.LastDetail;
     firstdetail:=subrep.FirstDetail;
     for i:=1 to subrep.GroupCount do
     begin
      asec:=subrep.Sections.Items[lastdetail+i].Section;
      if asec=asection then
      begin
       index:=i;
       break;
      end;
     end;
     if index<0 then
      exit;
     if index<2 then
      exit;
     // Group footer
     asec:=subrep.Sections.Items[lastdetail+index-1].Section;
     subrep.Sections.Items[lastdetail+index-1].Section:=subrep.Sections.Items[lastdetail+index].Section;
     subrep.Sections.Items[lastdetail+index].Section:=asec;
     // Group Header
     asec:=subrep.Sections.Items[firstdetail-index+1].Section;
     subrep.Sections.Items[firstdetail-index+1].Section:=subrep.Sections.Items[firstdetail-index].Section;
     subrep.Sections.Items[firstdetail-index].Section:=asec;
     // Update
     SetReport(FReport);
     SelectDataItem(asection);
    end
    else
    begin
     if asection.SectionType=rpsecgheader then
     begin
      if subrep.GroupCount<2 then
       exit;
      index:=-1;
      lastdetail:=subrep.LastDetail;
      firstdetail:=subrep.FirstDetail;
      for i:=1 to subrep.GroupCount do
      begin
       asec:=subrep.Sections.Items[firstdetail-i].Section;
       if asec=asection then
       begin
        index:=i;
        break;
       end;
      end;
      if index<0 then
       exit;
      if index>=subrep.groupcount then
       exit;
      // Group footer
      asec:=subrep.Sections.Items[lastdetail+index+1].Section;
      subrep.Sections.Items[lastdetail+index+1].Section:=subrep.Sections.Items[lastdetail+index].Section;
      subrep.Sections.Items[lastdetail+index].Section:=asec;
      // Group Header
      asec:=subrep.Sections.Items[firstdetail-index-1].Section;
      subrep.Sections.Items[firstdetail-index-1].Section:=subrep.Sections.Items[firstdetail-index].Section;
      subrep.Sections.Items[firstdetail-index].Section:=asec;
      // Update
      SetReport(FReport);
      SelectDataItem(asection);
     end
     else
     begin
      if asection.SectionType=rpsecpheader then
      begin

      end;
     end;
    end;
   end;
  end;
 end;
end;

procedure TFRpStructure.ADownExecute(Sender: TObject);
var
 subrep:TRpSubreport;
 arep:TRpSubReport;
 changesubrep:integer;
 i:integer;
 aobject:TObject;
 index:integer;
 asection,asec:TRpSection;
 asectype:TRpSectionType;
 firstdetail,lastdetail:integer;
begin
 // Goes down
 aobject:=FindSelectedObject;
 if (aobject is TRpSubReport) then
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
 end
 else
 begin
  if (aobject is TRpSection) then
  begin
   subrep:=FindSelectedSubreport;
   asection:=TRpSection(aobject);
   // It can be a detail,pheader,pfooter
   if asection.SectionType in [rpsecdetail,rpsecpheader,rpsecpfooter] then
   begin
    asectype:=asection.SectionType;
    firstdetail:=subrep.FirstSectionThatIs(asectype);
    lastdetail:=subrep.LastSectionThatIs(asectype);
    if firstdetail=lastdetail then
     exit;
    index:=-1;
    for i:=firstdetail to lastdetail do
    begin
     if subrep.Sections[i].Section=asection then
     begin
      index:=i;
      break;
     end;
    end;
    if index<0 then
     exit;
    if index<lastdetail then
    begin
     asec:=subrep.Sections[index+1].Section;
     subrep.Sections[index+1].Section:=subrep.Sections[index].Section;
     subrep.Sections[index].Section:=asec;
     SetReport(FReport);
     SelectDataItem(asection);
    end;
   end
   else
   begin
    if asection.SectionType=rpsecgfooter then
    begin
     if subrep.GroupCount<2 then
      exit;
     index:=-1;
     lastdetail:=subrep.LastDetail;
     firstdetail:=subrep.FirstDetail;
     for i:=1 to subrep.GroupCount do
     begin
      asec:=subrep.Sections.Items[lastdetail+i].Section;
      if asec=asection then
      begin
       index:=i;
       break;
      end;
     end;
     if index<0 then
      exit;
     if index>=subrep.GroupCount then
      exit;
     // Group footer
     asec:=subrep.Sections.Items[lastdetail+index+1].Section;
     subrep.Sections.Items[lastdetail+index+1].Section:=subrep.Sections.Items[lastdetail+index].Section;
     subrep.Sections.Items[lastdetail+index].Section:=asec;
     // Group Header
     asec:=subrep.Sections.Items[firstdetail-index-1].Section;
     subrep.Sections.Items[firstdetail-index-1].Section:=subrep.Sections.Items[firstdetail-index].Section;
     subrep.Sections.Items[firstdetail-index].Section:=asec;
     // Update
     SetReport(FReport);
     SelectDataItem(asection);
    end;
   end;
  end;
 end;
end;

procedure TFRpStructure.ADeleteExecute(Sender: TObject);
begin
 fmainf.ADeleteSelection.Execute;
end;

procedure TFRpStructure.BNewClick(Sender: TObject);
var
 apoint:TPoint;
begin
 apoint.x:=BNew.Left;
 apoint.y:=BNew.Top+BNew.Height;
 apoint:=BNew.Parent.ClientToScreen(apoint);
 BNew.DropDownMenu.Popup(apoint.x,apoint.y);
end;

end.
