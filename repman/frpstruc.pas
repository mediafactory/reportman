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
{$IFNDEF PROFILE}  rpsection,rpobjinsp,rpprintitem, QActnList, QImgList, QButtons, QExtCtrls;{$ENDIF}
{$IFDEF PROFILE}  rpsection,rpobjinsp,rpprintitem, QActnList, QImgList, QButtons, QExtCtrls ,Proftimx;{$ENDIF}

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
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,104; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 FReport:=Value;
 if Not Assigned(FReport) then
  exit;
 // Creates the interface
 CreateInterface;
 RView.Selected:=RView.Items.Item[0];
 RView.FullExpand;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,104; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

function TFRpStructure.FindSelectedSubreport:TRpSubreport;
var
 selectednode:TTreeNode;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,105; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,105; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


function TFRpStructure.FindSelectedObject:TObject;
var
 selectednode:TTreeNode;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,106; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 selectednode:=RView.Selected;
 if Not Assigned(selectednode) then
  Raise Exception.Create(SRPNoSelectedSubreport);
 Assert(selectednode.data<>nil,'Node without data assertion error');
 Assert(selectednode.data<>nil,'Expected data with a value');
 Result:=TObject(selectednode.data);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,106; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFRpStructure.CreateInterface;
var
 anew:TTreeNode;
 i,j:integer;
 subr:TRpSubreport;
 child:TTreeNode;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,107; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,107; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


procedure TFRpStructure.Expand1Click(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,108; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 RView.FullExpand;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,108; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFRpStructure.DeleteSelectedNode;
var
 secorsub:TObject;
 selsubreport:TRpSubReport;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,109; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,109; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFRpStructure.RViewClick(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,110; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,110; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

function FindDataInTree(nodes:TTreeNodes;data:TObject):TTreeNode;
var
 i:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,111; xor eax,eax; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,111; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFRpStructure.SelectDataItem(data:TObject);
var
 anode:TTreeNode;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,112; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 anode:=FindDataInTree(RView.Items,data);
 if Assigned(anode) then
 begin
  RView.Selected:=anode;
  RViewClick(Self);
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,112; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


procedure TFRpStructure.AUpExecute(Sender: TObject);
var
 subrep:TRpSubreport;
 arep:TRpSubReport;
 changesubrep:integer;
 i:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,113; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,113; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFRpStructure.ADownExecute(Sender: TObject);
var
 subrep:TRpSubreport;
 arep:TRpSubReport;
 changesubrep:integer;
 i:integer;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,114; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,114; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

end.
