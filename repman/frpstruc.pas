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
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}

unit frpstruc;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QComCtrls,rpreport,rpsubreport,rpconsts, QMenus, QTypes,rpsection;

type
  TFRpStructure = class(TFrame)
    RView: TTreeView;
    procedure Expand1Click(Sender: TObject);
  private
    { Private declarations }
    FReport:TRpReport;
    procedure SetReport(Value:TRpReport);
    procedure CreateInterface;
  public
    { Public declarations }
    function FindSelectedSubreport:TRpSubreport;
    function FindSelectedObject:TObject;
    procedure DeleteSelectedNode;
    property Report:TRpReport read FReport write SetReport;
  end;

implementation


{$R *.xfm}

procedure TFRpStructure.SetReport(Value:TRpReport);
begin
 FReport:=Value;
 if Not Assigned(FReport) then
  exit;
 // Creates the interface
 CreateInterface;
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

end.
