{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       fdesign                                         }
{       Design frame of the Main form                   }
{       Used by a subreport                             }
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

unit fdesign;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs, QMenus,
  QTypes, QExtCtrls,frpstruc,rpobinsint,rpreport,rpmunits,
  fsectionint,rpsubreport,rpsection, rpruler,rpobjinsp;

const
 CONS_RULER_LEFT=20;
 CT_TITLE_HEIGHT=15;
type
  TFDesignFrame = class(TFrame)
    PTop: TPanel;
    TopRuler: TRpRuler;
    PLeft: TPanel;
    SectionScrollBox: TScrollBox;
    PSection: TPanel;
  private
    { Private declarations }
    FReport:TRpReport;
    FObjInsp:TFObjInsp;
    leftrulers:Tlist;
    FSubReport:TRpSubreport;
    toptitles:Tlist;
    secinterfaces:TList;
    procedure SetReport(Value:TRpReport);
    procedure SecPosChange(Sender:TObject);
  public
    { Public declarations }
    freportstructure:TFRpStructure;
    procedure UpdateInterface;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure UpdateSelection(force:boolean);
    procedure SelectSubReport(subreport:TRpSubReport);
    property Report:TRpReport read FReport write SetReport;
    property ObjInsp:TFObjInsp read FObjInsp write FObjInsp;
  end;


implementation

{$R *.xfm}

uses fmain;

constructor TFDesignFrame.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 leftrulers:=Tlist.Create;
 toptitles:=Tlist.Create;
 secinterfaces:=TList.Create;

end;

destructor TFDesignFrame.Destroy;
begin
 leftrulers.free;
 toptitles.free;
 secinterfaces.free;

 inherited Destroy;
end;


procedure TFDesignFrame.SetReport(Value:TRpReport);
begin
 FReport:=Value;
 if Not Assigned(FReport) then
  exit;
 SelectSubReport(Freport.SubReports[0].SubReport);
 UpdateSelection(false);
end;



procedure TFDesignFrame.UpdateSelection(force:boolean);
var
 data:Pointer;
 dataobj:TOBject;
 FSectionInterface:TRpSectionInterface;
 i:integer;
begin
 if Not Assigned(freportstructure) then
  exit;
 if Not Assigned(freportstructure.RView.Selected) then
  exit;
 data:=freportstructure.RView.Selected.Data;
 if Not Assigned(data) then
  exit;
 dataobj:=TObject(data);
 if (dataobj is TRpSubReport) then
 begin
  if assigned(fobjinsp) then
  begin
   fobjinsp.CompItem:=nil;
  end;
  exit;
 end;
 if (dataobj is TRpSection) then
 begin
  i:=0;
  FSectionInterface:=nil;
  while i<secinterfaces.count do
  begin
   if TRpSectionInterface(secinterfaces.items[i]).printitem=dataobj then
   begin
    FSectionInterface:=TRpSectionInterface(secinterfaces.items[i]);
    break;
   end;
   inc(i);
  end;
  if Assigned(FSectionInterface) then
   fobjinsp.CompItem:=FSectionInterface;
 end;
(*{$IFDEF MSWINDOWS}
  Application.ProcessMessages;
{$ENDIF}
  SectionScrollBox.HorzScrollBar.Position:=0;
  SectionScrollBox.VertScrollBar.Position:=0;
{$IFDEF MSWINDOWS}
  if Assigned(FSectionInterface) then
   FSectionInterface.InvalidateAll;
{$ENDIF}
 end;
*)end;


procedure TFDesignFrame.SecPosChange(Sender:TObject);
var
 i:integer;
 aruler:TRpRuler;
 despy:integer;
begin
 TopRuler.Left:=CONS_RULER_LEFT-SectionScrollBox.HorzScrollBar.Position;;
 for i:=0 to leftrulers.count-1 do
 begin
  despy:=SectionScrollBox.VertScrollBar.Position;
  aruler:=TRpRuler(leftrulers.items[i]);
  aruler.Top:=TRpSectionInterface(secinterfaces.Items[i]).Top-despy;
 end;
end;



procedure TFDesignFrame.SelectSubReport(subreport:TRpSubReport);
var
 i:integer;
 asecint:TRpSectionInterface;
 apanel:Tpanel;
 aruler:TRpRuler;
 posx:integer;
 maxwidth:integer;
begin
 // If subreport is not the same frees
 if Fsubreport=subreport then
  exit;
 if assigned(fsubreport) then
 begin
  for i:=0 to secinterfaces.Count-1 do
  begin
   TRpSectionInterface(secinterfaces.Items[i]).Free;
   TPanel(TopTitles.Items[i]).Free;
   TRpRuler(LeftRulers.Items[i]).Free;
  end;
  secinterfaces.clear;
  toptitles.clear;
  leftrulers.Clear;
 end;
 Fsubreport:=subreport;
 if not assigned(fsubreport) then
  exit;
 SectionScrollBox.Visible:=false;
 try
  maxwidth:=0;
  posx:=0;
  for i:=0 to fsubreport.Sections.Count-1 do
  begin
   apanel:=TPanel.Create(self);
   apanel.Height:=CT_TITLE_HEIGHT;
   apanel.Caption:=' '+FSubReport.Sections.Items[i].Section.SectionCaption;
   apanel.Alignment:=taLeftJustify;
   apanel.Color:=clAppWorkSpace;
   apanel.BorderStyle:=bsSingle;
   apanel.BevelInner:=bvNone;
   apanel.BevelOuter:=bvNone;
   apanel.Top:=posx;
   posx:=posx+apanel.Height;
   apanel.parent:=PSection;
   toptitles.Add(apanel);

   asecint:=TRpSectionInterface.Create(Self,fsubreport.Sections.Items[i].Section);
   asecint.OnPosChange:=SecPosChange;
   asecint.fobjinsp:=FObjInsp;
   asecint.freportstructure:=freportstructure;
   asecint.Left:=0;
   asecint.Top:=posx;
   asecint.UpdatePos;
   asecint.Parent:=PSection;
   asecint.CreateChilds;
   secinterfaces.Add(asecint);

   apanel.Width:=asecint.Width;


   aruler:=TRpRuler.Create(Self);
   aruler.RType:=rVertical;
   aruler.Width:=20;
   aruler.Left:=0;
   aruler.parent:=PLeft;
   leftrulers.Add(aruler);
   aruler.Top:=posx;
   aruler.Height:=asecint.Height;
   if rpmunits.defaultunit=rpUnitCms then
    aruler.Metrics:=rCms
   else
    aruler.Metrics:=rInchess;

   if maxwidth<asecint.width then
    maxwidth:=asecint.width;
   posx:=posx+asecint.Height;

  end;
  for i:=0 to secinterfaces.Count-1 do
  begin
   asecint:=TRpSectionInterface(secinterfaces.items[i]);
   asecint.SendToBack;
  end;
  TopRuler.Width:=maxwidth;
  if rpmunits.defaultunit=rpUnitCms then
   TopRuler.Metrics:=rCms
  else
   TopRuler.Metrics:=rInchess;
  PSection.Height:=posx+Height;
  PSection.Width:=maxwidth;
 finally
  SectionScrollBox.Visible:=true;
 end;
end;


procedure TFDesignFrame.UpdateInterface;
var
 i,j:integer;
 asecint:TRpSectionInterface;
 apanel:Tpanel;
 aruler:TRpRuler;
 posx:integer;
 maxwidth:integer;
begin
 if not Assigned(FSubreport) then
  exit;
 SectionScrollBox.Visible:=true;
 try
  SectionScrollBox.HorzScrollBar.Position:=0;
  SectionScrollBox.VertScrollBar.Position:=0;
  maxwidth:=0;
  posx:=0;
  for i:=0 to secinterfaces.Count-1 do
  begin
   apanel:=TPanel(toptitles.Items[i]);
   asecint:=TRpSectionInterface(secinterfaces.items[i]);
   apanel.Width:=asecint.Width;
   apanel.Caption:=' '+FSubReport.Sections.Items[i].Section.SectionCaption;
   apanel.Top:=posx;
   posx:=posx+apanel.Height;

   asecint.Top:=posx;
   asecint.UpdatePos;
   for j:=0 to asecint.childlist.Count-1 do
   begin
    TRpSizePosInterface(asecint.childlist.Items[i]).UpdatePos;
   end;
   apanel.Width:=asecint.Width;

   aruler:=TRpRuler(leftrulers.items[i]);
   aruler.Top:=posx;
   aruler.Height:=asecint.Height;
   if rpmunits.defaultunit=rpUnitCms then
    aruler.Metrics:=rCms
   else
    aruler.Metrics:=rInchess;

   if maxwidth<asecint.width then
    maxwidth:=asecint.width;
   posx:=posx+asecint.Height;
   asecint.SendToBack;
  end;
  TopRuler.Width:=maxwidth;
  if rpmunits.defaultunit=rpUnitCms then
   TopRuler.Metrics:=rCms
  else
   TopRuler.Metrics:=rInchess;
 finally
  SectionScrollBox.Visible:=true;
 end;
 PSection.Height:=posx+Height;
 PSection.Width:=maxwidth;
 SectionScrollBox.HorzScrollBar.Position:=0;
 SectionScrollBox.VertScrollBar.Position:=0;
end;


end.
