{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdfdesign                                     }
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

unit rpmdfdesign;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs, QMenus,
  QTypes, QExtCtrls,rpmdfstruc,rpmdobinsint,rpreport,rpmunits,
  rpmdfsectionint,rpsubreport,rpsection, rpruler,rpmdobjinsp;

const
 CONS_RULER_LEFT=20;
 CT_TITLE_HEIGHT=15;
type

  // A ScrollBox that not scrolls in view focused controls
  TRpScrollBox=class(TScrollBox)
   protected
    procedure AutoScrollInView(AControl: TControl); override;
   end;

  TRpPaintEventPanel=Class(TPanel)
   private
    FOnPaint:TNotifyEvent;
    Updating:boolean;
   protected
    procedure Paint;override;
   public
    CaptionText:string;
    constructor Create(AOwner:TComponent);override;
    property OnPaint:TNotifyEvent read FOnPaint write FOnPaint;
   end;

  TFDesignFrame = class(TFrame)
    PTop: TPanel;
    TopRuler: TRpRuler;
    PLeft: TPanel;
  private
    { Private declarations }
    PSection: TRpPaintEventPanel;
    FReport:TRpReport;
    FObjInsp:TFRpObjInsp;
    leftrulers:Tlist;
    FSubReport:TRpSubreport;
    toptitles:Tlist;
    secinterfaces:TList;
    procedure SetReport(Value:TRpReport);
    procedure SecPosChange(Sender:TObject);
  public
    { Public declarations }
    freportstructure:TFRpStructure;
    SectionScrollBox: TScrollBox;
    procedure UpdateInterface;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure UpdateSelection(force:boolean);
    procedure SelectSubReport(subreport:TRpSubReport);
    property Report:TRpReport read FReport write SetReport;
    property ObjInsp:TFRpObjInsp read FObjInsp write FObjInsp;
  end;


implementation

{$R *.xfm}

uses rpmdfmain;

procedure TrpScrollBox.AutoScrollInView(AControl: TControl);
begin

end;


constructor TrpPaintEventPanel.Create(AOwner:TComponent);
begin
 Inherited Create(AOwner);

 BevelInner:=bvNone;
 BevelOuter:=bvNone;
 BorderStyle:=bsNone;
end;

procedure TRpPaintEventPanel.Paint;
begin
 inherited Paint;

 if not updating then
 begin
  if Assigned(FOnPaint) then
   FOnPaint(Self);
 end;

 if not assigned(parent) then
  exit;

 if (parent.parent is TScrollBox) then
  Canvas.TextOut(TScrollBox(parent.parent).HorzScrollBar.Position,0,CaptionText);
end;



constructor TFDesignFrame.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 SectionScrollBox:=TRpScrollBox.Create(Self);
 SectionScrollBox.BorderStyle:=bsNone;
 SectionScrollBox.Align:=Alclient;
 SectionScrollBox.Parent:=Self;


 leftrulers:=Tlist.Create;
 toptitles:=Tlist.Create;
 secinterfaces:=TList.Create;

 PSection:=TRpPaintEventPanel.Create(Self);
 PSection.Parent:=SectionSCrollBox;
 PSection.OnPaint:=SecPosChange;
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
 asubreport:TRpSubReport;
begin
 if Not Assigned(freportstructure) then
  exit;
 if Not Assigned(freportstructure.RView.Selected) then
  exit;
 data:=freportstructure.RView.Selected.Data;
 if Not Assigned(data) then
  exit;
 if force then
 begin
  SelectSubReport(nil);
 end;

 dataobj:=TObject(data);
 // Looks if there is a subreport selected
 asubreport:=freportstructure.FindSelectedSubreport;
 if asubreport<>FSubReport then
 begin
  SelectSubReport(asubreport);
 end;
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
 apanel:TRpPaintEventPanel;
begin
 TopRuler.Left:=CONS_RULER_LEFT-SectionScrollBox.HorzScrollBar.Position;;
 for i:=0 to leftrulers.count-1 do
 begin
  despy:=SectionScrollBox.VertScrollBar.Position;
  aruler:=TRpRuler(leftrulers.items[i]);
  aruler.Top:=TRpSectionInterface(secinterfaces.Items[i]).Top-despy;
  apanel:=TRpPaintEventPanel(toptitles.Items[i]);
  apanel.Updating:=true;
  try
   apanel.Invalidate;
   apanel.Update;
  finally
   apanel.Updating:=false;
  end;
 end;
end;



procedure TFDesignFrame.SelectSubReport(subreport:TRpSubReport);
var
 i:integer;
 asecint:TRpSectionInterface;
 apanel:TRpPaintEventpanel;
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
 FObjInsp.RecreateChangesize;
 FObjInsp.CompItem:=nil;
 if not assigned(fsubreport) then
  exit;
 SectionScrollBox.Visible:=true;
 try
  maxwidth:=0;
  posx:=0;
  for i:=0 to fsubreport.Sections.Count-1 do
  begin
   apanel:=TRpPaintEventPanel.Create(self);
   apanel.OnPaint:=SecPosChange;
   apanel.Height:=CT_TITLE_HEIGHT;
   apanel.Caption:='';
   apanel.CaptionText:=' '+FSubReport.Sections.Items[i].Section.SectionCaption;
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
   asecint.UpdatePos;
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
 SectionScrollBox.VertScrollBar.Position:=0;
 SectionScrollBox.HorzScrollBar.Position:=0;
end;


procedure TFDesignFrame.UpdateInterface;
var
 i,j:integer;
 asecint:TRpSectionInterface;
 apanel:TRpPaintEventpanel;
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
   apanel:=TRpPaintEventpanel(toptitles.Items[i]);
   asecint:=TRpSectionInterface(secinterfaces.items[i]);
   apanel.Width:=asecint.Width;
   apanel.Caption:='';
   apanel.CaptionText:=' '+FSubReport.Sections.Items[i].Section.SectionCaption;
   apanel.Top:=posx;
   posx:=posx+apanel.Height;

   asecint.Top:=posx;
   asecint.UpdatePos;
   for j:=0 to asecint.childlist.Count-1 do
   begin
    TRpSizePosInterface(asecint.childlist.Items[j]).UpdatePos;
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