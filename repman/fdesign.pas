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
type
  TFDesignFrame = class(TFrame)
    PTop: TPanel;
    TopRuler: TRpRuler;
    PLeft: TPanel;
    LeftRuler: TRpRuler;
    SectionScrollBox: TScrollBox;
    PSection: TPanel;
  private
    { Private declarations }
    FReport:TRpReport;
    FObjInsp:TFObjInsp;
    FSectionInterface:TRpSectionInterface;
    procedure SectionDestroy(Sender:TObject);
    procedure SetReport(Value:TRpReport);
    procedure SecPosChange(Sender:TObject);
  public
    { Public declarations }
    freportstructure:TFRpStructure;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure UpdateSelection(force:boolean);
    procedure UpdateInterface;
    property Report:TRpReport read FReport write SetReport;
    property ObjInsp:TFObjInsp read FObjInsp write FObjInsp;
  end;


implementation

{$R *.xfm}

uses fmain;

constructor TFDesignFrame.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

end;

destructor TFDesignFrame.Destroy;
begin
 inherited Destroy;
end;


procedure TFDesignFrame.SetReport(Value:TRpReport);
begin
 FReport:=Value;
 if Not Assigned(FReport) then
  exit;
 UpdateSelection(false);
end;


procedure TFDesignFrame.UpdateInterface;
begin
 if Assigned(FSectionInterface) then
 begin
  FSectionInterface.UpdatePos;
  TopRuler.Width:=FSectionInterface.Width;
  LeftRuler.Height:=FSectionInterface.Height;
  if rpmunits.defaultunit=rpUnitCms then
  begin
   LeftRuler.Metrics:=rCms;
   TopRuler.Metrics:=rCms;
  end
  else
  begin
   LeftRuler.Metrics:=rInchess;
   TopRuler.Metrics:=rInchess;
  end;
 end;
end;

procedure TFDesignFrame.UpdateSelection(force:boolean);
var
 data:Pointer;
 dataobj:TOBject;
begin
 if Assigned(FSectionInterface) then
 begin
  if assigned(fobjinsp) then
  begin
   if ((freportstructure.FindSelectedObject=FSectionInterface.printitem)
    and (not force)) then
    exit;
  end;
  FSectionInterface.free;
  FSectionInterface:=nil;
  TopRuler.Visible:=False;
  LeftRuler.Visible:=False;
  FObjInsp.CompItem:=nil;
  SectionScrollBox.HorzScrollBar.Position:=0;
  SectionScrollBox.VertScrollBar.Position:=0;
 end;
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
  FSectionInterface:=TRpSectionInterface.Create(Self,TRpSection(dataobj));
  FSectionInterface.OnDestroy:=SectionDestroy;
  FSectionInterface.OnPosChange:=SecPosChange;
  FSectionInterface.fobjinsp:=FObjInsp;
  FSectionInterface.Parent:=PSection;
  FSectionInterface.freportstructure:=freportstructure;
  PSection.Left:=0;
  PSection.TOp:=0;
  FSectionInterface.Top:=0;
  FSectionInterface.Left:=0;
  TopRuler.Width:=FSectionInterface.Width;
  LeftRuler.Height:=FSectionInterface.Height;
  if rpmunits.defaultunit=rpUnitCms then
  begin
   LeftRuler.Metrics:=rCms;
   TopRuler.Metrics:=rCms;
  end
  else
  begin
   LeftRuler.Metrics:=rInchess;
   TopRuler.Metrics:=rInchess;
  end;
  TopRuler.Visible:=true;
  LeftRuler.Visible:=true;
  FSectionInterface.CreateChilds;
  if assigned(fobjinsp) then
  begin
   fobjinsp.CompItem:=FSectionInterface;
  end;
{$IFDEF MSWINDOWS}
  Application.ProcessMessages;
{$ENDIF}
  SectionScrollBox.HorzScrollBar.Position:=0;
  SectionScrollBox.VertScrollBar.Position:=0;
{$IFDEF MSWINDOWS}
  if Assigned(FSectionInterface) then
   FSectionInterface.InvalidateAll;
{$ENDIF}
 end;
end;

procedure TFDesignFrame.SecPosChange(Sender:TObject);
begin
 TopRuler.Left:=CONS_RULER_LEFT-SectionScrollBox.HorzScrollBar.Position;;
 LeftRuler.Top:=-SectionScrollBox.VertScrollBar.Position;;
 PSection.Height:=FSectionInterface.Height;
 PSection.Width:=FSectionInterface.Width;
end;

procedure TFDesignFrame.SectionDestroy(Sender:TObject);
begin
 FSectionInterface:=nil;
end;

end.
