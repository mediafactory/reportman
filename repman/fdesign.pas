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
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}

unit fdesign;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs, QMenus,
  QTypes, QExtCtrls,frpstruc,rpobinsint,rpreport,
  fsectionint,rpsubreport,rpsection, rpruler,rpobjinsp;

type
  TFDesignFrame = class(TFrame)
    SectionScrollBox: TScrollBox;
    TopRuler: TRpRuler;
    LeftRuler: TRpRuler;
  private
    { Private declarations }
    FReport:TRpReport;
    FObjInsp:TFObjInsp;
    FSectionInterface:TRpSectionInterface;
    procedure SetReport(Value:TRpReport);
  public
    { Public declarations }
    freportstructure:TFRpStructure;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure UpdateSelection;
    procedure UpdateInterface;
    property Report:TRpReport read FReport write SetReport;
    property ObjInsp:TFObjInsp read FObjInsp write FObjInsp;
  end;

implementation

{$R *.xfm}

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
 UpdateSelection;
end;


procedure TFDesignFrame.UpdateInterface;
begin
 if Assigned(FSectionInterface) then
 begin
  FSectionInterface.UpdatePos;
  TopRuler.Width:=FSectionInterface.Width;
  LeftRuler.Height:=FSectionInterface.Height;
 end;
end;

procedure TFDesignFrame.UpdateSelection;
var
 data:Pointer;
 dataobj:TOBject;
begin
 if Assigned(FSectionInterface) then
 begin
  if assigned(fobjinsp) then
  begin
   if (fobjinsp.CompItem=FSectionInterface) then
    fobjinsp.CompItem:=nil;
  end;
  FSectionInterface.free;
  FSectionInterface:=nil;
  TopRuler.Visible:=False;
  LeftRuler.Visible:=False;
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
  exit;
 if (dataobj is TRpSection) then
 begin
  FSectionInterface:=TRpSectionInterface.Create(Self,TRpSection(dataobj));
  FSectionInterface.Parent:=SectionScrollBox;
  FSectionInterface.Top:=TopRuler.Height;
  FSectionInterface.Left:=LeftRuler.Width;
  TopRuler.Width:=FSectionInterface.Width;
  LeftRuler.Height:=FSectionInterface.Height;
  TopRuler.Visible:=true;
  LeftRuler.Visible:=true;
  if assigned(fobjinsp) then
  begin
   fobjinsp.CompItem:=FSectionInterface;
  end;
 end;
end;

end.
