{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       Rpobjinsp                                       }
{                                                       }
{       Object inspector frame                          }
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

unit rpobjinsp;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  rpobinsint,QGrids,rpconsts,rpprintitem,QStdCtrls;

const
  CONS_LEFTGAP=3;
  CONS_CONTROLPOS=65;
  CONS_LABELTOPGAP=2;
  CONS_RIGHTBARGAP=25;

type
  TFObjInsp = class(TFrame)
  private
    { Private declarations }
    FCompItem:TRpSizeInterface;
    FDesignFrame:TObject;
    LNames:TStringList;
    LTypes:TStringList;
    LValues:TStringList;
    procedure SetCompItem(Value:TRpSizeInterface);
    procedure ReleaseAllControls;
    procedure EditChange(Sender:TObject);
  public
    { Public declarations }
    LLabels:TList;
    LControls:TStringList;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property CompItem:TRpSizeInterface read FCompItem write SetCompItem;
    property DesignFrame:TObject read FDesignFrame write FDesignFrame;
  end;



implementation

{$R *.xfm}

uses fdesign;

procedure TFObjInsp.ReleaseAllControls;
var
 i:integer;
begin
 for i:=0 to LLabels.Count-1 do
 begin
  TObject(LLabels.items[i]).Free;
  LLabels.items[i]:=nil;
 end;
 LLabels.Clear;
 for i:=0 to LControls.Count-1 do
 begin
  TObject(LControls.Objects[i]).Free;
  LControls.Objects[i]:=nil;
 end;
 LCOntrols.Clear;

end;


procedure TFObjInsp.SetCompItem(Value:TRpSizeInterface);
var
 i:integer;
 ALabel:TLabel;
 posy:integer;
 control:TControl;
 typename:string;
begin
 ReleaseAllControls;
 FCompItem:=Value;
 if Not Assigned(Value) then
 begin
  exit;
 end;
 HorzScrollBar.Position:=0;
 VertScrollBar.Position:=0;
 // Creates the labels and controls
 FCompItem.GetProperties(LNames,LTypes,LValues);
 posy:=0;
 for i:=0 to LNames.Count-1 do
 begin
  ALabel:=TLabel.Create(Self);
  ALabel.Left:=CONS_LEFTGAP;
  ALabel.Top:=posy+CONS_LABELTOPGAP;
  ALabel.Caption:=LNames.Strings[i];
  typename:=LTypes.Strings[i];
  Control:=TEdit.Create(Self);
  Control.Top:=Posy;
  Control.Left:=CONS_CONTROLPOS;
  Control.Width:=Self.Width-Control.Left-CONS_RIGHTBARGAP;
  TEdit(Control).Text:=LValues.Strings[i];
  TEdit(Control).OnChange:=EditChange;
  control.parent:=self;
  ALabel.parent:=self;
  Control.tag:=i;
  LLabels.Add(ALabel);
  LControls.AddObject(LNames.Strings[i],Control);
  posy:=posy+control.height;
 end;
end;

constructor TFObjInsp.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 LNames:=TStringList.Create;
 LValues:=TStringList.Create;
 LTypes:=TStringList.Create;

 LLabels:=TList.Create;
 LControls:=TStringList.Create;
end;

destructor TFObjInsp.Destroy;
begin
 LNames.free;
 LValues.free;
 LTypes.free;
 LLabels.free;
 LControls.free;
 inherited Destroy;
end;

procedure TFObjInsp.EditChange(Sender:TObject);
var
 index:integer;
begin
 index:=TControl(Sender).tag;
 FCompItem.SetProperty(Lnames.strings[index],TEdit(Sender).Text);
 if Assigned(FDesignFrame) then
  TFDesignFrame(FDesignFrame).UpdateInterface;
end;

end.
