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
  rpobinsint,QGrids,rpconsts,rpprintitem;

const
  CONS_GRIDHEIGHT=3;
type
  TRpObjGrid=class(TStringGrid)
  public

  end;

  TFObjInsp = class(TFrame)
    StringGrid1: TStringGrid;
  private
    { Private declarations }
    FCompItem:TRpSizeInterface;
    LNames:TStringList;
    LTypes:TStringList;
    LValues:TStringList;
    procedure SetCompItem(Value:TRpSizeInterface);
    procedure GridSetEditText(Sender: TObject; ACol,
     ARow: Integer; const Value: WideString);
  public
    { Public declarations }
    Grid:TRpObjGrid;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property CompItem:TRpSizeInterface read FCompItem write SetCompItem;
  end;



implementation

{$R *.xfm}


procedure TFObjInsp.SetCompItem(Value:TRpSizeInterface);
var
 i:integer;
begin
 FCompItem:=Value;
 if Not Assigned(Value) then
 begin
  Grid.Visible:=false;
  Grid.RowCount:=2;
  exit;
 end;
 Grid.Visible:=false;
 FCompItem.GetProperties(LNames,LTypes,LValues);
 Grid.RowCount:=LNames.Count+1;
 for i:=0 to LNames.Count-1 do
 begin
  Grid.Cells[0,i+1]:=LNames.Strings[i];
  Grid.Cells[1,i+1]:=LValues.Strings[i];
 end;

 Grid.Visible:=true;
end;

constructor TFObjInsp.Create(AOwner:TComponent);
var
 opts:TGridOptions;
begin
 inherited Create(AOwner);

 LNames:=TStringList.Create;
 LValues:=TStringList.Create;
 LTypes:=TStringList.Create;

 Grid:=TRpObjGrid.Create(Self);
 opts:=Grid.Options;
 include(opts,goColSizing);
 include(opts,goEditing);
 include(opts,goAlwaysShowEditor);
 Grid.Options:=opts;
 Grid.ColWidths[0]:=Grid.Canvas.TextWidth('WWWWWWWWWW');
 Grid.ColWidths[1]:=Grid.Canvas.TextWidth('WWWWWWWWWW');
 Grid.FixedRows:=0;
 Grid.Align:=alClient;
 Grid.Visible:=false;
 Grid.ColCount:=2;
 Grid.RowCount:=2;
 Grid.FixedCols:=1;
 Grid.FixedRows:=1;
 Grid.OnSetEditText:=GridSetEditText;
 Grid.Cells[0,0]:=SRpPropName;
 Grid.Cells[1,0]:=SRpPropValue;
 Grid.DefaultRowHeight:=Grid.Canvas.TextHeight('Mg')+CONS_GRIDHEIGHT;
 Grid.Parent:=Self;
end;

destructor TFObjInsp.Destroy;
begin
 LNames.free;
 LValues.free;
 LTypes.free;
 inherited Destroy;
end;

procedure TFObjInsp.GridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: WideString);
var
 propvalue:string;
begin
 // Sets the value

end;


end.
