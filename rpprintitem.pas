{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpprintitem                                     }
{       TRpPrintItem: Base class for printable comps    }
{       TRpGenTextItem: Base class for text items       }
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

unit rpprintitem;

interface

uses Sysutils,Classes,rptypes,rpconsts;

type

 TRpCommonComponent=class(TComponent)
  private
   FHeight:TRpTwips;
   FWidth:TRpTwips;
   FDoBeforePrint,FDoAfterPrint:string;
   FPrintCondition:string;
  public
   constructor Create(AOwner:TComponent);override;
   property PrintCondition:string read FPrintCondition write FPrintCondition;
   property DoBeforePrint:string read FDoBeforePrint write FDoBeforePrint;
   property DoAfterPrint:string read FDoAfterPrint write FDoAfterPrint;
  published
   property Width:TRpTwips read FWidth write FWidth;
   property Height:TRpTwips read FHeight write FHeight;
  end;

 TRpCommonPosComponent=class(TRpCommonComponent)
  private
   FPosY:TRpTwips;
   FPosX:TRpTwips;
  published
   property PosX:TRpTwips read FPosX write FPosX;
   property PosY:TRpTwips read FPosY write FPosY;
  end;

 TRpCommonListItem=class(TCollectionItem)
  private
   FComponent:TRpCommonComponent;
   procedure SetComponent(Value:TRpCommonComponent);
  public
   procedure Assign(Source:TPersistent);override;
  published
   property Component:TRpCommonComponent read FComponent write SetComponent;
 end;


 TRpCommonList=class(TCollection)
  private
   FSection:TComponent;
   function GetItem(Index:Integer):TRpCommonListItem;
   procedure SetItem(index:integer;Value:TRpCommonListItem);
  public
   function Add:TRpCommonListItem;
   function Insert(index:integer):TRpCommonListItem;
   function IndexOf(Value:TRpCommonComponent):integer;
   property Items[index:integer]:TRpCommonListItem read GetItem write SetItem;default;
   constructor Create(sec:TComponent);
 end;

 TRpGenTextComponent=class(TRpCommonPosComponent)
  private
   FFontName:widestring;
   FFontSize:integer;
   FFontStyle:integer;
   FFontColor:integer;
   FBackColor:integer;
   FTransparent:Boolean;
  public
   constructor Create(AOwner:TComponent);override;
  published
   property FontName:widestring read FFontName write FFontName;
   property FontSize:integer read FFontSize write FFontSize default 10;
   property FontStyle:integer read FFontStyle write FFontStyle default 0;
   property FontColor:integer read FFontColor write FFontColor default 0;
   property BackColor:integer read FBackColor write FBackColor default $FFFFFF;
   property Transparent:Boolean read FTransparent write FTransparent default true;

  end;

implementation

uses rpreport;

constructor TRpCommonComponent.Create(AOwner:TComponent);
begin
 // The owner must be a report
 if Assigned(AOwner) then
  if (Not (AOwner is TRpReport)) then
   Raise Exception.Create(SRpOnlyAReportOwner+classname);

 inherited Create(AOwner);
 FHeight:=0;
 FWidth:=0;
end;

constructor TrpCOmmonList.Create(sec:TComponent);
begin
 inherited Create(TRpCommonListItem);
 FSection:=sec;
end;

procedure TRpCommonListItem.SetComponent(Value:TRpCommonComponent);
begin
 FComponent:=Value;
 Changed(False);
end;

function TRpCommonList.GetItem(Index:Integer):TRpCommonListItem;
begin
 Result:=TRpCommonListItem(inherited GetItem(index));
end;

procedure TRpCommonList.SetItem(index:integer;Value:TRpCommonListItem);
begin
 inherited SetItem(Index,Value);
end;


procedure TRpCommonListItem.Assign(Source:TPersistent);
begin
 if Source is TRpCommonListItem then
 begin
  FComponent:=TRpCommonListItem(Source).FComponent;
 end
 else
  inherited Assign(Source);
end;

function TRpCommonList.Add:TRpCommonListItem;
begin
 Result:=TRpCommonListItem(inherited Add);
end;

function TRpCommonList.Insert(index:integer):TRpCommonListItem;
begin
 Result:=TRpCommonListItem(inherited Insert(index));
end;


function TRpCommonList.IndexOf(Value:TRpCommonComponent):integer;
var
 i:integer;
begin
 Result:=-1;
 i:=0;
 While i<count do
 begin
  if items[i].FComponent=Value then
  begin
   Result:=i;
   break;
  end;
  inc(i);
 end;
end;

constructor TRpGenTextComponent.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FontName:='Helvetica';
 FontSize:=10;
 FontStyle:=0;
 FontColor:=0;
 FBackColor:=$FFFFFF;
 FTransparent:=true;

end;

end.
