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
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rpprintitem;

interface

{$I rpconf.inc}

uses Sysutils,Classes,rptypes,rpmdconsts,
 rpeval,
{$IFDEF MSWINDOWS}
 Windows,
{$ENDIF}
{$IFDEF USEVARIANTS}
 types,
{$ENDIF}
 rpmetafile;

// Maximum width or height of a element, that is 60 inch
const
 MAX_ELEMENT_WIDTH=86400;
 MAX_ELEMENT_HEIGHT=86400;

type
 TRpPosAlign=(rpalnone,rpalbottom,rpalright,rpalbotright);

 TRpCommonComponent=class(TComponent)
  private
   FHeight:TRpTwips;
   FWidth:TRpTwips;
   FDoBeforePrint,FDoAfterPrint:string;
   FPrintCondition:string;
   procedure SetWidth(Value:TRpTwips);
   procedure SetHeight(Value:TRpTwips);
  protected
   procedure DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);virtual;
  public
   lastextent:TPoint;
   constructor Create(AOwner:TComponent);override;
   function GetExtension(adriver:IRpPrintDriver):TPoint;virtual;
   function EvaluatePrintCondition:boolean;
   procedure Print(aposx,aposy:integer;metafile:TRpMetafileReport);
   procedure SubReportChanged(newstate:TRpReportChanged;newgroup:string='');virtual;
  published
   property PrintCondition:string read FPrintCondition write FPrintCondition;
   property DoBeforePrint:string read FDoBeforePrint write FDoBeforePrint;
   property DoAfterPrint:string read FDoAfterPrint write FDoAfterPrint;
   property Width:TRpTwips read FWidth write SetWidth;
   property Height:TRpTwips read FHeight write SetHeight;
  end;

 TRpCommonPosComponent=class(TRpCommonComponent)
  private
   FPosY:TRpTwips;
   FPosX:TRpTwips;
   FAlign:TRpPosAlign;
  published
   property PosX:TRpTwips read FPosX write FPosX;
   property PosY:TRpTwips read FPosY write FPosY;
   property Align:TRpPosAlign read FAlign write FAlign
    default rpalnone;
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
   FWFontName:widestring;
   FLFontName:widestring;
   FFontSize:smallint;
   FFontRotation:smallint;
   FFontStyle:integer;
   FFontColor:integer;
   FBackColor:integer;
   FTransparent:Boolean;
   FCutText:Boolean;
   FWordWrap:Boolean;
   FAlignMent:integer;
   FVAlignMent:integer;
   FSingleLine:boolean;
   FType1Font:TRpType1Font;
  public
   constructor Create(AOwner:TComponent);override;
  published
   property WFontName:widestring read FWFontName write FWFontName;
   property LFontName:widestring read FLFontName write FLFontName;
   property Type1Font:TRpType1Font read FType1Font write FType1Font;
   property FontSize:smallint read FFontSize write FFontSize default 10;
   property FontRotation:smallint read FFontRotation write FFontRotation default 0;
   property FontStyle:integer read FFontStyle write FFontStyle default 0;
   property FontColor:integer read FFontColor write FFontColor default 0;
   property BackColor:integer read FBackColor write FBackColor default $FFFFFF;
   property Transparent:Boolean read FTransparent write FTransparent default true;
   property CutText:Boolean read FCutText write FCutText default false;
   property Alignment:integer read FAlignment write FAlignment default 0;
   property VAlignment:integer read FVAlignment write FVAlignment default 0;
   property WordWrap:Boolean read FWordWrap write FWordWrap default false;
   property SingleLine:boolean read FSingleLine write FSingleLine default false;
  end;

implementation

uses rpreport,rpsection;

constructor TRpCommonComponent.Create(AOwner:TComponent);
begin
 // The owner must be a report
 if Assigned(AOwner) then
  if (Not (AOwner is TRpReport)) then
   if (Not (AOwner is TRpSection)) then
    Raise Exception.Create(SRpOnlyAReportOwner+classname);

 inherited Create(AOwner);
 FHeight:=0;
 FWidth:=0;
end;

procedure TRpCommonComponent.SetWidth(Value:TRpTwips);
begin
 if Value>MAX_ELEMENT_WIDTH then
  Value:=MAX_ELEMENT_WIDTH;
 if Value<0 then
  Value:=0;
 FWidth:=Value;
end;

procedure TRpCommonComponent.SetHeight(Value:TRpTwips);
begin
 if Value>MAX_ELEMENT_HEIGHT then
  Value:=MAX_ELEMENT_HEIGHT;
 if Value<0 then
  Value:=0;
 FHeight:=Value;
end;

function TRpCommonComponent.GetExtension(adriver:IRpPrintDriver):TPoint;
begin
 Result.X:=Width;
 Result.Y:=Height;
 LastExtent:=Result;
end;

function TRpCommonComponent.EvaluatePrintCondition:boolean;
var
 fevaluator:TRpEvaluator;
begin
 if Length(Trim(PrintCondition))<1 then
 begin
  Result:=true;
  exit;
 end;
 try
  fevaluator:=TRpREport(Owner).Evaluator;
  fevaluator.Expression:=PrintCondition;
  fevaluator.Evaluate;
  Result:=fevaluator.EvalResult;
 except
  on E:Exception do
  begin
   Raise TRpReportException.Create(E.Message+':'+SRpSPrintCondition,self,SRpSPrintCondition);
  end;
 end;
end;


procedure TRpCommonComponent.DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);
begin

end;


procedure TRpCommonComponent.Print(aposx,aposy:integer;metafile:TRpMetafileReport);
var
 fevaluator:TRpEvaluator;
begin
 if Not EvaluatePrintCondition then
  exit;

 // Do Before print and doafter print
 if Length(FDoBeforePrint)>0 then
 begin
  try
   fevaluator:=TRpREport(Owner).Evaluator;
   fevaluator.Expression:=FDoBeforePrint;
   fevaluator.Evaluate;
  except
   on E:Exception do
   begin
    Raise TRpReportException.Create(E.Message+':'+SRpSBeforePrint+' '+Name,self,SRpSBeforePrint);
   end;
  end;
 end;

 DoPrint(aposx,aposy,metafile);

 if Length(FDoAfterPrint)>0 then
 begin
  try
   fevaluator:=TRpREport(Owner).Evaluator;
   fevaluator.Expression:=FDoAfterPrint;
   fevaluator.Evaluate;
  except
   on E:Exception do
   begin
    Raise TRpReportException.Create(E.Message+':'+SRpSAfterPrint+' '+Name,self,SRpSAfterPrint);
   end;
  end;
 end;
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

 FLFontName:='Helvetica';
 FWFontName:='Arial';
 FontSize:=10;
 FontRotation:=0;
 FontStyle:=0;
 FontColor:=0;
 FBackColor:=$FFFFFF;
 FTransparent:=true;
 FCutText:=false;

end;


procedure TRpCommonComponent.SubReportChanged(newstate:TRpReportChanged;newgroup:string='');
begin
 // Base class does nothing
end;

end.
