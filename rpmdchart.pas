{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpmdchart                                       }
{       TRpChar printable component                     }
{       Is a small chart drawing component              }
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

unit rpmdchart;

interface

{$I rpconf.inc}

uses Classes,SysUtils,Math,rpprintitem,rpmdconsts,rpeval,
 rptypeval,rptypes,
{$IFNDEF USEVARIANTS}
 windows,
{$ENDIF}
{$IFDEF USEVARIANTS}
 Types,
{$ENDIF}
 rpmetafile;

const
 CONS_HORZGAP=1000;
 DEFAULT_ALLOCATION=100;
 DEFAULT_STRINGALLOCATION=2000;
 MAX_SERIECOLORS=7;

type
 TRpChartType=(rpchartline,rpchartbar,rpchartpoint);

 TRpSeriesItem=class(TCollectionItem)
  private
   FValues:array of Double;
   FPoolPositions:array of integer;
   FPoolSizes:array of integer;
   FValueCount:Integer;
   FMinValue:double;
   FMaxValue:double;
   FPool:widestring;
   FPoolPos:integer;
   FMaxAllocated:integer;
   FColor:integer;
   FChangeValue:Variant;
   procedure SetValue(index:integer;AValue:double);
   function GetValue(index:integer):double;
  public
   constructor Create(Collection: TCollection);override;
   property Values[index:integer]:double read GetValue write SetValue;
   property ValueCount:integer read FValueCount;
   procedure AddValue(avalue:double;acaption:widestring='');
   procedure Assign(Source:TPersistent);override;
   procedure ClearValues;
  published
   property Color:integer read FColor write FColor;
 end;

 TRpSeries=class(TCollection)
 private
  function GetItem(Index:Integer):TRpSeriesItem;
  procedure SetItem(index:integer;Value:TRpSeriesItem);
 public
  function Add:TRpSeriesItem;
  property Items[index:integer]:TRpSeriesItem read GetItem write SetItem;default;
 end;

 TIdenRpChart=class;

 TRpChart=class(TRpGenTextComponent)
  private
   FChartType:TRpChartType;
   FValue:Variant;
   FUpdated:Variant;
   FSeries:TRpSeries;
   FGetValuecondition:widestring;
   FValueExpression:widestring;
   FChangeSerieExpression:widestring;
   FChangeSerieBool:boolean;
   FCaptionExpression:widestring;
   FIdenChart:TIdenRpChart;
   FIdentifier:string;
   procedure SetIdentifier(Value:string);
   procedure SetSeries(avalue:TRpSeries);
   function CheckValueCondition:boolean;
   function EvaluateSerieExpression:Variant;
   function EvaluateCaption:Variant;
  protected
   procedure DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);override;
  public
   procedure GetNewValue;
   procedure Evaluate;
   property IdenChart:TIdenRpChart read FIdenChart;
   procedure SubReportChanged(newstate:TRpReportChanged;newgroup:string='');override;
   constructor Create(AOwner:TComponent);override;
  published
   property Series:TRpSeries read FSeries write SetSeries;
   property GetValueCondition:widestring read FGetValueCondition
    write FGetValuecondition;
   property ValueExpression:widestring read FValueExpression
    write FValueExpression;
   property ChangeSerieExpression:widestring read FChangeSerieExpression write
    FChangeSerieExpression;
   property ChangeSerieBool:boolean read FChangeSerieBool write FChangeSerieBool
    default false;
   property CaptionExpression:widestring read FCaptionExpression
    write FCaptionExpression;
   property ChartType:TRpChartType read FChartType write FChartType
    default rpchartline;
   property Identifier:string read FIdentifier write SetIdentifier;
  end;

  TIdenRpChart=class(TIdenFunction)
  private
   FChartitem:TRpChart;
  protected
   function GeTRpValue:TRpValue;override;
  public
  end;

  const ChartTypeStrings:array[rpchartline..rpchartpoint] of string=
   ('Lines','Bars','Points');
  const SeriesColors:Array[0..MAX_SERIECOLORS-1] of integer=
   ($000000,$FF0000,$00FF00,$0000FF,$FFFF00,$FF00FF,$00FFFF);

implementation

uses rpreport;


const
 AlignmentFlags_SingleLine=64;

function TIdenRpChart.GeTRpValue:TRpValue;
begin
 if Not Assigned(FChartItem) then
  Raise Exception.Create(SRpErrorIdenExpression);
 FChartItem.Evaluate;
 Result:=FChartItem.FValue;
end;

function TRpSeries.Add:TRpSeriesItem;
begin
 Result:=TRpSeriesItem(inherited Add);
end;


function TRpSeries.GetItem(Index:Integer):TRpSeriesItem;
begin
 Result:=TRpSeriesItem(inherited GetItem(index));
end;

procedure TRpSeries.SetItem(index:integer;Value:TRpSeriesItem);
begin
 inherited SetItem(Index,Value);
end;

constructor TRpSeriesItem.Create(Collection: TCollection);
begin
 inherited Create(Collection);

 FValueCount:=0;
 SetLength(FValues,DEFAULT_ALLOCATION);
 SetLength(FPoolPositions,DEFAULT_ALLOCATION);
 SetLength(FPoolSizes,DEFAULT_ALLOCATION);
 FMaxAllocated:=DEFAULT_ALLOCATION;
 Fpool:='';
 FPoolPos:=1;
 FMaxValue:=-Power(10,300);
 FMinValue:=+Power(10,300);
end;

procedure TRpSeriesItem.Assign(Source:TPersistent);
var
 aitem:TRpSeriesItem;
begin
 if SOurce is TRpSeriesItem then
 begin
  aitem:=TRpSeriesItem(Source);
  FColor:=aitem.FColor;
 end
 else
  inherited Assign(Source);
end;


procedure TRpSeriesItem.SetValue(index:integer;AValue:double);
begin
 if index>=FValueCount then
  Raise Exception.Create(SRpIndexOutOfBounds+':'+ClassName);
end;

function TRpSeriesItem.GetValue(index:integer):double;
begin
 if index>=FValueCount then
  Raise Exception.Create(SRpIndexOutOfBounds+':'+ClassName);
 Result:=FValues[index];
end;

procedure TRpSeriesItem.ClearValues;
begin
 FValueCount:=0;
 FPoolPos:=1;
 FPool:='';
 FMaxValue:=-Power(10,300);
 FMinValue:=+Power(10,300);
end;


procedure TRpSeriesItem.AddValue(avalue:double;acaption:widestring='');
var
 caplength:integer;
begin
 if FValueCount>=FMaxAllocated then
 begin
  SetLength(FValues,FMaxAllocated*2);
  SetLength(FPoolPositions,FMaxAllocated*2);
  SetLength(FPoolSizes,FMaxAllocated*2);
  FMaxAllocated:=FMaxAllocated*2;
 end;
 FValues[FValueCount]:=avalue;
 // Adds the string
 caplength:=Length(acaption);
 if caplength>0 then
 begin
  FPool:=FPool+acaption;
  FPoolPositions[FValueCount]:=FPoolPos;
  FPoolSizes[FValueCount]:=caplength;
  FPoolPos:=FPoolPos+caplength;
 end
 else
 begin
  FPoolPositions[FValueCount]:=0;
  FPoolSizes[FValueCount]:=0;
 end;
 inc(FValueCount);
 if avalue>FMaxValue then
  FMaxValue:=avalue;
 if avalue<FMinValue then
  FMinValue:=avalue;
end;

constructor TRpChart.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FSeries:=TRpSeries.Create(TRpSeriesItem);
 FChangeSerieBool:=false;
 FIdenChart:=TIdenRpChart.Create(Self);
 FIdenChart.FChartItem:=Self;
end;

procedure TRpChart.SetSeries(avalue:TRpSeries);
begin
 Series.Assign(avalue);
end;

procedure TRpChart.SetIdentifier(Value:string);
var
 fidens:TStringList;
 index:integer;
begin
 if (csloading in componentstate) then
 begin
  FIdentifier:=Value;
  exit;
 end;
 // Check if the identifier is used
 Value:=UpperCase(Trim(Value));
 if Value=FIdentifier then
  exit;
 fidens:=TRpReport(Owner).Identifiers;
 index:=fidens.IndexOf(Value);
 if index>=0 then
  Raise Exception.Create(SRpIdentifierAlreadyExists);
 // Erases previous identifier
 index:=fidens.IndexOf(FIdentifier);
 if index>=0 then
  fidens.Delete(index);
 FIdentifier:=Value;
 if Length(FIdentifier)>0 then
 begin
  fidens.AddObject(FIdentifier,self);
 end;
end;

procedure TRpChart.GetNewValue;
var
 aserie:TRpSeriesItem;
 newvalue:Variant;
 changeserie:boolean;
 caption:widestring;
begin
 if FSeries.Count<1 then
 begin
  aserie:=FSeries.Add;
  newvalue:=EvaluateSerieExpression;
  aserie.FChangeValue:=newvalue;
 end
 else
 begin
  // Looks if the serie has changed
  aserie:=FSeries.Items[FSeries.Count-1];
  newvalue:=EvaluateSerieExpression;
  changeserie:=false;
  if ChangeSerieBool then
  begin
   if newvalue then
    changeserie:=true
  end
  else
  begin
   if aserie.FChangeValue<>newvalue then
    changeserie:=true;
  end;
  if changeserie then
  begin
   aserie:=FSeries.Add;
   aserie.FChangeValue:=newvalue;
  end;
 end;
 // Gests the data
 Evaluate;
 if Length(TRim(FCaptionExpression))<1 then
  Caption:=IntToStr(aserie.ValueCount+1)
 else
  Caption:=EvaluateCaption;
 aserie.AddValue(FValue,Caption);
end;


procedure TRpChart.Evaluate;
var
 fevaluator:TRpEvaluator;
begin
 if FUpdated then
  exit;
 try
  fevaluator:=TRpREport(Owner).Evaluator;
  fevaluator.Expression:=FValueExpression;
  fevaluator.Evaluate;
  FValue:=fevaluator.EvalResult;
  FUpdated:=true;
 except
  on E:Exception do
  begin
   Raise TRpReportException.Create(E.Message+':'+SRpSExpression+' '+Name,self,SRpSExpression);
  end;
 end;
end;


function TRpChart.EvaluateCaption:Variant;
var
 fevaluator:TRpEvaluator;
begin
 if Length(Trim(FCaptionExpression))<1 then
 begin
  Result:='';
  exit;
 end;
 try
  fevaluator:=TRpREport(Owner).Evaluator;
  fevaluator.Expression:=FCaptionExpression;
  fevaluator.Evaluate;
  Result:=fevaluator.EvalResult;
 except
  on E:Exception do
  begin
   Raise TRpReportException.Create(E.Message+':'+SRpSChart+' '+Name,self,SRpSCaptionExp);
  end;
 end;
end;

function TRpChart.EvaluateSerieExpression:Variant;
var
 fevaluator:TRpEvaluator;
begin
 if Length(Trim(ChangeSerieExpression))<1 then
 begin
  Result:=True;
  exit;
 end;
 try
  fevaluator:=TRpREport(Owner).Evaluator;
  fevaluator.Expression:=ChangeSerieExpression;
  fevaluator.Evaluate;
  Result:=fevaluator.EvalResult;
 except
  on E:Exception do
  begin
   Raise TRpReportException.Create(E.Message+':'+SRpSChart+' '+Name,self,SrpSChangeSerieExp);
  end;
 end;
end;

function TRpChart.CheckValueCondition:boolean;
var
 fevaluator:TRpEvaluator;
begin
 if Length(Trim(FGetValuecondition))<1 then
 begin
  Result:=True;
  exit;
 end;
 try
  fevaluator:=TRpREport(Owner).Evaluator;
  fevaluator.Expression:=FGetValuecondition;
  fevaluator.Evaluate;
  Result:=fevaluator.EvalResult;
 except
  on E:Exception do
  begin
   Raise TRpReportException.Create(E.Message+':'+SRpSChart+' '+Name,self,SrpSGetValueCondition);
  end;
 end;
end;


procedure TRpChart.SubReportChanged(newstate:TRpReportChanged;newgroup:string='');
begin
 inherited SubReportChanged(newstate,newgroup);
 case newstate of
  rpReportStart:
   begin
    FUpdated:=false;
    FSeries.Clear;
   end;
  rpDataChange:
   begin
    FUpdated:=false;
    // Gets a value if the condition is true
    if CheckValueCondition then
     GetNewValue;
   end;
 end;
end;


procedure TRpChart.DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);
{var
 aText:WideString;
 expre:WideString;
}
var
 i:integer;
 Maxvalue,Minvalue:double;
 aserie:TRpSeriesItem;
 gridvsep:integer;
 horzgap,vertgap:integer;
 numvlabels:integer;
 valueinc:double;
 posy:integer;
 avalue:double;
 aText:WideString;
 aalign:integer;
 xdesp:integer;
 origin,destination:TPoint;
 j:integer;
 shape:TRpShapeType;
 acolor:integer;
 pencolor:integer;
begin
{

 expre:=Trim(Expression);
 aText:=GetText;
 metafile.Pages[metafile.CurrentPage].NewTextObject(aposy,
   aposx,width,height,aText,WFontName,LFontName,FontSize,FontRotation,
   FontStyle,smallint(Type1Font),FOntColor,BackColor,Transparent,CutText,aalign,WordWrap);
}
 if FSeries.Count<1 then
  exit;
 aalign:=Alignment or VAlignment;
 if SingleLine then
  aalign:=aalign or AlignmentFlags_SingleLine;
 // To draw for each serie find macvalue and minvalue
 MaxValue:=-Power(10,300);
 MinValue:=+Power(10,300);
 for i:=0 to FSeries.Count-1 do
 begin
  aserie:=FSeries.Items[i];
  if aserie.FMaxValue>MaxValue then
   MaxValue:=aserie.FMaxValue;
  if aserie.FMinValue<MinValue then
   MinValue:=aserie.FMinValue;
 end;
 // The number of grid rows depends on font height
 gridvsep:=Round(FontSize/72*1440*2);
 vertgap:=Round(FontSize/72*1440*1.5);
 horzgap:=CONS_HORZGAP;
 // Draws coordinate system
 metafile.Pages[metafile.CurrentPage].NewDrawObject(aposy,aposx+horzgap,1,Height-vertgap,
  integer(rpsVertLine),0,0,0,0,0);
 // Draws coordinate system
 metafile.Pages[metafile.CurrentPage].NewDrawObject(aposy+Height-vertgap,aposx+horzgap,Width-horzgap,1,
  integer(rpsHorzLine),0,0,0,0,0);
 // Draw Texts for scales
 numvlabels:=(Height-vertgap) div gridvsep;
 // Value relation
 valueinc:=gridvsep*(MaxValue-MinValue)/(Height-vertgap);
 avalue:=MinValue;
 for i:=0 to numvlabels do
 begin
  posy:=Height-vertgap-i*gridvsep;
  // Draw the line
  metafile.Pages[metafile.CurrentPage].NewDrawObject(aposy+posy,aposx+horzgap,Width-horzgap,1,
   integer(rpsHorzLine),0,0,1,0,0);
  // Draw the caption
  aText:=FormatFloat('########0.00',avalue);
  metafile.Pages[metafile.CurrentPage].NewTextObject(aposy+posy-(gridvsep div 4),
   aposx,horzgap,gridvsep,aText,WFontName,LFontName,FontSize,FontRotation,
   FontStyle,smallint(Type1Font),FOntColor,BackColor,Transparent,CutText,aalign,WordWrap);
  avalue:=avalue+valueinc;
 end;
 // Draws the lines
 acolor:=0;
 for i:=0 to Series.Count-1 do
 begin
  aserie:=Series.Items[i];
  pencolor:=SeriesColors[acolor];
  if ASerie.FMinValue<>ASerie.FMaxValue then
  begin
   xdesp:=Round((Width-horzgap)/(aserie.FValueCount));
   for j:=0 to aserie.FValueCount-1 do
   begin
    if j=0 then
    begin
     origin.X:=horzgap;
     origin.Y:=Height-vertgap-Round((aserie.Values[j]-aserie.FMinValue)/(aserie.FMaxValue-aserie.FMinValue)*(Height-vertgap));
    end;
    destination.X:=origin.X+xdesp;
    destination.Y:=Height-vertgap-Round((aserie.Values[j]-aserie.FMinValue)/(aserie.FMaxValue-aserie.FMinValue)*(Height-vertgap));
    if destination.Y>origin.Y then
    begin
     shape:=rpsOblique1;
     // Draw the line
      metafile.Pages[metafile.CurrentPage].NewDrawObject(aposy+origin.Y,aposx+origin.X,xdesp,Destination.Y-Origin.Y,
       integer(shape),0,0,0,0,pencolor);
    end
    else
    begin
     if destination.Y<origin.Y then
     begin
      shape:=rpsOblique2;
      metafile.Pages[metafile.CurrentPage].NewDrawObject(aposy+origin.Y-(origin.Y-destination.Y),aposx+origin.X,xdesp,Origin.Y-Destination.Y,
       integer(shape),0,0,0,0,pencolor);
     end
     else
     begin
      shape:=rpsHorzLine;
      metafile.Pages[metafile.CurrentPage].NewDrawObject(aposy+origin.Y,aposx+origin.X,xdesp,1,
       integer(shape),0,0,0,0,pencolor);
     end;
    end;
    Origin:=Destination;
   end;
  end;
  acolor:=((acolor+1) mod MAX_SERIECOLORS);
 end;
end;


end.
