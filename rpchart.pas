unit rpchart;

interface

uses Classes,SysUtils,Math;


resourcestring

 SRpIndexOutOfBounds='Index out of bounds';

const
 DEFAULT_ALLOCATION=100;
 DEFAULT_STRINGALLOCATION=2000;

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

 TRpChart=class(TComponent)
  private
   FChartType:TRpChartType;
   FBackColor:integer;
   FTransparent:boolean;
   FSeries:TRpSeries;
   FGetValuecondition:widestring;
   FValueExpression:widestring;
   FChangeSerieExpression:widestring;
   FChangeSerieBool:boolean;
   FCaptionExpression:widestring;
   procedure SetSeries(avalue:TRpSeries);
  public
   constructor Create(AOwner:TComponent);override;
  published
   property BackColor:integer read FBackColor write FBackColor default $FFFFFF;
   property Transparent:boolean read FTransparent write FTransparent default true;
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
  end;

implementation


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
 FMaxValue:=Power(10,300);
 FMinValue:=-Power(10,300);
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
 FMaxValue:=Power(10,300);
 FMinValue:=-Power(10,300);
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
 FBackColor:=$FFFFFF;
 FTransparent:=True;
 FSeries:=TRpSeries.Create(TRpSeriesItem);
 FChangeSerieBool:=false;
end;

procedure TRpChart.SetSeries(avalue:TRpSeries);
begin
 Series.Assign(avalue);
end;



end.
