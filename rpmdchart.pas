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
 rptypeval,rptypes,rpevalfunc,rpmunits,
{$IFNDEF USEVARIANTS}
 windows,
{$ENDIF}
{$IFDEF USEVARIANTS}
 Types,
{$ENDIF}
{$IFDEF USETEECHART}
 Chart,Series,rpdrawitem,
 teEngine,ArrowCha,BubbleCh,GanttCh,
 {$IFDEF MSWINDOWS}
   rpgraphutilsvcl,rpvgraphutils,Graphics,
 {$ENDIF}
 {$IFDEF LINUX}
   rpgraphutils,QGraphics,
 {$ENDIF}
{$ENDIF}
 rpmetafile;

const
 CONS_HORZGAP=1000;
 DEFAULT_ALLOCATION=100;
 DEFAULT_STRINGALLOCATION=2000;
 MAX_SERIECOLORS=7;


type
 TRpChartType=(rpchartline,rpchartbar,rpchartpoint,
  rpcharthorzbar,rpchartarea,rpchartpie,rpchartarrow,
  rpchartbubble,rpchartgantt);

 TRpChartDriver=(rpchartdriverdefault,rpchartdriverengine,rpchartdriverteechart);

 TRpMultiBar=(rpMultiNone,rpMultiside,rpMultiStacked,
  rpMultiStacked100);

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
   function GetValueCaption(index:integer):WideString;
  public
   constructor Create(Collection: TCollection);override;
   property Values[index:integer]:double read GetValue write SetValue;
   property ValueCaptions[index:integer]:WideString read GetValueCaption;
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


 TRpChart=class(TRpGenTextComponent)
  private
   FChartType:TRpChartType;
   FValue:Variant;
   FUpdated:Variant;
   FSeries:TRpSeries;
   FGetValuecondition:widestring;
   FValueExpression:widestring;
   FChangeSerieExpression:widestring;
   FChangeSerieBool,FClearExpressionBool:Boolean;
   FCaptionExpression,FSerieCaption,FClearExpression:widestring;
   FIdenChart:TVariableGrap;
   FIdentifier:string;
   FDriver:TRpChartDriver;
   FView3d:Boolean;
   FView3dWalls:Boolean;
   FPerspective:Integer;
   FElevation:Integer;
   FRotation:Integer;
   FOrthogonal:Boolean;
   FZoom:Integer;
   FHorzOffset:Integer;
   FVertOffset:Integer;
   FTilt:Integer;
   FMultiBar:TRpMultiBar;
   procedure OnClear(Sender:TObject);
   procedure OnNewValue(Y:Single;Cambio:Boolean;leyen,textleyen:string);
   procedure SetIdentifier(Value:string);
   procedure SetSeries(avalue:TRpSeries);
   function CheckValueCondition:boolean;
   function EvaluateSerieExpression:Variant;
   function EvaluateCaption:Variant;
   procedure WriteGetValueCondition(Writer:TWriter);
   procedure ReadGetValueCondition(Reader:TReader);
   procedure WriteValueExpression(Writer:TWriter);
   procedure ReadValueExpression(Reader:TReader);
   procedure WriteChangeSerieExpression(Writer:TWriter);
   procedure ReadChangeSerieExpression(Reader:TReader);
   procedure WriteCaptionExpression(Writer:TWriter);
   procedure ReadCaptionExpression(Reader:TReader);
   procedure WriteSerieCaption(Writer:TWriter);
   procedure ReadSerieCaption(Reader:TReader);
   procedure WriteClearExpression(Writer:TWriter);
   procedure ReadClearExpression(Reader:TReader);
{$IFDEF USETEECHART}
   procedure DoPrintTeeChart(aposx,aposy:integer;metafile:TRpMetafileReport);
{$ENDIF}
  protected
   procedure DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);override;
   procedure DefineProperties(Filer:TFiler);override;
  public
   procedure GetNewValue;
   procedure Evaluate;
   property IdenChart:TVariableGrap read FIdenChart;
   procedure SubReportChanged(newstate:TRpReportChanged;newgroup:string='');override;
   constructor Create(AOwner:TComponent);override;
   property ChangeSerieExpression:widestring read FChangeSerieExpression write
    FChangeSerieExpression;
   property GetValueCondition:widestring read FGetValueCondition
    write FGetValuecondition;
   property ValueExpression:widestring read FValueExpression
    write FValueExpression;
   property CaptionExpression:widestring read FCaptionExpression
    write FCaptionExpression;
   property SerieCaption:widestring read FSerieCaption
    write FSerieCaption;
   property ClearExpression:widestring read FClearExpression
    write FClearExpression;
  published
   property Series:TRpSeries read FSeries write SetSeries;
   property ChangeSerieBool:boolean read FChangeSerieBool write FChangeSerieBool
    default false;
   property ChartType:TRpChartType read FChartType write FChartType
    default rpchartline;
   property Identifier:string read FIdentifier write SetIdentifier;
   property ClearExpressionBool:boolean read FClearExpressionBool write FClearExpressionBool
    default false;
   property Driver:TRpChartDriver read FDriver write FDriver;
   property View3d:Boolean read FView3d write FView3d default true;
   property View3dWalls:Boolean read FView3dWalls write FView3dWalls default true;
   property Perspective:Integer read FPerspective write FPersPective default 15;
   property Elevation:Integer read FElevation write FElevation default 345;
   property Rotation:Integer read FRotation write FRotation default 345;
   property Zoom:Integer read FZoom write FZoom default 100;
   property HorzOffset:Integer read FHorzOffset write FHorzOffset default 0;
   property VertOffset:Integer read FVertOffset write FVertOffset default 0;
   property Tilt:Integer read FTilt write FTilt default 0;
   property Orthogonal:Boolean read FOrthogonal write FOrthogonal default true;
   property MultiBar:TRpMultiBar read FMultiBar write FMultiBar
    default rpMultiside;
  end;


  const SeriesColors:Array[0..MAX_SERIECOLORS-1] of integer=
   ($000000,$FF0000,$00FF00,$0000FF,$FFFF00,$FF00FF,$00FFFF);

function RpChartTypeToString(charttype:TRpChartType):String;
function StringToRpChartType(Value:String):TRpChartType;
procedure GetRpChartTypePossibleValues(alist:TStrings);

function RpChartDriverToString(driver:TRpChartDriver):String;
function StringToRpChartDriver(Value:String):TRpChartDriver;
procedure GetRpChartDriverPossibleValues(alist:TStrings);

function RpMultiBarToString(multibar:TRpMultibar):String;
function StringToRpMultibar(Value:String):TRpMultiBar;
procedure GetRpMultiBarPossibleValues(alist:TStrings);


implementation

uses rpreport;


const
 AlignmentFlags_SingleLine=64;


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

function TRpSeriesItem.GetValueCaption(index:integer):WideString;
begin
 if index>=FValueCount then
  Raise Exception.Create(SRpIndexOutOfBounds+':'+ClassName);
 Result:=Copy(FPool,FPoolPositions[index],FPoolSizes[index]);
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
 FIdenChart:=TVariableGrap.Create(Self);
 FIdenChart.OnClear:=OnClear;
 FIdenChart.OnNewValue:=OnNewValue;
 FView3d:=true;
 FPerspective:=15;
 FElevation:=345;
 FRotation:=345;
 FZoom:=100;
 FOrthogonal:=True;
 FMultiBar:=rpMultiside;
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
 fidens:=TRpReport(GetReport).Identifiers;
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
  fevaluator:=TRpREport(GetReport).Evaluator;
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
  fevaluator:=TRpREport(GetReport).Evaluator;
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
  fevaluator:=TRpREport(GetReport).Evaluator;
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
  fevaluator:=TRpREport(GetReport).Evaluator;
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


{$IFDEF USETEECHART}
procedure TRpChart.DoPrintTeeChart(aposx,aposy:integer;metafile:TRpMetafileReport);
var
 achart:TChart;
 aserie:TChartSeries;
 i,j:integer;
 rec:TRect;
 intserie:TRpSeriesItem;
 abitmap:TBitmap;
 FMStream:TMemoryStream;
 acolor:integer;
begin
 achart:=TChart.Create(nil);
 try
  achart.Legend.Visible:=false;
  achart.View3D:=FView3d;
  achart.View3DOptions.Rotation:=FRotation;
  achart.View3DOptions.Perspective:=FPerspective;
  achart.View3DOptions.Elevation:=FElevation;
  achart.View3DOptions.Orthogonal:=FOrthogonal;
  achart.View3DOptions.Zoom:=FZoom;
  achart.View3DOptions.Tilt:=FTilt;
  achart.View3DOptions.HorizOffset:=FHorzOffset;
  achart.View3DOptions.VertOffset:=FVertOffset;
  achart.View3DWalls:=FView3DWalls;
  achart.BackColor:=clTeeColor;
  achart.BackWall.Brush.Style:=bsClear;
  achart.Gradient.Visible:=false;
  achart.Color:=clWhite;
{$IFDEF MSWINDOWS}
  achart.LeftAxis.LabelsFont.Name:=WFontName;
  achart.BottomAxis.LabelsFont.Name:=WFontName;
{$ENDIF}
{$IFDEF MSWINDOWS}
  achart.LeftAxis.LabelsFont.Name:=LFontName;
  achart.BottomAxis.LabelsFont.Name:=LFontName;
{$ENDIF}
  achart.LeftAxis.LabelsFont.Size:=FontSize;
  // Convert to degrees first
  achart.LeftAxis.LabelsAngle:=Abs(FontRotation div 10) mod 360;
{$IFDEF MSWINDOWS}
  achart.LeftAxis.LabelsFont.Style:=CLXIntegerToFontStyle(FontStyle);
{$ENDIF}
{$IFDEF LINUX}
  achart.LeftAxis.LabelsFont.Style:=IntegerToFontStyle(FontStyle);
{$ENDIF}
  achart.BottomAxis.LabelsFont.Size:=FontSize;
  // Convert to degrees first
  achart.BottomAxis.LabelsAngle:=Abs(FontRotation div 10) mod 360;
{$IFDEF MSWINDOWS}
  achart.BottomAxis.LabelsFont.Style:=CLXIntegerToFontStyle(FontStyle);
{$ENDIF}
{$IFDEF LINUX}
  achart.BottomAxis.LabelsFont.Style:=IntegerToFontStyle(FontStyle);
{$ENDIF}

  acolor:=1;
  for i:=0 to Series.Count-1 do
  begin
   aserie:=nil;
   case ChartType of
    rpchartline:
     begin
      aserie:=TLineSeries.Create(nil);
     end;
    rpchartbar:
     begin
      aserie:=TBarSeries.Create(nil);
      case FMultiBar of
       rpMultiNone:
        TBarSeries(aserie).MultiBar:=mbNone;
       rpMultiside:
        TBarSeries(aserie).MultiBar:=mbSide;
       rpMultiStacked:
        TBarSeries(aserie).MultiBar:=mbStacked;
       rpMultiStacked100:
        TBarSeries(aserie).MultiBar:=mbStacked100;
      end;
     end;
    rpchartpoint:
     aserie:=TPointSeries.Create(nil);
    rpcharthorzbar:
     aserie:=THorizBarSeries.Create(nil);
    rpchartarea:
     aserie:=TAreaSeries.Create(nil);
    rpchartpie:
     aserie:=TPieSeries.Create(nil);
    rpchartarrow:
     aserie:=TArrowSeries.Create(nil);
    rpchartbubble:
     aserie:=TBubbleSeries.Create(nil);
    rpchartgantt:
     aserie:=TGanttSeries.Create(nil);
   end;
   if not assigned(aserie) then
    exit;
   aserie.ParentChart:=achart;
   // Assigns the color for this serie
   intserie:=Series.Items[i];
   for j:=0 to intserie.FValueCount-1 do
   begin
    if ChartType=rpchartpie then
     aserie.Add(intserie.Values[j],
      intSerie.ValueCaptions[j],SeriesColors[aColor])
    else
    aserie.Add(intserie.Values[j],
     intSerie.ValueCaptions[j]);
    if series.count<2 then
    begin
     if ChartType=rpchartpie then
      acolor:=((acolor+1) mod (MAX_SERIECOLORS-1))+1;
    end;
   end;
   abitmap:=TBitmap.Create;
   try
{$IFDEF MSWINDOWS}
    abitmap.HandleType:=bmDIB;
    abitmap.PixelFormat:=pf24bit;
{$ENDIF}
{$IFDEF LINUX}
    abitmap.PixelFormat:=pf32bit;
{$ENDIF}
    // Chart resolution to default screen
    abitmap.Width:=Round(twipstoinchess(Width)*twipstopixels(TWIPS_PER_INCHESS));
    abitmap.Height:=Round(twipstoinchess(Height)*twipstopixels(TWIPS_PER_INCHESS));
    rec.Top:=0;
    rec.Left:=0;
    rec.Bottom:=abitmap.Height-1;
    rec.Right:=abitmap.Width-1;
    achart.Draw(abitmap.Canvas,rec);
    // Finally print it
    FMStream:=TMemoryStream.Create;
    try
     abitmap.SaveToStream(FMStream);
     metafile.Pages[metafile.CurrentPage].NewImageObject(aposy,aposx,
      Width,Height,DEF_COPYMODE,Integer(rpDrawStretch),
      Integer(DEFAULT_DPI),FMStream);
    finally
     FMStream.Free;
    end;
   finally
    abitmap.free;
   end;
   acolor:=((acolor+1) mod MAX_SERIECOLORS);
  end;
 finally
  for i:=0 to achart.SeriesList.Count-1 do
  begin
   TObject(achart.SeriesList.Items[i]).free;
  end;
  achart.Free;
 end;
end;
{$ENDIF}

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
 MaxValueCount:integer;
begin
 inherited DoPrint(aposx,aposy,metafile);
 if FSeries.Count<1 then
  exit;
 // Draws a TeeChart
{$IFDEF USETEECHART}
 if FDriver<>rpchartdriverengine then
 begin
  DoPrintTeeChart(aposx,aposy,metafile);
  exit;
 end;
{$ENDIF}
 aalign:=Alignment or VAlignment;
 if SingleLine then
  aalign:=aalign or AlignmentFlags_SingleLine;
 // To draw for each serie find macvalue and minvalue
 MaxValue:=-Power(10,300);
 MinValue:=+Power(10,300);
 MaxValueCount:=0;
 for i:=0 to FSeries.Count-1 do
 begin
  aserie:=FSeries.Items[i];
  if aserie.FMaxValue>MaxValue then
   MaxValue:=aserie.FMaxValue;
  if aserie.FMinValue<MinValue then
   MinValue:=aserie.FMinValue;
  if aserie.FValueCount>MaxValueCount then
   MaxValueCount:=aserie.FValueCount;
 end;
 // The number of grid rows depends on font height
 gridvsep:=Round(FontSize/POINTS_PER_INCHESS*TWIPS_PER_INCHESS*2);
 vertgap:=Round(FontSize/POINTS_PER_INCHESS*TWIPS_PER_INCHESS*1.5);
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
 xdesp:=Round((Width-horzgap)/(MaxValueCount));
 for i:=0 to Series.Count-1 do
 begin
  aserie:=Series.Items[i];
  pencolor:=SeriesColors[acolor];
  if ASerie.FMinValue<>ASerie.FMaxValue then
  begin
   for j:=0 to aserie.FValueCount-1 do
   begin
    if j=0 then
    begin
     origin.X:=horzgap;
     origin.Y:=Height-vertgap-Round((aserie.Values[j]-MinValue)/(MaxValue-MinValue)*(Height-vertgap));
    end;
    destination.X:=origin.X+xdesp;
    destination.Y:=Height-vertgap-Round((aserie.Values[j]-MinValue)/(MaxValue-MinValue)*(Height-vertgap));
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

procedure TRpChart.OnNewValue(Y:Single;Cambio:Boolean;leyen,textleyen:string);
var
 aserie:TRpSeriesItem;
begin
 if FSeries.Count<1 then
 begin
  aserie:=FSeries.Add;
 end
 else
 begin
  aserie:=FSeries.Items[FSeries.Count-1];
 end;
  // Looks if the serie has changed
 if Cambio then
 begin
  aserie:=FSeries.Add;
 end;
 aserie.AddValue(Y,leyen);
end;

procedure TRpChart.OnClear(Sender:TObject);
begin
 Series.Clear;
end;

procedure TRpChart.WriteGetValueCondition(Writer:TWriter);
begin
 WriteWideString(Writer, FGetValueCondition);
end;

procedure TRpChart.ReadGetValueCondition(Reader:TReader);
begin
 FGetValueCondition:=ReadWideString(Reader);
end;

procedure TRpChart.WriteValueExpression(Writer:TWriter);
begin
 WriteWideString(Writer, FValueExpression);
end;

procedure TRpChart.ReadValueExpression(Reader:TReader);
begin
 FValueExpression:=ReadWideString(Reader);
end;

procedure TRpChart.WriteChangeSerieExpression(Writer:TWriter);
begin
 WriteWideString(Writer, FChangeSerieExpression);
end;

procedure TRpChart.ReadChangeSerieExpression(Reader:TReader);
begin
 FChangeSerieExpression:=ReadWideString(Reader);
end;

procedure TRpChart.WriteCaptionExpression(Writer:TWriter);
begin
 WriteWideString(Writer, FCaptionExpression);
end;

procedure TRpChart.ReadCaptionExpression(Reader:TReader);
begin
 FCaptionExpression:=ReadWideString(Reader);
end;

procedure TRpChart.WriteSerieCaption(Writer:TWriter);
begin
 WriteWideString(Writer, FSerieCaption);
end;

procedure TRpChart.ReadSerieCaption(Reader:TReader);
begin
 FSerieCaption:=ReadWideString(Reader);
end;

procedure TRpChart.WriteClearExpression(Writer:TWriter);
begin
 WriteWideString(Writer, FClearExpression);
end;

procedure TRpChart.ReadClearExpression(Reader:TReader);
begin
 FClearExpression:=ReadWideString(Reader);
end;

procedure TRpChart.DefineProperties(Filer:TFiler);
begin
 inherited;

 Filer.DefineProperty('GetValueCondition',ReadGetValueCondition,WriteGetValueCondition,True);
 Filer.DefineProperty('ValueExpression',ReadValueExpression,WriteValueExpression,True);
 Filer.DefineProperty('ChangeSerieExpression',ReadChangeSerieExpression,WriteChangeSerieExpression,True);
 Filer.DefineProperty('CaptionExpression',ReadCaptionExpression,WriteCaptionExpression,True);
 Filer.DefineProperty('SerieCaption',ReadSerieCaption,WriteSerieCaption,True);
 Filer.DefineProperty('ClearExpression',ReadClearExpression,WriteClearExpression,True);
end;

function RpChartDriverToString(driver:TRpChartDriver):String;
begin
 case driver of
  rpchartdriverdefault:
   Result:=SRpSDefault;
  rpchartdriverengine:
   Result:=SRpSChartDriverEngine;
  rpchartdriverteechart:
   Result:=SRpSChartDriverTeeChart;
 end;
end;

procedure GetRpChartDriverPossibleValues(alist:TStrings);
begin
 alist.Clear;
 alist.Add(SRpSDefault);
 alist.Add(SRpSChartDriverEngine);
 alist.Add(SRpSChartDriverTeeChart);
end;

function StringToRpChartDriver(Value:String):TRpChartDriver;
begin
 Result:=rpchartdriverdefault;
 if Value=SRpSChartDriverEngine then
 begin
  Result:=rpchartdriverengine;
  exit;
 end;
 if Value=SRpSChartDriverTeeChart then
 begin
  Result:=rpchartdriverteechart;
  exit;
 end;
end;

function RpChartTypeToString(charttype:TRpChartType):String;
begin
 Result:=SRpChartLine;
 case charttype of
  rpchartline:
   Result:=SRpChartLine;
  rpchartbar:
   Result:=SRpChartBar;
  rpchartpoint:
   Result:=SRpChartPoint;
  rpcharthorzbar:
   Result:=SRpChartHorzBar;
  rpchartarea:
   Result:=SRpChartArea;
  rpchartpie:
   Result:=SRpChartPie;
  rpchartarrow:
   Result:=SRpChartArrow;
  rpchartgantt:
   Result:=SRpChartgantt;
  rpchartbubble:
   Result:=SRpChartBubble;
 end;
end;

function StringToRpChartType(Value:String):TRpChartType;
begin
 Result:=rpchartline;
 if Value=SRpChartBar then
 begin
  Result:=rpchartbar;
  exit;
 end;
 if Value=SRpChartPoint then
 begin
  Result:=rpchartpoint;
  exit;
 end;
 if Value=SRpChartHorzBar then
 begin
  Result:=rpcharthorzbar;
  exit;
 end;
 if Value=SRpChartArea then
 begin
  Result:=rpchartarea;
  exit;
 end;
 if Value=SRpChartPie then
 begin
  Result:=rpchartpie;
  exit;
 end;
 if Value=SRpChartArrow then
 begin
  Result:=rpchartarrow;
  exit;
 end;
 if Value=SRpChartBubble then
 begin
  Result:=rpchartbubble;
  exit;
 end;
 if Value=SRpChartgantt then
 begin
  Result:=rpchartgantt;
  exit;
 end;
end;

procedure GetRpChartTypePossibleValues(alist:TStrings);
begin
 alist.Clear;
 alist.add(SRpChartLine);
 alist.Add(SRpChartBar);
 alist.Add(SRpChartPoint);
 alist.Add(SRpChartHorzBar);
 alist.Add(SRpChartArea);
 alist.Add(SRpChartPie);
 alist.Add(SRpChartArrow);
 alist.Add(SRpChartBubble);
 alist.Add(SRpChartGantt);
end;


function RpMultiBarToString(multibar:TRpMultibar):String;
begin
 Result:=SRPSNone;
 case multibar of
  rpMultiNone:
   Result:=SRPSNone;
  rpMultiside:
   Result:=SRPSSide;
  rpMultiStacked:
   Result:=SRPSStacked;
  rpMultiStacked100:
   Result:=SRPSStacked100;
 end;
end;

function StringToRpMultibar(Value:String):TRpMultiBar;
begin
 Result:=rpMultiNone;
 if Value=SRPSSide then
 begin
  Result:=rpMultiSide;
  exit;
 end;
 if Value=SRPSStacked then
 begin
  Result:=rpMultiStacked;
  exit;
 end;
 if Value=SRPSStacked100 then
 begin
  Result:=rpMultiStacked100;
  exit;
 end;
end;

procedure GetRpMultiBarPossibleValues(alist:TStrings);
begin
 alist.Clear;
 alist.Add(SRpSNone);
 alist.Add(SRpSSide);
 alist.Add(SRpSStacked);
 alist.Add(SRpSStacked100);
end;

end.

