{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdfchartint                                   }
{       Implementation chart designer                   }
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


unit rpmdfchartint;

interface

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,types,
  rpprintitem,rpmdchart,rpmdobinsint,rpmdconsts,
  rpgraphutils,rptypes,Qt;

type
 TRpChartInterface=class(TRpGenTextInterface)
  private
  protected
   procedure Paint;override;
  public
   class procedure FillAncestors(alist:TStrings);override;
   constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);override;
   procedure GetProperties(lnames,ltypes,lvalues:TStrings);override;
   procedure SetProperty(pname:string;value:Widestring);override;
   function GetProperty(pname:string):Widestring;override;
   procedure GetPropertyValues(pname:string;lpossiblevalues:TStrings);override;
 end;




implementation


constructor TRpChartInterface.Create(AOwner:TComponent;pritem:TRpCommonComponent);
begin
 if Not (pritem is TRpChart) then
  Raise Exception.Create(SRpIncorrectComponentForInterface);
 inherited Create(AOwner,pritem);
end;

class procedure TRpChartInterface.FillAncestors(alist:TStrings);
begin
 inherited FillAncestors(alist);
 alist.Add('TRpChartInterface');
end;

procedure TRpChartInterface.GetProperties(lnames,ltypes,lvalues:TStrings);
begin
 inherited GetProperties(lnames,ltypes,lvalues);

 // Expression
 lnames.Add(SrpSExpression);
 ltypes.Add(SRpSExpression);
 if Assigned(lvalues) then
  lvalues.Add(TRpChart(printitem).ValueExpression);

 // Identifier
 lnames.Add(SrpSIdentifier);
 ltypes.Add(SRpSString);
 if Assigned(lvalues) then
  lvalues.Add(TRpChart(printitem).Identifier);

 // Chart type
// lnames.Add(SrpSChartType);
// ltypes.Add(SRpSList);
 //if Assigned(lvalues) then
//  lvalues.Add(ChartTypeStrings[TRpChart(printitem).ChartType]);

 // GetValue condition
 lnames.Add(SrpSGetValueCondition);
 ltypes.Add(SRpSExpression);
 if Assigned(lvalues) then
  lvalues.Add(TRpChart(printitem).GetValueCondition);

 // Serie Expression
 lnames.Add(SrpSChangeSerieExp);
 ltypes.Add(SRpSExpression);
 if Assigned(lvalues) then
  lvalues.Add(TRpChart(printitem).ChangeSerieExpression);

 // Serie Bool  Expression
 lnames.Add(SrpSChangeSerieBool);
 ltypes.Add(SRpSBool);
 if Assigned(lvalues) then
  lvalues.Add(BoolToStr(TRpChart(printitem).ChangeSerieBool,True));

 // Caption Expression
 lnames.Add(SrpSCaptionExp);
 ltypes.Add(SRpSExpression);
 if Assigned(lvalues) then
  lvalues.Add(TRpChart(printitem).CaptionExpression);

end;

function StringToChartType(Value:string):TRpChartType;
var
 i:TRpChartType;
begin
 Result:=rpchartline;
 for i:=rpchartline to rpchartpoint do
 begin
  if ChartTypeStrings[i]=Value then
  begin
   Result:=i;
   break;
  end;
 end;
end;

procedure TRpChartInterface.SetProperty(pname:string;value:Widestring);
begin
 if length(value)<1 then
  exit;
 if pname=SRpSExpression then
 begin
  TRpChart(fprintitem).ValueExpression:=value;
  exit;
 end;
 if pname=SRpSIdentifier then
 begin
  TRpChart(fprintitem).Identifier:=value;
  exit;
 end;
 if pname=SrpSChartType then
 begin
  TRpChart(fprintitem).ChartType:=StringToChartType(Value);
  invalidate;
  exit;
 end;
 if pname=SrpSGetValueCondition then
 begin
  TRpChart(fprintitem).GetValueCondition:=value;
  exit;
 end;
 if pname=SrpSChangeSerieExp then
 begin
  TRpChart(fprintitem).ChangeSerieExpression:=value;
  exit;
 end;
 if pname=SrpSChangeSerieBool then
 begin
  TRpChart(fprintitem).ChangeSerieBool:=StrToBool(value);
  exit;
 end;
 if pname=SrpSCaptionExp then
 begin
  TRpChart(fprintitem).CaptionExpression:=value;
  exit;
 end;
 inherited SetProperty(pname,value);
end;

function TRpChartInterface.GetProperty(pname:string):Widestring;
begin
 Result:='';
 if pname=SrpSExpression then
 begin
  Result:=TRpChart(printitem).ValueExpression;
  exit;
 end;
 if pname=SrpSIdentifier then
 begin
  Result:=TRpChart(printitem).Identifier;
  exit;
 end;
 if pname=SrpSChartType then
 begin
  Result:=ChartTypeStrings[TRpChart(printitem).ChartType];
  exit;
 end;
 if pname=SrpSGetValueCondition then
 begin
  Result:=TRpChart(printitem).GetValueCondition;
  exit;
 end;
 if pname=SrpSChangeSerieExp then
 begin
  Result:=TRpChart(printitem).ChangeSerieExpression;
  exit;
 end;
 if pname=SrpSChangeSerieBool then
 begin
  Result:=BoolToStr(TRpChart(printitem).ChangeSerieBool,True);
  exit;
 end;
 if pname=SrpSCaptionExp then
 begin
  Result:=TRpChart(printitem).CaptionExpression;
  exit;
 end;
 Result:=inherited GetProperty(pname);
end;

procedure TRpChartInterface.GetPropertyValues(pname:string;
 lpossiblevalues:TStrings);
var
 it:TRpChartType;
begin
 if pname=SRpSChartType then
 begin
  for it:=rpchartline to rpchartpoint do
  begin
   lpossiblevalues.Add(ChartTypeStrings[it]);
  end;
  exit;
 end;
 inherited GetPropertyValues(pname,lpossiblevalues);
end;


procedure TRpChartInterface.Paint;
var
 aexp:TRpChart;
 rec:TRect;
 aalign:integer;
begin
 aexp:=TRpChart(printitem);
 if csDestroying in aexp.ComponentState then
  exit;

 // Draws the text
{$IFDEF MSWINDOWS}
 Canvas.Font.Name:=aexp.WFontName;
{$ENDIF}
{$IFDEF LINUX}
 Canvas.Font.Name:=aexp.LFontName;
{$ENDIF}
 Canvas.Font.Color:=aexp.FontColor;
 Canvas.Font.Size:=aexp.FontSize;
 Canvas.Font.Style:=IntegerToFontStyle(aexp.FontStyle);

 rec.Top:=0;
 rec.Left:=0;
 rec.Right:=Width-1;
 rec.Bottom:=Height-1;
 aalign:=aexp.Alignment or aexp.VAlignment;
 if aexp.SingleLine then
  aalign:=aalign or Integer(AlignmentFlags_SingleLine);
 if aexp.Wordwrap then
  aalign:=aalign or Integer(AlignmentFlags_WordBreak);
 if Not aexp.Transparent then
 begin
  Canvas.TextExtent(aexp.ValueExpression,rec,aalign);
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=aexp.BackColor;
  Canvas.FillRect(rec);
 end;
 Canvas.Brush.Style:=bsClear;
 Canvas.TextRect(rec,0,0,aexp.ValueExpression,aalign);
 Canvas.Pen.Color:=clBlack;
 Canvas.Pen.Style:=psDashDot;
 Canvas.Brush.Style:=bsClear;
 Canvas.Rectangle(0,0,Width,Height);
 DrawSelected;
end;


end.
