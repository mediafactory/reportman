{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdflabelint                                   }
{       Implementation label and expression designer    }
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


unit rpmdflabelint;

interface

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,types,
  rpprintitem,rplabelitem,rpmdobinsint,rpconsts,
  rpgraphutils,rptypes,Qt;

type
 TRpLabelInterface=class(TRpGenTextInterface)
  private
  protected
   procedure Paint;override;
  public
   constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);override;
   procedure GetProperties(lnames,ltypes,lvalues:TStrings);override;
   procedure SetProperty(pname:string;value:string);override;
   function GetProperty(pname:string):string;override;
 end;

 TRpExpressionInterface=class(TRpGenTextInterface)
  private
  protected
   procedure Paint;override;
  public
   constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);override;
   procedure GetProperties(lnames,ltypes,lvalues:TStrings);override;
   procedure SetProperty(pname:string;value:string);override;
   function GetProperty(pname:string):string;override;
   procedure GetPropertyValues(pname:string;lpossiblevalues:TStrings);override;
 end;




implementation

const
 AggregatesString:array [rpAgNone..rpAgGeneral] of string=
  (SRpNone,SRpGroup,SRpPage,SRpGeneral);
 AggretypeString:array [rpagSum..rpagStdDev] of string=
  (SrpSum,SRpMin,SRpMax,SRpAvg,SRpStdDev);


function StringToAgeType(value:string):TRpAggregateType;
var
 i:TRpAggregateType;
begin
 Result:=rpAgSum;
 for i:=rpAgsum to rpAgStdDev do
 begin
  if AggreTypeString[i]=Value then
  begin
   Result:=i;
   break;
  end;
 end;
end;

function StringToAggregate(value:string):TRpAggregate;
var
 i:TRpAggregate;
begin
 Result:=rpAgNone;
 for i:=rpAgNone to rpAgGeneral do
 begin
  if AggregatesString[i]=Value then
  begin
   Result:=i;
   break;
  end;
 end;
end;


constructor TRpLabelInterface.Create(AOwner:TComponent;pritem:TRpCommonComponent);
begin
 if Not (pritem is TRpLabel) then
  Raise Exception.Create(SRpIncorrectComponentForInterface);
 inherited Create(AOwner,pritem);
end;

procedure TRpLabelInterface.GetProperties(lnames,ltypes,lvalues:TStrings);
begin
 inherited GetProperties(lnames,ltypes,lvalues);
 // Text
 lnames.Add(SrpSText);
 ltypes.Add(SRpSString);
 lvalues.Add(TRpLabel(printitem).Text);
end;

procedure TRpLabelInterface.SetProperty(pname:string;value:string);
begin
 if length(value)<1 then
  exit;
 if pname=SRpSText then
 begin
  TRpLabel(fprintitem).Text:=value;
  invalidate;
  exit;
 end;
 inherited SetProperty(pname,value);
end;

function TRpLabelInterface.GetProperty(pname:string):string;
begin
 Result:='';
 if pname=SrpSText then
 begin
  Result:=TRpLabel(printitem).Text;
  exit;
 end;
 Result:=inherited GetProperty(pname);
end;


procedure TRpLabelInterface.Paint;
var
 alabel:TRpLabel;
 rec:TRect;
 aalign:integer;
begin
 alabel:=TRpLabel(printitem);
 if csDestroying in alabel.ComponentState then
  exit;

 // Draws the text
{$IFDEF MSWINDOWS}
 Canvas.Font.Name:=alabel.WFontName;
{$ENDIF}
{$IFDEF LINUX}
 Canvas.Font.Name:=alabel.LFontName;
{$ENDIF}

 Canvas.Font.Color:=alabel.FontColor;
 Canvas.Font.Size:=alabel.FontSize;
 Canvas.Font.Style:=IntegerToFontStyle(alabel.FontStyle);
 rec.Top:=0;
 rec.Left:=0;
 rec.Right:=Width-1;
 rec.Bottom:=Height-1;
 // Adds word wrap and single line
 aalign:=alabel.Alignment or alabel.VAlignment;
 if alabel.SingleLine then
  aalign:=aalign or Integer(AlignmentFlags_SingleLine);
 if alabel.Wordwrap then
  aalign:=aalign or Integer(AlignmentFlags_WordBreak);
 if Not alabel.Transparent then
 begin
  Canvas.TextExtent(alabel.Text,rec,aalign);
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=alabel.BackColor;
  Canvas.FillRect(rec);
 end;
 Canvas.Brush.Style:=bsClear;
 Canvas.TextRect(rec,0,0,alabel.Text,aalign);
 Canvas.Pen.Color:=clBlack;
 Canvas.Pen.Style:=psSolid;
 Canvas.Brush.Style:=bsClear;
 Canvas.Rectangle(0,0,Width,Height);
end;


constructor TRpExpressionInterface.Create(AOwner:TComponent;pritem:TRpCommonComponent);
begin
 if Not (pritem is TRpExpression) then
  Raise Exception.Create(SRpIncorrectComponentForInterface);
 inherited Create(AOwner,pritem);
end;

procedure TRpExpressionInterface.GetProperties(lnames,ltypes,lvalues:TStrings);
begin
 inherited GetProperties(lnames,ltypes,lvalues);
 // Expression
 lnames.Add(SrpSExpression);
 ltypes.Add(SRpSExpression);
 lvalues.Add(TRpExpression(printitem).Expression);

 // Display format
 lnames.Add(SrpSDisplayFOrmat);
 ltypes.Add(SRpSString);
 lvalues.Add(TRpExpression(printitem).DisplayFormat);

 // Identifier
 lnames.Add(SrpSIdentifier);
 ltypes.Add(SRpSString);
 lvalues.Add(TRpExpression(printitem).Identifier);

 // Aggregate
 lnames.Add(SrpSAggregate);
 ltypes.Add(SRpSList);
 lvalues.Add(AggregatesString[TRpExpression(printitem).Aggregate]);


 // Aggregate group
 lnames.Add(SrpSAgeGroup);
 ltypes.Add(SRpGroup);
 lvalues.Add(TRpExpression(printitem).GroupName);

 // Aggregate type
 lnames.Add(SrpSAgeType);
 ltypes.Add(SRpSList);
 lvalues.Add(AggretypeString[TRpExpression(printitem).AgType]);

  // Aggregate Ini value
 lnames.Add(SrpSIniValue);
 ltypes.Add(SRpSExpression);
 lvalues.Add(TRpExpression(printitem).AgIniValue);

 // Print Only One
 lnames.Add(SRpSOnlyOne);
 ltypes.Add(SRpSBool);
 lvalues.Add(BoolToStr(TRpExpression(printitem).PrintOnlyOne,true));
end;

procedure TRpExpressionInterface.SetProperty(pname:string;value:string);
begin
 if length(value)<1 then
  exit;
 if pname=SRpSExpression then
 begin
  TRpExpression(fprintitem).Expression:=value;
  invalidate;
  exit;
 end;
 if pname=SRpSDisplayFormat then
 begin
  TRpExpression(fprintitem).DisplayFormat:=value;
  invalidate;
  exit;
 end;
 if pname=SRpSIdentifier then
 begin
  TRpExpression(fprintitem).Identifier:=value;
  invalidate;
  exit;
 end;
 if pname=SrpSAggregate then
 begin
  TRpExpression(fprintitem).Aggregate:=StringToAggregate(Value);
  exit;
 end;
 if pname=SrpSAgeGroup then
 begin
  TRpExpression(fprintitem).GroupName:=Value;
  exit;
 end;
 if pname=SrpSAgeType then
 begin
  TRpExpression(fprintitem).AgType:=StringToAgeType(Value);
  exit;
 end;
 if pname=SrpSIniValue then
 begin
  TRpExpression(fprintitem).AgIniValue:=Value;
  exit;
 end;
 if pname=SrpSOnlyOne then
 begin
  TRpExpression(fprintitem).PrintOnlyOne:=StrToBool(Value);
  exit;
 end;
 inherited SetProperty(pname,value);
end;

function TRpExpressionInterface.GetProperty(pname:string):string;
begin
 Result:='';
 if pname=SrpSExpression then
 begin
  Result:=TRpExpression(printitem).Expression;
  exit;
 end;
 if pname=SrpSDisplayFormat then
 begin
  Result:=TRpExpression(printitem).DisplayFormat;
  exit;
 end;
 if pname=SrpSIdentifier then
 begin
  Result:=TRpExpression(printitem).Identifier;
  exit;
 end;
 if pname=SrpSAggregate then
 begin
  Result:=AggregatesString[TRpExpression(printitem).Aggregate];
  exit;
 end;
 if pname=SrpSAgeGroup then
 begin
  Result:=TRpExpression(printitem).GroupName;
  exit;
 end;
 if pname=SrpSAgeType then
 begin
  Result:=AggretypeString[TRpExpression(printitem).AgType];
  exit;
 end;
 if pname=SrpSIniValue then
 begin
  Result:=TRpExpression(printitem).AgIniValue;
  exit;
 end;
 if pname=SrpSOnlyOne then
 begin
  Result:=BoolToStr(TRpExpression(printitem).PrintOnlyOne,true);
  exit;
 end;
 Result:=inherited GetProperty(pname);
end;

procedure TRpExpressionInterface.GetPropertyValues(pname:string;
 lpossiblevalues:TStrings);
var
 i:TRpAggregate;
 it:TRpAggregateType;
begin
 if pname=SRpSAgeType then
 begin
  for it:=rpAgsum to rpAgStdDev do
  begin
   lpossiblevalues.Add(AggreTypeString[it]);
  end;
  exit;
 end;

 if pname=SRpSAggregate then
 begin
  for i:=rpAgNone to rpAgGeneral do
  begin
   lpossiblevalues.Add(AggregatesString[i]);
  end;
  exit;
 end;

 inherited GetPropertyValues(pname,lpossiblevalues);
end;


procedure TRpExpressionInterface.Paint;
var
 aexp:TRpExpression;
 rec:TRect;
 aalign:integer;
begin
 aexp:=TRpExpression(printitem);
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
  Canvas.TextExtent(aexp.Expression,rec,aalign);
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=aexp.BackColor;
  Canvas.FillRect(rec);
 end;
 Canvas.Brush.Style:=bsClear;
 Canvas.TextRect(rec,0,0,aexp.Expression,aalign);
 Canvas.Pen.Color:=clBlack;
 Canvas.Pen.Style:=psDashDot;
 Canvas.Brush.Style:=bsClear;
 Canvas.Rectangle(0,0,Width,Height);
end;


end.