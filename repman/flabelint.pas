{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       flabelint                                       }
{       Implementation label and expression designer    }
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


unit flabelint;

interface

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,types,
  rpprintitem,rplabelitem,rpobinsint,rpconsts,
  rpgraphutils;

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
 end;




  TFLabelInterface = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.xfm}

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
begin
 alabel:=TRpLabel(printitem);
 Canvas.Pen.Color:=clBlack;
 Canvas.Pen.Style:=psSolid;
 if alabel.transparent then
  Canvas.Brush.Style:=bsClear
 else
 begin
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=alabel.BackColor;
 end;
 Canvas.Rectangle(0,0,Width,Height);

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

 Canvas.TextOut(0,0,alabel.Text);
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
 // Text
 lnames.Add(SrpSExpression);
 ltypes.Add(SRpSString);
 lvalues.Add(TRpExpression(printitem).Expression);
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
 Result:=inherited GetProperty(pname);
end;


procedure TRpExpressionInterface.Paint;
var
 aexp:TRpExpression;
begin
 aexp:=TRpExpression(printitem);
 Canvas.Pen.Color:=clBlack;
 Canvas.Pen.Style:=psDashDot;
 if aexp.transparent then
  Canvas.Brush.Style:=bsClear
 else
 begin
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=aexp.BackColor;
 end;
 Canvas.Rectangle(0,0,Width,Height);

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

 Canvas.TextOut(0,0,aexp.Expression);
end;


end.
