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
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}


unit flabelint;

interface

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,types,
  rpprintitem,rplabelitem,rpobinsint,rpconsts,
{$IFNDEF PROFILE}  rpgraphutils;{$ENDIF}
{$IFDEF PROFILE}  rpgraphutils ,Proftimx;{$ENDIF}

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
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,54; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 if Not (pritem is TRpLabel) then
  Raise Exception.Create(SRpIncorrectComponentForInterface);
 inherited Create(AOwner,pritem);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,54; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TRpLabelInterface.GetProperties(lnames,ltypes,lvalues:TStrings);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,55; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 inherited GetProperties(lnames,ltypes,lvalues);
 // Text
 lnames.Add(SrpSText);
 ltypes.Add(SRpSString);
 lvalues.Add(TRpLabel(printitem).Text);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,55; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TRpLabelInterface.SetProperty(pname:string;value:string);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,56; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 if length(value)<1 then
  exit;
 if pname=SRpSText then
 begin
  TRpLabel(fprintitem).Text:=value;
  invalidate;
  exit;
 end;
 inherited SetProperty(pname,value);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,56; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

function TRpLabelInterface.GetProperty(pname:string):string;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,57; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 Result:='';
 if pname=SrpSText then
 begin
  Result:=TRpLabel(printitem).Text;
  exit;
 end;
 Result:=inherited GetProperty(pname);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,57; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


procedure TRpLabelInterface.Paint;
var
 alabel:TRpLabel;
 rec:TRect;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,58; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
 rec.Top:=0;
 rec.Left:=0;
 rec.Right:=Width-1;
 rec.Bottom:=Height-1;
 Canvas.TextRect(rec,0,0,alabel.Text,alabel.Alignment or alabel.VAlignment);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,58; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


constructor TRpExpressionInterface.Create(AOwner:TComponent;pritem:TRpCommonComponent);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,59; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 if Not (pritem is TRpExpression) then
  Raise Exception.Create(SRpIncorrectComponentForInterface);
 inherited Create(AOwner,pritem);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,59; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TRpExpressionInterface.GetProperties(lnames,ltypes,lvalues:TStrings);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,60; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 inherited GetProperties(lnames,ltypes,lvalues);
 // Expression
 lnames.Add(SrpSExpression);
 ltypes.Add(SRpSExpression);
 lvalues.Add(TRpExpression(printitem).Expression);

 // Display format
 lnames.Add(SrpSDisplayFOrmat);
 ltypes.Add(SRpSString);
 lvalues.Add(TRpExpression(printitem).DisplayFormat);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,60; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TRpExpressionInterface.SetProperty(pname:string;value:string);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,61; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
 inherited SetProperty(pname,value);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,61; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

function TRpExpressionInterface.GetProperty(pname:string):string;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,62; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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
 Result:=inherited GetProperty(pname);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,62; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


procedure TRpExpressionInterface.Paint;
var
 aexp:TRpExpression;
 rec:TRect;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,63; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
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

 rec.Top:=0;
 rec.Left:=0;
 rec.Right:=Width-1;
 rec.Bottom:=Height-1;
 Canvas.TextRect(rec,0,0,aexp.Expression,aexp.Alignment or aexp.VAlignment);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,63; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


end.
