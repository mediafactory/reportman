unit flabelint;

interface

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,types,
  rpprintitem,rplabelitem,rpobinsint,rpconsts;

type
 TRpLabelInterface=class(TRpGenTextInterface)
  private
  protected
   procedure Paint;override;
  public
   constructor Create(AOwner:TComponent;pritem:TRpCommonComponent);override;
   procedure GetProperties(lnames,ltypes,lvalues:TStrings);override;
   procedure SetProperty(pname:string;value:string);override;
   procedure GetProperty(pname:string;var value:string);override;
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

procedure TRpLabelInterface.GetProperty(pname:string;var value:string);
begin
 if pname=SrpSText then
 begin
  value:=TRpLabel(printitem).Text;
  exit;
 end;

 inherited GetProperty(pname,value);
end;


procedure TRpLabelInterface.Paint;
var
 alabel:TRpLabel;
 rec:TRect;
begin
 alabel:=TRpLabel(printitem);
 if (Not alabel.Transparent) then
 begin
  Canvas.Brush.Color:=alabel.BackColor;
  rec.Left:=0;rec.Top:=0;
  rec.Bottom:=Height;rec.Right:=Width;
  Canvas.FillRect(rec);
 end
 else
 begin
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Style:=psSolid;
  Canvas.Brush.Style:=bsClear;
  Canvas.Rectangle(0,0,Width,Height);
 end;
 // Draws the text
 Canvas.Font.Name:=alabel.FontName;
 Canvas.Font.Color:=alabel.FontColor;
 Canvas.Font.Size:=alabel.FontSize;
 // The Style must be converted from integer
// Canvas.Font.Style:=RpFontStyleToQtFontStyle(alabel.FontStyle);
 Canvas.TextOut(0,0,alabel.Text);
end;

end.
