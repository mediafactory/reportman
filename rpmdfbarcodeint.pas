{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdfbarcodeint                                 }
{       Implementation barcode design interface         }
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


unit rpmdfbarcodeint;

interface

uses SysUtils, Classes, QGraphics, QForms,rpmunits,
  QButtons, QExtCtrls, QControls, QStdCtrls,types,
  rpprintitem,rpmdbarcode,rpmdobinsint,rpmdconsts,
  rpgraphutils,rptypes,Qt;

type
 TRpBarcodeInterface=class(TRpSizePosInterface)
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


constructor TRpBarcodeInterface.Create(AOwner:TComponent;pritem:TRpCommonComponent);
begin
 if Not (pritem is TRpBarcode) then
  Raise Exception.Create(SRpIncorrectComponentForInterface);
 inherited Create(AOwner,pritem);
end;

class procedure TRpBarcodeInterface.FillAncestors(alist:TStrings);
begin
 inherited FillAncestors(alist);
 alist.Add('TRpBarcodeInterface');
end;

procedure TRpBarcodeInterface.GetProperties(lnames,ltypes,lvalues:TStrings);
begin
 inherited GetProperties(lnames,ltypes,lvalues);

 // Barcode Type
 lnames.Add(SRpSBarcodeType);
 ltypes.Add(SRpSList);
 if Assigned(lvalues) then
  lvalues.Add(BarcodeTypeStrings[TRpBarcode(printitem).Typ]);

 // Checksum
 lnames.Add(SRpSChecksum);
 ltypes.Add(SRpSBool);
 if Assigned(lvalues) then
  lvalues.Add(BoolToStr(TRpBarcode(printitem).CheckSum,True));
 // Modul
 lnames.Add(SrpSModul);
 ltypes.Add(SRpSCurrency);
 if Assigned(lvalues) then
  lvalues.Add(gettextfromtwips(TRpBarcode(printitem).Modul));
 // Ratio
 lnames.Add(SrpSRatio);
 ltypes.Add(SRpSCurrency);
 if Assigned(lvalues) then
  lvalues.Add(FormatCurr('#####0.00',TRpBarcode(printitem).Ratio));

 // Expression
 lnames.Add(SrpSExpression);
 ltypes.Add(SRpSExpression);
 if Assigned(lvalues) then
  lvalues.Add(TRpBarcode(printitem).Expression);

 // Display format
 lnames.Add(SrpSDisplayFOrmat);
 ltypes.Add(SRpSString);
 if Assigned(lvalues) then
  lvalues.Add(TRpBarcode(printitem).DisplayFormat);
end;

procedure TRpBarcodeInterface.SetProperty(pname:string;value:Widestring);
begin
 if length(value)<1 then
  exit;

 if pname=SRpSBarcodeType then
 begin
  TRpBarcode(fprintitem).Typ:=StringBarcodeToBarCodeType(value);
  invalidate;
  exit;
 end;
 if pname=SRpSCheckSum then
 begin
  TRpBarcode(fprintitem).Checksum:=StrToBool(value);
  invalidate;
  exit;
 end;
 if pname=SRpSModul then
 begin
  TRpBarcode(fprintitem).Modul:=gettwipsfromtext(value);
  invalidate;
  exit;
 end;
 if pname=SRpSRatio then
 begin
  TRpBarcode(fprintitem).Ratio:=StrToCurr(value);
  invalidate;
  exit;
 end;
 if pname=SRpSExpression then
 begin
  TRpBarcode(fprintitem).Expression:=value;
  invalidate;
  exit;
 end;
 if pname=SRpSDisplayFormat then
 begin
  TRpBarcode(fprintitem).DisplayFormat:=value;
  invalidate;
  exit;
 end;
 inherited SetProperty(pname,value);
end;

function TRpBarcodeInterface.GetProperty(pname:string):Widestring;
begin
 Result:='';
 if pname=SrpSBarcodeType then
 begin
  Result:=BarcodeTypeStrings[TRpBarcode(printitem).Typ];
  exit;
 end;
 if pname=SrpSCheckSum then
 begin
  Result:=BoolToStr(TRpBarcode(printitem).CheckSum,true);
  exit;
 end;
 if pname=SrpSModul then
 begin
  Result:=gettextfromtwips(TRpBarcode(printitem).Modul);
  exit;
 end;
 if pname=SrpSRatio then
 begin
  Result:=FormatCurr('######0.00',TRpBarcode(printitem).Ratio);
  exit;
 end;
 if pname=SrpSExpression then
 begin
  Result:=TRpBarcode(printitem).Expression;
  exit;
 end;
 if pname=SrpSDisplayFormat then
 begin
  Result:=TRpBarcode(printitem).DisplayFormat;
  exit;
 end;
 Result:=inherited GetProperty(pname);
end;

procedure TRpBarcodeInterface.GetPropertyValues(pname:string;
 lpossiblevalues:TStrings);
var
 it:TRpBarCodeType;
begin
 if pname=SRpSBarcodeType then
 begin
  for it:=bcCode_2_5_interleaved to bcCodeEAN13 do
  begin
   lpossiblevalues.Add(BarcodeTypeStrings[it]);
  end;
  exit;
 end;
 inherited GetPropertyValues(pname,lpossiblevalues);
end;


procedure TRpBarcodeInterface.Paint;
var
 aexp:TRpBarcode;
 rec:TRect;
begin
 aexp:=TRpBarcode(printitem);
 if csDestroying in aexp.ComponentState then
  exit;

 // Draws the text
 rec.Top:=0;
 rec.Left:=0;
 rec.Right:=Width-1;
 rec.Bottom:=Height-1;
 Canvas.TextRect(rec,0,0,aexp.Expression,0);
 Canvas.Pen.Color:=clBlack;
 Canvas.Pen.Style:=psDashDotDot;
 Canvas.Brush.Style:=bsClear;
 Canvas.Rectangle(0,0,Width,Height);
 DrawSelected;
end;


end.