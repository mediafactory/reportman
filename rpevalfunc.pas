{*******************************************************}
{                                                       }
{       Rpevalfunc                                      }
{       Functions for the TRpEvaluator for              }
{        Manager                                        }
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

unit rpevalfunc;

interface

{$I rpconf.inc}

uses
  SysUtils, Classes,
  rpmdconsts,DB,
{$IFDEF USEVARIANTS}
  Variants,
{$ENDIF}
  rptypeval,rptypes;

type

 { Function Uppercase }
 TIdenUppercase=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function Lowercase }
 TIdenLowercase=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function HourMinSec }
 TIdenHourMinSec=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function Sinus }
 TIdenSinus=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function FloatToDataTime }
 TIdenFloatToDateTime=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function Round }
 TIdenRound=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function INT }
 TIdenInt=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function Val }
 TIdenVal=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function STR }
 TIdenSTR=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function LEFT }
 TIdenTrim=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function LEFT }
 TIdenLEFT=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function Pos }
 TIdenPos=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function GraphicLear }
 TIdenGraphicClear=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;
 TIdenGraphicNew=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 TIdenGraphicOperation=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 TIdenTextOperation=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function Modul }
 TIdenModul=class(TIdenFunction)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Function SQRT }
 TIdenSQRT=class(TIdenFunction)
 protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOWner:TComponent);override;
  end;

 { Constant }
 TIdenToday=class(TIdenConstant)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Constant }
 TIdenTime=class(TIdenConstant)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 TIdenNow=class(TIdenConstant)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 TIdenNULL=class(TIdenConstant)
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 { Returns the month }
 TIdenMonth=Class(TIdenFunction)
 protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOWner:TComponent);override;
  end;

 TIdenEvalText=class(TIdenFunction)
 protected
   function GeTRpValue:TRpValue;override;
 public
   constructor Create(AOWner:TComponent);override;
 end;


 TIdenMonthname=Class(TIdenFunction)
 protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOWner:TComponent);override;
  end;

 TIdenYear=Class(TIdenFunction)
 protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOWner:TComponent);override;
  end;

 TIdenDay=Class(TIdenFunction)
 protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOWner:TComponent);override;
  end;

 TIdenRight=Class(TIdenFunction)
 protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOWner:TComponent);override;
  end;

 TIdenSubstr=Class(TIdenFunction)
 protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOWner:TComponent);override;
  end;

 TIdenFormatStr=class(TIdenFunction)
 protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOWner:TComponent);override;
  end;

 TIdenNumToText=class(TIdenFunction)
 protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOWner:TComponent);override;
  end;


 TRpNewValue=procedure (Y:Single;Cambio:Boolean;leyen,textleyen:string) of object;

 TVariableGrap=class(TIdenVariable)
  protected
   FOnNewValue:TRpNewValue;
   FOnClear:TNotifyEvent;
   procedure SetRpValue(Value:TRpValue);override;
   function GetRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
   procedure NewValue(Y:Single;Cambio:Boolean;leyen,textleyen:string);
   procedure Clear;
   property OnClear:TNotifyEvent read FOnClear write FOnClear;
   property OnNewValue:TRpNewValue read FOnNewValue write FOnNewValue;
  end;

 //added FRB 20030204
 TIdenReplaceStr=class(TIdenFunction)
 protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOWner:TComponent);override;
  end;


 function Roundfloat(num:double;redondeo:double):double;




implementation

uses rpeval,Math;

{**************************************************************************}

{ TIdenUppercase }

constructor TIdenUppercase.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=1;
 IdenName:='Uppercase';
 Help:=SRpUppercase;
 Model:='function '+'Uppercase'+'(s:string):string';
 AParams:=SRpPUppercase;
end;

{**************************************************************************}

function TIdenUppercase.GeTRpValue:TRpValue;
begin
 if Params[0]=NULL then
 begin
  result:='';
  exit;
 end;
 if VarType(Params[0])<>varString then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 Result:=Uppercase(Params[0]);
end;

{**************************************************************************}

{ TIdenLowercase }

constructor TIdenLowercase.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=1;
 IdenName:='Lowercase';
 Help:=SRpLowercase;
 Model:='function '+'Lowercase'+'(s:string):string';
 AParams:=SRpPLowercase;
end;

{**************************************************************************}

function TIdenLowercase.GeTRpValue:TRpValue;
begin
 if Params[0]=NULL then
 begin
  result:='';
  exit;
 end;
 if VarType(Params[0])<>varString then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 Result:=Lowercase(Params[0]);
end;

{**************************************************************************}

{ TIdenHourMinSec }

constructor TIdenHourMinSec.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=4;
 IdenName:='HourMinSec';
 Help:=SRpHourMinSec;
 Model:='function '+'HourMinSec'+'(h:Double;idenH:string;idenM:string;idenS:string):string';
 AParams:=SRpPHourMinSec;
end;

{**************************************************************************}

function TIdenHourMinSec.GeTRpValue:TRpValue;
var
 racional:double;
 hores:integer;
 minuts,segons:word;
 minutsstr,segonsstr:string;
begin
 if (not (VarType(Params[1])=varString)) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 if (not (VarType(Params[2])=varString)) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 if (not (VarType(Params[3])=varString)) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 case Vartype(Params[0]) of
  varSmallInt..varCurrency:
   begin
    racional:=Extended(Params[0]);
    //calculations in racional
    hores:=Round(Int(racional));
    minuts:=Round(Int(Frac(racional)*60));
    segons:=Round(Frac(Frac(racional)*60)*60);
    if segons<10 then
     segonsstr:='0'+IntToStr(segons)
    else
     segonsstr:=IntToStr(segons);
    if minuts<10 then
     minutsstr:='0'+IntToStr(minuts)
    else
     minutsstr:=IntToStr(minuts);
    Result:=IntToStr(hores)+Params[1]+minutsstr+Params[2]
     +segonsstr+Params[3];
   end;
  else
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
  end;
end;

{**************************************************************************}

{ TFloatToDateTime }

constructor TIdenFloatToDateTime.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=1;
 IdenName:='FloatToDateTime';
 Help:=SRpFloatToDateTime;
 Model:='function '+'FloatToDateTime'+'(n:Double):TDateTime';
 AParams:=SRpPFloatToDateTime;
end;

{**************************************************************************}

function TIdenFloatToDateTime.GeTRpValue:TRpValue;
begin
 case Vartype(Params[0]) of
  varSmallInt..varCurrency:
   begin
    Result:=TDateTime(Params[0]);
   end;
  vardate:
    Result:=TDateTime(Params[0]);
  varNull:
   begin
    Result:=Null;
   end;
  else
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
  end;
end;


{**************************************************************************}

{ TIdenSinus }

constructor TIdenSinus.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=1;
 IdenName:='Sin';
 Help:=SRpSin;
 Model:='function '+'Sin'+'(ang:Double):double';
 AParams:=SRpPSin;
end;

{**************************************************************************}

function TIdenSinus.GeTRpValue:TRpValue;
begin
 case Vartype(Params[0]) of
  varSmallInt..varCurrency:
   begin
    Result:=Sin(Extended(Params[0]));
   end;
  else
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
  end;
end;


{**************************************************************************}

{ TIdenRound }

constructor TIdenRound.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=2;
 IdenName:='Round';
 Help:=SRpRound;
 model:='function '+'Round'+'(num:double,r:double):double';
 aParams:=SRpPRound;
end;

{**************************************************************************}

function Roundfloat(num:double;redondeo:double):double;
var
 provanum,provaredon,quocient:currency;
 intnum,intredon:Comp;
 reste:integer;
 signenum,escala:integer;
begin
 if ((redondeo=0) or (num=0)) then
 begin
  result:=num;
  exit;
 end;
 // Original number
 signenum:=1;
 if num<0 then
  signenum:=-1;
 // Has decimal?
 provanum:=abs(num);
 provaredon:=abs(redondeo);
 escala:=1;
 While (frac(provanum)<>0) do
 begin
  provanum:=provanum*10;
  provaredon:=provaredon*10;
  escala:=escala*10;
 end;
 While (frac(provaredon)<>0) do
 begin
  provanum:=provanum*10;
  provaredon:=provaredon*10;
  escala:=escala*10;
 end;
 // Integers
 intnum:=int(provanum);
 intredon:=int(provaredon);
 // Mod
 quocient:=intnum/intredon;
 reste:=Round(intnum-intredon*Int(quocient));
 if (reste<(intredon/2)) then
  intnum:=intnum-reste
 else
  intnum:=intnum-reste+intredon;
 result:=intnum/escala*signenum;
end;

{**************************************************************************}

function TIdenRound.GeTRpValue:TRpValue;
begin
 if (not (VarType(Params[0]) in [varSmallInt..varCurrency])) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 if (not (VarType(Params[1]) in [varSmallInt..varCurrency])) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 Result:=Roundfloat(Extended(Params[0]),Extended(Params[1]));
end;

{**************************************************************************}

{ TIdenInt }

constructor TIdenInt.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=1;
 IdenName:='Int';
 Help:=SRpInt;
 model:='function '+'Int'+'(num:double):integer';
 aParams:=SRpPInt;
end;

{**************************************************************************}

function TIdenInt.GeTRpValue:TRpValue;
begin
 if (not (VarType(Params[0]) in [varSmallInt..varDate,varVariant])) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 case Vartype(Params[0]) of
  varSmallInt..varDate:
   begin
    Result:=Int(Extended(Params[0]))
   end;
  varVariant:
   begin
    Result:=Int(Extended(Params[0]))
   end;
  else
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
  end;

end;

{ TIdenSTR }

constructor TIdenSTR.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=1;
 IdenName:='Str';
 Help:=SRpStr;
 model:='function '+'Str'+'(num:variant):string';
 aParams:=SRpPStr;
end;

{**************************************************************************}

function TIdenSTR.GeTRpValue:TRpValue;
begin
 if VarType(Params[0])=varString then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 if VarIsNull(Params[0]) then
  Result:=''
 else
  Result:=String(Params[0]);
end;

{**************************************************************************}

{ TIdenVal }

constructor TIdenVal.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=1;
 IdenName:='Val';
 Help:=SRpVal;
 model:='function '+'Val'+'(s:string):double';
 aParams:=SRpPVal;
end;

{**************************************************************************}

function TIdenVal.GeTRpValue:TRpValue;
begin
 try
  if VarType(Params[0])=varstring then
  begin
   if Params[0]=''+chr(0) then
   begin
    Result:=0;
    Exit;
   end;
   Result:=StrToFloat(Params[0]);
  end
  else
  begin
   Result:=Extended(Params[0]);
  end;
  { To integer }
 except
   Raise TRpNamedException.Create(SRpConvertError,
         IdenName);
 end;
end;

{**************************************************************************}

{ TIdenTrim }

constructor TIdenTrim.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=1;
 IdenName:='Trim';
 Help:=SRpTrim;
 model:='function '+'Trim'+'(s:string):string';
 aParams:=SRpPTrim;
end;

{**************************************************************************}

function TIdenTrim.GeTRpValue:TRpValue;
begin
 if Params[0]=NULL then
 begin
  result:='';
  exit;
 end;
 if Vartype(Params[0])<>varstring then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 Result:=Trim(String(Params[0]));
end;

{**************************************************************************}

{ TIdenLeft }

constructor TIdenLEFT.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=2;
 IdenName:='Left';
 Help:=SRpLeft;
 model:='function '+'Left'+'(s:string,count:integer):string';
 aParams:=SRpPLeft;
end;

{**************************************************************************}

function TIdenLEFT.GeTRpValue:TRpValue;
begin
 if Vartype(Params[0])<>varstring then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 if ( Not (VarType(Params[1]) in [varSmallInt..varInteger])) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 Result:=Copy(String(Params[0]),1,Integer(Params[1]));
end;

{ TIdenPos }

constructor TIdenPos.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=2;
 IdenName:='Pos';
 Help:=SRpPos;
 model:='function '+'Pos'+'(substr:string,str:string):integer';
 aParams:=SRpPPos;
end;

{**************************************************************************}

function TIdenPos.GeTRpValue:TRpValue;
begin
 if Params[0]=null then
 begin
  Result:=0;
  exit;
 end;
 if Vartype(Params[0])<>varstring then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 if Vartype(Params[1])<>varstring then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 Result:=Pos(String(Params[0]),string(Params[1]));
end;


constructor TIdenGraphicClear.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=1;
 IdenName:='GraphicClear';
 Help:=SRpGraphicClear;
 model:='function '+'GraphicClear'+'(Gr:string):Boolean';
 aParams:=SRpPGraphicClear;
end;


function TIdenGraphicClear.GeTRpValue:TRpValue;
var
 iden:TRpIdentifier;
begin
 if Vartype(Params[0])<>varstring then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 // Buscamos el identificador
 iden:=(evaluator As TRpEvaluator).Searchidentifier(Params[0]);
 if iden=nil then
 begin
   Raise TRpNamedException.Create(SRpIdentifierexpected,
         IdenName+'-'+Params[0]);
 end;
 if Not (iden is TVariableGrap) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName+'-'+Params[0]);
 Result:=True;
 (iden As TVariableGrap).Clear;
end;


constructor TIdenGraphicNew.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=5;
 IdenName:='GraphicNew';
 Help:=SRpGraphicNew;
 model:='function '+'GraphicNew'+'(Gr:string,V:Single,C:Boolean,Etiq:string,Caption:string):Boolean';
 aParams:=SRPPgraphicnew;
end;


function TIdenGraphicNew.GeTRpValue:TRpValue;
var
 iden:TRpIdentifier;
begin
 if Vartype(Params[0])<>varstring then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 if Not (Vartype(Params[1]) in [varsmallint..varcurrency]) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 if Vartype(Params[2])<>varBoolean then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 if Vartype(Params[3])<>varstring then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 if Vartype(Params[4])<>varstring then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 // Buscamos el identificador
 iden:=(evaluator As TRpEvaluator).SearchIdentifier(Params[0]);
 if iden=nil then
 begin
   Raise TRpNamedException.Create(SRpIdentifierexpected,
         IdenName+'-'+Params[0]);
 end;
 if Not (iden is TVariableGrap) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName+'-'+Params[0]);

 Result:=True;
 (iden As TVariableGrap).NewValue(single(Params[1]),Boolean(Params[2]),string(Params[3]),string(Params[4]));
end;



constructor TIdenSQRT.Create(AOwner:TComponent);
begin
 inherited Create(Aowner);
 FParamcount:=1;
 IdenName:='Sqrt';
 Help:=SRpSqrt;
 model:='function '+'Sqrt'+'(num:double):double';
 aParams:=SRpPSQrt;
end;

{**************************************************************************}

function TIdenSQRT.GeTRpValue:TRpValue;
begin
 case varType(Params[0]) of
  varSmallInt..VarCurrency:
    Result:=SQRT(Extended(Params[0]));
  else
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 end;
end;

{**************************************************************************}

{ TIdenModul }

constructor TIdenModul.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=2;
 IdenName:='Mod';
 Help:=SRpMod;
 model:='function '+'Mod'+'(d1:integer,d2:integer):integer';
 aParams:=SRpPMod;
end;

{**************************************************************************}

function TIdenModul.GeTRpValue:TRpValue;
begin
 case Vartype(Params[0]) of
  varSmallInt..VarCurrency:
   begin
    if Integer(Params[0])<>Params[0] then
    Raise TRpNamedException.Create(SRpEvalType,
          IdenName);
    if Integer(Params[1])<>Params[1] then
    Raise TRpNamedException.Create(SRpEvalType,
          IdenName);
    ReSult:=Integer(Params[0]) mod Integer(Params[1]);
   end;
  else
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 end;
end;

{ TIdenToday }
constructor TIdenToday.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 IdenName:='Today';
 Help:=SRpToday;
 model:='function '+'Today'+':date';
end;

function TIdenToday.GeTRpValue:TRpValue;
begin
 REsult:=Date;
end;

{ TIdenAhora }
constructor TIdenTime.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 IdenName:='Time';
 Help:=SRpTime;
 model:='function '+'Time'+':time';
end;

{**************************************************************************}

function TIdenTime.GeTRpValue:TRpValue;
begin
 REsult:=Time;
end;

{ TIdenNULL }
constructor TIdenNULL.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 IdenName:='NULL';
 Help:=SRpNull;
 model:='function '+'NULL'+':variant';
end;

{**************************************************************************}

function TIdenNULL.GeTRpValue:TRpValue;
begin
 REsult:=Null;
end;

{ TIdenTime }
constructor TIdenNow.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 IdenName:='Now';
 Help:=SRpNow;
 model:='function '+'Now'+':datetime';
end;

{**************************************************************************}

function TIdenNow.GeTRpValue:TRpValue;
begin
 REsult:=Now;
end;

{ TIdenMonthname }
{**************************************************************************}

constructor TIdenMonthname.Create(AOwner:TComponent);
begin
 inherited Create(Aowner);
 FParamcount:=1;
 IdenName:='Monthname';
 Help:=SRpMonthname;
 model:='function '+'Monthname'+'(d:datetime):string';
 aParams:=SRpPMonthName;
end;

{**************************************************************************}

function TIdenMonthname.GeTRpValue:TRpValue;
var any,mes,dia:Word;
begin
 case varType(Params[0]) of
  varDate:
   begin
    DecodeDate(TDateTime(Params[0]),Any,Mes,Dia);
    Result:=LongMonthNames[Mes];
   end;
  else
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 end;
end;

// TIdenEvalText

constructor TIdenEvalText.Create(AOwner:TComponent);
begin
 inherited Create(Aowner);
 FParamcount:=1;
 IdenName:='EvalText';
 Help:=SRpEvaltext;
 model:='function '+'EvalText'+'(expr:string):variant';
 aParams:=SRpPEvalText;
end;

function TIdenEvalText.GeTRpValue:TRpValue;
var avaluador:TRpEvaluator;
begin
 if varType(Params[0])<>varstring then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 // Evalue
 avaluador:=(evaluator As TRpEvaluator);
 result:=avaluador.Evaluatetext(Params[0]);
end;

{ TIdenMonth }
{**************************************************************************}

constructor TIdenMonth.Create(AOwner:TComponent);
begin
 inherited Create(Aowner);
 FParamcount:=1;
 IdenName:='Month';
 Help:=SRpMonth;
 model:='function '+'Month'+'(d:datetime):integer';
 aParams:=SRpPMonth;
end;

{**************************************************************************}

function TIdenMonth.GeTRpValue:TRpValue;
var any,mes,dia:Word;
begin
 case varType(Params[0]) of
  varDate:
   begin
    DecodeDate(TDateTime(Params[0]),Any,Mes,Dia);
    Result:=Mes;
   end;
  else
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 end;
end;

{ TIdenYear }
{**************************************************************************}

constructor TIdenYear.Create(AOwner:TComponent);
begin
 inherited Create(Aowner);
 FParamcount:=1;
 IdenName:='Year';
 Help:=SRpYear;
 model:='function '+'Year'+'(d:datetime):integer';
 aParams:=SRpPYear;
end;

{**************************************************************************}

function TIdenYear.GeTRpValue:TRpValue;
var any,mes,dia:Word;
begin
 case varType(Params[0]) of
  varDate:
   begin
    DecodeDate(TDateTime(Params[0]),Any,Mes,Dia);
    Result:=Any;
   end;
  else
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 end;
end;

{ TIdenDay }
{**************************************************************************}

constructor TIdenDay.Create(AOwner:TComponent);
begin
 inherited Create(Aowner);
 FParamcount:=1;
 IdenName:='Day';
 Help:=SRpDay;
 model:='function '+'Day'+'(d:datetime):integer';
 aParams:=SRpPDay;
end;

{**************************************************************************}

function TIdenDay.GeTRpValue:TRpValue;
var any,mes,dia:Word;
begin
 case varType(Params[0]) of
  varDate:
   begin
    DecodeDate(TDateTime(Params[0]),Any,Mes,Dia);
    Result:=integer(Dia);
   end;
  else
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 end;
end;

{ TIdenRight }

constructor TIdenRight.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=2;
 IdenName:='Right';
 Help:=SRpRight;
 model:='function '+'Right'+'(s:string,count:integer):string';
 aParams:=SRpPRight;
end;

{**************************************************************************}

function TIdenRight.GeTRpValue:TRpValue;
begin
 if Vartype(Params[0])<>varstring then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 if ( Not (VarType(Params[1]) in [varSmallInt..varInteger])) or
    ( Integer(Params[1])<1 ) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);

 Result:=Copy(String(Params[0]),
              Length(String(Params[0]))+1-Integer(Params[1]),
              Integer(Params[1]));
end;


{ TIdenSubstr }

constructor TIdenSubstr.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=3;
 IdenName:='Substr';
 Help:=SRpSubStr;
 model:='function '+'Substr'+'(cadena:string,index:integer,count:integer):string';
 aParams:=SRpPSubStr;
end;

{**************************************************************************}

function TIdenSubstr.GeTRpValue:TRpValue;
begin
 if Vartype(Params[0])<>varstring then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 if ( Not (VarType(Params[1]) in [varSmallInt..varInteger])) or
    ( Integer(Params[1])<1 ) or
    ( Not (VarType(Params[2]) in [varSmallInt..varInteger])) or
    ( Integer(Params[2])<1 )  then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);

 Result:=Copy(String(Params[0]),
              Integer(Params[1]),
              Integer(Params[2]));
end;

{ TIdenFormatStr }

constructor TIdenFormatstr.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=2;
 IdenName:='FormatStr';
 Help:=SRpFormatStr;
 model:='function '+'Formatstr'+'(format:string,v:variant):string';
 aParams:=SRpPFormatStr;
end;

{**************************************************************************}

function TIdenFormatstr.GeTRpValue:TRpValue;
var
 Value:variant;
begin
 if Vartype(Params[0])<>varstring then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);

 // fem el case
 Value:=Params[1];
 if Value=NULL then
 begin
  result:='';
  exiT;
 end;
 case vartype(Value) of
  varSmallint..VarCurrency:
   begin
    Result:=FormatFloat(Params[0],extended(Value));
   end;
  // Modify by TAHUL TAMRAKR 27/05/2003
  // Bugfix for detecting DateTime datatypes (MySQL)
  varDate,272:
   begin
    Result:=FormatDateTime(Params[0],VarToDateTime(Value));
   end;
  varBoolean:
   begin
    if Value.asboolean then
     result:='True'
    else
     result:='False';
   end;
  else
  begin
   Result:=VarToStr(Value);
  end;
 end;
end;

{ TIdenNumToText }

constructor TIdenNumToText.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=2;
 IdenName:='NumToText';
 Help:=SRpNumToText;
 model:='function '+'NumToText'+'(n:double,f:boolean):string';
 aParams:=SRpPNumToText;
end;

{**************************************************************************}

function TIdenNumToText.GeTRpValue:TRpValue;
var
 Value:variant;
begin
 if Vartype(Params[1])<>varboolean then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 if NOt (Vartype(Params[0]) in [varsmallint..varcurrency,varShortInt,varInt64,varWord,varLongWord,varByte]) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);

 Value:=Params[1];
 if Value=NULL then
 begin
  result:='';
  exiT;
 end;
 Result:=NumberToText(Params[0],Params[1],TRpEvaluator(evaluator).Language);
end;



constructor TVariableGrap.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
end;

procedure TVariableGrap.SetRpValue(Value:TRpValue);
begin
 // Asignem i pasem
 NewValue(Value,False,string(Value),'');
end;


procedure TVariableGrap.NewValue(Y:Single;Cambio:Boolean;leyen,textleyen:string);
begin
 if Assigned(FOnNewValue) then
  FOnNewValue(Y,Cambio,leyen,textleyen);
end;

procedure TVariableGrap.Clear;
begin
 if Assigned(FOnClear) then
  FOnClear(Self);
end;

function TVariableGrap.GetRpValue:TRpValue;
begin
 Raise Exception.Create(SRpErrorIdenExpression);
end;



//added FRB 20030204
{ TIdenReplaceStr }

constructor TIdenReplaceStr.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=3;
 IdenName:='ReplaceStr';
 Help:=SRpReplaceStr;
 model:='function '+'ReplaceStr'+'(const S, OldPattern, NewPattern:string;): string;';
 aParams:=SRpPReplaceStr;
end;

{**************************************************************************}

function TIdenReplaceStr.GeTRpValue:TRpValue;
begin
 if (Vartype(Params[0])<>varstring)
  or (Vartype(Params[1])<>varstring)
  or (Vartype(Params[2])<>varstring) then
   Raise TRpNamedException.Create(SRpEvalType,IdenName);
 Result:=StringReplace(String(Params[0]),String(Params[1]),String(Params[2]),
  [rfReplaceAll, rfIgnoreCase]);
end;


{ TIdenGraphicOperation }

constructor TIdenGraphicOperation.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=10;
 IdenName:='GraphicOp';
 Help:='';
 model:='function '+'GraphicOp'+'(Top,Left,Width,Height:integer;'+#10+
    'DrawStyle:integer;BrushStyle:integer;BrushColor:integer;'+#10+
    'PenStyle:integer;PenWidth:integer; PenColor:integer):Boolean';
 aParams:='';
end;

{**************************************************************************}

function TIdenGraphicOperation.GeTRpValue:TRpValue;
var
 i:integer;
begin
 for i:=0 to 3 do
 begin
  if (Not Vartype(Params[i]) in [varSmallInt..varCurrency,varShortInt..varInt64]) then
   Raise TRpNamedException.Create(SRpEvalType,IdenName);
 end;
 for i:=4 to ParamCount-1 do
 begin
  if (Not Vartype(Params[i]) in [varSmallInt..varInteger,varShortInt..varInt64]) then
   Raise TRpNamedException.Create(SRpEvalType,IdenName);
 end;
 if Assigned((evaluator As TRpEvaluator).OnGraphicOp) then
  Result:=(evaluator As TRpEvaluator).OnGraphicOp(Round(Params[0]),Round(Params[1]),Round(Params[2]),Round(Params[3]),Params[4],
   Params[5],Params[6],Params[7],Params[8],Params[9])
 else
  Result:=false;
end;

{ TIdenTextOperation }

constructor TIdenTextOperation.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=19;
 IdenName:='TextOp';
 Help:='';
 model:='function TextOp(Top,Left,Width,Height:integer;'+#10+
  'Text,LFontName,WFontName:WideString;'+#10+
  'FontSize,FontRotation,FontStyle,FontColor,Type1Font:integer;'+#10+
  'CutText:boolean;Alignment:integer;WordWrap,RightToLeft:Boolean;'+#10+
  'PrintStep,BackColor:integer;transparent:boolean) of Object;';
 aParams:='';
end;

{**************************************************************************}

function TIdenTextOperation.GeTRpValue:TRpValue;
var
 i:integer;
begin
 for i:=0 to ParamCount-1 do
 begin
  if (i in [0..3]) then
   if (Not (Vartype(Params[i]) in [varSmallInt..varCurrency,varShortInt..varInt64])) then
    Raise TRpNamedException.Create(SRpEvalType,IdenName);
  if (i in [7..11,13,16,17]) then
   if (Not (Vartype(Params[i]) in [varSmallInt..varInteger,varShortInt..varInt64])) then
    Raise TRpNamedException.Create(SRpEvalType,IdenName);
  if (i in [12,14,15,18]) then
   if (Not (Vartype(Params[i])=varBoolean)) then
    Raise TRpNamedException.Create(SRpEvalType,IdenName);
  if (i in [4..6]) then
   if (Not (Vartype(Params[i])=varString)) then
    Raise TRpNamedException.Create(SRpEvalType,IdenName);
 end;
 if Assigned((evaluator As TRpEvaluator).OnTextOp) then
  Result:=(evaluator As TRpEvaluator).OnTextOp(Round(Params[0]),Round(Params[1]),Round(Params[2]),Round(Params[3]),Params[4],
   Params[5],Params[6],Params[7],Params[8],Params[9],
   Params[10],Params[11],Params[12],Params[13],Params[14],
   Params[15],Params[16],Params[17],Params[18])
 else
  Result:=false;
end;

end.
