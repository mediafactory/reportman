{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rptypes                                         }
{       TRpTypes: Generic type definitions used by      }
{       common components of Report manager             }
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

unit rptypes;

interface

{$I rpconf.inc}

uses Sysutils,
{$IFDEF MSWINDOWS}
 Windows,
{$ENDIF}
 Classes,
{$IFDEF USEVARIANTS}
 Variants,
{$ENDIF}
{$IFDEF USEBCD}
 FMTBcd,
{$ENDIF}

 rpconsts;


const
 MAX_LANGUAGES=3;
{$IFNDEF USEVARIANTS}
 varWord     = $0012;
 varLongWord = $0013;
 varInt64    = $0014;
 varShortInt = $0010;
{$ENDIF}

type
{$IFNDEF USEVARIANTS}
 TVarType=integer;
{$ENDIF}



 TRpTwips=integer;
 TRpImageDrawStyle=(rpDrawCrop,rpDrawStretch,rpDrawFull,rpDrawTile);
 TRpAggregate=(rpAgNone,rpAgGroup,rpAgPage,rpAgGeneral);
 TRpAggregateType=(rpagSum,rpagMin,rpagMax,rgagAvg,rpagStdDev);
 TRpReportChanged=(rpReportStart,rpDataChange,rpGroupChange,rpPageChange);
 TRpShapeType=(rpsRectangle, rpsSquare, rpsRoundRect, rpsRoundSquare,
  rpsEllipse, rpsCircle,rpsHorzLine,rpsVertLine);

 TRpPageSize=(rpPageSizeDefault,rpPageSizeCustom);

 TRpColor=integer;

 // How to show preview
 TRpPreviewStyle = (spWide,spNormal,spEntirePage);

 TRpReportException=class(Exception)
  private
   FComponent:TComponent;
  public
   constructor Create(AMessage:String;compo:TComponent);
   property Component:TComponent read FComponent;
  end;

// Compares 2 streams and returns true if they are equal
function StreamCompare(Stream1:TStream;Stream2:TStream):Boolean;
procedure Generatenewname(Component:TComponent);
function FormatVariant(displayformat:string;Value:Variant):widestring;
function CopyFileTo(const Source, Destination: string): Boolean;

// Language identifiers
var
 rplangids:array [0..MAX_LANGUAGES-1] of string=('EN','ES','CAT');
 rplangdesc:array [0..MAX_LANGUAGES-1] of string=(SRpEnglish,SRpSpanish,SRpCatalan);

{$IFNDEF USEVARIANTS}
function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
function StrToBool(const S: string): Boolean;
function StrToBoolDef(const S: string; const Default: Boolean): Boolean;
function TryStrToBool(const S: string; out Value: Boolean): Boolean;

var
  TrueBoolStrs: array of String;
  FalseBoolStrs: array of String;

const
  DefaultTrueBoolStr = 'True';   // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE
{$ENDIF}

implementation

{$IFNDEF USEVARIANTS}

function TryStrToFloat(const S: string; out Value: Extended): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvExtended);
end;

procedure VerifyBoolStrArray;
begin
  if Length(TrueBoolStrs) = 0 then
  begin
    SetLength(TrueBoolStrs, 1);
    TrueBoolStrs[0] := DefaultTrueBoolStr;
  end;
  if Length(FalseBoolStrs) = 0 then
  begin
    SetLength(FalseBoolStrs, 1);
    FalseBoolStrs[0] := DefaultFalseBoolStr;
  end;
end;

function StrToBool(const S: string): Boolean;
begin
  if not TryStrToBool(S, Result) then
    Raise Exception.Create(SRpInvalidBoolean+S);
end;

function StrToBoolDef(const S: string; const Default: Boolean): Boolean;
begin
  if not TryStrToBool(S, Result) then
    Result := Default;
end;

function TryStrToBool(const S: string; out Value: Boolean): Boolean;
  function CompareWith(const aArray: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(aArray) to High(aArray) do
//      if AnsiSameText(S, aArray[I]) then
      if UpperCase(S)=UpperCase(aArray[I]) then
      begin
        Result := True;
        Break;
      end;
  end;
var
  LResult: Extended;
begin
  Result := TryStrToFloat(S, LResult);
  if Result then
    Value := LResult <> 0
  else
  begin
    VerifyBoolStrArray;
    Result := CompareWith(TrueBoolStrs);
    if Result then
      Value := True
    else
    begin
      Result := CompareWith(FalseBoolStrs);
      if Result then
        Value := False;
    end;
  end;
end;

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array [boolean] of String = ('0', '-1');
begin
  if UseBoolStrs then
  begin
    VerifyBoolStrArray;
    if B then
      Result := TrueBoolStrs[0]
    else
      Result := FalseBoolStrs[0];
  end
  else
    Result := cSimpleBoolStrs[B];
end;
{$ENDIF}


constructor TRpReportException.Create(AMessage:String;compo:TComponent);
begin
 FComponent:=compo;
 inherited Create(AMessage);
end;

function StreamCompare(Stream1:TStream;Stream2:TStream):Boolean;
const
 SIZE_BUF=4096;
var
 buf1,buf2:array [0..SIZE_BUF] of char;
 readcount:integer;
begin
 Result:=True;
 if Stream1.Size<>Stream2.Size then
 begin
  Result:=False;
  Exit;
 end;
 // If the same size then compare memory
 Stream1.Seek(0,soFromBeginning);
 Stream2.Seek(0,soFromBeginning);
 readcount:=Stream1.Read(buf1,SIZE_BUF);
 Stream2.Read(buf2,SIZE_BUF);
 while (readcount<>0) do
 begin
  if Not CompareMem(@buf1,@buf2,readcount) then
  begin
   result:=False;
   break;
  end;
  readcount:=Stream1.Read(buf1,SIZE_BUF);
  Stream2.Read(buf2,SIZE_BUF);
 end;
end;

{$IFDEF LINUX}
function CopyFileTo(const Source, Destination: string): Boolean;
var
  SourceStream: TFileStream;
begin
  result := false;
  if not FileExists(Destination) then begin
    SourceStream := TFileStream.Create(Source, fmOpenRead); try
      with TFileStream.Create(Destination, fmCreate) do try
        CopyFrom(SourceStream, 0);
      finally free; end;
    finally SourceStream.free; end;
    result := true;
  end;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
function CopyFileTo(const Source, Destination: string): Boolean;
begin
  Result := CopyFile(PChar(Source), PChar(Destination), true);
end;
{$ENDIF}


procedure Generatenewname(Component:TComponent);
var i:integer;
    name1:string;
begin
 i:=0;
 name1:=Component.ClassName;
 while (nil<>Component.Owner.FindComponent(name1+IntToStr(i))) do
  inc(i);
 Component.Name:=name1+IntToStr(i);
end;


function FormatVariant(displayformat:string;Value:Variant):widestring;
var
 atype:TVarType;
begin
 if VarIsNull(Value) then
 begin
  Result:='';
  exit;
 end;
 if Length(displayformat)<1 then
 begin
  Result:=widestring(Value);
  exit;
 end;
 atype:=VarType(value);
 case atype of
  varEmpty,varNull:
   Result:='';
  varSmallint,varInteger,varSingle,varDouble,varByte:
   Result:=FormatFloat(displayformat,Value);  varWord,varInt64,varLongWord,varShortInt:   Result:=FormatFloat(displayformat,Value);  varCurrency:   Result:=FormatCurr(displayformat,Value);  varDate:   Result:=FormatDateTime(displayformat,Value);  varString:   Result:=Format(displayformat,[Value]);  varBoolean:   Result:=Format(displayformat,[BoolToStr(Value,true)]);  else  begin{$IFDEF USEBCD}   if atype=varFmtBCD then    Result:=FormatBCD(displayformat,VarToBcd(Value))   else{$ENDIF}    Result:=SRpUnknownType;  end; end;{  varOleStr   = $0008;  varDispatch = $0009;  varError    = $000A;  varVariant  = $000C;  varUnknown  = $000D;  varStrArg   = $0048;  varAny      = $0101;  varTypeMask = $0FFF;  varArray    = $2000;  varByRef    = $4000;
}
end;


end.
