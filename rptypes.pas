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

uses Sysutils,
{$IFDEF MSWINDOWS}
windows,
{$ENDIF}
Classes,rpconsts,Variants,FMTBcd;

const
 MAX_LANGUAGES=3;

type
 TRpTwips=integer;
 TRpImageDrawStyle=(rpDrawCrop,rpDrawStretch,rpDrawFull,rpDrawTile);
 TRpAggregate=(rpAgNone,rpAgGroup,rpAgPage,rpAgGeneral);
 TRpAggregateType=(rpagSum,rpagMin,rpagMax,rgagAvg,rpagStdDev);
 TRpReportChanged=(rpReportStart,rpDataChange,rpGroupChange);
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


implementation


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
  varSmallint,varInteger,varSingle,varWord,varDouble,
   varInt64,varLongWord,varShortInt,varByte:   Result:=FormatFloat(displayformat,Value);  varCurrency:   Result:=FormatCurr(displayformat,Value);  varDate:   Result:=FormatDateTime(displayformat,Value);  varString:   Result:=Format(displayformat,[Value]);  varBoolean:   Result:=Format(displayformat,[BoolToStr(Value,true)]);  else  begin   if atype=varFmtBCD then    Result:=FormatBCD(displayformat,VarToBcd(Value))   else    Result:=SRpUnknownType;  end; end;{  varOleStr   = $0008;  varDispatch = $0009;  varError    = $000A;  varVariant  = $000C;  varUnknown  = $000D;  varStrArg   = $0048;  varAny      = $0101;  varTypeMask = $0FFF;  varArray    = $2000;  varByRef    = $4000;
}
end;


end.
