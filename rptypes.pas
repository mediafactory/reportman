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

uses
{$IFDEF MSWINDOWS}
 Windows,
{$ENDIF}
 Sysutils,IniFiles,rpmdshfolder,Classes,
{$IFDEF USEVARIANTS}
 Variants,Types,
{$ENDIF}
{$IFDEF USEBCD}
 FMTBcd,
{$ENDIF}
 rpmdconsts;


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

 TRpOrientation=(rpOrientationDefault,rpOrientationPortrait,rpOrientationLandscape);

 TRpBidiMode=(rpBidiNo,rpBidiPartial,rpBidiFull);

 TRpParamtype=(rpParamString,rpParamInteger,rpParamDouble,rpParamDate,
  rpParamTime,rpParamDateTime,rpParamCurrency,rpParamBool,
  rpParamExpreB,rpParamExpreA,rpParamSubst,rpParamList,rpParamUnknown);


 TRpParamObject=class(TObject)
  public
   Value:Variant;
   Stream:TStream;
  end;

 TRpType1Font=(poHelvetica,poCourier,poTimesRoman,poSymbol,poZapfDingbats);
 TRpBrushStyle=(rpbsSolid, rpbsClear, rpbsHorizontal, rpbsVertical,
  rpbsFDiagonal, rpbsBDiagonal, rpbsCross, rpbsDiagCross, rpbsDense1,
  rpbsDense2, rpbsDense3, rpbsDense4, rpbsDense5, rpbsDense6, rpbsDense7);

 TRpTwips=integer;
 TRpImageDrawStyle=(rpDrawCrop,rpDrawStretch,rpDrawFull,rpDrawTile);
 TRpAggregate=(rpAgNone,rpAgGroup,rpAgPage,rpAgGeneral);
 TRpAggregateType=(rpagSum,rpagMin,rpagMax,rpagAvg,rpagStdDev);
 TRpReportChanged=(rpReportStart,rpDataChange,rpGroupChange,rpPageChange,rpInvalidateValue,rpSubReportStart);
 TRpShapeType=(rpsRectangle, rpsSquare, rpsRoundRect, rpsRoundSquare,
  rpsEllipse, rpsCircle,rpsHorzLine,rpsVertLine,rpsOblique1,rpsOblique2);

 TRpPrinterFontsOption=(rppfontsdefault,rppfontsalways,rppfontsnever); 
 TRpPageSize=(rpPageSizeDefault,rpPageSizeCustom,rpPageSizeUser);
 TPageSizeQt=record
  Indexqt:integer;
  Custom:boolean;
  CustomWidth:integer;
  CustomHeight:integer;
 end;

 TRpPrinterSelect=(pRpDefaultPrinter,pRpReportPrinter,
  pRpTicketPrinter,pRpGraphicprinter,
  pRpCharacterprinter,pRpReportPrinter2,
  pRpTicketPrinter2,pRpUserPrinter1,pRpUserPrinter2,
  pRpUserPrinter3,pRpUserPrinter4,pRpUserPrinter5,
  pRpUserPrinter6,pRpUserPrinter7,pRpUserPrinter8,
  pRpUserPrinter9);

 TRpColor=integer;

 // How to show preview
 TRpPreviewStyle = (spWide,spNormal,spEntirePage,spCustom);
 TRpPreviewWindowStyle=(spwNormal,spwMaximized);

 TPrinterRawOp=(rawopcutpaper,rawopopendrawer);

 TBitmapResizeEvent=procedure (awidth,aheight:integer;var scale:double) of object;

 TRpReportException=class(Exception)
  private
   FComponent:TComponent;
   FPropertyName:string;
  public
   constructor Create(AMessage:String;compo:TComponent;apropname:string);
   property Component:TComponent read FComponent;
   property PropertyName:string read FPropertyName;
  end;

  TRpWString = record
    WString: WideString;
  end;

  TRpWideStrings = class(TObject)
  private
    FWideList: TList;
    function GetString(Index: Integer): WideString;
    procedure PutString(Index: Integer; const S: WideString);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function  Count: Integer;
    procedure Insert(Index: Integer; const S: WideString);
    function Add(const S: WideString): Integer;
    property Strings[Index: Integer]: WideString read GetString write PutString;
  end;


// Compares 2 streams and returns true if they are equal
function StreamCompare(Stream1:TStream;Stream2:TStream):Boolean;
procedure WriteWideString(Writer:TWriter;Value:WideString);
function  ReadWideString(Reader:TReader):WideString;
procedure Generatenewname(Component:TComponent);
function FormatVariant(displayformat:string;Value:Variant;paramtype:TRpParamType;printnulls:boolean):widestring;
function CopyFileTo(const Source, Destination: string): Boolean;
function GetPrinterConfigName(printerindex:TRpPrinterSelect):string;
function GetPrinterOffset(printerindex:TRpPrinterSelect):TPoint;
function GetDeviceFontsOption(printerindex:TRpPrinterSelect):boolean;
function GetPrinterRawOp(printerindex:TRpPrinterSelect;rawop:TPrinterRawOp):string;
procedure FillTreeDir(adirectory:String;alist:TStringList);
function WideStringToDOS(astring:WideString):WideString;
function NumberToText(FNumero:currency;female:boolean;idiom:integer):String;
procedure GetLanguageDescriptions(alist:TStrings);
procedure GetBidiDescriptions(alist:TStrings);
function RpBidiModeToString(BidiMode:TRpBidiMode):String;
function StringToRpBidiMode(Value:String):TRpBidiMode;
function DoReverseString(Value:String):String;
function DoReverseStringW(Value:WideString):WideString;

{$IFNDEF USEVARIANTS}
procedure RaiseLastOSError;
{$ENDIF}

{$IFDEF MSWINDOWS}
function IsWindowsNT:Boolean;
{$ENDIF}



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

var
 printerconfigfile:TMemInifile;
{$IFDEF MSWINDOWS}
 osinfo:TOsVersionInfo;
{$ENDIF}


implementation

var
  cajpeg:array [0..10] of char=(chr($FF),chr($D8),chr($FF),chr($E0),chr($0),chr($10),'J','F','I','F',chr(0));

{$IFDEF MSWINDOWS}
var
 obtainedversion:Boolean;
{$ENDIF}


{$IFNDEF USEVARIANTS}


procedure RaiseLastOSError;
{$IFDEF FPC}
var
 LastError:Integer;
{$ENDIF}
begin
{$IFDEF FPC}
 LastError := GetLastError;
 if LastError <> 0 then
  Raise Exception.Create('SystemError: '+IntToStr(LastError));
//  Error := EOSError.CreateResFmt(@SOSError, [LastError,
//       SysErrorMessage(LastError)])
//   else
//     Error := EOSError.CreateRes(@SUnkOSError);
//   Error.ErrorCode := LastError;
//   raise Error;
// end;
{$ENDIF}
{$IFNDEF FPC}
 RaiseLastWin32Error;
{$ENDIF}
end;

function TryStrToFloat(const S: string; out Value: Extended): Boolean;
begin
{$IFNDEF FPC}
  Result := TextToFloat(PChar(S), Value, fvExtended);
{$ENDIF}
{$IFDEF FPC}
  Result:=true;
  try
   Value:=StrToFloat(S);
  except
   Result:=false;
  end;
{$ENDIF}
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


constructor TRpReportException.Create(AMessage:String;compo:TComponent;
 apropname:string);
begin
 FComponent:=compo;
 FPropertyName:=apropname;
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


function FormatVariant(displayformat:string;Value:Variant;
 paramtype:TRpParamType;printnulls:boolean):widestring;
var
 atype:TVarType;
begin
 if paramtype<>rpParamunknown then
 begin
  if VarIsNull(Value) then
  begin
   case paramtype of
    rpParamString,rpParamExpreB,rpParamExpreA,rpParamSubst:
     Value:='';
    rpParamInteger,rpParamDouble,rpParamCurrency:
     Value:=0;
    rpParamBool:
     Value:=false;
   end;
  end;
 end
 else
 begin
  if VarIsNull(Value) then
  begin
   Result:='';
   exit;
  end;
 end;
 if Not VarIsNull(Value) then
 begin
  if Length(displayformat)<1 then
  begin
   if printnulls then
   begin
    Result:=widestring(Value);
    exit;
   end;
  end;
 end;
 atype:=VarType(value);
 case atype of
  varEmpty,varNull:
   Result:='';
  varSmallint,varInteger,varByte:
   begin
    if ((Value=0) and (Not printnulls)) then
     Result:=''
    else
    begin
     if length(displayformat)<1 then
      Result:=widestring(Value)
     else
      Result:=FormatFloat(displayformat,Value);
    end;
   end;
  varWord,varInt64,varLongWord,varShortInt:
   begin
    if ((Value=0) and (Not printnulls)) then
     Result:=''
    else
    begin
     if length(displayformat)<1 then
      Result:=widestring(Value)
     else
      Result:=FormatFloat(displayformat,Value);
     end;
   end;
  varSingle,varDouble:
   begin
    if ((Value=0.0) and (Not printnulls)) then
     Result:=''
    else
    begin
     if length(displayformat)<1 then
      Result:=widestring(Value)
     else
      Result:=FormatFloat(displayformat,Value);
     end;
   end;
  varCurrency:
   begin
    if ((Value=0.0) and (Not printnulls)) then
     Result:=''
    else
    begin
     if length(displayformat)<1 then
      Result:=widestring(Value)
     else
      Result:=FormatCurr(displayformat,Value);
     end;
   end;
  varDate:
   Result:=FormatDateTime(displayformat,Value);
  varString:
   begin
    if Length(displayformat)>0 then
     Result:=Format(displayformat,[Value])
    else
     Result:=widestring(Value);
   end;
  varBoolean:
   Result:=Format(displayformat,[BoolToStr(Value,true)]);
  else
  begin
{$IFDEF USEBCD}
   if atype=varFmtBCD then
   begin
    if ( (BCDCompare(VarToBCD(Value),IntegerToBcd(0))=0) and (not printnulls)) then
     Result:=''
    else
     Result:=FormatBCD(displayformat,VarToBcd(Value));
   end
   else
{$ENDIF}
    Result:=SRpUnknownType;
  end;
 end;
 {  varOleStr   = $0008;  varDispatch = $0009;  varError    = $000A;  varVariant  = $000C;  varUnknown  = $000D;  varStrArg   = $0048;  varAny      = $0101;  varTypeMask = $0FFF;  varArray    = $2000;  varByRef    = $4000;}
end;

procedure CheckLoadedPrinterConfig;
var
 systemconfigfilename:string;
 userconfigfilename:string;
 configfilename:string;
begin
 if Not assigned(printerconfigfile) then
 begin
  systemconfigfilename:=Obtainininamecommonconfig('','','reportman');
  userconfigfilename:=Obtainininameuserconfig('','','reportman');
  if FileExists(systemconfigfilename) then
  begin
   configfilename:=systemconfigfilename;
  end
  else
  begin
   configfilename:=userconfigfilename;
  end;
  printerconfigfile:=TMemInifile.Create(configfilename);
 end;
end;

function GetDeviceFontsOption(printerindex:TRpPrinterSelect):boolean;
begin
 CheckLoadedPrinterConfig;
 if printerindex=pRpDefaultPrinter then
 begin
  Result:=printerconfigfile.ReadBool('PrinterFonts','Default',false);
  Exit;
 end;
 Result:=printerconfigfile.ReadBool('PrinterFonts','Printer'+IntToStr(integer(printerindex)),false);
end;

function GetPrinterConfigName(printerindex:TRpPrinterSelect):string;
begin
 CheckLoadedPrinterConfig;
 if printerindex=pRpDefaultPrinter then
 begin
  Result:='';
  Exit;
 end;
 Result:=printerconfigfile.ReadString('PrinterNames','Printer'+IntToStr(integer(printerindex)),'');
end;

function GetPrinterOffset(printerindex:TRpPrinterSelect):TPoint;
begin
 CheckLoadedPrinterConfig;
 Result.X:=printerconfigfile.ReadInteger('PrinterOffsetX','Printer'+IntToStr(integer(printerindex)),0);
 Result.Y:=printerconfigfile.ReadInteger('PrinterOffsetY','Printer'+IntToStr(integer(printerindex)),0);
end;

// A escape coded string is for example for Epson Tear off
// #27#50#27C#1#12
// That is escape code+ascci(50)...
function EscapeCodedToString(astring:string):string;
var
 index,i:integer;
begin
 astring:=astring+chr(0);
 result:='';
 index:=1;
 while length(astring)>=index do
 begin
   case astring[index] of
    '#':
     begin
      Inc(index);
      I := 0;
      while astring[index] in ['0'..'9'] do
      begin
       I := I * 10 + (Ord(astring[index]) - Ord('0'));
       Inc(index);
      end;
      result:=result+Chr(I);
     end;
    chr(0):
     begin
      inc(index);
     end;
   else
    begin
     result:=result+astring[index];
     inc(index);
    end;
  end;
 end;
end;


function GetPrinterRawOp(printerindex:TRpPrinterSelect;rawop:TPrinterRawOp):string;
var
 operation:String;
begin
 CheckLoadedPrinterConfig;
 Result:='';
 case rawop of
  rawopcutpaper:
   Operation:='CutPaper';
  rawopopendrawer:
   Operation:='OpenDrawer';
 end;
 // Check if active
 if Not printerconfigfile.ReadBool(Operation+'On','Printer'+IntToStr(integer(printerindex)),false) then
  exit;
 // If active decode and return result
 Result:=printerconfigfile.ReadString(Operation,'Printer'+IntToStr(integer(printerindex)),'');
 // Transform the string to a real string
 Result:=EscapeCodedToString(Result);
end;

procedure FillTreeDir(adirectory:string;alist:TStringList);
var
 adirlength:integer;

procedure IntFillTreeDir(adir:string);
var
 srec:TSearchRec;
 Attr:Integer;
 ares:Integer;
 sdir:string;
begin
 Attr:=faAnyfile;
 sdir:=adir+C_DIRSEPARATOR+'*.*';
 ares:=FindFirst(sdir,Attr,srec);
 try
  if ares=0 then
  begin
   repeat
    if ((srec.Attr AND faDirectory)>0) then
    begin
     if ((srec.Name<>'.') AND (srec.Name<>'..')) then
     begin
//      alist.Add(Copy(adir,adirlength,Length(adir))+C_DIRSEPARATOR+srec.Name);
      IntFillTreeDir(adir+C_DIRSEPARATOR+srec.Name);
     end;
    end
    else
    begin
     // Only adds the .rep files
     if ExtractFileExt(srec.Name)='.rep' then
      alist.Add(Copy(adir,adirlength+1,Length(adir))+C_DIRSEPARATOR+ChangeFileExt(srec.Name,''));
    end;
   until FindNext(srec)<>0;
  end;
 finally
  if ares=0 then
   FindClose(srec);
 end;
end;

begin
 adirlength:=Length(adirectory)+1;
 IntFillTreeDir(adirectory);
end;

constructor TRpWideStrings.Create;
begin
  FWideList := TList.Create;
end;

destructor TRpWideStrings.Destroy;
var
  i: Integer;
  PWStr: ^TRpWString;
begin
  for i := 0 to FWideList.Count-1 do
  begin
    PWStr := FWideList.Items[i];
    if PWStr <> nil then
      Dispose(PWStr);
  end;
  FWideList.Free;
  inherited Destroy;
end;

function TRpWideStrings.GetString(Index: Integer): WideString;
var
  PWStr: ^TRpWString;
begin
  Result := '';
  if ( (Index >= 0) and (Index < FWideList.Count) ) then
  begin
    PWStr := FWideList.Items[Index];
    if PWStr <> nil then
      Result := PWStr^.WString;
  end;
end;

procedure TRpWideStrings.PutString(Index: Integer; const S: WideString);
begin
  Insert(Index,S);
end;

function TRpWideStrings.Add(const S: WideString): Integer;
var
  PWStr: ^TRpWString;
begin
  New(PWStr);
  PWStr^.WString := S;
  Result := FWideList.Add(PWStr);
end;

function TRpWideStrings.Count: Integer;
begin
  Result := FWideList.Count;
end;

procedure TRpWideStrings.Clear;
var
  i: Integer;
  PWStr: ^TRpWString;
begin
  for i:=0 to FWideList.Count-1 do
  begin
    PWStr := FWideList.Items[i];
    if PWStr <> nil then
      Dispose(PWStr);
  end;
  FWideList.Clear;
end;

procedure TRpWideStrings.Insert(Index: Integer; const S: WideString);
var
  PWStr: ^TRpWString;
begin
  if((Index < 0) or (Index > FWideList.Count)) then
    raise Exception.Create(SRpIndexOutOfBounds);
  if Index < FWideList.Count then
  begin
    PWStr := FWideList.Items[Index];
    if PWStr <> nil then
      PWStr.WString := S;
  end
  else
    Add(S);
end;

{$IFDEF MSWINDOWS}
function IsWindowsNT:Boolean;
begin
 if Not obtainedversion then
 begin
   osinfo.dwOSVersionInfoSize:=sizeof(osinfo);
  if Not GetVersionEx(osinfo) then
   Raise Exception.Create(SRpError+' GetVersionEx');
  obtainedversion:=True;
 end;
 Result:=osinfo.dwPlatformId=VER_PLATFORM_WIN32_NT;
end;
{$ENDIF}


procedure WriteWideString(Writer:TWriter;Value:WideString);
var
  L: Integer;
  aval:TValueType;
begin
 aval:=vaWString;
 Writer.Write(aval,SizeOf(aval));
 L := Length(Value);
 Writer.Write(L, SizeOf(Integer));
 Writer.Write(Pointer(Value)^, L * 2);
end;

function ReadWideString(Reader:TReader):WideString;
begin
 Result:=Reader.ReadWideString;
end;


function WideStringToDOS(astring:WideString):WideString;
var
 i:integer;
 along:integer;
begin
 Result:='';
 i:=1;
 along:=Length(astring);
 while i<=along do
 begin
  // ignore #13
  while Ord(astring[i])=13 do
  begin
   inc(i);
   if i>along then
    exit;
  end;
  // Adds lines
  while Ord(astring[i])=10 do
  begin
   inc(i);
   Result:=Result+WideChar(13)+WideChar(10);
   if i>along then
    exit;
  end;
  if (Ord(astring[i])<>13) then
  begin
   Result:=Result+astring[i];
  end;
  inc(i);
 end;
end;


function NumberToTextCatalan(Fnumero:currency;female:boolean):String;
var s:String;
    centavos:Integer;
    i:Integer;
    Numero:Int64;
    fseparador:char;

     Function longitud(numero:LongInt):integer;
     {Esta funciÓn nos da la longitud del número que vamos a
     deletrear}
     begin
	  If numero div 10 =0 then
	     longitud:=1
	  else
	     longitud:=1+longitud(numero div 10);
     end;

     Function Unidades(numero:Integer):String;
     begin
	  case numero of
	  0: Unidades:='';
	  1:
	     If Not Female then
	      Unidades:='un'
	     else
	      Unidades:='una';
	  2: Unidades:='dos';
	  3: Unidades:='tres';
	  4: Unidades:='quatre';
	  5: Unidades:='cinc';
	  6: Unidades:='sis';
	  7: Unidades:='set';
	  8: Unidades:='vuit';
	  9: Unidades:='nou';
	  end;
     end;

     Function Decenas (numero:integer):String;
     begin
	  Case numero of
	  0:Decenas:='';
	  1..9:Decenas:=Unidades(numero);
	  10: Decenas:='deu';
	  11: Decenas:='onze';
	  12: Decenas:='dotze';
	  13: Decenas:='tretze';
	  14: Decenas:='catorze';
	  15: Decenas:='quinze';
	  16: Decenas:='setze';
	  17: Decenas:='diset';
	  18: Decenas:='divuit';
	  19: Decenas:='dinou';
	  20: Decenas:='vint';
	  21: If Not Female then
	       Decenas:='veint-i-un'
              else
	       Decenas:='veint-i-una';
	  22: Decenas:='vint-i-dos';
	  23: Decenas:='vint-i-tres';
	  24..29: Decenas:='vint-i-'+Unidades(numero mod 10);
	  30: Decenas:='trenta';
	  31: If Not Female then
	       Decenas:='trenta-un'
              else
	       Decenas:='trenta-una';
	  40: Decenas:='quaranta';
	  41: If Not Female then
	       Decenas:='quaranta-un'
              else
	       Decenas:='quaranta-una';
	  50: Decenas:='cinquanta';
	  51: If Not Female then
	       Decenas:='cinquanta-un'
              else
	       Decenas:='cinquanta-una';
	  60: Decenas:='seixanta';
	  61: If Not Female then
	       Decenas:='seixanta-un'
              else
	       Decenas:='seixanta-una';
	  70: Decenas:='setenta';
	  71: If Not Female then
	       Decenas:='setanta-un'
              else
	       Decenas:='setanta-una';
	  80: Decenas:='vuitanta';
	  81: If Not Female then
	       Decenas:='vuitanta-un'
              else
	       Decenas:='vuitanta-una';
	  90: Decenas:='noranta';
	  91: If Not Female then
	       Decenas:='noranta-un'
              else
	       Decenas:='noranta-una';
	  else
	       Decenas:=Decenas(numero - numero mod 10)+'-'+ unidades(numero mod 10);
	  end;
     end;
     Function centenas(numero:integer):String;
     begin
	  case numero of
	  0: centenas:='';
	  1..99:centenas:=decenas(numero);
	  100: centenas:='cent';
	  101..199: centenas:='cent '+decenas(numero mod 100);
	  200: If Not Female then
		centenas:='dos-cents'
	       else
		centenas:='dos-centes';
	  500: If Not Female then
		centenas:='cinc-cents'
	       else
		centenas:='cinc-centes';
	  501..599:
	       If Not Female then
		centenas:='cinc-cents '+decenas(numero mod 100)
	       else
		centenas:='cinc-centes '+decenas(numero mod 100);

	  700: If Not Female then
		centenas:='set-cents'
	       else
		centenas:='set-centes';

	  701..799:
	       If Not Female then
		centenas:='set-cents '+decenas(numero mod 100)
	       else
		centenas:='set-centes '+decenas(numero mod 100);
	  900..999:
	       If Not Female then
		centenas:='nou-cents '+decenas(numero mod 100)
	       else
		centenas:='nou-centes '+decenas(numero mod 100);
	  else
	   If Not Female then
	    centenas:=unidades(numero div 100)+'cents'+' '+decenas(numero mod 100)
	   else
	      centenas:=unidades(numero div 100)+'centes'+' '+decenas(numero mod 100);
	  end;
     end;

     Function UnidadesDeMillar(numero:Integer):String;
     begin
     if numero > 999 then begin
	  if numero > 1999 then
	       UnidadesDeMillar:=Unidades(numero div 1000) +' mil '+ centenas(numero mod 1000)
	  else
	      UnidadesDeMillar:='mil '+ centenas(numero mod 1000);
     end
     else
	 UnidadesDeMillar:=Centenas(numero);
     end;

     Function DecenasDeMillar(Numero:LongInt):String;
     begin
     If numero > 9999 then
	 DecenasDeMillar:=Decenas(Numero div 1000) +' mil '+Centenas(Numero mod 1000)
     else
	 DecenasDeMillar:=UnidadesDeMillar(Numero);
     end;

     Function CentenasDeMillar(Numero:LongInt):String;
     begin
	  If Numero > 99999 then
	       CentenasDeMillar:=Centenas(Numero div 1000) +' mil '+Centenas(numero mod 1000)
	  else
	       CentenasDeMillar:=DecenasDeMillar(numero);
     end;

     Function UnidadesDeMillon(Numero:LongInt):String;
     begin
	  if numero > 1999999 then
	  UnidadesDeMillon:=Unidades(Numero div 1000000)+' milions '+CentenasDeMillar(Numero mod 1000000)
	  else
	  UnidadesDeMillon:= 'un milió '+CentenasDeMillar(Numero mod 1000000)
     end;

     Function DecenasDeMillon(Numero:LongInt):String;
     var tmp,c:String;
	 i:Byte;
     begin
	  tmp:= Decenas(Numero div 1000000);
	  i:=Length(tmp);
	  c:= tmp[i-1]+tmp[i];
	  if c='na' then
	     SetLength(tmp,i-1);
	  DecenasDeMillon:=tmp+ ' milions '+CentenasDeMillar(Numero mod 1000000)
     end;

     Function CentenasDeMillon(Numero:LongInt):String;
     var tmp:String;
	 c:String;
	 i:byte;
     begin
     {Pasamos del femenino al masculino en las centenas}
       tmp:=Centenas(Numero div 1000000);
       c:=tmp[1];
       i:=1;
       While ( (c<>' ') and (length(tmp)>=i) ) do
       begin
	    if (tmp[i]='a') and (tmp[i+1] ='s') then begin
	       tmp[i]:='o';
	       break;
	    end;
	    inc(i);
       end;
       i:=Length(tmp);
       c:= tmp[i-1]+tmp[i];
       if c='na' then
	     SetLength(tmp,i-1);
       CentenasDeMillon:=tmp+' milions '+CentenasDeMillar(Numero mod 1000000)
     end;

begin
FNumero:=Abs(Fnumero);
fseparador:=decimalseparator;
s:=FormatFloat('##########0.00',Fnumero);
i:=pos(Fseparador,s);
numero:=StrToInt(copy(s,1,i-1));
s:=copy(s,i+1,Length(s));
centavos:=StrToInt(s);
if Length(s)=1 then centavos:=centavos*10;
Case Longitud(numero) of
     1: s:=Unidades(numero);
     2: s:=Decenas(numero);
     3: s:=Centenas(numero);
     4: s:=UnidadesDeMillar(numero);
     5: s:=DecenasDeMillar(numero);
     6: s:=CentenasDeMillar(numero);
     7: s:=UnidadesDeMillon(numero);
     8: s:=DecenasdeMillon(numero);
     9: s:=CentenasDeMillon(numero);
     else
	 s:='Demasiado grande';
     end;
If (i<>0) and (centavos>0) then begin
 case longitud(centavos) of
   1: Result:=Unidades(Centavos);
   2: Result:=Decenas(Centavos);
 end;
 Result:=s+' con '+Result;
end else
 Result:=s;
end;

function NumberToTextCastellano(FNumero:currency;female:boolean):String;
var s:String;
    centavos:Integer;
    i:Integer;
    Numero:Int64;
    fseparador:char;

     Function longitud(numero:LongInt):integer;
     {Esta funciÓn nos da la longitud del número que vamos a
     deletrear}
     begin
	  If numero div 10 =0 then
	     longitud:=1
	  else
	     longitud:=1+longitud(numero div 10);
     end;

     Function Unidades(numero:Integer):String;
     begin
	  case numero of
	  0: Unidades:='';
	  1:
	     If Not Female then
	      Unidades:='un'
	     else
	      Unidades:='una';
	  2: Unidades:='dos';
	  3: Unidades:='tres';
	  4: Unidades:='cuatro';
	  5: Unidades:='cinco';
	  6: Unidades:='seis';
	  7: Unidades:='siete';
	  8: Unidades:='ocho';
	  9: Unidades:='nueve';
	  end;
     end;

     Function Decenas (numero:integer):String;
     begin
	  Case numero of
	  0:Decenas:='';
	  1..9:Decenas:=Unidades(numero);
	  10: Decenas:='diez';
	  11: Decenas:='once';
	  12: Decenas:='doce';
	  13: Decenas:='trece';
	  14: Decenas:='catorce';
	  15: Decenas:='quince';
	  16: Decenas:='dieciséis';
	  17: Decenas:='diecisiete';
	  18: Decenas:='dieciocho';
	  19: Decenas:='diecinueve';
	  20: Decenas:='veinte';
	  21: Decenas:='veintiuna';
	  22: Decenas:='veintidós';
	  23: Decenas:='veintitrés';
	  24..29: Decenas:='veinti'+Unidades(numero mod 10);
	  30: Decenas:='treinta';
	  40: Decenas:='cuarenta';
	  50: Decenas:='cincuenta';
	  60: Decenas:='sesenta';
	  70: Decenas:='setenta';
	  80: Decenas:='ochenta';
	  90: Decenas:='noventa';
	  else
	       Decenas:=Decenas(numero - numero mod 10)+' y '+ unidades(numero mod 10);
	  end;
     end;
     Function centenas(numero:integer):String;
     begin
	  case numero of
	  0: centenas:='';
	  1..99:centenas:=decenas(numero);
	  100: centenas:='cien';
	  101..199: centenas:='ciento '+decenas(numero mod 100);
	  200: If Not Female then
		centenas:='doscientos'
	       else
		centenas:='doscientas';
	  500: If Not Female then
		centenas:='quinientos'
	       else
		centenas:='quinientas';
	  501..599:
	       If Not Female then
		centenas:='quinientos '+decenas(numero mod 100)
	       else
		centenas:='quinientas '+decenas(numero mod 100);

	  700: If Not Female then
		centenas:='setecientos'
	       else
		centenas:='setecientas';

	  701..799:
	       If Not Female then
		centenas:='setecientos '+decenas(numero mod 100)
	       else
		centenas:='setecientas '+decenas(numero mod 100);
	  900..999:
	       If Not Female then
		centenas:='novecientos '+decenas(numero mod 100)
	       else
		centenas:='novecientas '+decenas(numero mod 100);
	  else
	   If Not Female then
	    centenas:=unidades(numero div 100)+'cientos'+' '+decenas(numero mod 100)
	   else
	      centenas:=unidades(numero div 100)+'cientas'+' '+decenas(numero mod 100);
	  end;
     end;

     Function UnidadesDeMillar(numero:Integer):String;
     begin
     if numero > 999 then begin
	  if numero > 1999 then
	       UnidadesDeMillar:=Unidades(numero div 1000) +' mil '+ centenas(numero mod 1000)
	  else
	      UnidadesDeMillar:='mil '+ centenas(numero mod 1000);
     end
     else
	 UnidadesDeMillar:=Centenas(numero);
     end;

     Function DecenasDeMillar(Numero:LongInt):String;
     begin
     If numero > 9999 then
	 DecenasDeMillar:=Decenas(Numero div 1000) +' mil '+Centenas(Numero mod 1000)
     else
	 DecenasDeMillar:=UnidadesDeMillar(Numero);
     end;

     Function CentenasDeMillar(Numero:LongInt):String;
     begin
	  If Numero > 99999 then
	       CentenasDeMillar:=Centenas(Numero div 1000) +' mil '+Centenas(numero mod 1000)
	  else
	       CentenasDeMillar:=DecenasDeMillar(numero);
     end;

     Function UnidadesDeMillon(Numero:LongInt):String;
     begin
	  if numero > 1999999 then
	  UnidadesDeMillon:=Unidades(Numero div 1000000)+' millones '+CentenasDeMillar(Numero mod 1000000)
	  else
	  UnidadesDeMillon:= 'un millón '+CentenasDeMillar(Numero mod 1000000)
     end;

     Function DecenasDeMillon(Numero:LongInt):String;
     var tmp,c:String;
	 i:Byte;
     begin
	  tmp:= Decenas(Numero div 1000000);
	  i:=Length(tmp);
	  c:= tmp[i-1]+tmp[i];
	  if c='na' then
	     SetLength(tmp,i-1);
	  DecenasDeMillon:=tmp+ ' millones '+CentenasDeMillar(Numero mod 1000000)
     end;

     Function CentenasDeMillon(Numero:LongInt):String;
     var tmp:String;
	 c:String;
	 i:byte;
     begin
     {Pasamos del femenino al masculino en las centenas}
       tmp:=Centenas(Numero div 1000000);
       c:=tmp[1];
       i:=1;
       While ( (c<>' ') and (length(tmp)>=i) ) do
       begin
	    if (tmp[i]='a') and (tmp[i+1] ='s') then begin
	       tmp[i]:='o';
	       break;
	    end;
	    inc(i);
       end;
       i:=Length(tmp);
       c:= tmp[i-1]+tmp[i];
       if c='na' then
	     SetLength(tmp,i-1);
       CentenasDeMillon:=tmp+' millones '+CentenasDeMillar(Numero mod 1000000)
     end;

begin
FNumero:=Abs(Fnumero);
fseparador:=decimalseparator;
s:=FormatFloat('##########0.00',Fnumero);
i:=pos(Fseparador,s);
numero:=StrToInt(copy(s,1,i-1));
s:=copy(s,i+1,Length(s));
centavos:=StrToInt(s);
if Length(s)=1 then centavos:=centavos*10;
Case Longitud(numero) of
     1: s:=Unidades(numero);
     2: s:=Decenas(numero);
     3: s:=Centenas(numero);
     4: s:=UnidadesDeMillar(numero);
     5: s:=DecenasDeMillar(numero);
     6: s:=CentenasDeMillar(numero);
     7: s:=UnidadesDeMillon(numero);
     8: s:=DecenasdeMillon(numero);
     9: s:=CentenasDeMillon(numero);
     else
	 s:='Demasiado grande';
     end;
If (i<>0) and (centavos>0) then begin
 case longitud(centavos) of
   1: Result:=Unidades(Centavos);
   2: Result:=Decenas(Centavos);
 end;
 Result:=s+' con '+Result;
end else
 Result:=s;
end;


function NumberToText(FNumero:currency;female:boolean;idiom:integer):String;
begin
 Result:='';
 case idiom of
  1:
   Result:=NumberToTextCastellano(FNumero,female);
  2:
   Result:=NumberToTextCatalan(FNumero,female);
 end;
end;


procedure GetLanguageDescriptions(alist:TStrings);
//var
// rplangids:array [0..MAX_LANGUAGES-1] of string=('EN','ES','CAT','FR');
begin
 alist.Clear;
 alist.Add(SRpEnglish);
 alist.Add(SRpSpanish);
 alist.Add(SRpCatalan);
 alist.Add(SRpFrench);
end;


procedure GetBidiDescriptions(alist:TStrings);
begin
 alist.Clear;
 alist.Add(SRpSBidiNo);
 alist.Add(SRpSBidiPartial);
 alist.Add(SRpSBidiFull);
end;

function RpBidiModeToString(BidiMode:TRpBidiMode):String;
begin
 Result:=SRpSBidiNo;
 if BidiMode=rpBidiPartial then
  Result:=SRpSBidiPartial
 else
  if BidiMode=rpBidiFull then
   Result:=SRpSBidiFull;
end;

function StringToRpBidiMode(Value:String):TRpBidiMode;
begin
 Result:=rpBidiNo;
 if Value=SRpSBidiPartial then
  Result:=rpBidiPartial
 else
  if Value=SRpSBidiFull then
   Result:=rpBidiFull;
end;

function DoReverseString(Value:String):String;
var
 i:integer;
begin
 Result:='';
 for i:=1 to Length(Value) do
 begin
  Result:=Value[i]+Result;
 end;
end;

function DoReverseStringW(Value:WideString):WideString;
var
 i:integer;
begin
 Result:='';
 for i:=1 to Length(Value) do
 begin
  Result:=Value[i]+Result;
 end;
end;


initialization

{$IFDEF MSWINDOWS}
obtainedversion:=false;
{$ENDIF}
printerconfigfile:=nil;

finalization

if assigned(printerconfigfile) then
begin
 printerconfigfile.free;
 printerconfigfile:=nil;
end;

end.
