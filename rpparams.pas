{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpparams                                        }
{                                                       }
{       Parameter collection to assign to datasets      }
{       and expresion evaluator                         }
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

unit rpparams;

interface

{$I rpconf.inc}

uses Classes, SysUtils,rpmdconsts,
{$IFDEF USEVARIANTS}
  Variants,
{$ENDIF}
{$IFNDEF FORWEBAX}
 DB,
{$ENDIF}
 rptypes;

type

  TRpParam=class(TCollectionitem)
   private
    FName:string;
    FDescription:widestring;
    FSearch:WideString;
    FVisible:boolean;
    FAllowNulls:boolean;
    FValue:variant;
    FParamType:TRpParamType;
    FDatasets:TStrings;
    FItems:TStrings;
    FValues:TStrings;
    procedure SetVisible(AVisible:boolean);
    procedure SetAllowNulls(AAllowNulls:boolean);
    procedure SetName(AName:String);
    procedure SetValue(AValue:variant);
    procedure SetDescription(ADescription:widestring);
    procedure SetSearch(ASearch:widestring);
    procedure SetParamType(AParamType:TRpParamType);
    procedure WriteDescription(Writer:TWriter);
    procedure ReadSearch(Reader:TReader);
    procedure WriteSearch(Writer:TWriter);
    procedure ReadDescription(Reader:TReader);
    function GetAsString:WideString;
    procedure SetAsString(NewValue:WideString);
   protected
    procedure DefineProperties(Filer:TFiler);override;
   public
    LastValue:Variant;
    Constructor Create(Collection:TCollection);override;
    procedure Assign(Source:TPersistent);override;
    destructor Destroy;override;
    procedure SetDatasets(AList:TStrings);
    procedure SetItems(AList:TStrings);
    procedure SetValues(AList:TStrings);
{$IFNDEF FORWEBAX}
    function GetListValue:Variant;
{$ENDIF}
    property Description:widestring read FDescription write SetDescription;
    property Search:widestring read FSearch write SetSearch;
    property AsString:WideString read GetAsString write SetAsString;
   published
    property Name:string read FName write SetName;
    property Visible:Boolean read FVisible write SetVisible default True;
    property AllowNulls:Boolean read FAllowNulls write SetAllowNulls default True;
    property Value:Variant read FValue write SetValue;
    property ParamType:TRpParamType read FParamtype write SetParamType
     default rpParamString;
    property Datasets:TStrings read FDatasets write SetDatasets;
    property Items:TStrings read FItems write SetItems;
    property Values:TStrings read FValues write SetValues;
{$IFNDEF FORWEBAX}
    property ListValue:Variant read GetListValue;
{$ENDIF}
  end;

  TRpParamList=class(TCollection)
   private
    FReport:TComponent;
   public
    function GetItem(Index:Integer):TRpParam;
    procedure SetItem(index:integer;Value:TRpParam);
   public
    constructor Create(AOwner:TComponent);
    function Add(AName:String):TRpParam;
    function IndexOf(AName:String):integer;
    function FindParam(AName:string):TRpParam;
    function ParamByName(AName:string):TRpParam;
    property Items[index:integer]:TRpParam read GetItem write SetItem;default;
   end;

  TRpParamComp=class(TComponent)
   private
    fparams:TRpParamList;
    procedure SetParams(avalue:TRpParamList);
   public
    constructor Create(AOwner:TComponent);override;
   published
    property Params:TRpParamList read FParams write
     SetParams;
   end;

{$IFNDEF FORWEBAX}
function ParamTypeToDataType(paramtype:TRpParamType):TFieldType;
function VariantTypeToDataType(avariant:Variant):TFieldType;
{$ENDIF}

function ParamTypeToString(paramtype:TRpParamType):String;
function StringToParamType(Value:String):TRpParamType;
procedure GetPossibleDataTypes(alist:TRpWideStrings);
procedure GetPossibleDataTypesA(alist:TStrings);
procedure ParseCommandLineParams(params:TRpParamList);

implementation

{$IFNDEF FORWEBAX}
uses rpeval;
{$ENDIF}

procedure TRpParamComp.SetParams(avalue:TRpParamList);
begin
 fparams.Assign(avalue);
end;

constructor TRpParamComp.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 fparams:=TRpParamList.Create(Self);
end;

Constructor TRpParam.Create(Collection:TCollection);
begin
 inherited Create(Collection);
 FVisible:=true;
 FAllowNulls:=true;
 FParamType:=rpParamString;
 FDatasets:=TStringList.Create;
 FItems:=TStringList.Create;
 FValues:=TStringList.Create;
end;

procedure TRpParam.Assign(Source:TPersistent);
begin
 if (Source is TRpParam) then
 begin
  FName:=TRpParam(Source).FName;
  FVisible:=TRpParam(Source).FVisible;
  FAllowNulls:=TRpParam(Source).FAllowNulls;
  FDescription:=TRpParam(Source).FDescription;
  FSearch:=TRpParam(Source).FSearch;
  FValue:=TRpParam(Source).FValue;
  FParamType:=TRpParam(Source).FParamType;
  FDatasets.Assign(TRpParam(Source).FDatasets);
  FItems.Assign(TRpParam(Source).FItems);
  FValues.Assign(TRpParam(Source).FValues);
 end
 else
  inherited Assign(Source);
end;

destructor TRpParam.Destroy;
begin
 FDatasets.Free;
 FItems.Free;
 FValues.Free;
 inherited Destroy;
end;

procedure TRpParam.SetVisible(AVisible:boolean);
begin
 FVisible:=AVisible;
 Changed(false);
end;

procedure TRpParam.SetAllowNulls(AAllowNulls:boolean);
begin
 FAllowNulls:=AAllowNulls;
 Changed(false);
end;

procedure TRpParam.SetDatasets(AList:TStrings);
begin
 FDatasets.Assign(Alist);
 Changed(False);
end;

procedure TRpParam.SetItems(AList:TStrings);
begin
 FItems.Assign(Alist);
 Changed(False);
end;

procedure TRpParam.SetValues(AList:TStrings);
begin
 FValues.Assign(Alist);
 Changed(False);
end;

{$IFNDEF FORWEBAX}
function TRpParam.GetListValue:Variant;
var
 aexpression:String;
 aoption:integer;
begin
 if ParamType<>rpParamList then
  Result:=FValue
 else
 begin
  aoption:=0;
  if (VarType(FValue) in [varInteger,varSmallint,varShortint,varByte,varWord,varLongWord,
    varInt64]) then
  begin
   aoption:=FValue;
   if aoption<0 then
    aoption:=0;
  end
  else
  begin
   if ((VarType(FValue)=varString) or (VarType(FValue)=varOleStr)) then
   begin
    aoption:=fvalues.IndexOf(FValue);
    if aoption<0 then
     aoption:=0;
   end;
  end;
  if aoption>=fvalues.Count then
  begin
   Result:=Null;
  end
  else
  begin
   aexpression:=fvalues.strings[aoption];
   try
    Result:=rpeval.EvaluateExpression(aexpression);
   except
    on E:Exception do
    begin
{$IFDEF DOTNETD}
     Raise Exception.Create(E.Message+' - '+SRpParameter+':'+Name);
{$ENDIF}
{$IFNDEF DOTNETD}
     E.Message:=E.Message+' - '+SRpParameter+':'+Name;
     raise;
{$ENDIF}
    end;
   end;
  end;
 end;
end;
{$ENDIF}

procedure TRpParam.SetName(AName:String);
begin
 FName:=AnsiUpperCase(AName);
 Changed(false);
end;

procedure TRpParam.SetParamType(AParamType:TRpParamType);
begin
 FParamType:=AParamType;
 Changed(False);
end;

procedure TRpParam.SetDescription(ADescription:wideString);
begin
 FDescription:=ADescription;
 Changed(false);
end;

procedure TRpParam.SetSearch(ASearch:wideString);
begin
 FSearch:=ASearch;
 Changed(false);
end;

procedure TRpParam.SetValue(AValue:Variant);
begin
 if VarType(AValue)=varString then
 begin
  FValue:=WideString(AValue);
 end
 else
 begin
  // To be compatible with Delhi 4-5
  if VarType(AValue)=20 then
   FValue:=Integer(AValue)
  else
   FValue:=AValue;
 end;
 Changed(false);
end;


function TRpParamList.GetItem(Index:Integer):TRpParam;
begin
 Result:=TRpParam(inherited GetItem(index));
end;


procedure TRpParamList.SetItem(index:integer;Value:TRpParam);
begin
 inherited SetItem(Index,Value);
end;

constructor TRpParamList.Create(AOwner:TComponent);
begin
 inherited Create(TRpParam);
 FReport:=AOwner;
end;

function TRpParamList.Add(AName:String):TRpParam;
begin
 // Checks if it exists
 if IndexOf(AName)>0 then
  Raise Exception.Create(SRpAliasExists);
 Result:=TRpParam(inherited Add);
 Result.FName:=AName;
 Result.FVisible:=true;
 Result.FAllowNulls:=true;
 Result.FParamType:=rpParamString;
 Result.FValue:=Null;
end;


function TRpParamList.IndexOf(AName:String):integer;
var
 i:integer;
begin
 AName:=AnsiUpperCase(AName);
 Result:=-1;
 i:=0;
 While i<count do
 begin
  if items[i].FName=AName then
  begin
   Result:=i;
   break;
  end;
  inc(i);
 end;
end;

function TRpParamList.FindParam(AName:string):TRpParam;
var
 index:integer;
begin
 Result:=nil;
 index:=Indexof(AName);
 if index>=0 then
  Result:=items[index];
end;

function TRpParamList.ParamByName(AName:string):TRpParam;
var
 index:integer;
begin
 index:=Indexof(AName);
 if index<0 then
  Raise Exception.Create(SRpParamNotFound+AName);
 Result:=items[index];
end;


{$IFNDEF FORWEBAX}
function VariantTypeToDataType(avariant:Variant):TFieldType;
begin
 case VarType(avariant) of
  varEmpty,varNull:
   Result:=ftUnknown;
  varSmallInt,varInteger,varShortInt,varWord,varLongWord:
   Result:=ftInteger;
  varSingle,varDouble:
   Result:=ftFloat;
  varCurrency:
   Result:=ftCurrency;
  varDate:
   Result:=ftDate;
  varBoolean:
   Result:=ftBoolean;
  varInt64:
   Result:=ftLargeint;
  varString:
   Result:=ftString;
  varOleStr:
   Result:=ftWideString;
  else
   Result:=ftUnknown;
 end;
end;


function ParamTypeToDataType(paramtype:TRpParamType):TFieldType;
begin
  Result:=ftUnknown;
  case ParamType of
   rpParamString:
    Result:=ftString;
   rpParamInteger:
    Result:=ftInteger;
   rpParamDouble:
    Result:=ftFloat;
   rpParamCurrency:
    Result:=ftCurrency;
   rpParamDate:
    Result:=ftDate;
   rpParamTime:
    Result:=ftDateTime;
   rpParamDateTime:
    Result:=ftDateTime;
   rpParamBool:
    Result:=ftBoolean;
   rpParamExpreB:
    Result:=ftString;
   rpParamExpreA:
    Result:=ftString;
   rpParamSubst:
    Result:=ftString;
  end;
end;
{$ENDIF}

procedure TRpParam.WriteDescription(Writer:TWriter);
begin
 WriteWideString(Writer, FDescription);
end;

procedure TRpParam.ReadDescription(Reader:TReader);
begin
 FDescription:=ReadWideString(Reader);
end;

procedure TRpParam.WriteSearch(Writer:TWriter);
begin
 WriteWideString(Writer, FSearch);
end;

procedure TRpParam.ReadSearch(Reader:TReader);
begin
 FSearch:=ReadWideString(Reader);
end;

procedure TRpParam.DefineProperties(Filer:TFiler);
begin
 inherited;

 Filer.DefineProperty('Description',ReadDescription,WriteDescription,True);
 Filer.DefineProperty('Search',ReadSearch,WriteSearch,True);
end;

function TRpParam.GetAsString:WideString;
begin
 if Value=Null then
 begin
  Result:='';
  exit;
 end;
 case ParamType of
  rpParamUnknown:
   Result:='';
  rpParamString,rpParamExpreA,rpParamExpreB,rpParamSubst,rpParamList:
   Result:=String(Value);
  rpParamInteger,rpParamDouble,rpParamCurrency:
   Result:=FloatToStr(Value);
  rpParamDate:
   Result:=DateToStr(Value);
  rpParamTime:
   Result:=TimeToStr(Value);
  rpParamDateTime:
   Result:=DateTimeToStr(Value);
  rpParamBool:
   Result:=BoolToStr(Value,True);
 end;
end;

procedure TRpParam.SetAsString(NewValue:WideString);
begin
 case ParamType of
  rpParamString,rpParamExpreB,rpParamExpreA,rpParamSubst:
   Value:=NewValue;
  rpParamInteger,rpParamDouble,rpParamCurrency:
   Value:=StrToFloat(NewValue);
  rpParamDate:
   Value:=StrToDate(NewValue);
  rpParamTime:
   Value:=StrToTime(NewValue);
  rpParamDateTime:
   Value:=StrToDateTime(NewValue);
  rpParamBool:
   Value:=StrToBool(NewValue);
  rpParamUnknown:
   begin
    ParamType:=rpParamString;
    Value:=newValue;
   end;
 end;
end;

function ParamTypeToString(paramtype:TRpParamType):String;
begin
 case ParamType of
  rpParamString:
   Result:=SRpSString;
  rpParamExpreB:
   Result:=SrpSExpressionB;
  rpParamExpreA:
   Result:=SrpSExpressionA;
  rpParamSubst:
   Result:=SRpSParamSubs;
  rpParamInteger:
   Result:=SRpSInteger;
  rpParamDouble:
   Result:=SRpSFloat;
  rpParamCurrency:
   Result:=SRpSCurrency;
  rpParamDate:
   Result:=SRpSDate;
  rpParamTime:
   Result:=SRpSTime;
  rpParamDateTime:
   Result:=SRpSDateTime;
  rpParamBool:
   Result:=SRpSBoolean;
  rpParamList:
   Result:=SRpSParamList;
  rpParamUnknown:
   Result:=SRpSUnknownType;
 end;
end;


function StringToParamType(Value:String):TRpParamType;
begin
 Result:=rpParamUnknown;
 if Value=SRpSString then
 begin
  Result:=rpParamString;
  exit;
 end;
 if Value=SrpSExpressionA then
 begin
  Result:=rpParamExpreA;
  exit;
 end;
 if Value=SrpSExpressionB then
 begin
  Result:=rpParamExpreB;
  exit;
 end;
 if Value=SRpSParamSubs then
 begin
  Result:=rpParamSubst;
  exit;
 end;
 if Value=SRpSInteger then
 begin
  Result:=rpParamInteger;
  exit;
 end;
 if Value=SRpSFloat then
 begin
  Result:=rpParamDouble;
  exit;
 end;
 if Value=SRpSCurrency then
 begin
  Result:=rpParamCurrency;
  exit;
 end;
 if Value=SRpSDate then
 begin
  Result:=rpParamDate;
  exit;
 end;
 if Value=SRpSDateTime then
 begin
  Result:=rpParamDateTime;
  exit;
 end;
 if Value=SRpSTime then
 begin
  Result:=rpParamTime;
  exit;
 end;
 if Value=SRpSBoolean then
 begin
  Result:=rpParamBool;
  exit;
 end;
 if Value=SRpSParamList then
 begin
  Result:=rpParamList;
  exit;
 end;
end;

procedure GetPossibleDataTypesA(alist:TStrings);
var
 list:TRpWideStrings;
begin
 list:=TRpWideStrings.Create;
 try
  GetPossibleDataTypes(list);
  alist.Assign(TPersistent(list));
 finally
  list.free;
 end;
end;

procedure GetPossibleDataTypes(alist:TRpWideStrings);
begin
 alist.Clear;
 alist.Add(SRpSUnknownType);
 alist.Add(SRpSString);
 alist.Add(SRpSInteger);
 alist.Add(SRpSFloat);
 alist.Add(SRpSCurrency);
 alist.Add(SRpSDate);
 alist.Add(SRpSDateTime);
 alist.Add(SRpSTime);
 alist.Add(SRpSBoolean);
 alist.Add(SrpSExpressionB);
 alist.Add(SrpSExpressionA);
 alist.Add(SRpSParamSubs);
 alist.Add(SRpSParamList);
end;

// Command line params are in form of:
// -paramPARAMNAME=paramvalueinstringformat
procedure ParseCommandLineParams(params:TRpParamList);
var
 i:integer;
 aparam:String;
 paramname:String;
 paramvalue:String;
 index:integer;
begin
 for i:=1 to ParamCount do
 begin
  aparam:=ParamStr(i);
  if Pos('-param',aparam)=1 then
  begin
   aparam:=copy(aparam,7,Length(aparam));
   index:=Pos('=',aparam);
   paramname:=copy(aparam,1,index-1);
   paramvalue:=copy(aparam,index+1,Length(aparam));
   params.ParamByName(paramname).AsString:=paramvalue;
  end;
 end;
end;


end.
