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
 DB,rptypes;

type
  TRpParamtype=(rpParamString,rpParamInteger,rpParamDouble,rpParamDate,
   rpParamTime,rpParamDateTime,rpParamCurrency,rpParamBool,rpParamExpre);


  TRpParam=class(TCollectionitem)
   private
    FName:string;
    FDescription:widestring;
    FVisible:boolean;
    FValue:variant;
    FParamType:TRpParamType;
    FDatasets:TStrings;
    procedure SetVisible(AVisible:boolean);
    procedure SetName(AName:String);
    procedure SetValue(AValue:variant);
    procedure SetDescription(ADescription:widestring);
    procedure SetParamType(AParamType:TRpParamType);
    procedure WriteDescription(Writer:TWriter);
    procedure ReadDescription(Reader:TReader);
    function GetAsString:String;
    procedure SetAsString(NewValue:String);
   protected
    procedure DefineProperties(Filer:TFiler);override;
   public
    Constructor Create(Collection:TCollection);override;
    procedure Assign(Source:TPersistent);override;
    destructor Destroy;override;
    procedure SetDatasets(AList:TStrings);
    property Description:widestring read FDescription write SetDescription;
    property AsString:String read GetAsString write SetAsString;
   published
    property Name:string read FName write SetName;
    property Visible:Boolean read FVisible write SetVisible default True;
    property Value:Variant read FValue write SetValue;
    property ParamType:TRpParamType read FParamtype write SetParamType
     default rpParamString;
    property Datasets:TStrings read FDatasets write SetDatasets;
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

function ParamTypeToDataType(paramtype:TRpParamType):TFieldType;
function VariantTypeToDataType(avariant:Variant):TFieldType;

implementation

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
 FParamType:=rpParamString;
 FDatasets:=TStringList.Create;
end;

procedure TRpParam.Assign(Source:TPersistent);
begin
 if (Source is TRpParam) then
 begin
  FName:=TRpParam(Source).FName;
  FVisible:=TRpParam(Source).FVisible;
  FDescription:=TRpParam(Source).FDescription;
  FValue:=TRpParam(Source).FValue;
  FParamType:=TRpParam(Source).FParamType;
  FDatasets.Assign(TRpParam(Source).FDatasets);
 end
 else
  inherited Assign(Source);
end;

destructor TRpParam.Destroy;
begin
 FDatasets.Free;
 inherited Destroy;
end;

procedure TRpParam.SetVisible(AVisible:boolean);
begin
 FVisible:=AVisible;
 Changed(false);
end;

procedure TRpParam.SetDatasets(AList:TStrings);
begin
 FDatasets.Assign(Alist);
 Changed(False);
end;

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

procedure TRpParam.SetValue(AValue:Variant);
begin
 FValue:=AValue;
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


function VariantTypeToDataType(avariant:Variant):TFieldType;
begin
 Result:=ftUnknown;
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
    Result:=ftTime;
   rpParamBool:
    Result:=ftBoolean;
   rpParamExpre:
    Result:=ftString;
  end;
end;


procedure TRpParam.WriteDescription(Writer:TWriter);
begin
 WriteWideString(Writer, FDescription);
end;

procedure TRpParam.ReadDescription(Reader:TReader);
begin
 FDescription:=ReadWideString(Reader);
end;

procedure TRpParam.DefineProperties(Filer:TFiler);
begin
 inherited;

 Filer.DefineProperty('Description',ReadDescription,WriteDescription,True);
end;

function TRpParam.GetAsString:String;
begin
 if Value=Null then
 begin
  Result:='';
  exit;
 end;
 case ParamType of
  rpParamString,rpParamExpre:
   Result:=Value;
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

procedure TRpParam.SetAsString(NewValue:String);
begin
 case ParamType of
  rpParamString,rpParamExpre:
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
 end;
end;

end.
