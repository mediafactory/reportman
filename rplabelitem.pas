{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rplabelitem                                     }
{       TRpLabel printable component constant text      }
{       TRpExpression printable expression              }
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


unit rplabelitem;

interface


{$I rpconf.inc}

uses Sysutils,Classes,rptypes,rpprintitem,rpmdconsts,
 rpmetafile,rpeval,rpparams,
{$IFDEF MSWINDOWS}
 windows,
{$ENDIF}
{$IFDEF USEVARIANTS}
  Variants,Types,
{$ENDIF}
 rptypeval,math;

const
 AlignmentFlags_SingleLine=64;

type
 TRpLabel=class(TRpGenTextComponent)
  private
   FAllText:TStrings;
   FAllStrings:TRpWideStrings;
   FWideText:WideString;
   procedure SetText(Value:WideString);
   function GetText:WideString;
   procedure SetAllText(Value:TStrings);
   procedure UpdateAllStrings;
   procedure WriteWideText(Writer:TWriter);
   procedure ReadWideText(Reader:TReader);
  protected
   procedure DefineProperties(Filer:TFiler);override;
   procedure DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);override;
   procedure Loaded;override;
  public
   procedure UpdateWideText;
   property AllStrings:TRpWideStrings read FAllStrings write FAllStrings;
   constructor Create(AOwner:TComponent);override;
   property Text:widestring read GetText write SetText;
   destructor Destroy;override;
   property WideText:WideString read FWideText write FWideText;
  published
   // Compatibility with RC1,RC2
   property AllText:TStrings read FAllText write SetAllText;
  end;

 TIdenRpExpression=class;

 TRpExpression=class(TRpGenTextComponent)
  private
   FExpression:widestring;
   FGroupName:string;
   FAggregate:TRpAggregate;
   FAgType:TRpAggregateType;
   FIdentifier:string;
   FAutoExpand:Boolean;
   FAutoContract:Boolean;
   FDisplayFormat:string;
   FDataType:TRpParamType;
   FValue:Variant;
   FSumValue:Variant;
   FDataCount:integeR;
   FUpdated:boolean;
   FAgIniValue:widestring;
   FValues:array of Double;
   FPrintOnlyOne:boolean;
   FIdenExpression:TIdenRpExpression;
   FOldString:widestring;
   FPrintNulls:boolean;
   procedure SetIdentifier(Value:string);
   procedure Evaluate;
   procedure WriteExpression(Writer:TWriter);
   procedure ReadExpression(Reader:TReader);
   procedure WriteAgIniValue(Writer:TWriter);
   procedure ReadAgIniValue(Reader:TReader);
  protected
   procedure DefineProperties(Filer:TFiler);override;
   procedure DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);override;
  public
   constructor Create(AOwner:TComponent);override;
   procedure SubReportChanged(newstate:TRpReportChanged;newgroup:string='');override;
   function GetText:widestring;
   property IdenExpression:TIdenRpExpression read FIdenExpression;
   function GetExtension(adriver:IRpPrintDriver):TPoint;override;
   property Expression:widestring read FExpression write FExpression;
   property AgIniValue:widestring read FAgIniValue write FAgIniValue;
  published
   property DataType:TRpParamType read FDataType write FDataType default rpParamUnknown;
   property DisplayFormat:string read FDisplayformat write FDisplayFormat;
   property Identifier:string read FIdentifier write SetIdentifier;
   property Aggregate:TRpAggregate read FAggregate write FAggregate
    default rpagNone;
   property GroupName:string read FGroupName write FGroupName;
   property AgType:TRpAggregateType read FAgType write FAgType
    default rpAgSum;
   property AutoExpand:Boolean read FAutoExpand write FAutoExpand;
   property AutoContract:Boolean read FAutoContract write FAutoContract;
   property PrintOnlyOne:Boolean read FPrintOnlyOne write FPrintOnlyOne
    default false;
   property PrintNulls:Boolean read FPrintNulls write FPrintNulls default true;
  end;

  TIdenRpExpression=class(TIdenFunction)
  private
   FExpreitem:TRpExpression;
  protected
   function GeTRpValue:TRpValue;override;
  public
  end;


implementation

uses rpreport;

function TIdenRpExpression.GeTRpValue:TRpValue;
begin
 if Not Assigned(FExpreItem) then
  Raise Exception.Create(SRpErrorIdenExpression);
 FExpreitem.Evaluate;
 Result:=FExpreitem.FValue;
end;

constructor TRpLabel.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 Height:=275;
 Width:=1440;
 FAllText:=TStringList.Create;
 FAllStrings:=TRpWideStrings.Create;
end;

destructor TRpLabel.Destroy;
begin
 FAllText.free;
 FAllStrings.Free;
 inherited destroy;
end;



procedure TRpLabel.SetText(Value:WideString);
var
 langindex:integer;
 acopy:WideString;
begin
 langindex:=TRpReport(GetReport).Language+1;
 if langindex<0 then
  langindex:=0;
 acopy:='';
 if FAllStrings.Count>0 then
  acopy:=FAllStrings.Strings[0];
 while ((FAllStrings.Count-1)<langindex) do
 begin
  FAllStrings.Add(acopy);
 end;
 FAllStrings.Strings[langindex]:=Value;
 UpdateWideText;
end;


function TRpLabel.GetText:WideString;
var
 langindex:integer;
 acopy:WideString;
begin
 langindex:=TRpReport(GetReport).Language+1;
 if langindex<0 then
  langindex:=0;
 acopy:='';
 if FAllStrings.Count>0 then
  acopy:=FAllStrings.Strings[0];
 if FAllStrings.Count>langindex then
  Result:=FAllStrings.Strings[langindex]
 else
  Result:=acopy;
end;

procedure TRpLabel.SetAllText(Value:TStrings);
var
 i:integer;
begin
 FAllText.Assign(Value);
 if FAllText.Count>0 then
 begin
  FAllStrings.Clear;
  for i:=0 to FAllText.Count-1 do
  begin
   FAllStrings.Add(FAllText.Strings[i]);
  end;
  FAllText.Clear;
  UpdateWideText;
 end
end;

procedure TRpLabel.Loaded;
var
 i:integer;
begin
 inherited Loaded;
 if FAllText.Count>0 then
 begin
  FAllStrings.Clear;
  for i:=0 to FAllText.Count-1 do
  begin
   FAllStrings.Add(FAllText.Strings[i]);
  end;
  FAllText.Clear;
  UpdateWideText;
 end
 else
  UpdateAllStrings;
end;


procedure TRpLabel.UpdateWideText;
var
 i:integer;
begin
 FWideText:='';
 for i:=0 to FAllStrings.Count-1 do
 begin
  FWideText:=FWideText+FAllStrings.Strings[i];
  if i<FAllStrings.Count-1 then
   FWideText:=FWideText+WideChar(chr(10));
 end;
end;

procedure TRpLabel.UpdateAllStrings;
var
 i:integer;
 tempwide:WideString;
 alength:integer;
begin
 FAllStrings.Clear;
 i:=1;
 tempwide:='';
 alength:=Length(FWideText);
 while i<=alength do
 begin
  if FWideText[i]=WideChar(chr(10)) then
  begin
   FAllStrings.Add(tempwide);
   tempwide:='';
  end
  else
   tempwide:=tempwide+FWideText[i];
  inc(i);
 end;
 if Length(tempwide)>0 then
  FAllStrings.Add(tempwide);
end;


constructor TRpExpression.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FPrintNulls:=true;
 Height:=275;
 FAgIniValue:='0';
 Width:=1440;
 FIdenExpression:=TIdenRpExpression.Create(Self);
 FIdenExpression.FExpreitem:=Self;
 FDataType:=rpParamUnknown;
end;

procedure TRpLabel.DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);
var
 aalign:integer;
begin
 inherited DoPrint(aposx,aposy,metafile);
 aalign:=Alignment or VAlignment;
 if SingleLine then
  aalign:=aalign or AlignmentFlags_SingleLine;
 metafile.Pages[metafile.CurrentPage].NewTextObject(aposy,
  aposx,width,height,Text,WFontName,LFontName,FontSize,FontRotation,
  FontStyle,smallint(Type1Font),FOntColor,BackColor,Transparent,CutText,aalign,WordWrap);
end;


procedure TRpExpression.SetIdentifier(Value:string);
var
 fidens:TStringList;
 index:integer;
begin
 if (csloading in componentstate) then
 begin
  FIdentifier:=Value;
  exit;
 end;
 // Check if the identifier is used
 Value:=UpperCase(Trim(Value));
 if Value=FIdentifier then
  exit;
 fidens:=TRpReport(GetReport).Identifiers;
 index:=fidens.IndexOf(Value);
 if index>=0 then
  Raise Exception.Create(SRpIdentifierAlreadyExists);
 // Erases previous identifier
 index:=fidens.IndexOf(FIdentifier);
 if index>=0 then
  fidens.Delete(index);
 FIdentifier:=Value;
 if Length(FIdentifier)>0 then
 begin
  fidens.AddObject(FIdentifier,self);
 end;
end;

procedure TRpExpression.Evaluate;
var
 fevaluator:TRpEvaluator;
begin
 if FUpdated then
  exit;
 try
  fevaluator:=TRpREport(GetReport).Evaluator;
//  fevaluator.Expression:=FExpression;
//  fevaluator.Evaluate;
//  FValue:=fevaluator.EvalResult;
  FValue:=fevaluator.EvaluateText(FExpression);
  FUpdated:=true;
 except
  on E:Exception do
  begin
   Raise TRpReportException.Create(E.Message+':'+SRpSExpression+' '+Name,self,SRpSExpression);
  end;
 end;
end;

function TRpExpression.GetText:widestring;
var
 expre:WideString;
begin
 expre:=Trim(Expression);
 if Length(expre)<1 then
 begin
  Result:='';
  exit;
 end;
 // Is Total pages variable?
 if (UpperCase(expre)='PAGECOUNT') then
 begin
  // 20 spaces
  Result:='                    ';
 end
 else
 begin
  Evaluate;
  Result:=FormatVariant(displayformat,FValue,FDataType,FPrintNulls);
 end;
end;

procedure TRpExpression.DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);
var
 aText:WideString;
 expre:WideString;
 aalign:integer;
begin
 inherited DoPrint(aposx,aposy,metafile);
 expre:=Trim(Expression);
 aText:=GetText;
 if PrintOnlyOne then
 begin
  if FOldString=aText then
   exit;
  FOldString:=aText;
 end;
 aalign:=Alignment or VAlignment;
 if SingleLine then
  aalign:=aalign or AlignmentFlags_SingleLine;
 metafile.Pages[metafile.CurrentPage].NewTextObject(aposy,
   aposx,width,height,aText,WFontName,LFontName,FontSize,FontRotation,
   FontStyle,smallint(Type1Font),FOntColor,BackColor,Transparent,CutText,aalign,WordWrap);
 // Is Total pages variable?
 if (UpperCase(expre)='PAGECOUNT') then
 begin
  TRpReport(GetReport).AddTotalPagesItem(metafile.currentpage,metafile.Pages[metafile.currentpage].ObjectCount-1,displayformat);
 end;
end;

procedure TRpExpression.SubReportChanged(newstate:TRpReportChanged;newgroup:string='');
var
 eval:TRpEvaluator;
begin
 inherited SubReportChanged(newstate,newgroup);
 case newstate of
  rpReportStart:
   begin
    FOldString:='';
    FUpdated:=false;
    FDataCount:=0;
    if (FAggregate<>rpAgNone) then
    begin
     // Update with the initial value
     eval:=TRpReport(GetReport).Evaluator;
     eval.Expression:=FAgIniValue;
     eval.Evaluate;
     FValue:=eval.EvalResult;
     FSumValue:=FValue;
     FUpdated:=true;
    end;
   end;
  rpSubReportStart:
   begin
    FOldString:='';
    FUpdated:=false;
    FDataCount:=0;
    if ((FAggregate<>rpAgNone) AND (FAggregate<>rpAgGeneral)) then
    begin
     // Update with the initial value
     eval:=TRpReport(GetReport).Evaluator;
     eval.Expression:=FAgIniValue;
     eval.Evaluate;
     FValue:=eval.EvalResult;
     FSumValue:=FValue;
     FUpdated:=true;
    end;
   end;
  rpDataChange:
   begin
    FUpdated:=false;
    inc(FDataCount);
    if (FAggregate<>rpAgNone) then
    begin
     // Update with the initial value
     eval:=TRpReport(GetReport).Evaluator;
     eval.Expression:=FExpression;
     eval.Evaluate;
     // Do the operation
     case AgType of
      rpagSum:
       begin
        if Not VarIsNull(eval.EvalResult) then
         FValue:=FValue+eval.EvalResult;
       end;
      rpagMin:
       begin
        if FDataCount=1 then
         FValue:=eval.EvalResult
        else
        begin
         if FValue>eval.EvalResult then
          FValue:=eval.EvalResult;
        end;
       end;
      rpagMax:
       begin
        if FDataCount=1 then
         FValue:=eval.EvalResult
        else
        begin
         if FValue<eval.EvalResult then
          FValue:=eval.EvalResult;
        end;
       end;
      rpagAvg:
       begin
        FSumValue:=FSumValue+eval.EvalResult;
        FValue:=FSumValue/FDataCount;
       end;
      rpagStdDev:
       begin
        SetLength(FValues,FDataCount);
        FValues[FDatacount-1]:=eval.EvalResult;
        if High(FValues)=Low(FValues) then
         FValue:=0
        else
         FValue:=StdDev(FValues);
       end;
     end;
     FUpdated:=true;
    end;
   end;
  rpGroupChange:
   begin
    FUpdated:=false;
    FOldString:='';
    if (FAggregate=rpAgGroup) then
    begin
     if Uppercase(GroupName)=UpperCase(newgroup) then
     begin
      // Update with the initial value
      eval:=TRpReport(GetReport).Evaluator;
      eval.Expression:=FAgIniValue;
      eval.Evaluate;
      FValue:=eval.EvalResult;
      FSumValue:=FValue;
      FDataCount:=0;
      FUpdated:=true;
     end;
    end;
   end;
  rpPageChange:
   begin
    FOldString:='';
    if (FAggregate=rpAgNone) then
    begin
     // Page variable must be recalculated
     FUpdated:=False;
    end;
   end;
  rpInvalidateValue:
   begin
    FOldString:='';
    FUpdated:=false;
   end;
 end;
end;

function TRpExpression.GetExtension(adriver:IRpPrintDriver):TPoint;
var
 aText:TRpTextObject;
begin
 Result:=inherited GetExtension(adriver);
 aText.Text:=GetText;
 aText.LFontName:=LFontName;
 aText.WFontName:=WFontName;
 aText.FontSize:=FontSize;
 aText.FontRotation:=FontRotation;
 aText.FontStyle:=FontStyle;
 aText.Type1Font:=integer(Type1Font);
 aText.CutText:=CutText;
 aText.Alignment:=Alignment;
 aText.WordWrap:=WordWrap;

 adriver.TextExtent(aText,Result);
 LastExtent:=Result;
end;


procedure TRpExpression.WriteExpression(Writer:TWriter);
begin
 WriteWideString(Writer, FExpression);
end;

procedure TRpExpression.ReadExpression(Reader:TReader);
begin
 FExpression:=ReadWideString(Reader);
end;

procedure TRpExpression.WriteAgIniValue(Writer:TWriter);
begin
 WriteWideString(Writer, FAgIniValue);
end;

procedure TRpExpression.ReadAgIniValue(Reader:TReader);
begin
 FAgIniValue:=ReadWideString(Reader);
end;

procedure TRpExpression.DefineProperties(Filer:TFiler);
begin
 inherited;

 Filer.DefineProperty('Expression',ReadExpression,WriteExpression,True);
 Filer.DefineProperty('AgIniValue',ReadAgIniValue,WriteAgIniValue,True);
end;

procedure TRpLabel.WriteWideText(Writer:TWriter);
begin
 WriteWideString(Writer, FWideText);
end;

procedure TRpLabel.ReadWideText(Reader:TReader);
begin
 FWideText:=ReadWideString(Reader);
end;

procedure TRpLabel.DefineProperties(Filer:TFiler);
begin
 inherited;

 Filer.DefineProperty('WideText',ReadWideText,WriteWideText,True);
end;


end.
