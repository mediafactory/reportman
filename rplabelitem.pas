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

uses Sysutils,Classes,rptypes,rpprintitem,rpconsts,
 rpmetafile,rpeval,
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
   procedure SetText(Value:WideString);
   function GetText:WideString;
   procedure SetAllText(Value:TStrings);
  protected
   procedure DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);override;
  public
   constructor Create(AOwner:TComponent);override;
   property Text:widestring read GetText write SetText;
   destructor Destroy;override;
  published
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
   FValue:Variant;
   FSumValue:Variant;
   FDataCount:integeR;
   FUpdated:boolean;
   FAgIniValue:widestring;
   FValues:array of Double;
   FPrintOnlyOne:boolean;
   FIdenExpression:TIdenRpExpression;
   FOldString:widestring;
   procedure SetIdentifier(Value:string);
   procedure Evaluate;
  protected
   procedure DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);override;
  public
   constructor Create(AOwner:TComponent);override;
   procedure SubReportChanged(newstate:TRpReportChanged;newgroup:string='');
   function GetText:widestring;
   property IdenExpression:TIdenRpExpression read FIdenExpression;
   function GetExtension(adriver:IRpPrintDriver):TPoint;override;
  published
   property DisplayFormat:string read FDisplayformat write FDisplayFormat;
   property Expression:widestring read FExpression write FExpression;
   property Identifier:string read FIdentifier write SetIdentifier;
   property Aggregate:TRpAggregate read FAggregate write FAggregate
    default rpagNone;
   property GroupName:string read FGroupName write FGroupName;
   property AgType:TRpAggregateType read FAgType write FAgType
    default rpAgSum;
   property AgIniValue:widestring read FAgIniValue write FAgIniValue;
   property AutoExpand:Boolean read FAutoExpand write FAutoExpand;
   property AutoContract:Boolean read FAutoContract write FAutoContract;
   property PrintOnlyOne:Boolean read FPrintOnlyOne write FPrintOnlyOne
    default false;
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
end;

destructor TRpLabel.Destroy;
begin
 FAllText.free;
 inherited destroy;
end;



procedure TRpLabel.SetText(Value:WideString);
var
 langindex:integer;
begin
 langindex:=TRpReport(Owner).Language;
 while ((FAllText.Count-1)<langindex) do
 begin
  FAllText.Add('');
 end;
 FAllText.Strings[langindex]:=Value;
end;


function TRpLabel.GetText:WideString;
var
 langindex:integer;
begin
 langindex:=TRpReport(Owner).Language;
 if FAlltext.Count>langindex then
  Result:=FAllText.Strings[langindex]
 else
  Result:='';
end;

procedure TRpLabel.SetAllText(Value:TStrings);
begin
 FAllTExt.Assign(Value);
end;

constructor TRpExpression.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 Height:=275;
 FAgIniValue:='0';
 Width:=1440;
 FIdenExpression:=TIdenRpExpression.Create(Self);
 FIdenExpression.FExpreitem:=Self;
end;

procedure TRpLabel.DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);
var
 aalign:integer;
begin
 aalign:=Alignment or VAlignment;
 if SingleLine then
  aalign:=aalign or AlignmentFlags_SingleLine;
 metafile.Pages[metafile.CurrentPage].NewTextObject(aposy+PosY,
  aposx+PosX,width,height,Text,WFontName,LFontName,FontSize,FontRotation,
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
 fidens:=TRpReport(Owner).Identifiers;
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
  fevaluator:=TRpREport(Owner).Evaluator;
  fevaluator.Expression:=Expression;
  fevaluator.Evaluate;
  FValue:=fevaluator.EvalResult;
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
  Result:=FormatVariant(displayformat,FValue);
 end;
end;

procedure TRpExpression.DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);
var
 aText:WideString;
 expre:WideString;
 aalign:integer;
begin
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
 metafile.Pages[metafile.CurrentPage].NewTextObject(aposy+PosY,
   aposx+PosX,width,height,aText,WFontName,LFontName,FontSize,FontRotation,
   FontStyle,smallint(Type1Font),FOntColor,BackColor,Transparent,CutText,aalign,WordWrap);
 // Is Total pages variable?
 if (UpperCase(expre)='PAGECOUNT') then
 begin
  TRpReport(Owner).AddTotalPagesItem(metafile.currentpage,metafile.Pages[metafile.currentpage].ObjectCount-1,displayformat);
 end;
end;

procedure TRpExpression.SubReportChanged(newstate:TRpReportChanged;newgroup:string='');
var
 eval:TRpEvaluator;
begin
 case newstate of
  rpReportStart:
   begin
    FOldString:='';
    FUpdated:=false;
    FDataCount:=0;
    if (FAggregate<>rpAgNone) then
    begin
     // Update with the initial value
     eval:=TRpReport(Owner).Evaluator;
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
     eval:=TRpReport(Owner).Evaluator;
     eval.Expression:=FExpression;
     eval.Evaluate;
     // Do the operation
     case AgType of
      rpagSum:
       begin
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
      rgagAvg:
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
    FOldString:='';
    if (FAggregate=rpAgGroup) then
    begin
     if GroupName=newgroup then
     begin
      // Update with the initial value
      eval:=TRpReport(Owner).Evaluator;
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
end;

end.
