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

uses Sysutils,Classes,rptypes,rpprintitem,rpconsts,
 rpmetafile,rpeval,variants;

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
   FUpdated:boolean;
   FAgIniValue:widestring;
   procedure SetIdentifier(Value:string);
   procedure Evaluate;
  protected
   procedure DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);override;
  public
   constructor Create(AOwner:TComponent);override;
   procedure SubReportChanged(newstate:TRpReportChanged;newgroup:string='');
   function GetText:widestring;
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
  end;

implementation

uses rpreport;

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
end;

procedure TRpLabel.DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);
begin
 metafile.Pages[metafile.CurrentPage].NewTextObject(aposy+PosY,
  aposx+PosX,width,height,Text,WFontName,LFontName,FontSize,
  FontStyle,FOntColor,BackColor,Transparent,CutText,Alignment or VAlignment,WordWrap);
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
   Raise TRpReportException.Create(E.Message+':'+SRpSExpression,self);
  end;
 end;
end;

function TRpExpression.GetText:widestring;
begin
 if Length(Trim(Expression))<1 then
 begin
  Result:='';
  exit;
 end;
 Evaluate;
 Result:=FormatVariant(displayformat,FValue);
end;

procedure TRpExpression.DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);
var
 Text:string;
begin
 Text:=GetText;
 metafile.Pages[metafile.CurrentPage].NewTextObject(aposy+PosY,
   aposx+PosX,width,height,Text,WFontName,LFontName,FontSize,
   FontStyle,FOntColor,BackColor,Transparent,CutText,Alignment or VAlignment,WordWrap);
end;

procedure TRpExpression.SubReportChanged(newstate:TRpReportChanged;newgroup:string='');
var
 eval:TRpEvaluator;
begin
 case newstate of
  rpReportStart:
   begin
    FUpdated:=false;
    if (FAggregate<>rpAgNone) then
    begin
     // Update with the initial value
     eval:=TRpReport(Owner).Evaluator;
     eval.Expression:=FAgIniValue;
     eval.Evaluate;
     FValue:=eval.EvalResult;
     FUpdated:=true;
    end;
   end;
  rpPageStart:
   begin
    if (FAggregate=rpAgPage) then
    begin
     // Update with the initial value plus the current
     eval:=TRpReport(Owner).Evaluator;
     eval.Expression:=FAgIniValue;
     eval.Evaluate;
     FValue:=eval.EvalResult;
     FUpdated:=true;
    end;
   end;
  rpDataChange:
   begin
    FUpdated:=false;
    if (FAggregate<>rpAgNone) then
    begin
     // Update with the initial value
     eval:=TRpReport(Owner).Evaluator;
     eval.Expression:=FExpression;
     eval.Evaluate;
     FValue:=FValue+eval.EvalResult;
     FUpdated:=true;
    end;
   end;
  rpGroupChange:
   begin
    FUpdated:=false;
    if (FAggregate=rpAgGroup) then
    begin
     if GroupName=newgroup then
     begin
      // Update with the initial value
      eval:=TRpReport(Owner).Evaluator;
      eval.Expression:=FAgIniValue;
      eval.Evaluate;
      FValue:=eval.EvalResult;
      FUpdated:=true;
     end;
    end;
   end;
 end;
end;



end.
