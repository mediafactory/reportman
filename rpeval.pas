{*******************************************************}
{                                                       }
{       Rpeval                                          }
{       TRpCustomEvaluator: The Expression evaluator for}
{       Report Manager                                  }
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

unit rpeval;


interface


uses
  SysUtils, Classes,DB,rptypeval,rpevalfunc,
  rpconsts,sysconst,rpparser,variants,rpalias;
  
type
 TRpCustomEvaluator=class(TComponent)
 private
  // Component to access fields
  fRpalias:TRpalias;
  // Error information
  FError:string;
  FPosError:LongInt;
  FLineError:Word;
  // The parser
  Rpparser:TRpparser;
  // The expresion to evaluate
  FExpression:string;
  // Result of the evaluation
  FEvalResult:TRpValue;
  FIdentifiers:TStringList;
  FPartial:TRpValue;
  // Variable that contains if we are doing syntax checking
  FChecking:Boolean;
  procedure SetExpression(Value:string);
  // Recursive functions to evaluate the expresion
  procedure variables(var Value:TRpValue);
  procedure separator(var Value:TRpValue);
  procedure logicalOR(var Value:TRpValue);
  procedure logicalAND(var Value:TRpValue);
  procedure comparations(var Value:TRpValue);
  procedure sum_dif(var Value:TRpValue);
  procedure mul_div(var Value:TRpValue);
  procedure dosign(var Value:TRpValue);
  procedure ExecuteIIF(var Value:TRpValue);
  procedure parentesis(var Value:TRpValue);
  procedure operand(var Value:TRpValue);
  // Aditional priva procedures
  function EvaluateExpression:TRpValue;
  // Searching indentifiers
  function Searchwithoutdot(name1:Shortstring):TIdentifier;
  function GetEvalResultString:string;
  procedure AddIdentifiers;
 protected
  procedure Notification(AComponent:TComponent;Operation:TOperation);override;
 public
  // To avoid infinite recursive
  Evaluating:Boolean;
  // Creation and destruction
  constructor Create(AOwner:TComponent);override;
  constructor CreateWithoutiden(AOwner:TComponent;AddIdens:boolean);
  destructor Destroy;override;
  // Adds identifiers
  procedure AddVariable(name1:string;objecte:TIdentifier);
  procedure AddIden(name1:string;objecte:TIdentifier);
  function NewVariable(name1:string;ValueIni:TRpValue):TIdenVariable;
  // Searching identifiers
  function Searchidentifier(name1:shortstring):TIdentifier;
  // The evaluation procedure
  procedure Evaluate;
  // The evaluation procedure without Expression property
  function EvaluateText(text:string):TRpValue;
  // Checking Syntax
  procedure CheckSyntax;
  property Expression:string Read FExpression write SetExpression;
  property EvalResult:TRpValue Read FEvalResult;
  // The identifiers including functions
  property Identifiers:TStringList read FIdentifiers
   write FIdentifiers;
  // Error information
  property Error:string read FError;
  property PosError:LongInt read FPosError;
  property LineError:Word read FLineError;
  property EvalResultString:string read GetEvalResultString;
  // Database access component link
  property Rpalias:TRpalias read FRpalias write FRpalias;
 end;

 // The visual component
 TRpEvaluator = class(TRpCustomEvaluator)
  private
  protected
  public
   { Public declarations }
  published
   { Published declarations }
   property Rpalias;
   property EvalResult;
   property Expression;
  end;

  // The visual editor can check syntax and evaluate
  // on design time
{  TRpEvalEditor=class(TComponentEditor)
  protected
  public
   procedure Edit;override;
   function GetVerbCount:integer;override;
   function GetVerb(Index:integer):string;override;
   procedure ExecuteVerb(Index:Integer);override;
  end;
}
implementation


var
 Rpfunctions:TStringList;



// TRpCustomEvaluator

constructor TRpCustomEvaluator.CreateWithoutiden(AOwner:TComponent;AddIdens:boolean);
begin
 inherited Create(AOwner);
 Evaluating:=false;
 FExpression:=String(chr(0));
 // Creates de parser
 Rpparser:=TRpparser.Create;
 // The identifiers list
 FIdentifiers:=TStringList.Create;
 FIdentifiers.Sorted:=True;
 FIdentifiers.Duplicates:=dupError;
 if AddIdens then
  AddIdentifiers;
end;

constructor TRpCustomEvaluator.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 Evaluating:=false;
 FExpression:=String(chr(0));
 // The parser
 Rpparser:=TRpparser.Create;
 // The identifiers
 FIdentifiers:=TStringList.Create;
 FIdentifiers.Sorted:=True;
 FIdentifiers.Duplicates:=dupError;
 // Always add with this constructor
 AddIdentifiers;
end;

// Creates only one instance of functions for all
// Evaluators
procedure FillRpcache;
var iden:TIdentifier;
begin
 if Rpfunctions.count>0 then
  exit;
  // Boolean constants
  iden:=TIdenTrue.Create(nil);
  Rpfunctions.AddObject('TRUE',iden);
  iden:=TIdenFalse.Create(nil);
  Rpfunctions.AddObject('FALSE',iden);

 // Datetime constants
 iden:=TIdenToday.Create(nil);
 Rpfunctions.AddObject('TODAY',iden);
 iden:=TIdenTime.Create(nil);
 Rpfunctions.AddObject('TIME',iden);
 iden:=TIdenNow.Create(nil);
 Rpfunctions.AddObject('NOW',iden);

 // Null constant
 iden:=TIdenNULL.Create(nil);
 Rpfunctions.AddObject('NULL',iden);

 // Functions
 iden:=TIdenSinus.Create(nil);
 Rpfunctions.AddObject('SIN',iden);
 iden:=TIdenFloatToDateTime.Create(nil);
 Rpfunctions.AddObject('FLOATTODATETIME',iden);
 iden:=TIdenRound.Create(nil);
 Rpfunctions.AddObject('ROUND',iden);
 iden:=TIdenInt.Create(nil);
 Rpfunctions.AddObject('INT',iden);
 iden:=TIdenSQRT.Create(nil);
 Rpfunctions.AddObject('SQRT',iden);
 iden:=TIdenSTR.Create(nil);
 Rpfunctions.AddObject('STR',iden);
 iden:=TIdenVal.Create(nil);
 Rpfunctions.AddObject('VAL',iden);
 iden:=TIdenLEFT.Create(nil);
 Rpfunctions.AddObject('LEFT',iden);
 iden:=TIdenTrim.Create(nil);
 Rpfunctions.AddObject('TRIM',iden);
 iden:=TIdenPos.Create(nil);
 Rpfunctions.AddObject('POS',iden);
 iden:=TIdenModul.Create(nil);
 Rpfunctions.AddObject('MOD',iden);
 iden:=TIdenMonthname.Create(nil);
 Rpfunctions.AddObject('MONTHNAME',iden);
 iden:=TIdenMonth.Create(nil);
 Rpfunctions.AddObject('MONTH',iden);
 iden:=TIdenYear.Create(nil);
 Rpfunctions.AddObject('YEAR',iden);
 iden:=TIdenDay.Create(nil);
 Rpfunctions.AddObject('DAY',iden);
 iden:=TIdenRight.Create(nil);
 Rpfunctions.AddObject('RIGHT',iden);
 iden:=TIdenSubstr.Create(nil);
 Rpfunctions.AddObject('SUBSTR',iden);
 iden:=TIdenFormatstr.Create(nil);
 Rpfunctions.AddObject('FORMATSTR',iden);
 iden:=TIdenHourMinSec.Create(nil);
 Rpfunctions.AddObject('HOURMINSEC',iden);
 iden:=TIdenUppercase.Create(nil);
 Rpfunctions.AddObject('UPPERCASE',iden);
 iden:=TIdenLowercase.Create(nil);
 Rpfunctions.AddObject('LOWERCASE',iden);
 iden:=TIdenEvalText.Create(nil);
 Rpfunctions.AddObject('EVALTEXT',iden);

 // Graphic functions
{ iden:=TIdenGraphicClear.Create(nil);
 Rpfunctions.AddObject('GRAPHICLEAR',iden);
 iden:=TIdenGraphicNew.Create(nil);
 Rpfunctions.AddObject('GraphicNew',iden);
 iden:=TIdenNumToText.Create(nil);
 Rpfunctions.AddObject('NUMTOTEXT',iden);
}
end;

// Adds the identifiers that are on cache
procedure TRpCustomEvaluator.AddIdentifiers;
begin
 FillRpcache;
 Identifiers.Assign(Rpfunctions);
end;

destructor TRpCustomEvaluator.Destroy;
begin
 if Rpparser<>nil then
  Rpparser.free;
 FIdentifiers.free;
 inherited Destroy;
end;

procedure TRpCustomEvaluator.SetExpression(Value:string);
begin
 if Evaluating then
  Raise Exception.Create(SRpsetexpression);
 FExpression:=Value;
end;

// To evaluate a text we must create another evaluator
function TRpCustomEvaluator.EvaluateText(text:string):TRpValue;
var eval:TRpCustomEvaluator;
    oldiden:TStringList;
begin
 oldiden:=nil;
 if Evaluating then
 begin
  eval:=TRpCustomEvaluator.CreateWithoutiden(Self,false);
  try
   oldiden:=eval.Identifiers;
   eval.Identifiers:=Identifiers;
   eval.Rpalias:=FRpalias;
   eval.Expression:=text;
   eval.Evaluate;
   Result:=eval.EvalResult;
  finally
   eval.Identifiers:=oldiden;
   eval.free;
  end;
 end
 else
 begin
  Expression:=text;
  Evaluate;
  Result:=EvalResult;
 end;
end;

// Checking for syntax
procedure TRpCustomEvaluator.CheckSyntax;
begin
 Rpparser.Expression:=Pchar(FExpression);

 if Rpparser.TokenString='' then
 begin
  FEvalResult:=True;
  Exit;
 end;
 FChecking:=True;
 try
  EvaluateExpression;
  FEvalResult:=True;
 except
  FEvalResult:=False;
  Raise;
 end;
end;

procedure TRpCustomEvaluator.Evaluate;
begin
 Rpparser.Expression:=Pchar(FExpression);
 FChecking:=False;
 if ((Rpparser.TokenString='') AND (Rpparser.Token<>tostring)) then
 begin
  FEvalResult:=True;
  Exit;
 end;
 FEvalResult:=EvaluateExpression;
end;

function TRpCustomEvaluator.EvaluateExpression:TRpValue;
begin
 FPartial:=varnull;
 try
  Evaluating:=True;
  try
   // Call the recursive tree to evaluate the expresion
   variables(FPartial);
  finally
   Evaluating:=False;
  end;
 except
  // We can assign error information here
  on E:EZeroDivide do
   begin
    FError:=SRpDivisioZero;
    FLineError:=Rpparser.SourceLine;
    FPosError:=Rpparser.SourcePos;
    Raise TRpEvalException.Create(FError,
        Rpparser.TokenString,FLineError,FPosError);
   end;
  On E:TRpNamedException do
   begin
   FError:=E.ErrorMessage;
   FLineError:=Rpparser.SourceLine;
   FPosError:=Rpparser.SourcePos;
   Raise TRpEvalException.Create(FError+' '''+E.ElementError+'''',
        Rpparser.TokenString,FLineError,FPosError)
   end;
  on EParserError do
   begin
    FError:=SRpEvalSyntax;
    FLineError:=Rpparser.SourceLine;
    FPosError:=Rpparser.SourcePos;
    Raise TRpEvalException.Create(Ferror,
        Rpparser.TokenString,FLineError,FPosError);
   end;
  on E:TRpEvalException do
   begin
    FError:=E.ErrorMessage;
    FLineError:=E.ErrorLine;
    FPosError:=E.ErrorPosition;
    Raise;
   end;
  on E:EVariantError do
   begin
    if E.Message=SInvalidCast then
    begin
     FError:=SRpEvalType;
     FLineError:=Rpparser.SourceLine;
     FPosError:=Rpparser.SourcePos;
     Raise TRpEvalException.Create(Ferror,
         Rpparser.TokenString,FLineError,FPosError);
    end
    else
     if E.Message=SInvalidVarOp then
     begin
      FError:=SRpInvalidOperation;
      FLineError:=Rpparser.SourceLine;
      FPosError:=Rpparser.SourcePos;
      Raise TRpEvalException.Create(Ferror,
         Rpparser.TokenString,FLineError,FPosError);
     end;
   end;
  else
  begin
   FError:=SRpEvalSyntax;
   FLineError:=Rpparser.SourceLine;
   FPosError:=Rpparser.SourcePos;
   Raise;
  end;
 end;

 if Rpparser.Token<>toEOF then
 begin
  FError:=SRpEvalSyntax;
  FLineError:=Rpparser.SourceLine;
  FPosError:=Rpparser.SourcePos;
  Raise TRpEvalException.Create(SRpEvalSyntax+Rpparser.TokenString,
        Rpparser.TokenString,Rpparser.SourceLine,Rpparser.SourcePos);
 end;
 Result:=FPartial;
end;

procedure TRpCustomEvaluator.variables(var Value:TRpValue);
var
 iden:TIdentifier;
begin
 if Rpparser.Token=toSymbol then
 begin
  // Exists this identifier?
  iden:=Searchidentifier(Rpparser.TokenString);
  if iden=nil then
  begin
   Raise TRpEvalException.Create(SRpEvalDescIden+
         Rpparser.TokenString,Rpparser.TokenString,
        Rpparser.SourceLine,Rpparser.SourcePos);
  end
  else
  begin
   // Is a variable?
   if iden.RType=RTypeidenvariable then
   begin
    // Is a := , so we must assign
    if Rpparser.NextTokenIs(':=') then
    begin
     Rpparser.NextToken;
     // The :=
     Rpparser.NextToken;
     // Look for the value
     variables(Value);
     // If syntax checking not touch the variable
     if (Not FChecking) then
      (iden As TIdenVariable).Value:=Value;
    end
    else
     // not a := we must continue
     logicalOR(Value);
   end
   else
    // look for it
    logicalOR(Value);
  end
 end
 else
 begin
   // look for it
  logicalOR(Value);
 end;
end;

{**************************************************************************}

procedure TRpCustomEvaluator.logicalOR(var Value:TRpValue);
var operador:string[3];
    Auxiliar:TRpValue;
begin
 // First precedes the AND operator
 logicalAND(Value);

 if Rpparser.Token=toOperator then
 begin
  operador:=UpperCase(Rpparser.TokenString);
  while (operador='OR') do
  begin
   Rpparser.NextToken;
   logicalAND(Auxiliar);
   // Compatible types?
   if (Not FChecking) then
    Value:=LogicalORTRpValue(Value,Auxiliar);
   if Rpparser.Token<>toOperator then
    Exit
   else
    operador:=UpperCase(Rpparser.TokenString);
  end;
 end;
end;

procedure TRpCustomEvaluator.logicalAND(var Value:TRpValue);
var operador:string[3];
    Auxiliar:TRpValue;
begin
 separator(Value);

 if Rpparser.Token=toOperator then
 begin
  operador:=UpperCase(Rpparser.TokenString);
  while (operador='AND') do
  begin
   Rpparser.NextToken;
   separator(Auxiliar);
   // Compatible types?
   if (Not FChecking) then
    Value:=LogicalANDTRpValue(Value,Auxiliar);
   if Rpparser.Token<>toOperator then
    Exit
   else
    operador:=UpperCase(Rpparser.TokenString);
  end;
 end;
end;

procedure TRpCustomEvaluator.separator(var Value:TRpValue);
begin
 comparations(Value);
 if Rpparser.Token=toOperator then
  while (Rpparser.TokenString[1]=';') do
  begin
   Rpparser.NextToken;
   comparations(Value);
   if Rpparser.Token<>tooperator then
    Exit
  end;
end;

procedure TRpCustomEvaluator.comparations(var Value:TRpValue);
var
operation:string[3];
auxiliar:TRpValue;
begin
 sum_dif(Value);
 while Rpparser.Token=tooperator do
 begin
  operation:=Rpparser.TokenString;
  if operation='=' then
    begin
     Rpparser.NextToken;
     sum_dif(auxiliar);
     if (NOT FChecking) then
      Value:=EqualTRpValue(Value,auxiliar);
    end
   else
   if ((operation='<>') OR (operation='><')) then
   begin
    Rpparser.NextToken;
    sum_dif(auxiliar);
    if (NOT FChecking) then
    Value:=DiferentTRpValue(Value,auxiliar);
   end
   else
   if operation='>=' then
   begin
    Rpparser.NextToken;
    sum_dif(auxiliar);
    if (NOT FChecking) then
    Value:=MoreThanEqualTRpValue(Value,auxiliar);
   end
   else
   if operation='<=' then
   begin
    Rpparser.NextToken;
    sum_dif(auxiliar);
    if (NOT FChecking) then
    Value:=LessThanEqualTRpValue(Value,auxiliar);
   end
   else
   if operation='>' then
   begin
    Rpparser.NextToken;
    sum_dif(auxiliar);
    if (NOT FChecking) then
    Value:=MoreThanTRpValue(Value,auxiliar);
   end
   else
   if operation='<' then
   begin
    Rpparser.NextToken;
    sum_dif(auxiliar);
    if (NOT FChecking) then
    Value:=LessThanTRpValue(Value,auxiliar);
   end
   else
   if operation='==' then
   begin
    Rpparser.NextToken;
    sum_dif(auxiliar);
    if (NOT FChecking) then
    Value:=EqualEqualTRpValue(Value,auxiliar);
   end
   else
    Exit;
 end;
end;

procedure TRpCustomEvaluator.sum_dif(var Value:TRpValue);
var operador:string[3];
    Auxiliar:TRpValue;
begin
 mul_div(Value);

 if Rpparser.Token=toOperator then
 begin
  operador:=UpperCase(Rpparser.TokenString);
  while ((operador='+') or (operador='-')) do
  begin
   Rpparser.NextToken;
   mul_div(Auxiliar);

   // Compatible types?
   if (Not FChecking) then
   case operador[1] of
    '+':Value:=SumTRpValue(Value,Auxiliar);
    '-':Value:=DifTRpValue(Value,Auxiliar);
   end;
   if Rpparser.Token<>toOperator then
    Exit
   else
    operador:=UpperCase(Rpparser.TokenString);
  end;
 end;
end;

procedure TRpCustomEvaluator.mul_div(var Value:TRpValue);
var operador:string[4];
    Auxiliar:TRpValue;
begin
 dosign(Value);

 if Rpparser.Token=toOperator then
 begin
  operador:=Uppercase(Rpparser.TokenString);
  while ((operador='*') or (operador='/')) do
  begin
   Rpparser.NextToken;
   dosign(Auxiliar);
   if (Not FChecking) then
   case operador[1] of
    '*':Value:=MultTRpValue(Value,Auxiliar);
    '/':Value:=DivTRpValue(Value,Auxiliar);
   end;
   if Rpparser.Token<>toOperator then
      Exit
   else
    operador:=UpperCase(Rpparser.TokenString);
  end;
 end;
end;

// The sign is same precedence than functions
procedure TRpCustomEvaluator.dosign(var Value:TRpValue);
var operador:string[4];
    iden:TIdentifier;
    i:integer;
begin
 iden:=nil;
 operador:='';
 if Rpparser.Token=toOperator then
 begin
  operador:=Rpparser.TokenString;
  if ((operador='+') or (operador='-')
       or (operador='NOT') or (operador='IIF')) then
   Rpparser.NextToken;
 end
 else
 // Is a function?
 if Rpparser.Token=toSymbol then
 begin
  iden:=Searchidentifier(Rpparser.TokenString);
  if iden=nil then
   Raise TRpEvalException.Create(SRpEvalDescIden+
       Rpparser.TokenString,Rpparser.TokenString,
      Rpparser.SourceLine,Rpparser.SourcePos);
  if iden.RType=RTypeidenfunction then
  begin
   // Ok is a function assign params
   if iden.paramcount>0 then
   begin
    Rpparser.NextToken;
    if Rpparser.Token<>toOperator then
       Raise TRpEvalException.Create(SRpEvalParent+' ('+
       Rpparser.TokenString,' ( '+Rpparser.TokenString,Rpparser.SourceLine,
       Rpparser.SourcePos);
    if Rpparser.TokenString<>'(' then
       Raise TRpEvalException.Create(SRpEvalParent+
       Rpparser.TokenString,'('+Rpparser.TokenString,Rpparser.SourceLine,
       Rpparser.SourcePos);
   end;
   for i:=0 to iden.paramcount-1 do
   begin
    // Next param
    Rpparser.NextToken;
    // Look for the value
    variables(iden.Params[i]);
    // Param separator is ','
    if iden.paramcount>i+1 then
    begin
      if Rpparser.Token<>ToOperator then
       Raise TRpEvalException.Create(SRpEvalSyntax+
       Rpparser.TokenString,Rpparser.TokenString,Rpparser.SourceLine,
       Rpparser.SourcePos);
      if Rpparser.TokenString[1]<>',' then
       Raise TRpEvalException.Create(SRpEvalSyntax+
       Rpparser.TokenString,Rpparser.TokenString,Rpparser.SourceLine,
       Rpparser.SourcePos);
     end;
   end;
   // Now the close ) expected
   if iden.paramcount>0 then
   begin
    if Rpparser.Token<>toOperator then
       Raise TRpEvalException.Create(SRpEvalParent+' )'+
       Rpparser.TokenString,' ) '+Rpparser.TokenString,Rpparser.SourceLine,
       Rpparser.SourcePos);
    if Rpparser.TokenString<>')' then
       Raise TRpEvalException.Create(SRpEvalParent+' )'+
       Rpparser.TokenString,' ) '+Rpparser.TokenString,Rpparser.SourceLine,
       Rpparser.SourcePos);
    Rpparser.NextToken;
   end;
  end
  else
   // If not a function must be an operator
   iden:=nil;
 end;
 if iden=nil then
 begin
  if operador='IIF' then
     ExecuteIIF(Value)
  else
   parentesis(Value)
 end
 else
  if iden.paramcount=0 then
   Rpparser.NextToken;

 // If it's a funcion execute it
 if iden<>nil then
 begin
   Value:=iden.Value;
 end
 else
 begin
  if operador='-' then
     if (NOT FChecking) then
      Value:=SignTRpValue(Value)
     else
     begin
     end
  else
   if operador='NOT' then
     if (NOT FChecking) then
      Value:=LogicalNOTTRpValue(Value);
 end;
end;

procedure TRpCustomEvaluator.Operand(var Value:TRpValue);
VAR
    iden:TIdentifier;
begin
 case Rpparser.Token of
  toSymbol:
   begin
    // Obtaining the value of an identifier
    iden:=Searchidentifier(Rpparser.TokenString);
    if iden=nil then
    begin
     Raise TRpEvalException.Create(SRpEvalDescIden+
         Rpparser.TokenString,Rpparser.TokenString,
        Rpparser.SourceLine,Rpparser.SourcePos);
    end;
    Value:=iden.Value;
    Rpparser.NextToken;
   end;
  toString:
   begin
    Value:=Rpparser.TokenString;
    Rpparser.NextToken;
   end;
  toInteger:
   begin
    Value:=Rpparser.TokenInt;
    Rpparser.NextToken;
   end;
  toFloat:
   begin
    Value:=Rpparser.TokenFloat;
    Rpparser.NextToken;
   end;
  else
  begin
   Raise TRpEvalException.Create(SRpEvalSyntax+
         Rpparser.TokenString,Rpparser.TokenString,
        Rpparser.SourceLine,Rpparser.SourcePos);
  end;
 end;
end;

procedure TRpCustomEvaluator.parentesis(var Value:TRpValue);
var operation:char;
begin
 if Rpparser.Token=toOperator then
 begin
  operation:=Rpparser.TokenString[1];
  if operation='(' then
  begin
   Rpparser.NextToken;
   // Look into the parentesis
   variables(Value);

   if (Rpparser.Token<>toOperator) then
    Raise TRpEvalException.Create(SRpEvalParent,'',
        Rpparser.SourceLine,Rpparser.SourcePos);
   operation:=Rpparser.TokenString[1];
   if (operation<>')') then
      Raise TRpEvalException.Create(SRpEvalParent,'',
        Rpparser.SourceLine,Rpparser.SourcePos);
   Rpparser.NextToken;
  end
  else
   operand(Value);
 end
 else
   operand(Value);
end;

procedure TRpCustomEvaluator.ExecuteIIF(var Value:TRpValue);
var
 auxiliar:TRpValue;
 AnticFChecking:Boolean;
begin
 // Must be a parentesis
 if Rpparser.Token<>toOperator then
   Raise TRpEvalException.Create(SRpEvalParent,'(',
      Rpparser.SourceLine,Rpparser.SourcePos);
 if Rpparser.TokenString<>'(' then
   Raise TRpEvalException.Create(SRpEvalParent,'(',
      Rpparser.SourceLine,Rpparser.SourcePos);
 // Decision term
 Rpparser.NextToken;
 variables(Value);
 // Not boolean error
 if ((VarType(Value)<>varBoolean) AND (Not FChecking)) then
   Raise TRpEvalException.Create(SRpEvalType,'IIF',
      Rpparser.SourceLine,Rpparser.SourcePos);
 // Next tokens
 if Rpparser.Token<>tooperator then
   Raise TRpEvalException.Create(SRpEvalSyntax,'IIF',
      Rpparser.SourceLine,Rpparser.SourcePos);
 if Rpparser.Tokenstring<>',' then
   Raise TRpEvalException.Create(SRpEvalSyntax,'IIF',
      Rpparser.SourceLine,Rpparser.SourcePos);
 Rpparser.NextToken;
 // If yes and not checking syntax
 if Not FChecking then
 begin
  if Value then
  begin
   variables(Value);
   // Skip the second term
   if Rpparser.Token<>tooperator then
     Raise TRpEvalException.Create(SRpEvalSyntax,'IIF',
        Rpparser.SourceLine,Rpparser.SourcePos);
   if Rpparser.Tokenstring<>',' then
     Raise TRpEvalException.Create(SRpEvalSyntax,'IIF',
        Rpparser.SourceLine,Rpparser.SourcePos);
   Rpparser.NextToken;

   AnticFChecking:=FChecking;
   FChecking:=True;
   variables(auxiliar);
   FChecking:=AnticFChecking;
  end
  else
  begin
   AnticFChecking:=FChecking;
   FChecking:=True;
   variables(auxiliar);

   if Rpparser.Token<>tooperator then
     Raise TRpEvalException.Create(SRpEvalSyntax,'IIF',
        Rpparser.SourceLine,Rpparser.SourcePos);
   if Rpparser.Tokenstring<>',' then
     Raise TRpEvalException.Create(SRpEvalSyntax,'IIF',
        Rpparser.SourceLine,Rpparser.SourcePos);
   Rpparser.NextToken;

   FChecking:=AnticFChecking;
   variables(Value);
  end;
 end
 else
 // Syntax checking
 begin
  // Skip the params
  variables(Value);
  if Rpparser.Token<>tooperator then
    Raise TRpEvalException.Create(SRpEvalSyntax,'IIF',
       Rpparser.SourceLine,Rpparser.SourcePos);
  if Rpparser.Tokenstring<>',' then
    Raise TRpEvalException.Create(SRpEvalSyntax,'IIF',
       Rpparser.SourceLine,Rpparser.SourcePos);
  Rpparser.NextToken;
  variables(auxiliar);
 end;
 // Must be a ) now
 if Rpparser.Token<>toOperator then
   Raise TRpEvalException.Create(SRpEvalParent,')',
      Rpparser.SourceLine,Rpparser.SourcePos);
 if Rpparser.TokenString<>')' then
   Raise TRpEvalException.Create(SRpEvalParent,')',
      Rpparser.SourceLine,Rpparser.SourcePos);
 Rpparser.NextToken;
end;

procedure TRpCustomEvaluator.AddVariable(name1:string;objecte:TIdentifier);
begin
 objecte.idenname:=name1;
 Identifiers.ADDObject('M.'+AnsiUpperCase(name1),objecte);
end;

procedure TRpCustomEvaluator.AddIden(name1:string;objecte:TIdentifier);
begin
 Identifiers.ADDObject(name1,objecte);
end;

function TRpCustomEvaluator.Searchwithoutdot(name1:Shortstring):TIdentifier;
var
 index:integer;
 Doble:Boolean;
begin
  Doble:=False;
  Result:=nil;
  index:=Identifiers.Indexof(name1);
  if index>-1 then
  begin
   Result:=Identifiers.Objects[index] As TIdentifier;
   Exit;
  end;
  // Memory variable?
  index:=Identifiers.Indexof('M.'+name1);
  if index>-1 then
  begin
   Result:=Identifiers.Objects[index] As TIdentifier;
   Exit;
  end;
  // May be a field ?
  if FRpalias<>nil then
   Result:=FRpalias.Searchfield(name1,'',Doble);
  if Doble then
     Raise TRpEvalException.Create(SRpFieldDuplicated+ Rpparser.TokenString,
       Rpparser.TokenString,Rpparser.SourceLine,Rpparser.SourcePos);
end;

function TRpCustomEvaluator.Searchidentifier(name1:Shortstring):TIdentifier;
var
pospunt:byte;
primer,sensepunt:string;
index:integer;
doble:Boolean;
begin
 Result:=nil;
 // Have a point ?
 pospunt:=Pos('.',name1);
 if pospunt=0 then
 begin
  Result:=Searchwithoutdot(name1);
  Exit;
 end;
 primer:=copy(name1,0,pospunt-1);
  // Memory variable ?
 if primer='M' then
 begin
  index:=Identifiers.Indexof(name1);
  if index>-1 then
   Result:=Identifiers.Objects[index] As TIdentifier;
  Exit;
 end;
 sensepunt:=copy(name1,pospunt+1,ord(name1[0])-pospunt);
 if FRpalias<>nil then
  Result:=FRpalias.Searchfield(sensepunt,primer,Doble);
end;

function TRpCustomEvaluator.GetEvalResultString:string;
begin
 Result:=TRpValueToString(EvalResult);
end;

procedure TRpCustomEvaluator.Notification(AComponent:TComponent;Operation:TOperation);
begin
 inherited Notification(AComponent,Operation);
 if operation=opRemove then
 begin
  if AComponent=FRpalias then
   Rpalias:=nil;
 end;
end;

{procedure TRpEvalEditor.Edit;
begin
 if Component is TRpEvaluator then
 begin
  (Component As TRpEvaluator).Evaluate;
  messageDlg((Component as TRpEvaluator).EvalResultString,mtinformation,[mbOK],0);
 end;
end;

function TRpEvalEditor.GetVerbCount:integer;
begin
 Result:=3;
end;

function TRpEvalEditor.GetVerb(Index:integer):string;
begin
 case index of
  0:
   Result:='Evaluate';
  1:
   Result:='Syntax checking';
  2:
   Result:='Build expression';
 end;
end;

procedure TRpEvalEditor.ExecuteVerb(Index:Integer);
begin
 case index of
  0:
   begin
   (Component As TRpEvaluator).Evaluate;
   messageDlg((Component as TRpEvaluator).EvalResultString,mtinformation,[mbOK],0);
   end;
  1:
   (Component As TRpEvaluator).CheckSyntax;
  2:
   (Component As TRpEvaluator).Expression:=ChangeExpression((Component As TRpCustomEvaluator).expression,(Component As TRpCustomEvaluator));
 end;
end;
}

function TRpCustomEvaluator.NewVariable(name1:string;ValueIni:TRpValue):TIdenVariable;
var iden:TIdenVariable;
begin
 if Searchidentifier(name1)=nil then
 begin
  iden:=TIdenVariable.Create(Self);
  iden.Value:=ValueIni;
  // The owner is the TRprmEvaluator to free the variable
  AddVariable(name1,iden);
  REsult:=Iden;
 end
 else
  // Error variable redeclared
  Raise Exception.Create(SRpVariabledefined+name1)
end;

procedure Freerprmfunctions;
var
 i:integer;
begin
 for i:=0 to Rpfunctions.count-1 do
 begin
  Rpfunctions.objects[i].free;
 end;
end;

initialization

// Cache of functions used by all of Rpevaluators
Rpfunctions:=TStringList.create;
Rpfunctions.Sorted:=True;
Rpfunctions.Duplicates:=dupError;


finalization

Freerprmfunctions;

end.
