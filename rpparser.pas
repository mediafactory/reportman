{*******************************************************}
{                                                       }
{       Rptparser                                       }
{       Expression parser for TRpEvaluator              }
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

unit rpparser;

interface

uses Classes,sysutils,
 rpconsts,rptypeval;
type
 // A expresion can be divided in tokens, that is
 // numbers, identifiers, strings, operators
 TRpParser = Class(TObject)
 private
  NewExpresion:string;
  // Stream position
  FOrigin:Longint;
  // Position inside the optimizer buffer
  FBufPtr:PChar;
  // Pointer to the last position of the buffer
  FBufEnd:Pchar;
  // Pointer to the begging of the token
  FSourcePtr:PChar;
  // Pointer to the end of the current line
  FSourceEnd:PChar;
  // Pointer to the token to be analized
  FTokenPtr:PChar;
  // Pointer to the last string
  FStringPtr:PChar;
  // Vurrent Linenumber
  FSourceLine:Integer;
  // Token type
  FToken:Char;
  // Skips blancs, CRs...
  procedure SkipBlanks;
  procedure SetExpression(Value:Pchar);
 public
  // Checks if token is T type else exception
  procedure CheckToken(T: Char);
  // Checks for a identifier
  procedure CheckTokenSymbol(const S: string);
  // Raises an exception by a number
  procedure Error(MessageID: string);
  procedure HexToBinary(Stream: TStream);
  // Skips to the next token, analizing
  function NextToken: Char;
  // Position
  function SourcePos: Longint;
  // Token as a type
  function TokenFloat: Extended;
  function TokenInt: Longint;
  function TokenString: Shortstring;
  // Compares the token with S
  function TokenSymbolIs(const S: string): Boolean;
  // Ask for the next token
  function NextTokenIs(Value:string):Boolean;
  // Current line
  property SourceLine: Integer read FSourceLine;
  // Token type
  property Token: Char read FToken;
  // The property to assign the expression to be parsed
  property Expression:Pchar read FBufPtr write SetExpression;
 end;

implementation

procedure TRpParser.SetExpression(Value:Pchar);
begin
  NewExpresion:=StrPas(Value);
  Value:=Pchar(NewExpresion);
  FBufPtr := Value;
  FBufEnd := Value + strlen(Value);
  FSourcePtr := Value;
  FSourceEnd := Value;
  FTokenPtr := Value;
  FSourceLine := 1;
  NextToken;
end;

procedure TRpParser.CheckToken(T: Char);
begin
  if Token <> T then
    case T of
      toSymbol:
        Error(SRpIdentifierExpected);
      toString:
        Error(SRpstringExpected);
      toInteger, toFloat:
        Error(SRpNumberExpected);
      toOperator:
        Error(SRpOperatorExpected);
    end;
end;


procedure TRpParser.Error(MessageId: string);
begin
  Raise TRpEvalException.Create(MessageID,Tokenstring,SourceLine,SourcePos);
end;

procedure TRpParser.HexToBinary(Stream: TStream);
var
  Count: Integer;
  Buffer: array[0..255] of Char;
begin
  SkipBlanks;
  while FSourcePtr^ <> '}' do
  begin
    Count := HexToBin(FSourcePtr, Buffer, SizeOf(Buffer));
    if Count = 0 then Error(SRpInvalidBinary);
    Stream.Write(Buffer, Count);
    Inc(FSourcePtr, Count * 2);
    SkipBlanks;
  end;
  NextToken;
end;

function TRpParser.NextToken: Char;
var
  I: Integer;
  P, S: PChar;
  operador:char;
  operadors:string;
begin
  SkipBlanks;
  P := FSourcePtr;
  FTokenPtr := P;
  case P^ of
    // Identifiers
    'A'..'Z', 'a'..'z','á','à','é','è','í','ó','ò','ú', 'Ñ','ñ','_':
      begin
        Inc(P);
        while P^ in ['A'..'Z', 'a'..'z','á','à','é','è','í','ó','ò','ú', '0'..'9', 'Ñ','ñ', '_','.'] do Inc(P);
        Result := toSymbol;
      end;
    // Identifiers with blanks into brackets
    '[':
      begin
        Inc(P);
        while ((P^<>chr(0)) AND (P^<>']')) do Inc(P);
        // Finish?
        if P^<>']' then
         Raise Exception.Create(Format(SRpExpected,[']']));
        Inc(P);
        Result := toSymbol;
      end;

    // Operators
    '*','+','-','/','(',')',',','=','>','<',':',';':
      begin
       Result:=toOperator;
       operador:=P^;
       Inc(P);
       case P^ of
        '=':
         if operador in [':','!','<','>','='] then
          Inc(P);
        '<':
         if operador='>' then
          Inc(P);
        '>':
         if operador='<' then
          Inc(p);
       end;
      end;
    // Strings and chars
    '#', '''':
      begin
        S := P;
        while True do
          case P^ of
            '#':
              begin
                Inc(P);
                I := 0;
                while P^ in ['0'..'9'] do
                begin
                  I := I * 10 + (Ord(P^) - Ord('0'));
                  Inc(P);
                end;
                S^ := Chr(I);
                Inc(S);
              end;
            '''':
              begin
                Inc(P);
                while True do
                begin
                  case P^ of
                    #0, #10, #13:
                      Error(SRpEvalSyntax);
                    '''':
                      begin
                        Inc(P);
                        if P^ <> '''' then Break;
                      end;
                  end;
                  S^ := P^;
                  Inc(S);
                  Inc(P);
                end;
              end;
          else
            Break;
          end;
        FStringPtr := S;
        Result := toString;
      end;
    // Hex numbers
    '$':
      begin
        Inc(P);
        while P^ in ['0'..'9', 'A'..'F', 'a'..'f'] do Inc(P);
        Result := toInteger;
      end;
    // Numbers
    '0'..'9':
      begin
        Inc(P);
        while P^ in ['0'..'9'] do Inc(P);
        Result := toInteger;
        if P^ in ['.','e','E'] then
        begin
         Result := toFloat;
         if P^='.' then
         begin
          // Change it to convert correctly
          P^:=DecimalSeparator;
          Inc(P);
          while P^ in ['0'..'9'] do
            Inc(P);
         end;
         if P^ in ['E','e'] then
         begin
          Inc(P);
          if P^ in['+','-'] then Inc(P);
          while P^ in ['0'..'9'] do
            Inc(P);
         end;
        end;
      end;
  else
    Result:=P^;
    if Result <> toEOF then
    begin
     Result:=toSymbol;
     Inc(P);
    end;
  end;
  FSourcePtr := P;
  FToken := Result;
  // Symbols
  if FToken=toSymbol then
  begin
   OperadorS:=UpperCase(TokenString);
   if ((OperadorS='OR') OR (OperadorS='NOT')
        OR (OperadorS='AND') OR (OperadorS='IIF')) then
   begin
    Result:=toOperator;
    FToken:=toOperator;
   end;
  end;
end;


procedure TRpParser.SkipBlanks;
begin
  while True do
  begin
    case FSourcePtr^ of
      #0:
        begin
          if FSourcePtr^ = #0 then Exit;
          Continue;
        end;
      #10:
        Inc(FSourceLine);
      #33..#255:
        Exit;
    end;
    Inc(FSourcePtr);
  end;
end;

function TRpParser.SourcePos: Longint;
begin
  Result := FOrigin + (FTokenPtr - FBufptr);
end;

procedure TRpParser.CheckTokenSymbol(const S: string);
begin
  if not TokenSymbolIs(S) then
   Raise TRpEvalException.Create(Format(SRpExpected, [S]),'',SourceLine,SourcePos);
end;

function TRpParser.TokenFloat: Extended;
begin
  Result := StrToFloat(TokenString);
end;

function TRpParser.TokenInt: Longint;
begin
  Result := StrToInt(TokenString);
end;

function TRpParser.TokenString: Shortstring;
var
  L: Integer;
begin
  if FToken = toString then
    L := FStringPtr - FTokenPtr else
    L := FSourcePtr - FTokenPtr;
  if L > 255 then L := 255;
  Result[0] := Char(L);
  Move(FTokenPtr[0], Result[1], L);
  // Brackets out
  if Result[1]='[' then
  begin
   Result:=Copy(Result,2,Byte(Result[0])-2);
  end;
end;

function TRpParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (Token = toSymbol) and (CompareText(S, TokenString) = 0);
end;

function TRpParser.NextTokenIs(Value:string):Boolean;
var NewParser:TRpParser;
    Apuntador:PChar;
begin
  // A new parser must be create for checking the next token
  Apuntador:=FSourcePtr;
  NewParser:=TRpParser.Create;
  try
   NewParser.Expression:=Apuntador;
   Result:=False;
   if NewParser.Token in [toSymbol,toOperator] then
    if NewParser.TokenString=Value then
     Result:=True;
  finally
   NewParser.free;
  end;
end;

end.
