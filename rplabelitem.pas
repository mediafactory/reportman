{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rplabelitem                                     }
{       TRpLabel printeable component constant text     }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}


unit rplabelitem;

interface

uses Sysutils,Classes,rptypes,rpprintitem,rpconsts;

type
 TRpLabel=class(TRpGenTextComponent)
  private
   FAllText:TStrings;
   procedure SetText(Value:WideString);
   function GetText:WideString;
   procedure SetAllText(Value:TStrings);
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
  public
   constructor Create(AOwner:TComponent);override;
  published
   property Expression:widestring read FExpression write FExpression;
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
 Width:=1440;
end;

end.
