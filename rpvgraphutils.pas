unit rpvgraphutils;

interface

uses Classes,Graphics;

function CLXColorToVCLColor(CLXColor:integer):integer;
function CLXIntegerToFontStyle(aFontStyle:integer):TFontStyles;

implementation

function CLXColorToVCLColor(CLXColor:integer):integer;
begin
 Result:=CLXColor AND $00FFFFFF;
end;

function CLXIntegerToFontStyle(aFontStyle:Integer):TFontStyles;
begin
 Result:=[];
end;
end.
