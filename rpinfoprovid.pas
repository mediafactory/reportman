{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       TRpInfoProvider  Base class                     }
{       Provides information about fonts and bitmaps    }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{                                                       }
{*******************************************************}

unit rpinfoprovid;


interface

{$I rpconf.inc}

uses Classes,SysUtils,
{$IFDEF USEVARIANTS}
 Types,
{$ENDIF}
{$IFNDEF USEVARIANTS}
 Windows,
{$ENDIF}
 rptypes;

type
 TWinAnsiWidthsArray=array [32..255] of integer;
 PWinAnsiWidthsArray= ^TWinAnsiWidthsArray;

 TRpPDFFont=class(TObject)
  public
   Name:TRpType1Font;
   WFontName:WideString;
   LFontName:WideString;
   Size:integer;
   Color:integer;
   Style:Integer;
   Italic:Boolean;
   Underline:boolean;
   Bold:boolean;
   StrikeOut:boolean;
   constructor Create;
  end;



 TRpTTFontData=class(TObject)
  embedded:Boolean;
  fontdata:TMemoryStream;
  postcriptname:String;
  Encoding:String;
  Ascent,Descent,Leading,CapHeight,Flags,FontWeight:integer;
  MaxWidth:integer;
  AvgWidth:integer;
  StemV:double;
  FontFamily:String;
  FontStretch:String;
  ItalicAngle:double;
  FontBBox:TRect;
  FamilyName:String;
  FullName:String;
  FaceName:String;
  StyleName:String;
  type1:boolean;
  havekerning:Boolean;
  ObjectName:String;
  ObjectIndex:integer;
  ObjectIndexParent:integer;
  DescriptorIndex:Integer;
  loadedwidths,loadedkernings:TStringList;
  constructor Create;
  destructor Destroy;override;
 end;

 IRpInfoProvider=interface
  ['{59F66653-ACEC-4FC9-B918-C22136F576F1}']
  procedure FillFontData(pdffont:TRpPDFFont;data:TRpTTFontData);
  function GetCharWidth(pdffont:TRpPDFFont;data:TRpTTFontData;charcode:widechar):Integer;
  function GetKerning(pdffont:TRpPDFFont;data:TRpTTFontData;leftchar,rightchar:widechar):integer;
 end;


implementation

constructor TRpTTFontData.Create;
begin
 inherited Create;

 loadedwidths:=TStringList.Create;
 loadedwidths.sorted:=true;
 loadedkernings:=TStringList.Create;
 loadedkernings.sorted:=true;
end;

destructor TRpTTFontData.Destroy;
begin
 loadedkernings.free;
 loadedwidths.free;

 inherited;
end;

constructor TrpPdfFont.Create;
begin
 inherited Create;

 Name:=poCourier;
 Size:=10;
end;

end.
