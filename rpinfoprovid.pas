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

 TRpTTFontInfo=class(TObject)
  charwidths:TWinAnsiWidthsArray;
  ttname:String;
  ObjectName:String;
  ObjectIndex:integer;
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
  ObjectIndex:integer;
  DescriptorIndex:Integer;
 end;

 IRpInfoProvider=interface
  ['{59F66653-ACEC-4FC9-B918-C22136F576F1}']
  procedure FillFontInfo(pdffont:TRpPDFFont;info:TRpTTFontInfo);
  procedure FillFontData(pdffont:TRpPDFFont;data:TRpTTFontData);
 end;


implementation

constructor TrpPdfFont.Create;
begin
 inherited Create;

 Name:=poCourier;
 Size:=10;
end;

end.
