{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       TRpInfoProvider Fretype library                 }
{       Provides information about fonts and bitmaps    }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{                                                       }
{*******************************************************}

unit rpinfoprovft;


interface

{$I rpconf.inc}

uses Classes,SysUtils,Windows,rpinfoprovid,
    rpmdconsts,rpfreetype2;


type
 TRpLogFont=class(TObject)
  fixedpitch:boolean;
  postcripname:string;
  familyname:String;
  stylename:string;
  italic:Boolean;
  bold:Boolean;
  filename:String;
  widths:array [32..255] of integer;
  ascent:integer;
  descent:integer;
  weight:integer;
  MaxWidth:integer;
  avCharWidth:Integer;
  Capheight:integer;
  ItalicAngle:double;
  BBox:TRect;
  StemV:double;
 end;

 TRpFTInfoProvider=class(TInterfacedObject,IRpInfoProvider)
  currentname:String;
  defaultfont:TRpLogFont;
  currentstyle:integer;
  alibrary:FT_Library;
  fontlist:TStringList;
  fontpaths:TStringList;
  fontfiles:TStringList;
  currentfont:TRpLogFont;
  procedure InitLibrary;
  procedure SelectFont(pdffont:TRpPDFFOnt);
  procedure FillFontInfo(pdffont:TRpPDFFont;info:TRpTTFontInfo);
  procedure FillFontData(pdffont:TRpPDFFont;data:TRpTTFontData);
  constructor Create;
  destructor destroy;override;
 end;

implementation

const
 TTF_PRECISION=1000;

procedure GetFontsDirectories(alist:TStringList);
begin
 alist.Add('c:\winnt\fonts\*.ttf');
end;

procedure TRpFTInfoProvider.InitLibrary;
var
 i,j:integer;
 f:TSearchRec;
 retvalue:integer;
 aobj:TRpLogFont;
 alibrary:FT_Library;
 aface:FT_Face;
 w:FT_UInt;
 validcmap:boolean;
begin
 if Assigned(fontlist) then
  exit;
 CheckFreeTypeLoaded;
 // reads font directory
 fontlist:=TStringList.Create;
 GetFontsDirectories(fontpaths);
 fontfiles.Clear;
 for i:=0 to fontpaths.Count-1 do
 begin
  retvalue:=SysUtils.FindFirst(fontpaths.strings[i],faAnyFile,F);
  if 0=retvalue then
  begin
   try
    while retvalue=0 do
    begin
     fontfiles.Add(ExtractFilePath(fontpaths.strings[i])+C_DIRSEPARATOR+F.Name);
     retvalue:=SysUtils.FindNext(F);
    end;
   finally
    SysUtils.FindClose(F);
   end;
  end;
 end;
 defaultfont:=nil;
 CheckFreeType(FT_Init_FreeType(alibrary));
 try
  // Now fill the font list with all font files
  for i:=0 to fontfiles.Count-1 do
  begin
   CheckFreeType(FT_New_Face(alibrary,Pchar(fontfiles.strings[i]),0,aface));
   try
    // Add it only if it's a TrueType or OpenType font
    if  (FT_FACE_FLAG_SFNT AND aface.face_flags)<>0 then
    begin
     aobj:=TRpLogFont.Create;
     try
      // Fill font properties
      aobj.filename:=fontfiles.strings[i];
      aobj.postcripname:=StrPas(aface.style_name);
      aobj.familyname:=StrPas(aface.family_name);
      aobj.fixedpitch:=(aface.face_flags AND FT_FACE_FLAG_FIXED_WIDTH)<>0;
      aobj.BBox.Left:=aface.bbox.xMin;
      aobj.BBox.Right:=aface.bbox.xMax;
      aobj.BBox.Top:=aface.bbox.yMin;
      aobj.BBox.Bottom:=aface.bbox.yMax;
      aobj.ascent:=aface.ascender;
      aobj.descent:=aface.descender;
      aobj.MaxWidth:=aface.max_advance_width;
      aobj.Capheight:=aface.ascender;
      aobj.stylename:=StrPas(aface.style_name);
      aobj.bold:=(aface.style_flags AND FT_STYLE_FLAG_BOLD)<>0;
      aobj.italic:=(aface.style_flags AND FT_STYLE_FLAG_ITALIC)<>0;
      validcmap:=false;
      if FT_Select_Charmap(aface,FT_ENCODING_UNICODE)=0 then
       validcmap:=true
      else
       if FT_Select_Charmap(aface,FT_ENCODING_ADOBE_LATIN_1)=0 then
        validcmap:=true
       else
        if FT_Select_Charmap(aface,FT_ENCODING_ADOBE_STANDARD)=0 then
         validcmap:=true
        else
         if FT_Select_Charmap(aface,FT_ENCODING_OLD_LATIN_2)=0 then
          validcmap:=true;
      for j:=32 to 255 do
      begin
       if validcmap then
       begin
        w:=FT_Get_Char_Index(aface,j);
        if W>0 then
        begin
         CheckFreeType(FT_Load_Glyph(aface,w,FT_LOAD_NO_SCALE));
         aobj.Widths[j]:=aface.glyph.metrics.width;
        end
        else
         aobj.Widths[j]:=0;
       end
       else
        aobj.Widths[j]:=0;
      end;
      if not assigned(defaultfont) then
      begin
       if ((not aobj.italic) and (not aobj.bold)) then
        defaultfont:=aobj;
      end
      else
      begin
       if ((not aobj.italic) and (not aobj.bold)) then
       begin
        if ((aobj.familyname='Arial') or
         (aobj.familyname='Helvetica')) then
        begin
         defaultfont:=aobj;
        end;
       end;
      end;
      fontlist.AddObject(UpperCase(aobj.familyname),aobj);
     except
      aobj.free;
     end;
    end;
   finally
    CheckFreeType(FT_Done_Face(aface));
   end;
  end;
 finally
  CheckFreeType(FT_Done_FreeType(alibrary));
 end;
end;

constructor TRpFTInfoProvider.Create;
begin
 currentname:='';
 currentstyle:=0;
 fontlist:=nil;
 fontfiles:=TStringList.Create;
 fontpaths:=TStringList.Create;
end;

destructor TRpFTInfoProvider.destroy;
var
 i:integer;
begin
 if assigned(fontlist) then
 begin
  for i:=0 to fontlist.count-1 do
  begin
   fontlist.Objects[i].free;
  end;
  fontlist.clear;
  fontlist.free;
  fontlist:=nil;
 end;
 fontpaths.free;
 fontfiles.free;
 inherited destroy;
end;

procedure TRpFtInfoProvider.SelectFont(pdffont:TRpPDFFOnt);
var
 afontname:string;
 isbold:boolean;
 isitalic:boolean;
 i:integer;
 match:boolean;
 afont:TRpLogFont;
begin
 InitLibrary;
{$IFDEF MSWINDOWS}
 afontname:=UpperCase(pdffont.WFontName);
{$ENDIF}
{$IFDEF LINUX}
 afontname:=UpperCase(pdffont.LFontName);
{$ENDIF}
 if ((currentname=afontname) and (currentstyle=pdffont.Style)) then
  exit;
 currentname:=afontname;
 currentstyle:=pdffont.Style;
 // Selects de font by font matching
 // First exact coincidence of family and style
 isbold:=(pdffont.style and 1)>0;
 isitalic:=(pdffont.style and (1 shl 1))>0;
 match:=false;
 i:=0;
 while i<fontlist.Count do
 begin
  if fontlist.strings[i]=afontname then
  begin
   afont:=TRpLogFont(fontlist.Objects[i]);
   if isitalic=afont.italic then
    if isbold=afont.bold then
    begin
     match:=true;
     currentfont:=afont;
     break;
    end;
  end;
  inc(i);
 end;
 if match then
  exit;
 // If not matching search for similar font name
 i:=0;
 while i<fontlist.Count do
 begin
  if Pos(afontname,fontlist.strings[i])>0 then
  begin
   afont:=TRpLogFont(fontlist.Objects[i]);
   if isitalic=afont.italic then
    if isbold=afont.bold then
    begin
     match:=true;
     currentfont:=afont;
     break;
    end;
  end;
  inc(i);
 end;
 if match then
  exit;
 // Ignoring styles
 match:=false;
 i:=0;
 while i<fontlist.Count do
 begin
  if fontlist.strings[i]=afontname then
  begin
   afont:=TRpLogFont(fontlist.Objects[i]);
   match:=true;
   currentfont:=afont;
   break;
  end;
  inc(i);
 end;
 if match then
  exit;
 // Ignoring styles partial match
 match:=false;
 i:=0;
 while i<fontlist.Count do
 begin
  if Pos(afontname,fontlist.strings[i])>0 then
  begin
   afont:=TRpLogFont(fontlist.Objects[i]);
   match:=true;
   currentfont:=afont;
   break;
  end;
  inc(i);
 end;
 if match then
  exit;
 // Finally gets default font
 currentfont:=defaultfont;
 if not assigned(currentfont) then
  Raise Exception.Create('No active font');
end;


procedure TRpFTInfoProvider.FillFontInfo(pdffont:TRpPDFFont;info:TRpTTFontInfo);
var
 i:integer;
begin
 InitLibrary;
 SelectFont(pdffont);
 for i:=32 to 255 do
 begin
  info.charwidths[i]:=currentfont.widths[i];
 end;
end;

procedure TRpFTInfoProvider.FillFontData(pdffont:TRpPDFFont;data:TRpTTFontData);
begin
 InitLibrary;
 // See if data can be embedded
 SelectFont(pdffont);
 data.fontdata.Clear;
 data.fontdata.LoadFromFile(currentfont.filename);
 data.postcriptname:=currentfont.postcripname;
 data.FamilyName:=currentfont.familyname;
 data.FaceName:=currentfont.familyname;
 data.Ascent:=currentfont.ascent;
 data.Descent:=currentfont.descent;
 data.Leading:=0;
 data.capHeight:=currentfont.Capheight;
 data.FontWeight:=0;
 data.MaxWidth:=currentfont.MaxWidth;
 data.AvgWidth:=currentfont.avCharWidth;
 data.StemV:=0;
 data.FontStretch:='/Normal';
 data.FontBBox:=currentfont.BBox;
 if currentfont.italic then
  data.ItalicAngle:=-15
 else
  data.ItalicAngle:=0;
 data.StyleName:=currentfont.stylename;
 data.Flags:=32;
 if (currentfont.fixedpitch) then
  data.Flags:=data.Flags+1;
 if currentfont.italic then
   data.Flags:=data.Flags+64;
end;

end.
