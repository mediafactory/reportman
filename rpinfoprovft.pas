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

uses Classes,SysUtils,
{$IFDEF USEVARIANTS}
    Types,
{$ENDIF}
{$IFDEF MSWINDOWS}
    Windows,
{$ENDIF}
    rpinfoprovid,SyncObjs,
    rpmdconsts,rpfreetype2;


type
 TRpWidths=array [32..255] of integer;

 TRpLogFont=class(TObject)
  fixedpitch:boolean;
  postcriptname:string;
  familyname:String;
  stylename:string;
  italic:Boolean;
  bold:Boolean;
  filename:String;
  widths:TRpWidths;
  ascent:integer;
  descent:integer;
  weight:integer;
  MaxWidth:integer;
  avCharWidth:Integer;
  Capheight:integer;
  ItalicAngle:double;
  BBox:TRect;
  StemV:double;
  // kerning is not implemented but here checked
  havekerning:Boolean;
  type1:boolean;
 end;

 TRpFTInfoProvider=class(TInterfacedObject,IRpInfoProvider)
  currentname:String;
  defaultfont:TRpLogFont;
  currentstyle:integer;
  alibrary:FT_Library;
  currentfont:TRpLogFont;
  crit:TCriticalSection;
  procedure InitLibrary;
  procedure SelectFont(pdffont:TRpPDFFOnt);
  procedure FillFontInfo(pdffont:TRpPDFFont;info:TRpTTFontInfo);
  procedure FillFontData(pdffont:TRpPDFFont;data:TRpTTFontData);
  constructor Create;
  destructor destroy;override;
 end;

implementation

var
  fontlist:TStringList;
  fontpaths:TStringList;
  fontfiles:TStringList;


const
 TTF_PRECISION=1000;


// add self directory and subdirectories to the lis
procedure Parsedir(alist:TStringList;adir:string);
var
 f:TSearchRec;
 retvalue:integer;
begin
 adir:=ExpandFileName(adir);
 alist.Add(adir);
 retvalue:=SysUtils.FindFirst(adir+C_DIRSEPARATOR+'*',faDirectory,F);
 if 0=retvalue then
 begin
  try
   while retvalue=0 do
   begin
    if ((F.Name<>'.') AND (F.Name<>'..')) then
    begin
     if (f.Attr AND faDirectory)<>0 then
      Parsedir(alist,adir+C_DIRSEPARATOR+F.Name);
    end;
    retvalue:=SysUtils.FindNext(F);
   end;
  finally
   SysUtils.FindClose(F);
  end;
 end;
end;

// Parses /etc/fonts/fonts.conf for font directories
// also includes subdirectories
procedure GetFontsDirectories(alist:TStringList);
var
 afile:TStringList;
 astring:String;
 diderror:Boolean;
 apath:String;
 index:integer;
begin
 diderror:=false;
 alist.clear;
 afile:=TStringList.create;
 try
  afile.LoadFromFile('/etc/fonts/fonts.conf');
 except
  afile.free;
  diderror:=true;
 end;
 if diderror then
 begin
  // Default font directories
  ParseDir(alist,'/usr/X11R6/lib/X11/fonts');
  ParseDir(alist,'~/fonts');
  exit;
 end;
 astring:=afile.Text;
 index:=Pos('<dir>',astring);
 while index>0 do
 begin
  astring:=Copy(astring,index+5,Length(astring));
  index:=Pos('</dir>',astring);
  if index>0 then
  begin
   apath:=Copy(astring,1,index-1);
   ParseDir(alist,apath);
   astring:=Copy(astring,index+6,Length(astring));
  end;
  index:=Pos('<dir>',astring);
 end;
end;

procedure TRpFTInfoProvider.InitLibrary;
var
 i:integer;
 f:TSearchRec;
 retvalue:integer;
 aobj:TRpLogFont;
 afilename:string;
// kerningfile:string;
 alibrary:FT_Library;
 aface:FT_Face;
 w:FT_UInt;
 validcmap:boolean;
 awide:WideString;
 aucs:UCS4String;
 convfactor,widthmult:Double;
 j,k:FT_ULong;
 indexl,indexr:FT_UInt;
begin
 if Assigned(fontlist) then
  exit;
 CheckFreeTypeLoaded;
 // reads font directory
 fontlist:=TStringList.Create;
 fontfiles:=TStringList.Create;
 fontfiles.Sorted:=true;
 fontpaths:=TStringList.Create;

 GetFontsDirectories(fontpaths);
 fontfiles.Clear;
 for i:=0 to fontpaths.Count-1 do
 begin
  retvalue:=SysUtils.FindFirst(fontpaths.strings[i]+C_DIRSEPARATOR+'*.pf*',faAnyFile,F);
  if 0=retvalue then
  begin
   try
    while retvalue=0 do
    begin
     if ((F.Name<>'.') AND (F.Name<>'..')) then
     begin
      if (f.Attr AND faDirectory)=0 then
       fontfiles.Add(fontpaths.strings[i]+C_DIRSEPARATOR+F.Name);
     end;
     retvalue:=SysUtils.FindNext(F);
    end;
   finally
    SysUtils.FindClose(F);
   end;
  end;
  retvalue:=SysUtils.FindFirst(fontpaths.strings[i]+C_DIRSEPARATOR+'*.ttf',faAnyFile,F);
  if 0=retvalue then
  begin
   try
    while retvalue=0 do
    begin
     if ((F.Name<>'.') AND (F.Name<>'..')) then
     begin
      if (f.Attr AND faDirectory)=0 then
       fontfiles.Add(fontpaths.strings[i]+C_DIRSEPARATOR+F.Name);
     end;
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
   afilename:=fontfiles.strings[i];
   if FT_New_Face(alibrary,Pchar(afilename),0,aface)=0 then
   begin
    try
     // Add it only if it's a TrueType or OpenType font
     // Type1 fonts also supported
     if  (FT_FACE_FLAG_SCALABLE AND aface.face_flags)<>0 then
     begin
      CheckFreeType(FT_Set_Char_Size(aface,0,64*100,300,300));
      aobj:=TRpLogFont.Create;
      try
       // Fill font properties
       aobj.Type1:=(FT_FACE_FLAG_SFNT AND aface.face_flags)=0;
       if aobj.Type1 then
       begin
        convfactor:=1;
        widthmult:=1;
        // Try finding and reading kernings
//        kerningfile:=ChangeFileExt(afilename,'.afm');
//        if FileExists(kerningfile) then
//        begin
//         CheckFreeType(FT_Attach_File(aface,Pchar(kerningfile)));
//        end;
       end
       else
       begin
        // Sizes perfect in Suse 9.1
        // Truetype should contain 2048 value but contains 1000
        // In Suse 9.0 contains 2048

//        convfactor:=916/1880*2048/aface.units_per_EM;
//        convfactor:=916/1880;
//        convfactor:=1400/1880;
        convfactor:=1;
        widthmult:=1;
        // Note same text printed with OpenOffice, can be larger
        // If exported to PDF from OpenOffice will be the same
       end;
       aobj.filename:=fontfiles.strings[i];
       aobj.postcriptname:=StringReplace(StrPas(aface.family_name),' ','',[rfReplaceAll]);
       aobj.familyname:=StrPas(aface.family_name);
       aobj.fixedpitch:=(aface.face_flags AND FT_FACE_FLAG_FIXED_WIDTH)<>0;
       aobj.HaveKerning:=(aface.face_flags AND FT_FACE_FLAG_KERNING)<>0;
       aobj.BBox.Left:=Round(convfactor*aface.bbox.xMin);
       aobj.BBox.Right:=Round(convfactor*aface.bbox.xMax);
       aobj.BBox.Top:=Round(convfactor*aface.bbox.yMax);
       aobj.BBox.Bottom:=Round(convfactor*aface.bbox.yMin);
       aobj.ascent:=Round(convfactor*aface.ascender);
       aobj.descent:=Round(convfactor*aface.descender);
       aobj.MaxWidth:=Round(convfactor*aface.max_advance_width);
       aobj.Capheight:=Round(convfactor*aface.ascender);
       aobj.stylename:=StrPas(aface.style_name);
       aobj.bold:=(aface.style_flags AND FT_STYLE_FLAG_BOLD)<>0;
       aobj.italic:=(aface.style_flags AND FT_STYLE_FLAG_ITALIC)<>0;
       validcmap:=false;
       if FT_Select_Charmap(aface,FT_ENCODING_ADOBE_LATIN_1)=0 then
        validcmap:=true
       else
       if FT_Select_Charmap(aface,FT_ENCODING_UNICODE)=0 then
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
         if 0=FT_Load_Char(aface,j,FT_LOAD_NO_SCALE) then
         begin
          // Some fonts translated from freetype returns fixed
          // advance (wrong result)
          // for example casmira.ttf
          // take care kerning is not implemented
          aobj.Widths[j]:=Round(widthmult*aface.glyph.advance.x);
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
     FT_Done_Face(aface);
    end;
   end;
  end;
 finally
  FT_Done_FreeType(alibrary);
 end;
end;

constructor TRpFTInfoProvider.Create;
begin
 currentname:='';
 currentstyle:=0;
 crit:=TCriticalSection.Create;
end;


procedure FreeFontList;
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
  fontpaths.free;
  fontfiles.free;
 end;
end;

destructor TRpFTInfoProvider.destroy;
begin
 crit.free;

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
 crit.Enter;
 try
  InitLibrary;
 finally
  crit.Leave;
 end;
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
 crit.Enter;
 try
  InitLibrary;
 finally
  crit.Leave;
 end;
 SelectFont(pdffont);
 for i:=32 to 255 do
 begin
  info.charwidths[i]:=currentfont.widths[i];
 end;
end;

procedure TRpFTInfoProvider.FillFontData(pdffont:TRpPDFFont;data:TRpTTFontData);
begin
 crit.Enter;
 try
  InitLibrary;
 finally
  crit.Leave;
 end;
 // See if data can be embedded
 SelectFont(pdffont);
 data.fontdata.Clear;
 if not currentfont.type1 then
  data.fontdata.LoadFromFile(currentfont.filename);
 data.postcriptname:=currentfont.postcriptname;
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
 if not currentfont.bold then
 begin
  if pdffont.Bold then
   data.postcriptname:=data.postcriptname+',Bold';
 end;
 if currentfont.italic then
   data.Flags:=data.Flags+64
 else
 begin
  if pdffont.Italic then
  begin
   if data.postcriptname<>currentfont.postcriptname then
    data.postcriptname:=data.postcriptname+'Italic'
   else
    data.postcriptname:=data.postcriptname+',Italic';
  end;
 end;
 data.Type1:=currentfont.Type1;
end;


initialization
 fontlist:=nil;
finalization
 FreeFontList;
end.
