{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpgdifonts                                      }
{       Utilities for Windows GDI fonts                 }
{       allow the use of device dependent fonts         }
{       in a device independent way                     }
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

unit rpgdifonts;

interface

uses windows, Messages, SysUtils, Classes, Graphics, Controls,printers,
 rpmunits,Forms;

type
  TFontStep=(cpi20,cpi17,cpi15,cpi12,cpi10,cpi6,cpi5);

  TPrinterFont=class(TObject)
   private
    FFont:TFont;
    procedure SetFont(Valor:TFont);
   public
    fstep:TFontStep;
    LogFont:TLogFont;
    isred:boolean;
    constructor Create;virtual;
    destructor destroy;override;
    property Font:TFont read FFont write SetFont;
   end;

   TGDIFontType=(tfontraster,tfonttruetype,tfontdevice,tfonttruetypedevice);
   TCaracfont=class(TObject)
    public
     name:string;
     sizes:TStringlist;
     ftype:TGDIFontType;
     fstep:TFontStep;
     fixed:boolean;
     constructor Create;
     destructor destroy;override;
    end;

function UpdatePrinterFontList:boolean;

procedure FindDeviceFont(DC:HDC;Font:Tfont;fstep:TFontStep);
function FindRotatedFont(Desti:HDC;Font:TFont;rotation:integer):HFont;
function FontStepToString(fstep:TFontStep):string;
function StringToPasFont(cad:string):TFontStep;
function FindFontStep(Font:TFont):integer;
function FontSizeToStep(asize:integer):TFontStep;

var
 PrinterFonts:TList;
 PrinterSorted:TStringList;
 caracfonts:TStringlist;
 ScreenSorted:TStringList;
 valorsCPITWIPS:array [cpi20..cpi5] of integer=(76,84,96,120,144,240,288);
 valorsCPICPI:array [cpi20..cpi5] of integer=(20,17,15,12,10,6,5);
implementation

var
 currentprinter:integer;
 sizestruetype:Tstringlist;

function FontSizeToStep(asize:integer):TFontStep;
begin
 case asize of
  8:
   Result:=cpi17;
  9:
   Result:=cpi15;
  10:
   Result:=cpi12;
  11..12:
   Result:=cpi10;
  13..15:
    Result:=cpi6;
  else
   begin
    if asize>15 then
     Result:=cpi5
    else
     REsult:=cpi20;
   end;
 end;
end;

procedure lliberacaracfonts;
var
 i:integeR;
begin
 for i:=0 to caracfonts.count-1 do
 begin
  caracfonts.objects[i].free;
 end;
 caracfonts.clear;
end;

function CompareFonts(font1,font2:pointer):integer;
var
 log1,log2:TPrinterFont;
begin
 log1:=TPrinterFont(Font1);
 log2:=TPrinterFont(Font2);
 if log1.fstep>log2.fstep then
  Result:=1
 else
  if log1.fstep<log2.fstep then
   result:=-1
  else
  begin
   if log1.Font.Name>log2.Font.Name then
    Result:=-1
   else
    if log1.Font.Name>log2.Font.Name then
     Result:=1
    else
     Result:=0;
  end;
end;

constructor TPrinterFont.Create;
begin
 FFOnt:=TFont.Create;
end;

destructor TPrinterFont.Destroy;
begin
 FFont.Destroy;
 inherited Destroy;
end;

procedure TPrinterFont.SetFont(Valor:TFont);
begin
 FFOnt.Assign(Valor);
end;

function enumfontfamprocbase(var ENUMLOGFONT:TEnumlogfont;var TextMetric:TNewTextMetric;
          FontType:integer;Data:integer):integer;stdcall;
begin
 TStringList(Data).Add(enumlogfont.elfLogFont.lfFaceName);
 Result:=1;
end;


function enumfontfamproc(var ENUMLOGFONT:TEnumlogfont;var TextMetric:TNewTextMetric;
          FontType:integer;Data:integer):integer;stdcall;
var
 Fontimp:TPrinterFont;
 index:integer;
 carac:TCaracFont;
 size:integer;
 logfont:TLogFont;
begin
 fontimp:=nil;
 logfont:=enumlogfont.elfLogFont;
 size:=LogFont.lfHeight*240 div 3500;
 if (fonttype=3)then
 begin
  FontImp:=TPrinterFont.Create;
  case LogFont.lfWidth of
   40..80: FOntImp.fstep:=cpi20;
   81..90: FOntImp.fstep:=cpi17;
   91..110: FOntImp.fstep:=cpi15;
   111..135: FOntImp.fstep:=cpi12;
   136..180: FOntImp.fstep:=cpi10;
   181..240: FOntImp.fstep:=cpi6;
   241..400: FOntImp.fstep:=cpi5;
  else
    FontImp.fstep:=cpi10;
  end;
  FontImp.LogFont:=LogFont;
  FontImp.Font.Name:=LogFont.lfFaceName;
  if Pos('RED',UpperCase(FontImp.Font.Name))>0 then
  begin
   Fontimp.isred:=true;
  end
  else
  begin
   if Pos('ROJO',UpperCase(FontImp.Font.Name))>0 then
    Fontimp.isred:=true
   else
    fontimp.isred:=false;
  end;
  FontImp.Font.Size:=LogFont.lfHeight*POINTS_PER_INCHESS div TWIPS_PER_INCHESS;
//  FontImp.Font.Height:=LogFont.lfHeight;
//  FontImp.Font.:=LogFont.lfHeight;
  TList(Data).Add(FontImp);
 end;
 // Caracfonts
 index:=caracfonts.IndexOf(logfont.lfFaceName);
 if index<0 then
 begin
  carac:=TCaracFont.create;
  carac.name:=logfont.lfFaceName;
  if (fonttype=3)then
  begin
   carac.ftype:=tfontdevice;
   carac.fstep:=fontimp.fstep;
  end
  else
   if (fonttype=2) then
    carac.ftype:=tfonttruetypedevice
   else
    if (fonttype AND RASTER_FONTTYPE)>0 then
     carac.ftype:=tfontraster
    else
     carac.ftype:=tfonttruetype;
  if carac.ftype=tfonttruetype then
   carac.sizes.assign(sizestruetype)
  else
   carac.sizes.Add(Format('%3s',[IntTostr(size)]));
  carac.fixed:=((textmetric.tmPitchAndFamily AND TMPF_FIXED_PITCH)=0);
  caracfonts.addobject(carac.name,carac);
 end
 else
 begin
  carac:=Tcaracfont(caracfonts.objects[index]);
  carac.sizes.Add(Format('%3s',[IntTostr(size)]));
 end;
 Result:=1;
end;


function UpdatePrinterFontList:boolean;
var
 Anticmapmode:HDC;
 base:TStringList;
 i:integer;
begin
 Result:=false;
 if Printer.printers.count<1 then
 begin
  PrinterFonts.clear;
  exit;
 end;
 // If the list is updated exit
 if currentprinter=Printer.PrinterIndex then
  Exit;
 result:=true;
 currentprinter:=printer.printerindex;
 lliberacaracfonts;
 PrinterFonts.Clear;
 anticmapmode:=SetMapMode(Printer.Handle,MM_TWIPS);
 try
  base:=TStringList.create;
  try
   EnumFontfamilies(Printer.Handle,nil,@enumfontfamprocbase,Integer(Pointer(base)));
   for i:=0 to base.count-1 do
   begin
    EnumFontfamilies(Printer.Handle,Pchar(base.strings[i]),@enumfontfamproc,Integer(Pointer(PrinterFonts)));
   end;
  finally
   base.free;
  end;
 finally
  SetMapMode(Printer.Handle,anticmapmode);
 end;
 printerFonts.Sort(CompareFonts);
 printerSorted.Assign(Printer.Fonts);
end;

procedure FindScreenDeviceFont(FOnt:TFont;fstep:TFontStep);
var LogFont:TLogFont;
begin
 // To skip device font simulation ignore this function
// Exit;
 // This function selects a font that is like printer font
 // with the selected step

 LogFont.lfHeight:=Font.Height;

 LogFont.lfWidth:=Trunc(ValorsCPITWIPS[fstep]/TWIPS_PER_INCHESS*Screen.PixelsPerInch);
 LogFont.lfEscapement:=0;
 LogFont.lfOrientation:=0;
// if fsBold in Font.Style then
//  LogFont.lfWeight:=FW_BOLD
// else
//  LogFont.lfWeight:=FW_NORMAL;
 LogFont.lfWeight:=FW_dontcaRE;
 if fsITalic in Font.Style then
  LogFont.lfItalic:=1
 else
  LogFont.lfItalic:=0;
 if fsUnderline in Font.Style then
  LogFont.lfUnderline:=1
 else
  Logfont.lfUnderline:=0;
 if fsStrikeOut in Font.Style then
  LogFont.lfStrikeOut:=1
 else
  LogFont.lfStrikeOut:=0;
 LogFont.lfCharSet:=DEFAULT_CHARSET;
 lOGfONT.lfOutPrecision:=OUT_tt_onLy_PRECIS;
 LogFont.lfClipPrecision:=CLIP_DEFAULT_PRECIS;
 // Low Quality high measurement precision
 // LogFont.lfQuality:=Draft_QUALITY;
 // Improving quality
 LogFont.lfQuality:=PROOF_QUALITY;
 LogFont.lfPitchAndFamily:=FF_DONTCARE or FIXED_PITCH;
 LogFont.lffACEnAME:='';
 Font.handle:= CreateFontIndirect(LogFont);
 if Font.Color=clWhite then
  Font.Color:=clBlack;
end;

procedure FindPrinterDeviceFont(FOnt:TFont;fstep:TFontStep);
var
 i,fontsize:integer;
 fontname:string;
 font1:TPrinterFont;
 fontstep:TFontStep;
begin
 fontsize:=10;
 fontstep:=cpi10;
 if printerFonts.Count<1 then
  Exit;
 i:=0;
 fontname:='';
 while i<PrinterFonts.Count do
 begin
  font1:=TPrinterFont(printerfonts.Items[i]);
  if Font.Name=font1.font.name then
  begin
   fontname:='';
   break;
  end;
  if fstep>=font1.fstep then
  begin
   // Font Color
   if font.color=clred then
   begin
    if font1.isred then
    begin
     fontname:=font1.Font.Name;
     fontsize:=font1.Font.Size;
     fontstep:=font1.fstep;
    end;
   end
   else
   begin
    if Not font1.isred then
    begin
     fontname:=font1.Font.Name;
     fontsize:=font1.Font.Size;
     fontstep:=font1.fstep;
    end;
   end;
  end
  else
   break;
  inc(i);
 end;
 if fontname<>'' then
 begin
  // Looks for draft fonts in this size
  i:=0;
  while i<PrinterFonts.Count do
  begin
   font1:=TPrinterFont(printerfonts.Items[i]);
   if font1.fstep=fontstep then
   begin
    if (Pos('SUPER DRAFT',UpperCase(font1.Font.Name))>0) then
    begin
     fontname:=font1.Font.Name;
     break;
    end;
    if (Pos('DRAFT',UpperCase(font1.Font.Name))>0) then
    begin
     fontname:=font1.Font.Name;
    end;
   end;
   inc(i);
  end;
  Font.Name:=fontname;
  Font.Size:=fontsize;
 end;
end;

procedure FindDeviceFont(DC:HDC;Font:Tfont;fstep:TFontStep);
begin
 if Printer.Printers.count<1 then
 begin
  FindScreenDeviceFont(Font,fstep);
  exit;
 end;
 if DC=Printer.Handle then
  FindPrinterDeviceFont(Font,fstep)
 else
  FindScreenDeviceFont(Font,fstep);
end;

function FindFontStep(Font:TFont):integer;
var
 anticobject:integer;
 metric:TTextMetric;
begin
 if Printer.Printers.count<1 then
 begin
  Result:=10;
  exit;
 end;
 anticobject:=SelectObject(Printer.handle,Font.handle);
 try
  GetTextMetrics(Printer.handle,metric);
  Result:=metric.tmAveCharWidth;
 finally
  SelectObject(Printer.handle,anticobject);
 end;
end;

function FindRotatedFont(DEsti:HDC;Font:TFont;rotation:integer):HFont;
var
 logfont:tlogfont;
begin
{ LogFont.lfHeight:=Font.Height;
 LogFont.lfWidth:=0;
 LogFont.lfEscapement:=rotation;
 LogFont.lfOrientation:=rotation;
} if fsBold in Font.Style then
  LogFont.lfWeight:=FW_BOLD
 else
  LogFont.lfWeight:=FW_NORMAL;
 LogFont.lfWeight:=FW_dontcaRE;
 if fsITalic in Font.Style then
  LogFont.lfItalic:=1
 else
  LogFont.lfItalic:=0;
 if fsUnderline in Font.Style then
  LogFont.lfUnderline:=1
 else
  Logfont.lfUnderline:=0;
 if fsStrikeOut in Font.Style then
  LogFont.lfStrikeOut:=1
 else
  LogFont.lfStrikeOut:=0;
{ LogFont.lfCharSet:=DEFAULT_CHARSET;
 lOGfONT.lfOutPrecision:=OUT_DEFAULT_PRECIS;
 LogFont.lfClipPrecision:=CLIP_DEFAULT_PRECIS;
 // Poca qualitat, gran semblança
 // LogFont.lfQuality:=Draft_QUALITY;
 // Gran qualitat, semblança normal
// LogFont.lfQuality:=PROOF_QUALITY;
 LogFont.lfQuality:=DEFAULT_QUALITY;
 LogFont.lfPitchAndFamily:=FF_DONTCARE {or FIXED_PITCH};

  LOGFONT.lfheight:=-MulDiv(Font.size, GetDeviceCaps(desti, LOGPIXELSY), POINTS_PER_INCHESS);
  logfont.lfwidth:=0;
  logfont.lfWeight:=FW_DONTCARE;
  logfont.lfEscapement:=rotation;
  logfont.lfOrientation:=rotation;
{  logfont.lfItalic:=0;
  logfont.lfUnderline:=0;
   logfont.lfStrikeout:=0;
}  logfont.lfCharSet:=DEFAULT_CHARSET;
  logfont.lfOutPrecision:=OUT_DEFAULT_PRECIS;
  logfont.lfclipprecision:=CLIP_DEFAULT_PRECIS;
  logfont.lfQuality:=DRAFT_QUALITY;
  logfont.lfPitchAndFamily:=DEFAULT_PiTCH;
  logfont.lfFaceName[0]:=chr(0);
  StrPCopy(LogFont.lffACEnAME,Font.Name);
  Result:=CreateFontIndirect(LOGFONT);
end;

function FontStepToString(fstep:TFontStep):string;
begin
 case fstep of
  cpi20:
   Result:='20 cpi';
  cpi17:
   Result:='17 cpi';
  cpi15:
   Result:='15 cpi';
  cpi12:
   Result:='12 cpi';
  cpi10:
   Result:='10 cpi';
  cpi6:
   Result:='6 cpi';
  cpi5:
   Result:='5 cpi';
 end;
end;

function StringToPasFont(cad:string):TFontStep;
begin
 result:=cpi10;
 if cad='10 cpi' then
 begin
  result:=cpi10;
  exit;
 end;
 if cad='12 cpi' then
 begin
  result:=cpi12;
  exit;
 end;
 if cad='15 cpi' then
 begin
  result:=cpi15;
  exit;
 end;
 if cad='5 cpi' then
 begin
  result:=cpi5;
  exit;
 end;
 if cad='6 cpi' then
 begin
  result:=cpi6;
  exit;
 end;
 if cad='20 cpi' then
 begin
  result:=cpi20;
  exit;
 end;
 if cad='17 cpi' then
 begin
  result:=cpi17;
  exit;
 end;
end;

constructor TCaracfont.Create;
begin
 sizes:=TStringList.create;
 sizes.sorted:=true;
end;

destructor TCaracfont.destroy;
begin
 sizes.free;
 inherited destroy;
end;



initialization
PrinterSorted:=TStringList.Create;
PrinterSorted.Sorted:=True;
PrinterFonts:=TList.Create;
ScreenSorted:=TStringList.Create;
ScreenSorted.Sorted:=True;
caracfonts:=TStringlist.create;
caracfonts.sorted:=true;
ScreenSorted.Assign(Screen.Fonts);
currentprinter:=-1;
sizestruetype:=Tstringlist.create;
with sizestruetype do
begin
 Add('  8');
 Add('  9');
 Add(' 10');
 Add(' 11');
 Add(' 12');
 Add(' 14');
 Add(' 16');
 Add(' 18');
 Add(' 20');
 Add(' 22');
 Add(' 24');
 Add(' 26');
 Add(' 28');
 Add(' 36');
 Add(' 48');
 Add(' 72');
end;


finalization
PrinterFonts.free;
PrinterSorted.free;
ScreenSorted.free;
lliberacaracfonts;
caracfonts.free;
sizestruetype.free;
end.
