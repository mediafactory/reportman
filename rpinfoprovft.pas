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

uses Classes,SysUtils,Windows,rpinfoprovid,
    rpgraphutilsvcl,rpmdconsts;


type
 TRpFTInfoProvider=class(TInterfacedObject,IRpInfoProvider)
  currentname:String;
  currentstyle:integer;
  procedure SelectFont(pdffont:TRpPDFFOnt);
  procedure FillFontInfo(pdffont:TRpPDFFont;info:TRpTTFontInfo);
  procedure FillFontData(pdffont:TRpPDFFont;data:TRpTTFontData);
  constructor Create;
  destructor destroy;override;
 end;

implementation

const
 TTF_PRECISION=1000;


constructor TRpFTInfoProvider.Create;
begin
 currentname:='';
 currentstyle:=0;
end;

destructor TRpFTInfoProvider.destroy;
begin
 inherited destroy;
end;

procedure TRpFtInfoProvider.SelectFont(pdffont:TRpPDFFOnt);
var
 LogFont:TLogFont;
begin
 if ((currentname=pdffont.WFontName) and (currentstyle=pdffont.Style)) then
  exit;
 currentname:=pdffont.WFontName;
 currentstyle:=pdffont.Style;
 if fonthandle<>0 then
 begin
  DeleteObject(fonthandle);
  fonthandle:=0;
 end;
 LogFont.lfHeight:=Round(-TTF_PRECISION*GetDeviceCaps(adc,LOGPIXELSX)/72);

 LogFont.lfWidth:=0;
 LogFont.lfEscapement:=0;
 LogFont.lfOrientation:=0;

 if (pdffont.style and 1)>0 then
  LogFont.lfWeight:=FW_BOLD
 else
  LogFont.lfWeight:=FW_NORMAL;
 if (pdffont.style and (1 shl 1))>0 then
  LogFont.lfItalic:=1
 else
  LogFont.lfItalic:=0;
 if (pdffont.style and (1 shl 2))>0 then
  LogFont.lfUnderline:=1
 else
  Logfont.lfUnderline:=0;
 if (pdffont.style and (1 shl 3))>0 then
  LogFont.lfStrikeOut:=1
 else
  LogFont.lfStrikeOut:=0;
 LogFont.lfCharSet:=DEFAULT_CHARSET;
 lOGfONT.lfOutPrecision:=OUT_tt_onLy_PRECIS;
 LogFont.lfClipPrecision:=CLIP_DEFAULT_PRECIS;
 LogFont.lfEscapement:=0;
 LogFont.lfOrientation:=0;
 // Low Quality high measurement precision
 // LogFont.lfQuality:=Draft_QUALITY;
 // Improving quality
 LogFont.lfQuality:=PROOF_QUALITY;
 LogFont.lfPitchAndFamily:=FF_DONTCARE or DEFAULT_PITCH;
 StrPCopy(LogFont.lffACEnAME,Copy(pdffont.WFontName,1,LF_FACESIZE));
 Fonthandle:= CreateFontIndirect(LogFont);
 SelectObject(adc,fonthandle);
end;


procedure TRpFTInfoProvider.FillFontInfo(pdffont:TRpPDFFont;info:TRpTTFontInfo);
var
 logx,i:integer;
 aabc:array [32..255] of ABC;
begin
 SelectFont(pdffont);
 logx:=GetDeviceCaps(adc,LOGPIXELSX);
 if not GetCharABCWidths(adc,32,255,aabc[32]) then
  RaiseLastOSError;
 for i:=32 to 255 do
  info.charwidths[i]:=Round(
   (Integer(aabc[i].abcA)+Integer(aabc[i].abcB)+Integer(aabc[i].abcC))/logx*72000/TTF_PRECISION
   );
end;

procedure TRpFTInfoProvider.FillFontData(pdffont:TRpPDFFont;data:TRpTTFontData);
var
 potm:POUTLINETEXTMETRIC;
 asize:integer;
 embeddable:boolean;
 logx:integer;
 multipli:double;
 apchar:PChar;
 alog:LOGFONT;
 acomp:byte;
begin
   // See if data can be embedded
   embeddable:=false;
   SelectFont(pdffont);
   data.postcriptname:='';
   data.Encoding:='WinAnsiEncoding';
   asize:=GetOutlineTextMetrics(adc,0,nil);
   if asize>0 then
   begin
    potm:=AllocMem(asize);
    try
     if 0<>GetOutlineTextMetrics(adc,asize,potm) then
     begin
      if (potm^.otmfsType AND $8000)=0 then
       embeddable:=true;
      logx:=GetDeviceCaps(adc,LOGPIXELSX);
      multipli:=1/logx*72000/TTF_PRECISION;
      data.Ascent:=Round(potm^.otmTextMetrics.tmAscent*multipli);
      data.Descent:=-Round(potm^.otmTextMetrics.tmDescent*multipli);
      data.FontWeight:=potm^.otmTextMetrics.tmWeight;
      data.FontBBox:=potm^.otmrcFontBox;
      data.FontBBox.Left:=Round(data.FontBBox.Left*multipli);
      data.FontBBox.Right:=Round(data.FontBBox.Right*multipli);
      data.FontBBox.Bottom:=Round(data.FontBBox.Bottom*multipli);
      data.FontBBox.Top:=Round(data.FontBBox.Top*multipli);
      // CapHeight is not really correct, where to get?
      data.CapHeight:=Round(potm^.otmAscent*multipli);
      data.StemV:=0;
      data.MaxWidth:=Round(potm^.otmTextMetrics.tmMaxCharWidth*multipli);
      data.AvgWidth:=Round(potm^.otmTextMetrics.tmAveCharWidth*multipli);

      data.Leading:=Round(potm^.otmTextMetrics.tmExternalLeading*multipli);
      apchar:=PChar(potm);
      data.FamilyName:=StrPas(@apchar[Integer(potm^.otmpFamilyName)]);
      data.FullName:=StrPas(@apchar[Integer(potm^.otmpFullName)]);
      data.StyleName:=StrPas(@apchar[Integer(potm^.otmpStyleName)]);
      data.FaceName:=StrPas(@apchar[Integer(potm^.otmpFaceName)]);
      data.ItalicAngle:=potm^.otmItalicAngle/10;
      if ((potm^.otmTextMetrics.tmPitchAndFamily AND TMPF_TRUETYPE)=0) then
       Raise Exception.Create(SRpNoTrueType+'-'+data.FaceName);
      data.postcriptname:=data.FamilyName;
{      if (fsBold in FBitmap.Canvas.Font.Style) then
      begin
       data.postcriptname:=data.postcriptname+',Bold';
       if (fsItalic in FBitmap.Canvas.Font.Style) then
        data.postcriptname:=data.postcriptname+',Italic';
      end
      else
      begin
       if (fsItalic in FBitmap.Canvas.Font.Style) then
        data.postcriptname:=data.postcriptname+',Italic';
      end;
}      data.postcriptname:=StringReplace(data.postcriptname,' ','',[rfReplaceAll]);
      //
      data.Flags:=32;
      // Fixed pitch? Doc says inverse meaning
      if ((potm^.otmTextMetrics.tmPitchAndFamily AND TMPF_FIXED_PITCH)=0) then
       data.Flags:=data.Flags+1;
      if GetObject(FontHandle,sizeof(alog),@alog)>0 then
      begin
       acomp:=(alog.lfPitchAndFamily AND $C0);
       if ((acomp or FF_SCRIPT)=alog.lfPitchAndFamily) then
        data.Flags:=data.Flags+8;
       if ((acomp or FF_ROMAN)=alog.lfPitchAndFamily) then
        data.Flags:=data.Flags+2;
      end;
      if potm^.otmTextMetrics.tmItalic<>0 then
       data.Flags:=data.Flags+64;
      data.FontStretch:='/Normal';
     end;
    finally
     FreeMem(potm);
    end;
   end;
   if embeddable then
   begin
    asize:=GetFontData(adc,0,0,nil,0);
    if asize>0 then
    begin
     // Gets the raw data of the font
     data.FontData.SetSize(asize);
     if GDI_ERROR=GetFontData(adc,0,0,data.FontData.Memory,asize) then
      RaiseLastOSError;
     data.FontData.Seek(0,soFromBeginning);
    end;
   end;
end;

end.
