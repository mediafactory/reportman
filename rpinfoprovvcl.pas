{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       TRpInfoProvider VCL                             }
{       Provides information about fonts and bitmaps    }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{                                                       }
{*******************************************************}

unit rpinfoprovvcl;


interface

uses Classes,SysUtils,Windows,Graphics,rpinfoprovid,
    rpgraphutilsvcl,rpmdconsts;


type
 TRpVCLInfoProvider=class(TInterfacedObject,IRpInfoProvider)
  FBitmap:TBitmap;
  procedure FillFontInfo(pdffont:TRpPDFFont;info:TRpTTFontInfo);
  procedure FillFontData(pdffont:TRpPDFFont;data:TRpTTFontData);
  constructor Create;
  destructor destroy;override;
 end;

implementation

constructor TRpVCLInfoProvider.Create;
begin
 FBitmap:=TBitmap.Create;
 FBitmap.Width:=100;
 FBitmap.Height:=100;
end;

destructor TRpVCLInfoProvider.destroy;
begin
 FBitmap.free;

 inherited destroy;
end;


procedure TRpVCLInfoProvider.FillFontInfo(pdffont:TRpPDFFont;info:TRpTTFontInfo);
var
 logx,i:integer;
 adc:hdc;
 aabc:array [32..255] of ABC;
begin
 FBitmap.Canvas.Font.Name:=pdfFont.WFontName;
 FBitmap.Canvas.Font.Size:=1000;
 FBitmap.Canvas.Font.Style:=CLXIntegerToFontStyle(pdfFont.Style);
 adc:=FBitmap.Canvas.Handle;
 logx:=GetDeviceCaps(adc,LOGPIXELSX);
 if not GetCharABCWidths(adc,32,255,aabc[32]) then
  RaiseLastOSError;
 for i:=32 to 255 do
  info.charwidths[i]:=Round(
   (Integer(aabc[i].abcA)+Integer(aabc[i].abcB)+Integer(aabc[i].abcC))/logx*72000/1000
   );
end;

procedure TRpVCLInfoProvider.FillFontData(pdffont:TRpPDFFont;data:TRpTTFontData);
var
 potm:POUTLINETEXTMETRIC;
 asize:integer;
 adc:hdc;
 embeddable:boolean;
 logx:integer;
 multipli:double;
 apchar:PChar;
 alog:LOGFONT;
 acomp:byte;
begin
   // See if data can be embedded
   embeddable:=false;
   FBitmap.Canvas.Font.Name:=pdfFont.WFontName;
   FBitmap.Canvas.Font.Size:=1000;
   FBitmap.Canvas.Font.Style:=CLXIntegerToFontStyle(pdfFont.Style);
   adc:=FBitmap.Canvas.Handle;
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
      multipli:=1/logx*72000/1000;
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
      if GetObject(FBitmap.Canvas.Font.Handle,sizeof(alog),@alog)>0 then
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
