{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpvgraphutils                                   }
{       Utilities for Windows GDI printer driver        }
{       can be used only for windows                    }
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

{$I rpconf.inc}

unit rpvgraphutils;

interface

uses Classes,SysUtils,Windows,Graphics,rpmunits,Printers,WinSpool,
 rpconsts;

type
 TGDIPageSize=record
  // -1 default, 0 user defined, 1 A4...
  PageIndex:integer;
  // Custom size in user defined type
  // 1000 units = 1 Inch
  Width:integer;
  Height:integer;
  papername:string;
 end;

 TPrinterForm=class(TObject)
   public
    Forminfo:Form_Info_1;
    name:string;
    sizex:double;
    sizey:double;
  end;

procedure DrawBitmap (Destination:TCanvas;Bitmap:TBitmap;Rec,RecSrc:TRect);
procedure DrawBitmapMosaic(canvas:TCanvas;rec:TRect;bitmap:TBitmap);
function CLXColorToVCLColor(CLXColor:integer):integer;
function CLXIntegerToFontStyle(intfontstyle:integer):TFontStyles;
procedure DrawBitmapMosaicSlow(canvas:TCanvas;rec:Trect;bitmap:TBitmap);
function GetPhysicPageSizeTwips:TPoint;
function GetPageSizeTwips:TPoint;
function GetPageMarginsTWIPS:TRect;
function QtPageSizeToGDIPageSize(qtsize:integer):TGDIPageSize;
function IsWindowsNT:Boolean;
function FindIndexPaperName(device,name:string):integer;
procedure SetCurrentPaper(apapersize:TGDIPageSize);
function GetCurrentPaper:TGDIPageSize;

var
 osinfo:TOsVersionInfo;
 formslist:TStringlist;


implementation

var
 obtainedversion:Boolean;
 FPrinters:TStringList;


{ Rutina que imprime un bitmap  }
{ Bitmap: el bitmap a imprimir }
{ REcDesti.Top,Left Situacino en coord.logicas }
procedure DrawBitmap (Destination:TCanvas;Bitmap:TBitmap;Rec,RecSrc:TRect);
var
  Info:PBitmapInfo;
  InfoSize:DWORD;
  Image:Pointer;
  ImageSize:DWORD;
begin
 With Bitmap do
 begin
  Graphics.GetDIBSizes(Handle,InfoSize,ImageSize);
  Info:=AllocMem(InfoSize);
  try
   Image:=AllocMem(Imagesize);
   try
    GetDIB(Handle,Palette,Info^,Image^);
    if not Monochrome then
     SetStretchBltMode(Destination.handle,STRETCH_DELETESCANS);
    with info^.bmiHeader do
     StretchDIBits(Destination.Handle,rec.Left,rec.Top,rec.Right-rec.Left+1,
        rec.Bottom-rec.Top+1,recsrc.Left,recsrc.Top,
         recsrc.Right-recsrc.Left+1,recsrc.Bottom-recsrc.Top+1,Image,Info^,
        DIB_RGB_COLORS,SRCCOPY);
   finally
    {$R-}
    Dispose(Image);
    {$R+}
   end;
  finally
   FreeMem(Info,InfoSize);
  end;
 end;
end;


procedure DrawBitmapMosaic(canvas:TCanvas;rec:Trect;bitmap:TBitmap);
var
 x,y:integer;
 aheight,awidth:integer;
 source,destination:Trect;
begin
 x:=rec.Left;
 y:=rec.Top;
 aheight:=bitmap.height;
 awidth:=bitmap.width;
 Canvas.Draw(x,y,bitmap);
 While ((y+aheight<rec.Bottom) or (x+awidth<rec.Right)) do
 begin
  // Copy the bitmap to three positions
  source.left:=x;
  source.top:=y;
  source.right:=awidth;
  source.bottom:=aheight;
  // Right
  destination.Left:=x+awidth;
  destination.top:=y;
  destination.right:=x+awidth+awidth;
  destination.bottom:=y+aheight;
  Canvas.CopyRect(destination,canvas,source);
  // Down
  destination.Left:=x;
  destination.top:=y+aheight;
  destination.right:=x+awidth;
  destination.bottom:=y+aheight+aheight;
  Canvas.CopyRect(destination,canvas,source);
  // Down-Right
  destination.Left:=x+awidth;
  destination.top:=y+aheight;
  destination.right:=x+awidth+awidth;
  destination.bottom:=y+aheight+aheight;
  Canvas.CopyRect(destination,canvas,source);

  // NextStep
  aheight:=aheight*2;
  awidth:=awidth*2;
 end;
end;


function CLXIntegerToFontStyle(intfontstyle:integer):TFontStyles;
begin
 Result:=[];
 if (intfontstyle and 1)>0 then
  include(Result,fsBold);
 if (intfontstyle and (1 shl 1))>0 then
  include(Result,fsItalic);
 if (intfontstyle and (1 shl 2))>0 then
  include(Result,fsUnderline);
 if (intfontstyle and (1 shl 3))>0 then
  include(Result,fsStrikeOut);
end;


function CLXColorToVCLColor(CLXColor:integer):integer;
begin
 Result:=CLXColor AND $00FFFFFF;
end;

procedure DrawBitmapMosaicSlow(canvas:TCanvas;rec:Trect;bitmap:TBitmap);
var
 x,y:integer;
 arec,recsrc:TRect;
begin
 recsrc.Left:=0;
 recsrc.Top:=0;
 recsrc.Right:=Bitmap.Width-1;
 recsrc.Bottom:=Bitmap.Height-1;
 x:=rec.Left;
 y:=rec.Top;
 While y<rec.Bottom do
 begin
  While x<rec.right do
  begin
   arec.Left:=x;
   arec.Top:=y;
   arec.Bottom:=y+bitmap.Height-1;
   arec.Right:=x+bitmap.Width-1;
   DrawBitmap(Canvas,Bitmap,arec,recsrc);
   x:=x+bitmap.width;
  end;
  x:=rec.left;
  y:=y+bitmap.Height;
 end;
end;

function GetPageMarginsTWIPS:TRect;
var rec:TRect;
    DC:HDC;
    pagesize:TPoint;
    physical:TPoint;
    offset:TPoint;
    dpix,dpiy:integer;
    apagewidth,apageheight:integer;
begin
 if Printer.printers.count<1 then
 begin
  result.Left:=0;
  result.Top:=0;
  result.Bottom:=16637;
  result.Right:=12047;
  exit;
 end;
 DC:=Printer.handle;

 dpix:=GetDeviceCaps(DC,LOGPIXELSX); //  printer.XDPI;
 dpiy:=GetDeviceCaps(DC,LOGPIXELSY);  // printer.YDPI;

 apagewidth:=GetDeviceCaps(DC,HORZRES);
 apageheight:=GetDeviceCaps(DC,VERTRES);
 pagesize.x:=Round(apagewidth/dpix*TWIPS_PER_INCHESS);
 pagesize.y:=Round(apageheight/dpix*TWIPS_PER_INCHESS);

 physical.x:=GetDeviceCaps(DC,PHYSICALWIDTH);
 physical.y:=GetDeviceCaps(DC,PHYSICALHEIGHT);
 // Transform to twips
 physical.X:=Round(physical.X/dpix*TWIPS_PER_INCHESS);
 physical.Y:=Round(physical.Y/dpiy*TWIPS_PER_INCHESS);

 // Gets top/left offser
 offset.x:=GetDeviceCaps(DC,PHYSICALOFFSETX);
 offset.y:=GetDeviceCaps(DC,PHYSICALOFFSETY);
 // Transform to twips
 offset.X:=Round(offset.X/dpix*TWIPS_PER_INCHESS);
 offset.Y:=Round(offset.Y/dpiy*TWIPS_PER_INCHESS);

 rec.Left:=offset.X;
 rec.Top:=offset.Y;
 rec.Right:=physical.X-(physical.X-pagesize.X-offset.X);
 rec.Bottom:=physical.Y-(physical.Y-pagesize.Y-offset.Y);

 Result:=rec;
end;


function GetPageSizeTwips:TPoint;
var
 DC:HDC;
 dpix,dpiy:integer;
 apagewidth,apageheight:integer;
begin
 if Printer.printers.count<1 then
 begin
  result.y:=16637;
  result.x:=12047;
  exit;
 end;
 DC:=Printer.handle;
 // Get the device units of
 dpix:=GetDeviceCaps(DC,LOGPIXELSX); //  printer.XDPI;
 dpiy:=GetDeviceCaps(DC,LOGPIXELSY);  // printer.YDPI;

 apagewidth:=GetDeviceCaps(DC,HORZRES);
 apageheight:=GetDeviceCaps(DC,VERTRES);
 Result.x:=Round(apagewidth/dpix*TWIPS_PER_INCHESS);
 Result.y:=Round(apageheight/dpiy*TWIPS_PER_INCHESS);
end;

function GetPhysicPageSizeTwips:TPoint;
var
 DC:HDC;
 dpix,dpiy:integer;
begin
 if Printer.printers.count<1 then
 begin
  result.y:=16637;
  result.x:=12047;
  exit;
 end;
 DC:=Printer.handle;
 // Get the device units of
 dpix:=GetDeviceCaps(DC,LOGPIXELSX); //  printer.XDPI;
 dpiy:=GetDeviceCaps(DC,LOGPIXELSY);  // printer.YDPI;

 Result.x:=GetDeviceCaps(DC,PHYSICALWIDTH);
 Result.y:=GetDeviceCaps(DC,PHYSICALHEIGHT);
 // Transform to twips
 Result.X:=Round(Result.X/dpix*TWIPS_PER_INCHESS);
 Result.Y:=Round(Result.Y/dpiy*TWIPS_PER_INCHESS);
end;


{    (
      (Width: 8268; Height: 11693),  // psA4
      (Width: 7165; Height: 10118),  // psB5
      (Width: 8500; Height: 11000),  // psLetter
      (Width: 8500; Height: 14000),  // psLegal
      (Width: 7500; Height: 10000),  // psExecutive
      (Width: 33110; Height: 46811), // psA0
      (Width: 23386; Height: 33110), // psA1
      (Width: 16535; Height: 23386), // psA2
      (Width: 11693; Height: 16535), // psA3
      (Width: 5827; Height: 8268),   // psA5
      (Width: 4134; Height: 5827),   // psA6
      (Width: 2913; Height: 4134),   // psA7
      (Width: 2047; Height: 2913),   // psA8
      (Width: 1457; Height: 2047),   // psA9
      (Width: 40551; Height: 57323), // psB0
      (Width: 28661; Height: 40551), // psB1
      (Width: 1260; Height: 1772),   // psB10
      (Width: 20276; Height: 28661), // psB2
      (Width: 14331; Height: 20276), // psB3
      (Width: 10118; Height: 14331), // psB4
      (Width: 5039; Height: 7165),   // psB6
      (Width: 3583; Height: 5039),   // psB7
      (Width: 2520; Height: 3583),   // psB8
      (Width: 1772; Height: 2520),   // psB9
      (Width: 6417; Height: 9016),   // psC5E
      (Width: 4125; Height: 9500),   // psComm10E
      (Width: 4331; Height: 8661),   // psDLE
      (Width: 8250; Height: 13000),  // psFolio
      (Width: 17000; Height: 11000), // psLedger
      (Width: 11000; Height: 17000), // psTabloid
      (Width: -1; Height: -1)        // psNPageSize
    );
}

function QtPageSizeToGDIPageSize(qtsize:integer):TGDIPageSize;
begin
 case qtsize of
  0:
   Result.PageIndex:=DMPAPER_A4;
  1:
   Result.PageIndex:=DMPAPER_B5;
  2:
   Result.PageIndex:=DMPAPER_LETTER;
  3:
   Result.PageIndex:=DMPAPER_LEGAL;
  4:
   Result.PageIndex:=DMPAPER_EXECUTIVE;
  5: // A0
   begin
    Result.PageIndex:=0;
    Result.Width:= 33110;
    Result.Height:= 46811;
   end;
  6: // A1
   begin
    Result.PageIndex:=0;
    Result.Width:= 23386;
    Result.Height:= 33110;
   end;
  7:
   Result.PageIndex:=DMPAPER_A2;
  8:
   Result.PageIndex:=DMPAPER_A3;
  9:
   Result.PageIndex:=DMPAPER_A5;
  10:
   Result.PageIndex:=DMPAPER_A6;
  11: // A7
   begin
    Result.PageIndex:=0;
    Result.Width:= 2913;
    Result.Height:= 4134;
   end;
  12: // A8
   begin
    Result.PageIndex:=0;
    Result.Width:= 2047;
    Result.Height:= 2913;
   end;
  13: // A9
   begin
    Result.PageIndex:=0;
    Result.Width:= 1457;
    Result.Height:= 2047;
   end;
  14: // B0
   begin
    Result.PageIndex:=0;
    Result.Width:= 40551;
    Result.Height:= 57323;
   end;
  15: // B1
   begin
    Result.PageIndex:=0;
    Result.Width:= 28661;
    Result.Height:= 40551;
   end;
  16: // B10
   begin
    Result.PageIndex:=0;
    Result.Width:= 1260;
    Result.Height:= 1772;
   end;
  17: // B2
   begin
    Result.PageIndex:=0;
    Result.Width:= 20276;
    Result.Height:= 28661;
   end;
  18: // B3
   begin
    Result.PageIndex:=0;
    Result.Width:= 14331;
    Result.Height:= 20276;
   end;
  19:
   Result.PageIndex:=DMPAPER_B4;
  20: // B6
   begin
    Result.PageIndex:=0;
    Result.Width:= 5039;
    Result.Height:= 7165;
   end;
  21: // B7
   begin
    Result.PageIndex:=0;
    Result.Width:= 3583;
    Result.Height:= 5039;
   end;
  22: // B8
   begin
    Result.PageIndex:=0;
    Result.Width:= 2520;
    Result.Height:= 3583;
   end;
  23: // B9
   begin
    Result.PageIndex:=0;
    Result.Width:= 1772;
    Result.Height:= 2520;
   end;
  24:
   Result.PageIndex:=DMPAPER_ENV_C5;
  25:
   Result.PageIndex:=DMPAPER_ENV_10;
  26:
   Result.PageIndex:=DMPAPER_ENV_DL;
  27:
   Result.PageIndex:=DMPAPER_FOLIO;
  28:
   Result.PageIndex:=DMPAPER_LEDGER;
  29:
   Result.PageIndex:=DMPAPER_TABLOID;
  else
   Result.PageIndex:=DMPAPER_A4;
 end;
 if Result.PageIndex=0 then
 begin
  // Converts to decs of milimeter
  Result.Width:=Round(Result.Width*1000/251);
  Result.Height:=Round(Result.Height*1000/251);
 end
 else
 begin
  Result.Width:=0;
  Result.Height:=0;
 end;
end;




// Gets current paper page size
function GetCurrentPaper:TGDIPageSize;
var
  DeviceMode: THandle;
  PDevMode :  ^TDeviceMode;
  Device, Driver, Port: array[0..1023] of char;
begin
 if printer.printers.count<1 then
 begin
  Result.PageIndex:=DMPAPER_A4;
  Result.Width:=0;
  Result.Height:=0;
  exit;
 end;
 Printer.GetPrinter(Device, Driver, Port, DeviceMode);
 PDevMode := GlobalLock(DeviceMode);
 // Warning the custom page size does not work in all drivers
 // especially in Windows NT drivers
 if PDevMode.dmPapersize=256 then
 begin
  Result.PageIndex:= 0;     { User defined (custom page size) }
  Result.Height:=PDevMode.dmPaperlength;
  Result.Width:=PDevMode.dmPaperwidth;
  Result.papername:=PDevmode.dmFormName;
 end
 else
 begin
  REsult.PageIndex:=PDevMode.dmPaperSize;
  Result.Height:=0;
  Result.Width:=0;
  Result.papername:=PDevmode.dmFormName;
 end;
 GlobalUnLock(DeviceMode);
// Printer.SetPrinter(Device, Driver, Port, DeviceMode);
end;


function IsWindowsNT:Boolean;
begin
 if Not obtainedversion then
 begin
   osinfo.dwOSVersionInfoSize:=sizeof(osinfo);
  if Not GetVersionEx(osinfo) then
   Raise Exception.Create(SRpError+' GetVersionEx');
  obtainedversion:=True;
 end;
 Result:=osinfo.dwPlatformId=VER_PLATFORM_WIN32_NT;
end;


function GetPrinters: TStrings;
var
  Buffer, PrinterInfo: PChar;
  Flags, Count, NumInfo: DWORD;
  I: Integer;
  Level: Byte;
begin
  if FPrinters = nil then
  begin
    FPrinters := TStringList.Create;
    Result := FPrinters;
    try
      if Win32Platform = VER_PLATFORM_WIN32_NT then
      begin
        Flags := PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL;
        Level := 4;
      end
      else
      begin
        Flags := PRINTER_ENUM_LOCAL;
        Level := 5;
      end;
      Count := 0;
      EnumPrinters(Flags, nil, Level, nil, 0, Count, NumInfo);
      if Count = 0 then Exit;
      GetMem(Buffer, Count);
      try
        if not EnumPrinters(Flags, nil, Level, PByte(Buffer), Count, Count, NumInfo) then
          Exit;
        PrinterInfo := Buffer;
        for I := 0 to NumInfo - 1 do
        begin
          if Level = 4 then
            with PPrinterInfo4(PrinterInfo)^ do
            begin
              FPrinters.Add(pPrinterName);
              Inc(PrinterInfo, sizeof(TPrinterInfo4));
            end
          else
            with PPrinterInfo5(PrinterInfo)^ do
            begin
              FPrinters.Add(pPrinterName);
              Inc(PrinterInfo, sizeof(TPrinterInfo5));
            end;
        end;
      finally
        FreeMem(Buffer, Count);
      end;
    except
      FPrinters.Free;
      FPrinters := nil;
      raise;
    end;
  end;
  Result := FPrinters;
end;


procedure FillFormsList;
var
 pforms,p:^Form_info_1;
 buf:Pchar;
 fprinterhandle:THandle;
 needed,received:dword;
 forminfo:Form_info_1;
 i:integer;
 indexprint:integer;
 llistaf:TStringList;
 cadenaimp:string;
 formobject:TPrinterForm;
begin
 FormsList.clear;
 for indexprint:=0 to printer.Printers.count-1 do
 begin
  cadenaimp:=GetPrinters[indexprint];
  buf:=Pchar(cadenaimp);
  if Not OpenPrinter(buf,fprinterhandle,nil) then
    Raise Exception.create(SRpError+Strpas(buf));
  try
   // Creeem un objecte de llista de forms
   llistaf:=TStringList.Create;
   FormsList.AddObject(cadenaimp,llistaf);
   pforms:=nil;
   if Not EnumForms(fprinterhandle,1,pforms,0,needed,received) then
    if GetLastError<>122 then
{$IFDEF USEVARIANTS}
     RaiseLastOSError;
{$ELSE}
     RaiseLastWin32Error;
{$ENDIF}
   pforms:=Allocmem(needed);
   try
    if NOt EnumForms(fprinterhandle,1,pforms,needed,needed,received) then
{$IFDEF USEVARIANTS}
     RaiseLastOSError;
{$ELSE}
     RaiseLastWin32Error;
{$ENDIF}
    for i:=0 to received-1 do
    begin
     p:=Pointer(integer(pforms)+sizeof(Form_info_1)*i);
     forminfo:=p^;
     formobject:=TPrinterForm.Create;
     formobject.Forminfo:=forminfo;
     formobject.name:=StrPas(forminfo.pName);
     llistaf.AddObject(formobject.name,formobject);
    end;
   finally
    Freemem(pforms);
   end;
  finally
   ClosePrinter(fprinterhandle);
  end;
 end;
end;


procedure SetCurrentPaper(apapersize:TGDIPageSize);
var
  DeviceMode: THandle;
  PDevMode :  ^TDeviceMode;
  printername,apapername:string;
  FPrinterHandle:THandle;
  pforminfo:^Form_info_1;
  foundpaper:boolean;
  needed:DWord;
  indexpaper:integer;

  Device, Driver, Port: array[0..1023] of char;
  laste:integer;
begin
 if printer.Printers.count<1 then
  exit;
 Printer.GetPrinter(Device, Driver, Port, DeviceMode);
 PDevMode := GlobalLock(DeviceMode);
 try
  // Custom page size, warning not all drivers supports it
  // especially Windows NT drivers
  // In Windows NT only Administrator has rights to add a
  // custom paper once added it's stored until the print driver
  // is removed
  if apapersize.PageIndex=0 then
  begin
   if Not IsWIndowsNT then
   begin
    // If is not Windows NT select custom paper
    PDevMode.dmPaperSize := 256;
    PDevMode.dmPaperlength := apapersize.Height;
    PDevMode.dmPaperwidth  := apapersize.Width;
   end
   else
   begin
    // In Windows NT we must search or create a form
    foundpaper:=false;
    // Busquem un form que s'adapti
    apapername:='User ('+
    IntToStr(apapersize.Width)+'x'+
    IntToStr(apapersize.Height)+')';

    PrinterName := Format('%s', [Device]);
    if not OpenPrinter(PChar(PrinterName), FPrinterHandle, nil) then
{$IFDEF USEVARIANTS}
     RaiseLastOSError;
{$ELSE}
     RaiseLastWin32Error;
{$ENDIF}
    try
     pforminfo:=allocmem(sizeof(form_info_1));
     try
      if Not GetForm(FPrinterhandle,Pchar(apapername),1,pforminfo,sizeof(Form_info_1),needed) then
      begin
       laste:=GetLasterror;
       if ((laste<>122) AND (Laste<>123) AND (laste<>1902)) then
{$IFDEF USEVARIANTS}
        RaiseLastOSError
{$ELSE}
        RaiseLastWin32Error
{$ENDIF}
       else
       begin
        if laste<>1902 then
        begin
         if needed>0 then
         begin
          freemem(pforminfo);
          pforminfo:=AllocMem(needed);
          if Not GetForm(FPrinterhandle,Pchar(apapername),1,pforminfo,needed,needed) then
{$IFDEF USEVARIANTS}
           RaiseLastOSError;
{$ELSE}
           RaiseLastWin32Error;
{$ENDIF}
         if pforminfo^.pname<>nil then
          foundpaper:=true;
         // Si l'ha trobat trobem el seu index
         end;
        end;
       end;
      end;
      if Not foundpaper then
      begin
       pforminfo^.pname:=Pchar(apapername);
       pforminfo^.Flags:=FORM_USER;
       pforminfo^.Size.cx:=apapersize.Width*100;
       pforminfo^.size.cy:=apapersize.Height*100;
       pforminfo^.ImageableArea.Top:=0;
       pforminfo^.ImageableArea.left:=0;
       pforminfo^.ImageableArea.Right:=pforminfo^.Size.cx;
       pforminfo^.ImageableArea.Bottom:=pforminfo^.size.cy;
       if not AddForm(fprinterhandle,1,pforminfo) then
{$IFDEF USEVARIANTS}
        RaiseLastOSError;
{$ELSE}
        RaiseLastWin32Error;
{$ENDIF}
       // Updates the form list
       FillFormsList;
      end;
      FillFormsList;
      indexpaper:=FindIndexPaperName(printername,apapername);
      if indexpaper=0 then
      begin
       FillFormsList;
       indexpaper:=FindIndexPaperName(printername,apapername);
       if indexpaper=0 then
        Raise Exception.Create(SRpPaperNotFount+':'+apapername);
      end;
      // Seleccionem per fi
      PDevMode.dmPaperSize :=indexpaper;
      PDevMode.dmPaperlength := apapersize.height;
      PDevMode.dmPaperwidth  := apapersize.Width;
      // It can also selected by name
//      StrPCopy(PDevMode.dmFormName,apapername);
//      PDevMode.dmFields:=PDevMode.dmFields or dm_formname;
//      PDevMode.dmFields:=PDevMode.dmFields AND (NOT dm_papersize);
     finally
      freemem(pforminfo);
     end;
    finally
     ClosePrinter(FPrinterhandle);
    end;
   end;
  end
  else
  begin
   PDevMode.dmPaperSize :=apapersize.PageIndex;
   PDevMode.dmPaperlength := apapersize.Width;
   PDevMode.dmPaperwidth  := apapersize.Height;
  end;
{  PDevMode.dmPrintQuality:=SmallInt(DMRES_DRAFT);
  PDevMode.dmFields:=PDevMode.dmFields or dm_PrintQuality;
  if (PDevMode.dmFields AND dm_PrintQuality)>0 then
   ShowMessage('CorrectPrint');
  PDevMode.dmFields:=PDevMode.dmFields AND (Not dm_YResolution);
  if (PDevMode.dmFields AND dm_YResolution)>0 then
   ShowMessage('ErrorRes');
} finally
  GlobalUnLock(DeviceMode);
 end;
 Printer.SetPrinter(Device, Driver, Port, DeviceMode);
end;


function FindIndexPaperName(device,name:string):integer;
var
 index:integer;
 llista:TStringList;
begin
 Result:=0;
 index:=formslist.indexof(device);
 if index<0 then
  exit;
 llista:=TStringList(formslist.objects[index]);
 index:=llista.indexof(name);
 if index<0 then
  exit;
 result:=index+1;
end;



procedure FreeFormsList;
var
 i:integer;
 j:integer;
 llistaf:TStringlist;
begin
 for i:=0 to FormsList.count-1 do
 begin
  llistaf:=TStringList(FormsList.objects[i]);
  for j:=0 to llistaf.count-1 do
  begin
   llistaf.objects[j].free;
  end;
  llistaf.free;
 end;
 FormsList.clear;
end;

initialization

obtainedversion:=false;
formslist:=TStringList.Create;
if IsWindowsNT then
 FillFormsList;
FPrinters:=nil;

finalization

if IsWindowsNT then
 FreeFormsList;
FormsList.Free;



end.
