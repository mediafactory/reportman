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
 rpmdconsts,rptypes,Forms,
{$IFNDEF DOTNETD}
 jpeg,
{$ENDIF}
{$IFDEF DOTNETD}
 System.Runtime.InteropServices,
{$ENDIF}
 ComCtrls;

type
 TRpNodeInfo=class(TObject)
  public
   Node:TTreeNode;
   ReportName:WideString;
   Group_Code:Integer;
   Parent_Group:integer;
   Path:String;
  end;

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

procedure DrawBitmap (Destination:TCanvas; Bitmap:TBitmap; Rec,RecSrc:TRect);
procedure DrawBitmapMosaic (canvas:TCanvas; rec:TRect; bitmap:TBitmap);
procedure DrawBitmapMosaicSlow (canvas:TCanvas; rec:Trect; bitmap:TBitmap);
function GetPhysicPageSizeTwips:TPoint;
function GetPageSizeTwips:TPoint;
function GetPageMarginsTWIPS:TRect;
function QtPageSizeToGDIPageSize (qtsize:TPageSizeQt):TGDIPageSize;
function GDIPageSizeToQtPageSize (gdisize:TGDIPageSize):TPageSizeQt;
//function FindIndexPaperName (device, name:string):integer;
procedure SetCurrentPaper (apapersize:TGDIPageSize);
procedure SetPrinterCopies(copies:integer);
procedure SetPrinterCollation(collation:boolean);
function GetPrinterCopies:Integer;
function GetPrinterCollation:Boolean;
function PrinterSupportsCollation:Boolean;
function PrinterSupportsCopies(copies:integer):Boolean;
function GetCurrentPaper:TGDIPageSize;
procedure SendControlCodeToPrinter (S: string);
{$IFNDEF DOTNETD}
procedure JPegStreamToBitmapStream(AStream:TMemoryStream);
{$ENDIF}
function FindFormNameFromSize(width,height:integer):String;
function PrinterMaxCopiesSupport:Integer;
procedure FillTreeView (ATree:TTreeView;alist:TStringList);
function GetFullFileName(ANode:TTreeNode;dirseparator:char):String;


implementation



var
 FPrinters:TStringList;

 {$IFNDEF DOTNETD}
 procedure JPegStreamToBitmapStream(AStream:TMemoryStream);
var
 jpegimage:TJPegImage;
 bitmap:TBitmap;
begin
 bitmap:=TBitmap.Create;
 try
  AStream.Seek(0,soFromBeginning);
  jpegimage:=TJPegImage.Create;
  try
   jpegimage.LoadFromStream(AStream);
   bitmap.Assign(jpegimage);
   AStream.Clear;
   bitmap.SaveToStream(AStream);
   AStream.Seek(0,soFromBeginning);
  finally
   jpegimage.Free;
  end;
 finally
  bitmap.Free;
 end;
end;
{$ENDIF}

// Bitmap print routine
{$IFNDEF DOTNETD}
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
{$ENDIF}

// Bitmap print routine
{$IFDEF DOTNETD}
procedure DrawBitmap (Destination:TCanvas;Bitmap:TBitmap;Rec,RecSrc:TRect);
var
 abitmap:TBitmap;
 arec:TRect;
begin
 abitmap:=TBitmap.Create;
 abitmap.Width:=RecSrc.Right-RecSrc.Left;
 abitmap.Height:=RecSrc.Bottom-RecSrc.Top;
 arec.Top:=0;
 arec.Left:=0;
 arec.Bottom:=abitmap.Height-1;
 arec.Right:=abitmap.Width-1;
 abitmap.Canvas.CopyRect(arec,bitmap.canvas,recsrc);
 Destination.StretchDraw(rec,abitmap);
end;
{$ENDIF}


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
    printererror:boolean;
begin
 if Printer.printers.count<1 then
 begin
  result.Left:=0;
  result.Top:=0;
  result.Bottom:=16637;
  result.Right:=12047;
  exit;
 end;
 DC:=0;
 // Printer selected not valid error
 printererror:=false;
 try
  DC:=Printer.handle;
 except
  printererror:=true;
 end;
 if printererror then
 begin
  result.Left:=0;
  result.Top:=0;
  result.Bottom:=16637;
  result.Right:=12047;
  exit;
 end;

 dpix:=GetDeviceCaps(DC,LOGPIXELSX); //  printer.XDPI;
 dpiy:=GetDeviceCaps(DC,LOGPIXELSY);  // printer.YDPI;

 apagewidth:=GetDeviceCaps(DC,HORZRES);
 apageheight:=GetDeviceCaps(DC,VERTRES);
 pagesize.x:=Round(apagewidth/dpix*TWIPS_PER_INCHESS);
 pagesize.y:=Round(apageheight/dpiy*TWIPS_PER_INCHESS);

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
 printererror:boolean;
begin
 if Printer.printers.count<1 then
 begin
  result.y:=16637;
  result.x:=12047;
  exit;
 end;
 DC:=0;
 // Printer selected not valid error
 printererror:=false;
 try
  DC:=Printer.handle;
 except
  printererror:=true;
 end;
 if printererror then
 begin
  result.y:=16637;
  result.x:=12047;
  exit;
 end;
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
 printererror:boolean;
begin
 if Printer.printers.count<1 then
 begin
  result.y:=16637;
  result.x:=12047;
  exit;
 end;
 // Printer selected not valid error
 DC:=0;
 printererror:=false;
 try
  DC:=Printer.handle;
 except
  printererror:=true;
 end;
 if printererror then
 begin
  result.y:=16637;
  result.x:=12047;
  exit;
 end;
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

function GDIPageSizeToQtPageSize (gdisize:TGDIPageSize):TPageSizeQt;
begin
 Result.Custom:=False;
 case gdisize.PageIndex of
  DMPAPER_A4:
   Result.Indexqt:=0;
  DMPAPER_B5:
   Result.Indexqt:=1;
  DMPAPER_LETTER,2:
   Result.Indexqt:=2;
  DMPAPER_LEGAL:
   Result.Indexqt:=3;
  DMPAPER_EXECUTIVE:
   Result.Indexqt:=4;
  DMPAPER_A2:
   Result.Indexqt:=7;
  DMPAPER_A3:
   Result.Indexqt:=8;
  DMPAPER_A5:
   Result.Indexqt:=9;
  DMPAPER_A6:
   Result.Indexqt:=10;
  DMPAPER_B4:
   Result.Indexqt:=19;
  DMPAPER_ENV_C5:
   Result.Indexqt:=24;
  DMPAPER_ENV_10:
   Result.Indexqt:=25;
  DMPAPER_ENV_DL:
   Result.Indexqt:=26;
  DMPAPER_FOLIO:
   Result.Indexqt:=27;
  DMPAPER_LEDGER:
   Result.Indexqt:=28;
  DMPAPER_TABLOID:
   Result.Indexqt:=29;
  else
  begin
   if gdisize.PageIndex>=0 then
   begin
    Result.Custom:=True;
    // Converts to twips
    Result.Indexqt:=0;
    Result.CustomWidth:=Round(gdisize.Width/100/CMS_PER_INCHESS*TWIPS_PER_INCHESS);
    Result.CustomHeight:=Round(gdisize.Height/100/CMS_PER_INCHESS*TWIPS_PER_INCHESS);
   end
   else
   begin
    Result.indexqt:=0;
    Result.Custom:=False;
   end;
  end;
 end;
end;

function QtPageSizeToGDIPageSize(qtsize:TPageSizeQt):TGDIPageSize;
begin
 if qtsize.Custom then
 begin
  Result.PageIndex:=0;
  // Converts to decs of milimeter
  Result.Width:=Round(qtsize.CustomWidth/TWIPS_PER_INCHESS*CMS_PER_INCHESS*100);
  Result.Height:=Round(qtsize.CustomHeight/TWIPS_PER_INCHESS*CMS_PER_INCHESS*100);
 end
 else
 begin
  case qtsize.indexqt of
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
   Result.Width:=Round(Result.Width*1000/2540);
   Result.Height:=Round(Result.Height*1000/2540);
  end
  else
  begin
   Result.Width:=0;
   Result.Height:=0;
  end;
 end;
end;




// Gets current paper page size
function GetCurrentPaper:TGDIPageSize;
var
{$IFNDEF DOTNETD}
  DeviceMode: THandle;
  PDevMode :  ^TDeviceMode;
  Device, Driver, Port: array[0..1023] of char;
{$ENDIF}
{$IFDEF DOTNETD}
  DeviceMode: IntPtr;
  PDevMode :  TDeviceMode;
  Device, Driver, Port:String;
{$ENDIF}
  printererror:boolean;
begin
 if printer.printers.count<1 then
 begin
  Result.PageIndex:=DMPAPER_A4;
  Result.Width:=0;
  Result.Height:=0;
  exit;
 end;
 // Printer selected not valid error
 printererror:=false;
 try
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
 except
  printererror:=true;
 end;
 if printererror then
 begin
  Result.PageIndex:=DMPAPER_A4;
  Result.Width:=0;
  Result.Height:=0;
  exit;
 end;
{$IFNDEF DOTNETD}
 PDevMode := GlobalLock(DeviceMode);
{$ENDIF}
{$IFDEF DOTNETD}
 PDevMode := TDeviceMode(Marshal.PtrToStructure(GlobalLock(Integer(DeviceMode)),TypeOf(TDeviceMode)));
{$ENDIF}
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
{$IFNDEF DOTNETD}
 GlobalUnLock(DeviceMode);
{$ENDIF}
{$IFDEF DOTNETD}
 GlobalUnLock(Integer(DeviceMode));
{$ENDIF}
 Printer.SetPrinter(Device, Driver, Port, DeviceMode);
end;




{$IFNDEF DOTNETD}
procedure SendControlCodeToPrinter(S: string);
var
 Handle, hDeviceMode: THandle;
 N: DWORD;
 DocInfo1: TDocInfo1;
 Device, Driver, Port: array[0..255] of char;
 PrinterName: string;
 buf:pchar;
 lbuf:integer;
begin
 Printer.GetPrinter(Device, Driver, Port, hDeviceMode);
 PrinterName := Format('%s', [Device]);
 if not OpenPrinter(PChar(PrinterName), Handle, nil) then
   RaiseLastOSError;
 try
  with DocInfo1 do
  begin
   pDocName := 'Control';
   pOutputFile := nil;
   pDataType := 'RAW';
  end;
  StartDocPrinter(Handle, 1, @DocInfo1);
  try
//   StartPagePrinter(Handle);
   lbuf:=length(s);
   buf:=Allocmem(lbuf+2);
   try
    copymemory(buf,Pchar(s),lbuf);
    if not WritePrinter(Handle, buf, lbuf, N) then
     RaiseLastOSError;
   finally
    freemem(buf);
   end;
//   EndPagePrinter(Handle);
  finally
   EndDocPrinter(Handle);
  end;
 finally
  ClosePrinter(Handle);
 end;
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

function FindFormNameFromSize(width,height:integer):String;
var
 pforms,p:^Form_info_1;
 needed,received:dword;
 fprinterhandle:THandle;
 buf:Pchar;
 i:integer;
 cadenaimp:String;
 forminfo:Form_info_1;
begin
 cadenaimp:=GetPrinters[Printer.PrinterIndex];
 buf:=Pchar(cadenaimp);
 Result:='';
 if Not OpenPrinter(buf,fprinterhandle,nil) then
   Raise Exception.create(SRpError+Strpas(buf));
 try
  pforms:=nil;
  if Not EnumForms(fprinterhandle,1,pforms,0,needed,received) then
    if GetLastError<>122 then
     RaiseLastOSError;
  pforms:=Allocmem(needed);
  try
   if NOt EnumForms(fprinterhandle,1,pforms,needed,needed,received) then
    RaiseLastOSError;
   for i:=0 to received-1 do
   begin
    p:=Pointer(integer(pforms)+sizeof(Form_info_1)*i);
    forminfo:=p^;
    if forminfo.Size.cx=width*100 then
     if forminfo.Size.cy=height*100 then
     begin
      Result:=StrPas(forminfo.pName);
     end;
   end;
  finally
   Freemem(pforms);
  end;
 finally
  ClosePrinter(fprinterhandle);
 end;
end;

procedure SetCurrentPaper(apapersize:TGDIPageSize);
var
  Device, Driver, Port: array[0..1023] of char;
  DeviceMode: THandle;
  PDevmode:^TDevicemode;
  pforminfo:^Form_info_1;
  printername,apapername:string;
  FPrinterHandle:THandle;
  foundpaper:boolean;
  needed:DWord;
  printererror:boolean;
  laste:integer;
begin
 if printer.Printers.count<1 then
  exit;
 // Printer selected not valid error
 printererror:=false;
 try
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
 except
  printererror:=true;
 end;
 if printererror then
  exit;
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
    foundpaper:=false;
    // In Windows NT we must search or create a form
    apapername:=FindFormNameFromSize(apapersize.Width,apapersize.Height);
    if Length(apapername)>0 then
     foundpaper:=true;
    if not foundpaper then
    begin
     // Busquem un form que s'adapti
     apapername:='User ('+
     IntToStr(apapersize.Width)+'x'+
     IntToStr(apapersize.Height)+')';
    end;
    PrinterName := Format('%s', [Device]);
    if not OpenPrinter(PChar(PrinterName), FPrinterHandle, nil) then
     RaiseLastOSError;
    try
     pforminfo:=allocmem(sizeof(form_info_1));
     try
      if Not GetForm(FPrinterhandle,Pchar(apapername),1,pforminfo,sizeof(Form_info_1),needed) then
      begin
       laste:=GetLasterror;
       if ((laste<>122) AND (Laste<>123) AND (laste<>1902)) then
        RaiseLastOSError
       else
       begin
        if laste<>1902 then
        begin
         if needed>0 then
         begin
          freemem(pforminfo);
          pforminfo:=AllocMem(needed);
          if Not GetForm(FPrinterhandle,Pchar(apapername),1,pforminfo,needed,needed) then
           RaiseLastOSError;
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
       try
        if not AddForm(fprinterhandle,1,pforminfo) then
         RaiseLastOSError;
       except
        on E:Exception do
        begin
         E.Message:=SRpErrorCreatingPaper+apapername+#10+E.Message;
         Raise;
        end;
       end;
      end;
     finally
      freemem(pforminfo);
     end;
     // Select by name
     StrPCopy(PDevMode.dmFormName,apapername);
     PDevMode.dmFields:=PDevMode.dmFields or dm_formname;
     PDevMode.dmFields:=PDevMode.dmFields AND (NOT dm_papersize);
    finally
     ClosePrinter(FPrinterhandle);
    end;
   end;
  end
  else
  begin
   PDevMode.dmPaperSize :=apapersize.PageIndex;
   PDevMode.dmPaperlength := apapersize.Height;
   PDevMode.dmPaperwidth  := apapersize.Width;
  end;
 finally
  GlobalUnLock(DeviceMode);
 end;
 Printer.SetPrinter(Device, Driver, Port, DeviceMode);
end;


{$ENDIF}
{$IFDEF DOTNETD}
procedure SendControlCodeToPrinter(S: string);
var
 hDeviceMode: IntPtr;
 Handle:THandle;
 N: DWORD;
 DocInfo1: TDocInfo1;
 pdocinfo: IntPtr;
 Device, Driver, Port: String;
 PrinterName: string;
 lbuf:integer;
 buf:IntPtr;
begin
 Printer.GetPrinter(Device, Driver, Port, hDeviceMode);
 PrinterName := Format('%s', [Device]);
 if not OpenPrinter(PrinterName, Handle, nil) then
   RaiseLastOSError;
 try
  with DocInfo1 do
  begin
   pDocName := 'Control';
   pOutputFile := nil;
   pDataType := 'RAW';
  end;
  pdocinfo:=Marshal.AllocHGlobal(sizeof(TDocInfo1));
  Marshal.StructureToPtr(DocInfo1,pdocinfo,true);
  StartDocPrinter(Handle, 1, pdocinfo);
  try
//   StartPagePrinter(Handle);
   buf:=Marshal.StringToHGlobalAnsi(s);
   lbuf:=length(s);
   if not WritePrinter(Handle, buf, lbuf, N) then
    RaiseLastOSError;
//   EndPagePrinter(Handle);
  finally
   EndDocPrinter(Handle);
  end;
 finally
  ClosePrinter(Handle);
 end;
end;


function GetPrinters: TStrings;
var
 i:integer;
begin
 if FPrinters = nil then
 begin
  FPrinters.Assign(Printer.Printers);
 end;
 Result:=FPrinters;
end;


procedure SetCurrentPaper(apapersize:TGDIPageSize);
var
  Device, Driver, Port: String;
  DeviceMode: IntPtr;
  PDevmode:TDevicemode;
  pforms:IntPtr;
  pforminfo:Form_info_1;
  printername,apapername:string;
  FPrinterHandle:THandle;
  foundpaper:boolean;
  needed:DWord;
  printererror:boolean;
  laste:integer;
begin
 if printer.Printers.count<1 then
  exit;
 // Printer selected not valid error
 printererror:=false;
 try
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
 except
  printererror:=true;
 end;
 if printererror then
  exit;
 PDevMode := TDeviceMode(Marshal.PtrToStructure(GlobalLock(Integer(DeviceMode)),TypeOf(TDeviceMode)));
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
    foundpaper:=false;
    // In Windows NT we must search or create a form
    apapername:=FindFormNameFromSize(apapersize.Width,apapersize.Height);
    if Length(apapername)>0 then
     foundpaper:=true;
    if not foundpaper then
    begin
     // Busquem un form que s'adapti
     apapername:='User ('+
     IntToStr(apapersize.Width)+'x'+
     IntToStr(apapersize.Height)+')';
    end;
    PrinterName := Format('%s', [Device]);
    if not OpenPrinter(PrinterName, FPrinterHandle, nil) then
     RaiseLastOSError;
    try
     if Not GetForm(FPrinterhandle,apapername,1,pforminfo,sizeof(Form_info_1),needed) then
     begin
      laste:=GetLasterror;
      if ((laste<>122) AND (Laste<>123) AND (laste<>1902)) then
       RaiseLastOSError
      else
      begin
       if laste<>1902 then
       begin
        if needed>0 then
        begin
         pforminfo:=Form_info_1(Marshal.PtrToStructure(Marshal.AllocHGlobal(needed),TypeOf(form_info_1)));
         if Not GetForm(FPrinterhandle,apapername,1,pforminfo,needed,needed) then
          RaiseLastOSError;
        if pforminfo.pname<>nil then
         foundpaper:=true;
        // Si l'ha trobat trobem el seu index
        end;
       end;
      end;
     end;
     if Not foundpaper then
     begin
      pforminfo.pname:=apapername;
      pforminfo.Flags:=FORM_USER;
      pforminfo.Size.cx:=apapersize.Width*100;
      pforminfo.size.cy:=apapersize.Height*100;
      pforminfo.ImageableArea.Top:=0;
      pforminfo.ImageableArea.left:=0;
      pforminfo.ImageableArea.Right:=pforminfo.Size.cx;
      pforminfo.ImageableArea.Bottom:=pforminfo.size.cy;
      try
       if not AddForm(fprinterhandle,1,pforminfo) then
        RaiseLastOSError;
      except
       on E:Exception do
       begin
        Raise Exception.Create(SRpErrorCreatingPaper+apapername+#10+E.Message);
       end;
      end;
     end;
     // Select by name
     PDevMode.dmFormName:=apapername;
     PDevMode.dmFields:=PDevMode.dmFields or dm_formname;
     PDevMode.dmFields:=PDevMode.dmFields AND (NOT dm_papersize);
    finally
     ClosePrinter(FPrinterhandle);
    end;
   end;
  end
  else
  begin
   PDevMode.dmPaperSize :=apapersize.PageIndex;
   PDevMode.dmPaperlength := apapersize.Height;
   PDevMode.dmPaperwidth  := apapersize.Width;
  end;
 finally
  GlobalUnLock(Integer(DeviceMode));
 end;
 Printer.SetPrinter(Device, Driver, Port, DeviceMode);
end;


function FindFormNameFromSize(width,height:integer):String;
var
// pforms,p:^Form_info_1;
 pforms,p:IntPtr;
 needed,received:dword;
 fprinterhandle:THandle;
 aprintername:String;
 i:integer;
 cadenaimp:String;
 forminfo:Form_info_1;
begin
 aprintername:=GetPrinters[Printer.PrinterIndex];
 Result:='';
 if Not OpenPrinter(aprintername,fprinterhandle,nil) then
   Raise Exception.create(SRpError+aprintername);
 try
  pforms:=nil;
  if Not EnumForms(fprinterhandle,1,pforms,0,needed,received) then
    if GetLastError<>122 then
     RaiseLastOSError;
  pforms:=Marshal.AllocHGlobal(needed);
  try
   if NOt EnumForms(fprinterhandle,1,pforms,needed,needed,received) then
    RaiseLastOSError;
   for i:=0 to received-1 do
   begin
    p:=IntPtr(Integer(pforms)+sizeof(Form_info_1)*i);
//    forminfo:=p^;
    forminfo:=Form_info_1(Marshal.PtrToStructure(p,TypeOf(form_info_1)));
    if forminfo.Size.cx=width*100 then
     if forminfo.Size.cy=height*100 then
     begin
      Result:=forminfo.pName;
     end;
   end;
  finally
   Marshal.FreeHGlobal(pforms);
  end;
 finally
  ClosePrinter(fprinterhandle);
 end;
end;

{$ENDIF}



{procedure FillFormsList(FormsList:TStringList);
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
     RaiseLastOSError;
   pforms:=Allocmem(needed);
   try
    if NOt EnumForms(fprinterhandle,1,pforms,needed,needed,received) then
     RaiseLastOSError;
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
}

function PrinterSupportsCollation:Boolean;
var
{$IFDEF DOTNETD}
  Device, Driver, Port: String;
  DeviceMode: IntPtr;
{$ENDIF}
{$IFNDEF DOTNETD}
  Device, Driver, Port: array[0..1023] of char;
  DeviceMode: THandle;
{$ENDIF}
  printererror:boolean;
  aresult:DWord;
begin
 Result:=false;
 if printer.Printers.count<1 then
  exit;
 // Printer selected not valid error
 printererror:=false;
 try
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
 except
  printererror:=true;
 end;
 if printererror then
  exit;
 try
  aresult:=DeviceCapabilities(Device,Port,DC_COLLATE,nil,nil);
  // Function fail =-1
  if aresult>0 then
    Result:=true;
 except
 end;
end;


function PrinterMaxCopiesSupport:Integer;
var
{$IFDEF DOTNETD}
  Device, Driver, Port: String;
  DeviceMode: IntPtr;
{$ENDIF}
{$IFNDEF DOTNETD}
  Device, Driver, Port: array[0..1023] of char;
  DeviceMode: THandle;
{$ENDIF}
  printererror:boolean;
  maxcopies:integer;
begin
 Result:=1;
 if printer.Printers.count<1 then
  exit;
 maxcopies:=1;
 // Printer selected not valid error
 printererror:=false;
 try
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
 except
  printererror:=true;
 end;
 if printererror then
  exit;
 try
   maxcopies:=DeviceCapabilities(Device,Port,DC_COPIES,nil,nil);
   if maxcopies<0 then
    maxcopies:=1;
 except
 end;
 Result:=maxcopies;
end;

function PrinterSupportsCopies(copies:integer):Boolean;
var
 maxcopies:integer;
begin
 maxcopies:=PrinterMaxCopiesSupport;
 Result:=maxcopies>copies;
end;

procedure SetPrinterCopies(copies:integer);
var
{$IFDEF DOTNETD}
  DeviceMode: IntPtr;
  PDevMode :  TDeviceMode;
  Device, Driver, Port: String;
{$ENDIF}
{$IFNDEF DOTNETD}
  DeviceMode: THandle;
  PDevMode :  ^TDeviceMode;
  Device, Driver, Port: array[0..1023] of char;
{$ENDIF}
  printererror:boolean;
begin
 if printer.Printers.count<1 then
  exit;
 // Printer selected not valid error
 printererror:=false;
 try
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
 except
  printererror:=true;
 end;
 if printererror then
  exit;
{$IFDEF DOTNETD}
 PDevMode := TDeviceMode(Marshal.PtrToStructure(GlobalLock(Integer(DeviceMode)),TypeOf(TDeviceMode)));
{$ENDIF}
{$IFNDEF DOTNETD}
 PDevMode := GlobalLock(DeviceMode);
{$ENDIF}
 try
  PDevMode.dmCopies:=copies;
 finally
{$IFNDEF DOTNETD}
  GlobalUnLock(DeviceMode);
{$ENDIF}
{$IFDEF DOTNETD}
  GlobalUnLock(Integer(DeviceMode));
{$ENDIF}
 end;
 Printer.SetPrinter(Device, Driver, Port, DeviceMode);
end;

function GetPrinterCopies:Integer;
var
{$IFDEF DOTNETD}
  Device, Driver, Port: String;
  DeviceMode: IntPtr;
  PDevmode:TDevicemode;
{$ENDIF}
{$IFNDEF DOTNETD}
  Device, Driver, Port: array[0..1023] of char;
  DeviceMode: THandle;
  PDevmode:^TDevicemode;
{$ENDIF}
  printererror:boolean;
begin
 Result:=1;
 if printer.Printers.count<1 then
  exit;
 // Printer selected not valid error
 printererror:=false;
 try
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
 except
  printererror:=true;
 end;
 if printererror then
  exit;
{$IFDEF DOTNETD}
 PDevMode := TDeviceMode(Marshal.PtrToStructure(GlobalLock(Integer(DeviceMode)),TypeOf(TDeviceMode)));
{$ENDIF}
{$IFNDEF DOTNETD}
 PDevMode := GlobalLock(DeviceMode);
{$ENDIF}
 try
  Result:=PDevMode.dmCopies;
 finally
{$IFNDEF DOTNETD}
  GlobalUnLock(DeviceMode);
{$ENDIF}
{$IFDEF DOTNETD}
  GlobalUnLock(Integer(DeviceMode));
{$ENDIF}
 end;
 Printer.SetPrinter(Device, Driver, Port, DeviceMode);
end;

procedure SetPrinterCollation(collation:boolean);
var
{$IFDEF DOTNETD}
  Device, Driver, Port: String;
  DeviceMode: IntPtr;
  PDevmode:TDevicemode;
{$ENDIF}
{$IFNDEF DOTNETD}
  Device, Driver, Port: array[0..1023] of char;
  DeviceMode: THandle;
  PDevmode:^TDevicemode;
{$ENDIF}
  printererror:boolean;
begin
 if printer.Printers.count<1 then
  exit;
 // Printer selected not valid error
 printererror:=false;
 try
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
 except
  printererror:=true;
 end;
 if printererror then
  exit;
{$IFDEF DOTNETD}
 PDevMode := TDeviceMode(Marshal.PtrToStructure(GlobalLock(Integer(DeviceMode)),TypeOf(TDeviceMode)));
{$ENDIF}
{$IFNDEF DOTNETD}
 PDevMode := GlobalLock(DeviceMode);
{$ENDIF}
 try
  if collation then
   PDevMode.dmCollate:=DMCOLLATE_TRUE
  else
   PDevMode.dmCollate:=DMCOLLATE_FALSE;
 finally
{$IFNDEF DOTNETD}
  GlobalUnLock(DeviceMode);
{$ENDIF}
{$IFDEF DOTNETD}
  GlobalUnLock(Integer(DeviceMode));
{$ENDIF}
 end;
 Printer.SetPrinter(Device, Driver, Port, DeviceMode);
end;

function GetPrinterCollation:Boolean;
var
{$IFDEF DOTNETD}
  Device, Driver, Port: String;
  DeviceMode: IntPtr;
  PDevmode:TDevicemode;
{$ENDIF}
{$IFNDEF DOTNETD}
  Device, Driver, Port: array[0..1023] of char;
  DeviceMode: THandle;
  PDevmode:^TDevicemode;
{$ENDIF}
  printererror:boolean;
begin
 Result:=false;
 if printer.Printers.count<1 then
  exit;
 // Printer selected not valid error
 printererror:=false;
 try
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
 except
  printererror:=true;
 end;
 if printererror then
  exit;
{$IFDEF DOTNETD}
 PDevMode := TDeviceMode(Marshal.PtrToStructure(GlobalLock(Integer(DeviceMode)),TypeOf(TDeviceMode)));
{$ENDIF}
{$IFNDEF DOTNETD}
 PDevMode := GlobalLock(DeviceMode);
{$ENDIF}
 try
  Result:=PDevMode.dmCollate=DMCOLLATE_TRUE;
 finally
{$IFNDEF DOTNETD}
  GlobalUnLock(DeviceMode);
{$ENDIF}
{$IFDEF DOTNETD}
  GlobalUnLock(Integer(DeviceMode));
{$ENDIF}
 end;
 Printer.SetPrinter(Device, Driver, Port, DeviceMode);
end;



function SearchnodeInt(ATree:TTreeView;astring:String;anode:TTreeNode):TTreeNode;
var
 i:integer;
 firstname:string;
begin
 firstname:=GetFirstName(astring);
 Result:=nil;
 for i:=0 to anode.Count-1 do
 begin
  if firstname=anode.Item[i].Text then
  begin
   if firstname=astring then
   begin
    Result:=anode.Item[i];
   end
   else
    Result:=Searchnodeint(ATree,Copy(astring,length(firstname)+2,length(astring)),anode.Item[i]);
  end;
 end;
 if Not Assigned(Result) then
 begin
  Result:=ATree.Items.AddChild(anode,firstname);
  Result.ImageIndex:=2;
  if firstname<>astring then
  begin
   Result:=Searchnodeint(ATree,Copy(astring,length(firstname)+2,length(astring)),Result);
  end;
 end;
end;

function Searchnode(FTopItems:TStringList;ATree:TTreeView;astring:String):TTreeNode;
var
 i:integer;
 firstname:string;
begin
 firstname:=GetFirstName(astring);
 Result:=nil;
 for i:=0 to FTopItems.Count-1 do
 begin
  if firstname=FTopItems.Strings[i] then
  begin
   if firstname=astring then
   begin
    Result:=TTreeNode(FTopItems.Objects[i]);
   end
   else
    Result:=Searchnodeint(ATree,Copy(astring,length(firstname)+2,length(astring)),TTreeNode(FTopItems.Objects[i]));
  end;
 end;
 if Not Assigned(Result) then
 begin
  Result:=ATree.Items.AddChild(nil,firstname);
  Result.ImageIndex:=2;
  FTopItems.AddObject(firstname,Result);
  if firstname<>astring then
  begin
   Result:=Searchnodeint(ATree,Copy(astring,length(firstname)+2,length(astring)),Result);
  end;
 end;
end;



procedure FillTreeView(ATree:TTreeView;alist:TStringList);
var
 newitem,anode:TTreeNode;
 astring:string;
 repname,dirname:String;
 i:integer;
 FTopItems:TStringList;
begin
 FTopitems:=TStringList.Create;
 try
  for i:=0 to alist.count-1 do
  begin
   if Length(alist.Strings[i])<1 then
    continue;
   astring:=alist.Strings[i];
   repname:=GetLastName(astring);
   dirname:=GetPathName(astring);
   anode:=SearchNode(FTopItems,ATree,dirname);
   newitem:=ATree.Items.AddChild(anode,repname);
   newitem.ImageIndex:=3;
  end;
 finally
  FTopItems.Free;
 end;
end;


function GetFullFileName(ANode:TTreeNode;dirseparator:char):String;
begin
 if Assigned(ANode.Parent) then
  Result:=GetFullFileName(ANode.Parent,dirseparator)+dirseparator+ANode.Text
 else
  Result:=ANode.Text;
end;


end.
