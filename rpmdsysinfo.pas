{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdysinfo                                      }
{       Form showing info about printer and system      }
{                                                       }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2003 Toni Martir             }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{*******************************************************}

unit rpmdsysinfo;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls,Printers,rpmdconsts,WinSpool,Dialogs;

type
  TFRpSysInfo = class(TForm)
    BOK: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    EStatus: TEdit;
    EPrinterName: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    EDevice: TEdit;
    EDriver: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    EPort: TEdit;
    Label6: TLabel;
    LMaxCopies: TLabel;
    Label7: TLabel;
    LCollation: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LColor: TLabel;
    LResolution: TLabel;
    GroupBox2: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    LProcessors: TLabel;
    LOEMID: TLabel;
    LDisplay: TLabel;
    Label13: TLabel;
    LOS: TLabel;
    Label14: TLabel;
    Label12: TLabel;
    LVersion: TLabel;
    Label15: TLabel;
    LTechnology: TLabel;
    Label16: TLabel;
    CLineCaps: TComboBox;
    CRasterCaps: TComboBox;
    Label17: TLabel;
    CPolyCaps: TComboBox;
    Label18: TLabel;
    Label19: TLabel;
    CTextCaps: TComboBox;
    Label20: TLabel;
    CCurveCaps: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure RpShowSystemInfo;

implementation

uses rpvgraphutils;

{$R *.dfm}

procedure RpShowSystemInfo;
var
 dia:TFRpSysInfo;
begin
 dia:=TFRpSysInfo.Create(Application);
 try
  dia.ShowModal;
 finally
  dia.free;
 end;
end;



procedure TFRpSysInfo.FormCreate(Sender: TObject);
begin
// Caption:=TranslateStr(1,Caption);
 BOK.Caption:=SRpOk;
 LMaxCopies.Font.Style:=[fsBold];
 Lresolution.Font.Style:=[fsBold];
 LCollation.Font.Style:=[fsBold];
 LColor.Font.Style:=[fsBold];
 LOemID.Font.Style:=[fsBold];
 LProcessors.Font.Style:=[fsBold];
 LDisplay.Font.Style:=[fsBold];
 LOS.Font.Style:=[fsBold];
 LVersion.Font.Style:=[fsBold];
 LTechnology.Font.Style:=[fsBold];
end;

procedure TFRpSysInfo.FormShow(Sender: TObject);
var
 DeviceMode: THandle;
 Device, Driver, Port: array[0..1023] of char;
 printererror:boolean;
 printererrormessage:String;
 FPrinterHandle:THandle;
 maxcopies:Integer;
 pdevmode:^DEVMODE;
 buf:PChar;
 asize:integer;
 cadenaimp:String;
 sysinfo:SYSTEM_INFO;
 osinfo:TOsVersionInfo;
 caps:integer;
 dc:HDC;
begin
 dc:=0;
 if Printer.Printers.Count>0 then
 begin
  EPrinterName.text:=Printer.Printers.Strings[Printer.PrinterIndex];
  // Printer selected not valid error
  printererror:=false;
  try
   dc:=Printer.Handle;
   Printer.GetPrinter(Device, Driver, Port, DeviceMode);
  except
   on E:Exception do
   begin
    printererror:=true;
    printererrormessage:=E.Message;
   end;
  end;
  if printererror then
  begin
   EStatus.Text:=printererrormessage;
  end
  else
  begin
   EStatus.Text:=SRpSReady;
   EDevice.Text:=StrPas(Device);
   EDriver.Text:=StrPas(Driver);
   EPort.Text:=StrPas(Port);
   maxcopies:=PrinterMaxCopiesSupport;
   LMaxCopies.Caption:=FormatFloat('####,0',maxcopies);
   if PrinterSupportsCollation then
    LCollation.Caption:=SRpYes
   else
    LCollation.Caption:=SRpNo;
   cadenaimp:=Device;
   buf:=Pchar(cadenaimp);
   if OpenPrinter(buf,fprinterhandle,nil) then
   begin
    try
     pdevmode:=AllocMem(sizeof(devmode));
     try
      asize:=DocumentProperties(0,fprinterhandle,Device,pdevmode^,pdevmode^,0);
      if asize>0 then
      begin
       FreeMem(pdevmode);
       pdevmode:=AllocMem(asize);
       if IDOK=DocumentProperties(0,fprinterhandle,Device,pdevmode^,pdevmode^,DM_OUT_BUFFER) then
       begin
        // Dev mode properties
        if (pdevmode^.dmFields AND DM_COLOR)>0 then
        begin
         if (pdevmode^.dmColor=DMCOLOR_COLOR) then
         begin
          LColor.Caption:=SRpSColorPrinting;
         end
         else
         begin
          LColor.Caption:=SRpSMonoPrinting;
         end;
        end;
        if (pdevmode^.dmFields AND DM_YRESOLUTION)>0 then
        begin
         LResolution.Caption:=FormatFloat('###,###',pdevmode^.dmPrintQuality)+
          ' x '+FormatFloat('###,###',pdevmode^.dmYResolution);
        end
        else
        begin
         if integer(pdevmode^.dmPrintQuality)<=0 then
         begin
          case DWORD(pdevmode^.dmPrintQuality) of
           DMRES_HIGH:
            LResolution.Caption:=SRpSHighResolution;
           DMRES_MEDIUM:
            LResolution.Caption:=SRpSMediumResolution;
           DMRES_LOW:
            LResolution.Caption:=SRpSLowResolution;
           DMRES_DRAFT:
            LResolution.Caption:=SRpSDraftResolution;
          end;
         end
         else
         begin
          LResolution.Caption:=FormatFloat('###,###',pdevmode^.dmPrintQuality)+
           ' x '+FormatFloat('###,###',pdevmode^.dmPrintQuality);
         end;
        end;
       end;
      end;
     finally
      FreeMem(pdevmode);
     end;
    finally
     ClosePrinter(fprinterhandle);
    end;
   end;
   caps:=GetDeviceCaps(dc,TECHNOLOGY);
   case caps of
    DT_PLOTTER:
     LTechnology.Caption:=SRSPlotter;
    DT_RASDISPLAY:
     LTechnology.Caption:=SRSRasterDisplay;
    DT_RASPRINTER:
     LTechnology.Caption:=SRSRasterPrinter;
    DT_RASCAMERA:
     LTechnology.Caption:=SRSRasterCamera;
    DT_CHARSTREAM:
     LTechnology.Caption:=SRSCharStream;
    DT_METAFILE:
     LTechnology.Caption:=SRpSMetafile;
    DT_DISPFILE:
     LTechnology.Caption:=SRSDisplayFile;
    else
     LTechnology.Caption:=SRpSUnknownType;
   end;
   caps:=GetDeviceCaps(dc,LINECAPS);
   if caps=LC_NONE then
    CLineCaps.Items.Add(SRpNone)
   else
   begin
    CLineCaps.Items.Add(SRpYes);
    if (caps AND LC_POLYLINE)>0 then
     CLineCaps.Items.Add(SRpSPolyline);
    if (caps AND LC_MARKER)>0 then
     CLineCaps.Items.Add(SRpSMarker);
    if (caps AND LC_WIDE)>0 then
     CLineCaps.Items.Add(SRpSWideCap);
    if (caps AND LC_STYLED)>0 then
     CLineCaps.Items.Add(SRpSSTyledCap);
    if (caps AND LC_WIDESTYLED)>0 then
     CLineCaps.Items.Add(SRpSWideSTyledCap);
    if (caps AND LC_INTERIORS)>0 then
     CLineCaps.Items.Add(SRpSInteriorsCap);
   end;
   CLineCaps.ItemIndex:=0;
   caps:=GetDeviceCaps(dc,POLYGONALCAPS);
   if caps=PC_NONE then
    CPolyCaps.Items.Add(SRpNone)
   else
   begin
    CPolyCaps.Items.Add(SRpYes);
    if (caps AND PC_POLYGON)>0 then
     CPolyCaps.Items.Add(SRpSPolygon);
    if (caps AND PC_RECTANGLE)>0 then
     CPolyCaps.Items.Add(SRpSRectanglecap);
    if (caps AND PC_WINDPOLYGON)>0 then
     CPolyCaps.Items.Add(SRpSWindPolygon);
    if (caps AND PC_STYLED)>0 then
     CPolyCaps.Items.Add(SRpSSTyledCap);
    if (caps AND PC_WIDE)>0 then
     CPolyCaps.Items.Add(SRpSWideCap);
    if (caps AND PC_WIDESTYLED)>0 then
     CPolyCaps.Items.Add(SRpSWideSTyledCap);
    if (caps AND PC_INTERIORS)>0 then
     CPolyCaps.Items.Add(SRpSInteriorsCap);
   end;
   CPolyCaps.ItemIndex:=0;
   caps:=GetDeviceCaps(dc,CURVECAPS);
   if caps=CC_NONE then
    CCurveCaps.Items.Add(SRpNone)
   else
   begin
    CCurveCaps.Items.Add(SRpYes);
    if (caps AND CC_CIRCLES)>0 then
     CCurveCaps.Items.Add(SRpSCircleCap);
    if (caps AND CC_PIE)>0 then
     CCurveCaps.Items.Add(SRpSPiecap);
    if (caps AND CC_CHORD)>0 then
     CCurveCaps.Items.Add(SRpSCHordCap);
    if (caps AND CC_ELLIPSES)>0 then
     CCurveCaps.Items.Add(SRpSEllipses);
    if (caps AND CC_ROUNDRECT)>0 then
     CCurveCaps.Items.Add(SRpSRoundRectCap);
    if (caps AND CC_STYLED)>0 then
     CCurveCaps.Items.Add(SRpSSTyledCap);
    if (caps AND CC_WIDE)>0 then
     CCurveCaps.Items.Add(SRpSWideCap);
    if (caps AND CC_WIDESTYLED)>0 then
     CCurveCaps.Items.Add(SRpSWideSTyledCap);
    if (caps AND CC_INTERIORS)>0 then
     CCurveCaps.Items.Add(SRpSInteriorsCap);
   end;
   CCurveCaps.ItemIndex:=0;
   caps:=GetDeviceCaps(dc,RASTERCAPS);
   if (caps AND RC_BANDING)>0 then
    CRasterCaps.Items.Add(SRpSBandingRequired);
   if (caps AND RC_BITBLT)>0 then
    CRasterCaps.Items.Add(SRpSBitmapTransfer);
   if (caps AND RC_BITMAP64)>0 then
    CRasterCaps.Items.Add(SRpSBitmapTransfer64);
   if (caps AND RC_DI_BITMAP)>0 then
    CRasterCaps.Items.Add(SRpSDIBTransfer);
   if (caps AND RC_DIBTODEV)>0 then
    CRasterCaps.Items.Add(SRpSDIBDevTransfer);
   if (caps AND RC_FLOODFILL)>0 then
    CRasterCaps.Items.Add(SRpSFloodFillcap);
   if (caps AND RC_GDI20_OUTPUT	)>0 then
    CRasterCaps.Items.Add(SRpSGDI20Out);
   if (caps AND RC_PALETTE)>0 then
    CRasterCaps.Items.Add(SRPSPaletteDev);
   if (caps AND RC_SCALING)>0 then
    CRasterCaps.Items.Add(SRpSScalingCap);
   if (caps AND RC_STRETCHBLT)>0 then
    CRasterCaps.Items.Add(SRpSStretchCap);
   if (caps AND RC_STRETCHDIB	)>0 then
    CRasterCaps.Items.Add(SRpSStretchDIBCap);

   if CRasterCaps.Items.Count=0 then
    CRasterCaps.Items.Add(SRpNone)
   else
    CRasterCaps.Items.Insert(0,SRpYes);
   CRasterCaps.ItemIndex:=0;
   // Text caps
   caps:=GetDeviceCaps(dc,TEXTCAPS);
   if (caps AND TC_OP_CHARACTER)>0 then
    CTextCaps.Items.Add(SRpSCharOutput);
   if (caps AND TC_OP_STROKE)>0 then
    CTextCaps.Items.Add(SRpSCharStroke);
   if (caps AND TC_CP_STROKE)>0 then
    CTextCaps.Items.Add(SRpSClipStroke);
   if (caps AND TC_CR_90)>0 then
    CTextCaps.Items.Add(SRpS90Rotation);
   if (caps AND TC_CR_ANY)>0 then
    CTextCaps.Items.Add(SRpSAnyRotation);
   if (caps AND TC_SF_X_YINDEP)>0 then
    CTextCaps.Items.Add(SRpSScaleXY);
   if (caps AND TC_SA_DOUBLE)>0 then
    CTextCaps.Items.Add(SRpSDoubleChar);
   if (caps AND TC_SA_INTEGER)>0 then
    CTextCaps.Items.Add(SRpSIntegerScale);
   if (caps AND TC_SA_CONTIN)>0 then
    CTextCaps.Items.Add(SRpSAnyrScale);
   if (caps AND TC_EA_DOUBLE)>0 then
    CTextCaps.Items.Add(SRpSDoubleWeight);
   if (caps AND TC_IA_ABLE)>0 then
    CTextCaps.Items.Add(SRpItalic);
   if (caps AND TC_UA_ABLE)>0 then
    CTextCaps.Items.Add(SRpUnderline);
   if (caps AND TC_SO_ABLE)>0 then
    CTextCaps.Items.Add(SRpStrikeOut);
   if (caps AND TC_RA_ABLE)>0 then
    CTextCaps.Items.Add(SRpRasterFonts);
   if (caps AND TC_VA_ABLE)>0 then
    CTextCaps.Items.Add(SRpVectorFonts);
   if (caps AND TC_SCROLLBLT)>0 then
    CTextCaps.Items.Add(SRpNobitBlockScroll);
   if CTextCaps.Items.Count=0 then
    CTextCaps.Items.Add(SRpNone)
   else
    CTextCaps.Items.Insert(0,SRpYes);
   CTextCaps.ItemIndex:=0;
  end;
 end;
 GetSystemInfo(sysinfo);
 LOemID.Caption:=IntToStr(sysinfo.dwOemId);
 LProcessors.Caption:=IntToStr(sysinfo.dwNumberOfProcessors);
 LDisplay.Caption:=FormatCurr('##,##',GetSysTemMetrics(SM_CXSCREEN))+
  ' x '+FormatCurr('##,##',GetSysTemMetrics(SM_CYSCREEN))+' '+
   SRpDPIRes+':'+IntToStr(Screen.PixelsPerInch);
 osinfo.dwOSVersionInfoSize:=sizeof(osinfo);
 if GetVersionEx(osinfo) then
 begin
  if osinfo.dwPlatformId=VER_PLATFORM_WIN32_NT then
   LOS.Caption:='Windows NT/XP/NET'
  else
   LOS.Caption:='Windows 95/98/ME';
  LVersion.Caption:=IntToStr(osinfo.dwMajorVersion)+
   '.'+IntToStr(osinfo.dwMinorVersion)+' Build:'+IntToStr(osinfo.dwBuildNumber)+
   '-'+StrPas(osinfo.szCSDVersion);
 end;
end;

end.