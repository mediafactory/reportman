{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdysinfoqt                                    }
{       Form showing info about printer and system      }
{       Qt version                                      }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2003 Toni Martir             }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{*******************************************************}

unit rpmdsysinfoqt;

interface

{$I rpconf.inc}

uses SysUtils, Classes, QGraphics, QForms, QControls, QStdCtrls,
  QButtons, QExtCtrls,QPrinters,rpmdconsts,QDialogs,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Qt;

type
  TFRpSysInfo = class(TForm)
    BOK: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    EStatus: TEdit;
    EPrinterName: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    LCollation: TLabel;
    Label9: TLabel;
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
    EPrinterDevice: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure RpShowSystemInfoQt;

implementation

{$IFDEF MSWINDOWS}
uses rpvgraphutils;
{$ENDIF}

{$R *.xfm}

procedure RpShowSystemInfoQt;
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
 Lresolution.Font.Style:=[fsBold];
 LCollation.Font.Style:=[fsBold];
 LOemID.Font.Style:=[fsBold];
 LProcessors.Font.Style:=[fsBold];
 LDisplay.Font.Style:=[fsBold];
 LOS.Font.Style:=[fsBold];
 LVersion.Font.Style:=[fsBold];
end;

procedure TFRpSysInfo.FormShow(Sender: TObject);
var
 abuffer:widestring;
{$IFDEF MSWINDOWS}
 sysinfo:SYSTEM_INFO;
 osinfo:TOsVersionInfo;
{$ENDIF}
begin
 if Printer.Printers.Count>0 then
 begin
  SetLength(abuffer,500);
  QPrinter_printerName(QPrinterH(Printer.Handle),@abuffer);
  EPrinterDevice.Text:=abuffer;
  // If there is no printer selected, selects one
  if Printer.Printers.Count>0 then
  begin
   if Length(EPrinterDevice.Text)<1 then
   begin
    Printer.SetPrinter(Printer.Printers.Strings[0]);
    QPrinter_printerName(QPrinterH(Printer.Handle),@abuffer);
    EPrinterDevice.Text:=abuffer;
   end;
  end;
  EPrinterName.text:=EPrinterDevice.Text;
  LResolution.Caption:=FormatFloat('###,###',Printer.XDPI)+
      ' x '+FormatFloat('###,###',Printer.YDPI);
  // Printer selected not valid error
  if Printer.XDPI=0 then
   EStatus.Text:=SRpSNotAvail
  else
   EStatus.Text:=SRpSReady;
 end;
 LDisplay.Caption:=FormatCurr('##,##',Screen.Width)+
  ' x '+FormatCurr('##,##',Screen.Height)+' '+
   SRpDPIRes+':'+IntToStr(Screen.PixelsPerInch);
{$IFDEF MSWINDOWS}
 GetSystemInfo(sysinfo);
 LOemID.Caption:=IntToStr(sysinfo.dwOemId);
 LProcessors.Caption:=IntToStr(sysinfo.dwNumberOfProcessors);
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
{$ENDIF}

end;

end.
