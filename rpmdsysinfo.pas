{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdysinfo                                      }
{       Form showing info about printer and system      }
{                                                       }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
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
begin
 if Printer.Printers.Count>0 then
 begin
  EPrinterName.text:=Printer.Printers.Strings[Printer.PrinterIndex];
  // Printer selected not valid error
  printererror:=false;
  try
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
  end;
 end;
end;

end.
