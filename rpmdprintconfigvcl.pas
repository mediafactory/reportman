{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpmdprintconfigvcl                              }
{                                                       }
{       Configuration dialog for user printers          }
{       it stores all info in config files              }
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

unit rpmdprintconfigvcl;

interface

{$I rpconf.inc}

uses SysUtils, Classes,
  Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls,Printers,
  rpmdconsts,IniFiles,rpmdshfolder,
  rptypes,rpmunits;

type
  TFRpPrinterConfigVCL = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    LSelPrinter: TListBox;
    LSelectPrinter: TLabel;
    ComboPrinters: TComboBox;
    CheckPrinterFonts: TCheckBox;
    GConfigFile: TGroupBox;
    EConfigFile: TEdit;
    RadioUser: TRadioButton;
    RadioSystem: TRadioButton;
    GPageMargins: TGroupBox;
    LLeft: TLabel;
    ELeftMargin: TEdit;
    ETopMargin: TEdit;
    LTop: TLabel;
    LMetrics3: TLabel;
    LMetrics4: TLabel;
    LOperations: TLabel;
    LExample: TLabel;
    CheckCutPaper: TCheckBox;
    ECutPaper: TEdit;
    CheckOpenDrawer: TCheckBox;
    EOpenDrawer: TEdit;
    LExample2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LSelPrinterClick(Sender: TObject);
    procedure RadioUserClick(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure CheckPrinterFontsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboPrintersChange(Sender: TObject);
    procedure ELeftMarginChange(Sender: TObject);
    procedure ECutPaperChange(Sender: TObject);
    procedure CheckCutPaperClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoSave;
  public
    { Public declarations }
  end;

procedure ShowPrintersConfiguration;

implementation

{$R *.dfm}

var
 configfilename:string;
 userconfigfilename:string;
 systemconfigfilename:string;
 configinifile:TMemInifile;
 printernames:TStringList;
 userconfig:boolean;

procedure ShowPrintersConfiguration;
var
 dia:TFRpPrinterConfigVCL;
begin
 dia:=TFRpPrinterConfigVCL.Create(Application);
 try
  dia.showmodal;
 finally
  dia.free;
 end;
end;

procedure TFRpPrinterConfigVCL.FormCreate(Sender: TObject);
begin
 BOK.Caption:=TranslateStr(93,BOK.Caption);
 BCancel.Caption:=TranslateStr(94,BCancel.Caption);
 CheckPrinterFonts.Caption:=TranslateStr(113,CheckPrinterFonts.Caption);
 LSelectPrinter.Caption:=TranslateStr(741,LSelectPrinter.Caption);
 GConfigFile.Caption:=TranslateStr(743,GConfigFile.Caption);
 RadioUser.Caption:=TranslateStr(744,RadioUser.Caption);
 RadioSystem.Caption:=TranslateStr(745,RadioSystem.Caption);
 Caption:=TranslateStr(742,Caption);
 GPageMargins.Caption:=TranslateStr(746,GPagemargins.Caption);
 LLeft.Caption:=TranslateStr(100,LLeft.Caption);
 LTop.Caption:=TranslateStr(102,LTop.Caption);
 LMetrics3.Caption:=rpunitlabels[defaultunit];
 LMetrics4.Caption:=LMetrics3.Caption;
 LOperations.Caption:=TranslateStr(763,LOperations.Caption);
 LExample.Caption:=TranslateStr(764,LExample.Caption);
 LExample2.Caption:=TranslateStr(765,LExample2.Caption);
 CheckCutPaper.Caption:=TranslateStr(766,CheckCutPaper.Caption);
 CheckOpenDrawer.Caption:=TranslateStr(767,CheckOpenDrawer.Caption);
 with LSelPrinter.Items do
 begin
  Add(SRpDefaultPrinter);
  Add(SRpReportPrinter);
  Add(SRpTicketPrinter);
  Add(SRpGraphicprinter);
  Add(SRpCharacterprinter);
  Add(SRpReportPrinter2);
  Add(SRpTicketPrinter2);
  Add(SRpUserPrinter1);
  Add(SRpUserPrinter2);
  Add(SRpUserPrinter3);
  Add(SRpUserPrinter4);
  Add(SRpUserPrinter5);
  Add(SRpUserPrinter6);
  Add(SRpUserPrinter7);
  Add(SRpUserPrinter8);
  Add(SRpUserPrinter9);
 end;
 LSelPrinter.ItemIndex:=0;
 LSelPrinterClick(Self);
 RadioSystem.Checked:=not userconfig;
 RadioUser.Checked:=userconfig;
 RadioUserClick(Self);

 ComboPrinters.Items.Assign(printernames);
 ComboPrinters.Items.Insert(0,SRpDefaultPrinter);
end;

procedure TFRpPrinterConfigVCL.LSelPrinterClick(Sender: TObject);
begin
 if LSelPrinter.ItemIndex=0 then
 begin
  LSelectPrinter.Visible:=False;
  ComboPrinters.ItemIndex:=0;
  ComboPrinters.Visible:=false;
  // Reads the configuration for the default printer
  CheckPrinterFonts.Checked:=configinifile.ReadBool('PrinterFonts','Default',false);
 end
 else
 begin
  LSelectPrinter.Visible:=True;
  ComboPrinters.Visible:=True;
  // Read the configuration for the selected printer
  CheckPrinterFonts.Checked:=configinifile.ReadBool('PrinterFonts','Printer'+IntToStr(LSelPrinter.ItemIndex),false);
  ComboPrinters.ItemIndex:=ComboPrinters.Items.IndexOf(configinifile.ReadString('PrinterNames','Printer'+IntToStr(LSelPrinter.ItemIndex),''));
  if ComboPrinters.ItemIndex<0 then
  begin
   ComboPrinters.ItemIndex:=0;
   ComboPrintersChange(Self);
  end;
 end;
 ELeftMargin.Text:=gettextfromtwips(configinifile.ReadInteger('PrinterOffsetX','Printer'+IntToStr(LSelPrinter.ItemIndex),0));
 ETopMargin.Text:=gettextfromtwips(configinifile.ReadInteger('PrinterOffsetY','Printer'+IntToStr(LSelPrinter.ItemIndex),0));
 CheckCutPaper.Checked:=configinifile.ReadBool('CutPaperOn','Printer'+IntToStr(LSelPrinter.ItemIndex),false);
 ECutPaper.Text:=configinifile.ReadString('CutPaper','Printer'+IntToStr(LSelPrinter.ItemIndex),'');
 CheckOpenDrawer.Checked:=configinifile.ReadBool('OpenDrawerOn','Printer'+IntToStr(LSelPrinter.ItemIndex),false);
 EOpenDrawer.Text:=configinifile.ReadString('OpenDrawer','Printer'+IntToStr(LSelPrinter.ItemIndex),'');
end;

procedure ReadPrintersConfig;
begin
 userconfig:=true;
 systemconfigfilename:=Obtainininamecommonconfig('','','reportman');
 userconfigfilename:=Obtainininameuserconfig('','','reportman');
 if FileExists(systemconfigfilename) then
 begin
  configfilename:=systemconfigfilename;
  userconfig:=false;
 end
 else
 begin
  configfilename:=userconfigfilename;
 end;
 if assigned(configinifile) then
 begin
  configinifile.free;
  configinifile:=nil;
 end;
 configinifile:=TMemInifile.Create(configfilename);
end;

procedure TFRpPrinterConfigVCL.RadioUserClick(Sender: TObject);
begin
 // Sets the filename
 if RadioSystem.Checked then
 begin
  EConfigFile.Text:=systemconfigfilename;
 end
 else
 begin
  EConfigFile.Text:=userconfigfilename;
 end;
end;

procedure TFRpPrinterConfigVCL.BOKClick(Sender: TObject);
begin
 DoSave;
 Close;
end;

procedure TFRpPrinterConfigVCL.DoSave;
begin
 if configinifile.FileName=EConfigFile.TExt then
  configinifile.UpdateFile
 else
 begin
  configinifile.Rename(EConfigFile.Text,false);
  configinifile.UpdateFile
 end;
end;

procedure TFRpPrinterConfigVCL.CheckPrinterFontsClick(Sender: TObject);
begin
 if LSelPrinter.ItemIndex=0 then
 begin
  configinifile.WriteBool('PrinterFonts','Default',CheckPrinterFonts.Checked);
 end
 else
 begin
  configinifile.WriteBool('PrinterFonts','Printer'+IntToStr(LSelPrinter.ItemIndex),CheckPrinterFonts.Checked);
 end;
end;

procedure TFRpPrinterConfigVCL.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 ReadPrintersConfig;
 printerconfigfile.free;
 printerconfigfile:=nil;
end;


procedure TFRpPrinterConfigVCL.ComboPrintersChange(Sender: TObject);
begin
 if LSelPrinter.ItemIndex<=0 then
  exit;
 configinifile.WriteString('PrinterNames','Printer'+IntToStr(LSelPrinter.ItemIndex),ComboPrinters.Text);
end;

procedure TFRpPrinterConfigVCL.ELeftMarginChange(Sender: TObject);
var
 margin:integer;
begin
 if LSelPrinter.ItemIndex<0 then
  exit;
 try
  margin:=gettwipsfromtext(TEdit(Sender).Text);
 except
  margin:=0;
 end;
 if Sender=ELeftMargin then
  configinifile.WriteInteger('PrinterOffsetX','Printer'+IntToStr(LSelPrinter.ItemIndex),margin)
 else
  configinifile.WriteInteger('PrinterOffsetY','Printer'+IntToStr(LSelPrinter.ItemIndex),margin);
end;

procedure TFRpPrinterConfigVCL.ECutPaperChange(Sender: TObject);
var
 Operation:String;
begin
 if Sender=ECutPaper then
  Operation:='CutPaper'
 else
  Operation:='OpenDrawer';
 configinifile.WriteString(Operation,'Printer'+IntToStr(LSelPrinter.ItemIndex),(Sender As TEdit).Text);
end;

procedure TFRpPrinterConfigVCL.CheckCutPaperClick(Sender: TObject);
var
 Operation:String;
begin
 if Sender=CheckCutPaper then
  Operation:='CutPaper'
 else
  Operation:='OpenDrawer';
 configinifile.WriteBool(Operation+'On','Printer'+IntToStr(LSelPrinter.ItemIndex),(Sender As TCheckBox).Checked);
end;

initialization
printernames:=TStringList.Create;
ReadPrintersConfig;
printernames.Assign(Printer.Printers);

finalization

if assigned(configinifile) then
begin
 configinifile.free;
 configinifile:=nil;
end;
printernames.free;
printernames:=nil;

end.
