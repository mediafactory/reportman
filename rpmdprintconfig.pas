unit rpmdprintconfig;

interface

uses SysUtils, Classes, QGraphics, QForms, QControls, QStdCtrls,
  QButtons, QExtCtrls,rpmdconsts,IniFiles,rpmdshfolder,QPrinters,
  rptypes;

type
  TFRpPrinterConfig = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure LSelPrinterClick(Sender: TObject);
    procedure RadioUserClick(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure CheckPrinterFontsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboPrintersChange(Sender: TObject);
  private
    { Private declarations }
    procedure DoSave;
  public
    { Public declarations }
  end;

procedure ShowPrintersConfiguration;

implementation

{$R *.xfm}

var
 configfilename:string;
 userconfigfilename:string;
 systemconfigfilename:string;
 configinifile:TMemInifile;
 printernames:TStringList;
 userconfig:boolean;

procedure ShowPrintersConfiguration;
var
 dia:TFRpPrinterConfig;
begin
 dia:=TFRpPrinterConfig.Create(Application);
 try
  dia.showmodal;
 finally
  dia.free;
 end;
end;

procedure TFRpPrinterConfig.FormCreate(Sender: TObject);
begin
 BOK.Caption:=TranslateStr(93,BOK.Caption);
 BCancel.Caption:=TranslateStr(94,BCancel.Caption);
 CheckPrinterFonts.Caption:=TranslateStr(113,CheckPrinterFonts.Caption);
 LSelectPrinter.Caption:=TranslateStr(741,LSelectPrinter.Caption);
 GConfigFile.Caption:=TranslateStr(743,GConfigFile.Caption);
 RadioUser.Caption:=TranslateStr(744,RadioUser.Caption);
 RadioSystem.Caption:=TranslateStr(745,RadioSystem.Caption);
 Caption:=TranslateStr(742,Caption);
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
 SetInitialBounds;
end;

procedure TFRpPrinterConfig.LSelPrinterClick(Sender: TObject);
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

procedure TFRpPrinterConfig.RadioUserClick(Sender: TObject);
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

procedure TFRpPrinterConfig.BOKClick(Sender: TObject);
begin
 DoSave;
 Close;
end;

procedure TFRpPrinterConfig.DoSave;
begin
 if configinifile.FileName=EConfigFile.TExt then
  configinifile.UpdateFile
 else
 begin
  configinifile.Rename(EConfigFile.Text,false);
  configinifile.UpdateFile
 end;
end;

procedure TFRpPrinterConfig.CheckPrinterFontsClick(Sender: TObject);
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

procedure TFRpPrinterConfig.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 ReadPrintersConfig;
 printerconfigfile.free;
 printerconfigfile:=nil;
end;


procedure TFRpPrinterConfig.ComboPrintersChange(Sender: TObject);
begin
 if LSelPrinter.ItemIndex<=0 then
  exit;
 configinifile.WriteString('PrinterNames','Printer'+IntToStr(LSelPrinter.ItemIndex),ComboPrinters.Text);
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
