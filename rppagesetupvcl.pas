{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpmpagesetup                                    }
{       Page setup dialog for a report                  }
{       avaliable in runtime and design time            }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2003 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rppagesetupvcl;

interface

{$I rpconf.inc}

uses
  SysUtils,
{$IFDEF USEVARIANTS}
  Types,
{$ENDIF}
  Classes,rpmunits,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls,rpreport, ExtCtrls,Buttons,Printers,
  rptypes,
  rpmetafile,rpmdconsts,rpmdprintconfigvcl, ComCtrls, Mask, rpmaskedit;

type
  TFRpPageSetupVCL = class(TForm)
    PControl: TPageControl;
    TabPage: TTabSheet;
    TabPrint: TTabSheet;
    Panel1: TPanel;
    BOK: TButton;
    BCancel: TButton;
    SColor: TShape;
    RPageSize: TRadioGroup;
    GPageSize: TGroupBox;
    ComboPageSize: TComboBox;
    RPageOrientation: TRadioGroup;
    RCustomOrientation: TRadioGroup;
    BBackground: TButton;
    GPageMargins: TGroupBox;
    LLeft: TLabel;
    LTop: TLabel;
    LMetrics3: TLabel;
    LMetrics4: TLabel;
    LMetrics5: TLabel;
    LRight: TLabel;
    LBottom: TLabel;
    LMetrics6: TLabel;
    ELeftMargin: TRpMaskEdit;
    ETopMargin: TRpMaskEdit;
    ERightMargin: TRpMaskEdit;
    EBottomMargin: TRpMaskEdit;
    GUserDefined: TGroupBox;
    LMetrics7: TLabel;
    LMetrics8: TLabel;
    LWidth: TLabel;
    LHeight: TLabel;
    EPageheight: TRpMaskEdit;
    EPageWidth: TRpMaskEdit;
    ColorDialog1: TColorDialog;
    LSelectPrinter: TLabel;
    ComboSelPrinter: TComboBox;
    BConfigure: TButton;
    CheckPrintOnlyIfData: TCheckBox;
    CheckTwoPass: TCheckBox;
    LCopies: TLabel;
    ECopies: TRpMaskEdit;
    CheckCollate: TCheckBox;
    LPrinterFonts: TLabel;
    ComboPrinterFonts: TComboBox;
    LRLang: TLabel;
    ComboLanguage: TComboBox;
    LPreview: TLabel;
    ComboPreview: TComboBox;
    ComboStyle: TComboBox;
    TabOptions: TTabSheet;
    LPreferedFormat: TLabel;
    ComboFormat: TComboBox;
    CheckDrawerAfter: TCheckBox;
    CheckDrawerBefore: TCheckBox;
    CheckPreviewAbout: TCheckBox;
    CheckMargins: TCheckBox;
    procedure BCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure SColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BBackgroundClick(Sender: TObject);
    procedure RPageSizeClick(Sender: TObject);
    procedure RPageOrientationClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BConfigureClick(Sender: TObject);
  private
    { Private declarations }
    report:TRpReport;
    oldleftmargin,oldtopmargin,oldrightmargin,oldbottommargin:string;
    oldcustompagewidth,oldcustompageheight:string;
    dook:boolean;
    procedure SaveOptions;
    procedure ReadOptions;
  public
    { Public declarations }
  end;


function ExecutePageSetup(report:TRpReport):boolean;

implementation

{$R *.dfm}



type
  TPageWidthHeight = record
    Width: Integer;
    Height: Integer;
  end;


 // Qt Page sizes
  TPageSize = (psA4, psB5, psLetter, psLegal, psExecutive, psA0, psA1, psA2,
    psA3, psA5, psA6, psA7, psA8, psA9, psB0, psB1, psB10, psB2, psB3, psB4, psB6,
    psB7, psB8, psB9, psC5E, psComm10E, psDLE, psFolio, psLedger, psTabloid, psNPageSize);

const PageSizeNames: array [psA4..psNPageSize] of widestring =
('A4', 'B5','Letter','Legal','Executive','A0', 'A1', 'A2',
    'A3', 'A5', 'A6', 'A7', 'A8', 'A9', 'B0', 'B1', 'B10', 'B2',
     'B3', 'B4', 'B6','B7', 'B8', 'B9', 'C5E', 'Comm10E',
     'DLE', 'Folio', 'Ledger', 'Tabloid', 'psNPageSize');

const
  PageSizeArray: array[0..30] of TPageWidthHeight =
    (
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




function ExecutePageSetup(report:TRpReport):boolean;
var
 dia:TFRpPageSetupVCL;
begin
 dia:=TFRpPageSetupVCL.Create(Application);
 try
  dia.report:=report;
  dia.ShowModal;
  Result:=dia.dook;
 finally
  dia.free;
 end;
end;

procedure TFRpPageSetupVCL.BCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TFRpPageSetupVCL.FormCreate(Sender: TObject);
var
 psize:TPagesize;
 astring:widestring;
 awidth:integer;
 aheight:integer;
begin
 PControl.ActivePage:=TabPage;
 LMetrics3.Caption:=rpunitlabels[defaultunit];
 LMetrics4.Caption:=LMetrics3.Caption;
 LMetrics5.Caption:=LMetrics3.Caption;
 LMetrics6.Caption:=LMetrics3.Caption;
 LMetrics7.Caption:=LMetrics3.Caption;
 LMetrics8.Caption:=LMetrics3.Caption;
 GetLanguageDescriptions(ComboLanguage.Items);
 ComboLanguage.Items.Insert(0,TranslateStr(95,'Default'));
 for psize:=Low(psize) to High(psize) do
 begin
  astring:=PageSizeNames[psize];
  awidth:=Round(PageSizeArray[Integer(psize)].Width/1000*TWIPS_PER_INCHESS);
  aheight:=Round(PageSizeArray[Integer(psize)].Height/1000*TWIPS_PER_INCHESS);
  astring:=astring+' ('+gettextfromtwips(awidth)+'x'+
   gettextfromtwips(aheight)+') '+rpunitlabels[defaultunit];
  ComboPageSize.Items.Add(astring);
 end;
 BOK.Caption:=TranslateStr(93,BOK.Caption);
 BCancel.Caption:=TranslateStr(94,BCancel.Caption);
 RPageSize.Items.Strings[0]:=TranslateStr(95,RPageSize.Items.Strings[0]);
 RPageSize.Items.Strings[1]:=TranslateStr(96,RPageSize.Items.Strings[1]);
 RPageSize.Items.Strings[2]:=TranslateStr(732,RPageSize.Items.Strings[2]);
 RPageOrientation.Items.Strings[0]:=TranslateStr(95,RPageOrientation.Items.Strings[0]);
 RPageOrientation.Items.Strings[1]:=TranslateStr(96,RPageOrientation.Items.Strings[1]);
 RPageSize.Caption:=TranslateStr(97,RPageSize.Caption);
 GPageSize.Caption:=TranslateStr(104,GPageSize.Caption);
 GUserDefined.Caption:=TranslateStr(733,GPageSize.Caption);
 LWidth.Caption:=SRpSWidth;
 LHeight.Caption:=SRpSHeight;
 RPageOrientation.Caption:=TranslateStr(98,RPageOrientation.Caption);
 GPageMargins.Caption:=TranslateStr(99,GPagemargins.Caption);
 LLeft.Caption:=TranslateStr(100,LLeft.Caption);
 LRight.Caption:=TranslateStr(101,LRight.Caption);
 LTop.Caption:=TranslateStr(102,LTop.Caption);
 LBottom.Caption:=TranslateStr(103,LBottom.Caption);
 RCustomOrientation.Caption:=TranslateStr(105,RCustomOrientation.Caption);
 RCustomOrientation.Items.Strings[0]:=TranslateStr(106,RCustomOrientation.Items.Strings[0]);
 RCustomOrientation.Items.Strings[1]:=TranslateStr(107,RCustomOrientation.Items.Strings[1]);
 LCopies.Caption:=TranslateStr(108,LCopies.Caption);
 CheckCollate.Caption:=TranslateStr(109,CheckCollate.Caption);
 Caption:=TranslateStr(110,Caption);
 CheckTwoPass.Caption:=TranslateStr(111,CheckTwoPass.Caption);
 CheckPrintOnlyIfData.Caption:=TranslateStr(800,CheckPrintOnlyIfData.Caption);
 LRLang.Caption:=TranslateStr(112,LRLang.Caption);
 LPrinterFonts.Caption:=TranslateStr(113,LPrinterFonts.Caption);
 ComboPrinterFonts.Items.Strings[0]:=TranslateStr(95,ComboPrinterFonts.Items.Strings[0]);
 ComboPrinterFonts.Items.Strings[1]:=TranslateStr(114,ComboPrinterFonts.Items.Strings[1]);
 ComboPrinterFonts.Items.Strings[2]:=TranslateStr(115,ComboPrinterFonts.Items.Strings[2]);
 BBAckground.Caption:=TranslateStr(116,BBAckground.Caption);
 CheckPreviewAbout.Caption:=SRpAboutBoxPreview;
 with ComboSelPrinter.Items do
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
 LSelectPrinter.Caption:=TranslateStr(741,LSelectPrinter.Caption);
 BConfigure.Caption:=TranslateStr(143,BConfigure.Caption);
 LPreview.Caption:=TranslateStr(840,LPreview.Caption);
 ComboPreview.Items.Strings[0]:=TranslateStr(841,ComboPreview.Items.Strings[0]);
 ComboPreview.Items.Strings[1]:=TranslateStr(842,ComboPreview.Items.Strings[1]);
 ComboStyle.Items.Strings[0]:=TranslateStr(843,ComboStyle.Items.Strings[0]);
 ComboStyle.Items.Strings[1]:=TranslateStr(844,ComboStyle.Items.Strings[1]);
 ComboStyle.Items.Strings[2]:=TranslateStr(845,ComboStyle.Items.Strings[2]);
 CheckMargins.Caption:=SRpPreviewMargins;
 TabPage.Caption:=TranslateStr(857,TabPage.Caption);
 TabPrint.Caption:=TranslateStr(858,TabPrint.Caption);
 TabOptions.Caption:=SRpSOptions;
 LPreferedFormat.Caption:=SRpPreferedFormat;
 ComboFormat.Items.Add(SRpStreamZLib);
 ComboFormat.Items.Add(SRpStreamText);
 ComboFormat.Items.Add(SRpStreamBinary);
 CheckDrawerAfter.Caption:=SRpOpenDrawerAfter;
 CheckDrawerBefore.Caption:=SRpOpenDrawerBefore;
end;

procedure TFRpPageSetupVCL.BOKClick(Sender: TObject);
begin
 SaveOptions;
 close;
end;

procedure TFRpPageSetupVCL.SaveOptions;
var
 acopies:integer;
 FReportAction:TRpReportActions;
begin
 acopies:=StrToInt(ECopies.Text);
 if acopies<=0 then
  acopies:=1;
 report.Copies:=acopies;
 report.CollateCopies:=CheckCollate.Checked;
 report.TwoPass:=CheckTwoPass.Checked;
 report.PreviewAbout:=CheckPreviewAbout.Checked;
 report.PrintOnlyIfDataAvailable:=CheckPrintOnlyIfData.Checked;
 FReportAction:=[];
 if CheckDrawerAfter.Checked then
  include(FreportAction,rpDrawerAfter);
 if CheckDrawerBefore.Checked then
  include(FreportAction,rpDrawerBefore);
 report.ReportAction:=FReportAction;
 // Saves the options to report
 report.Pagesize:=TRpPageSize(RPageSize.ItemIndex);
  // Assigns the with and height in twips
 report.PagesizeQt:=ComboPageSize.ItemIndex;
 report.PageHeight:=Round(PageSizeArray[report.PageSizeQt].Height*1000/TWIPS_PER_INCHESS);
 report.PageWidth:=Round(PageSizeArray[report.PageSizeQt].Width*1000/TWIPS_PER_INCHESS);
 if EPageWidth.Text<>oldcustompagewidth then
  report.CustomPageWidth:=gettwipsfromtext(EPageWidth.Text);
 if EPageHeight.Text<>oldcustompageheight then
  report.CustomPageHeight:=gettwipsfromtext(EPageHeight.Text);
 if ELeftMargin.Text<>oldleftmargin then
  report.LeftMargin:=gettwipsfromtext(ELeftMargin.Text);
 if ERightMargin.Text<>oldrightmargin then
  report.RightMargin:=gettwipsfromtext(ERightMargin.Text);
 if ETopMargin.Text<>oldtopmargin then
  report.TopMargin:=gettwipsfromtext(ETopMargin.Text);
 if EBottomMargin.Text<>oldbottommargin then
  report.BottomMargin:=gettwipsfromtext(EBottomMargin.Text);
 report.PageOrientation:=rpOrientationDefault;
 report.PrinterSelect:=TRpPrinterSelect(ComboSelPrinter.ItemIndex);
 if RPageOrientation.itemindex=1 then
 begin
  if RCustomOrientation.itemindex=0 then
   report.PageOrientation:=rpOrientationPortrait
  else
   report.PageOrientation:=rpOrientationLandscape;
 end;
 report.PageBackColor:=SColor.Brush.Color;
 // Language
 report.Language:=ComboLanguage.ItemIndex-1;
 // Other
 report.PrinterFonts:=TRpPrinterFontsOption(ComboPrinterFonts.ItemIndex);
 report.PreviewStyle:=TRpPreviewStyle(ComboStyle.ItemIndex);
 report.PreviewMargins:=CheckMargins.Checked;
 report.PreviewWindow:=TRpPreviewWindowStyle(ComboPreview.ItemIndex);
 report.StreamFormat:=TRpStreamFormat(ComboFormat.ItemIndex);

 dook:=true;
end;

procedure TFRpPageSetupVCL.ReadOptions;
begin
 // ReadOptions
 ECopies.Text:=IntToStr(report.Copies);
 CheckCollate.Checked:=report.CollateCopies;
 CheckTwoPass.Checked:=report.TwoPass;
 CheckPrintOnlyIfData.Checked:=report.PrintOnlyIfDataAvailable;
 CheckDrawerBefore.Checked:=rpDrawerBefore in report.ReportAction;
 CheckDrawerAfter.Checked:=rpDrawerAfter in report.ReportAction;
 CheckPreviewAbout.Checked:=report.PreviewAbout;

 // Size
 ComboPageSize.ItemIndex:=report.PagesizeQt;
 GPageSize.Visible:=false;
 RPageSize.ItemIndex:=0;
 ELeftMargin.Text:=gettextfromtwips(report.LeftMargin);
 ERightMargin.Text:=gettextfromtwips(report.RightMargin);
 ETopMargin.Text:=gettextfromtwips(report.TopMargin);
 EBottomMargin.Text:=gettextfromtwips(report.BottomMargin);
 EPageWidth.Text:=gettextfromtwips(report.CustomPageWidth);
 EPageHeight.Text:=gettextfromtwips(report.CustomPageheight);
 oldcustompagewidth:=EPageWidth.Text;
 oldcustompageheight:=EPageheight.Text;
 oldleftmargin:=ELeftMargin.Text;
 oldrightmargin:=ERightMargin.Text;
 oldTopmargin:=ETopMargin.Text;
 oldBottommargin:=EBottomMargin.Text;

 RPageSize.ItemIndex:=integer(report.Pagesize);
 RPageSizeClick(Self);
 // Orientation
 RPageOrientation.Itemindex:=0;
 RCustomOrientation.Itemindex:=0;
 RCustomOrientation.Visible:=false;
 if report.PageOrientation>rpOrientationdefault then
 begin
  RCustomOrientation.Visible:=true;
  RPageOrientation.itemindex:=1;
 end;
 if report.PageOrientation=rpOrientationPortrait then
  RCustomOrientation.Itemindex:=0;
 if report.PageOrientation=rpOrientationLandscape then
  RCustomOrientation.Itemindex:=1;
 ComboSelPrinter.ItemIndex:=integer(report.PrinterSelect);
 // Color
 SColor.Brush.Color:=TColor(report.PageBackColor);
 // Language
 ComboLanguage.ItemIndex:=0;
 ComboPrinterFonts.ItemIndex:=integer(report.PrinterFonts);
 if (report.Language+1)<ComboLanguage.Items.Count then
  ComboLanguage.ItemIndex:=report.Language+1;
 ComboStyle.ItemIndex:=integer(report.PreviewStyle);
 ComboPreview.ItemIndex:=integer(report.PreviewWindow);
 ComboFormat.ItemIndex:=integer(report.StreamFormat);
 CheckMargins.Checked:=report.PreviewMargins;
end;

procedure TFRpPageSetupVCL.SColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 BBackgroundClick(Self);
end;

procedure TFRpPageSetupVCL.BBackgroundClick(Sender: TObject);
begin
 if ColorDialog1.Execute then
  SColor.Brush.Color:=ColorDialog1.Color;
end;

procedure TFRpPageSetupVCL.RPageSizeClick(Sender: TObject);
begin
 GPageSize.Visible:=RPageSize.Itemindex=1;
 GUserDefined.Visible:=RPageSize.Itemindex=2;
end;

procedure TFRpPageSetupVCL.RPageOrientationClick(Sender: TObject);
begin
 RCustomOrientation.Visible:=RPageOrientation.Itemindex=1;
end;

procedure TFRpPageSetupVCL.FormShow(Sender: TObject);
begin
 ReadOptions;
end;

procedure TFRpPageSetupVCL.BConfigureClick(Sender: TObject);
begin
 ShowPrintersConfiguration;
end;



end.
