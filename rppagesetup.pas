{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpmpagesetup                                    }
{       Page setup dialog for a report                  }
{       avaliable in runtime and design time            }
{                                                       }
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

unit rppagesetup;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls,rpreport, QExtCtrls,rpmunits, QButtons,rptypes,
  rpmetafile,QPrinters;

type
  TFRpPageSetup = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    RPageSize: TRadioGroup;
    GPageSize: TGroupBox;
    RPageOrientation: TRadioGroup;
    RCustomOrientation: TRadioGroup;
    ColorDialog1: TColorDialog;
    SColor: TShape;
    BBackground: TButton;
    Label3: TLabel;
    ComboLanguage: TComboBox;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    ELeftMargin: TEdit;
    ETopMargin: TEdit;
    Label5: TLabel;
    LMetrics3: TLabel;
    LMetrics4: TLabel;
    LMetrics5: TLabel;
    ERightMargin: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    EBottomMargin: TEdit;
    LMetrics6: TLabel;
    Label1: TLabel;
    ComboPageSize: TComboBox;
    Label2: TLabel;
    ECopies: TEdit;
    CheckCollate: TCheckBox;
    CheckTwoPass: TCheckBox;
    Label6: TLabel;
    ComboPrinterFonts: TComboBox;
    procedure BCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure SColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BBackgroundClick(Sender: TObject);
    procedure RPageSizeClick(Sender: TObject);
    procedure RPageOrientationClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    report:TRpReport;
    oldleftmargin,oldtopmargin,oldrightmargin,oldbottommargin:string;
    procedure SaveOptions;
    procedure ReadOptions;
  public
    { Public declarations }
  end;


procedure ExecutePageSetup(report:TRpReport);

implementation

{$R *.xfm}

type
  TPageWidthHeight = record
    Width: Integer;
    Height: Integer;
  end;


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




procedure ExecutePageSetup(report:TRpReport);
var
 dia:TFRpPageSetup;
begin
 dia:=TFRpPageSetup.Create(Application);
 try
  dia.report:=report;

  dia.ShowModal;
 finally
  dia.free;
 end;
end;

procedure TFRpPageSetup.BCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TFRpPageSetup.FormCreate(Sender: TObject);
var
 i:integer;
 psize:TPagesize;
 astring:widestring;
 awidth:integer;
 aheight:integer;
begin
 LMetrics3.Caption:=rpunitlabels[defaultunit];
 LMetrics4.Caption:=LMetrics3.Caption;
 LMetrics5.Caption:=LMetrics3.Caption;
 LMetrics6.Caption:=LMetrics3.Caption;
 for i:=0 to MAX_LANGUAGES-1 do
 begin
  ComboLanguage.Items.Add(rplangdesc[i]);
 end;
 for psize:=Low(psize) to High(psize) do
 begin
  astring:=PageSizeNames[psize];
  awidth:=Round(PageSizeArray[Integer(psize)].Width/1000*TWIPS_PER_INCHESS);
  aheight:=Round(PageSizeArray[Integer(psize)].Height/1000*TWIPS_PER_INCHESS);
  astring:=astring+' ('+gettextfromtwips(awidth)+'x'+
   gettextfromtwips(aheight)+') '+rpunitlabels[defaultunit];
  ComboPageSize.Items.Add(astring);
 end;
end;

procedure TFRpPageSetup.BOKClick(Sender: TObject);
begin
 SaveOptions;
 close;
end;

procedure TFRpPageSetup.SaveOptions;
var
 acopies:integer;
begin
 acopies:=StrToInt(ECopies.Text);
 if acopies<=0 then
  acopies:=1;
 report.Copies:=acopies;
 report.CollateCopies:=CheckCollate.Checked;
 report.TwoPass:=CheckTwoPass.Checked;

 // Saves the options to report
 if RPageSize.itemindex=0 then
 begin
  report.Pagesize:=rpPageSizeDefault;
 end
 else
 begin
  report.Pagesize:=rpPageSizeCustom;
  // Assigns the with and height in twips
  report.PagesizeQt:=ComboPageSize.ItemIndex;
  report.PageHeight:=Round(PageSizeArray[report.PageSizeQt].Height*1000/TWIPS_PER_INCHESS);
  report.PageWidth:=Round(PageSizeArray[report.PageSizeQt].Width*1000/TWIPS_PER_INCHESS);
 end;
 report.PagesizeQt:=ComboPageSize.ItemIndex;
 if ELeftMargin.Text<>oldleftmargin then
  report.LeftMargin:=gettwipsfromtext(ELeftMargin.Text);
 if ERightMargin.Text<>oldrightmargin then
  report.RightMargin:=gettwipsfromtext(ERightMargin.Text);
 if ETopMargin.Text<>oldtopmargin then
  report.TopMargin:=gettwipsfromtext(ETopMargin.Text);
 if EBottomMargin.Text<>oldbottommargin then
  report.BottomMargin:=gettwipsfromtext(EBottomMargin.Text);
 report.PageOrientation:=rpOrientationDefault;
 if RPageOrientation.itemindex=1 then
 begin
  if RCustomOrientation.itemindex=0 then
   report.PageOrientation:=rpOrientationPortrait
  else
   report.PageOrientation:=rpOrientationLandscape;
 end;
 report.PageBackColor:=SColor.Brush.Color;
 // Language
 report.Language:=ComboLanguage.ItemIndex;
 // Other
 report.PrinterFonts:=TRpPrinterFontsOption(ComboPrinterFonts.ItemIndex);
end;

procedure TFRpPageSetup.ReadOptions;
begin
 // ReadOptions
 ECopies.Text:=IntToStr(report.Copies);
 CheckCollate.Checked:=report.CollateCopies;
 CheckTwoPass.Checked:=report.TwoPass;

 // Size
 ComboPageSize.ItemIndex:=report.PagesizeQt;
 GPageSize.Visible:=false;
 RPageSize.ItemIndex:=0;
 ELeftMargin.Text:=gettextfromtwips(report.LeftMargin);
 ERightMargin.Text:=gettextfromtwips(report.RightMargin);
 ETopMargin.Text:=gettextfromtwips(report.TopMargin);
 EBottomMargin.Text:=gettextfromtwips(report.BottomMargin);
 oldleftmargin:=ELeftMargin.Text;
 oldrightmargin:=ERightMargin.Text;
 oldTopmargin:=ETopMargin.Text;
 oldBottommargin:=EBottomMargin.Text;

 if report.Pagesize=RpPageSizeCustom then
 begin
  GPageSize.visible:=true;
  RPageSize.ItemIndex:=1;
 end;
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
 // Color
 SColor.Brush.Color:=TColor(report.PageBackColor);
 // Language
 ComboLanguage.ItemIndex:=0;
 ComboPrinterFonts.ItemIndex:=integer(report.PrinterFonts);
 if report.Language<ComboLanguage.Items.Count then
  ComboLanguage.ItemIndex:=report.Language;
end;

procedure TFRpPageSetup.SColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 BBackgroundClick(Self);
end;

procedure TFRpPageSetup.BBackgroundClick(Sender: TObject);
begin
 if ColorDialog1.Execute then
  SColor.Brush.Color:=ColorDialog1.Color;
end;

procedure TFRpPageSetup.RPageSizeClick(Sender: TObject);
begin
 GPageSize.Visible:=RPageSize.Itemindex=1;
end;

procedure TFRpPageSetup.RPageOrientationClick(Sender: TObject);
begin
 RCustomOrientation.Visible:=RPageOrientation.Itemindex=1;
end;

procedure TFRpPageSetup.FormShow(Sender: TObject);
begin
 ReadOptions;
end;

end.
