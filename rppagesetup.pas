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
  rpmetafile;

type
  TFPageSetup = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    RPageSize: TRadioGroup;
    GPageSize: TGroupBox;
    Label1: TLabel;
    EWidth: TEdit;
    EHeight: TEdit;
    Label2: TLabel;
    LMetrics1: TLabel;
    LMetrics2: TLabel;
    RPageOrientation: TRadioGroup;
    RCustomOrientation: TRadioGroup;
    ColorDialog1: TColorDialog;
    SColor: TShape;
    BBackground: TBitBtn;
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
    oldwidth,oldheight:string;
    oldleftmargin,oldtopmargin,oldrightmargin,oldbottommargin:string;
    procedure SaveOptions;
    procedure ReadOptions;
  public
    { Public declarations }
  end;


procedure ExecutePageSetup(report:TRpReport);

implementation

{$R *.xfm}



procedure ExecutePageSetup(report:TRpReport);
var
 dia:TFPageSetup;
begin
 dia:=TFPageSetup.Create(Application);
 try
  dia.report:=report;

  dia.ShowModal;
 finally
  dia.free;
 end;
end;

procedure TFPageSetup.BCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TFPageSetup.FormCreate(Sender: TObject);
var
 i:integer;
begin
 LMetrics1.Caption:=rpunitlabels[defaultunit];
 LMetrics2.Caption:=LMetrics1.Caption;
 for i:=0 to MAX_LANGUAGES-1 do
 begin
  ComboLanguage.Items.Add(rplangdesc[i]);
 end;
end;

procedure TFPageSetup.BOKClick(Sender: TObject);
begin
 SaveOptions;
 close;
end;

procedure TFPageSetup.SaveOptions;
begin
 // Saves the options to report
 if RPageSize.itemindex=0 then
 begin
  report.Pagesize:=rpPageSizeDefault;
 end
 else
 begin
  report.Pagesize:=rpPageSizeCustom;
  if EHeight.Text<>oldheight then
   report.PageHeight:=gettwipsfromtext(EHeight.Text);
  if EWidth.Text<>oldwidth then
   report.PageWidth:=gettwipsfromtext(EWidth.Text);
 end;
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
end;

procedure TFPageSetup.ReadOptions;
begin
 // ReadOptions
 // Size
 GPageSize.Visible:=false;
 RPageSize.ItemIndex:=0;
 EWidth.Text:=gettextfromtwips(report.PageWidth);
 EHeight.Text:=gettextfromtwips(report.PageHeight);
 ELeftMargin.Text:=gettextfromtwips(report.LeftMargin);
 ERightMargin.Text:=gettextfromtwips(report.RightMargin);
 ETopMargin.Text:=gettextfromtwips(report.TopMargin);
 EBottomMargin.Text:=gettextfromtwips(report.BottomMargin);
 oldwidth:=EWidth.Text;
 oldheight:=EHeight.Text;
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
 if report.Language<ComboLanguage.Items.Count then
  ComboLanguage.ItemIndex:=report.Language;
end;

procedure TFPageSetup.SColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 BBackgroundClick(Self);
end;

procedure TFPageSetup.BBackgroundClick(Sender: TObject);
begin
 if ColorDialog1.Execute then
  SColor.Brush.Color:=ColorDialog1.Color;
end;

procedure TFPageSetup.RPageSizeClick(Sender: TObject);
begin
 GPageSize.Visible:=RPageSize.Itemindex=1;
end;

procedure TFPageSetup.RPageOrientationClick(Sender: TObject);
begin
 RCustomOrientation.Visible:=RPageOrientation.Itemindex=1;
end;

procedure TFPageSetup.FormShow(Sender: TObject);
begin
 ReadOptions;
end;

end.
