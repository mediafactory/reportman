{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdfwizardvcl                                  }
{                                                       }
{       Wizard to create new reports                    }
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

unit rpmdfwizardvcl;

interface

uses
  Windows, Messages, SysUtils,
{$IFDEF USEVARIANTS}
  Variants,
{$ENDIF}
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  rpmdconsts,rpreport,rpmdfconnectionvcl, ExtCtrls;

type
  TFRpWizardVCL = class(TForm)
    PControl: TPageControl;
    TabInstructions: TTabSheet;
    LDesign: TLabel;
    LPass1: TLabel;
    LPass2: TLabel;
    LPass3: TLabel;
    LPass4: TLabel;
    LBegin: TLabel;
    TabConnections: TTabSheet;
    TabDatasets: TTabSheet;
    TabFields: TTabSheet;
    TabReportType: TTabSheet;
    BNExt3: TButton;
    BNext4: TButton;
    BFinish: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    PBottom2: TPanel;
    BCancel: TButton;
    BNext1: TButton;
    PBottom3: TPanel;
    Button1: TButton;
    BNext2: TButton;
    procedure BNext1Click(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure BFinishClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    report:TRpReport;
    Created:Boolean;
    conframe:TFRpConnectionVCL;
  public
    { Public declarations }
  end;

function NewReportWizard(report:TRpReport):Boolean;

implementation

{$R *.dfm}


function NewReportWizard(report:TRpReport):boolean;
var
 dia:TFRpWizardVCL;
begin
 dia:=TFRpWizardVCL.Create(Application);
 try
  dia.conframe.Databaseinfo:=report.DatabaseInfo;
  dia.report:=report;
  dia.ShowModal;
  Result:=dia.Created;
 finally
  dia.free;
 end;
end;

procedure TFRpWizardVCL.BNext1Click(Sender: TObject);
begin
 PControl.ActivePageIndex:=PControl.ActivePageIndex+1;
end;

procedure TFRpWizardVCL.BCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TFRpWizardVCL.BFinishClick(Sender: TObject);
begin
 // Creates a new report
 report.CreateNew;

 Created:=True;
 Close;
end;

procedure TFRpWizardVCL.FormCreate(Sender: TObject);
begin
 // Load strings
 LDesign.Caption:=TranslateStr(869,LDesign.Caption);
 LPass1.Caption:=TranslateStr(870,LPass1.Caption);
 LPass2.Caption:=TranslateStr(871,LPass2.Caption);
 LPass3.Caption:=TranslateStr(872,LPass3.Caption);
 LPass4.Caption:=TranslateStr(873,LPass4.Caption);
 LBegin.Caption:=TranslateStr(874,LBegin.Caption);
 TabInstructions.Caption:=TranslateStr(875,TabInstructions.Caption);
 TabConnections.Caption:=TranslateStr(142,TabConnections.Caption);
 TabDatasets.Caption:=TranslateStr(876,TabDatasets.Caption);
 TabFields.Caption:=TranslateStr(877,TabFields.Caption);
 TabReportType.Caption:=TranslateStr(878,TabReportType.Caption);

 // Create the connections frame
 conframe:=TFRpConnectionVCL.Create(Self);
 conframe.Parent:=TabConnections;
 PControl.ActivePage:=TabInstructions;
end;

end.
