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
  rpmdconsts,rpreport,rpmdfconnectionvcl, ExtCtrls,rpmdfdatasetsvcl;

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
    PBottom3: TPanel;
    BCancel: TButton;
    BNext: TButton;
    BFinish: TButton;
    BBack: TButton;
    procedure BNext1Click(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure BFinishClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BBackClick(Sender: TObject);
    procedure PControlChange(Sender: TObject);
  private
    { Private declarations }
    report:TRpReport;
    Created:Boolean;
    conframe:TFRpConnectionVCL;
    dataframe:TFRpDatasetsVCL;
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
 if PControl.ActivePageIndex<PControl.PageCount-1 then
 begin
  PControl.ActivePageIndex:=PControl.ActivePageIndex+1;
  PControlChange(PControl);
 end;
end;

procedure TFRpWizardVCL.BCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TFRpWizardVCL.BFinishClick(Sender: TObject);
begin
 // Creates a new report
 report.CreateNew;
 report.Datainfo:=dataframe.Datainfo;
 Created:=True;
 Close;
end;

procedure TFRpWizardVCL.FormCreate(Sender: TObject);
begin
 // Load strings
 Caption:=TranslateStr(935,Caption);
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
 BNext.Caption:=TranslateStr(932,BNext.Caption);
 BBack.Caption:=TranslateStr(933,BBack.Caption);
 BFinish.Caption:=TranslateStr(934,BFinish.Caption);

 // Create the connections frame
 conframe:=TFRpConnectionVCL.Create(Self);
 conframe.Parent:=TabConnections;
 dataframe:=TFRpDatasetsVCL.Create(Self);
 dataframe.Parent:=TabDatasets;
 PControl.ActivePage:=TabInstructions;
 PControlChange(PControl);
end;

procedure TFRpWizardVCL.BBackClick(Sender: TObject);
begin
 if PControl.ActivePageIndex>0 then
 begin
  PControl.ActivePageIndex:=PControl.ActivePageIndex-1;
  PControlChange(PControl);
 end;
end;

procedure TFRpWizardVCL.PControlChange(Sender: TObject);
begin
 BBack.Enabled:=PControl.ActivePageIndex>0;
 BNext.Enabled:=PControl.ActivePageIndex<PControl.PageCount-1;
 BFinish.Enabled:=PControl.ActivePageIndex=PControl.PageCount-1;
 if PControl.ActivePage=TabDatasets then
 begin
  dataframe.Databaseinfo:=conframe.databaseinfo;
  // Gets the datasets
  dataframe.FillDatasets;
 end;
end;

end.
