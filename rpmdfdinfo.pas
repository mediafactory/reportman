{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpmdfdinfo                                      }
{       : Printer driver for  VCL Lib                   }
{       can be used only for windows                    }
{       it includes printer and bitmap support          }
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
unit rpmdfdinfo;

interface

{$I rpconf.inc}

uses
 Classes,sysutils,QDialogs,QControls,QGraphics,QForms,rpmdconsts,
{$IFDEF USEVARIANTS}
 types,
{$ENDIF}
 rptypes,rpdatainfo,rpreport,
 rpmdfdatasets,rpmdfconnection,
 QStdCtrls, QExtCtrls,QComCtrls;

type
  TFRpDInfo = class(TForm)
    PBottom: TPanel;
    BOk: TButton;
    BCancel: TButton;
    PControl: TPageControl;
    TabConnections: TTabSheet;
    TabDatasets: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure PControlChange(Sender: TObject);
  private
    { Private declarations }
    freport:TRpReport;
    fdatasets:TFRpDatasets;
    fconnections:TFRpConnection;
    procedure SetReport(value:TRpReport);
  public
    { Public declarations }
    property report:TRpReport read FReport write SetReport;
  end;


procedure ShowDataConfig(report:TRpReport);


implementation

uses rpdbxconfigvcl;


{$R *.xfm}

procedure TFRpDInfo.SetReport(value:TRpReport);
begin
 freport:=value;
 fconnections:=TFRpConnection.Create(Self);
 fconnections.Parent:=TabConnections;
 fdatasets:=TFRpDatasets.Create(Self);
 fdatasets.Parent:=TabDatasets;
 fdatasets.Datainfo:=report.DataInfo;
 fdatasets.Databaseinfo:=report.DatabaseInfo;
 fdatasets.params:=report.params;
 if report.DatabaseInfo.Count>0 then
  PControl.ActivePage:=TabDatasets
 else
  PControl.ActivePage:=TabConnections;
 fconnections.Databaseinfo:=report.DatabaseInfo;
end;

procedure ShowDataConfig(report:TRpReport);
var
 dia:TFRpDInfo;
begin
 UpdateConAdmin;

 dia:=TFRpDInfo.Create(Application);
 try
  dia.report:=report;
  dia.showmodal;
 finally
  dia.free;
 end;
end;



procedure TFRpDInfo.FormCreate(Sender: TObject);
begin
 BOK.Caption:=TranslateStr(93,BOK.Caption);
 BCancel.Caption:=TranslateStr(94,BCancel.Caption);
// Caption:=TranslateStr(259,Caption);
end;






procedure TFRpDInfo.PControlChange(Sender: TObject);
begin
 fdatasets.Databaseinfo:=report.DatabaseInfo;
end;

end.

