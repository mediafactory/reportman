{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpmdfopenlibvcl                                 }
{       Dialog for Report Library maintainance          }
{       add/delete reports                              }
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

unit rpmdfopenlibvcl;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls,rpmdconsts,rpmdftreevcl,DB,rpdatainfo;

type
  TFRpOpenLibVCL = class(TForm)
    Panel1: TPanel;
    BOK: TButton;
    BCancel: TButton;
    procedure BOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    dook:Boolean;
    atree:TFRpDBTreeVCL;
  public
    { Public declarations }
    SelectedReport:String;
  end;

function SelectReportFromLibrary(dbinfo:TRpDatabaseInfoItem):String;

implementation

{$R *.DFM}


function SelectReportFromLibrary(dbinfo:TRpDatabaseInfoItem):String;
var
 dia:TFRpOpenLibVCL;
begin
 Result:='';
 dia:=TFRpOpenLibVCL.Create(Application);
 try
  dia.atree.EditTree(dbinfo);
  dia.ShowModal;
  if dia.dook then
  begin
  end;
 finally
  dia.free;
 end;
end;

procedure TFRpOpenLibVCL.BOKClick(Sender: TObject);
begin
 // Is there a report selected?
 dook:=True;
 Close;
end;

procedure TFRpOpenLibVCL.FormCreate(Sender: TObject);
begin
 atree:=TFRpDBTreeVCL.Create(Self);
// atree.OnLoadReport:=Self.OnLoadReport;
 atree.Top:=0;
 atree.Left:=0;
 atree.Parent:=Self;
end;

end.
