{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       freportgroup                                    }
{       Group properties and wizard to add groups       }
{       included in a subreport                         }
{                                                       }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}

unit freportgroup;

interface

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,rpconsts,
  rpsubreport;

type
  TFGroup = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    EGroupName: TEdit;
    MExpression: TMemo;
    GGroupChange: TRadioGroup;
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    dook:boolean;
  public
    { Public declarations }
  end;


function AddGroup(subreport:TRpSubreport):boolean;

implementation

{$R *.xfm}

function AddGroup(subreport:TRpSubreport):boolean;
var
 dia:TFGroup;
begin
 Assert(subreport<>nil,'Called AddGroup with subreport unassigned');

 Result:=false;
 dia:=TFGroup.Create(Application);
 try
  dia.ShowModal;
  if dia.dook then
  begin
   subreport.AddGroup(Trim(dia.EGroupname.Text));
   Result:=true;
  end;
 finally
  dia.free;
 end;
end;

procedure TFGroup.OKBtnClick(Sender: TObject);
begin
 if Length(TRim(EGroupName.Text))<1 then
 begin
  Raise Exception.Create(SRpGroupNameRequired);
 end;
 dook:=true;
 Close;
end;

procedure TFGroup.FormCreate(Sender: TObject);
begin
 GGroupChange.Itemindex:=0;
end;

end.
