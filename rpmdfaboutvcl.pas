{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpmdfaboutvcl                                   }
{       About box for report manager designer           }
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
unit rpmdfaboutvcl;

interface

{$I rpconf.inc}

uses SysUtils, Classes,
  Graphics, Forms,
  Buttons, ExtCtrls, Controls, StdCtrls,Dialogs,
  rpmdconsts;

type
  TFRpAboutBoxVCL = class(TForm)
    BOK: TButton;
    LReport: TLabel;
    LAuthor: TLabel;
    LName: TLabel;
    Label2: TLabel;
    LEmail: TLabel;
    Label3: TLabel;
    Image1: TImage;
    LVersion: TLabel;
    LProject: TLabel;
    Label5: TLabel;
    Memo1: TMemo;
    LContributors: TLabel;
    Image2: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure ShowAbout;

implementation


{$R *.dfm}

procedure ShowAbout;
var dia:TFRpAboutBoxVCL;
begin
 dia:=TFRpAboutBoxVCL.Create(Application);
 try
  dia.ShowModal;
 finally
  dia.free;
 end;
end;



procedure TFRpAboutBoxVCL.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
 LReport.Font.Name:='Arial';
{$ENDIF}
 Caption:=TranslateStr(88,Caption);
 LAuthor.Caption:=TranslateStr(89,LAuthor.Caption);
 LProject.Caption:=TranslateStr(90,LProject.Caption);
 LContributors.Caption:=TranslateStr(92,LContributors.Caption);
 LVersion.Caption:=TranslateStr(91,'Version')+' '+LVersion.Caption;
 BOK.Caption:=TranslateStr(93,BOK.Caption);


 LReport.Font.Size:=20;
 LReport.Font.Style:=[fsBold];
 LName.Font.Style:=[fsBold];
 LVersion.Font.Size:=16;
 LEmail.Font.Style:=[fsBold];

end;

end.
