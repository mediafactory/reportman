{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       fabour                                          }
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
unit fabout;

interface

uses SysUtils, Classes, QGraphics, QForms, 
{$IFNDEF PROFILE}  QButtons, QExtCtrls, QControls, QStdCtrls;{$ENDIF}
{$IFDEF PROFILE}  QButtons, QExtCtrls, QControls, QStdCtrls ,Proftimx;{$ENDIF}

type
  TFAboutBox = class(TForm)
    OKBtn: TButton;
    LReport: TLabel;
    Label1: TLabel;
    LName: TLabel;
    Label2: TLabel;
    LEmail: TLabel;
    Label3: TLabel;
    Image1: TImage;
    LVersion: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure ShowAbout;

implementation

procedure ShowAbout;
var dia:TFAboutBox;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,0; xor eax,eax; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 dia:=TFAboutBox.Create(Application);
 try
  dia.ShowModal;
 finally
  dia.free;
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,0; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


{$R *.xfm}

procedure TFAboutBox.FormCreate(Sender: TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,1; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 LReport.Font.Size:=20;
 LReport.Font.Style:=[fsBold];
 LName.Font.Style:=[fsBold];
 LVersion.Font.Size:=16;
 LEmail.Font.Style:=[fsBold];
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,1; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

end.
