{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       Help form                                       }
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

unit rpmdfhelpform;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QActnList, QImgList, QComCtrls;

type
  TFRpHelpForm = class(TForm)
    ToolBar1: TToolBar;
    ImageList1: TImageList;
    ActionList1: TActionList;
    ABackward: TAction;
    AForward: TAction;
    AExit: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    TextBrowser1: TTextBrowser;
    procedure AExitExecute(Sender: TObject);
    procedure ABackwardExecute(Sender: TObject);
    procedure AForwardExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.xfm}

procedure TFRpHelpForm.AExitExecute(Sender: TObject);
begin
 Close;
end;

procedure TFRpHelpForm.ABackwardExecute(Sender: TObject);
begin
 TextBrowser1.Backward;
end;

procedure TFRpHelpForm.AForwardExecute(Sender: TObject);
begin
 TextBrowser1.Forward;
end;

end.
