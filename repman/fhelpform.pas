unit fhelpform;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QActnList, QImgList, QComCtrls;

type
  TFHelpf = class(TForm)
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

procedure TFHelpf.AExitExecute(Sender: TObject);
begin
 Close;
end;

procedure TFHelpf.ABackwardExecute(Sender: TObject);
begin
 TextBrowser1.Backward;
end;

procedure TFHelpf.AForwardExecute(Sender: TObject);
begin
 TextBrowser1.Forward;
end;

end.
