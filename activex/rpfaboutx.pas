unit rpfaboutx;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TReportManXAbout = class(TForm)
    CtlImage: TSpeedButton;
    NameLbl: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LVersion: TLabel;
    Image1: TImage;
    Label4: TLabel;
    LName: TLabel;
    Label5: TLabel;
    LEmail: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Button1: TButton;
    Memo1: TMemo;
  end;

procedure ShowReportManXAbout;

implementation

{$R *.DFM}

procedure ShowReportManXAbout;
begin
  with TReportManXAbout.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

end.
