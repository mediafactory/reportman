unit rpfaboutx;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TTAXReportAbout = class(TForm)
    NameLbl: TLabel;
    OkBtn: TButton;
    CopyrightLbl: TLabel;
    DescLbl: TLabel;
    LVersion: TLabel;
    Image1: TImage;
    Label1: TLabel;
    LName: TLabel;
    Label2: TLabel;
    LEmail: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Memo1: TMemo;
    Label6: TLabel;
  end;

procedure ShowTAXReportAbout;

implementation

{$R *.DFM}

procedure ShowTAXReportAbout;
begin
  with TTAXReportAbout.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

end.
