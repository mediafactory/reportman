unit aboutrpax;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TReportManXAbout = class(TForm)
    CtlImage: TSpeedButton;
    NameLbl: TLabel;
    DescLbl: TLabel;
    Image1: TImage;
    Label1: TLabel;
    LName: TLabel;
    Label2: TLabel;
    LEmail: TLabel;
    Label3: TLabel;
    LVersion: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Memo1: TMemo;
    Label6: TLabel;
    LReport: TLabel;
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
