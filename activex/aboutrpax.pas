unit aboutrpax;

//{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons,rpmdconsts,ShellApi;

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
    Memo1: TMemo;
    Label6: TLabel;
    LReport: TLabel;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Label5MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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

procedure TReportManXAbout.FormCreate(Sender: TObject);
begin
 LVersion.Caption:=TranslateStr(91,'Version')+' '+RM_VERSION+' XP';
end;

procedure TReportManXAbout.Label5MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 // Starts the explorer
 ShellExecute(Self.handle,Pchar('open'),Pchar(TLabel(Sender).Caption),nil,nil,SW_SHOWNORMAL);
end;

end.
