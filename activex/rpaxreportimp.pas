unit rpaxreportimp;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, Controls, Graphics, Menus, Forms, StdCtrls,
  ComServ, StdVCL, AXCtrls, TAXReportXControl1_TLB, rpactivexreport;

type
  TTAXReport = class(TActiveXControl, ITAXReport)
  private
    { Private declarations }
    FDelphiControl: TRpActiveXReport;
    FEvents: ITAXReportEvents;
  protected
    { Protected declarations }
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    procedure InitializeControl; override;
    function DrawTextBiDiModeFlagsReadingOnly: Integer; safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_Cursor: Smallint; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Filename: WideString; safecall;
    function Get_HelpKeyword: WideString; safecall;
    function Get_HelpType: TxHelpType; safecall;
    function Get_Preview: WordBool; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    function IsRightToLeft: WordBool; safecall;
    function UseRightToLeftReading: WordBool; safecall;
    function UseRightToLeftScrollBar: WordBool; safecall;
    procedure AboutBox; safecall;
    procedure Execute; safecall;
    procedure InitiateAction; safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Filename(const Value: WideString); safecall;
    procedure Set_HelpKeyword(const Value: WideString); safecall;
    procedure Set_HelpType(Value: TxHelpType); safecall;
    procedure Set_Preview(Value: WordBool); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    procedure SetSubComponent(IsSubComponent: WordBool); safecall;
  end;

implementation

uses ComObj, rpfaboutx;

{ TTAXReport }

procedure TTAXReport.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  {TODO: Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_TAXReportPage); }
end;

procedure TTAXReport.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as ITAXReportEvents;
end;

procedure TTAXReport.InitializeControl;
begin
  FDelphiControl := Control as TRpActiveXReport;
end;

function TTAXReport.DrawTextBiDiModeFlagsReadingOnly: Integer;
begin
  Result := FDelphiControl.DrawTextBiDiModeFlagsReadingOnly;
end;

function TTAXReport.Get_AlignDisabled: WordBool;
begin
  Result := FDelphiControl.AlignDisabled;
end;

function TTAXReport.Get_Cursor: Smallint;
begin
  Result := Smallint(FDelphiControl.Cursor);
end;

function TTAXReport.Get_DoubleBuffered: WordBool;
begin
  Result := FDelphiControl.DoubleBuffered;
end;

function TTAXReport.Get_Enabled: WordBool;
begin
  Result := FDelphiControl.Enabled;
end;

function TTAXReport.Get_Filename: WideString;
begin
  Result := WideString(FDelphiControl.Filename);
end;

function TTAXReport.Get_HelpKeyword: WideString;
begin
  Result := WideString(FDelphiControl.HelpKeyword);
end;

function TTAXReport.Get_HelpType: TxHelpType;
begin
  Result := Ord(FDelphiControl.HelpType);
end;

function TTAXReport.Get_Preview: WordBool;
begin
  Result := FDelphiControl.Preview;
end;

function TTAXReport.Get_Visible: WordBool;
begin
  Result := FDelphiControl.Visible;
end;

function TTAXReport.Get_VisibleDockClientCount: Integer;
begin
  Result := FDelphiControl.VisibleDockClientCount;
end;

function TTAXReport.IsRightToLeft: WordBool;
begin
  Result := FDelphiControl.IsRightToLeft;
end;

function TTAXReport.UseRightToLeftReading: WordBool;
begin
  Result := FDelphiControl.UseRightToLeftReading;
end;

function TTAXReport.UseRightToLeftScrollBar: WordBool;
begin
  Result := FDelphiControl.UseRightToLeftScrollBar;
end;

procedure TTAXReport.AboutBox;
begin
  ShowTAXReportAbout;
end;

procedure TTAXReport.Execute;
begin
  FDelphiControl.Execute;
end;

procedure TTAXReport.InitiateAction;
begin
  FDelphiControl.InitiateAction;
end;

procedure TTAXReport.Set_Cursor(Value: Smallint);
begin
  FDelphiControl.Cursor := TCursor(Value);
end;

procedure TTAXReport.Set_DoubleBuffered(Value: WordBool);
begin
  FDelphiControl.DoubleBuffered := Value;
end;

procedure TTAXReport.Set_Enabled(Value: WordBool);
begin
  FDelphiControl.Enabled := Value;
end;

procedure TTAXReport.Set_Filename(const Value: WideString);
begin
  FDelphiControl.Filename := String(Value);
end;

procedure TTAXReport.Set_HelpKeyword(const Value: WideString);
begin
  FDelphiControl.HelpKeyword := String(Value);
end;

procedure TTAXReport.Set_HelpType(Value: TxHelpType);
begin
  FDelphiControl.HelpType := THelpType(Value);
end;

procedure TTAXReport.Set_Preview(Value: WordBool);
begin
  FDelphiControl.Preview := Value;
end;

procedure TTAXReport.Set_Visible(Value: WordBool);
begin
  FDelphiControl.Visible := Value;
end;

procedure TTAXReport.SetSubComponent(IsSubComponent: WordBool);
begin
  FDelphiControl.SetSubComponent(IsSubComponent);
end;

initialization
  TActiveXControlFactory.Create(
    ComServer,
    TTAXReport,
    TRpActiveXReport,
    Class_TAXReport,
    1,
    '',
    0,
    tmApartment);
end.
