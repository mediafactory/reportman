unit rpaxreportimp;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, Controls, Graphics, Menus, Forms, StdCtrls,
  ComServ, StdVCL, AXCtrls, ReportMan_TLB, rpactivexreport;

type
  TReportManX = class(TActiveXControl, IReportManX)
  private
    { Private declarations }
    FDelphiControl: TRpActiveXReport;
    FEvents: IReportManXEvents;
  protected
    { Protected declarations }
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    procedure InitializeControl; override;
    function DrawTextBiDiModeFlagsReadingOnly: Integer; safecall;
    function Execute: WordBool; safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_Cursor: Smallint; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Filename: WideString; safecall;
    function Get_HelpKeyword: WideString; safecall;
    function Get_HelpType: TxHelpType; safecall;
    function Get_Language: Integer; safecall;
    function Get_Preview: WordBool; safecall;
    function Get_ShowPrintDialog: WordBool; safecall;
    function Get_ShowProgress: WordBool; safecall;
    function Get_Title: WideString; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    function GetDatabaseConnectionString(
      const databasename: WideString): WideString; safecall;
    function GetDatasetSQL(const datasetname: WideString): WideString;
      safecall;
    function GetParamValue(const paramname: WideString): OleVariant; safecall;
    function IsRightToLeft: WordBool; safecall;
    function PrintRange(frompage, topage, copies: Integer;
      collate: WordBool): WordBool; safecall;
    function ShowParams: WordBool; safecall;
    function UseRightToLeftReading: WordBool; safecall;
    function UseRightToLeftScrollBar: WordBool; safecall;
    procedure AboutBox; safecall;
    procedure InitiateAction; safecall;
    procedure PrinterSetup; safecall;
    procedure SaveToPDF(const filename: WideString; compressed: WordBool);
      safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Filename(const Value: WideString); safecall;
    procedure Set_HelpKeyword(const Value: WideString); safecall;
    procedure Set_HelpType(Value: TxHelpType); safecall;
    procedure Set_Language(Value: Integer); safecall;
    procedure Set_Preview(Value: WordBool); safecall;
    procedure Set_ShowPrintDialog(Value: WordBool); safecall;
    procedure Set_ShowProgress(Value: WordBool); safecall;
    procedure Set_Title(const Value: WideString); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    procedure SetDatabaseConnectionString(const databasename,
      connectionstring: WideString); safecall;
    procedure SetDatasetSQL(const datasetname, sqlsentence: WideString);
      safecall;
    procedure SetParamValue(const paramname: WideString;
      paramvalue: OleVariant); safecall;
    procedure SetSubComponent(IsSubComponent: WordBool); safecall;
  end;

implementation

uses ComObj, aboutrpax;

{ TReportManX }

procedure TReportManX.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  {TODO: Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_ReportManXPage); }
end;

procedure TReportManX.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IReportManXEvents;
end;

procedure TReportManX.InitializeControl;
begin
  FDelphiControl := Control as TRpActiveXReport;
end;

function TReportManX.DrawTextBiDiModeFlagsReadingOnly: Integer;
begin
  Result := FDelphiControl.DrawTextBiDiModeFlagsReadingOnly;
end;

function TReportManX.Execute: WordBool;
begin
  Result := FDelphiControl.Execute;
end;

function TReportManX.Get_AlignDisabled: WordBool;
begin
  Result := FDelphiControl.AlignDisabled;
end;

function TReportManX.Get_Cursor: Smallint;
begin
  Result := Smallint(FDelphiControl.Cursor);
end;

function TReportManX.Get_DoubleBuffered: WordBool;
begin
  Result := FDelphiControl.DoubleBuffered;
end;

function TReportManX.Get_Enabled: WordBool;
begin
  Result := FDelphiControl.Enabled;
end;

function TReportManX.Get_Filename: WideString;
begin
  Result := WideString(FDelphiControl.Filename);
end;

function TReportManX.Get_HelpKeyword: WideString;
begin
  Result := WideString(FDelphiControl.HelpKeyword);
end;

function TReportManX.Get_HelpType: TxHelpType;
begin
  Result := Ord(FDelphiControl.HelpType);
end;

function TReportManX.Get_Language: Integer;
begin
  Result := FDelphiControl.Language;
end;

function TReportManX.Get_Preview: WordBool;
begin
  Result := FDelphiControl.Preview;
end;

function TReportManX.Get_ShowPrintDialog: WordBool;
begin
  Result := FDelphiControl.ShowPrintDialog;
end;

function TReportManX.Get_ShowProgress: WordBool;
begin
  Result := FDelphiControl.ShowProgress;
end;

function TReportManX.Get_Title: WideString;
begin
  Result := WideString(FDelphiControl.Title);
end;

function TReportManX.Get_Visible: WordBool;
begin
  Result := FDelphiControl.Visible;
end;

function TReportManX.Get_VisibleDockClientCount: Integer;
begin
  Result := FDelphiControl.VisibleDockClientCount;
end;

function TReportManX.GetDatabaseConnectionString(
  const databasename: WideString): WideString;
begin
  Result := FDelphiControl.GetDatabaseConnectionString(databasename);
end;

function TReportManX.GetDatasetSQL(
  const datasetname: WideString): WideString;
begin
  Result := FDelphiControl.GetDatasetSQL(datasetname);
end;

function TReportManX.GetParamValue(
  const paramname: WideString): OleVariant;
begin
  Result := FDelphiControl.GetParamValue(paramname);
end;

function TReportManX.IsRightToLeft: WordBool;
begin
  Result := FDelphiControl.IsRightToLeft;
end;

function TReportManX.PrintRange(frompage, topage, copies: Integer;
  collate: WordBool): WordBool;
begin
  Result := FDelphiControl.PrintRange(frompage, topage, copies, collate);
end;

function TReportManX.ShowParams: WordBool;
begin
  Result := FDelphiControl.ShowParams;
end;

function TReportManX.UseRightToLeftReading: WordBool;
begin
  Result := FDelphiControl.UseRightToLeftReading;
end;

function TReportManX.UseRightToLeftScrollBar: WordBool;
begin
  Result := FDelphiControl.UseRightToLeftScrollBar;
end;

procedure TReportManX.AboutBox;
begin
  ShowReportManXAbout;
end;

procedure TReportManX.InitiateAction;
begin
  FDelphiControl.InitiateAction;
end;

procedure TReportManX.PrinterSetup;
begin
  FDelphiControl.PrinterSetup;
end;

procedure TReportManX.SaveToPDF(const filename: WideString;
  compressed: WordBool);
begin
  FDelphiControl.SaveToPDF(filename, compressed);
end;

procedure TReportManX.Set_Cursor(Value: Smallint);
begin
  FDelphiControl.Cursor := TCursor(Value);
end;

procedure TReportManX.Set_DoubleBuffered(Value: WordBool);
begin
  FDelphiControl.DoubleBuffered := Value;
end;

procedure TReportManX.Set_Enabled(Value: WordBool);
begin
  FDelphiControl.Enabled := Value;
end;

procedure TReportManX.Set_Filename(const Value: WideString);
begin
  FDelphiControl.Filename := String(Value);
end;

procedure TReportManX.Set_HelpKeyword(const Value: WideString);
begin
  FDelphiControl.HelpKeyword := String(Value);
end;

procedure TReportManX.Set_HelpType(Value: TxHelpType);
begin
  FDelphiControl.HelpType := THelpType(Value);
end;

procedure TReportManX.Set_Language(Value: Integer);
begin
  FDelphiControl.Language := Value;
end;

procedure TReportManX.Set_Preview(Value: WordBool);
begin
  FDelphiControl.Preview := Value;
end;

procedure TReportManX.Set_ShowPrintDialog(Value: WordBool);
begin
  FDelphiControl.ShowPrintDialog := Value;
end;

procedure TReportManX.Set_ShowProgress(Value: WordBool);
begin
  FDelphiControl.ShowProgress := Value;
end;

procedure TReportManX.Set_Title(const Value: WideString);
begin
  FDelphiControl.Title := String(Value);
end;

procedure TReportManX.Set_Visible(Value: WordBool);
begin
  FDelphiControl.Visible := Value;
end;

procedure TReportManX.SetDatabaseConnectionString(const databasename,
  connectionstring: WideString);
begin
  FDelphiControl.SetDatabaseConnectionString(databasename, connectionstring);
end;

procedure TReportManX.SetDatasetSQL(const datasetname,
  sqlsentence: WideString);
begin
  FDelphiControl.SetDatasetSQL(datasetname, sqlsentence);
end;

procedure TReportManX.SetParamValue(const paramname: WideString;
  paramvalue: OleVariant);
begin
  FDelphiControl.SetParamValue(paramname, paramvalue);
end;

procedure TReportManX.SetSubComponent(IsSubComponent: WordBool);
begin
  FDelphiControl.SetSubComponent(IsSubComponent);
end;

initialization
  TActiveXControlFactory.Create(
    ComServer,
    TReportManX,
    TRpActiveXReport,
    Class_ReportManX,
    1,
    '',
    0,
    tmApartment);
end.
