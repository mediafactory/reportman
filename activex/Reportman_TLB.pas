unit Reportman_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 06/11/2003 19:43:31 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\prog\toni\cvsroot\reportman\reportman\activex\ReportMan.tlb (1)
// LIBID: {D4D26F6B-6564-44F4-A913-03C91CE37740}
// LCID: 0
// Helpfile: 
// HelpString: Report Manager ActiveX Library
// DepndLst: 
//   (1) v2.0 stdole, (H:\WINDOWS\System32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  ReportmanMajorVersion = 1;
  ReportmanMinorVersion = 6;

  LIBID_Reportman: TGUID = '{D4D26F6B-6564-44F4-A913-03C91CE37740}';

  IID_IReportManX: TGUID = '{B3AE1470-158D-4855-83DB-BC3A2746C26E}';
  DIID_IReportManXEvents: TGUID = '{50909EA4-8F4F-4865-877D-287FC7072177}';
  CLASS_ReportManX: TGUID = '{DC30E149-4129-450F-BDFE-BD9E6F31147E}';
  IID_IReportReport: TGUID = '{2FCB34BE-8DD4-4567-A771-9965C2FD3A04}';
  CLASS_ReportReport: TGUID = '{E30FD4FC-F47A-4932-A3E6-6694550588F3}';
  IID_IReportParameters: TGUID = '{A5F6E90E-DFE7-49DA-AA38-C1A41C995B6B}';
  CLASS_ReportParameters: TGUID = '{F79CF82C-C2AD-46CC-ABEA-084016CFE58A}';
  IID_IReportParam: TGUID = '{F1634F9E-DE5A-411E-9A9E-3A46707A7ABB}';
  CLASS_ReportParam: TGUID = '{E96B253E-143E-40E8-BFDA-366C5F112DAE}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum TxHelpType
type
  TxHelpType = TOleEnum;
const
  htKeyword = $00000000;
  htContext = $00000001;

// Constants for enum TxParamType
type
  TxParamType = TOleEnum;
const
  rpParamString = $00000000;
  rpParamInteger = $00000001;
  rpParamDouble = $00000002;
  rpParamDate = $00000003;
  rpParamTime = $00000004;
  rpParamDateTime = $00000005;
  rpParamCurrency = $00000006;
  rpParamBool = $00000007;
  rpParamExpreB = $00000008;
  rpParamExpreA = $00000009;
  rpParamSubst = $0000000A;
  rpParamList = $0000000B;
  rpParamUnknown = $0000000C;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IReportManX = interface;
  IReportManXDisp = dispinterface;
  IReportManXEvents = dispinterface;
  IReportReport = interface;
  IReportReportDisp = dispinterface;
  IReportParameters = interface;
  IReportParametersDisp = dispinterface;
  IReportParam = interface;
  IReportParamDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  ReportManX = IReportManX;
  ReportReport = IReportReport;
  ReportParameters = IReportParameters;
  ReportParam = IReportParam;


// *********************************************************************//
// Interface: IReportManX
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B3AE1470-158D-4855-83DB-BC3A2746C26E}
// *********************************************************************//
  IReportManX = interface(IDispatch)
    ['{B3AE1470-158D-4855-83DB-BC3A2746C26E}']
    procedure SetDatasetSQL(const datasetname: WideString; const sqlsentence: WideString); safecall;
    procedure SetDatabaseConnectionString(const databasename: WideString; 
                                          const connectionstring: WideString); safecall;
    function GetDatasetSQL(const datasetname: WideString): WideString; safecall;
    function GetDatabaseConnectionString(const databasename: WideString): WideString; safecall;
    procedure SetParamValue(const paramname: WideString; paramvalue: OleVariant); safecall;
    function GetParamValue(const paramname: WideString): OleVariant; safecall;
    function Execute: WordBool; safecall;
    procedure PrinterSetup; safecall;
    function ShowParams: WordBool; safecall;
    procedure SaveToPDF(const filename: WideString; compressed: WordBool); safecall;
    function PrintRange(frompage: Integer; topage: Integer; copies: Integer; collate: WordBool): WordBool; safecall;
    function Get_filename: WideString; safecall;
    procedure Set_filename(const Value: WideString); safecall;
    function Get_Preview: WordBool; safecall;
    procedure Set_Preview(Value: WordBool); safecall;
    function Get_ShowProgress: WordBool; safecall;
    procedure Set_ShowProgress(Value: WordBool); safecall;
    function Get_ShowPrintDialog: WordBool; safecall;
    procedure Set_ShowPrintDialog(Value: WordBool); safecall;
    function Get_Title: WideString; safecall;
    procedure Set_Title(const Value: WideString); safecall;
    function Get_Language: Integer; safecall;
    procedure Set_Language(Value: Integer); safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    function DrawTextBiDiModeFlagsReadingOnly: Integer; safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure InitiateAction; safecall;
    function IsRightToLeft: WordBool; safecall;
    function UseRightToLeftReading: WordBool; safecall;
    function UseRightToLeftScrollBar: WordBool; safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_Cursor: Smallint; safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    function Get_HelpType: TxHelpType; safecall;
    procedure Set_HelpType(Value: TxHelpType); safecall;
    function Get_HelpKeyword: WideString; safecall;
    procedure Set_HelpKeyword(const Value: WideString); safecall;
    procedure SetSubComponent(IsSubComponent: WordBool); safecall;
    procedure AboutBox; safecall;
    procedure ExecuteRemote(const hostname: WideString; port: Integer; const user: WideString; 
                            const password: WideString; const aliasname: WideString; 
                            const reportname: WideString); safecall;
    procedure CalcReport(ShowProgress: WordBool); safecall;
    procedure Compose(const Report: ReportReport; Execute: WordBool); safecall;
    procedure SaveToText(const filename: WideString; const textdriver: WideString); safecall;
    function Get_Report: ReportReport; safecall;
    procedure SaveToExcel(const filename: WideString); safecall;
    property filename: WideString read Get_filename write Set_filename;
    property Preview: WordBool read Get_Preview write Set_Preview;
    property ShowProgress: WordBool read Get_ShowProgress write Set_ShowProgress;
    property ShowPrintDialog: WordBool read Get_ShowPrintDialog write Set_ShowPrintDialog;
    property Title: WideString read Get_Title write Set_Title;
    property Language: Integer read Get_Language write Set_Language;
    property DoubleBuffered: WordBool read Get_DoubleBuffered write Set_DoubleBuffered;
    property AlignDisabled: WordBool read Get_AlignDisabled;
    property VisibleDockClientCount: Integer read Get_VisibleDockClientCount;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property Cursor: Smallint read Get_Cursor write Set_Cursor;
    property HelpType: TxHelpType read Get_HelpType write Set_HelpType;
    property HelpKeyword: WideString read Get_HelpKeyword write Set_HelpKeyword;
    property Report: ReportReport read Get_Report;
  end;

// *********************************************************************//
// DispIntf:  IReportManXDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B3AE1470-158D-4855-83DB-BC3A2746C26E}
// *********************************************************************//
  IReportManXDisp = dispinterface
    ['{B3AE1470-158D-4855-83DB-BC3A2746C26E}']
    procedure SetDatasetSQL(const datasetname: WideString; const sqlsentence: WideString); dispid 1;
    procedure SetDatabaseConnectionString(const databasename: WideString; 
                                          const connectionstring: WideString); dispid 2;
    function GetDatasetSQL(const datasetname: WideString): WideString; dispid 3;
    function GetDatabaseConnectionString(const databasename: WideString): WideString; dispid 4;
    procedure SetParamValue(const paramname: WideString; paramvalue: OleVariant); dispid 5;
    function GetParamValue(const paramname: WideString): OleVariant; dispid 6;
    function Execute: WordBool; dispid 7;
    procedure PrinterSetup; dispid 8;
    function ShowParams: WordBool; dispid 9;
    procedure SaveToPDF(const filename: WideString; compressed: WordBool); dispid 10;
    function PrintRange(frompage: Integer; topage: Integer; copies: Integer; collate: WordBool): WordBool; dispid 11;
    property filename: WideString dispid 12;
    property Preview: WordBool dispid 13;
    property ShowProgress: WordBool dispid 14;
    property ShowPrintDialog: WordBool dispid 15;
    property Title: WideString dispid 16;
    property Language: Integer dispid 17;
    property DoubleBuffered: WordBool dispid 18;
    property AlignDisabled: WordBool readonly dispid 19;
    property VisibleDockClientCount: Integer readonly dispid 20;
    function DrawTextBiDiModeFlagsReadingOnly: Integer; dispid 22;
    property Enabled: WordBool dispid -514;
    procedure InitiateAction; dispid 23;
    function IsRightToLeft: WordBool; dispid 24;
    function UseRightToLeftReading: WordBool; dispid 27;
    function UseRightToLeftScrollBar: WordBool; dispid 28;
    property Visible: WordBool dispid 29;
    property Cursor: Smallint dispid 30;
    property HelpType: TxHelpType dispid 31;
    property HelpKeyword: WideString dispid 32;
    procedure SetSubComponent(IsSubComponent: WordBool); dispid 34;
    procedure AboutBox; dispid -552;
    procedure ExecuteRemote(const hostname: WideString; port: Integer; const user: WideString; 
                            const password: WideString; const aliasname: WideString; 
                            const reportname: WideString); dispid 201;
    procedure CalcReport(ShowProgress: WordBool); dispid 202;
    procedure Compose(const Report: ReportReport; Execute: WordBool); dispid 203;
    procedure SaveToText(const filename: WideString; const textdriver: WideString); dispid 204;
    property Report: ReportReport readonly dispid 21;
    procedure SaveToExcel(const filename: WideString); dispid 25;
  end;

// *********************************************************************//
// DispIntf:  IReportManXEvents
// Flags:     (4096) Dispatchable
// GUID:      {50909EA4-8F4F-4865-877D-287FC7072177}
// *********************************************************************//
  IReportManXEvents = dispinterface
    ['{50909EA4-8F4F-4865-877D-287FC7072177}']
  end;

// *********************************************************************//
// Interface: IReportReport
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2FCB34BE-8DD4-4567-A771-9965C2FD3A04}
// *********************************************************************//
  IReportReport = interface(IDispatch)
    ['{2FCB34BE-8DD4-4567-A771-9965C2FD3A04}']
    function Get_Params: ReportParameters; safecall;
    function Get_VCLReport: PChar; safecall;
    property Params: ReportParameters read Get_Params;
    property VCLReport: PChar read Get_VCLReport;
  end;

// *********************************************************************//
// DispIntf:  IReportReportDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2FCB34BE-8DD4-4567-A771-9965C2FD3A04}
// *********************************************************************//
  IReportReportDisp = dispinterface
    ['{2FCB34BE-8DD4-4567-A771-9965C2FD3A04}']
    property Params: ReportParameters readonly dispid 1;
    property VCLReport: {??PChar}OleVariant readonly dispid 3;
  end;

// *********************************************************************//
// Interface: IReportParameters
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A5F6E90E-DFE7-49DA-AA38-C1A41C995B6B}
// *********************************************************************//
  IReportParameters = interface(IDispatch)
    ['{A5F6E90E-DFE7-49DA-AA38-C1A41C995B6B}']
    function Get_Count: Integer; safecall;
    function Get_Items(Index: Integer): ReportParam; safecall;
    property Count: Integer read Get_Count;
    property Items[Index: Integer]: ReportParam read Get_Items;
  end;

// *********************************************************************//
// DispIntf:  IReportParametersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A5F6E90E-DFE7-49DA-AA38-C1A41C995B6B}
// *********************************************************************//
  IReportParametersDisp = dispinterface
    ['{A5F6E90E-DFE7-49DA-AA38-C1A41C995B6B}']
    property Count: Integer readonly dispid 1;
    property Items[Index: Integer]: ReportParam readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IReportParam
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F1634F9E-DE5A-411E-9A9E-3A46707A7ABB}
// *********************************************************************//
  IReportParam = interface(IDispatch)
    ['{F1634F9E-DE5A-411E-9A9E-3A46707A7ABB}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const Value: WideString); safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_ParamType: TxParamType; safecall;
    procedure Set_ParamType(Value: TxParamType); safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(Value: OleVariant); safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Description: WideString read Get_Description write Set_Description;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property ParamType: TxParamType read Get_ParamType write Set_ParamType;
    property Value: OleVariant read Get_Value write Set_Value;
  end;

// *********************************************************************//
// DispIntf:  IReportParamDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F1634F9E-DE5A-411E-9A9E-3A46707A7ABB}
// *********************************************************************//
  IReportParamDisp = dispinterface
    ['{F1634F9E-DE5A-411E-9A9E-3A46707A7ABB}']
    property Name: WideString dispid 1;
    property Description: WideString dispid 2;
    property Visible: WordBool dispid 3;
    property ParamType: TxParamType dispid 4;
    property Value: OleVariant dispid 5;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TReportManX
// Help String      : ReportManX Control
// Default Interface: IReportManX
// Def. Intf. DISP? : No
// Event   Interface: IReportManXEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TReportManX = class(TOleControl)
  private
    FIntf: IReportManX;
    function  GetControlInterface: IReportManX;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_Report: ReportReport;
  public
    procedure SetDatasetSQL(const datasetname: WideString; const sqlsentence: WideString);
    procedure SetDatabaseConnectionString(const databasename: WideString; 
                                          const connectionstring: WideString);
    function GetDatasetSQL(const datasetname: WideString): WideString;
    function GetDatabaseConnectionString(const databasename: WideString): WideString;
    procedure SetParamValue(const paramname: WideString; paramvalue: OleVariant);
    function GetParamValue(const paramname: WideString): OleVariant;
    function Execute: WordBool;
    procedure PrinterSetup;
    function ShowParams: WordBool;
    procedure SaveToPDF(const filename: WideString; compressed: WordBool);
    function PrintRange(frompage: Integer; topage: Integer; copies: Integer; collate: WordBool): WordBool;
    function DrawTextBiDiModeFlagsReadingOnly: Integer;
    procedure InitiateAction;
    function IsRightToLeft: WordBool;
    function UseRightToLeftReading: WordBool;
    function UseRightToLeftScrollBar: WordBool;
    procedure SetSubComponent(IsSubComponent: WordBool);
    procedure AboutBox;
    procedure ExecuteRemote(const hostname: WideString; port: Integer; const user: WideString; 
                            const password: WideString; const aliasname: WideString; 
                            const reportname: WideString);
    procedure CalcReport(ShowProgress: WordBool);
    procedure Compose(const Report: ReportReport; Execute: WordBool);
    procedure SaveToText(const filename: WideString; const textdriver: WideString);
    procedure SaveToExcel(const filename: WideString);
    property  ControlInterface: IReportManX read GetControlInterface;
    property  DefaultInterface: IReportManX read GetControlInterface;
    property DoubleBuffered: WordBool index 18 read GetWordBoolProp write SetWordBoolProp;
    property AlignDisabled: WordBool index 19 read GetWordBoolProp;
    property VisibleDockClientCount: Integer index 20 read GetIntegerProp;
    property Enabled: WordBool index -514 read GetWordBoolProp write SetWordBoolProp;
    property Visible: WordBool index 29 read GetWordBoolProp write SetWordBoolProp;
    property Report: ReportReport read Get_Report;
  published
    property Anchors;
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property filename: WideString index 12 read GetWideStringProp write SetWideStringProp stored False;
    property Preview: WordBool index 13 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowProgress: WordBool index 14 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowPrintDialog: WordBool index 15 read GetWordBoolProp write SetWordBoolProp stored False;
    property Title: WideString index 16 read GetWideStringProp write SetWideStringProp stored False;
    property Language: Integer index 17 read GetIntegerProp write SetIntegerProp stored False;
    property Cursor: Smallint index 30 read GetSmallintProp write SetSmallintProp stored False;
    property HelpType: TOleEnum index 31 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property HelpKeyword: WideString index 32 read GetWideStringProp write SetWideStringProp stored False;
  end;

// *********************************************************************//
// The Class CoReportReport provides a Create and CreateRemote method to          
// create instances of the default interface IReportReport exposed by              
// the CoClass ReportReport. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoReportReport = class
    class function Create: IReportReport;
    class function CreateRemote(const MachineName: string): IReportReport;
  end;

// *********************************************************************//
// The Class CoReportParameters provides a Create and CreateRemote method to          
// create instances of the default interface IReportParameters exposed by              
// the CoClass ReportParameters. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoReportParameters = class
    class function Create: IReportParameters;
    class function CreateRemote(const MachineName: string): IReportParameters;
  end;

// *********************************************************************//
// The Class CoReportParam provides a Create and CreateRemote method to          
// create instances of the default interface IReportParam exposed by              
// the CoClass ReportParam. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoReportParam = class
    class function Create: IReportParam;
    class function CreateRemote(const MachineName: string): IReportParam;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'Servers';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

procedure TReportManX.InitControlData;
const
  CControlData: TControlData2 = (
    ClassID: '{DC30E149-4129-450F-BDFE-BD9E6F31147E}';
    EventIID: '';
    EventCount: 0;
    EventDispIDs: nil;
    LicenseKey: nil (*HR:$00000000*);
    Flags: $00000008;
    Version: 401);
begin
  ControlData := @CControlData;
end;

procedure TReportManX.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IReportManX;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TReportManX.GetControlInterface: IReportManX;
begin
  CreateControl;
  Result := FIntf;
end;

function TReportManX.Get_Report: ReportReport;
begin
    Result := DefaultInterface.Report;
end;

procedure TReportManX.SetDatasetSQL(const datasetname: WideString; const sqlsentence: WideString);
begin
  DefaultInterface.SetDatasetSQL(datasetname, sqlsentence);
end;

procedure TReportManX.SetDatabaseConnectionString(const databasename: WideString; 
                                                  const connectionstring: WideString);
begin
  DefaultInterface.SetDatabaseConnectionString(databasename, connectionstring);
end;

function TReportManX.GetDatasetSQL(const datasetname: WideString): WideString;
begin
  Result := DefaultInterface.GetDatasetSQL(datasetname);
end;

function TReportManX.GetDatabaseConnectionString(const databasename: WideString): WideString;
begin
  Result := DefaultInterface.GetDatabaseConnectionString(databasename);
end;

procedure TReportManX.SetParamValue(const paramname: WideString; paramvalue: OleVariant);
begin
  DefaultInterface.SetParamValue(paramname, paramvalue);
end;

function TReportManX.GetParamValue(const paramname: WideString): OleVariant;
begin
  Result := DefaultInterface.GetParamValue(paramname);
end;

function TReportManX.Execute: WordBool;
begin
  Result := DefaultInterface.Execute;
end;

procedure TReportManX.PrinterSetup;
begin
  DefaultInterface.PrinterSetup;
end;

function TReportManX.ShowParams: WordBool;
begin
  Result := DefaultInterface.ShowParams;
end;

procedure TReportManX.SaveToPDF(const filename: WideString; compressed: WordBool);
begin
  DefaultInterface.SaveToPDF(filename, compressed);
end;

function TReportManX.PrintRange(frompage: Integer; topage: Integer; copies: Integer; 
                                collate: WordBool): WordBool;
begin
  Result := DefaultInterface.PrintRange(frompage, topage, copies, collate);
end;

function TReportManX.DrawTextBiDiModeFlagsReadingOnly: Integer;
begin
  Result := DefaultInterface.DrawTextBiDiModeFlagsReadingOnly;
end;

procedure TReportManX.InitiateAction;
begin
  DefaultInterface.InitiateAction;
end;

function TReportManX.IsRightToLeft: WordBool;
begin
  Result := DefaultInterface.IsRightToLeft;
end;

function TReportManX.UseRightToLeftReading: WordBool;
begin
  Result := DefaultInterface.UseRightToLeftReading;
end;

function TReportManX.UseRightToLeftScrollBar: WordBool;
begin
  Result := DefaultInterface.UseRightToLeftScrollBar;
end;

procedure TReportManX.SetSubComponent(IsSubComponent: WordBool);
begin
  DefaultInterface.SetSubComponent(IsSubComponent);
end;

procedure TReportManX.AboutBox;
begin
  DefaultInterface.AboutBox;
end;

procedure TReportManX.ExecuteRemote(const hostname: WideString; port: Integer; 
                                    const user: WideString; const password: WideString; 
                                    const aliasname: WideString; const reportname: WideString);
begin
  DefaultInterface.ExecuteRemote(hostname, port, user, password, aliasname, reportname);
end;

procedure TReportManX.CalcReport(ShowProgress: WordBool);
begin
  DefaultInterface.CalcReport(ShowProgress);
end;

procedure TReportManX.Compose(const Report: ReportReport; Execute: WordBool);
begin
  DefaultInterface.Compose(Report, Execute);
end;

procedure TReportManX.SaveToText(const filename: WideString; const textdriver: WideString);
begin
  DefaultInterface.SaveToText(filename, textdriver);
end;

procedure TReportManX.SaveToExcel(const filename: WideString);
begin
  DefaultInterface.SaveToExcel(filename);
end;

class function CoReportReport.Create: IReportReport;
begin
  Result := CreateComObject(CLASS_ReportReport) as IReportReport;
end;

class function CoReportReport.CreateRemote(const MachineName: string): IReportReport;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ReportReport) as IReportReport;
end;

class function CoReportParameters.Create: IReportParameters;
begin
  Result := CreateComObject(CLASS_ReportParameters) as IReportParameters;
end;

class function CoReportParameters.CreateRemote(const MachineName: string): IReportParameters;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ReportParameters) as IReportParameters;
end;

class function CoReportParam.Create: IReportParam;
begin
  Result := CreateComObject(CLASS_ReportParam) as IReportParam;
end;

class function CoReportParam.CreateRemote(const MachineName: string): IReportParam;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ReportParam) as IReportParam;
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TReportManX]);
end;

end.
