unit TAXReportXControl1_TLB;

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

// PASTLWTR : $Revision: 1.1 $
// File generated on 13/06/2002 11:29:30 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\prog\toni\cvsroot\reportman\reportman\activex\TAXReportXControl1.tlb (1)
// LIBID: {C878B0C7-91EA-43EB-A8D8-EA350082F64D}
// LCID: 0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
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
  TAXReportXControl1MajorVersion = 0;
  TAXReportXControl1MinorVersion = 99;

  LIBID_TAXReportXControl1: TGUID = '{C878B0C7-91EA-43EB-A8D8-EA350082F64D}';

  IID_ITAXReport: TGUID = '{034377A4-0299-4D59-8992-C95AB6E82668}';
  DIID_ITAXReportEvents: TGUID = '{F3636CC5-5ED7-4E1D-B5E2-8B9A4C900A66}';
  CLASS_TAXReport: TGUID = '{A3905DC4-2E8C-4E51-9F13-FCA6E1BA5125}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum TxHelpType
type
  TxHelpType = TOleEnum;
const
  htKeyword = $00000000;
  htContext = $00000001;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ITAXReport = interface;
  ITAXReportDisp = dispinterface;
  ITAXReportEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  TAXReport = ITAXReport;


// *********************************************************************//
// Interface: ITAXReport
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {034377A4-0299-4D59-8992-C95AB6E82668}
// *********************************************************************//
  ITAXReport = interface(IDispatch)
    ['{034377A4-0299-4D59-8992-C95AB6E82668}']
    procedure Execute; safecall;
    function Get_Filename: WideString; safecall;
    procedure Set_Filename(const Value: WideString); safecall;
    function Get_Preview: WordBool; safecall;
    procedure Set_Preview(Value: WordBool); safecall;
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
    property Filename: WideString read Get_Filename write Set_Filename;
    property Preview: WordBool read Get_Preview write Set_Preview;
    property DoubleBuffered: WordBool read Get_DoubleBuffered write Set_DoubleBuffered;
    property AlignDisabled: WordBool read Get_AlignDisabled;
    property VisibleDockClientCount: Integer read Get_VisibleDockClientCount;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property Cursor: Smallint read Get_Cursor write Set_Cursor;
    property HelpType: TxHelpType read Get_HelpType write Set_HelpType;
    property HelpKeyword: WideString read Get_HelpKeyword write Set_HelpKeyword;
  end;

// *********************************************************************//
// DispIntf:  ITAXReportDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {034377A4-0299-4D59-8992-C95AB6E82668}
// *********************************************************************//
  ITAXReportDisp = dispinterface
    ['{034377A4-0299-4D59-8992-C95AB6E82668}']
    procedure Execute; dispid 1;
    property Filename: WideString dispid 2;
    property Preview: WordBool dispid 3;
    property DoubleBuffered: WordBool dispid 4;
    property AlignDisabled: WordBool readonly dispid 5;
    property VisibleDockClientCount: Integer readonly dispid 6;
    function DrawTextBiDiModeFlagsReadingOnly: Integer; dispid 8;
    property Enabled: WordBool dispid -514;
    procedure InitiateAction; dispid 9;
    function IsRightToLeft: WordBool; dispid 10;
    function UseRightToLeftReading: WordBool; dispid 13;
    function UseRightToLeftScrollBar: WordBool; dispid 14;
    property Visible: WordBool dispid 15;
    property Cursor: Smallint dispid 16;
    property HelpType: TxHelpType dispid 17;
    property HelpKeyword: WideString dispid 18;
    procedure SetSubComponent(IsSubComponent: WordBool); dispid 20;
    procedure AboutBox; dispid -552;
  end;

// *********************************************************************//
// DispIntf:  ITAXReportEvents
// Flags:     (0)
// GUID:      {F3636CC5-5ED7-4E1D-B5E2-8B9A4C900A66}
// *********************************************************************//
  ITAXReportEvents = dispinterface
    ['{F3636CC5-5ED7-4E1D-B5E2-8B9A4C900A66}']
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TTAXReport
// Help String      : TAXReport Control
// Default Interface: ITAXReport
// Def. Intf. DISP? : No
// Event   Interface: ITAXReportEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TTAXReport = class(TOleControl)
  private
    FIntf: ITAXReport;
    function  GetControlInterface: ITAXReport;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    procedure Execute;
    function DrawTextBiDiModeFlagsReadingOnly: Integer;
    procedure InitiateAction;
    function IsRightToLeft: WordBool;
    function UseRightToLeftReading: WordBool;
    function UseRightToLeftScrollBar: WordBool;
    procedure SetSubComponent(IsSubComponent: WordBool);
    procedure AboutBox;
    property  ControlInterface: ITAXReport read GetControlInterface;
    property  DefaultInterface: ITAXReport read GetControlInterface;
    property DoubleBuffered: WordBool index 4 read GetWordBoolProp write SetWordBoolProp;
    property AlignDisabled: WordBool index 5 read GetWordBoolProp;
    property VisibleDockClientCount: Integer index 6 read GetIntegerProp;
    property Enabled: WordBool index -514 read GetWordBoolProp write SetWordBoolProp;
    property Visible: WordBool index 15 read GetWordBoolProp write SetWordBoolProp;
  published
    property Filename: WideString index 2 read GetWideStringProp write SetWideStringProp stored False;
    property Preview: WordBool index 3 read GetWordBoolProp write SetWordBoolProp stored False;
    property Cursor: Smallint index 16 read GetSmallintProp write SetSmallintProp stored False;
    property HelpType: TOleEnum index 17 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property HelpKeyword: WideString index 18 read GetWideStringProp write SetWideStringProp stored False;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'Servers';

implementation

uses ComObj;

procedure TTAXReport.InitControlData;
const
  CControlData: TControlData2 = (
    ClassID: '{A3905DC4-2E8C-4E51-9F13-FCA6E1BA5125}';
    EventIID: '';
    EventCount: 0;
    EventDispIDs: nil;
    LicenseKey: nil (*HR:$80040154*);
    Flags: $00000008;
    Version: 401);
begin
  ControlData := @CControlData;
end;

procedure TTAXReport.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as ITAXReport;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TTAXReport.GetControlInterface: ITAXReport;
begin
  CreateControl;
  Result := FIntf;
end;

procedure TTAXReport.Execute;
begin
  DefaultInterface.Execute;
end;

function TTAXReport.DrawTextBiDiModeFlagsReadingOnly: Integer;
begin
  Result := DefaultInterface.DrawTextBiDiModeFlagsReadingOnly;
end;

procedure TTAXReport.InitiateAction;
begin
  DefaultInterface.InitiateAction;
end;

function TTAXReport.IsRightToLeft: WordBool;
begin
  Result := DefaultInterface.IsRightToLeft;
end;

function TTAXReport.UseRightToLeftReading: WordBool;
begin
  Result := DefaultInterface.UseRightToLeftReading;
end;

function TTAXReport.UseRightToLeftScrollBar: WordBool;
begin
  Result := DefaultInterface.UseRightToLeftScrollBar;
end;

procedure TTAXReport.SetSubComponent(IsSubComponent: WordBool);
begin
  DefaultInterface.SetSubComponent(IsSubComponent);
end;

procedure TTAXReport.AboutBox;
begin
  DefaultInterface.AboutBox;
end;

procedure Register;
begin
  RegisterComponents('ActiveX',[TTAXReport]);
end;

end.
