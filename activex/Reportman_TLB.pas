unit ReportMan_TLB;

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
// File generated on 13/06/2002 17:57:33 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\prog\toni\cvsroot\reportman\reportman\activex\ReportMan.tlb (1)
// LIBID: {ACDAC827-F948-44F1-989F-788A67B45A97}
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
  ReportManMajorVersion = 1;
  ReportManMinorVersion = 0;

  LIBID_ReportMan: TGUID = '{ACDAC827-F948-44F1-989F-788A67B45A97}';

  IID_IReportManX: TGUID = '{08D4AA91-80AB-49DF-9E6B-62B4F4BED6D9}';
  DIID_IReportManXEvents: TGUID = '{BF1380A3-DE22-4675-BE8F-6F877A9CE402}';
  CLASS_ReportManX: TGUID = '{12B4E008-C646-4FB7-89F1-716BBD8EA06D}';

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
  IReportManX = interface;
  IReportManXDisp = dispinterface;
  IReportManXEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  ReportManX = IReportManX;


// *********************************************************************//
// Interface: IReportManX
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {08D4AA91-80AB-49DF-9E6B-62B4F4BED6D9}
// *********************************************************************//
  IReportManX = interface(IDispatch)
    ['{08D4AA91-80AB-49DF-9E6B-62B4F4BED6D9}']
    function Execute: WordBool; safecall;
    procedure PrinterSetup; safecall;
    function ShowParams: WordBool; safecall;
    procedure SaveToPDF(const filename: WideString); safecall;
    function PrintRange(frompage: Integer; topage: Integer; copies: Integer; collate: WordBool): WordBool; safecall;
    function Get_Filename: WideString; safecall;
    procedure Set_Filename(const Value: WideString); safecall;
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
    property Filename: WideString read Get_Filename write Set_Filename;
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
  end;

// *********************************************************************//
// DispIntf:  IReportManXDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {08D4AA91-80AB-49DF-9E6B-62B4F4BED6D9}
// *********************************************************************//
  IReportManXDisp = dispinterface
    ['{08D4AA91-80AB-49DF-9E6B-62B4F4BED6D9}']
    function Execute: WordBool; dispid 1;
    procedure PrinterSetup; dispid 2;
    function ShowParams: WordBool; dispid 3;
    procedure SaveToPDF(const filename: WideString); dispid 4;
    function PrintRange(frompage: Integer; topage: Integer; copies: Integer; collate: WordBool): WordBool; dispid 5;
    property Filename: WideString dispid 6;
    property Preview: WordBool dispid 7;
    property ShowProgress: WordBool dispid 8;
    property ShowPrintDialog: WordBool dispid 9;
    property Title: WideString dispid 10;
    property Language: Integer dispid 11;
    property DoubleBuffered: WordBool dispid 12;
    property AlignDisabled: WordBool readonly dispid 13;
    property VisibleDockClientCount: Integer readonly dispid 14;
    function DrawTextBiDiModeFlagsReadingOnly: Integer; dispid 16;
    property Enabled: WordBool dispid -514;
    procedure InitiateAction; dispid 17;
    function IsRightToLeft: WordBool; dispid 18;
    function UseRightToLeftReading: WordBool; dispid 21;
    function UseRightToLeftScrollBar: WordBool; dispid 22;
    property Visible: WordBool dispid 23;
    property Cursor: Smallint dispid 24;
    property HelpType: TxHelpType dispid 25;
    property HelpKeyword: WideString dispid 26;
    procedure SetSubComponent(IsSubComponent: WordBool); dispid 28;
    procedure AboutBox; dispid -552;
  end;

// *********************************************************************//
// DispIntf:  IReportManXEvents
// Flags:     (0)
// GUID:      {BF1380A3-DE22-4675-BE8F-6F877A9CE402}
// *********************************************************************//
  IReportManXEvents = dispinterface
    ['{BF1380A3-DE22-4675-BE8F-6F877A9CE402}']
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
  public
    function Execute: WordBool;
    procedure PrinterSetup;
    function ShowParams: WordBool;
    procedure SaveToPDF(const filename: WideString);
    function PrintRange(frompage: Integer; topage: Integer; copies: Integer; collate: WordBool): WordBool;
    function DrawTextBiDiModeFlagsReadingOnly: Integer;
    procedure InitiateAction;
    function IsRightToLeft: WordBool;
    function UseRightToLeftReading: WordBool;
    function UseRightToLeftScrollBar: WordBool;
    procedure SetSubComponent(IsSubComponent: WordBool);
    procedure AboutBox;
    property  ControlInterface: IReportManX read GetControlInterface;
    property  DefaultInterface: IReportManX read GetControlInterface;
    property DoubleBuffered: WordBool index 12 read GetWordBoolProp write SetWordBoolProp;
    property AlignDisabled: WordBool index 13 read GetWordBoolProp;
    property VisibleDockClientCount: Integer index 14 read GetIntegerProp;
    property Enabled: WordBool index -514 read GetWordBoolProp write SetWordBoolProp;
    property Visible: WordBool index 23 read GetWordBoolProp write SetWordBoolProp;
  published
    property Filename: WideString index 6 read GetWideStringProp write SetWideStringProp stored False;
    property Preview: WordBool index 7 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowProgress: WordBool index 8 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowPrintDialog: WordBool index 9 read GetWordBoolProp write SetWordBoolProp stored False;
    property Title: WideString index 10 read GetWideStringProp write SetWideStringProp stored False;
    property Language: Integer index 11 read GetIntegerProp write SetIntegerProp stored False;
    property Cursor: Smallint index 24 read GetSmallintProp write SetSmallintProp stored False;
    property HelpType: TOleEnum index 25 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property HelpKeyword: WideString index 26 read GetWideStringProp write SetWideStringProp stored False;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'Servers';

implementation

uses ComObj;

procedure TReportManX.InitControlData;
const
  CControlData: TControlData2 = (
    ClassID: '{12B4E008-C646-4FB7-89F1-716BBD8EA06D}';
    EventIID: '';
    EventCount: 0;
    EventDispIDs: nil;
    LicenseKey: nil (*HR:$80040154*);
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

procedure TReportManX.SaveToPDF(const filename: WideString);
begin
  DefaultInterface.SaveToPDF(filename);
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

procedure Register;
begin
  RegisterComponents('ActiveX',[TReportManX]);
end;

end.
