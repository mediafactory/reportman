{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       fdesign                                         }
{       Design frame of the Main form                   }
{       Used by a subreport                             }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit fdesign;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs, QMenus,
  QTypes, QExtCtrls,frpstruc,rpobinsint,rpreport,
{$IFNDEF PROFILE}  fsectionint,rpsubreport,rpsection, rpruler,rpobjinsp;{$ENDIF}
{$IFDEF PROFILE}  fsectionint,rpsubreport,rpsection, rpruler,rpobjinsp ,Proftimx;{$ENDIF}

const
 CONS_RULER_LEFT=20;
type
  TFDesignFrame = class(TFrame)
    PTop: TPanel;
    TopRuler: TRpRuler;
    PLeft: TPanel;
    LeftRuler: TRpRuler;
    SectionScrollBox: TScrollBox;
    PSection: TPanel;
  private
    { Private declarations }
    FReport:TRpReport;
    FObjInsp:TFObjInsp;
    FSectionInterface:TRpSectionInterface;
    procedure SectionDestroy(Sender:TObject);
    procedure SetReport(Value:TRpReport);
    procedure SecPosChange(Sender:TObject);
  public
    { Public declarations }
    freportstructure:TFRpStructure;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure UpdateSelection(force:boolean);
    procedure UpdateInterface;
    property Report:TRpReport read FReport write SetReport;
    property ObjInsp:TFObjInsp read FObjInsp write FObjInsp;
  end;


implementation

{$R *.xfm}

uses fmain;

constructor TFDesignFrame.Create(AOwner:TComponent);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,27; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 inherited Create(AOwner);

{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,27; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

destructor TFDesignFrame.Destroy;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,28; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 inherited Destroy;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,28; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


procedure TFDesignFrame.SetReport(Value:TRpReport);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,29; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 FReport:=Value;
 if Not Assigned(FReport) then
  exit;
 UpdateSelection(false);
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,29; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;


procedure TFDesignFrame.UpdateInterface;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,30; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 if Assigned(FSectionInterface) then
 begin
  FSectionInterface.UpdatePos;
  TopRuler.Width:=FSectionInterface.Width;
  LeftRuler.Height:=FSectionInterface.Height;
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,30; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDesignFrame.UpdateSelection(force:boolean);
var
 data:Pointer;
 dataobj:TOBject;
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,31; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 if Assigned(FSectionInterface) then
 begin
  if assigned(fobjinsp) then
  begin
   if ((freportstructure.FindSelectedObject=FSectionInterface.printitem)
    and (not force)) then
    exit;
  end;
  FSectionInterface.free;
  FSectionInterface:=nil;
  TopRuler.Visible:=False;
  LeftRuler.Visible:=False;
  FObjInsp.CompItem:=nil;
  SectionScrollBox.HorzScrollBar.Position:=0;
  SectionScrollBox.VertScrollBar.Position:=0;
 end;
 if Not Assigned(freportstructure) then
  exit;
 if Not Assigned(freportstructure.RView.Selected) then
  exit;
 data:=freportstructure.RView.Selected.Data;
 if Not Assigned(data) then
  exit;
 dataobj:=TObject(data);
 if (dataobj is TRpSubReport) then
 begin
  if assigned(fobjinsp) then
  begin
   fobjinsp.CompItem:=nil;
  end;
  exit;
 end;
 if (dataobj is TRpSection) then
 begin
  FSectionInterface:=TRpSectionInterface.Create(Self,TRpSection(dataobj));
  FSectionInterface.OnDestroy:=SectionDestroy;
  FSectionInterface.OnPosChange:=SecPosChange;
  FSectionInterface.fobjinsp:=FObjInsp;
  FSectionInterface.Parent:=PSection;
  FSectionInterface.freportstructure:=freportstructure;
  PSection.Left:=0;
  PSection.TOp:=0;
  FSectionInterface.Top:=0;
  FSectionInterface.Left:=0;
  TopRuler.Width:=FSectionInterface.Width;
  LeftRuler.Height:=FSectionInterface.Height;
  TopRuler.Visible:=true;
  LeftRuler.Visible:=true;
  FSectionInterface.CreateChilds;
  if assigned(fobjinsp) then
  begin
   fobjinsp.CompItem:=FSectionInterface;
  end;
{$IFDEF MSWINDOWS}
  Application.ProcessMessages;
{$ENDIF}
  SectionScrollBox.HorzScrollBar.Position:=0;
  SectionScrollBox.VertScrollBar.Position:=0;
{$IFDEF MSWINDOWS}
  if Assigned(FSectionInterface) then
   FSectionInterface.InvalidateAll;
{$ENDIF}
 end;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,31; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDesignFrame.SecPosChange(Sender:TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,32; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 TopRuler.Left:=CONS_RULER_LEFT-SectionScrollBox.HorzScrollBar.Position;;
 LeftRuler.Top:=-SectionScrollBox.VertScrollBar.Position;;
 PSection.Height:=FSectionInterface.Height;
 PSection.Width:=FSectionInterface.Width;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,32; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

procedure TFDesignFrame.SectionDestroy(Sender:TObject);
begin
{$IFDEF PROFILE}asm DW 310FH; call Proftimx.ProfStop; end; Try; asm mov edx,33; mov eax,self; call Proftimx.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
 FSectionInterface:=nil;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,33; call Proftimx.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

end.
