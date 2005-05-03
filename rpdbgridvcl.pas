{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpdbgridvcl                                     }
{                                                       }
{       Grid with decimal separator detection           }
{                                                       }
{       Copyright (c) 1994-2005 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rpdbgridvcl;

interface

{$I rpconf.inc}

uses Windows,Messages,Classes,SysUtils,Grids,DBGrids,
{$IFDEF USEVARIANTS}
 Variants,
{$ENDIF}
 DB;

type
  TRpGridInplaceEdit = class(TInplaceEdit)
   private
   protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
  end;

  TRpGrid=class(TDBGrid)
   private
    FTabReturns:Boolean;
    procedure SuprimirData;
   protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState);override;
    procedure KeyPress(var Key: char);override;
    function CreateEditor: TInplaceEdit;override;
   public
    constructor Create(AOwner:TComponent);override;
   published
    property TabReturns:BOolean read FTabReturns write FTabReturns default true;

  end;

implementation

procedure KillMessage(Wnd: HWnd; Msg: Integer);
// Delete the requested message from the queue, but throw back
// any WM_QUIT msgs that PeekMessage may also return
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

procedure TRpGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
 camp:TField;
begin
 if Key=VK_DELETE then
  SuprimirData;
 // Mirem si la grid es una dbgrid
 camp:=SelectedField;
 if ((camp<>nil)and(camp.DataType in [ftfloat..ftBCD])) then
  if ((Key=VK_DECIMAL) AND (VK_DECIMAL<>Ord(DecimalSeparator))) then
  begin
           KillMessage(Handle, WM_CHAR);
   PostMessage(handle,WM_CHAR,ord(decimalseparator),0);
   Key:=0;
 end;
 inherited Keydown(key,shift);
end;

procedure TRpGrid.KeyUp(var Key: Word; Shift: TShiftState);
var
 camp:TField;
begin
 // Mirem si la grid es una dbgrid
 camp:=SelectedField;
 if ((camp<>nil) and (camp.DataType in [ftfloat..ftBCD])) then
  if ((Key=VK_DECIMAL) AND (VK_DECIMAL<>Ord(DecimalSeparator))) then
    Key:=0;
 inherited KeyUp(key,shift);
end;

procedure TRpGrid.KeyPress(var Key: char);
var
 camp:TField;
begin
 // Mirem si la grid es una dbgrid
 camp:=SelectedField;
 if camp.DataType in [ftfloat..ftBCD] then
  if ((Key=chr(VK_DECIMAL)) AND (VK_DECIMAL<>Ord(DecimalSeparator))) then
    Key:=chr(0);
 if Key<>chr(0) then
  inherited KeyPress(key);
end;

procedure TRpGrid.SuprimirData;
begin
 if SelectedField<>nil then
 begin
  if SelectedField.DataType in [ftTime..ftDateTime] then
  begin
   if Not (SelectedField.Dataset.state in dseditmodes) then
   begin
    if ((SelectedField.Dataset.EOF) AND (SelectedField.Dataset.BOF)) then
     Exit;
    SelectedField.DataSet.edit;
   end;
   SelectedField.AsVariant:=Null;
  end;
 end;
end;

procedure TRpGridInplaceEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
 camp:TField;
begin
 // Mirem si la grid es una dbgrid
 if Grid<>nil then
  if (Grid is TDBGRid) then
  begin
   camp:=(grid As TDBGrid).SelectedField;
   if camp.DataType in [ftfloat..ftBCD] then
    if ((Key=VK_DECIMAL) AND (VK_DECIMAL<>Ord(DecimalSeparator))) then
      Key:=0;
    if TRpGrid(Grid).FTabReturns then
    begin
     if (Key=VK_RETURN) then
     begin
      Key:=VK_TAB;
      inherited KeyUp(Key, Shift);
     end
    end;
  end;
 inherited KeyUp(key,shift);
end;

procedure TRpGridInplaceEdit.Keypress(var Key: char);
var
 camp:TField;
begin
 // Mirem si la grid es una dbgrid
 if Grid<>nil then
  if (Grid is TDBGRid) then
  begin
   camp:=(grid As TDBGrid).SelectedField;
   if camp.DataType in [ftfloat..ftBCD] then
    if ((Key=chr(VK_DECIMAL)) AND (VK_DECIMAL<>Ord(DecimalSeparator))) then
      Key:=chr(0);
  end;
 if key<>chr(0) then
  inherited KeyPress(key);
end;



procedure TRpGridInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
 data:tdataset;
 camp:tfield;
begin
 // Mirem si la grid es una dbgrid
 if Grid<>nil then
  if (Grid is TDBGRid) then
  begin
   camp:=(grid As TDBGrid).SelectedField;
   if camp.DataType in [ftfloat..ftBCD] then
    if ((Key=VK_DECIMAL) AND (VK_DECIMAL<>Ord(DecimalSeparator))) then
    begin
     KillMessage(Handle, WM_CHAR);
     PostMessage(handle,WM_CHAR,ord(decimalseparator),0);
     Key:=0;
    end;
  end;
  // Tractament dates nules en grid
  if Key=VK_DELETE then
   TRpGrid(Grid).SuprimirData;
  if TRpGrid(Grid).FTabReturns then
  begin
   if Key=VK_RETURN then
   begin
    Key:=VK_TAB;
    inherited KeyDown(Key, Shift);
   end
   else
   begin
    if ((Key=VK_DELETE) and (ssCtrl in shift)) then
    begin
     if assigned(TRpGrid(Grid).datasource) then
      if assigned(TRpGrid(Grid).datasource.dataset) then
      begin
       data:=TRpGrid(Grid).datasource.dataset;
       if (Not (data.eof and data.bof)) then
        data.delete;
      end;
    end
    else
     inherited KeyDown(Key, Shift);
   end;
  end
  else
  inherited KeyDown(Key, Shift);
end;

constructor TRpGrid.Create(AOwner:TComponent);
begin
 inherited Create(AOWner);
 FTabReturns:=true;
end;

function TRpGrid.CreateEditor: TInplaceEdit;
begin
 Result := TRpGridInplaceEdit.Create(Self);
end;


end.
