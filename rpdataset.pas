{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpdataset                                       }
{                                                       }
{                                                       }
{       A client dataset with two record buffer         }
{       can obtain large amounts of recors without      }
{       consuming memory                                }
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

unit rpdataset;

interface

{$I rpconf.inc}

uses Sysutils,Classes,DBClient,
 db;

// A client dataset with two record buffer obtaining records from
resourcestring
 SRpDatasetActive='Operation not allowed in a open dataset';
 SRpInvOpRpdata='Invalid operation for TRpDataset';

type
 TRpDataset=class(TClientDataSet)
  private
   FDataset:TDataset;
   FCopyDataset:TClientDataset;
   procedure SetDataset(Value:TDataset);
  protected
   procedure DoAfterOpen;override;
   procedure Notification(AComponent: TComponent;
    Operation: TOperation);override;
  public
   procedure DoOpen;
   procedure DoClose;
   procedure DoNext;
   procedure DoPrior;
   constructor Create(AOwner:TComponent);override;
  published
   property Dataset:TDataset read FDataset write SetDataset;
 end;

implementation

// Assign field source to destinaton (Assign not works well in Delphi 7)
// The bug appears when you edit a already assigned record and assign
// the blobfield, it clears the preceder field
// Sample4.rep, uses biolife.gdb and reproduce the bug in Delphi7
procedure AssignField(Source,Destination:TField);
{$IFDEF BLOBSTREAMBUG}
var
 bwstream:TStream;
 brstream:TStream;
 memstream:TMemoryStream;
{$ENDIF}
begin
 {$IFNDEF BLOBSTREAMBUG}
   Destination.Assign(Source);
 {$ENDIF}
 {$IFDEF BLOBSTREAMBUG}
   if (Destination is TBlobField) then
   begin
    Destination.Clear;
    bwstream:=Destination.DataSet.CreateBlobStream(Destination,bmWrite);
    try
     brstream:=Source.DataSet.CreateBlobStream(Source,bmRead);
     try
      memstream:=TMemoryStream.Create;
      try
       memstream.SetSize(brStream.Size);
       brStream.Read(memstream.memory^,memstream.Size);
       memstream.Seek(0,soFromBeginning);
       bwStream.Write(memstream.memory^,memstream.size);
      finally
       memstream.free;
      end;
     finally
      brstream.Free;
     end;
    finally
     bwstream.free;
    end;
   end
   else
   begin
    Destination.Assign(Source);
   end;
 {$ENDIF}
end;


constructor TRpDataSet.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FCopyDataset:=TCLientDataset.Create(Self);
end;

procedure TRpDataSet.DoOpen;
var
 i:integer;
 adef:TFieldDef;
begin
 if Assigned(FDataset) then
 begin
//  FDataset.FieldDefs.Update;
//  FieldDefs.Assign(FDataset.FieldDefs);
  FieldDefs.Clear;
  for i:=0 to FDataset.FieldCount-1 do
  begin
   adef:=FieldDefs.AddFieldDef;
   adef.Name:=FDataset.Fields[i].FieldName;
   adef.DataType:=FDataset.Fields[i].DataType;
   adef.Size:=FDataset.Fields[i].Size;
{$IFDEF USEBCD}
   if (FDataset.Fields[i] is TBCDField) then
    adef.Precision:=TBCDField(FDataset.Fields[i]).Precision;
{$ENDIF}
  end;
  CreateDataset;
  FCopyDataset.FieldDefs.Assign(FieldDefs);
  FCopyDataset.CreateDataSet;
  FCopyDataset.LogChanges:=false;
  if Not FDataset.Eof then
  begin
   Append;
   try
    for i:=0 to FDataset.FieldCount-1 do
    begin
     AssignField(FDataset.Fields[i],Fields[i]);
    end;
    Post;
   except
    Cancel;
    Raise;
   end;
  end;
 end;
end;

procedure TRpDataSet.DoClose;
begin
 FCopyDataset.Close;
 Close;
end;

procedure TRpDataSet.DoNext;
var
 i:integer;
 doappend:boolean;
begin
 if Assigned(FDataset) then
 begin
  if (Not ((recordcount>1) and (RecNo=1))) then
   FDataset.Next;
    // Copy the record to the copy
  if Not FDataset.Eof then
  begin
   disablecontrols;
   try
    doappend:=true;
    if recordcount>1 then
    begin
     if RecNo=1 then
     begin
      Next;
      // Exit because a prior sentence has been done before
      exit;
     end;
     doappend:=false;

     FCopyDataset.Edit;
     for i:=0 to FDataset.FieldCount-1 do
     begin
      AssignField(Fields[i],FCopyDataset.Fields[i]);
     end;
     FCopyDataset.Post;
     First;
     Edit;
     for i:=0 to FDataset.FieldCount-1 do
     begin
      AssignField(FCopyDataset.Fields[i],Fields[i]);
     end;
     Post;
     Last;
    end;
    if doappend then
     Append
    else
     Edit;
    try
     for i:=0 to FDataset.FieldCount-1 do
     begin
      AssignField(FDataset.Fields[i],Fields[i]);
     end;
     Post;
    except
     Cancel;
     Raise;
    end;
   finally
    enablecontrols;
   end;
  end
  else
  begin
   Next;
  end;
 end;
end;

procedure TRpDataSet.DoPrior;
begin
 if RecordCount<2 then
  Raise Exception.Create(SRpInvOpRpdata);
 if Recno<>2 then
  Raise Exception.Create(SRpInvOpRpdata);
 Prior;
end;


procedure TRpDataset.SetDataset(Value:TDataset);
begin
 if Active then
  Raise Exception.Create(SRpDatasetActive);
 FDataset:=Value;
 if Not Assigned(FDataset) then
  Active:=False;
end;

procedure TRpDataset.Notification(AComponent: TComponent; Operation: TOperation);
begin
 if Assigned(FDataset) then
  if Operation=opremove then
   if AComponent=FDataset then
    FDataset:=nil;
 inherited Notification(AComponent,Operation);
end;


procedure TRpDataSet.DoAfterOpen;
begin
 inherited DoAfterOpen;
 LogChanges:=false;
end;


end.
