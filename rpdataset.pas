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

uses Sysutils,Classes,DBClient,db;

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

constructor TRpDataSet.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FCopyDataset:=TCLientDataset.Create(Self);
end;

procedure TRpDataSet.DoOpen;
var
 i:integer;
begin
 CreateDataset;
 FCopyDataset.FieldDefs.Assign(FieldDefs);
 FCopyDataset.CreateDataSet;
 FCopyDataset.LogChanges:=false;

 if Assigned(FDataset) then
 begin
  if Not FDataset.Eof then
  begin
   Append;
   try
    for i:=0 to FDataset.FieldCount-1 do
    begin
     Fields[i].Assign(FDataset.Fields[i]);
    end;
    Post;
   except
    Cancel;
    Raise;
   end;
   FCOpyDataset.Append;
   for i:=0 to FDataset.FieldCount-1 do
   begin
    FCopyDataset.Fields[i].Assign(Fields[i]);
   end;
   FCOpyDataset.Post;
   FDataset.Next;
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
    // Copy the record to the copy
  if Not FDataset.Eof then
  begin
   doappend:=true;
   if recordcount>1 then
   begin
    if RecNo=1 then
    begin
     Next;
     Exit;
    end;
    doappend:=false;

    FCopyDataset.Edit;
    for i:=0 to FDataset.FieldCount-1 do
    begin
     FCopyDataset.Fields[i].Assign(Fields[i]);
    end;
    FCopyDataset.Post;
    First;
    Edit;
    for i:=0 to FDataset.FieldCount-1 do
    begin
     Fields[i].Assign(FCopyDataset.Fields[i]);
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
     Fields[i].Assign(FDataset.Fields[i]);
    end;
    Post;
   except
    Cancel;
    Raise;
   end;
   FDataset.Next;
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
 if Assigned(FDataset) then
 begin
  FDataset.FieldDefs.Update;
  FieldDefs.Assign(FDataset.FieldDefs);
 end
 else
 begin
  Active:=False;
 end;
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
