{*******************************************************}
{                                                       }
{       Rpalias                                         }
{       TRpAlias: A component that stores relations     }
{       between a name (alias) and datasets             }
{                                                       }
{       The utility is to use the expresion evaluator   }
{       With syntax alias.field                         }
{       Report Manager                                  }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{*******************************************************}


unit rpalias;

interface

uses SysUtils,Classes,DB,TypInfo,
  rptypeval,rpdatainfo;
type
  // Forward definitions
  TRpAliaslist=class;
  TRpAlias=class;



  // TRpAlias component non visual component that
  // can store the list relation between aliases and
  // datasets
  TRpAlias=Class(TComponent)
   private
    Iden:TIdenField;
    FList:TRpAliaslist;
    FConnections:TRpDatabaseInfoList;
    procedure SetList(Newlist:TRpAliaslist);
    procedure SetConnections(Newconn:TRpDatabaseInfoList);
   protected
    procedure Notification(AComponent:TComponent;Operation:TOperation);override;
   public
    constructor Create(AOWner:TComponent);override;
    destructor Destroy;override;
    function searchfield(aname,datasetname:ShortString;var duplicated:Boolean):TRpIdentifier;
    procedure fillwithfields(lines:TStrings);
    function IndexOf(Dataset:TDataSet):integer;
   published
    property List:TRpAliaslist read FList write SetList;
    property Connections:TRpDatabaseInfoList read FConnections write SetConnections;
   end;


  // Item of  TRpAliaslist
  TRpAliaslistItem=class(TCollectionitem)
   private
    FDataset:TDataSet;
    FAlias:string;
    procedure SetAlias(NewAlias:string);
    function GetDataset:TDataSet;
    procedure SetDataset(NewDataset:TDataSet);
   public
    Constructor Create(Collection:TCollection);override;
    procedure Assign(Source:TPersistent);override;
   published
    property Alias:string read FAlias write SetAlias;
    property Dataset:TDataSet read GetDataset write SetDataset;
   end;

  // TRpAliaslistItem Collection
  TRpAliaslist=Class(TCollection)
   private
    FRpAlias:TRpAlias;
    function GetItem(Index:Integer):TRpAliaslistItem;
    procedure SetItem(index:integer;Value:TRpAliaslistItem);
   public
    constructor Create(RpAlias1:TRpAlias);
    function Add:TRpAliaslistItem;
    function indexof(alias:string):integer;
    property Items[index:integer]:TRpAliaslistitem read GetItem write SetItem;default;
  end;


implementation


// TRpAliaslistItem

Constructor TRpAliaslistItem.Create(Collection:TCollection);
begin
 inherited Create(Collection);
 FDataset:=nil;
 FAlias:=chr(0);
end;

function TRpAliaslistItem.GetDataset:TDataSet;
var aname:string;
begin
 if FDataset=nil then
 begin
  Result:=fDataset;
  Exit;
 end;
 aname:='';
 aname:=aname+FDataset.Name;
 Result:=FDataset;
end;

// When a component is removed we need to remove the reference
procedure TRpAlias.Notification(AComponent:TComponent;Operation:TOperation);
var i:integer;
begin
 inherited Notification(AComponent,Operation);
 if Operation=OpRemove then
 begin
  if (AComponent is TDataset) then
  begin
   with FList do
   begin
    for i:=0 to Count -1 do
    begin
     if items[i]<>nil then
      if Items[i].Dataset=AComponent then
       Items[i].Dataset:=nil;
    end;
   end;
  end;
 end;
end;

procedure TRpAliaslistItem.SetAlias(NewAlias:string);
begin
 if FAlias<>NewAlias then
 begin
  FAlias:=AnsiUpperCase(NewAlias);
  Changed(False);
 end;
end;

procedure TRpAliaslistItem.SetDataset(NewDataset:TDataSet);
begin
 if FDataset<>NewDataset then
 begin
  FDataset:=NewDataset;
  Changed(False);
 end;
end;

procedure TRpAliaslistItem.Assign(Source:TPersistent);
begin
 if Source is TRpAliaslistItem then
 begin
  Alias:=TRpAliaslistItem(Source).Alias;
  Dataset:=(Source As TRpAliaslistItem).Dataset;
  Exit;
 end;
 inherited Assign(Source);
end;

// TRpAliaslist

constructor TRpAliaslist.Create(RpAlias1:TRpAlias);
begin
 inherited Create(TRpAliaslistItem);
 FRpAlias:=RpAlias1;
end;

function TRpAliaslist.Add:TRpAliaslistItem;
begin
 // Then function is defined by teh class TCollectionItem
 Result:=TRpAliaslistItem(inherited Add);
end;

function TRpAliaslist.GetItem(Index:Integer):TRpAliaslistItem;
begin
 // Then function is defined by teh class TCollectionItem
 Result:=TRpAliaslistItem(inherited GetItem(index));
end;

procedure TRpAliaslist.SetItem(index:integer;Value:TRpAliaslistItem);
begin
 // Then function is defined by teh class TCollectionItem
 inherited SetItem(Index,Value);
end;

function TRpAliaslist.indexof(alias:string):integer;
var
 i:integer;
begin
 Result:=-1;
 i:=0;
 While i<count do
 begin
  if Uppercase(items[i].alias)=Uppercase(alias) then
  begin
   Result:=i;
   break;
  end;
  inc(i);
 end;
end;


// TRpAlias

constructor TRpAlias.Create(AOWner:TComponent);
begin
 inherited Create(AOWner);
 FList:=TRpAliaslist.Create(Self);
 Iden:=TIdenField.CreateField(Self,'');
 FConnections:=TRpDatabaseInfoList.Create(Self);
end;

destructor TRpAlias.Destroy;
begin
 FList.free;
 FConnections.free;
 inherited Destroy;
end;

procedure TRpAlias.SetList(Newlist:TRpAliaslist);
begin
 FList.Assign(Newlist);
end;

procedure TRpAlias.SetConnections(Newconn:TRpDatabaseInfoList);
begin
 FConnections.Assign(Newconn);
end;

// Seartching a field in the List
function TRpAlias.searchfield(aname,datasetname:ShortString;var duplicated:Boolean):TRpIdentifier;
var i:integer;
    found:Boolean;
    Field:TField;
    Dataset:TDataset;
begin
 Result:=nil;
 duplicated:=False;
 found:=False;
 if Length(datasetname)=0 then
 begin
  with List do
  begin
   for i:=0 to count-1 do
   begin
    Dataset:=items[i].Dataset;
    if Dataset<>nil then
    begin
     Field:=Dataset.Findfield(aname);
     if Field<>nil then
     begin
      iden.Field:=Field;
      Result:=iden;
      if found then
      begin
       duplicated:=True;
       break;
      end
      else
       found:=True;
     end;
    end;
   end;
  end;
 end
 else
 begin
  with List do
  begin
   for i:=0 to count-1 do
   begin
    if (AnsiUpperCase(datasetname)=items[i].alias) then
    begin
     Dataset:=items[i].Dataset;
     if Dataset<>nil then
     begin
      Field:=Dataset.Findfield(aname);
      if Field<>nil then
      begin
       iden.Field:=Field;
       result:=iden;
       break;
      end
      else
      begin
       break;
      end;
     end;
    end;
   end;
  end;
 end;
end;

// Fills a string list with the fieldnames in a
// Aliaslist as alias.field
procedure TRpAlias.fillwithfields(lines:TStrings);
var i,j:integer;
    Dataset:TDataSet;
begin
 lines.clear;
 for i:=0 to List.Count-1 do
 begin
  Dataset:=List.items[i].Dataset;
  if Dataset<>nil then
  begin
   for j:=0 to Dataset.FieldCount-1 do
   begin
    lines.Add(List.items[i].alias+'.'+Dataset.Fields[j].fieldName);
   end;
  end;
 end;
end;

// Index of a dataset in a list
function TRpAlias.IndexOf(Dataset:TDataSet):integer;
var
 i:integer;
 found:Boolean;
begin
 i:=0;
 found:=False;
 While i<List.Count do
 begin
  if List.items[i].Dataset=Dataset then
  begin
   found:=true;
   break;
  end;
  Inc(i);
 end;
 if found then
  Result:=i
 else
  Result:=-1;
end;


end.
