{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       rpdbbrowser                                     }
{       Database broser frame                           }
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

unit rpdbbrowservcl;

interface

{$I rpconf.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  rpdatainfo,rpmdconsts,rpreport,rptypeval,rpparser,
  Dialogs, ComCtrls, ImgList, Menus;

type
  TFRpBrowserVCL = class(TFrame)
    ATree: TTreeView;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    MRefresh: TMenuItem;
    procedure ATreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure ATreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MRefreshClick(Sender: TObject);
  private
    { Private declarations }
    FReport:TRpReport;
    FShowDatasets:Boolean;
    FShowDatabases:Boolean;
    FShowEval:Boolean;
    procedure SetReport(Value:TRpReport);
    procedure InitTree;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    property Report:TRpReport read FReport write SetReport;
    property ShowEval:Boolean read FShowEval write FShowEval;
    property ShowDatasets:Boolean read FShowDatasets write FShowDatasets;
    property ShowDatabases:Boolean read FShowDatabases write FShowDatabases;
  end;

implementation

{$R *.DFM}

procedure TFRpBrowserVCL.SetReport(Value:TRpReport);
begin
 FReport:=Value;
 if Assigned(FReport) then
 begin
  InitTree;
 end
 else
 begin
  ATree.Items.Clear;
 end;
end;

procedure TFRpBrowserVCL.InitTree;
var
 i:integer;
 dbinfo:TRpDatabaseInfoList;
 dinfo:TRpDataInfoList;
 anode,nnode:TTreeNode;
 aiden:TRpIdentifier;
 alist:TStringList;
begin
 ATree.Items.Clear;
 if FShowDatabases then
 begin
  dbinfo:=FReport.DatabaseInfo;
  for i:=0 to dbinfo.Count-1 do
  begin
   anode:=ATree.Items.AddChild(nil,dbinfo.Items[i].Alias);
   anode.Data:=dbinfo.Items[i];
   anode.ImageIndex:=0;
   anode.SelectedIndex:=0;
   // Place an empty child
   ATree.Items.AddChild(anode,'');
  end;
 end;
 if FShowDatasets then
 begin
  dinfo:=FReport.DataInfo;
  for i:=0 to dinfo.Count-1 do
  begin
   anode:=ATree.Items.AddChild(nil,dinfo.Items[i].Alias);
   anode.Data:=dinfo.Items[i];
   anode.ImageIndex:=1;
   anode.SelectedIndex:=1;
   // Place an empty child
   ATree.Items.AddChild(anode,'');
  end;
 end;
 if FShowEval then
 begin
  anode:=ATree.Items.AddChild(nil,SRpVariables);
  anode.ImageIndex:=1;
  anode.SelectedIndex:=1;
  FReport.InitEvaluator;
  FReport.AddReportItemsToEvaluator(FReport.Evaluator);
  alist:=TStringList.Create;
  try
   alist.Sorted:=true;
   for i:=0 to FReport.Evaluator.Identifiers.Count-1 do
   begin
    aiden:=TRpIdentifier(FReport.Evaluator.Identifiers.Objects[i]);
    if Length(aiden.Idenname)>0 then
    begin
     if alist.Indexof(aiden.IdenName)<0 then
     begin
      if aiden is TIdenConstant then
      begin
       alist.Add(aiden.IdenName);
       nnode:=ATree.Items.AddChild(anode,aiden.Idenname);
       nnode.ImageIndex:=2;
       nnode.SelectedIndex:=2;
      end
      else
      begin
       if aiden is TIdenVariable then
       begin
        alist.Add(aiden.IdenName);
        nnode:=ATree.Items.AddChild(anode,aiden.idenname);
        nnode.ImageIndex:=2;
        nnode.SelectedIndex:=2;
       end
       else
       begin
        if aiden is TIdenFunction then
        begin
         if TIdenFunction(aiden).ParamCount=0 then
         begin
          alist.Add(aiden.IdenName);
          nnode:=ATree.Items.AddChild(anode,aiden.Idenname);
          nnode.ImageIndex:=2;
          nnode.SelectedIndex:=2;
         end;
        end;
       end;
      end;
     end;
    end;
   end;
   nnode:=ATree.Items.AddChild(anode,'PAGECOUNT');
   nnode.ImageIndex:=2;
   nnode.SelectedIndex:=2;
  finally
   alist.free;
  end;
 end;
end;

procedure TFRpBrowserVCL.ATreeExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
 dbitem:TRpDatabaseInfoItem;
 ditem:TRpDataInfoItem;
 achild:TTreeNode;
 alist,fieldtypes,fieldsizes:TStringList;
 usebrackets:Boolean;
 i,j:integer;
 aname:string;
begin
 if not assigned(Node.Data) then
  exit;
 if Node.Count<1 then
  exit;
 achild:=Node.Item[0];
 // It's already readed
 if achild.Text<>'' then
  exit;
 try
  if TObject(Node.Data) is TRpDatabaseInfoItem then
  begin
   dbitem:=TRpDatabaseInfoItem(TObject(Node.Data));
   alist:=TStringList.Create;
   fieldtypes:=TStringList.Create;
   fieldsizes:=TStringList.Create;
   try
    // Tables
    Atree.Items.Delete(achild);
    if Node.Parent=nil then
    begin
     dbitem.GetTableNames(alist);
     if alist.count<1 then
      AllowExpansion:=false
     else
     begin
      for i:=0 to alist.Count-1 do
      begin
       aname:=alist.Strings[i];
       if i<fieldtypes.Count then
        aname:=aname+'-'+fieldtypes.Strings[i];
       if i<fieldsizes.Count then
       begin
        if Length(fieldsizes.Strings[i])>0 then
         aname:=aname+'('+fieldsizes.Strings[i]+')';
       end;
       achild:=ATree.Items.AddChild(Node,aname);
       achild.Data:=dbitem;
       achild.ImageIndex:=1;
       achild.SelectedIndex:=1;
       ATree.Items.AddChild(achild,'');
      end;
     end;
    end
    // Fields
    else
    begin
     Atree.Items.Delete(achild);
     dbitem.GetFieldNames(Node.Text,alist,fieldtypes,fieldsizes);
     if alist.count<1 then
      AllowExpansion:=false
     else
     begin
      for i:=0 to alist.Count-1 do
      begin
       achild:=ATree.Items.AddChild(Node,alist.Strings[i]);
       achild.ImageIndex:=2;
       achild.SelectedIndex:=2;
      end;
     end;
    end;
   finally
    alist.free;
    fieldtypes.free;
    fieldsizes.free;
   end;
  end
  else
  begin
   if TObject(Node.Data) is TRpDataInfoItem then
   begin
    ditem:=TRpDataInfoItem(TObject(Node.Data));
    FReport.PrepareParamsBeforeOpen;
    ditem.Connect(Freport.DatabaseInfo,FReport.Params);
    alist:=TStringList.Create;
    fieldtypes:=TStringList.Create;
    fieldsizes:=TStringList.Create;
    try
     Atree.Items.Delete(achild);
     FillFieldsInfo(ditem.Dataset,alist,fieldtypes,fieldsizes);
     if alist.count<1 then
      AllowExpansion:=false
     else
     begin
      for i:=0 to alist.Count-1 do
      begin
       aname:=alist.Strings[i];
       usebrackets:=false;
       for j:=1 to Length(aname) do
       begin
        if Not (aname[j] in ParserSetChars) then
        begin
         usebrackets:=true;
         break;
        end;
       end;
       if usebrackets then
        aname:='['+ditem.Alias+'.'+aname+']'
       else
        aname:=ditem.Alias+'.'+aname;
       if i<fieldtypes.Count then
        aname:=aname+' '+fieldtypes.Strings[i];
       if i<fieldsizes.Count then
       begin
        if Length(fieldsizes.Strings[i])>0 then
         aname:=aname+'('+fieldsizes.Strings[i]+')';
       end;
       achild:=ATree.Items.AddChild(Node,aname);
       achild.Data:=ditem;
       achild.ImageIndex:=2;
       achild.SelectedIndex:=2;
      end;
     end;
    finally
     alist.free;
     fieldtypes.free;
     fieldsizes.free;
    end;
   end;
  end;
 except
  on E:Exception do
  begin
   ShowMessage(E.Message);
   AllowExpansion:=false;
  end;
 end;
end;

constructor TFRpBrowserVCL.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FShowDatasets:=true;
 FShowEval:=true;
 FShowDatabases:=true;
 MRefresh.Caption:=SRpRefresh;
end;


procedure TFRpBrowserVCL.ATreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 anode:TTreeNode;
begin
 if Not Assigned(ATree.Selected) then
  exit;
 anode:=ATree.Selected;
 if Not Assigned(anode.Parent) then
  exit;
 if anode.ImageIndex=2 then
 begin
  BeginDrag(False);
 end;
end;

procedure TFRpBrowserVCL.MRefreshClick(Sender: TObject);
begin
 SetReport(FReport);
end;

end.

