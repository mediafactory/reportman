{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpsection                                       }
{       TRpSection - Report manager Band                }
{       The class representation of a reporting section }
{       It can be a detail, a pageheader...             }
{       It has childs, that prints                      }
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

unit rpsection;

interface

{$I rpconf.inc}

uses Classes,
{$IFDEF USEZLIB}
rpmzlib,
{$ENDIF}
{$IFDEF MSWINDOWS}
 windows,
{$ENDIF}
{$IFDEF USEVARIANTS}
 Types,Variants,
{$ENDIF}
 rptypes,rpmdconsts,rpmunits,rpprintitem,rplabelitem,db,
 sysutils,rpmetafile,rpeval;

const
 C_DEFAULT_SECTION_WIDTH=19;
 C_DEFAULT_SECTION_HEIGHT=3;

type
 TRpSectionType=(rpsecpheader,rpsecgheader,
  rpsecdetail,rpsecgfooter,rpsecpfooter);

 TRpSkipType=(secskipdefault,secskipbefore,secskipafter);


 TRpSection=class(TRpCommonComponent)
  private
   FSubReport:TComponent;
   FChildSubReport:TComponent;
   FGroupName:String;           // [rpsecgheader,rpsecgfooter]
   FChangeExpression:WideString;// [rpsecgheader,rpsecgfooter]
   FChangeBool:boolean;         // [rpsecgheader,rpsecgfooter]
   FPageRepeat:boolean;         // [rpsecgheader,rpsecgfooter]
   FAlignBottom:boolean;        // [rpsecrheader,rpsecrfooter,rpsecgheader,rpsecgfooter,rpsecdetail]
   FSkipPage:boolean;           // [rpsecrheader,rpsecrfooter,rpsecgheader,rpsecgfooter,rpsecdetail]
   FSectionType:TRpSectionType;
   FReportComponents:TRpCommonList;
   FAutoExpand:Boolean;
   FAutoContract:Boolean;
   FHorzDesp:Boolean;
   FVertDesp:Boolean;
   FExternalFilename:string;
   FExternalConnection:String;
   FExternalTable:String;
   FExternalField:String;
   FExternalSearchField:String;
   FExternalSearchValue:String;
   FBeginPageExpression:widestring;
   FFooterAtReportEnd:boolean;
   FSkipRelativeV:Boolean;
   FSkipRelativeH:Boolean;
   FSkipExpreH:WideString;
   FSkipExpreV:WideString;
   FSkipToPageExpre:WideString;
   FIniNumPage:Boolean;
   // deprecated
   FBeginPage:boolean;
   FReadError:Boolean;
   FSkipType:TRpSkipType;
   // Global headers
   FGlobal:Boolean;
   procedure SetReportComponents(Value:TRpCommonList);
   procedure SetGroupName(Value:string);
   procedure SetChangeExpression(Value:widestring);
   procedure OnReadError(Reader: TReader; const Message: string; var Handled: Boolean);
   procedure SetChildSubReport(Value:TComponent);
   procedure AssignSection(sec:TRpSection);
   procedure WriteChangeExpression(Writer:TWriter);
   procedure ReadChangeExpression(Reader:TReader);
   procedure WriteBeginPageExpression(Writer:TWriter);
   procedure ReadBeginPageExpression(Reader:TReader);
   procedure WriteSkipExpreV(Writer:TWriter);
   procedure WriteSkipExpreH(Writer:TWriter);
   procedure WriteSkipToPageExpre(Writer:TWriter);
   procedure ReadSkipExpreH(Reader:TReader);
   procedure ReadSkipExpreV(Reader:TReader);
   procedure ReadSkipToPageExpre(Reader:TReader);
   procedure LoadExternalFromDatabase;
   procedure SetIniNumPage(Value:Boolean);
   function GetIsExternal:Boolean;
  protected
   procedure DoPrint(adriver:IRpPrintDriver;aposx,aposy:integer;metafile:TRpMetafileReport;
    MaxExtent:TPoint;var PartialPrint:Boolean);override;
   procedure DefineProperties(Filer:TFiler);override;
   procedure GetChildren(Proc: TGetChildProc; Root: TComponent);override;
   procedure Notification(AComponent: TComponent;
    Operation: TOperation);override;
   function GetOwnedComponent(index:Integer):TComponent;
   procedure SetPageRepeat(Value:Boolean);
  protected
   procedure Loaded;override;
  public
   GroupValue:Variant;
   constructor Create(AOwner:TComponent);override;
   destructor Destroy;override;
   function SectionCaption(addchild:boolean):WideString;
   procedure FreeComponents;
   procedure DeleteComponent(com:TRpCommonComponent);
   function GetExtension(adriver:IRpPrintDriver;MaxExtent:TPoint):TPoint;override;
   function EvaluateBeginPage:boolean;
   procedure LoadFromStream(stream:TStream);
   procedure SaveToStream(stream:TStream);
   procedure LoadExternal;
   procedure SaveExternal;
   procedure SaveExternalToDatabase;
   function GetExternalDataDescription:String;
   procedure GetChildSubReportPossibleValues(lvalues:TRpWideStrings);
   function AddComponent(componentclass:TRpCommonPosClass):TRpCommonPosComponent;
   function GetChildSubReportName:string;
   procedure SetChildSubReportByName(avalue:String);
   property ChangeExpression:widestring read FChangeExpression write SetChangeExpression;
   property BeginPageExpression:widestring read FBeginPageExpression
    write FBeginPageExpression;
   property SkipToPageExpre:WideString read FSkipToPageExpre
    write FSkipToPageExpre;
   property SkipExpreH:Widestring read FSkipExpreH write FSkipExpreH;
   property SkipExpreV:Widestring read FSkipExpreV write FSkipExpreV;
   property ReportComponents:TRpCommonList read FReportComponents;
   property OwnedComponents[index:integer]:TComponent read GetOwnedComponent;
   property IsExternal:Boolean read GetIsExternal;
  published
   property SubReport:TComponent read FSubReport write FSubReport;
   property GroupName:String read FGroupName write SetGroupName;
   property ChangeBool:boolean read FChangeBool write FChangeBool;
   property PageRepeat:boolean read FPageRepeat write SetPageRepeat;
   property SkipPage:boolean read FSkipPage write FSkipPage;
   property AlignBottom:boolean read FAlignBottom write FAlignBottom;
   property SectionType:TRpSectionType read FSectionType write FSectionType;
   property Components:TRpCommonList read FReportComponents write SetReportComponents;
   property AutoExpand:Boolean read FAutoExpand write FAutoExpand
    default false;
   property AutoContract:Boolean read FAutoContract write FAutoContract
    default false;
   property HorzDesp:Boolean read FHorzDesp write FHorzDesp default false;
   property VertDesp:Boolean read FVertDesp write FVertDesp default false;
//   property IsExternal:Boolean read FIsExternal
//    write FIsExternal default false;
   // External filename is a alias.field or if not exists a filename
   // If it's length is 0 it's not external
   property ExternalFilename:string read FExternalFilename
    write FExternalFilename;
   property ExternalConnection:string read FExternalConnection
    write FExternalConnection;
   property ExternalTable:string read FExternalTable write FExternalTable;
   property ExternalField:string read FExternalField write FExternalField;
   property ExternalSearchField:string read FExternalSearchField
    write FExternalSearchField;
   property ExternalSearchValue:string read FExternalSearchValue
    write FExternalSearchValue;

   property ChildSubReport:TComponent read FChildSubReport write SetChildSubReport;
   // Deprecated properties for compatibility only
   property BeginPage:boolean read FBeginpage write FBeginPage default false;
   property FooterAtReportEnd:boolean read FFooterAtReportEnd write
    FFooterAtReportEnd default true;
   // Skip inside page before print
   property SkipRelativeH:boolean Read FSkipRelativeH write FSkipRelativeH default false;
   property SkipRelativeV:boolean Read FSkipRelativeV write FSkipRelativeV default false;
   property SkipType:TRpSkipType read FSkipType write FSkipType default secskipdefault;
   property IniNumPage:Boolean read FIniNumPage write SetIniNumPage default false;
   // Global page headers and footers
   property Global:Boolean read FGlobal write FGlobal default false;
 end;


function RpSkipTypeToText(value:TRpSkipType):String;
function StringToRpSkipType(value:String):TRpSkipType;
procedure GetSkipTypePossibleValues(alist:TRpWideStrings);

implementation

uses rpsubreport,rpbasereport, Math;

constructor TRpSection.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FSkipType:=secskipdefault;
 FReportComponents:=TRpCommonList.Create(Self);
 FExternalTable:='REPMAN_REPORTS';
 FExternalField:='REPORT';
 FExternalSearchField:='REPORT_NAME';
 FFooterAtReportEnd:=true;

 Width:=Round(C_DEFAULT_SECTION_WIDTH*TWIPS_PER_INCHESS/CMS_PER_INCHESS);
 Height:=Round(C_DEFAULT_SECTION_HEIGHT*TWIPS_PER_INCHESS/CMS_PER_INCHESS);
end;

procedure TRpSection.SetReportComponents(Value:TRpCommonList);
begin
 FReportComponents.Assign(Value);
end;

destructor TRpSection.Destroy;
begin
 FReportComponents.Free;
 inherited destroy;
end;

procedure TRpSection.Notification(AComponent: TComponent;
 Operation: TOperation);
begin
 inherited Notification(AComponent,Operation);

 if Operation=opRemove then
 begin
  if (AComponent is TRpSubReport) then
  begin
   if AComponent=FChildSubReport then
    FChildSubReport:=nil;
  end;
 end;
end;

function TRpSection.SectionCaption(addchild:boolean):WideString;
begin
 case FSectionType of
  rpsecdetail:
   begin
    Result:=SRpDetail;
   end;
  rpsecpheader:
   begin
    Result:=SRpPageHeader;
   end;
  rpsecpfooter:
   begin
    Result:=SRpPageFooter;
   end;
  rpsecgheader:
   begin
    Result:=SRpHeader+' - '+FGroupName;
   end;
  rpsecgfooter:
   begin
    Result:=SRpFooter+' - '+FGroupName;
   end;
 end;
 if addchild then
 begin
  if Assigned(ChildSubreport) then
  begin
   Result:=Result+'('+TRpSubReport(ChildSubReport).GetDisplayName(false)+')';
  end;
 end;
end;

procedure TRpSection.FreeComponents;
var
 i:integer;
begin
 for i:=0 to FReportComponents.Count-1 do
 begin
  FReportComponents.Items[i].Component.free;
 end;
 FReportComponents.Clear;
end;

procedure TRpSection.DeleteComponent(com:TRpCommonComponent);
var
 i:integer;
begin
 i:=0;
 while i<FReportComponents.Count do
 begin
  if FReportComponents.Items[i].Component=Com then
  begin
   com.Free;
   FReportComponents.Items[i].Free;
//   Components.Delete(i);
   break;
  end;
  inc(i);
 end;
end;

procedure TRpSection.SetChangeExpression(Value:widestring);
var
 subrep:TRpSubreport;
 i:integer;
begin
 if (csLoading in ComponentState) then
 begin
  FChangeExpression:=Value;
  exit;
 end;
 if FChangeExpression=Value then
  exit;
 if not assigned(FSubreport) then
 begin
  FChangeExpression:=Value;
  exit;
 end;
 subrep:=TRpSubreport(FSubReport);
 // Assign header and footer
 for i:=0 to subrep.Sections.Count-1 do
 begin
  if (subrep.Sections.Items[i].Section.SectionType in [rpsecgheader,rpsecgfooter]) then
   if subrep.Sections.Items[i].Section.GroupName=FGroupName then
    subrep.Sections.Items[i].Section.FChangeExpression:=Value;
 end;
end;


procedure TRpSection.SetIniNumPage(Value:Boolean);
var
 subrep:TRpSubreport;
 i:integer;
 AGroupName:String;
begin
 if (csLoading in ComponentState) then
 begin
  exit;
 end;
 if not assigned(FSubreport) then
 begin
  FIniNumPage:=Value;
  exit;
 end;
 subrep:=TRpSubreport(FSubReport);
 // Assign header and footer
 AGroupName:=FGroupName;
 for i:=0 to subrep.Sections.Count-1 do
 begin
  if (subrep.Sections.Items[i].Section.SectionType in [rpsecgheader,rpsecgfooter]) then
   if subrep.Sections.Items[i].Section.FGroupName=AGroupName then
    subrep.Sections.Items[i].Section.FIniNumPage:=Value;
 end;
end;


procedure TRpSection.SetGroupName(Value:string);
var
 subrep:TRpSubreport;
 i:integer;
 aexpre:TRpExpression;
 AGroupName:string;
begin
 if (csLoading in ComponentState) then
 begin
  FGroupName:=Value;
  exit;
 end;
 Value:=UpperCase(Value);
 if FGroupName=Value then
  exit;
 if not assigned(FSubreport) then
 begin
  FGroupName:=Value;
  exit;
 end;
 subrep:=TRpSubreport(FSubReport);
 subrep.CheckGroupExists(Value);
 if Length(FGroupName)>0 then
 begin
  for i:=0 to Owner.ComponentCount-1 do
  begin
   if (Owner.Components[i] is TRpExpression) then
   begin
    aexpre:=TRpExpression(Owner.Components[i]);
    if aexpre.GroupName=FGroupName then
     aexpre.GroupName:=Value;
   end;
  end;
 end;
 // Assign header and footer
 AGroupName:=FGroupName;
 for i:=0 to subrep.Sections.Count-1 do
 begin
  if (subrep.Sections.Items[i].Section.SectionType in [rpsecgheader,rpsecgfooter]) then
   if subrep.Sections.Items[i].Section.FGroupName=AGroupName then
    subrep.Sections.Items[i].Section.FGroupName:=Value;
 end;
end;

procedure TRpSection.DoPrint(adriver:IRpPrintDriver;aposx,aposy:integer;metafile:TRpMetafileReport;
    MaxExtent:TPoint;var PartialPrint:Boolean);
var
 i:integer;
 compo:TRpCommonPosComponent;
 newposx,newposy:integer;
 intPartialPrint:Boolean;
 DoPartialPrint:BOolean;
begin
 inherited DoPrint(adriver,aposx,aposy,metafile,MaxExtent,PartialPrint);

 DoPartialPrint:=False;
 // Look for a partial print
 for i:=0 to FReportComponents.Count-1 do
 begin
  compo:=TRpCommonPosComponent(FReportComponents.Items[i].Component);
  if (compo is TRpExpression) then
   if TRpExpression(compo).IsPartial then
   begin
    DoPartialPrint:=true;
    break;
   end;
 end;
 PartialPrint:=false;
 for i:=0 to FReportComponents.Count-1 do
 begin
  compo:=TRpCommonPosComponent(FReportComponents.Items[i].Component);
  if DoPartialPrint then
  begin
   if (compo is TRpExpression) then
    if TRpExpression(Compo).IsPartial then
    begin
    // Alignbottom will change the component position
     if (compo.Align<>rpalnone) then
     begin
      if (compo.Align in [rpalbottom,rpalbotright]) then
      begin
       newposy:=aposy+lastextent.Y-compo.lastextent.Y;
      end
      else
       newposy:=aposy+compo.PosY;
      // Alignbottom will change the component position
      if (compo.Align in [rpalright,rpalbotright]) then
      begin
       newposx:=aposx+lastextent.X-compo.lastextent.X;
      end
      else
       newposx:=aposx+compo.PosX;
     end
     else
     begin
      newposx:=aposx+compo.PosX;
      newposy:=aposy+compo.PosY;
     end;
     IntPartialPrint:=false;
     FReportComponents.Items[i].Component.Print(adriver,newposx,newposy,metafile,
      MaxExtent,IntPartialPrint);
     if IntPartialPrint then
      PartialPrint:=True;
    end;
  end
  else
  begin
   // Evaluates print condition of each comonent
   if compo.EvaluatePrintCondition then
   begin
    // Alignbottom will change the component position
    if (compo.Align<>rpalnone) then
    begin
     if (compo.Align in [rpalbottom,rpalbotright]) then
     begin
      newposy:=aposy+lastextent.Y-compo.lastextent.Y;
     end
     else
      newposy:=aposy+compo.PosY;
     // Alignbottom will change the component position
     if (compo.Align in [rpalright,rpalbotright]) then
     begin
      newposx:=aposx+lastextent.X-compo.lastextent.X;
     end
     else
      newposx:=aposx+compo.PosX;
    end
    else
    begin
     newposx:=aposx+compo.PosX;
     newposy:=aposy+compo.PosY;
    end;
    IntPartialPrint:=false;
    FReportComponents.Items[i].Component.Print(adriver,newposx,newposy,metafile,
     MaxExtent,IntPartialPrint);
    if IntPartialPrint then
     PartialPrint:=True;
   end;
  end;
 end;
end;

function TRpSection.EvaluateBeginPage:boolean;
var
 report:TRpBaseReport;
 eval:TRpEvaluator;
begin
 Result:=false;

 if Length(FBeginPageExpression)<1 then
  exit;
 report:=TRpBaseReport(Owner);
 eval:=report.Evaluator;
 eval.Expression:=FBeginPageExpression;
 eval.Evaluate;
 Result:=eval.EvalResult;
end;


function TRpSection.GetExtension(adriver:IRpPrintDriver;MaxExtent:TPoint):TPoint;
var
 minsize,maxsize,currentsize:integer;
 compsize:TPoint;
 newsize:integer;
 i:integer;
 newextent:TPoint;
 acompo:TRpCommonPosComponent;
 DoPartialPrint:Boolean;
begin
 Result:=inherited GetExtension(adriver,MaxExtent);
 DoPartialPrint:=False;
 // Look for a partial print
 for i:=0 to FReportComponents.Count-1 do
 begin
  acompo:=TRpCommonPosComponent(FReportComponents.Items[i].Component);
  if (acompo is TRpExpression) then
   if TRpExpression(acompo).IsPartial then
   begin
    DoPartialPrint:=true;
    break;
   end;
 end;

 if FAutoContract then
 begin
  minsize:=0;
  currentsize:=0;
 end
 else
 begin
  minsize:=Result.Y;
  currentsize:=Result.Y;
 end;
 if FAutoExpand then
  maxsize:=MaxInt
 else
  maxsize:=Result.X;
 if ((Not FAutoExpand) and (Not FAutoContract)) then
 begin
  Result.Y:=currentsize;
  lastextent:=Result;
  exit;
 end;
 for i:=0 to FReportComponents.Count-1 do
 begin
  acompo:=TRpCommonPosComponent(FReportComponents.Items[i].Component);
  if DoPartialPrint then
  begin
   if (acompo is TRpExpression) then
    if TRpExpression(acompo).IsPartial then
    begin
     newextent:=MaxExtent;
     newextent.Y:=newextent.Y-acompo.PosY;
     compsize:=acompo.GetExtension(adriver,newExtent);
     if compsize.Y>0 then
     begin
      if acompo.Align in [rpalbottom,rpalbotright] then
       newsize:=compsize.Y
      else
       newsize:=acompo.PosY+compsize.Y;
      if newsize<maxsize then
      begin
       if newsize>currentsize then
        currentsize:=newsize;
      end;
     end;
    end;
  end
  else
  begin
   newextent:=MaxExtent;
   newextent.Y:=newextent.Y-acompo.PosY;
   compsize:=acompo.GetExtension(adriver,newExtent);
   if compsize.Y>0 then
   begin
    if acompo.Align in [rpalbottom,rpalbotright] then
     newsize:=compsize.Y
    else
     newsize:=acompo.PosY+compsize.Y;
    if newsize<maxsize then
    begin
     if newsize>currentsize then
      currentsize:=newsize;
    end;
   end;
  end;
 end;
 if currentsize<minsize then
  currentsize:=minsize;
 Result.Y:=currentsize;
 lastextent:=Result;
end;


procedure TRpSection.SaveToStream(stream:TStream);
var
 i:integer;
 acompo:TComponent;
{$IFDEF USEZLIB}
 zstream:TCompressionStream;
{$ENDIF}
 writer:TWriter;
begin
 // Looks if it's not external
 for i:=0 to FReportComponents.Count-1 do
 begin
  acompo:=FReportComponents.Items[i].Component;
  if assigned(acompo) then
  begin
   if (acompo.Owner=Owner) then
   begin
    Owner.RemoveComponent(acompo);
    Self.InsertComponent(acompo);
   end;
  end;
 end;
{$IFDEF USEZLIB}
 zstream:=TCompressionStream.Create(clDefault,stream);
 try
  writer:=TWriter.Create(zStream,4096);
  try
   writer.WriteRootComponent(Self);
  finally
   writer.free;
  end;
 finally
  zstream.free;
 end;
{$ENDIF}
{$IFNDEF USEZLIB}
 writer:=TWriter.Create(Stream,4096);
 try
  writer.WriteRootComponent(Self);
 finally
  writer.free;
 end;
{$ENDIF}
end;


procedure TRpSection.SaveExternal;
var
 AStream:TStream;
begin
 // Saves the components as a external section
 if Length(FExternalFilename)>0 then
 begin
  AStream:=TFileStream.Create(FExternalFilename,fmCreate);
  try
   SaveToStream(AStream);
  finally
   AStream.free;
  end;
 end
 else
 begin
  if Length(GetExternalDataDescription)>0 then
  begin
   SaveExternalToDatabase;
  end;
 end;
end;

procedure TRpSection.SaveExternalToDatabase;
var
 report:TRpBaseReport;
 astream:TStream;
 index:integer;
 sqlsentence:string;
 alist:TStringList;
 aparam:TRpParamObject;
 aparam2:TRpParamObject;
begin
 report:=TRpBaseReport(Owner);
 index:=report.DatabaseInfo.IndexOf(ExternalCOnnection);
 if index<0 then
  Exit;
 sqlsentence:='UPDATE '+ExternalTable+' SET '+
  ExternalField+'=:'+ExternalField+
  ' WHERE '+ExternalSearchField+'=:'+ExternalSearchField;
 alist:=TStringList.Create;
 try
  aparam:=TRpParamObject.Create;
  aparam2:=TRpParamObject.Create;
  try
   aparam.Value:=ExternalSearchValue;
   alist.AddObject(ExternalField,aparam2);
   alist.AddObject(ExternalSearchField,aparam);
   astream:=TMemoryStream.Create;
   try
    aparam2.Stream:=astream;
    SaveToStream(AStream);
    astream.Seek(0,soFromBeginning);
    report.DatabaseInfo.Items[index].OpenDatasetFromSQL(sqlsentence,alist,true);
   finally
    astream.free;
   end;
  finally
   aparam.free;
   aparam2.free;
  end;
 finally
  alist.free;
 end;
end;


procedure TRpSection.LoadFromStream(stream:TStream);
var
 i:integer;
 abyte:Byte;
 reader:TReader;
{$IFDEF USEZLIB}
 buf:pointer;
 zlibs:TDeCompressionStream;
 readed:integer;
{$ENDIF}
 memstream,amemstream:TMemoryStream;
 compressed:boolean;
 tempsec:TRpSection;
begin
 // Free all components
 for i:=0 to FReportComponents.Count-1 do
 begin
  if Assigned(FReportComponents.Items[i].Component) then
   FReportComponents.Items[i].Component.Free;
 end;
 FReportComponents.Clear;
 FReadError:=false;
 aMemStream:=TMemoryStream.Create;
 try
  compressed:=false;
  aMemStream.LoadFromStream(stream);
  amemstream.Seek(0,soFromBeginning);
  if amemstream.Size<1 then
   Raise Exception.Create(SRpStreamFormat);
  amemstream.Read(abyte,1);
  amemstream.Seek(0,soFromBeginning);
  if (Char(abyte)='x') then
   compressed:=true;
{$IFNDEF USEZLIB}
  if compressed then
   Raise Exception.Create(SRpZLibNotSupported);
{$ENDIF}
{$IFDEF USEZLIB}
  if compressed then
  begin
   MemStream:=TMemoryStream.Create;
   try
    zlibs:=TDeCompressionStream.Create(amemstream);
    try
     buf:=AllocMem(120000);
     try
      repeat
       readed:=zlibs.Read(buf^,120000);
       memstream.Write(buf^,readed);
      until readed<120000;
     finally
      freemem(buf);
     end;
     memstream.Seek(0,soFrombeginning);
     reader:=TReader.Create(memstream,1000);
     try
      reader.OnError:=OnReadError;
      tempsec:=TRpSection.Create(nil);
      try
       reader.ReadRootComponent(tempsec);
       AssignSection(tempsec);
      finally
       tempsec.free;
      end;
     finally
      reader.free;
     end;
    finally
     zlibs.Free;
    end;
   finally
    MemStream.free;
   end;
  end
  else
{$ENDIF}
  begin
   MemStream:=TMemoryStream.Create;
   try
    MemStream.LoadFromStream(amemstream);
    memstream.Seek(0,soFrombeginning);
    reader:=TReader.Create(memstream,1000);
    try
     reader.OnError:=OnReadError;
     tempsec:=TRpSection.Create(nil);
     try
      reader.ReadRootComponent(tempsec);
      AssignSection(tempsec);
     finally
      tempsec.free;
     end;
    finally
     reader.free;
    end;
   finally
    MemStream.Free;
   end;
  end;
 finally
  aMemStream.free;
 end;

 if FReadError then
 begin
  for i:=0 to ComponentCount-1 do
  begin
   Components[i].Free;
  end;
  Height:=0;
  exit;
 end;
end;


procedure TRpSection.LoadExternal;
var
 AStream:TStream;
 report:TRpBaseReport;
begin
 report:=TRpBaseReport(Owner);
 try
  // Try to load the section as an external section
  if Length(FExternalFilename)>0 then
  begin
   AStream:=TFileStream.Create(FExternalFilename,fmOpenRead or fmShareDenyWrite);
   try
    LoadFromStream(AStream);
   finally
    AStream.free;
   end;
  end
  else
  begin
   if Length(GetExternalDataDescription)>0 then
   begin
    LoadExternalFromDatabase;
   end;
  end;
 except
  On E:Exception do
  begin
   if report.FailIfLoadExternalError then
    Raise;
  end;
 end;
end;

procedure TRpSection.LoadExternalFromDatabase;
var
 report:TRpBaseReport;
 astream:TStream;
 index:integer;
 sqlsentence:string;
 errordata:boolean;
 alist:TStringList;
 aparam:TRpParamObject;
begin
 report:=TRpBaseReport(Owner);
 index:=report.DatabaseInfo.IndexOf(ExternalCOnnection);
 if index<0 then
  Exit;
 astream:=nil;
 sqlsentence:='SELECT '+ExternalField+' FROM '+ExternalTable+
  ' WHERE '+ExternalSearchField+'=:'+ExternalSearchField;
 alist:=TStringList.Create;
 try
  aparam:=TRpParamObject.Create;
  try
   aparam.Value:=ExternalSearchValue;
   alist.AddObject(ExternalSearchField,aparam);
   errordata:=false;
   try
    astream:=report.DatabaseInfo.Items[index].GetStreamFromSQL(sqlsentence,alist);
   except
    errordata:=True;
   end;
   if not errordata then
   begin
    try
     LoadFromStream(astream);
    finally
     astream.Free;
    end;
   end;
  finally
   aparam.free;
  end;
 finally
  alist.free;
 end;
end;


procedure TRpSection.OnReadError(Reader: TReader;
 const Message: string; var Handled: Boolean);
begin
 // Omit Messages
 FReadError:=true;
end;

// GetChildren helps streaming the subreports
procedure TRpSection.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  OwnedComponent: TComponent;
//  rpsubreport:TRpSubReport;
begin
 inherited GetChildren(Proc, Root);
 if Root = Self then
  for I := 0 to ComponentCount - 1 do
  begin
   OwnedComponent := OwnedComponents[I];
   if not OwnedComponent.HasParent then
    Proc(OwnedComponent);
//   if OwnedComponent is TRpSubReport then
//   begin
//    if subreport.
//      Proc(OwnedComponent);
//   end;
  end;
end;

procedure TRpSection.GetChildSubReportPossibleValues(lvalues:TRpWideStrings);
var
 rep:TRpBaseReport;
 i:integer;
begin
 rep:=TRpBaseReport(Subreport.Owner);
 lvalues.clear;
 lvalues.Add(' ');
 for i:=0 to rep.Subreports.count-1 do
 begin
  lvalues.Add(rep.Subreports.items[i].SubReport.GetDisplayName(false));
 end;
end;

function TRpSection.GetChildSubReportName:string;
begin
 Result:=' ';
 if Assigned(ChildSubReport) then
  Result:=TRpSubReport(ChildSubReport).GetDisplayName(false);
end;

procedure TRpSection.SetChildSubReportByName(avalue:String);
var
 rep:TRpBaseReport;
 i:integer;
begin
 rep:=TRpBaseReport(Subreport.Owner);
 ChildSubReport:=nil;
 for i:=0 to rep.Subreports.count-1 do
 begin
  if rep.Subreports.items[i].SubReport.ParentSection=Self then
  begin
   rep.Subreports.items[i].SubReport.ParentSection:=nil;
   rep.Subreports.items[i].SubReport.ParentSubReport:=nil;
  end;
  if rep.Subreports.items[i].SubReport.GetDisplayName(false)=avalue then
  begin
   rep.Subreports.items[i].SubReport.ParentSection:=Self;
   rep.Subreports.items[i].SubReport.ParentSubReport:=TRpSubReport(SubReport);
   ChildSubReport:=rep.Subreports.items[i].SubReport;
  end;
 end;
 if Assigned(FChildSubReport) then
  FPageRepeat:=False;
end;

procedure TRpSection.SetChildSubReport(Value:TComponent);
var
 i:integer;
 rep:TRpSubReport;
begin
 if (csReading in ComponentState) then
 begin
  FChildSubReport:=Value;
  exit;
 end;
 if (csLoading in ComponentState) then
 begin
  FChildSubReport:=Value;
  exit;
 end;
 if Not Assigned(Value) then
 begin
  FChildSubReport:=Value;
  exit;
 end;
 if Value=SubReport then
  Raise Exception.Create(SRpCircularDatalink);
 rep:=TRpSubReport(Value);
 for i:=0 to rep.Sections.Count-1 do
 begin
  if rep.Sections.Items[i].Section.ChildSubReport=SubReport then
   Raise Exception.Create(SRpCircularDatalink);
 end;
 FChildSubReport:=Value;
 if Assigned(FChildSubReport) then
  FPageRepeat:=False;
end;

procedure TRpSection.AssignSection(sec:TRpSection);
var
 i:integer;
begin
 Width:=sec.Width;
 Height:=sec.Height;
 FSkipPage:=sec.SkipPage;
 FAutoExpand:=sec.FAutoExpand;
 FAutoContract:=sec.FAutoContract;
 FBeginPageExpression:=sec.FBeginPageExpression;
 FSkipRelativeH:=sec.FSkipRelativeH;
 FSkipRelativeV:=sec.FSkipRelativeV;
 FSkipExpreH:=sec.FSkipExpreH;
 FSkipExpreV:=sec.FSkipExpreV;
 FBeginPage:=sec.FBeginPage;
 PrintCondition:=sec.PrintCondition;
 DoBeforePrint:=sec.DoBeforePrint;
 DoAfterPrint:=sec.DoAfterPrint;
 FVertDesp:=sec.FVertDesp;
 FGlobal:=sec.FGlobal;
 FFooterAtReportEnd:=sec.FFooterAtReportEnd;
 FIniNumPage:=sec.FIniNumPage;
 for i:=0 to FReportComponents.Count-1 do
 begin
  if FReportComponents.Items[i].Component.Owner=Self then
   FReportComponents.Items[i].Component.Free;
 end;
 FReportComponents.Clear;
 for i:=0 to sec.FReportComponents.Count-1 do
 begin
  (FReportComponents.Add).Component:=sec.FReportComponents.Items[i].Component;
  sec.RemoveComponent(sec.FReportComponents.Items[i].Component);
  InsertComponent(sec.FReportComponents.Items[i].Component);
 end;
end;


procedure TRpSection.WriteChangeExpression(Writer:TWriter);
begin
 WriteWideString(Writer, FChangeExpression);
end;

procedure TRpSection.ReadChangeExpression(Reader:TReader);
begin
 FChangeExpression:=ReadWideString(Reader);
end;

procedure TRpSection.WriteBeginPageExpression(Writer:TWriter);
begin
 WriteWideString(Writer, FBeginPageExpression);
end;

procedure TRpSection.ReadBeginPageExpression(Reader:TReader);
begin
 FBeginPageExpression:=ReadWideString(Reader);
end;

procedure TRpSection.WriteSkipExpreV(Writer:TWriter);
begin
 WriteWideString(Writer, FSkipExpreV);
end;

procedure TRpSection.ReadSkipExpreH(Reader:TReader);
begin
 FSkipExpreH:=ReadWideString(Reader);
end;

procedure TRpSection.WriteSkipExpreH(Writer:TWriter);
begin
 WriteWideString(Writer, FSkipExpreH);
end;

procedure TRpSection.WriteSkipToPageExpre(Writer:TWriter);
begin
 WriteWideString(Writer, FSkipToPageExpre);
end;

procedure TRpSection.ReadSkipExpreV(Reader:TReader);
begin
 FSkipExpreV:=ReadWideString(Reader);
end;

procedure TRpSection.ReadSkipToPageExpre(Reader:TReader);
begin
 FSkipToPageExpre:=ReadWideString(Reader);
end;

procedure TRpSection.DefineProperties(Filer:TFiler);
begin
 inherited;

 Filer.DefineProperty('ChangeExpression',ReadChangeExpression,WriteChangeExpression,True);
 Filer.DefineProperty('BeginPageExpression',ReadBeginPageExpression,WriteBeginPageExpression,True);
 Filer.DefineProperty('ChangeExpression',ReadChangeExpression,WriteChangeExpression,True);
 Filer.DefineProperty('SkipExpreV',ReadSkipExpreV,WriteSkipExpreV,True);
 Filer.DefineProperty('SkipExpreH',ReadSkipExpreH,WriteSkipExpreH,True);
 Filer.DefineProperty('SkipToPageExpre',ReadSkipToPageExpre,WriteSkipToPageExpre,True);
end;

function TRpSection.GetExternalDataDescription:String;
begin
 Result:='';
 if Length(ExternalConnection)<1 then
  exit;
 if Length(ExternalTable)<1 then
  exit;
 if Length(ExternalField)<1 then
  exit;
 if Length(ExternalSearchField)<1 then
  exit;
 if Length(ExternalSearchValue)<1 then
  exit;
 Result:=ExternalConnection+'-'+
  ExternalTable+'-'+ExternalField+'-'+
  ExternalSearchField+'-'+ExternalSearchValue;
end;

function RpSkipTypeToText(value:TRpSkipType):String;
begin
 case value of
  secskipdefault:
   begin
    Result:=SRpSDefault;
   end;
  secskipbefore:
   begin
    Result:=SRpSSkipBefore;
   end;
  secskipafter:
   begin
    Result:=SRpSSkipAfter;
   end;
 end;
end;

function StringToRpSkipType(value:String):TRpSkipType;
begin
 Result:=secskipdefault;
 if SRpSSkipBefore=value then
 begin
  Result:=secskipbefore;
  exit
 end;
 if SRpSSkipAfter=value then
 begin
  Result:=secskipafter;
  exit;
 end;
end;

procedure GetSkipTypePossibleValues(alist:TRpWideStrings);
begin
 alist.clear;
 alist.Add(SRpSDefault);
 alist.Add(SRpSSkipBefore);
 alist.Add(SRpSSkipAfter);
end;

function TRpSection.GetOwnedComponent(index:Integer):TComponent;
begin
 Result:=inherited Components[index];
end;

procedure TRpSection.Loaded;
begin
 inherited Loaded;
 If (Assigned(FChildSubReport)) then
  PageRepeat:=false;
end;

procedure TRpSection.SetPageRepeat(Value:Boolean);
begin
 if (csReading in ComponentState) then
 begin
  FPageRepeat:=Value;
  exit;
 end;
 if (csLoading in ComponentState) then
 begin
  FPageRepeat:=Value;
  exit;
 end;
 if Value then
  If (Assigned(FChildSubReport)) then
   FChildSubReport:=nil;
 FPageRepeat:=Value;
end;

function TRpSection.AddComponent(componentclass:TRpCommonPosClass):TRpCommonPosComponent;
begin
 Result:=componentclass.Create(Owner);
 GenerateNewName(Result);
 Components.Add.Component:=Result;
end;

function TRpSection.GetIsExternal:Boolean;
begin
 Result:=false;
 if Length(FExternalFilename)>0 then
 begin
  Result:=true;
  exit;
 end;
 if Length(GetExternalDataDescription)>0 then
 begin
  Result:=true;
  exit;
 end;
end;


end.
