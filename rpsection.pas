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

uses Classes,rpmzlib,
{$IFDEF MSWINDOWS}
 windows,
{$ENDIF}
{$IFDEF USEVARIANTS}
 Types,
{$ENDIF}
 rptypes,rpmdconsts,rpmunits,rpprintitem,rplabelitem,
 sysutils,rpmetafile,rpeval;

const
 C_DEFAULT_SECTION_WIDTH=19;
 C_DEFAULT_SECTION_HEIGHT=3;

type
 TRpSectionType=(rpsecpheader,rpsecgheader,
  rpsecdetail,rpsecgfooter,rpsecpfooter);


 TRpSection=class(TRpCommonComponent)
  private
   FSubReport:TComponent;
   FGroupName:String;           // [rpsecgheader,rpsecgfooter]
   FChangeExpression:WideString;// [rpsecgheader,rpsecgfooter]
   FChangeBool:boolean;         // [rpsecgheader,rpsecgfooter]
   FPageRepeat:boolean;         // [rpsecgheader,rpsecgfooter]
   FAlignBottom:boolean;        // [rpsecrheader,rpsecrfooter,rpsecgheader,rpsecgfooter,rpsecdetail]
   FSkipPage:boolean;           // [rpsecrheader,rpsecrfooter,rpsecgheader,rpsecgfooter,rpsecdetail]
   FSectionType:TRpSectionType;
   FComponents:TRpCommonList;
   FAutoExpand:Boolean;
   FAutoContract:Boolean;
   FHorzDesp:Boolean;
//   FIsExternal:boolean;
   FExternalFilename:string;
   FBeginPageExpression:widestring;
   // deprecated
   FBeginPage:boolean;
   FReadError:Boolean;
   function GetSectionCaption:String;
   procedure SetComponents(Value:TRpCommonList);
   procedure SetGroupName(Value:string);
   procedure SetChangeExpression(Value:widestring);
   procedure OnReadError(Reader: TReader; const Message: string; var Handled: Boolean);
  protected
   procedure DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);override;
   procedure GetChildren(Proc: TGetChildProc; Root: TComponent);override;
  public
   GroupValue:Variant;
   constructor Create(AOwner:TComponent);override;
   destructor Destroy;override;
   procedure FreeComponents;
   procedure DeleteComponent(com:TRpCommonComponent);
   property SectionCaption:String read GetSectionCaption;
   function GetExtension(adriver:IRpPrintDriver):TPoint;override;
   function EvaluateBeginPage:boolean;
   procedure LoadExternal;
   procedure SaveExternal;
  published
   property SubReport:TComponent read FSubReport write FSubReport;
   property GroupName:String read FGroupName write SetGroupName;
   property ChangeBool:boolean read FChangeBool write FChangeBool;
   property ChangeExpression:widestring read FChangeExpression write SetChangeExpression;
   property PageRepeat:boolean read FPageRepeat write FPageRepeat;
   property SkipPage:boolean read FSkipPage write FSkipPage;
   property AlignBottom:boolean read FAlignBottom write FAlignBottom;
   property SectionType:TRpSectionType read FSectionType write FSectionType;
   property Components:TRpCommonList read FComponents write SetComponents;
   property AutoExpand:Boolean read FAutoExpand write FAutoExpand
    default false;
   property AutoContract:Boolean read FAutoContract write FAutoContract
    default false;
   property HorzDesp:Boolean read FHorzDesp write FHorzDesp default false;
   property BeginPageExpression:widestring read FBeginPageExpression
    write FBeginPageExpression;
//   property IsExternal:Boolean read FIsExternal
//    write FIsExternal default false;
   // External filename is a alias.field or if not exists a filename
   // If it's lenght is 0 it's not external
   property ExternalFilename:string read FExternalFilename write FExternalFilename;
   // Deprecated properties for compatibility only
   property BeginPage:boolean read FBeginpage write FBeginPage default false;
 end;

implementation

uses rpsubreport,rpreport;

constructor TRpSection.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FComponents:=TRpCommonList.Create(Self);

 Width:=Round(C_DEFAULT_SECTION_WIDTH*TWIPS_PER_INCHESS/CMS_PER_INCHESS);
 Height:=Round(C_DEFAULT_SECTION_HEIGHT*TWIPS_PER_INCHESS/CMS_PER_INCHESS);
end;

procedure TRpSection.SetComponents(Value:TRpCommonList);
begin
 FComponents.Assign(Value);
end;

destructor TRpSection.Destroy;
begin
 FComponents.Free;
 inherited destroy;
end;

function TRpSection.GetSectionCaption:String;
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
end;

procedure TRpSection.FreeComponents;
var
 i:integer;
begin
 for i:=0 to Components.Count-1 do
 begin
  FComponents.Items[i].Component.free;
 end;
 FComponents.Clear;
end;

procedure TRpSection.DeleteComponent(com:TRpCommonComponent);
var
 i:integer;
begin
 i:=0;
 while i<Components.Count do
 begin
  if Components.Items[i].Component=Com then
  begin
   com.Free;
   Components.Items[i].Free;
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

procedure TRpSection.DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);
var
 i:integer;
 compo:TRpCommonPosComponent;
 newposx,newposy:integer;
begin
 for i:=0 to Components.Count-1 do
 begin
  compo:=TRpCommonPosComponent(Components.Items[i].Component);
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
   Components.Items[i].Component.Print(newposx,newposy,metafile);
  end;
 end;
end;

function TRpSection.EvaluateBeginPage:boolean;
var
 report:TRpReport;
 eval:TRpEvaluator;
begin
 Result:=false;

 if Length(FBeginPageExpression)<1 then
  exit;
 report:=TRpReport(Owner);
 eval:=report.Evaluator;
 eval.Expression:=FBeginPageExpression;
 eval.Evaluate;
 Result:=eval.EvalResult;
end;


function TRpSection.GetExtension(adriver:IRpPrintDriver):TPoint;
var
 minsize,maxsize,currentsize:integer;
 compsize:TPoint;
 newsize:integer;
 i:integer;
 acompo:TRpCommonPosComponent;
begin
 Result:=inherited GetExtension(adriver);
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
 for i:=0 to Components.Count-1 do
 begin
  acompo:=TRpCommonPosComponent(Components.Items[i].Component);
  compsize:=acompo.GetExtension(adriver);
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
 if currentsize<minsize then
  currentsize:=minsize;
 Result.Y:=currentsize;
 lastextent:=Result;
end;


procedure TRpSection.SaveExternal;
var
 i:integer;
 acompo:TComponent;
 AStream:TStream;
 zstream:TCompressionStream;
 writer:TWriter;
begin
 // Saves the components as a external section
 if Length(FExternalFilename)<1 then
  exit;
 // Looks if it's not external
 for i:=0 to Components.Count-1 do
 begin
  acompo:=Components.Items[i].Component;
  if assigned(acompo) then
  begin
   if (acompo.Owner=Owner) then
   begin
    Owner.RemoveComponent(acompo);
    Self.InsertComponent(acompo);
   end;
  end;
 end;
 AStream:=TFileStream.Create(FExternalFilename,fmCreate);
 try
  zstream:=TCompressionStream.Create(clDefault,AStream);
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
 finally
  AStream.free;
 end;
end;

procedure TRpSection.LoadExternal;
var
 i:integer;
 reader:TReader;
 AStream:TStream;
 buf:pointer;
 zlibs:TDeCompressionStream;
 readed:integer;
 memstream:TMemoryStream;
begin
 // Try to load the section as an external section
 if Length(FExternalFilename)<1 then
  exit;
 // Free all components
 for i:=0 to Components.Count-1 do
 begin
  if Assigned(Components.Items[i].Component) then
   Components.Items[i].Component.Free;
 end;
 Components.Clear;
 FReadError:=false;
 AStream:=TFileStream.Create(FExternalFilename,fmOpenRead or fmShareDenyWrite);
 try
  MemStream:=TMemoryStream.Create;
  try
   zlibs:=TDeCompressionStream.Create(AStream);
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
     reader.ReadRootComponent(Self);
    finally
     reader.free;
    end;
   finally
    zlibs.Free;
   end;
  finally
   MemStream.free;
  end;
 finally
  AStream.free;
 end;
 if FReadError then
 begin
  for i:=0 to ComponentCount-1 do
  begin
   inherited Components[i].Free;
  end;
  Height:=0;
  exit;
 end;
 for i:=0 to ComponentCount-1 do
 begin
  Components.Add.Component:=((inherited Components[i]) As TRpCommonComponent);
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
   OwnedComponent := inherited Components[I];
   if not OwnedComponent.HasParent then
    Proc(OwnedComponent);
//   if OwnedComponent is TRpSubReport then
//   begin
//    if subreport.
//      Proc(OwnedComponent);
//   end;
  end;
end;

end.
