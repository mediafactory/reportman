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

uses Classes,rptypes,rpconsts,rpmunits,rpprintitem,rplabelitem,
 sysutils,rpmetafile;

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
   FBeginPage:boolean;          // [rpsecrheader,rpsecrfooter,rpsecgheader,rpsecgfooter,rpsecdetail]
   FAlignBottom:boolean;        // [rpsecrheader,rpsecrfooter,rpsecgheader,rpsecgfooter,rpsecdetail]
   FSkipPage:boolean;           // [rpsecrheader,rpsecrfooter,rpsecgheader,rpsecgfooter,rpsecdetail]
   FSectionType:TRpSectionType;
   FComponents:TRpCommonList;
   FAutoExpand:Boolean;
   FAutoContract:Boolean;
   function GetSectionCaption:String;
   procedure SetComponents(Value:TRpCommonList);
   procedure SetGroupName(Value:string);
  public
   constructor Create(AOwner:TComponent);override;
   destructor Destroy;override;
   procedure FreeComponents;
   procedure DeleteComponent(com:TRpCommonComponent);
   property SectionCaption:String read GetSectionCaption;
   procedure Print(aposx,aposy:integer;metafile:TRpMetafileReport);override;
  published
   property SubReport:TComponent read FSubReport write FSubReport;
   property GroupName:String read FGroupName write SetGroupName;
   property ChangeBool:boolean read FChangeBool write FChangeBool;
   property ChangeExpression:widestring read FChangeExpression write FChangeExpression;
   property PageRepeat:boolean read FPageRepeat write FPageRepeat;
   property BeginPage:boolean read FBeginPage write FBeginPage;
   property SkipPage:boolean read FSkipPage write FSkipPage;
   property AlignBottom:boolean read FAlignBottom write FAlignBottom;
   property SectionType:TRpSectionType read FSectionType write FSectionType;
   property Components:TRpCommonList read FComponents write SetComponents;
   property AutoExpand:Boolean read FAutoExpand write FAutoExpand
    default false;
   property AutoContract:Boolean read FAutoContract write FAutoContract
    default false;
 end;

implementation

uses rpsubreport;

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
   Components.Delete(i);
   break;
  end;
  inc(i);
 end;
end;

procedure TRpSection.SetGroupName(Value:string);
var
 subrep:TRpSubreport;
 i:integer;
 j:integer;
 sec:TRpSection;
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
  for i:=0 to subrep.Sections.Count-1 do
  begin
   sec:=TRpSection(subrep.Sections[i]);
   for j:=0 to Components.Count-1 do
   begin
    if (sec.Components.Items[j].Component is TRpExpression) then
    begin
     TRpExpression(sec.Components.Items[j].Component).GroupName:=Value;
    end;
   end;
  end;
 end;
 // Assign header and footer
 for i:=0 to subrep.Sections.Count-1 do
 begin
  if (subrep.Sections.Items[i].Section.SectionType in [rpsecgheader,rpsecgfooter]) then
   if subrep.Sections.Items[i].Section.GroupName=FGroupName then
    subrep.Sections.Items[i].Section.GroupName:=Value;
 end;
end;

procedure TRpSection.Print(aposx,aposy:integer;metafile:TRpMetafileReport);
var
 i:integer;
begin
 for i:=0 to Components.Count-1 do
 begin
  Components.Items[i].Component.Print(aposx,aposy,metafile);
 end;
end;

end.
