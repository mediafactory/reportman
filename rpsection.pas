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
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}

unit rpsection;

interface

uses Classes,rptypes,rpconsts,rpmunits,rpprintitem;

const
 C_DEFAULT_SECTION_WIDTH=19;
 C_DEFAULT_SECTION_HEIGHT=3;

type
 TRpSectionType=(rpsecrheader,rpsecpheader,rpsecgheader,
  rpsecdetail,rpsecgfooter,rpsecpfooter,rpsecrfooter);

 TRpSection=class(TRpCommonComponent)
  private
   FGroupName:String;
   FSectionType:TRpSectionType;
   FComponents:TRpCommonList;
   function GetSectionCaption:String;
   procedure SetComponents(Value:TRpCommonList);
  public
   constructor Create(AOwner:TComponent);override;
   destructor Destroy;override;
   procedure FreeComponents;
   property SectionCaption:String read GetSectionCaption;
  published
   property GroupName:String read FGroupName write FGroupName;
   property SectionType:TRpSectionType read FSectionType write FSectionType;
   property Components:TRpCommonList read FComponents write SetComponents;
 end;

implementation

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
  rpsecrheader:
   begin
    Result:=SRpReportHeader;
   end;
  rpsecrfooter:
   begin
    Result:=SRpReportFooter;
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

end.
