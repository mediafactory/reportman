{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpsubreport                                     }
{       TRpSubReport: The subreport defines sections    }
{       and data access methods to compose a subreport  }
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

unit rpsubreport;


interface

uses Classes,SysUtils,rpsecutil,rpsection,rptypes,rpconsts,
 rplabelitem,rpprintitem;

type
 TRpSubReport=class(TComponent)
  private
   FSections:TRpSectionList;
   FAlias:string;
   // Methots for writing internal indexes
   procedure SetSections(Value:TRpSectionList);
   function GetDetailCount:integer;
   function GetFirstDetail:integer;
   function GetLastDetail:integer;
   function GetFirstPageHeader:integer;
   function GetPageHeaderCount:integer;
  protected
   procedure FillGroupValues;
   procedure CheckCurrentGroupChange;
  public
   // Creation and destruction
   CurrentGroup:string;
   constructor Create(AOWner:TComponent);override;
   destructor Destroy;override;
   procedure CreateNew;
   procedure FreeSections;
   procedure FreeSection(sec:TRpSection);
   procedure AddPageHeader;
   procedure AddPageFooter;
   procedure CheckGroupExists(groupname:string);
   procedure AddGroup(groupname:string);
   procedure AddDetail;
   procedure SubReportChanged(newstate:TRpReportChanged;newgroup:string='');
   property FirstDetail:integer read GetFirstDetail;
   property LastDetail:integer read GetLastDetail;
   property DetailCount:integer read GetDetailCount;
   property FirstPageHeader:integer read GetFirstPageHeader;
   property PageHeaderCount:integer read GetPageHeaderCount;
  published
   property Sections:TRpSectionList read FSections write SetSections;
   property Alias:String read FAlias write FAlias;
 end;

implementation



procedure TRpSubReport.AddPageHeader;
var
 i:integer;
 index:integer;
 sec:TRpSection;
begin
 // Search the index to insert the page header
 index:=0;
 // Move all sections one down
 Sections.Add;
 for i:=Sections.count-2 downto index do
 begin
  Sections.Items[i+1].Section:=Sections.Items[i].Section;
 end;
 sec:=TRpSection.Create(Owner);
 sec.SubReport:=Self;
 sec.SectionType:=rpsecpheader;
 Sections.Items[index].Section:=sec;
 Generatenewname(sec);
end;


procedure TRpSubReport.AddDetail;
var
 i:integer;
 index:integer;
 sec:TRpSection;
begin
 // Search the index to insert the page footer
 index:=0;
 while ((Sections.Items[index].Section.SectionType in [rpsecpheader..rpsecdetail])
       ) do
 begin
  inc(index);
  if (index>=Sections.Count) then
   break;
 end;
 // Move all sections one down
 Sections.Add;
 for i:=Sections.count-2 downto index do
 begin
  Sections.Items[i+1].Section:=Sections.Items[i].Section;
 end;
 sec:=TRpSection.Create(Owner);
 sec.SubReport:=Self;
 sec.SectionType:=rpsecdetail;
 Sections.Items[index].Section:=sec;
 Generatenewname(sec);
end;


procedure TRpSubReport.AddPageFooter;
var
 i:integer;
 index:integer;
 sec:TRpSection;
begin
 // Search the index to insert the page footer
 index:=0;
 while ((Sections.Items[index].Section.SectionType in [rpsecpheader..rpsecgfooter])
       ) do
 begin
  inc(index);
  if (index>=Sections.Count) then
   break;
 end;
 // Move all sections one down
 Sections.Add;
 for i:=Sections.count-2 downto index do
 begin
  Sections.Items[i+1].Section:=Sections.Items[i].Section;
 end;
 sec:=TRpSection.Create(Owner);
 sec.SubReport:=Self;
 sec.SectionType:=rpsecpfooter;
 Sections.Items[index].Section:=sec;
 Generatenewname(sec);
end;


procedure TRpSubReport.CheckGroupExists(groupname:string);
var
 i:integer;
begin
 for i:=0 to Sections.Count-1 do
 begin
  if (Sections[i].Section.SectionType in [rpsecgheader,rpsecgfooter]) then
  begin
   if Uppercase(Sections[i].Section.GroupName)=UpperCase(Groupname) then
    Raise Exception.Create(SRpGroupAlreadyExists+GroupName);
  end;
 end;
end;

procedure TRpSubReport.AddGroup(groupname:string);
var
 i:integer;
 index:integer;
 sec:TRpSection;
 sec1:TRpSection;
begin
 // Checks not exisss
 groupname:=UpperCase(groupname);
 if Length(groupname)<1 then
  Raise Exception.Create(SRpGroupNameRequired);
 // Search the index to insert the group header
 index:=0;
 while ((Sections.Items[index].Section.SectionType in [rpsecpheader..rpsecgheader])
       AND (index<Sections.Count)) do
 begin
  if (Sections.Items[index].Section.SectionType=rpsecgheader) then
  begin
   if (groupname=Sections.Items[index].Section.GroupName) then
    Raise Exception.Create(SRpGroupNameExists);
  end;
  inc(index);
 end;
 // Move all sections one down
 Sections.Add;
 for i:=Sections.count-2 downto index do
 begin
  Sections.Items[i+1].Section:=Sections.Items[i].Section;
 end;
 sec:=TRpSection.Create(Owner);
 sec.GroupName:=groupname;
 sec1:=sec;
 sec.SectionType:=rpsecgheader;
 Sections.Items[index].Section:=sec;
 Generatenewname(sec);
 // Search the index to insert the group footer
 index:=0;
 while ((Sections.Items[index].Section.SectionType in [rpsecpheader..rpsecdetail])
       ) do
 begin
  inc(index);
  if (index>=Sections.Count) then
   break;
 end;
 // Move all sections one down
 Sections.Add;
 for i:=Sections.count-2 downto index do
 begin
  Sections.Items[i+1].Section:=Sections.Items[i].Section;
 end;
 sec:=TRpSection.Create(Owner);
 sec.GroupName:=groupname;
 sec.SubReport:=Self;
 sec1.SubReport:=Self;
 sec.SectionType:=rpsecgfooter;
 Sections.Items[index].Section:=sec;
 Generatenewname(sec);
end;

procedure TRpSubReport.SetSections(Value:TRpSectionList);
begin
 FSections.Assign(Value);
end;

constructor TRpSubReport.Create(AOWner:TComponent);
begin
 inherited Create(AOwner);
 // Sections
 FSections:=TRpSectionList.Create(Self);
end;

destructor TRpSubReport.Destroy;
begin
 FSections.free;
 inherited Destroy;
end;

procedure TRpSubReport.FreeSections;
var
 i:integer;
begin
 // If is destroying left the component free sections
 if (csDestroying in Owner.ComponentState) then
  exit;
 for i:=0 to FSections.Count-1 do
 begin
  FSections.Items[i].Section.FreeComponents;
  FSections.Items[i].Section.Free;
  FSections.Items[i].Section:=nil;
 end;
 FSections.Clear;
end;

procedure TRpSubReport.CreateNew;
var
 it:TRpSectionListItem;
begin
 // Free the current sections
 FreeSections;
 // Create a new section, the owner is the report
 it:=FSections.Add;
 it.Section:=TRpSection.Create(Owner);
 it.Section.SubReport:=Self;
 it.Section.SectionType:=rpsecdetail;
 Generatenewname(it.Section);
end;

// Frees a section, if the section is a group
// the header and footer are freed
procedure TRpSubReport.FreeSection(sec:TRpSection);
var
 i:integer;
 detailcount:integer;
 groupname:string;
begin
 // If it's a detail looks if there is two details
 if sec.SectionType=rpsecdetail then
 begin
  i:=0;
  detailcount:=0;
  while i<FSections.Count do
  begin
   if Sections.Items[i].Section.SectionType=rpsecdetail then
   begin
    inc(detailcount);
    if detailcount>1 then
     break;
   end;
   inc(i);
  end;
  if detailcount<2 then
   Raise Exception.Create(SRpAtLeastOneDetail);
 end;
 if (sec.SectionType in [rpsecgheader,rpsecgfooter]) then
 begin
  groupname:=sec.GroupName;
  i:=0;
  while i<Sections.Count do
  begin
   if (Sections.Items[i].Section.GroupName=groupname) then
   begin
    if Sections.Items[i].Section.Sectiontype=rpsecgheader then
     break;
   end;
   inc(i);
  end;
  if (i>=Sections.Count) then
   Raise Exception.Create(SRpSectionNotFound);
  Sections.Items[i].Section.FreeComponents;
  Sections.Items[i].Section.Free;
  Sections.Delete(i);

    i:=0;
  while i<Sections.Count do
  begin
   if (Sections.Items[i].Section.GroupName=groupname) then
   begin
    if Sections.Items[i].Section.Sectiontype=rpsecgfooter then
     break;
   end;
   inc(i);
  end;
  if (i>=Sections.Count) then
   Raise Exception.Create(SRpSectionNotFound);
  Sections.Items[i].Section.FreeComponents;
  Sections.Items[i].Section.Free;
  Sections.Delete(i);
 end
 else
 begin
  i:=0;
  while (Sections.Items[i].Section<>sec) do
  begin
   inc(i);
   if i>Sections.count-1 then
    Raise Exception.Create(SRpSectionNotFound);
  end;
  Sections.Items[i].Section.FreeComponents;
  Sections.Items[i].Section.Free;
  Sections.Delete(i);
 end;
end;

function TRpSubReport.GetDetailCount:integer;
begin
 Result:=0;
end;

function TRpSubReport.GetFirstDetail:integer;
var
 i:integer;
begin
 Result:=-1;
 i:=0;
 while i<Sections.Count do
 begin
  if Sections.Items[i].Section.SectionType=rpsecdetail then
  begin
   Result:=i;
   break;
  end;
  inc(i);
 end;
end;


function TRpSubReport.GetLastDetail:integer;
var
 i:integer;
begin
 i:=0;
 while i<Sections.Count do
 begin
  if Sections.Items[i].Section.SectionType=rpsecdetail then
  begin
   break;
  end;
  inc(i);
 end;
 while i<Sections.Count do
 begin
  if Sections.Items[i].Section.SectionType<>rpsecdetail then
  begin
   dec(i);
   break;
  end;
  inc(i);
 end;
 dec(i);
 Result:=i;
end;


procedure TRpSubReport.FillGroupValues;
begin
 CurrentGroup:='';
end;

procedure TRpSubReport.CheckCurrentGroupChange;
begin

end;


procedure TRpSubReport.SubReportChanged(newstate:TRpReportChanged;newgroup:string='');
var
 i:integer;
 j:integer;
 sec:TRpSection;
 compo:TRpCommonComponent;
begin
 // Updates group values
 if newstate=rpReportStart then
 begin
  FillGroupValues;
 end;
 if newstate=rpDataChange then
 begin
  CheckCurrentGroupChange;
 end;
 for i:=0 to Sections.Count-1 do
 begin
  sec:=Sections.Items[i].Section;
  for j:=0 to sec.Components.Count-1 do
  begin
   compo:=sec.Components.Items[j].Component;
   if (compo is TRpExpression) then
   begin
    TRpExpression(compo).SubReportChanged(newstate,newgroup);
   end;
  end;
 end;
end;


function TRpSubreport.GetPageHeaderCount:integer;
var
 i:integer;
 detailindex:integer;
begin
 Result:=0;
 i:=0;
 detailindex:=FirstDetail;
 while i<detailindex do
 begin
  if Sections.Items[i].Section.SectionType=rpsecpheader then
  begin
   Inc(Result);
   break;
  end;
 end;
end;



function TRpSubreport.GetFirstPageHeader:integer;
var
 i:integer;
 detailindex:integer;
begin
 Result:=-1;
 i:=0;
 detailindex:=FirstDetail;
 while i<detailindex do
 begin
  if Sections.Items[i].Section.SectionType=rpsecpheader then
  begin
   Result:=i;
   break;
  end;
 end;
end;

end.
