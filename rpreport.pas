{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpreport                                        }
{       TRpReport: The report component, it contains    }
{       subreports, pagesetup, printer selection...     }
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
// One report is composed of subreports, the report has
// page setup properties and a subreport list
// The subreports are printed in order and can have
// diferent datasources, grouping, sections etc
unit rpreport;

interface

{$I rpconf.inc}

uses Classes,sysutils,rptypes,rpsubreport,rpsection,rpconsts,
 rpdatainfo,rpparams,rplabelitem,rpdrawitem,rpeval,rptypeval,
 rpmetafile,
{$IFDEF USEVARIANTS}
 types,dateutils,
{$ENDIF}
 rpalias,db,rpzlib,rpdataset,
{$IFDEF LINUX}
  Libc,
{$ENDIF}
{$IFDEF MSWINDOWS}
  mmsystem,windows,
{$ENDIF}
 rpmunits;


const
 MILIS_PROGRESS=500;
 // 1 cms=574
 // 0.5 cms=287
 CONS_DEFAULT_GRIDWIDTH=115;
 CONS_DEFAULT_GRIDCOLOR=$FF0000;
 CONS_MIN_GRID_WIDTH=50;
 // 29,7/2.51*1440
 DEFAULT_PAGEHEIGHT=17039;
 DEFAULT_PAGEWIDTH=12048;
 // default Margins
 // Left 1 cm, Right 1 cm, Top 1 cm Bottom 1.5 cm
 DEFAULT_LEFTMARGIN=574;
 DEFAULT_RIGHTMARGIN=574;
 DEFAULT_BOTTOMMARGIN=861;
 DEFAULT_TOPMARGIN=574;
 // Minimum grid
type
 TRpReport=class;
 TRpSubReportListItem=class;
 TRpProgressEvent=procedure (Sender:TRpReport;var docancel:boolean) of object;
 TRpSubReportList=class(TCollection)
  private
   FReport:TRpReport;
   function GetItem(Index:Integer):TRpSubReportListItem;
   procedure SetItem(index:integer;Value:TRpSubReportListItem);
  public
   constructor Create(rp:TRpReport);
   function Add:TRpSubReportListItem;
   function IndexOf(Value:TRpSubReport):integer;
   property Items[index:integer]:TRpSubReportListItem read GetItem write SetItem;default;
 end;
 TRpSubReportListItem=class(TCollectionItem)
  private
   FSubReport:TRpSubReport;
   procedure SetSubReport(Value:TRpSubReport);
  public
   procedure Assign(Source:TPersistent);override;
  published
   property SubReport:TRpSubReport read FSubReport write SetSubReport;
 end;

 TIdenReportVar=class(TIdenFunction)
   FReport:TRpReport;
  private
  protected
   function GeTRpValue:TRpValue;override;
  public
   varname:string;
  end;


 TRpReport=class(TComponent)
  private
   FSubReports:TRpSubReportList;
   FPageOrientation:TRpOrientation;
   FPagesize:TRpPagesize;
   FPageSizeQt:integer;
   FPageWidth:TRpTwips;
   FPageHeight:TRpTwips;
   FInternalPageWidth:TRpTwips;
   FInternalPageHeight:TRpTwips;
   FPageBackColor:TRpColor;
   FPreviewStyle:TRpPreviewStyle;
   FOnReadError:TReaderError;
   FDataInfo:TRpDataInfoList;
   FDatabaseInfo:TRpDatabaseInfoList;
   FParams:TRpParamList;
   FGridVisible:Boolean;
   FGridEnabled:Boolean;
   FGridColor:integer;
   FGridLines:Boolean;
   FGridWidth:integer;
   FGridHeight:integer;
   FLanguage:integer;
   FEvaluator:TRpEvaluator;
   FIdentifiers:TStringList;
   FMetafile:TRpMetafileReport;
   FDataAlias:TRpAlias;
   FOnProgress:TRpProgressEvent;
   FRecordCount:integer;
   FDriver:IRpPrintDriver;
   FLeftMargin,FTopMargin,FRightMargin,FBottomMargin:TRpTwips;
   Fidenpagenum:TIdenReportVar;
   Fidenfreespace:TIdenReportVar;
   Fidenfreespacecms:TIdenReportVar;
   Fidenfreespaceinch:TIdenReportVar;
   Fidencurrentgroup:TIdenReportVar;
   FCopies:integer;
   FCollateCopies:boolean;
   FTwoPass:boolean;
   FTotalPagesList:TList;
   FAliasList:TRpAlias;
   printingonepass:boolean;
   freespace:integer;
{$IFDEF MSWINDOWS}
   mmfirst,mmlast:DWORD;
{$ENDIF}
{$IFDEF LINUX}
   milifirst,mililast:TDatetime;
{$ENDIF}
   difmilis:int64;
   procedure FInternalOnReadError(Reader: TReader; const Message: string;
    var Handled: Boolean);
   procedure SetSubReports(Value:TRpSubReportList);
   procedure SetDataInfo(Value:TRpDataInfoList);
   procedure SetDatabaseInfo(Value:TRpDatabaseInfoList);
   procedure SetParams(Value:TRpParamList);
   procedure ClearTotalPagesList;
   procedure SetGridWidth(Value:TRpTwips);
   procedure SetGridHeight(Value:TRpTwips);
  protected
    section:TRpSection;
    subreport:TRpSubreport;
    procedure Notification(AComponent:TComponent;Operation:TOperation);override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent);override;
    procedure Loaded;override;
    function NextSection:boolean;
    // Skip to next record returns true if a group has
    // changed and sets internally CurrentGroup
    function NextRecord(grouprestore:boolean):boolean;
  public
   printing:boolean;
   CurrentSubReportIndex:integer;
   CurrentSectionIndex:integer;
   PageNum:integer;
   LastPage:Boolean;
   property RecordCount:integer read FRecordCount;
   property Metafile:TRpMetafileReport read FMetafile;
   property Identifiers:TStringList read FIdentifiers;
   constructor Create(AOwner:TComponent);override;
   destructor Destroy;override;
   procedure FreeSubreports;
   procedure AddSubReport;
   procedure DeleteSubReport(subr:TRpSubReport);
   // Streaming functions and properties
   procedure SaveToStream(Stream:TStream);
   procedure SaveToFile(Filename:string);
   procedure LoadFromStream(Stream:TStream);
   procedure LoadFromFile(FileName:string);
   property OnReadError:TReaderError read FOnReadError write FOnReadError;
   // Design functions
   procedure Createnew;
   // Print functions
   procedure ActivateDatasets;
   procedure DeActivateDatasets;
   procedure AddTotalPagesItem(apageindex,aobjectindex:integer;
    adisplayformat:widestring);
   property Evaluator:TRpEvaluator read FEvaluator;
   procedure BeginPrint(Driver:IRpPrintDriver);
   procedure EndPrint;
   function PrintNextPage:boolean;
   procedure PrintAll(Driver:IRpPrintDriver);
   procedure PrintRange(Driver:IRpPrintDriver;allpages:boolean;
    frompage,topage,copies:integer);
   property OnProgress:TRpProgressEvent read FOnProgress write FOnProgress;
   property AliasList:TRpAlias read FAliasList write FAliasList;
   property idenpagenum:TIdenReportVar read fidenpagenum;
   property idenfreespace:TIdenReportVar read fidenfreespace;
   property idenfreespacecms:TIdenReportVar read fidenfreespacecms;
   property idenfreespaceinch:TIdenReportVar read fidenfreespaceinch;
   property idencurrentgroup:TIdenReportVar read fidencurrentgroup;
  published
   // Grid options
   property GridVisible:Boolean read FGridVisible write FGridVisible default true;
   property GridLines:Boolean read FGridLines write FGridLines default false;
   property GridEnabled:Boolean read FGridEnabled write FGridEnabled default true;
   property GridColor:integer read FGridColor write FGridColor default CONS_DEFAULT_GRIDCOLOR;
   property GridWidth:TRpTwips read FGridWidth write SetGridWidth default CONS_DEFAULT_GRIDWIDTH;
   property GridHeight:TRpTwips read FGridHeight write SetGridHeight default CONS_DEFAULT_GRIDWIDTH;
   // PageSetup properties
   property PageOrientation:TRpOrientation read FPageOrientation
    write FPageOrientation default rpOrientationDefault;
   property Pagesize:TRpPageSize read FPagesize write FPageSize
     default rpPageSizeDefault;
   property PagesizeQt:integer read FPagesizeQt write FPageSizeQt
     default 0;
   property PageHeight:TRpTwips read FPageHeight write FPageHeight
    default DEFAULT_PAGEHEIGHT;
   property PageWidth:TRpTwips read FPageWidth write FPageWidth
    default DEFAULT_PAGEWIDTH;
   property PageBackColor:TRpColor read FPageBackColor write FPageBackColor;
   property PreviewStyle:TRpPreviewStyle read FPreviewStyle
    write FPreviewStyle default spWide;
   property LeftMargin:TRpTwips read FLeftMargin write FLeftMargin
    default DEFAULT_LEFTMARGIN;
   property TopMargin:TRpTwips read FTopMargin write FTopMargin
    default DEFAULT_TOPMARGIN;
   property RightMargin:TRpTwips read FRightMargin write FRightMargin
    default DEFAULT_RIGHTMARGIN;
   property BottomMargin:TRpTwips read FBottomMargin write FBottomMargin
    default DEFAULT_BOTTOMMARGIN;
   // Subreports
   property SubReports:TRpSubReportList read FSubReports write SetSubReports;
   property DataInfo:TRpDataInfoList read FDataInfo write SetDataInfo;
   property DatabaseInfo:TRpDatabaseInfoList read FDatabaseInfo write SetDatabaseInfo;
   property Params:TRpParamList read FParams write SetParams;
   // Language
   property Language:integer read FLanguage write FLanguage default 0;
   // Other
   property Copies:integer read FCopies write FCopies default 1;
   property CollateCopies:boolean read FCollateCopies write FCollateCopies default false;
   property TwoPass:boolean read FTwoPass write FTwoPass default false;
 end;

implementation

uses rpprintitem, rpsecutil;

function TIdenReportVar.GeTRpValue:TRpValue;
begin
 if varname='PAGE' then
  Result:=freport.PageNum+1
 else
  if varname='FREESPACETWIPS' then
   Result:=freport.freespace
  else
   if varname='FREESPACECMS' then
    Result:=twipstocms(freport.freespace)
   else
    if varname='FREESPACEINCH' then
     Result:=twipstocms(freport.freespace)
    else
     if varname='CURRENTGROUP' then
     begin
      Result:=freport.Subreports.Items[freport.CurrentSubreportIndex].SubReport.CurrentGroupIndex;
     end;
end;

// Constructors and destructors
constructor TRpReport.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FCopies:=1;
 FPageOrientation:=rpOrientationDefault;
 // Means default pagesize
 FPagesize:=rpPageSizeDefault;
 FLeftMargin:=DEFAULT_LEFTMARGIN;
 FRightMargin:=DEFAULT_RIGHTMARGIN;
 FBottomMargin:=DEFAULT_BOTTOMMARGIN;
 FTopMargin:=DEFAULT_TOPMARGIN;
  // Means white
 FPageBackColor:=High(FPageBackColor);
 FPageWidth:=DEFAULT_PAGEWIDTH;
 FPageheight:=DEFAULT_PAGEHEIGHT;
 FPreviewStyle:=spWide;
 // Def values of grid
 FGridVisible:=True;
 FGridEnabled:=True;
 FGridColor:=CONS_DEFAULT_GRIDCOLOR;
 FGridLines:=False;
 FGridWidth:=CONS_DEFAULT_GRIDWIDTH;
 FGridHeight:=CONS_DEFAULT_GRIDWIDTH;
 // Subreports
 FSubReports:=TRpSubReportList.Create(Self);
 // Data Info
 FDataInfo:=TRpDataInfoList.Create(Self);
 FDatabaseInfo:=TRpDatabaseInfoList.Create(Self);
 FParams:=TRpParamList.Create(Self);
 // Identifiers
 FIdentifiers:=TStringList.Create;
 FIdentifiers.Sorted:=true;
 FIdentifiers.Duplicates:=dupError;
 // Pagenum
 FIdenPagenum:=TIdenReportVar.Create(nil);
 Fidenpagenum.FReport:=self;
 FidenPagenum.varname:='PAGE';
 FIdenfreespace:=TIdenReportVar.Create(nil);
 Fidenfreespace.varname:='FREESPACE';
 Fidenfreespace.FReport:=self;
 FIdenfreespacecms:=TIdenReportVar.Create(nil);
 Fidenfreespacecms.varname:='FREESPACECMS';
 Fidenfreespacecms.FReport:=self;
 FIdenfreespaceinch:=TIdenReportVar.Create(nil);
 Fidenfreespaceinch.varname:='FREESPACEINCH';
 Fidenfreespaceinch.FReport:=self;
 FIdencurrentgroup:=TIdenReportVar.Create(nil);
 Fidencurrentgroup.varname:='CURRENTGROUP';
 Fidencurrentgroup.FReport:=self;
 // Metafile
 FMetafile:=TRpMetafileReport.Create(nil);
 FDataAlias:=TRpAlias.Create(nil);
 FTotalPagesList:=TList.Create;

end;

procedure TRpReport.SetGridWidth(Value:TRpTwips);
begin
 if Value<CONS_MIN_GRID_WIDTH then
  Value:=CONS_MIN_GRID_WIDTH;
 FGridWidth:=Value;
end;

procedure TRpReport.SetGridHeight(Value:TRpTwips);
begin
 if Value<CONS_MIN_GRID_WIDTH then
  Value:=CONS_MIN_GRID_WIDTH;
 FGridHeight:=Value;
end;


procedure TRpReport.AddTotalPagesItem(apageindex,aobjectindex:integer;
 adisplayformat:widestring);
var
 aobject:TTotalPagesObject;
begin
 aobject:=TTotalPagesObject.Create;
 FTotalPagesList.Add(aobject);
 aobject.PageIndex:=apageindex;
 aobject.ObjectIndex:=aobjectindex;
 aobject.DisplayFormat:=adisplayformat;
end;


procedure TRpReport.ClearTotalPagesList;
var
 i:integer;
begin
 for i:=0 to FTotalPagesList.Count-1 do
 begin
  TObject(FTotalPagesList.Items[i]).Free;
 end;
 FTotalPagesList.Clear;
end;


destructor TRpReport.Destroy;
begin
 FSubReports.free;
 FDataInfo.free;
 FDatabaseInfo.free;
 FParams.free;
 FIdentifiers.free;
 FMetafile.Free;
 FDataAlias.Free;
 FIdenPagenum.free;
 Fidenfreespace.free;
 FIdenCurrentGroup.free;
 Fidenfreespacecms.free;
 Fidenfreespaceinch.free;
 FTotalPagesList.free;
 if Assigned(FEvaluator) then
 begin
  FEvaluator.free;
  FEvaluator:=nil;
 end;
 inherited destroy;
end;

procedure TRpReport.Loaded;
var
 i,j,k:integer;
 subrep:TRpSubReport;
 sec:TRpSection;
 comp:TRpCommonComponent;
 rpexpre:TRpExpression;
begin
 inherited Loaded;

 for i:=0 to Subreports.Count-1 do
 begin
  subrep:=Subreports.items[i].SubReport;
  for j:=0 to Subrep.Sections.Count-1 do
  begin
   sec:=SubRep.Sections.Items[j].Section;
   for k:=0 to sec.Components.Count-1 do
   begin
    comp:=sec.Components.items[k].Component;
    if (comp is TRpExpression) then
    begin
     rpexpre:=TRpExpression(comp);
     if Length(rpexpre.Identifier)>0 then
     begin
      try
       FIdentifiers.AddObject(rpexpre.Identifier,comp);
      except
       rpexpre.Identifier:='';
      end;
     end;
    end;
   end;
  end;
 end;
end;


procedure TRpReport.Notification(AComponent:TComponent;Operation:TOperation);
var i,index:integer;
begin
 inherited Notification(AComponent,Operation);

 if Operation=OpRemove then
 begin
  if (AComponent is TRpSubReport) then
  begin
   with FSubReports do
   begin
    for i:=0 to Count -1 do
    begin
     if items[i]<>nil then
     begin
      if Items[i].FSubReport=AComponent then
       Items[i].FSubReport:=nil;
     end;
    end;
   end;
  end
  else
  if (AComponent is TRpExpression) then
  begin
   if Length(TRpExpression(AComponent).Identifier)>0 then
   begin
    index:=FIdentifiers.IndexOf(TRpExpression(AComponent).Identifier);
    if index>=0 then
     FIdentifiers.Delete(index);
   end;
  end
  else
  begin
   if AComponent=FAliasList then
    FAliasList:=nil;
  end;
 end;
end;


// Streaming procedures

// GetChildren helps streaming the subreports
procedure TRpReport.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  OwnedComponent: TComponent;
//  rpsubreport:TRpSubReport;
begin
 inherited GetChildren(Proc, Root);
 if Root = Self then
  for I := 0 to ComponentCount - 1 do
  begin
   OwnedComponent := Components[I];
   if not OwnedComponent.HasParent then
    Proc(OwnedComponent);
//   if OwnedComponent is TRpSubReport then
//   begin
//    if subreport.
//      Proc(OwnedComponent);
//   end;
  end;
end;


procedure TRpReport.SaveToStream(Stream:TStream);
var
 zstream:TCompressionStream;
begin
 zstream:=TCompressionStream.Create(clDefault,Stream);
 try
   zstream.WriteComponent(Self);
 finally
  zstream.free;
 end;
end;

procedure TRpReport.FreeSubreports;
var
 i:integer;
begin
 // If it's destroying left do the work
 if (csDestroying in ComponentState) then
  exit;
 // Frees all the reports
 for i:=0 to FSubreports.Count-1 do
 begin
  FSubReports.Items[i].FSubReport.Free;
 end;
 FSubReports.Clear;
 FDataInfo.Clear;
 FDatabaseInfo.Clear;
end;


procedure TRpReport.AddSubReport;
var
 it:TRpSubReportListItem;
begin
 it:=SubReports.Add;
 it.FSubReport:=TRpSubreport.Create(Self);
 Generatenewname(it.FSubReport);
 it.FSubReport.CreateNew;
end;

procedure TRpReport.CreateNew;
begin
 // Creates a new default report
 FreeSubreports;
 AddSubReport;
end;

procedure TRpReport.SaveToFile(Filename:string);
var
 fstream:TFileStream;
begin
 fstream:=TFileStream.Create(Filename,fmCreate);
 try
  SaveToStream(fstream);
 finally
  fstream.free;
 end;
end;


procedure TRpReport.LoadFromFile(FileName:string);
var
 stream:TFileStream;
begin
 stream:=TFileStream.Create(Filename,fmOpenRead or fmShareDenyWrite);
 try
  LoadFromStream(stream);
 finally
  stream.free;
 end;
end;


procedure TRpReport.LoadFromStream(Stream:TStream);
var
 reader:TReader;
 memstream:TMemoryStream;
 readed:integer;
 buf:pointer;
 zlibs:TDeCompressionStream;
begin
 // FreeSubrepots
 FreeSubreports;
 MemStream:=TMemoryStream.Create;
 try
  zlibs:=TDeCompressionStream.Create(Stream);
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
    if Assigned(FOnReadError) then
     reader.OnError:=FOnReadError
    else
     reader.OnError:=FInternalOnReadError;
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
end;

procedure TRpReport.FInternalOnReadError(Reader: TReader; const Message: string;
    var Handled: Boolean);
begin
 Handled:=false;
end;

procedure TRpReport.SetSubReports(Value:TRpSubReportList);
begin
 FSubReports.Assign(Value);
end;

procedure TRpReport.SetDataInfo(Value:TRpDataInfoList);
begin
 FDataInfo.Assign(Value);
end;

procedure TRpReport.SetDatabaseInfo(Value:TRpDatabaseInfoList);
begin
 FDatabaseInfo.Assign(Value);
end;

// Report collections

constructor TRpSubReportList.Create(rp:TRpReport);
begin
 inherited Create(TRpSubReportListItem);
 FReport:=rp;
end;

procedure TRpSubReportListItem.SetSubReport(Value:TRpSubReport);
begin
 FSubReport:=Value;
 Changed(False);
end;

procedure TRpSubReportListItem.Assign(Source:TPersistent);
begin
 if Source is TRpSubReportListItem then
 begin
  FSubReport:=TRpSubReportListItem(Source).FSubReport;
 end
 else
  inherited Assign(Source);
end;


function TRpSubReportList.Add:TRpSubReportListItem;
begin
 Result:=TRpSubReportListItem(inherited Add);
end;

function TRpSubReportList.IndexOf(Value:TRpSubReport):integer;
var
 i:integer;
begin
 Result:=-1;
 i:=0;
 While i<count do
 begin
  if items[i].FSubReport=vALUE then
  begin
   Result:=i;
   break;
  end;
  inc(i);
 end;
end;

function TRpSubReportList.GetItem(Index:Integer):TRpSubReportListItem;
begin
 Result:=TRpSubReportListItem(inherited GetItem(index));
end;

procedure TRpSubReportList.SetItem(index:integer;Value:TRpSubReportListItem);
begin
 inherited SetItem(Index,Value);
end;

procedure TRpReport.DeleteSubReport(subr:TRpSubReport);
var
 i:integer;
begin
 if FSubReports.Count<2 then
  Raise Exception.Create(SRpAtLeastOneSubreport);
 i:=0;
 while (FSubReports.Items[i].FSubReport<>subr) do
 begin
  inc(i);
  if (i>FSubReports.count-1) then
   Raise Exception.Create(SRpSubReportNotFound);
 end;
 FSubReports.Items[i].FSubReport.FreeSections;
 FSubReports.Items[i].FSubReport.Free;
 FSubReports.Delete(i);
end;

procedure TRpReport.SetParams(Value:TRpParamList);
begin
 FParams.Assign(Value);
end;

procedure TRpReport.ActivateDatasets;
var
 i,index:integer;
 docancel:boolean;
 alias:string;
begin
 if FDataInfo.Count<1 then
  exit;
 try
  for i:=0 to FDataInfo.Count-1 do
  begin
   FDataInfo.Items[i].Cached:=false;
  end;
  // The main datasets must be cached
  for i:=0 to SubReports.Count-1 do
  begin
   alias:=SubReports.items[i].Subreport.Alias;
   if Length(alias)>0 then
   begin
    index:=DataInfo.IndexOf(alias);
    if index<0 then
      Raise Exception.Create(SRpSubreportAliasNotFound+':'+alias);
    FDataInfo.Items[index].Cached:=true;
   end;
  end;

  for i:=0 to FDataInfo.Count-1 do
  begin
   // Watch if external dataset
   if Assigned(FAliasList) then
   begin
    index:=FAliasList.List.indexof(FDataInfo.Items[i].Alias);
    if index>=0 then
    begin
     if Assigned(FAliasList.List.Items[index].dataset) then
      FDataInfo.Items[i].Dataset:=FAliasList.List.Items[index].dataset;
    end;
   end;
   FDataInfo.Items[i].Connect(DatabaseInfo,Params);
   if Assigned(FOnProgress) then
   begin
    docancel:=false;
    FOnProgress(Self,docancel);
    if docancel then
    Raise Exception.Create(SRpOperationAborted);
   end;
  end;
 except
  for i:=0 to FDataInfo.Count-1 do
  begin
   FDataInfo.Items[i].Disconnect;
  end;
  Raise;
 end;
end;

procedure TRpReport.DeActivateDatasets;
var
 i:integer;
begin
 for i:=0 to FDataInfo.Count-1 do
 begin
  FDataInfo.Items[i].Disconnect;
 end;
 for i:=0 to FDatabaseInfo.Count-1 do
 begin
  FDatabaseInfo.Items[i].DisConnect;
 end;
end;

procedure TRpReport.PrintRange(Driver:IRpPrintDriver;allpages:boolean;
    frompage,topage,copies:integer);
var
 initiated:boolean;
 i:integer;
 finished:boolean;
begin
 if allpages then
 begin
  frompage:=0;
  topage:=99999999;
 end
 else
 begin
  dec(frompage);
  dec(topage);
 end;
 printingonepass:=true;
 try
  BeginPrint(Driver);
  try
   initiated:=false;
   try
    finished:=false;
    while Not PrintNextPage do
    begin
     if not initiated then
     begin
      Driver.NewDocument(metafile);
      initiated:=true;
     end;
     if ((PageNum>=frompage) and  (PageNum<=topage)) then
     begin
      for i:=0 to copies-1 do
      begin
       Driver.NewPage;
       Driver.DrawPage(metafile.pages[0]);
       Driver.EndPage;
      end;
     end;
     if pagenum=topage then
     begin
      finished:=true;
      break;
     end;
     metafile.Clear;
    end;
    if not finished then
    begin
     if ((PageNum>=frompage) and  (PageNum<=topage)) then
     begin
      Driver.DrawPage(metafile.pages[0]);
     end;
    end;
   finally
    Driver.EndDocument;
   end;
  finally
   EndPrint;
  end;
 finally
  printingonepass:=false;
 end;
end;

// Print all generaties the metafile, it's capable also
// of evaluate the totalpages expression
procedure TRpReport.PrintAll(Driver:IRpPrintDriver);
begin
 BeginPrint(Driver);
 try
  while Not PrintNextPage do;
 finally
  EndPrint;
 end;
end;

procedure TRpReport.EndPrint;
var
 i:integer;
begin
 DeActivateDatasets;
 FEvaluator.Free;
 FEvaluator:=nil;
 section:=nil;
 subreport:=nil;
 printing:=false;
 for i:=0 to SubReports.Count-1 do
 begin
  Subreports.Items[i].Subreport.LastRecord:=false;
 end;
 metafile.UpdateTotalPages(FTotalPagesList);
end;


function TRpReport.NextRecord(grouprestore:boolean):boolean;
var
 subrep:TRpSubreport;
 index:integeR;
 data:TRpDataset;
 docancel:boolean;
begin
 Result:=false;
 subrep:=Subreports.Items[CurrentSubreportIndex].SubReport;
 if Length(Trim(subrep.Alias))<1 then
  subrep.Lastrecord:=True
 else
 begin
  index:=DataInfo.IndexOf(subrep.Alias);
  if index<0 then
   Raise TRpReportException.Create(SRPAliasNotExists+subrep.alias,subrep);
  data:=DataInfo.Items[index].CachedDataset;
  data.DoNext;
  // If its the last record no group change
  if not grouprestore then
  begin
   subrep.LastRecord:=data.Eof;
  end;
  if Not Subrep.LastRecord then
  begin
   if not grouprestore then
   begin
    subrep.GroupChanged;
    if subrep.CurrentGroupIndex>0 then
    begin
     Result:=true;
     data.DoPrior;
    end
    else
     subrep.SubReportChanged(rpDataChange);
   end
   else
     subrep.SubReportChanged(rpDataChange);
  end;

  inc(FRecordCount);
  if Assigned(FOnProgress) then
  begin
{$IFDEF MSWINDOWS}
   mmlast:=TimeGetTime;
   difmilis:=(mmlast-mmfirst);
{$ENDIF}
{$IFDEF LINUX}
   mililast:=now;
   difmilis:=MillisecondsBetween(mililast,milifirst);
{$ENDIF}
   if difmilis>MILIS_PROGRESS then
   begin
     // Get the time
{$IFDEF MSWINDOWS}
    mmfirst:=TimeGetTime;
{$ENDIF}
{$IFDEF LINUX}
    milifirst:=now;
{$ENDIF}
    docancel:=false;
    FOnProgress(Self,docancel);
    if docancel then
     Raise Exception.Create(SRpOperationAborted);
   end;
  end;
 end;
end;

function TRpReport.NextSection:boolean;
var
 subrep:TRpSubreport;
 sec:TRpSection;
 oldsectionindex:integer;
 lastdetail,firstdetail:integer;
begin
 section:=nil;
 oldsectionindex:=currentsectionindex;


 // Check the condition
 while CurrentSubReportIndex<Subreports.count do
 begin
  subrep:=Subreports.Items[CurrentSubReportIndex].SubReport;
  // The first section are the group footers until
  // CurrentGropup
  while subrep.CurrentGroupIndex<>0 do
  begin
   lastdetail:=subrep.LastDetail;
   firstdetail:=subrep.FirstDetail;
   inc(CurrentSectionIndex);
   if subrep.CurrentGroupIndex>0 then
   begin
    if subrep.CurrentGroupIndex<(CurrentSectionIndex-lastdetail) then
    begin
     // Restore position
     // And the next will be group headers
     if subrep.LastRecord then
     begin
      CurrentSectionIndex:=subrep.Sections.Count;
      subrep.CurrentGroupIndex:=0;
      break;
     end
     else
     begin
      // Send Messages for each group
      subrep.InitGroups(subrep.CurrentGroupIndex);
      // Restores position
      NextRecord(true);
      CurrentSectionIndex:=subrep.FirstDetail-subrep.CurrentGroupIndex;
      subrep.CurrentGroupIndex:=-subrep.CurrentGroupIndex;
      sec:=Subrep.Sections[CurrentSectionIndex].Section;
      if Sec.EvaluatePrintCondition then
      begin
       Section:=sec;
       Subreport:=subrep;
       break;
      end;
     end;
    end
    else
    begin
     Sec:=subrep.Sections[CurrentSectionIndex].Section;
     if Sec.EvaluatePrintCondition then
     begin
      Section:=sec;
      Subreport:=subrep;
      break;
     end;
    end;
   end
   else
   begin
    // Group headers
    if CurrentSectionIndex<firstdetail then
    begin
     sec:=Subrep.Sections.Items[CurrentSectionIndex].Section;
     if sec.EvaluatePrintCondition then
     begin
      Section:=sec;
      Subreport:=subrep;
      break;
     end;
    end
    else
    begin
     subrep.CurrentGroupIndex:=0;
     CurrentSectionIndex:=-1;
    end;
   end;
  end;
  if Assigned(section) then
   break;
  while CurrentSectionIndex<subrep.Sections.Count do
  begin
   if CurrentSectionIndex<0 then
    CurrentSectionIndex:=subrep.FirstDetail
   else
    inc(CurrentSectionIndex);
   if Not subrep.LastRecord then
   begin
    if CurrentSectionIndex>subrep.LastDetail then
    begin
     if oldsectionindex>=0 then
      if NextRecord(false) then
      begin
       CurrentSectionIndex:=subrep.LastDetail;
       break;
      end;
     if Not subrep.LastRecord then
     begin
      CurrentSectionIndex:=subrep.FirstDetail;
      sec:=Subrep.Sections.Items[CurrentSectionIndex].Section;
      if sec.EvaluatePrintCondition then
      begin
       Section:=sec;
       subreport:=subrep;
       break;
      end;
     end
     else
     begin
      CurrentSectionIndex:=subrep.LastDetail;
      subrep.CurrentGroupIndex:=subrep.GroupCount;
      break;
     end;
    end
    else
    begin
     if CurrentSectionIndex<=subrep.LastDetail then
     begin
      sec:=Subrep.Sections.Items[CurrentSectionIndex].Section;
      if sec.EvaluatePrintCondition then
      begin
       Section:=sec;
       subreport:=subrep;
       break;
      end;
     end;
    end;
   end;
  end;
  if ((Not assigned(Section)) AND (subrep.CurrentGroupIndex=0)) then
  begin
   inc(CurrentSubReportIndex);
   if CurrentSubReportIndex>=Subreports.count then
    break;
   subrep:=Subreports.Items[CurrentSubReportIndex].SubReport;
//   subrep.SubReportChanged(rpDataChange);
   CurrentSectionIndex:=-1;
   subrep.LastRecord:=false;
  end
  else
   if subrep.CurrentGroupIndex=0 then
     break;
 end;
 Result:=Assigned(Section);
end;

procedure TRpReport.BeginPrint(Driver:IRpPrintDriver);
var
 i:integer;
 item:TRpAliaslistItem;
 apagesize:TPoint;
begin
 FDriver:=Driver;
 if Not Assigned(FDriver) then
  Raise Exception.Create(SRpNoDriverPassedToPrint);
 metafile.Clear;
 ClearTotalPagesList;
 // Sets page orientation
 if PageOrientation<>rpOrientationDefault then
 begin
  FDriver.SetOrientation(PageOrientation);
 end;
 if PageSize<>rpPageSizeDefault then
 begin
  metafile.PageSize:=-1;
  apagesize:=Driver.SetPagesize(PageSizeQt);
 end
 else
 begin
  metafile.PageSize:=PageSizeQt;
  apagesize:=Driver.GetPageSize;
 end;
 FInternalPageWidth:=apagesize.X;
 FInternalPageHeight:=apagesize.Y;
 // Get the time
{$IFDEF MSWINDOWS}
 mmfirst:=TimeGetTime;
{$ENDIF}
{$IFDEF LINUX}
 milifirst:=now;
{$ENDIF}
 metafile.CustomX:=FInternalPageWidth;
 metafile.CustomY:=FInternalPageHeight;
 metafile.Orientation:=FPageOrientation;
 metafile.BackColor:=FPageBackColor;
 LastPage:=false;
 EndPrint;
 PageNum:=-1;
 FRecordCount:=0;
 CurrentSubReportIndex:=0;
 ActivateDatasets;
 // Evaluator
 if Assigned(FEvaluator) then
 begin
  FEvaluator.free;
  FEvaluator:=nil;
 end;
 FEvaluator:=TRpEvaluator.Create(nil);
 // Insert page numeber
 FEvaluator.AddVariable('Page',fidenpagenum);
 FEvaluator.AddVariable('FREE_SPACE',fidenfreespace);
 FEvaluator.AddVariable('CURRENTGROUP',fidencurrentgroup);
 FEvaluator.AddVariable('FREE_SPACE_CMS',fidenfreespacecms);
 FEvaluator.AddVariable('FREE_SPACE_INCH',fidenfreespaceinch);
 // Insert params into rpEvaluator
 for i:=0 to Params.Count-1 do
 begin
  FEvaluator.NewVariable(params.items[i].Name,params.items[i].Value);
 end;
 // Here identifiers are added to evaluator
 for i:=0 to Identifiers.Count-1 do
 begin
  FEvaluator.AddVariable(FIdentifiers.Strings[i],
   TRpExpression(FIdentifiers.Objects[i]).IdenExpression);
 end;




 FDataAlias.List.Clear;
 for i:=0 to DataInfo.Count-1 do
 begin
  item:=FDataAlias.List.Add;
  item.Alias:=DataInfo.Items[i].Alias;
  if Datainfo.Items[i].Cached then
   item.Dataset:=DataInfo.Items[i].CachedDataset
  else
   item.Dataset:=DataInfo.Items[i].Dataset;
 end;
 FEvaluator.Rpalias:=FDataAlias;

 // Sends the message report header to all components
 for i:=0 to SubReports.Count-1 do
 begin
  Subreports.Items[i].Subreport.SubReportChanged(rpReportStart);
 end;
 Subreports.Items[0].SubReport.SubReportChanged(rpDataChange);

 CurrentSectionIndex:=-1;
 Subreports.Items[0].SubReport.CurrentGroupIndex:=-SubReports.Items[0].Subreport.GroupCount;
 if Subreports.Items[0].SubReport.CurrentGroupIndex<0 then
 begin
  CurrentSectionIndex:=SubReports.Items[0].Subreport.FirstDetail+Subreports.Items[0].SubReport.CurrentGroupIndex-1;
 end;
 section:=nil;
 subreport:=nil;
 if Not NextSection then
 begin
  EndPrint;
  Raise Exception.Create(SRpNothingToPrint);
 end;
 printing:=True;
end;

// Resturns true if is the last pabe
function TRpReport.PrintNextPage:boolean;
var
 printedsomething:boolean;
 pageposy,pageposx:integer;
 sectionext:TPoint;
 pagefooters:TStringList;
 asection:TrpSection;
 pagefooterpos:integer;
 havepagefooters:boolean;
 oldsubreport:TRpSubreport;
 oldprintedsection:TRpSection;
 oldprintedsectionext:TPoint;
 pagespacex:integer;
 sectionextevaluated:boolean;

function CheckSpace:boolean;
begin
 if not sectionextevaluated then
  sectionext:=asection.GetExtension;
 Result:=true;
 if sectionext.Y>freespace then
 begin
  if printedsomething then
  begin
   Result:=false;
  end
  else
  begin
   Raise Exception.Create(SRpNoSpaceToPrint+' '+
    SRpSubReport+':'+IntToStr(CurrentSubReportIndex)+' '+
    SRpSection+':'+IntToStr(CurrentSectionIndex));
  end;
 end;
 sectionextevaluated:=false;
end;

procedure PrintSection(datasection:boolean);
begin
 if datasection then
 begin
  printedsomething:=true;
  oldprintedsection:=section;
  oldprintedsectionext:=sectionext;
 end;
 // If the section is not aligned at bottom of the page then
 if Not asection.AlignBottom then
 begin
  asection.Print(pageposx,pageposy,metafile);
  freespace:=freespace-sectionext.Y;
  pageposy:=pageposy+sectionext.Y;
 end
 else
 // Align to bottom
 begin
  pageposy:=pageposy+freespace-sectionext.Y;
  asection.Print(pageposx,pageposy,metafile);
  freespace:=0;
 end;
end;

procedure PrintFixedSections(headers:boolean);
var
 pheader,pfooter:integer;
 pheadercount,pfootercount:integer;
 i:integer;
 psection:TRpSection;
 afirstdetail:integer;
begin
 if Headers then
 begin
  // Print the header fixed sections
  pheader:=subreport.FirstPageHeader;
  pheadercount:=subreport.PageHeaderCount;
  for i:=0 to pheadercount-1 do
  begin
   psection:=subreport.Sections.Items[i+pheader].Section;
   if psection.EvaluatePrintCondition then
   begin
    asection:=psection;
    CheckSpace;
    PrintSection(false);
   end;
  end;
  // Now prints repeated group headers
  afirstdetail:=subreport.FirstDetail;
  for i:=subreport.GroupCount downto 1 do
  begin
   psection:=subreport.Sections.Items[afirstdetail-i].Section;
   if psection.PageRepeat then
   begin
    if Abs(subreport.CurrentGroupIndex)<i then
    begin
     if psection.EvaluatePrintCondition then
     begin
      asection:=psection;
      CheckSpace;
      PrintSection(false);
     end;
    end;
   end;
  end;

  pagefooterpos:=pageposy+freespace;
  // Reserve space for page footers
  // Print conditions for footers are evaluated at the begining of
  // the page
  pfooter:=subreport.FirstPageFooter;
  pfootercount:=subreport.PageFooterCount;
  for i:=0 to pfootercount-1 do
  begin
   psection:=subreport.Sections.Items[i+pfooter].Section;
   if psection.EvaluatePrintCondition then
   begin
    asection:=psection;
    havepagefooters:=true;
    CheckSpace;
    pagefooters.add(IntToStr(i+pfooter));
    pagefooterpos:=pageposy+freespace-sectionext.Y;
    freespace:=freespace-sectionext.Y;
   end;
  end;
 end
 else
 begin
  // Print page footers
  pageposy:=pagefooterpos;
  for i:=0 to pagefooters.Count-1 do
  begin
   asection:=oldsubreport.Sections.Items[StrToInt(pagefooters.Strings[i])].Section;
   sectionext:=asection.GetExtension;
   PrintSection(false);
  end;
 end;
end;


begin
 if Not Assigned(Section) then
  Raise Exception.Create(SRpLastPageReached);
 if assigned(subreport) then
  subreport.SubReportChanged(rpPageChange);
 havepagefooters:=false;
 sectionextevaluated:=false;
 pageposy:=FTopMargin;
 pageposx:=FLeftMargin;
 oldprintedsection:=nil;
 printedsomething:=false;
 inc(Pagenum);
 if not printingonepass then
 begin
  if fmetafile.PageCount<=PageNum then
  begin
   fmetafile.NewPage;
  end;
  fmetafile.CurrentPage:=PageNum;
 end
 else
  fmetafile.NewPage;


 if PageOrientation=rpOrientationLandscape then
 begin
  freespace:=FInternalPageWidth;
  pagespacex:=FInternalPageheight;
 end
 else
 begin
  freespace:=FInternalPageheight;
  pagespacex:=FInternalPageWidth;
 end;
 freespace:=freespace-FTopMargin-FBottomMargin;

 pagefooters:=TStringList.Create;
 try
  // Fills the page with fixed sections
  PrintFixedSections(true);
  oldsubreport:=subreport;
  while Assigned(section)  do
  begin
   if printedsomething then
   begin
    if section.EvaluateBeginPage then
     break;
   end;
   asection:=section;
   // Horz.Desp.
   if Assigned(oldprintedsection) then
   begin
    if oldprintedsection.HorzDesp then
    begin
     if section.HorzDesp then
     begin
      sectionext:=section.GetExtension;
      sectionextevaluated:=true;
      if (pageposx+oldprintedsectionext.X+sectionext.X)<=pagespacex then
      begin
       pageposx:=pageposx+oldprintedsectionext.X;
       pageposy:=pageposy-oldprintedsectionext.Y;
      end
      else
       pageposx:=FLeftMargin;
     end
     else
     begin
      pageposx:=FLeftMargin;
     end;
    end
    else
    begin
     pageposx:=FLeftMargin;
    end;
   end;
   if Not CheckSpace then
    break;
   PrintSection(true);
   NextSection;
   if printedsomething then
   begin
    if asection.SkipPage then
     break;
   end;
   // if Subreport changed and has have pagefooter
   if ((oldsubreport<>subreport) and (havepagefooters)) then
    break;
   oldsubreport:=subreport;
  end;
  // Fills the page with fixed sections
  PrintFixedSections(false);
 finally
  pagefooters.Free;
 end;
 Result:=Not Assigned(Section);
 LastPage:=Result;
end;




initialization
 // Need clas registration to be streamable
 Classes.RegisterClass(TRpSection);
 Classes.RegisterClass(TRpReport);
 Classes.RegisterClass(TRpSubReport);
 Classes.RegisterClass(TRpImage);
 Classes.RegisterClass(TRpShape);
 Classes.RegisterClass(TRpLabel);
 Classes.RegisterClass(TRpExpression);

end.
