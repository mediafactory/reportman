{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
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

uses Classes,sysutils,rptypes,rpsubreport,rpsection,rpmdconsts,
 rpdatainfo,rpparams,rplabelitem,rpdrawitem,rpeval,rptypeval,
 rpmetafile,rpmdbarcode,rpmdchart,
{$IFDEF USEVARIANTS}
 types,dateutils,Variants,
{$ENDIF}
 rpalias,db,
{$IFDEF USEZLIB}
 rpmzlib,
{$ENDIF}
 rpdataset,
{$IFDEF LINUX}
  Libc,
{$ENDIF}
{$IFDEF MSWINDOWS}
  mmsystem,windows,
{$ENDIF}
 rpmunits;


const
 MILIS_PROGRESS_DEFAULT=500;
 // 1 cms=574
 // 0.5 cms=287
 CONS_DEFAULT_GRIDWIDTH=115;
 CONS_DEFAULT_GRIDCOLOR=$FF0000;
 CONS_MIN_GRID_WIDTH=50;
 // 29,7/2.54*1440
 DEFAULT_PAGEHEIGHT=16837;
 DEFAULT_PAGEWIDTH=11906;
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
  private
   FReport:TRpReport;
  protected
   function GeTRpValue:TRpValue;override;
  public
   varname:string;
  end;

 TIdenEOF=class(TIdenFunction)
  private
   FReport:TRpReport;
  protected
   function GeTRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 TRpReport=class(TComponent)
  private
   pageposy,pageposx:integer;
   FCompose:Boolean;
   FSubReports:TRpSubReportList;
   FPageOrientation:TRpOrientation;
   FPagesize:TRpPagesize;
   FPageSizeQt:integer;
   FPageWidth:TRpTwips;
   FPageHeight:TRpTwips;
   FCustomPageWidth:TRpTwips;
   FCustomPageHeight:TRpTwips;
   FInternalPageWidth:TRpTwips;
   FInternalPageHeight:TRpTwips;
   FPageBackColor:TRpColor;
   FPreviewStyle:TRpPreviewStyle;
   FPreviewWindow:TRpPreviewWindowStyle;
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
   Fidenpagenumgroup:TIdenReportVar;
   FidenEof:TIdenEof;
   Fidenfreespace:TIdenReportVar;
   Fidenpagewidth:TIdenReportVar;
   Fidenpageheight:TIdenReportVar;
   Fidenfreespacecms:TIdenReportVar;
   Fidenfreespaceinch:TIdenReportVar;
   Fidencurrentgroup:TIdenReportVar;
   FIdenfirstsection:TIdenReportVar;
   FCopies:integer;
   FCollateCopies:boolean;
   FTwoPass:boolean;
   FTotalPagesList:TList;
   FAliasList:TRpAlias;
   printingonepass:boolean;
   freespace:integer;
   FMilisProgres:integer;
   FPrinterFonts:TRpPrinterFontsOption;
{$IFDEF MSWINDOWS}
   mmfirst,mmlast:DWORD;
{$ENDIF}
{$IFDEF LINUX}
   milifirst,mililast:TDatetime;
{$ENDIF}
   difmilis:int64;
   printedsomething:Boolean;
   FPendingSections:TStringList;
   FPrinterSelect:TRpPrinterSelect;
   FPrintOnlyIfDataAvailable:Boolean;
   gheaders,gfooters:TList;
   FStreamFormat:TRpStreamFormat;
   FReportAction:TRpReportActions;
   procedure  FillGlobalHeaders;
   procedure FInternalOnReadError(Reader: TReader; const Message: string;
    var Handled: Boolean);
   procedure SetSubReports(Value:TRpSubReportList);
   procedure SetDataInfo(Value:TRpDataInfoList);
   procedure SetDatabaseInfo(Value:TRpDatabaseInfoList);
   procedure SetParams(Value:TRpParamList);
   procedure ClearTotalPagesList;
   procedure SetGridWidth(Value:TRpTwips);
   procedure SetGridHeight(Value:TRpTwips);
   procedure CheckIfDataAvailable;
   procedure UpdateCachedSources(alias:string);
   procedure CheckProgress;
   function OnGraphicOp(Top,Left,Width,Height:integer;
    DrawStyle:integer;BrushStyle:integer;BrushColor:integer;
    PenStyle:integer;PenWidth:integer; PenColor:integer):Boolean;
   function OnTextOp(Top,Left,Width,Height:integer;
    Text,LFontName,WFontName:WideString;
    FontSize,FontRotation,FontStyle,FontColor,Type1Font:integer;
    CutText:boolean;Alignment:integer;WordWrap,RightToLeft:Boolean;
    PrintStep,BackColor:integer;transparent:boolean):Boolean;
  protected
    section:TRpSection;
    subreport:TRpSubreport;
    procedure Notification(AComponent:TComponent;Operation:TOperation);override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent);override;
    procedure Loaded;override;
    function NextSection(child:boolean):boolean;
    // Skip to next record returns true if a group has
    // changed and sets internally CurrentGroup
    function NextRecord(grouprestore:boolean):boolean;
  public
   Ininumpage:boolean;
   FailIfLoadExternalError:Boolean;
   printing:boolean;
   CurrentSubReportIndex:integer;
   CurrentSectionIndex:integer;
   PageNum:integer;
   PageNumGroup:integer;
   LastPage:Boolean;
   property RecordCount:integer read FRecordCount;
   property Metafile:TRpMetafileReport read FMetafile;
   property Identifiers:TStringList read FIdentifiers;
   constructor Create(AOwner:TComponent);override;
   destructor Destroy;override;
   procedure FreeSubreports;
   procedure AddReportItemsToEvaluator(eval:TRpEvaluator);
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
   procedure Compose(PrevReport:TRpReport;execute:Boolean;ADriver:IRpPrintDriver);
   procedure PrintAll(Driver:IRpPrintDriver);
   procedure PrintRange(Driver:IRpPrintDriver;allpages:boolean;
    frompage,topage,copies:integer;collate:boolean);
   property OnProgress:TRpProgressEvent read FOnProgress write FOnProgress;
   property AliasList:TRpAlias read FAliasList write FAliasList;
   property idenpagenum:TIdenReportVar read fidenpagenum;
   property ideneof:TIdenEof read fideneof;
   property idenfreespace:TIdenReportVar read fidenfreespace;
   property idenfreespacecms:TIdenReportVar read fidenfreespacecms;
   property idenfreespaceinch:TIdenReportVar read fidenfreespaceinch;
   property idencurrentgroup:TIdenReportVar read fidencurrentgroup;
   property MilisProgres:integer read FMilisProgres write FMilisProgres
    default MILIS_PROGRESS_DEFAULT;
   procedure AlignSectionsTo1_6inchess;
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
   property CustomPageHeight:TRpTwips read FCustomPageHeight write FCustomPageHeight
    default DEFAULT_PAGEHEIGHT;
   property CustomPageWidth:TRpTwips read FCustomPageWidth write FCustomPageWidth
    default DEFAULT_PAGEWIDTH;
   property PageBackColor:TRpColor read FPageBackColor write FPageBackColor;
   property PreviewStyle:TRpPreviewStyle read FPreviewStyle
    write FPreviewStyle default spWide;
   property PreviewWindow:TRpPreviewWindowStyle read FPreviewWindow
    write FPreviewWindow default spwNormal;
   property LeftMargin:TRpTwips read FLeftMargin write FLeftMargin
    default DEFAULT_LEFTMARGIN;
   property TopMargin:TRpTwips read FTopMargin write FTopMargin
    default DEFAULT_TOPMARGIN;
   property RightMargin:TRpTwips read FRightMargin write FRightMargin
    default DEFAULT_RIGHTMARGIN;
   property BottomMargin:TRpTwips read FBottomMargin write FBottomMargin
    default DEFAULT_BOTTOMMARGIN;
   property PrinterSelect:TRpPrinterSelect read FPrinterSelect write FPrinterSelect
    default pRpDefaultPrinter;
   // Subreports
   property SubReports:TRpSubReportList read FSubReports write SetSubReports;
   property DataInfo:TRpDataInfoList read FDataInfo write SetDataInfo;
   property DatabaseInfo:TRpDatabaseInfoList read FDatabaseInfo write SetDatabaseInfo;
   property Params:TRpParamList read FParams write SetParams;
   // Language
   property Language:integer read FLanguage write FLanguage default -1;
   // Other
   property Copies:integer read FCopies write FCopies default 1;
   property CollateCopies:boolean read FCollateCopies write FCollateCopies default false;
   property TwoPass:boolean read FTwoPass write FTwoPass default false;
   property PrinterFonts:TRpPrinterFontsOption read FPrinterFonts
    write FPrinterFonts default rppfontsdefault;
   property PrintOnlyIfDataAvailable:Boolean read FPrintOnlyIfDataAvailable
    write FPrintOnlyIfDataAvailable default false;
   property StreamFormat:TRpStreamFormat read FStreamFormat
    write FStreamFormat default rpStreamzlib;
   property ReportAction:TRpReportActions read FReportAction write FReportAction;
 end;

procedure RegisterRpReportClasses;

implementation

uses rpprintitem, rpsecutil;

function TIdenReportVar.GeTRpValue:TRpValue;
var
 subrep:TRpSubReport;
begin
 if varname='PAGE' then
  Result:=freport.PageNum+1
 else
  if varname='PAGENUM' then
   Result:=freport.PageNumGroup+1
  else
  if varname='FREE_SPACE_TWIPS' then
   Result:=freport.freespace
  else
   if varname='FREE_SPACE_CMS' then
    Result:=twipstocms(freport.freespace)
   else
    if varname='FREE_SPACE_INCH' then
     Result:=twipstocms(freport.freespace)
    else
     if varname='CURRENTGROUP' then
     begin
      if freport.CurrentSubreportIndex>=freport.Subreports.Count then
       subrep:=freport.Subreports.Items[freport.CurrentSubreportIndex-1].SubReport
      else
        subrep:=freport.Subreports.Items[freport.CurrentSubreportIndex].SubReport;
      if subrep.LastRecord then
       Result:=subrep.GroupCount
      else
       Result:=subrep.CurrentGroupIndex;
     end
     else
     if varname='FIRSTSECTION' then
     begin
      Result:=not freport.printedsomething;
     end
     else
     if varname='PAGEWIDTH' then
     begin
      Result:=freport.FInternalPageWidth;
     end
     else
     if varname='PAGEHEIGHT' then
     begin
      Result:=freport.FInternalPageHeight;
     end;
end;

// Constructors and destructors
constructor TRpReport.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FStreamFormat:=rpStreamzlib;
 gheaders:=TList.Create;
 gfooters:=TList.Create;
 FailIfLoadExternalError:=True;
 FMilisProgres:=MILIS_PROGRESS_DEFAULT;
 FLanguage:=-1;
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
 FCustomPageWidth:=DEFAULT_PAGEWIDTH;
 FCustomPageheight:=DEFAULT_PAGEHEIGHT;
 FPreviewStyle:=spWide;
 // Def values of grid
 FGridVisible:=True;
 FGridEnabled:=True;
 FGridColor:=CONS_DEFAULT_GRIDCOLOR;
 FGridLines:=False;
 FGridWidth:=CONS_DEFAULT_GRIDWIDTH;
 FGridHeight:=CONS_DEFAULT_GRIDWIDTH;
 FPendingSections:=TStringList.Create;
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
 FIdenPagenumgroup:=TIdenReportVar.Create(nil);
 Fidenpagenumgroup.FReport:=self;
 FidenPagenumgroup.varname:='PAGENUM';
 FIdenfreespace:=TIdenReportVar.Create(nil);
 Fidenfreespace.varname:='FREE_SPACE_TWIPS';
 Fidenfreespace.FReport:=self;
 FIdenpagewidth:=TIdenReportVar.Create(nil);
 Fidenpagewidth.varname:='PAGEWIDTH';
 Fidenpagewidth.FReport:=self;
 FIdenpageheight:=TIdenReportVar.Create(nil);
 Fidenpageheight.varname:='PAGEHEIGHT';
 Fidenpageheight.FReport:=self;
 FIdenfreespacecms:=TIdenReportVar.Create(nil);
 Fidenfreespacecms.varname:='FREE_SPACE_CMS';
 Fidenfreespacecms.FReport:=self;
 FIdenfreespaceinch:=TIdenReportVar.Create(nil);
 Fidenfreespaceinch.varname:='FREE_SPACE_INCH';
 Fidenfreespaceinch.FReport:=self;
 FIdencurrentgroup:=TIdenReportVar.Create(nil);
 Fidencurrentgroup.varname:='CURRENTGROUP';
 Fidencurrentgroup.FReport:=self;
 FIdenfirstsection:=TIdenReportVar.Create(nil);
 Fidenfirstsection.varname:='FIRSTSECTION';
 FidenFirstSection.FReport:=self;
 FIdeneof:=TIdenEOF.Create(nil);
 Fideneof.FReport:=self;
 // Metafile
 FMetafile:=TRpMetafileReport.Create(nil);
 FDataAlias:=TRpAlias.Create(nil);
 FTotalPagesList:=TList.Create;
 // Other
 FPrinterFonts:=rppfontsdefault;
 FReportAction:=[];
end;

procedure  TRpReport.FillGlobalHeaders;
var
 subrep:TRpSubReport;
 i,j:integer;
 k:integer;
begin
 gheaders.clear;
 gfooters.clear;
 for i:=0 to Subreports.Count-1 do
 begin
  subrep:=SubReports.Items[i].SubReport;
  j:=subrep.FirstPageHeader;
  for k:=0 to subrep.PageHeaderCount-1 do
  begin
   if subrep.Sections[j+k].Section.Global then
    gheaders.Insert(0,subrep.Sections[j+k].Section);
  end;
  j:=subrep.FirstPageFooter;
  for k:=0 to subrep.PageFooterCount-1 do
  begin
   if subrep.Sections[j+k].Section.Global then
    gfooters.Add(subrep.Sections[j+k].Section);
  end;
 end;
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
 gheaders.free;
 gfooters.free;
 FPendingSections.Free;
 FSubReports.free;
 FDataInfo.free;
 FDatabaseInfo.free;
 FParams.free;
 FIdentifiers.free;
 FMetafile.Free;
 FDataAlias.Free;
 FIdenPagenum.free;
 FIdenPagenumgroup.free;
 Fidenfreespace.free;
 FIdenPagewidth.free;
 FIdenPageHeight.free;
 FIdenCurrentGroup.free;
 FIdenFirstSection.free;
 Fidenfreespacecms.free;
 Fidenfreespaceinch.free;
 FIdenEof.free;
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
 rpchart:TRpChart;
begin
 inherited Loaded;

 for i:=0 to Subreports.Count-1 do
 begin
  subrep:=Subreports.items[i].SubReport;
  for j:=0 to Subrep.Sections.Count-1 do
  begin
   sec:=SubRep.Sections.Items[j].Section;
   // If it's a external section try to load it
   sec.LoadExternal;

   for k:=0 to sec.ReportComponents.Count-1 do
   begin
    comp:=sec.ReportComponents.items[k].Component;
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
    if (comp is TRpChart) then
    begin
     rpchart:=TRpChart(comp);
     if Length(rpchart.Identifier)>0 then
     begin
      try
       FIdentifiers.AddObject(rpchart.Identifier,comp);
      except
       rpchart.Identifier:='';
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
  if (AComponent is TRpChart) then
  begin
   if Length(TRpChart(AComponent).Identifier)>0 then
   begin
    index:=FIdentifiers.IndexOf(TRpChart(AComponent).Identifier);
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
 if operation=Opinsert then
 begin
  if (AComponent is TRpExpression) then
  begin
   if Length(TRpExpression(AComponent).Identifier)>0 then
   begin
    index:=FIdentifiers.IndexOf(TRpExpression(AComponent).Identifier);
    if index>=0 then
     TRpExpression(AComponent).Identifier:=''
    else
     FIdentifiers.AddObject(TRpExpression(AComponent).Identifier,AComponent);
   end;
  end
  else
  if (AComponent is TRpChart) then
  begin
   if Length(TRpChart(AComponent).Identifier)>0 then
   begin
    index:=FIdentifiers.IndexOf(TRpChart(AComponent).Identifier);
    if index>=0 then
     TRpChart(AComponent).Identifier:=''
    else
     FIdentifiers.Add(TRpChart(AComponent).Identifier);
   end;
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
{$IFDEF USEZLIB}
 zstream:TCompressionStream;
{$ENDIF}
 theformat:TRpStreamFormat;
 memstream:TMemoryStream;
begin
 theformat:=FStreamFormat;
{$IFNDEF USEZLIB}
 if theformat=rpStreamZLib then
  theformat:=rpStreambinary;
{$ENDIF}
{$IFDEF USEZLIB}
 if theformat=rpStreamZLib then
 begin
  zstream:=TCompressionStream.Create(clDefault,Stream);
  try
    zstream.WriteComponent(Self);
  finally
   zstream.free;
  end;
 end
 else
{$ENDIF}
 if theformat=rpStreamBinary then
 begin
  Stream.WriteComponent(Self);
 end
 else
 begin
  memstream:=TMemoryStream.Create;
  try
   memstream.WriteComponent(Self);
   memstream.Seek(0,soFromBeginning);
   ObjectBinaryToText(memstream,Stream);
  finally
   memstream.free;
  end;
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
 memstream,amemstream:TMemoryStream;
 readed:integer;
 buf:pointer;
{$IFDEF USEZLIB}
 zlibs:TDeCompressionStream;
{$ENDIF}
 theformat:TRpStreamFormat;
 firstchar:char;
begin
 // FreeSubrepots
 FreeSubreports;
 MemStream:=TMemoryStream.Create;
 try
  // Copy to memory stream
  buf:=AllocMem(120000);
  try
   repeat
    readed:=Stream.Read(buf^,120000);
    memstream.Write(buf^,readed);
   until readed<120000;
  finally
   freemem(buf);
  end;
  memstream.Seek(0,soFrombeginning);
  // Looks stream type
  if (memstream.size<1) then
   Raise Exception.Create(SRpStreamFormat);
  firstchar:=PChar(memstream.memory)^;
  if firstchar='x' then
   theformat:=rpStreamzlib
  else
   if firstchar='o' then
    theformat:=rpStreamText
   else
    theformat:=rpStreambinary;
{$IFNDEF USEZLIB}
  if theformat=rpStreamzlib then
   Raise Exception.Create(SRpZLibNotSupported);
{$ENDIF}
{$IFDEF USEZLIB}
  if theformat=rpStreamzlib then
  begin
   amemstream:=TMemoryStream.Create;
   try
    zlibs:=TDeCompressionStream.Create(MemStream);
    try
     // Decompress
     buf:=AllocMem(120000);
     try
      repeat
       readed:=zlibs.Read(buf^,120000);
       amemstream.Write(buf^,readed);
      until readed<120000;
     finally
      freemem(buf);
     end;
     amemstream.Seek(0,soFromBeginning);
     reader:=TReader.Create(amemstream,1000);
     try
      reader.OnError:=FInternalOnReadError;
      reader.ReadRootComponent(Self);
     finally
      reader.free;
     end;
    finally
     zlibs.Free;
    end;
   finally
    amemstream.free;
   end;
  end
  else
{$ENDIF}
  if theformat=rpStreambinary then
  begin
   reader:=TReader.Create(memstream,1000);
   try
    reader.OnError:=FInternalOnReadError;
    reader.ReadRootComponent(Self);
   finally
    reader.free;
   end;
  end
  else
  begin
   amemstream:=TMemoryStream.Create;
   try
    ObjectTextToBinary(memstream,amemstream);
    amemstream.Seek(0,soFromBeginning);
    reader:=TReader.Create(amemstream,1000);
    try
     reader.OnError:=FInternalOnReadError;
     reader.ReadRootComponent(Self);
    finally
     reader.free;
    end;
   finally
    amemstream.free;
   end;
  end;
 finally
  MemStream.free;
 end;
end;

procedure TRpReport.FInternalOnReadError(Reader: TReader; const Message: string;
    var Handled: Boolean);
begin
 Handled:=false;
// if Pos('AllText',Message)>0 then
//  Handled:=True;
 if Assigned(FOnReadError) then
  reader.OnError:=FOnReadError
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
// FSubReports.Delete(i);
 FSubReports.Items[i].Free;
end;

procedure TRpReport.SetParams(Value:TRpParamList);
begin
 FParams.Assign(Value);
end;

procedure TRpReport.ActivateDatasets;
var
 i,index:integer;
 alias:string;
 dbinfo:TRpDatabaseInfoItem;
 dbalias:string;
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
    dbalias:=FDataInfo.Items[index].DatabaseAlias;
    index:=DatabaseInfo.IndexOf(dbalias);
    if index<0 then
     Raise Exception.Create(SRpSubreportAliasNotFound+':'+alias);
    dbinfo:=DatabaseInfo.Items[index];
    index:=DataInfo.IndexOf(alias);
    if (Not (dbinfo.Driver in [rpdataibx,rpdatamybase])) then
    begin
     FDataInfo.Items[index].Cached:=true;
    end;
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
   CheckProgress;
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
  FDataInfo.Items[i].DisConnect;
 end;
 for i:=0 to FDatabaseInfo.Count-1 do
 begin
  FDatabaseInfo.Items[i].DisConnect;
 end;
end;

procedure TRpReport.PrintRange(Driver:IRpPrintDriver;allpages:boolean;
    frompage,topage,copies:integer;collate:boolean);
var
 i,j,k:integer;
 finished:boolean;
 printedfirst:boolean;
 endprintexecuted:boolean;
 reportcopies:integer;
 forcetwopass:boolean;
 hardwarecopies:integer;
 hardwarecollate:boolean;
begin
 if copies<1 then
  exit;
 hardwarecopies:=1;
 hardwarecollate:=false;
 if (copies>1) then
 begin
  if collate then
  begin
   if Driver.SupportsCopies(copies) then
   begin
    if Driver.SupportsCollation then
    begin
     hardwarecopies:=copies;
     hardwarecollate:=true;
     collate:=false;
    end;
   end;
  end
  else
  begin
   if Driver.SupportsCopies(copies) then
    hardwarecopies:=copies;
  end;
 end;
 endprintexecuted:=False;
 printedfirst:=false;
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
 // Two pass report printing requires previeous calculation
 // of the metafile
 forceTwoPass:=TwoPass;
 if ((copies>1) and collate) then
  forcetwopass:=true;
 reportcopies:=1;
 if collate then
 begin
  reportcopies:=copies;
  copies:=1;
 end;
 if forceTwoPass then
 begin
  BeginPrint(Driver);
  try
   Driver.NewDocument(metafile,hardwarecopies,hardwarecollate);
   try
    // Calculate the report first
    while Not PrintNextPage do;

    EndPrint;
    endprintexecuted:=true;
    // Then draw the generated metafile
    if topage>metafile.PageCount-1 then
     topage:=metafile.PageCount-1;
    for k:=1 to reportcopies do
    begin
     for j:=frompage to topage do
     begin
      for i:=1 to copies do
      begin
       if printedfirst then
        Driver.NewPage;
       printedfirst:=true;
       Driver.DrawPage(metafile.pages[j]);
       Driver.EndPage;
       if hardwarecopies>1 then
        break;
      end;
     end;
    end;
    Driver.EndDocument;
   except
    Driver.AbortDocument;
    Raise;
   end;
  except
   if not endprintexecuted then
    EndPrint;
   Raise;
  end;
  exit;
 end;
 // One pass is more efficient in memory consuming
 // it frees each printed page
 printingonepass:=true;
 try
  BeginPrint(Driver);
  try
   Driver.NewDocument(metafile,hardwarecopies,hardwarecollate);
   try
    finished:=false;
    while Not PrintNextPage do
    begin
     if ((PageNum>=frompage) and  (PageNum<=topage)) then
     begin
      for i:=0 to copies-1 do
      begin
       if printedfirst then
        Driver.NewPage;
       printedfirst:=true;
       Driver.DrawPage(metafile.pages[0]);
       Driver.EndPage;
       if hardwarecopies>1 then
        break;
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
      for i:=0 to copies-1 do
      begin
       if printedfirst then
        Driver.NewPage;
       printedfirst:=true;
       Driver.DrawPage(metafile.pages[0]);
       if hardwarecopies>1 then
        break;
      end;
     end;
    end;
    Driver.EndDocument;
   except
    Driver.AbortDocument;
    Raise;
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
  Driver.NewDocument(metafile,1,false);
  try
   while Not PrintNextPage do;
  finally
   Driver.EndDocument;
  end;
 finally
  EndPrint;
 end;
end;

procedure TRpReport.EndPrint;
begin
 DeActivateDatasets;
 FEvaluator.Free;
 FEvaluator:=nil;
 section:=nil;
 subreport:=nil;
 printing:=false;
 metafile.UpdateTotalPages(FTotalPagesList);
end;

procedure TRpReport.UpdateCachedSources(alias:string);
var
 i:integer;
begin
 for i:=0 to datainfo.Count-1 do
 begin
  if datainfo.Items[i].DataSource=alias then
  begin
   if datainfo.Items[i].Cached then
   begin
    datainfo.Items[i].CachedDataset.DoClose;
    datainfo.Items[i].CachedDataset.DoOpen;
   end
   else
    if Not datainfo.Items[i].Dataset.Active then
     datainfo.Items[i].Connect(databaseinfo,params);
   UpdateCachedSources(datainfo.items[i].alias);
  end;
 end;
end;

function TRpReport.NextRecord(grouprestore:boolean):boolean;
var
 subrep:TRpSubreport;
 index:integeR;
 data:TRpDataset;
begin
{$IFDEF QUERYLINKBUG}
 datainfo.DisableLinks;
{$ENDIF}
 data:=nil;
 Result:=false;
 subrep:=Subreports.Items[CurrentSubreportIndex].SubReport;
 if Length(Trim(subrep.Alias))<1 then
  subrep.Lastrecord:=True
 else
 begin
  index:=DataInfo.IndexOf(subrep.Alias);
  if index<0 then
   Raise TRpReportException.Create(SRPAliasNotExists+subrep.alias,subrep,SRpMainDataset);
  if datainfo.Items[index].Cached then
  begin
   data:=DataInfo.Items[index].CachedDataset;
   data.DoNext;
  end
  else
   DataInfo.Items[index].Dataset.Next;
  UpdateCachedSources(subrep.Alias);
  // Update all dependent cached datasets
  // If its the last record no group change
  if not grouprestore then
  begin
   if datainfo.Items[index].Cached then
    subrep.LastRecord:=data.Eof
   else
    subrep.LastRecord:=datainfo.Items[index].Dataset.Eof;
  end;
  if Not Subrep.LastRecord then
  begin
   if not grouprestore then
   begin
    subrep.GroupChanged;
    if subrep.CurrentGroupIndex>0 then
    begin
     Result:=true;
     if datainfo.Items[index].Cached then
      data.DoPrior
     else
      datainfo.Items[index].Dataset.Prior;
    end
    else
     subrep.SubReportChanged(rpDataChange);
   end
   else
    subrep.SubReportChanged(rpDataChange);
  end;

  inc(FRecordCount);

  CheckProgress;
 end;
{$IFDEF QUERYLINKBUG}
 datainfo.EnableLinks;
{$ENDIF}
end;


procedure TRpReport.CheckProgress;
var
 docancel:boolean;
begin
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
   if difmilis>FMilisProgres then
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

function TRpReport.NextSection(child:boolean):boolean;
var
 subrep:TRpSubreport;
 sec:TRpSection;
 oldsection:TRpSection;
// oldsectionindex:integer;
 lastdetail,firstdetail:integer;
 dataavail:boolean;
 index:integer;
begin
 oldsection:=section;
 section:=nil;
// oldsectionindex:=currentsectionindex;
 // If the old selected section has a child subreport then execute first
 if (Assigned(oldsection) AND child) then
 begin
  if Assigned(oldsection.ChildSubReport) then
  begin
   dataavail:=false;
   subrep:=TRpSubReport(oldsection.ChildSubReport);
   if (Length(subrep.Alias)<1) then
    dataavail:=true
   else
   begin
    index:=DataInfo.IndexOf(subrep.Alias);
    if Datainfo.Items[index].Cached then
    begin
     if Datainfo.Items[index].Dataset.Bof then
     begin
      Datainfo.Items[index].CachedDataset.DoClose;
      Datainfo.Items[index].CachedDataset.DoOpen;
     end;
     if (Not Datainfo.Items[index].Dataset.Eof) then
     begin
      dataavail:=true;
     end;
    end
    else
    begin
     if (Not Datainfo.Items[index].Dataset.Eof) then
     begin
      dataavail:=true;
     end;
    end;
   end;
   subrep.LastRecord:=Not dataavail;
   if dataavail then
   begin
    subrep.SubReportChanged(rpSubReportStart);
//    subrep.SubReportChanged(rpDataChange);
    subreport:=subrep;
    section:=nil;
//    oldsectionindex:=-1;
    CurrentSectionIndex:=-1;
    FPendingSections.AddObject(IntToStr(CurrentSubReportIndex),oldsection);
    CurrentSubReportIndex:=Subreports.IndexOf(subreport);
    subreport.SubReportChanged(rpDataChange);
    Subreport.CurrentGroupIndex:=-Subreport.GroupCount;
    if SubReport.CurrentGroupIndex<0 then
    begin
     CurrentSectionIndex:=Subreport.FirstDetail+SubReport.CurrentGroupIndex-1;
    end;
   end;
  end;
 end;

 // Check the condition
 while CurrentSubReportIndex<Subreports.count do
 begin
  CheckProgress;

  subrep:=Subreports.Items[CurrentSubReportIndex].SubReport;
  // The first section are the group footers until
  // CurrentGropup
  while subrep.CurrentGroupIndex<>0 do
  begin
   CheckProgress;

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
   CheckProgress;
   if CurrentSectionIndex<0 then
    CurrentSectionIndex:=subrep.FirstDetail
   else
    inc(CurrentSectionIndex);
   if Not subrep.LastRecord then
   begin
    if CurrentSectionIndex>subrep.LastDetail then
    begin
//     if oldsectionindex>=0 then
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
   // If it's a child subreport
   // Returns null section so pending will print
   if Assigned(subrep.ParentSubReport) then
    break;
   repeat
    inc(CurrentSubReportIndex);
    if CurrentSubReportIndex>=Subreports.count then
     break;
    subrep:=Subreports.Items[CurrentSubReportIndex].SubReport;
    if subrep.ParentSubreport=nil then
    begin
     if subrep.IsDataAvailable then
     begin
      subrep.SubReportChanged(rpSubReportStart);
      subrep.SubReportChanged(rpDataChange);
      break;
     end;
    end;
   until false;
   if CurrentSubReportIndex>=Subreports.count then
    break;
//   subrep.SubReportChanged(rpDataChange);
   CurrentSectionIndex:=subrep.FirstDetail-subrep.GroupCount-1;
   subrep.CurrentGroupIndex:=-subrep.GroupCount;
   subrep.LastRecord:=false;
  end
  else
   if subrep.CurrentGroupIndex=0 then
     break;
 end;

 Result:=Assigned(Section);
 // If there are still pending sections
 if not Assigned(Section) then
 begin
  if FPendingSections.Count>0 then
  begin
   Section:=TRpSection(FPendingSections.Objects[FPendingSections.Count-1]);
   CurrentSubReportIndex:=StrToInt(FPendingSections.Strings[FPendingSections.Count-1]);
   FPendingSections.Delete(FPendingSections.Count-1);
   Subreport:=TRpSubReport(Section.SubReport);
   Currentsectionindex:=Subreport.Sections.IndexOf(Section);
   NextSection(false);
  end;
 end;
end;


procedure TRpReport.CheckIfDataAvailable;
var
 dataavail:boolean;
 dinfo:TRpDatainfoItem;
 i,index:integer;
begin
 if Not FPrintOnlyIfDataAvailable then
  exit;
 dataavail:=false;
 for i:=0 to SubReports.Count-1 do
 begin
  if Length(SubReports.Items[i].SubReport.Alias)>0 then
  begin
   index:=datainfo.IndexOf(SubReports.Items[i].SubReport.Alias);
   if index>=0 then
   begin
    dinfo:=datainfo.Items[index];
    if dinfo.Dataset.Active then
    begin
     if Not dinfo.Dataset.Eof then
     begin
      dataavail:=true;
      break;
     end;
    end;
   end;
  end;
 end;
 if not dataavail then
  Raise Exception.Create(SRpNoDataAvailableToPrint);
end;

procedure TRpReport.AddReportItemsToEvaluator(eval:TRpEvaluator);
var
 i:integer;
begin
 // Insert params into rpEvaluator
 for i:=0 to Params.Count-1 do
 begin
  eval.NewVariable(params.items[i].Name,params.items[i].Value);
 end;
 // Here identifiers are added to evaluator
 for i:=0 to Identifiers.Count-1 do
 begin
  if FIdentifiers.Objects[i] is TRpExpression then
  begin
   eval.AddVariable(FIdentifiers.Strings[i],
    TRpExpression(FIdentifiers.Objects[i]).IdenExpression);
  end
  else
  if FIdentifiers.Objects[i] is TRpChart then
  begin
   eval.AddVariable(FIdentifiers.Strings[i],
    TRpChart(FIdentifiers.Objects[i]).IdenChart);
  end
 end;
 // Insert page number and other variables
 eval.AddVariable('PAGE',fidenpagenum);
 eval.AddVariable('PAGENUM',fidenpagenumgroup);
 // Compatibility with earlier versions
 eval.AddVariable('PAGINA',fidenpagenum);
 eval.AddVariable('NUMPAGINA',fidenpagenumgroup);
 // Free space and sizes
 eval.AddVariable('FREE_SPACE_TWIPS',fidenfreespace);
 eval.AddVariable('PAGEWIDTH',fidenpagewidth);
 eval.AddVariable('PAGEHEIGHT',fidenpageheight);
 eval.AddVariable('CURRENTGROUP',fidencurrentgroup);
 eval.AddVariable('FIRSTSECTION',fidenfirstsection);
 eval.AddVariable('FREE_SPACE_CMS',fidenfreespacecms);
 eval.AddVariable('FREE_SPACE_INCH',fidenfreespaceinch);
 eval.AddIden('EOF',fideneof);
end;

procedure TRpReport.BeginPrint(Driver:IRpPrintDriver);
var
 i:integer;
 item:TRpAliaslistItem;
 apagesize:TPoint;
 paramname:string;
 rPageSizeQt:TPageSizeQt;
 subrep:TRpSubReport;
 dataavail:Boolean;
 index:integer;
begin
 FillGlobalHeaders;
 FDriver:=Driver;
 FPendingSections.Clear;
 if Not Assigned(FDriver) then
  Raise Exception.Create(SRpNoDriverPassedToPrint);
 Driver.SelectPrinter(PrinterSelect);
 if FCompose then
 begin
  printingonepass:=false;
  FInternalPageWidth:=metafile.CustomX;
  FInternalPageHeight:=metafile.CustomY;
  PageNum:=metafile.PageCount-1;
 end
 else
 begin
  metafile.Clear;
  ClearTotalPagesList;
  // Sets page orientation
  if PageOrientation<>rpOrientationDefault then
  begin
   FDriver.SetOrientation(PageOrientation);
  end;
  if PageSize<>rpPageSizeDefault then
  begin
   if PageSize=rpPageSizeUser then
   begin
     metafile.PageSize:=-1;
    rPageSizeQt.Indexqt:=PageSizeQt;
    rPageSizeQt.Custom:=true;
    rPageSizeQt.CustomWidth:=FCustomPageWidth;
    rPageSizeQt.Customheight:=FCustomPageHeight;
   end
   else
   begin
    metafile.PageSize:=PageSizeQt;
    rPageSizeQt.Indexqt:=PageSizeQt;
    rPageSizeQt.Custom:=false;
    rPageSizeQt.CustomWidth:=FCustomPageWidth;
    rPageSizeQt.Customheight:=FCustomPageHeight;
   end;
   apagesize:=Driver.SetPagesize(rPageSizeQt);
  end
  else
  begin
   apagesize:=Driver.GetPageSize(metafile.PageSize);
  end;
  FInternalPageWidth:=apagesize.X;
  FInternalPageHeight:=apagesize.Y;
  metafile.Orientation:=FPageOrientation;
  metafile.BackColor:=FPageBackColor;
  metafile.CustomX:=FInternalPageWidth;
  metafile.CustomY:=FInternalPageHeight;
  PageNum:=-1;
 end;
 metafile.PrinterSelect:=PrinterSelect;
 metafile.PreviewStyle:=PreviewStyle;
 metafile.PreviewWindow:=PreviewWindow;
 metafile.OpenDrawerBefore:=rpDrawerBefore in FReportAction;
 metafile.OpenDrawerAfter:=rpDrawerAfter in FReportAction;
 for i:=0 to SubReports.Count-1 do
 begin
  Subreports.Items[i].Subreport.LastRecord:=false;
 end;
 // Get the time
{$IFDEF MSWINDOWS}
 mmfirst:=TimeGetTime;
{$ENDIF}
{$IFDEF LINUX}
 milifirst:=now;
{$ENDIF}
 LastPage:=false;
 EndPrint;
 // Evaluator
 if Assigned(FEvaluator) then
 begin
  FEvaluator.free;
  FEvaluator:=nil;
 end;
 FEvaluator:=TRpEvaluator.Create(nil);
 FEvaluator.Language:=Language;
 FEvaluator.OnGraphicOp:=OnGraphicOp;
 FEvaluator.OnTextOp:=OnTextOp;
 PageNumGroup:=-1;
 FRecordCount:=0;

 // Add the report items to the evaluator
 AddReportItemsToEvaluator(FEvaluator);
 // Maybe parameters are used in ActivateDatasets (BDESetRange)

 // Evaluates parameter expressions before open
 for i:=0 to Params.Count-1 do
 begin
  if params.items[i].ParamType=rpParamExpreB then
  begin
   paramname:=params.items[i].Name;
   try
    if Not VarIsNull(params.items[i].Value) then
    FEvaluator.EvaluateText(paramname+':=('+String(params.items[i].Value)+')')
   except
    on E:Exception do
    begin
     E.Message:=E.Message+SRpParameter+'-'+paramname;
     Raise;
    end;
   end;
  end;
 end;


 ActivateDatasets;
 try
  // After activating dataset we must check wich subreport to activate
  // if printonly if data avaliable report option is check
  CheckIfDataAvailable;
 except
  DeActivateDatasets;
  Raise;
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

 // Evaluates parameter expressions after open
 for i:=0 to Params.Count-1 do
 begin
  if params.items[i].ParamType=rpParamExpreA then
  begin
   paramname:=params.items[i].Name;
   try
    if Not VarIsNull(params.items[i].Value) then
    FEvaluator.EvaluateText(paramname+':=('+String(params.items[i].Value)+')')
   except
    on E:Exception do
    begin
     E.Message:=E.Message+SRpParameter+'-'+paramname;
     Raise;
    end;
   end;
  end;
 end;

 // Sends the message report header to all components

 for i:=0 to SubReports.Count-1 do
 begin
  Subreports.Items[i].Subreport.SubReportChanged(rpReportStart);
 end;

 CurrentSubReportIndex:=-1;
 dataavail:=False;
 repeat
  inc(CurrentSubReportIndex);
  if CurrentSubReportIndex>=Subreports.count then
   break;
  subrep:=subreports.Items[CurrentSubReportIndex].SubReport;
  if Not Assigned(subrep.ParentSubReport) then
  begin
   if (Length(subrep.Alias)<1) then
    dataavail:=true
   else
   begin
    if subrep.PrintOnlyIfDataAvailable then
    begin
     index:=DataInfo.IndexOf(subrep.Alias);
     if Datainfo.Items[index].Cached then
     begin
      if (Not Datainfo.Items[index].CachedDataset.Eof) then
      begin
       dataavail:=true;
      end;
     end
     else
      begin
      if (Not Datainfo.Items[index].Dataset.Eof) then
      begin
       dataavail:=true;
      end;
     end;
    end
    else
    begin
     dataavail:=true;
    end;
   end;
  end;
  if dataavail then
  begin
   subrep.SubReportChanged(rpSubReportStart);
   subrep.SubReportChanged(rpDataChange);
   CurrentSectionIndex:=-1;
   Subrep.CurrentGroupIndex:=-subrep.GroupCount;
   if subrep.CurrentGroupIndex<0 then
   begin
    CurrentSectionIndex:=subrep.FirstDetail+subrep.CurrentGroupIndex-1;
   end;
   section:=nil;
   subreport:=nil;
   if Not NextSection(true) then
    dataavail:=false;
  end;
 until dataavail;
 if not dataavail then
 begin
  EndPrint;
  Raise Exception.Create(SRpNoDataAvailableToPrint);
 end;

 printing:=True;
end;

// Resturns true if is the last pabe
function TRpReport.PrintNextPage:boolean;
var
 sectionext:TPoint;
 pagefooters:TStringList;
 asection:TrpSection;
 pagefooterpos:integer;
 havepagefooters:boolean;
 oldsubreport:TRpSubreport;
 oldprintedsection:TRpSection;
 oldprintedsectionext:TPoint;
 i,oldhorzdespposition:integer;
 pagespacex:integer;
 sectionextevaluated:boolean;
 PartialPrint:Boolean;
 MaxExtent:TPoint;


procedure SkipToPageAndPosition;
var
 newpage:integer;
 newposx,newposy:integer;
 moveh,movev:boolean;
begin
 newposx:=0;
 newposy:=0;
 try
  // Go to page?
  if Length(asection.SkipToPageExpre)>0 then
  begin
   if not TwoPass then
    Raise Exception.Create(SRpSTwoPassReportNeeded);
   newpage:=Evaluator.EvaluateText(asection.SkipToPageExpre);
   newpage:=newpage-1;
   if newpage<0 then
    Raise Exception.Create(LoadStr(930));
   while newpage>Metafile.PageCount do
   begin
    Metafile.NewPage;
    CheckProgress;
   end;
   Metafile.CurrentPage:=newpage;
  end;
 except
  on E:Exception do
  begin
   Raise TRpReportException.Create(E.Message,asection,SRPSkipPage);
  end;
 end;
 moveh:=false;
 try
  // Go to page?
  if Length(asection.SkipExpreH)>0 then
  begin
   newposx:=Evaluator.EvaluateText(asection.SkipExpreH);
   moveh:=true;
  end;
 except
  on E:Exception do
  begin
   Raise TRpReportException.Create(E.Message,asection,SRpSHSkipExpre);
  end;
 end;
 movev:=false;
 try
  // Go to page?
  if Length(asection.SkipExpreV)>0 then
  begin
   newposy:=Evaluator.EvaluateText(asection.SkipExpreV);
   movev:=true;
  end;
 except
  on E:Exception do
  begin
   Raise TRpReportException.Create(E.Message,asection,SRpSVSkipExpre);
  end;
 end;
 if moveh then
 begin
  if asection.SkipRelativeH then
   pageposx:=pageposx+newposx
  else
//   pageposx:=FLeftMargin+newposx;
   pageposx:=newposx;
 end;
 if movev then
 begin
  if asection.SkipRelativeV then
   pageposy:=pageposy+newposy
  else
//   pageposy:=FTopMargin+newposy;
   pageposy:=newposy;
 end;
// freespace:=FInternalPageheight-pageposy;
 freespace:=pagefooterpos-pageposy;
 if freespace<0 then
  freespace:=0;
end;

function CheckSpace:boolean;
var
 MaxExtent:TPoint;
begin
 if freespace=0 then
 begin
  Result:=false;
  exit;
 end;
 MaxExtent.x:=pagespacex;
 MaxExtent.y:=freespace;
 if not sectionextevaluated then
 begin
  // Skip to page and position
  if asection.SkipType=secskipbefore then
   SkipToPageAndPosition;
  sectionext:=asection.GetExtension(FDriver,MaxExtent);
 end;
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

procedure PrintSection(adriver:IRpPrintDriver;datasection:boolean;var PartialPrint:Boolean);
var
 MaxExtent:TPoint;
begin
 PartialPrint:=False;
 MaxExtent.x:=pagespacex;
 MaxExtent.y:=freespace;

 if datasection then
 begin
  printedsomething:=true;
  oldprintedsection:=section;
  oldprintedsectionext:=sectionext;
 end;
 // If the section is not aligned at bottom of the page then
 if Not asection.AlignBottom then
 begin
  asection.Print(adriver,pageposx,pageposy,metafile,MaxExtent,PartialPrint);
  freespace:=freespace-sectionext.Y;
  pageposy:=pageposy+sectionext.Y;
 end
 else
 // Align to bottom
 begin
  pageposy:=pageposy+freespace-sectionext.Y;
  asection.Print(adriver,pageposx,pageposy,metafile,MaxExtent,PartialPrint);
  freespace:=0;
 end;
 if asection.SkipType=secskipafter then
  SkipToPageAndPosition;
end;

procedure PrintFixedSections(adriver:IRpPrintDriver;headers:boolean);
var
 pheader,pfooter:integer;
 pheadercount,pfootercount:integer;
 i:integer;
 psection:TRpSection;
 afirstdetail:integer;
 printit:boolean;
 MaxExtent:TPoint;
 PartialPrint:Boolean;
begin
 PartialPrint:=false;
 MaxExtent.x:=pagespacex;
 MaxExtent.y:=freespace;
 if Headers then
 begin
  // First the global headers
  for i:=0 to gheaders.count-1 do
  begin
   psection:=TRpSection(gheaders.Items[i]);
   if psection.EvaluatePrintCondition then
   begin
    asection:=psection;
    CheckSpace;
    PrintSection(adriver,false,PartialPrint);
   end;
  end;

  // Print the header fixed sections
  pheader:=subreport.FirstPageHeader;
  pheadercount:=subreport.PageHeaderCount;
  for i:=0 to pheadercount-1 do
  begin
   psection:=subreport.Sections.Items[i+pheader].Section;
   if Not psection.Global then
   begin
    if psection.EvaluatePrintCondition then
    begin
     asection:=psection;
     CheckSpace;
     PrintSection(adriver,false,PartialPrint);
    end;
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
      PrintSection(adriver,false,PartialPrint);
     end;
    end;
   end;
  end;

  pagefooterpos:=pageposy+freespace;
  // Reserve space for page footers
  // Print conditions for footers are evaluated at the begining of
  // the page

  // Global page footers
  for i:=0 to gfooters.count-1 do
  begin
   psection:=TRpSection(gfooters.Items[i]);
   asection:=psection;
   CheckSpace;
   pagefooterpos:=pageposy+freespace-sectionext.Y;
   freespace:=freespace-sectionext.Y;
  end;

  pfooter:=subreport.FirstPageFooter;
  pfootercount:=subreport.PageFooterCount;
  for i:=0 to pfootercount-1 do
  begin
   psection:=subreport.Sections.Items[i+pfooter].Section;
   if Not psection.Global then
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
  if ((gfooters.Count>0) or (pagefooters.Count>0)) then
  begin
   pageposy:=pagefooterpos;
   for i:=0 to pagefooters.Count-1 do
   begin
    asection:=oldsubreport.Sections.Items[StrToInt(pagefooters.Strings[i])].Section;
    if not asection.global then
    begin
     printit:=true;
     if Not asection.FooterAtReportEnd then
     begin
      if Not Assigned(Section) then
       printit:=false;
     end;
     sectionext:=asection.GetExtension(FDriver,MaxExtent);
     if printit then
      if asection.EvaluatePrintCondition then
       PrintSection(adriver,false,PartialPrint);
    end;
   end;
   // Global page footers
   for i:=0 to gfooters.count-1 do
   begin
    asection:=TRpSection(gfooters.Items[i]);
    printit:=true;
    if Not asection.FooterAtReportEnd then
    begin
     if Not Assigned(Section) then
      printit:=false;
    end;
    sectionext:=asection.GetExtension(FDriver,MaxExtent);
    if printit then
     if asection.EvaluatePrintCondition then
      PrintSection(adriver,false,PartialPrint);
   end;
  end;
 end;
end;


begin
 printedsomething:=false;
 if Not Assigned(Section) then
  Raise Exception.Create(SRpLastPageReached);
 if assigned(subreport) then
 begin
  for i:=0 to SubReports.Count-1 do
  begin
   subreports.Items[i].Subreport.SubReportChanged(rpPageChange);
  end;
 end;
 havepagefooters:=false;
 sectionextevaluated:=false;
 oldprintedsection:=nil;
 inc(Pagenum);
 if ininumpage then
 begin
  Pagenumgroup:=-1;
  ininumpage:=false;
 end;
 inc(Pagenumgroup);
 if Not FCompose then
 begin
  freespace:=FInternalPageheight;
  freespace:=freespace-FTopMargin-FBottomMargin;
  pageposy:=FTopMargin;
  pageposx:=FLeftMargin;
 end
 else
 begin
  if (gheaders.count>0) or (subreport.PageHeaderCount>0) then
  begin
   freespace:=FInternalPageheight;
   freespace:=freespace-FTopMargin-FBottomMargin;
   pageposy:=FTopMargin;
   pageposx:=FLeftMargin;
  end
  else
  begin
   printedsomething:=true;
   dec(Pagenum);
  end;
  FCompose:=false;
 end;

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

 pagespacex:=FInternalPageWidth;
 oldhorzdespposition:=FLeftMargin;


 pagefooters:=TStringList.Create;
 try
  // Fills the page with fixed sections
  PrintFixedSections(FDriver,true);
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
      MaxExtent.x:=pagespacex;
      MaxExtent.y:=freespace;
      sectionext:=section.GetExtension(FDriver,MaxExtent);
      sectionextevaluated:=true;
      if (pageposx+oldprintedsectionext.X+sectionext.X)<=pagespacex then
      begin
       pageposx:=pageposx+oldprintedsectionext.X;
       pageposy:=pageposy-oldprintedsectionext.Y;
       freespace:=freespace+oldprintedsectionext.Y;
      end
      else
       pageposx:=FLeftMargin;
     end
     else
     begin
      pageposx:=oldhorzdespposition;
     end;
    end
    else
     oldhorzdespposition:=pageposx;
   end;
   if Not CheckSpace then
    break;
   PartialPrint:=False;
   PrintSection(FDriver,true,PartialPrint);
   if Not PartialPrint then
    NextSection(true);
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
  PrintFixedSections(FDriver,false);
 finally
  pagefooters.Free;
 end;
 Result:=Not Assigned(Section);
 LastPage:=Result;
end;


constructor TIdenEOF.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FParamcount:=1;
 IdenName:='Eof';
// Help:=SRpInt;
 model:='function '+'Eof'+'(alias:string):Boolean';
// aParams:=SRpEof;
end;

function TIdenEof.GeTRpValue:TRpValue;
var
 aliasname:string;
 index:integer;
 dataset:TDataset;
begin
 if (not (VarType(Params[0])=varString)) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 Result:=true;
 aliasname:=String(Params[0]);
 index:=FReport.DataInfo.IndexOf(aliasname);
 if index<0 then
  exit;
 if FReport.DataInfo.Items[index].Cached then
  dataset:=FReport.DataInfo.Items[index].CachedDataset
 else
  dataset:=FReport.DataInfo.Items[index].Dataset;
 if Not dataset.Active then
  exit;
 Result:=dataset.Eof;
end;

procedure TRpReport.Compose(PrevReport:TRpReport;execute:Boolean;ADriver:IRpPrintDriver);
var
 i:integer;
 aobject:TTotalPagesObject;
begin
 if PrevReport.Metafile.PageCount<1 then
  exit;
 ClearTotalPagesList;
 metafile.Assign(PrevReport.Metafile);
 for i:=0 to PrevReport.FTotalPagesList.Count-1 do
 begin
  aobject:=TTotalPagesObject(PrevReport.FTotalPagesList.Items[i]);
  AddTotalPagesItem(aobject.PageIndex,aobject.ObjectIndex,aobject.DisplayFormat);
 end;
 freespace:=PrevReport.freespace;
 pageposy:=PrevReport.pageposy;
 pageposx:=FLeftMargin;
 FCompose:=True;
 TwoPass:=true;
 if execute then
 begin
  PrintAll(ADriver);
 end;
end;


procedure RegisterRpReportClasses;
begin
 Classes.RegisterClass(TRpSection);
 Classes.RegisterClass(TRpReport);
 Classes.RegisterClass(TRpSubReport);
 Classes.RegisterClass(TRpImage);
 Classes.RegisterClass(TRpShape);
 Classes.RegisterClass(TRpLabel);
 Classes.RegisterClass(TRpExpression);
 Classes.RegisterClass(TRpBarcode);
 Classes.RegisterClass(TRpChart);
end;


procedure TRpReport.AlignSectionsTo1_6inchess;
var
 subrep:TRpSubreport;
 sec:TRpSection;
 i,j:integer;
begin
 for i:=0 to SubReports.Count-1 do
 begin
  subrep:=Subreports.Items[i].Subreport;
  for j:=0 to subrep.Sections.Count-1 do
  begin
   sec:=subrep.Sections.Items[j].Section;
   sec.Height:=Round((TWIPS_PER_INCHESS/6)*Round(sec.Height/(TWIPS_PER_INCHESS/6)));
  end;
 end;
end;

function TRpReport.OnGraphicOp(Top,Left,Width,Height:integer;
    DrawStyle:integer;BrushStyle:integer;BrushColor:integer;
    PenStyle:integer;PenWidth:integer; PenColor:integer):Boolean;
begin
 Result:=true;
 metafile.Pages[metafile.CurrentPage].NewDrawObject(Top,Left,Width,Height,
  DrawStyle,BrushStyle,BrushColor,PenStyle,PenWidth,PenColor);
end;

function TRpReport.OnTextOp(Top,Left,Width,Height:integer;
    Text,LFontName,WFontName:WideString;
    FontSize,FontRotation,FontStyle,FontColor,Type1Font:integer;
    CutText:boolean;Alignment:integer;WordWrap,RightToLeft:Boolean;
    PrintStep,BackColor:integer;transparent:boolean):Boolean;
var
 textr:TRpTextObject;
begin
 Result:=true;
 textr.Text:=Text;
 textr.LFontName:=LFontName;
 textr.WFontName:=WFontName;
 textr.FontSize:=FontSize;
 textr.FontRotation:=FontRotation;
 textr.FontStyle:=FontStyle;
 textr.FontColor:=FontColor;
 textr.Type1Font:=Type1Font;
 textr.CutText:=CutText;
 textr.Alignment:=Alignment;
 textr.WordWrap:=WordWrap;
 textr.RightToLeft:=RightToLeft;
 textr.PrintStep:=TRpSelectFontStep(PrintStep);
 metafile.Pages[metafile.CurrentPage].NewTextObject(Top,Left,Width,Height,
  textr,BackColor,transparent);
end;

initialization
 // Need clas registration to be streamable
 RegisterRpReportClasses;
end.
