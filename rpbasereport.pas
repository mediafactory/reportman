{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       The report base component, it contains          }
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

unit rpbasereport;

interface

{$I rpconf.inc}

uses Classes,sysutils,rptypes,rpsubreport,rpsection,rpmdconsts,
 rpdatainfo,rpparams,rpeval,rptypeval,rpprintitem,rpmdbarcode,
 rpmetafile,
{$IFDEF USEVARIANTS}
 types,dateutils,Variants,
{$ENDIF}
 rpalias,db,
{$IFDEF USEZLIB}
 rpmzlib,
{$ENDIF}
{$IFDEF USERPDATASET}
 rpdataset,
{$ENDIF}
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
 TRpBaseReport=class;
 TRpSubReportListItem=class;
 TRpProgressEvent=procedure (Sender:TRpBaseReport;var docancel:boolean) of object;
 TRpSubReportList=class(TCollection)
  private
   FReport:TRpBaseReport;
   function GetItem(Index:Integer):TRpSubReportListItem;
   procedure SetItem(index:integer;Value:TRpSubReportListItem);
  public
   constructor Create(rp:TRpBaseReport);
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
   FReport:TRpBaseReport;
  protected
   function GetRpValue:TRpValue;override;
  public
   varname:string;
  end;

 TIdenEOF=class(TIdenFunction)
  private
   FReport:TRpBaseReport;
  protected
   function GetRpValue:TRpValue;override;
  public
   constructor Create(AOwner:TComponent);override;
  end;

 TRpBaseReport=class(TComponent)
  private
   FSubReports:TRpSubReportList;
   FPageOrientation:TRpOrientation;
   FPagesize:TRpPagesize;
   FPageSizeQt:integer;
   FPageWidth:TRpTwips;
   FPageHeight:TRpTwips;
   FCustomPageWidth:TRpTwips;
   FCustomPageHeight:TRpTwips;
   FPageBackColor:TRpColor;
   FPreviewStyle:TRpPreviewStyle;
   FPreviewWindow:TRpPreviewWindowStyle;
   FPreviewMargins:Boolean;
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
   FMetafile:TRpMetafileReport;
   FOnProgress:TRpProgressEvent;
   FLeftMargin,FTopMargin,FRightMargin,FBottomMargin:TRpTwips;
   FCopies:integer;
   FCollateCopies:boolean;
   FTwoPass:boolean;
   FMilisProgres:integer;
   FPrinterFonts:TRpPrinterFontsOption;
   difmilis:int64;
   FPrinterSelect:TRpPrinterSelect;
   FPrintOnlyIfDataAvailable:Boolean;
   FStreamFormat:TRpStreamFormat;
   FReportAction:TRpReportActions;
   FPreviewAbout:Boolean;
   // Default font properties
   FWFontName:widestring;
   FLFontName:widestring;
   FFontSize:smallint;
   FFontRotation:smallint;
   FFontStyle:integer;
   FFontColor:integer;
   FBackColor:integer;
   FTransparent:Boolean;
   FCutText:Boolean;
   FWordWrap:Boolean;
   FAlignMent:integer;
   FVAlignMent:integer;
   FSingleLine:boolean;
   FType1Font:TRpType1Font;
   FBidiModes:TStrings;
   FMultiPage:Boolean;
   FPrintStep:TRpSelectFontStep;
   FPaperSource:Integer;
   FDuplex:Integer;
   FForcePaperName:String;
   procedure FInternalOnReadError(Reader: TReader; const Message: string;
    var Handled: Boolean);
   procedure SetSubReports(Value:TRpSubReportList);
   procedure SetDataInfo(Value:TRpDataInfoList);
   procedure SetDatabaseInfo(Value:TRpDatabaseInfoList);
   procedure SetParams(Value:TRpParamList);
   procedure SetGridWidth(Value:TRpTwips);
   procedure SetGridHeight(Value:TRpTwips);
   procedure ReadWFontName(Reader:TReader);
   procedure WriteWFontName(Writer:TWriter);
   procedure ReadLFontName(Reader:TReader);
   procedure WriteLFontName(Writer:TWriter);
   procedure SetBidiModes(Value:TStrings);
   function Newlanguage(alanguage:integer):integer;
  protected
    FUpdatePageSize:Boolean;
    currentorientation:TRpOrientation;
    errorprocessing:Boolean;
    lasterrorprocessing:WideString;
    FTotalPagesList:TList;
    FEvaluator:TRpEvaluator;
    FIdentifiers:TStringList;
    FAliasList:TRpAlias;
    FCompose:Boolean;
    FPendingSections:TStringList;
    section:TRpSection;
    subreport:TRpSubreport;
    FRecordCount:integer;
    FDriver:IRpPrintDriver;
    // Identifiers
    Fidenpagenum:TIdenReportVar;
    Fidenlanguage:TIdenReportVar;
    Fidenpagenumgroup:TIdenReportVar;
    FidenEof:TIdenEof;
    Fidenfreespace:TIdenReportVar;
    Fidenpagewidth:TIdenReportVar;
    Fidenpageheight:TIdenReportVar;
    Fidenfreespacecms:TIdenReportVar;
    Fidenfreespaceinch:TIdenReportVar;
    Fidencurrentgroup:TIdenReportVar;
    FIdenfirstsection:TIdenReportVar;
    // Other
    FInternalPageWidth:TRpTwips;
    FInternalPageHeight:TRpTwips;
    FDataAlias:TRpAlias;
    pageposy,pageposx:integer;
    freespace:integer;
    printedsomething:Boolean;
    gheaders,gfooters:TList;
    FGroupHeaders:TStringList;
{$IFDEF MSWINDOWS}
   mmfirst,mmlast:DWORD;
{$ENDIF}
{$IFDEF LINUX}
   milifirst,mililast:TDatetime;
{$ENDIF}
    rPageSizeQt:TPageSizeQt;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent);override;
    procedure UpdateCachedSources(alias:string);
    procedure CheckProgress(finished:Boolean);
    procedure  FillGlobalHeaders;
    procedure ClearTotalPagesList;
    function OnGraphicOp(Top,Left,Width,Height:integer;
     DrawStyle:integer;BrushStyle:integer;BrushColor:integer;
     PenStyle:integer;PenWidth:integer; PenColor:integer):Boolean;
    function OnPageOp(indexqt:integer;custom:Boolean;
     customwidth,customheight,papersource:integer;
     ForcePaperName:String;duplex:integer):Boolean;
    function OnOrienationOp(orientation:integer):Boolean;
    function OnImageOp(Top,Left,Width,Height:integer;
     DrawStyle,DPIRes:integer;PreviewOnly:Boolean;Image:WideString):Boolean;
    function OnBarcodeOp (Top,Left,Width,Height:integer;
     Expression,DisplayFormat:WideString;BarType,Modul:Integer;Ratio,Rotation:Currency;
     CalcChecksum:Boolean;BrushColor:Integer):Boolean;
    function OnTextOp(Top,Left,Width,Height:integer;
     Text,LFontName,WFontName:WideString;
     FontSize,FontRotation,FontStyle,FontColor,Type1Font:integer;
     CutText:boolean;Alignment:integer;WordWrap,RightToLeft:Boolean;
     PrintStep,BackColor:integer;transparent:boolean):Boolean;
    function OnTextheight(Text,LFontName,WFontName:WideString;
     RectWidth,FontSize,FontStyle,Type1Font:integer;
     PrintStep:integer):integer;
    function ReOpenOp(datasetname:String;sql:Widestring):BOolean;
    procedure CheckIfDataAvailable;
    procedure UpdateParamsBeforeOpen(index:integer;doeval:boolean);
    procedure DefineProperties(Filer:TFiler);override;
  public
   FailIfLoadExternalError:Boolean;
   printing:boolean;
   CurrentSubReportIndex:integer;
   CurrentSectionIndex:integer;
   PageNum:integer;
   PageNumGroup:integer;
   LastPage:Boolean;
   ProgressToStdOut:Boolean;
   procedure InitEvaluator;
   procedure BeginPrint(Driver:IRpPrintDriver);virtual;abstract;
   procedure EndPrint;virtual;abstract;
   function PrintNextPage:boolean;virtual;abstract;
   procedure PrintAll(Driver:IRpPrintDriver);
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
   procedure Compose(PrevReport:TRpBaseReport;execute:Boolean;ADriver:IRpPrintDriver);
   property OnProgress:TRpProgressEvent read FOnProgress write FOnProgress;
   property AliasList:TRpAlias read FAliasList write FAliasList;
   property idenpagenum:TIdenReportVar read fidenpagenum;
   property idenlanguage:TIdenReportVar read fidenlanguage;
   property ideneof:TIdenEof read fideneof;
   property idenfreespace:TIdenReportVar read fidenfreespace;
   property idenfreespacecms:TIdenReportVar read fidenfreespacecms;
   property idenfreespaceinch:TIdenReportVar read fidenfreespaceinch;
   property idencurrentgroup:TIdenReportVar read fidencurrentgroup;
   property MilisProgres:integer read FMilisProgres write FMilisProgres
    default MILIS_PROGRESS_DEFAULT;
   procedure AlignSectionsTo(linesperinch:integer);
   procedure PrepareParamsBeforeOpen;
   procedure AssignDefaultFontTo(aitem:TRpGenTextComponent);
   procedure GetDefaultFontFrom(aitem:TRpGenTextComponent);
   function GetSQLValue(connectionname,sql:String):Variant;
   // Default Font properties
   property WFontName:widestring read FWFontName write FWFontName;
   property LFontName:widestring read FLFontName write FLFontName;
  published
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
   property PreviewMargins:Boolean read FPreviewMargins
    write FPreviewMargins default false;
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
    write FStreamFormat;
   property ReportAction:TRpReportActions read FReportAction write FReportAction;
   property PreviewAbout:Boolean read FPreviewAbout write FPreviewAbout
    default true;
   // Default font props
   property Type1Font:TRpType1Font read FType1Font write FType1Font;
   property FontSize:smallint read FFontSize write FFontSize default 10;
   property FontRotation:smallint read FFontRotation write FFontRotation default 0;
   property FontStyle:integer read FFontStyle write FFontStyle default 0;
   property FontColor:integer read FFontColor write FFontColor default 0;
   property BackColor:integer read FBackColor write FBackColor default $FFFFFF;
   property Transparent:Boolean read FTransparent write FTransparent default true;
   property CutText:Boolean read FCutText write FCutText default false;
   property Alignment:integer read FAlignment write FAlignment default 0;
   property VAlignment:integer read FVAlignment write FVAlignment default 0;
   property WordWrap:Boolean read FWordWrap write FWordWrap default false;
   property SingleLine:boolean read FSingleLine write FSingleLine default false;
   property BidiModes:TStrings read FBidiModes write SetBidiModes;
   property MultiPage:Boolean read FMultiPage write FMultiPage default false;
   property PrintStep:TRpSelectFontStep read FPrintStep write FPrintStep
    default rpselectsize;
   // Paper source
   property PaperSource:Integer read FPaperSource write FPaperSource default 0;
   property Duplex:Integer read FDuplex write FDuplex default 0;
   property ForcePaperName:String read FForcePaperName write FForcePaperName;

 end;


implementation


function TIdenReportVar.GeTRpValue:TRpValue;
var
 subrep:TRpSubReport;
begin
 Result:=Null;
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
     Result:=twipstoinchess(freport.freespace)
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
     end
     else
     if varname='LANGUAGE' then
     begin
      Result:=freport.FLanguage;
     end;
end;

// Constructors and destructors
constructor TRpBaseReport.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FPaperSource:=0;
 FDuplex:=0;
 FPreviewMargins:=false;
 FGroupHeaders:=TStringList.Create;
 FPreviewAbout:=true;
 FStreamFormat:=rpStreamtext;
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
 FPageBackColor:=$FFFFFF;

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
 FIdenLanguage:=TIdenReportVar.Create(nil);
 FIdenLanguage.FReport:=self;
 FIdenLanguage.varname:='LANGUAGE';
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
 // Default font
 FLFontName:='Helvetica';
 FWFontName:='Arial';
 FontSize:=10;
 FontRotation:=0;
 FontStyle:=0;
 FontColor:=0;
 FBackColor:=$FFFFFF;
 FTransparent:=true;
 FCutText:=false;
 FBidiModes:=TStringList.Create;

 //
 InitEvaluator;
end;

procedure  TRpBaseReport.FillGlobalHeaders;
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
    gheaders.Add(subrep.Sections[j+k].Section);
  end;
  j:=subrep.FirstPageFooter;
  for k:=0 to subrep.PageFooterCount-1 do
  begin
   if subrep.Sections[j+k].Section.Global then
    gfooters.Add(subrep.Sections[j+k].Section);
  end;
 end;
end;


procedure TRpBaseReport.SetGridWidth(Value:TRpTwips);
begin
 if Value<CONS_MIN_GRID_WIDTH then
  Value:=CONS_MIN_GRID_WIDTH;
 FGridWidth:=Value;
end;

procedure TRpBaseReport.SetGridHeight(Value:TRpTwips);
begin
 if Value<CONS_MIN_GRID_WIDTH then
  Value:=CONS_MIN_GRID_WIDTH;
 FGridHeight:=Value;
end;


procedure TRpBaseReport.AddTotalPagesItem(apageindex,aobjectindex:integer;
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


procedure TRpBaseReport.ClearTotalPagesList;
var
 i:integer;
begin
 for i:=0 to FTotalPagesList.Count-1 do
 begin
  TObject(FTotalPagesList.Items[i]).Free;
 end;
 FTotalPagesList.Clear;
end;


destructor TRpBaseReport.Destroy;
begin
 FGroupHeaders.free;
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


// Streaming procedures

// GetChildren helps streaming the subreports
procedure TRpBaseReport.GetChildren(Proc: TGetChildProc; Root: TComponent);
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


procedure TRpBaseReport.SaveToStream(Stream:TStream);
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

procedure TRpBaseReport.FreeSubreports;
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


procedure TRpBaseReport.AddSubReport;
var
 it:TRpSubReportListItem;
begin
 it:=SubReports.Add;
 it.FSubReport:=TRpSubreport.Create(Self);
 Generatenewname(it.FSubReport);
 it.FSubReport.CreateNew;
end;

procedure TRpBaseReport.CreateNew;
begin
 // Creates a new default report
 FreeSubreports;
 AddSubReport;
end;

procedure TRpBaseReport.SaveToFile(Filename:string);
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


procedure TRpBaseReport.LoadFromFile(FileName:string);
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


procedure TRpBaseReport.LoadFromStream(Stream:TStream);
var
 reader:TReader;
 memstream,amemstream:TMemoryStream;
 readed:integer;
 buf:array of Byte;
{$IFDEF USEZLIB}
 zlibs:TDeCompressionStream;
{$ENDIF}
 theformat:TRpStreamFormat;
 firstchar:char;
 first:boolean;
begin
 // FreeSubrepots
 FreeSubreports;
 MemStream:=TMemoryStream.Create;
 try
  firstchar:=chr(0);
  first:=false;
  // Copy to memory stream
  SetLength(buf,120000);
  repeat
 {$IFDEF DOTNETD}
   readed:=Stream.Read(buf,120000);
   memstream.Write(buf,readed);
 {$ENDIF}
 {$IFNDEF DOTNETD}
   readed:=Stream.Read(buf[0],120000);
   memstream.Write(buf[0],readed);
 {$ENDIF}
   if ((readed>0) and (not first)) then
   begin
    first:=true;
    firstchar:=char(buf[0]);
   end;
  until readed<120000;
  memstream.Seek(0,soFrombeginning);
  // Looks stream type
  if (memstream.size<1) then
   Raise Exception.Create(SRpStreamFormat);
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
     repeat
      readed:=zlibs.Read(buf[0],120000);
      amemstream.Write(buf[0],readed);
     until readed<120000;
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

procedure TRpBaseReport.FInternalOnReadError(Reader: TReader; const Message: string;
    var Handled: Boolean);
begin
 Handled:=false;
// if Pos('AllText',Message)>0 then
//  Handled:=True;
 if Assigned(FOnReadError) then
  reader.OnError:=FOnReadError
end;

procedure TRpBaseReport.SetSubReports(Value:TRpSubReportList);
begin
 FSubReports.Assign(Value);
end;

procedure TRpBaseReport.SetDataInfo(Value:TRpDataInfoList);
begin
 FDataInfo.Assign(Value);
end;

procedure TRpBaseReport.SetDatabaseInfo(Value:TRpDatabaseInfoList);
begin
 FDatabaseInfo.Assign(Value);
end;

// Report collections

constructor TRpSubReportList.Create(rp:TRpBaseReport);
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

procedure TRpBaseReport.DeleteSubReport(subr:TRpSubReport);
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

procedure TRpBaseReport.SetParams(Value:TRpParamList);
begin
 FParams.Assign(Value);
end;

procedure TRpBaseReport.ActivateDatasets;
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
   FDataInfo.Items[i].SQLOverride:='';
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
    dbalias:=UpperCase(FDataInfo.Items[index].DatabaseAlias);
    index:=DatabaseInfo.IndexOf(dbalias);
    if index<0 then
     Raise Exception.Create(SRpSubreportAliasNotFound+':'+alias);
    dbinfo:=DatabaseInfo.Items[index];
    index:=DataInfo.IndexOf(alias);
    if (Not (dbinfo.Driver in [rpdataibx,rpdatamybase,rpdatazeos])) then
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
     begin
      FDataInfo.Items[i].Cached:=false;
      FDataInfo.Items[i].Dataset:=FAliasList.List.Items[index].dataset;
     end;
    end;
   end;
   CheckProgress(false);
  end;
  for i:=0 to FDataInfo.Count-1 do
  begin
   UpdateParamsBeforeOpen(i,true);
   FDataInfo.Items[i].Connect(DatabaseInfo,Params);
   CheckProgress(false);
  end;
 except
  for i:=0 to FDataInfo.Count-1 do
  begin
   FDataInfo.Items[i].Disconnect;
  end;
  Raise;
 end;
end;

procedure TRpBaseReport.DeActivateDatasets;
var
 i:integer;
begin
 for i:=0 to FDataInfo.Count-1 do
 begin
  FDataInfo.Items[i].DisConnect;
  if Assigned(FDataInfo.Items[i].CachedDataset) then
   FDataInfo.Items[i].CachedDataset.DoClose;
 end;
 for i:=0 to FDatabaseInfo.Count-1 do
 begin
  FDatabaseInfo.Items[i].DisConnect;
 end;
end;


procedure TRpBaseReport.UpdateCachedSources(alias:string);
var
 i:integer;
begin
 for i:=0 to datainfo.Count-1 do
 begin
  if datainfo.Items[i].DataSource=alias then
  begin
   if datainfo.Items[i].Cached then
   begin
{$IFDEF USERPDATASET}
//    datainfo.Items[i].CachedDataset.DoClose;
    datainfo.Items[i].CachedDataset.DoOpen;
{$ENDIF}
   end
   else
    if Not datainfo.Items[i].Dataset.Active then
     datainfo.Items[i].Connect(databaseinfo,params);
   UpdateCachedSources(datainfo.items[i].alias);
  end;
 end;
end;



procedure TRpBaseReport.CheckProgress(finished:Boolean);
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
   if ((difmilis>FMilisProgres) or finished) then
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



procedure TRpBaseReport.CheckIfDataAvailable;
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
 if (not ( (VarType(Params[0])=varString) or (VarType(Params[0])=varOleStr) )) then
   Raise TRpNamedException.Create(SRpEvalType,
         IdenName);
 Result:=true;
 aliasname:=String(Params[0]);
 index:=FReport.DataInfo.IndexOf(aliasname);
 if index<0 then
  exit;
{$IFDEF USERPDATASET}
 if FReport.DataInfo.Items[index].Cached then
  dataset:=FReport.DataInfo.Items[index].CachedDataset
 else
{$ENDIF}
  dataset:=FReport.DataInfo.Items[index].Dataset;
 if Not dataset.Active then
  exit;
 Result:=dataset.Eof;
end;

procedure TRpBaseReport.Compose(PrevReport:TRpBaseReport;execute:Boolean;ADriver:IRpPrintDriver);
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



procedure TRpBaseReport.AlignSectionsTo(linesperinch:integer);
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
   sec.Height:=Round((TWIPS_PER_INCHESS/linesperinch)*Round(sec.Height/(TWIPS_PER_INCHESS/linesperinch)));
  end;
 end;
end;

function TRpBaseReport.OnPageOp(indexqt:integer;custom:Boolean;
     customwidth,customheight,papersource:integer;
     ForcePaperName:String;duplex:integer):Boolean;
begin
 rpagesizeqt.Indexqt:=indexqt;
 rpagesizeqt.Custom:=custom;
 rPageSizeQt.CustomHeight:=customheight;
 rPageSizeQt.CustomWidth:=customwidth;
 rPageSizeQt.papersource:=papersource;
 SetForcePaperName(rpagesizeqt,forcepapername);
 Result:=true;
 FUpdatePageSize:=true;
end;

function TRpBaseReport.OnOrienationOp(orientation:integer):Boolean;
begin
 if Not orientation in [0..2] then
 begin
  Result:=false
 end
 else
 begin
  currentorientation:=TRpOrientation(orientation);
  Result:=true;
 end;
 FUpdatePageSize:=true;
end;

function TRpBaseReport.OnGraphicOp(Top,Left,Width,Height:integer;
    DrawStyle:integer;BrushStyle:integer;BrushColor:integer;
    PenStyle:integer;PenWidth:integer; PenColor:integer):Boolean;
begin
 Result:=true;
 metafile.Pages[metafile.CurrentPage].NewDrawObject(Top,Left,Width,Height,
  DrawStyle,BrushStyle,BrushColor,PenStyle,PenWidth,PenColor);
end;

function TRpBaseReport.OnImageOp(Top,Left,Width,Height:integer;
     DrawStyle,DPIRes:integer;PreviewOnly:Boolean;Image:WideString):Boolean;
var
 astream:TMemoryStream;
begin
 // Search for the image
 Result:=false;
 astream:=Evaluator.GetStreamFromExpression(image);
 if assigned(astream) then
 begin
  try
   Result:=True;
   metafile.Pages[metafile.CurrentPage].NewImageObject(Top,Left,Width,Height,0,
    DrawStyle,DPIRes,astream,previewonly);
  finally
   astream.free;
  end;
 end;
end;

function TRpBaseReport.OnBarcodeOp (Top,Left,Width,Height:integer;
     Expression,DisplayFormat:WideString;BarType,Modul:Integer;Ratio,Rotation:Currency;
     CalcChecksum:Boolean;BrushColor:Integer):Boolean;
var
 barcode:TRpBarcode;
 FValue:Variant;
 data:string;
begin
 Result:=False;
 barcode:=TRpBarcode.Create(Self);
 try
  barcode.Width:=Width;
  barcode.Height:=Height;
  barcode.Typ:=TRpBarcodeType(BarType);
  barcode.Modul:=Modul;
  barcode.Ratio:=Ratio;
  barcode.Rotation:=Round(Rotation*10);
  barcode.Checksum:=CalcChecksum;
  FValue:=Evaluator.EvaluateText(Expression);
  barcode.CurrentText:=FormatVariant(displayformat,FValue,rpParamUnknown,true);
  try
   data:=barcode.Calculatebarcode;
  except
   on E:Exception do
   begin
    Raise TRpReportException.Create(E.Message+':'+SrpSCalculatingBarcode+' ',barcode,SRpSBarcode);
   end;
  end;
  // Draws Barcode
  barcode.PrintHeight:=Height;
  barcode.BColor:=BrushColor;
  barcode.DoLines(data, Left,Top,metafile);    // draw the barcode
  Result:=true;
 finally
  barcode.Free;
 end;
end;

function TRpBaseReport.OnTextheight(Text,LFontName,WFontName:WideString;
     RectWidth,FontSize,FontStyle,Type1Font:integer;
     PrintStep:integer):integer;
var
 textr:TRpTextObject;
 extent:TPoint;
begin
 textr.Text:=Text;
 textr.LFontName:=LFontName;
 textr.WFontName:=WFontName;
 textr.FontSize:=FontSize;
 textr.FontRotation:=0;
 textr.FontStyle:=FontStyle;
 textr.FontColor:=0;;
 textr.Type1Font:=Type1Font;
 textr.CutText:=false;
 textr.Alignment:=0;
 textr.WordWrap:=true;
 textr.RightToLeft:=false;
 textr.PrintStep:=TRpSelectFontStep(PrintStep);
 extent.Y:=0;
 extent.x:=rectwidth;
 FDriver.TextExtent(textr,extent);
 Result:=extent.Y;
end;

function TRpBaseReport.OnTextOp(Top,Left,Width,Height:integer;
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

// Print all generaties the metafile, it's capable also
// of evaluate the totalpages expression
procedure TRpBaseReport.PrintAll(Driver:IRpPrintDriver);
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


procedure TRpBaseReport.UpdateParamsBeforeOpen(index:integer;doeval:boolean);
var
 i:integer;
 paramname:string;
begin
 for i:=0 to Params.Count-1 do
 begin
  if Params.Items[i].Datasets.IndexOf(datainfo.Items[index].Alias)>=0 then
  if params.items[i].ParamType=rpParamExpreB then
  begin
   paramname:=params.items[i].Name;
   try
    if Not VarIsNull(params.items[i].Value) then
    begin
     if doeval then
     begin
      FEvaluator.EvaluateText(paramname+':=('+String(params.items[i].Value)+')');
      params.items[i].LastValue:=FEvaluator.EvaluateText(paramname);
     end
     else
     begin
      params.items[i].LastValue:=FEvaluator.EvaluateText(String(params.items[i].Value));
     end;
    end;
   except
    on E:Exception do
    begin
{$IFDEF DOTNETD}
     Raise Exception.Create(E.Message+SRpParameter+'-'+paramname);
{$ENDIF}
{$IFNDEF DOTNETD}
     E.Message:=E.Message+SRpParameter+'-'+paramname;
     Raise;
{$ENDIF}
    end;
   end;
  end;
end;
end;

procedure TRpBaseReport.PrepareParamsBeforeOpen;
var
 i:integer;
begin
 for i:=0 to DataInfo.Count-1 do
  UpdateParamsBeforeOpen(i,false);
end;

procedure TRpBaseReport.InitEvaluator;
begin
 if Assigned(FEvaluator) then
 begin
  FEvaluator.free;
  FEvaluator:=nil;
 end;
 FEvaluator:=TRpEvaluator.Create(nil);
 FEvaluator.Language:=Language;
 FEvaluator.OnNewLanguage:=Newlanguage;
 FEvaluator.OnGraphicOp:=OnGraphicOp;
 FEvaluator.OnOrientationOp:=OnOrienationOp;
 FEvaluator.OnpageOp:=OnPageOp;
 FEvaluator.OnImageOp:=OnImageOp;
 FEvaluator.OnBarcodeOp:=OnBarcodeOp;
 FEvaluator.OnTextOp:=OnTextOp;
 FEvaluator.OnTextHeight:=OnTextHeight;
 FEvaluator.OnReOpenOp:=ReOpenOp;
 FEvaluator.OnGetSQLValue:=GetSQLValue;
end;

procedure TRpBaseReport.SetBidiModes(Value:TStrings);
begin
 FBidiModes.Assign(Value);
end;


procedure TRpBaseReport.WriteWFontName(Writer:TWriter);
begin
 WriteWideString(Writer, FWFontName);
end;

procedure TRpBaseReport.WriteLFontName(Writer:TWriter);
begin
 WriteWideString(Writer, FLFontName);
end;



procedure TRpBaseReport.ReadLFontName(Reader:TReader);
begin
 FLFontName:=ReadWideString(Reader);
end;

procedure TRpBaseReport.ReadWFontName(Reader:TReader);
begin
 FWFontName:=ReadWideString(Reader);
end;

procedure TRpBaseReport.DefineProperties(Filer:TFiler);
begin
 inherited;

 Filer.DefineProperty('WFontName',ReadWFontName,WriteWFontName,True);
 Filer.DefineProperty('LFontName',ReadLFontName,WriteLFontName,True);
end;

procedure TRpBaseReport.AssignDefaultFontTo(aitem:TRpGenTextComponent);
begin
 aitem.Type1Font:=Type1Font;
 aitem.FontSize:=FontSize;
 aitem.FontStyle:=FontStyle;
 aitem.FontRotation:=FontRotation;
 aitem.FontColor:=FontColor;
 aitem.BackColor:=BackColor;
 aitem.Transparent:=Transparent;
 aitem.CutText:=CutText;
 aitem.Alignment:=Alignment;
 aitem.VAlignment:=VAlignment;
 aitem.Wordwrap:=Wordwrap;
 aitem.SingleLine:=SingleLine;
 aitem.BidiModes:=BidiModes;
 aitem.Multipage:=Multipage;
 aitem.PrintStep:=PrintStep;
 aitem.LFontName:=LFontName;
 aitem.WFontName:=WFontName;
end;

procedure TRpBaseReport.GetDefaultFontFrom(aitem:TRpGenTextComponent);
begin
 Type1Font:=aitem.Type1Font;
 FontSize:=aitem.FontSize;
 FontStyle:=aitem.FontStyle;
 FontRotation:=aitem.FontRotation;
 FontColor:=aitem.FontColor;
 BackColor:=aitem.BackColor;
 Transparent:=aitem.Transparent;
 CutText:=aitem.CutText;
 Alignment:=aitem.Alignment;
 VAlignment:=aitem.VAlignment;
 Wordwrap:=aitem.Wordwrap;
 SingleLine:=aitem.SingleLine;
 BidiModes:=aitem.BidiModes;
 Multipage:=aitem.Multipage;
 PrintStep:=aitem.PrintStep;
 LFontName:=aitem.LFontName;
 WFontName:=aitem.WFontName;
end;

function TRpBaseReport.GetSQLValue(connectionname,sql:String):Variant;
var
 adataset:TDataset;
begin
 Result:=Null;
 adataset:=databaseinfo.ItemByName(connectionname).OpenDatasetFromSQL(sql,nil,false);
 if Not adataset.Eof then
 begin
  Result:=adataset.Fields[0].AsVariant;
 end;
end;

function TRpBaseReport.ReOpenOp(datasetname:String;sql:Widestring):BOolean;
var
 adata:TRpDatainfoItem;
 index,i:integer;
begin
 Result:=false;
 index:=datainfo.IndexOf(datasetname);
 if index<0 then
  exit;
 adata:=datainfo.Items[index];
 adata.Disconnect;
 // Evaluates from evaluator all parameters
 for i:=0 to params.Count-1 do
 begin
  params.Items[i].LastValue:=evaluator.EvaluateText('M.'+params.Items[i].Name);
 end;
 adata.SQLOverride:=sql;
 adata.Connect(DatabaseInfo,Params);
end;

function TRpBaseReport.Newlanguage(alanguage:integer):integer;
begin
 Result:=FLanguage;
 FLanguage:=aLanguage;
 if Assigned(FEvaluator) then
  FEvaluator.Language:=FLanguage;
end;



end.
