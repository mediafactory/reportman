{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpmetafile                                      }
{       TRpMetafileReport: A Metafile report            }
{       is a collection of pages that can be printed    }
{       using a printer driver interface                }
{                                                       }
{       Copyright (c) 1994-2003 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

{
 The Report Metafile is a file that contains all
 the info to print a complete report.
 File Format Binary

 Signature='RPMETAFILE04'
 rpFHeader=integer(0);
 PageSize=integer;
 CustomX=integer;
 CustomY=integer;
 Orientation=integer;
 BackColor=integer;

 rpFPage=integer(1);
 ...
 rpFObject or rpFPage

}
unit rpmetafile;

interface

{$I rpconf.inc}

uses Classes,
{$IFDEF LINUX}
  Libc,DateUtils,
{$ENDIF}
{$IFDEF MSWINDOWS}
  mmsystem,windows,
 {$IFNDEF DOTNETD}
  rpcompilerep,
 {$ENDIF}
{$ENDIF}
{$IFDEF USEVARIANTS}
 types,
{$ENDIF}
 Sysutils,rpmdconsts,
{$IFNDEF FORWEBAX}
 rpmdcharttypes,
{$ENDIF}
{$IFDEF USEZLIB}
 rpmzlib,
{$ENDIF}
 rptypes;

const
 MILIS_PROGRESS=500;
 RP_SIGNATURELENGTH=13;
 // The metafile signature and version
 RpSignature:string='RPMETAFILE09'+chr(0);
 RpSignature2_2:string='RPMETAFILE07'+chr(0);
const
 FIRST_ALLOCATION_OBJECTS=50;
 FIRST_ALLOCATED_WIDESTRING=1000;
type

 ERpBadFileFormat=class(Exception)
  private
   FPosition:integer;
  public
   constructor CreatePos(Msg:String;APosition,Pos2:LongInt);
   property position:LongInt read FPosition;
  end;

 TRpMetafileReport=class;
 TRpMetafilePage=class;

 TTotalPagesObject=class(TObject)
  public
   PageIndex,ObjectIndex:integer;
   DisplayFormat:widestring;
  end;

 TRpMetaObjectType=(rpMetaText,rpMetaDraw,rpMetaImage,rpMetaPolygon,rpMetaExport);

 TRpMetaSeparator=(rpFHeader,rpFPage,rpFObject);

 TRpTextObject=record
  Text:WideString;
  LFontName:WideString;
  WFontName:WideString;
  FontSize:smallint;
  FontRotation:smallint;
  FontStyle:smallint;
  FontColor:Integer;
  Type1Font:smallint;
  CutText:boolean;
  Alignment:integer;
  WordWrap:boolean;
  RightToLeft:Boolean;
  PrintStep:TRpSelectFontStep;
 end;


// This is not a safe type, so
// .net metafiles are not compatible
// need to rework all streaming to
// be compatible or enlarge the file size a lot
// size *4
{$IFNDEF DOTNETDBUGS}
 TRpMetaObject=packed record
  Top,Left,Width,Height:integer;
  case Metatype:TRpMetaObjectType of
   rpMetaText:
    (TextP,TextS:integer;
    LFontNameP,LFontNameS:integer;
    WFontNameP,WFontNameS:integer;
    FontSize:smallint;
    FontRotation:smallint;
    FontStyle:smallint;
    Type1Font:smallint;
    FontColor:integer;
    BackColor:integer;
    Transparent:boolean;
    CutText:boolean;Alignment:integer;WordWrap:boolean;
    RightToLeft:Boolean;PrintStep:TRpSelectFontStep);
   rpMetaDraw:
    (DrawStyle:integer;
    BrushStyle:integer;
    BrushColor:integer;
    PenStyle:integer;
    PenWidth:integer;
    PenColor:integer);
   rpMetaImage:
    (CopyMode:integer;
     DrawImageStyle:integer;
     DPIres:integer;
     PreviewOnly:boolean;
     StreamPos:int64;
     StreamSize:int64);
   rpMetaPolygon:
    (PolyBrushStyle:integer;
     PolyBrushColor:integer;
     PolyPenStyle:integer;
     PolyPenWidth:integer;
     PolyPenColor:integer;
     PolyPointCount:integer;
     PolyStreamPos:int64;
     PolyStreamSize:int64);
   rpMetaExport:
    (TextExpP,TextExpS:integer;
     Line:Integer;
     Position:Integer;
     Size:Integer;
     DoNewLine:Boolean);
 end;
{$ENDIF}
{$IFDEF DOTNETDBUGS}
 TRpMetaObject=packed record
  Top,Left,Width,Height:integer;
//  case Metatype:TRpMetaObjectType of
   Metatype:TRpMetaObjectType;
//   rpMetaText:
    TextP,TextS:integer;
    LFontNameP,LFontNameS:integer;
    WFontNameP,WFontNameS:integer;
    FontSize:smallint;
    FontRotation:smallint;
    FontStyle:smallint;
    Type1Font:smallint;
    FontColor:integer;
    BackColor:integer;
    Transparent:boolean;
    CutText:boolean;Alignment:integer;WordWrap:boolean;
    RightToLeft:Boolean;PrintStep:TRpSelectFontStep;
//   rpMetaDraw:
    DrawStyle:integer;
    BrushStyle:integer;
    BrushColor:integer;
    PenStyle:integer;
    PenWidth:integer;
    PenColor:integer;
//   rpMetaImage:
    CopyMode:integer;
     DrawImageStyle:integer;
     DPIres:integer;
     PreviewOnly:boolean;
     StreamPos:int64;
     StreamSize:int64;
//   rpMetaPolygon:
    PolyBrushStyle:integer;
     PolyBrushColor:integer;
     PolyPenStyle:integer;
     PolyPenWidth:integer;
     PolyPenColor:integer;
     PolyPointCount:integer;
     PolyStreamPos:int64;
     PolyStreamSize:int64;
//   rpMetaExport:
    TextExpP,TextExpS:integer;
     Line:Integer;
     Position:Integer;
     Size:Integer;
     DoNewLine:Boolean;
 end;
{$ENDIF}


 IRpPrintDriver=interface
 ['{11EF15B0-5CDE-40F0-A204-973A25B38B81}']
  procedure NewDocument(report:TrpMetafileReport;hardwarecopies:integer;
   hardwarecollate:boolean);
  procedure EndDocument;
  procedure AbortDocument;
  procedure NewPage(metafilepage:TRpMetafilePage);
  procedure EndPage;
  function GetPageSize(var PageSizeQt:Integer):TPoint;
  function SetPagesize(PagesizeQt:TPageSizeQt):TPoint;
  procedure SetOrientation(Orientation:TRpOrientation);
  procedure DrawObject(page:TRpMetaFilePage;obj:TRpMetaObject);
{$IFNDEF FORWEBAX}
  procedure DrawChart(Series:TRpSeries;ametafile:TRpMetaFileReport;posx,posy:integer;achart:TObject);
{$ENDIF}
  procedure TextExtent(atext:TRpTextObject;var extent:TPoint);
  procedure GraphicExtent(Stream:TMemoryStream;var extent:TPoint;dpi:integer);
  procedure DrawPage(apage:TRpMetaFilePage);
  function SupportsCopies(maxcopies:integer):boolean;
  function SupportsCollation:boolean;
  function AllowCopies:boolean;
  procedure SelectPrinter(printerindex:TRpPrinterSelect);
  function GetFontDriver:IRpPrintDriver;
 end;

{$IFNDEF FORWEBAX}
 TDoDrawChartEvent=procedure (adriver:IRpPrintDriver;Series:TRpSeries;page:TRpMetaFilePage;
  aposx,aposy:integer;achart:TObject) of Object;
{$ENDIF}

 TRpMetafilePage=class(TObject)
  private
   Fversion2_2:Boolean;
   FUpdatedPageSize:Boolean;
   FObjects:array of TRpMetaObject;
   FObjectCount:Integer;
   FPool:Widestring;
   FPoolPos:integer;
   FStreamPos:int64;
   FMemStream:TMemoryStream;
   FIntStream:TMemoryStream;
   FMark:Integer;
   FOrientation:TRpOrientation;
   FPageSizeqt:TPageSizeQt;
   function GetObject(index:integer):TRpMetaObject;
   procedure NewWideString(var position,size:integer;const text:widestring);
  public
   procedure LoadFromStream(Stream:TStream);
   procedure SaveToStream(Stream:TStream);
   procedure DeleteObject(index:integer);
   constructor Create;
   destructor Destroy;override;
   procedure Clear;
   procedure NewTextObject(Top,Left,Width,Height:integer;
    aText:TRpTextObject;BackColor:integer;transparent:boolean);
   procedure NewExportObject(Top,Left,Width,Height:integer;
    aText:WideString;Line,Position,Size:Integer;DoNewLine:boolean);
   procedure NewDrawObject(Top,Left,Width,Height:integer;
    DrawStyle:integer;BrushStyle:integer;BrushColor:integer;
    PenStyle:integer;PenWidth:integer; PenColor:integer);
   procedure NewImageObject(Top,Left,Width,Height:integer;
    CopyMode:integer;DrawImageStyle:integer;DPIres:integer;stream:TStream;PreviewOnly:Boolean);
   function GetText(arecord:TRpMetaObject):widestring;
   function GetWFontName(arecord:TRpMetaObject):widestring;
   function GetLFontName(arecord:TRpMetaObject):widestring;
   function GetStream(arecord:TRpMetaObject):TMemoryStream;
   property Mark:Integer read FMark write FMark;
   property ObjectCount:integer read FObjectCount;
   property Pool:WideString read FPool;
   property Objects[Index:integer]:TRpMetaObject read GetObject;
   property Orientation:TRpOrientation read FOrientation write FOrientation;
   property PageSizeqt:TPageSizeQt read FPageSizeQt write FPageSizeQt;
   property UpdatedPageSize:Boolean read FUpdatedPageSize
    write FUpdatedPageSize default false;
  end;

 TRpMetafileStreamProgres=procedure (Sender:TRpMetafileReport;Position,Size:int64;page:integer) of object;

 TRpMetafileReport=class(TComponent)
  private
   FPages:TList;
   FCurrentPage:integer;
   FOnProgress:TRpMetafileStreamProgres;
{$IFDEF MSWINDOWS}
   mmfirst,mmlast:DWORD;
{$ENDIF}
{$IFDEF LINUX}
   milifirst,mililast:TDatetime;
{$ENDIF}
   difmilis:int64;
   FPreviewAbout:Boolean;
   FPreviewMargins:Boolean;
   Fversion2_2:Boolean;
   procedure SetCurrentPage(index:integer);
   function GetPageCount:integer;
   function GetPage(Index:integer):TRpMetafilePage;
   procedure IntSaveToStream(Stream:TStream;SaveStream:TStream);
   procedure IntLoadFromStream(Stream:TStream;LoadStream:TStream;clearfirst:boolean=true);
  public
   PageSize:integer;
   CustomX:integer;
   CustomY:integer;
   Orientation:TRpOrientation;
   PaperSource:Integer;
   Duplex:Word;
   LinesPerInch:Word;
   BackColor:integer;
   PrinterSelect:TRpPrinterSelect;
   PreviewStyle:TRpPreviewStyle;
   PreviewWindow:TRpPreviewWindowStyle;
   OpenDrawerBefore:Boolean;
   OpenDrawerAfter:Boolean;
{$IFNDEF FORWEBAX}
   OnDrawChart:TDoDrawChartEvent;
{$ENDIF}
   procedure Clear;
   procedure LoadFromStream(Stream:TStream;clearfirst:boolean=true);
   procedure LoadFromFile(filename:string;clearfirst:boolean=true);
   procedure SaveToStream(Stream:TStream);
   procedure SaveToFile(filename:string);
   procedure Assign(Source:TPersistent);override;
   constructor Create(AOwner:TComponent);override;
   destructor Destroy;override;
   procedure NewPage;
   procedure DrawPage(IDriver:IRpPrintDriver);
   procedure DrawAll(IDriver:IRpPrintDriver);
   procedure DrawPageOnly(IDriver:IRpPrintDriver);
   procedure InsertPage(index:integer);
   procedure DeletePage(index:integer);
   property CurrentPage:integer read FCurrentPage write SetCurrentPage;
   property PageCount:integer read GetPageCount;
   procedure UpdateTotalPages(alist:TList);
   procedure UpdateTotalPagesPCount(alist:TList;pcount:integer);
   procedure PageRange(frompage,topage:integer);
   property Pages[Index:integer]:TRpMetafilePage read GetPage;
   property OnProgress:TRpMetafileStreamProgres read FOnProgress write FOnProgress;
   property PreviewAbout:Boolean read FPreviewAbout write FPreviewAbout;
   property PreviewMargins:Boolean read FPreviewMargins write FPreviewMargins;
  published
  end;

  function CalcTextExtent(adriver:IRpPrintDriver;maxextent:TPoint;obj:TRpTextObject):integer;

{$IFDEF MSWINDOWS}
{$IFNDEF DOTNETD}
  procedure  MetafileToExe(metafile:TRpMetafileReport;filename:String);
{$ENDIF}
{$ENDIF}

implementation

constructor TrpMetafilePage.Create;
begin
 inherited Create;
 SetLength(FObjects,FIRST_ALLOCATION_OBJECTS);
 FObjectCount:=0;
 FMark:=0;
 FPoolPos:=1;
 FStreamPos:=0;
 FMemStream:=TMemoryStream.Create;
end;

procedure TRpMetafilePage.Clear;
begin
 SetLength(FObjects,FIRST_ALLOCATION_OBJECTS);
 FObjectCount:=0;
 FMark:=0;
 FPoolPos:=1;
 FStreamPos:=0;
end;

destructor TRpMetafilePage.Destroy;
begin
 FMemStream.Free;
 FMemStream:=nil;

 if Assigned(FIntStream) then
 begin
  FIntStream.Free;
  FIntStream:=nil;
 end;

 inherited Destroy;
end;


procedure TrpMetafilePage.NewImageObject(Top,Left,Width,Height:integer;
 CopyMode:integer; DrawImageStyle:integer;DPIres:integer;stream:TStream;PreviewOnly:boolean);
begin
 if FObjectCount>=High(FObjects)-1 then
 begin
  // Duplicates capacity
  SetLength(FObjects,High(FObjects)*2);
 end;
 FObjects[FObjectCount].Left:=Left;
 FObjects[FObjectCount].Top:=Top;
 FObjects[FObjectCount].Height:=Height;
 FObjects[FObjectCount].Width:=Width;
 FObjects[FObjectCount].CopyMode:=CopyMode;
 FObjects[FObjectCount].DrawImageStyle:=DrawImageStyle;
 FObjects[FObjectCount].DPIres:=DPIres;
 FObjects[FObjectCount].Metatype:=rpMetaImage;
 FObjects[FObjectCount].StreamPos:=FStreamPos;
 FObjects[FObjectCount].StreamSize:=stream.Size;
 FObjects[FObjectCount].PreviewOnly:=PreviewOnly;
 // Set the size of the stream
 if FMemStream.size=0 then
 begin
  FMemStream.SetSize(stream.size*2);
 end
 else
 begin
  if FMemStream.Size-FStreamPos-1<stream.size then
  begin
   if FMemStream.size<stream.size then
   begin
    FMemStream.SetSize(stream.size*2);
   end
   else
    FMemStream.SetSize(FMemStream.Size*2);
  end;
 end;
 Stream.Seek(0,soFromBeginning);
 FMemStream.Seek(FStreamPos,soFromBeginning);
 if (Stream.size<>FMemStream.CopyFrom(stream,stream.Size)) then
  Raise Exception.Create(SRpCopyStreamError);
 FStreamPos:=FMemStream.Position;
 inc(FObjectCount);
end;


function TrpMetafilePage.GetStream(arecord:TRpMetaObject):TMemoryStream;
begin
 if Assigned(FIntStream) then
 begin
  FIntStream.Free;
  FIntStream:=nil;
  FIntStream:=TMemoryStream.Create;
  FIntStream.SetSize(arecord.StreamSize);
 end
 else
 begin
  FIntStream:=TMemoryStream.Create;
 end;
 FMemStream.Seek(arecord.StreamPos,soFromBeginning);
 FIntStream.CopyFrom(FMemStream,arecord.StreamSize);
 FIntStream.Seek(0,soFromBeginning);
 Result:=FIntStream;
end;


procedure TrpMetafilePage.NewDrawObject(Top,Left,Width,Height:integer;
    DrawStyle:integer;BrushStyle:integer;BrushColor:integer;
    PenStyle:integer;PenWidth:integer; PenColor:integer);
begin
 if FObjectCount>=High(FObjects)-1 then
 begin
  // Duplicates capacity
  SetLength(FObjects,High(FObjects)*2);
 end;
 FObjects[FObjectCount].Left:=Left;
 FObjects[FObjectCount].Top:=Top;
 FObjects[FObjectCount].Height:=Height;
 FObjects[FObjectCount].Width:=Width;
 FObjects[FObjectCount].Metatype:=rpMetaDraw;

 FObjects[FObjectCount].DrawStyle:=DrawStyle;
 FObjects[FObjectCount].BrushStyle:=BrushStyle;
 FObjects[FObjectCount].BrushColor:=BrushColor;
 FObjects[FObjectCount].PenColor:=PenColor;
 FObjects[FObjectCount].PenWidth:=PenWidth;
 FObjects[FObjectCount].PenStyle:=PenStyle;

 inc(FObjectCount);
end;

procedure TrpMetafilePage.NewWideString(var position,size:integer;const text:widestring);
begin

 size:=Length(Text);
 FPool:=FPool+Text;
 position:=FPoolPos;
 FPoolPos:=FPoolPos+size;

end;


procedure TrpMetafilePage.NewExportObject(Top,Left,Width,Height:integer;
    aText:WideString;Line,Position,Size:Integer;DoNewLine:boolean);
begin
 if FObjectCount>=High(FObjects)-1 then
 begin
  // Duplicates capacity
  SetLength(FObjects,High(FObjects)*2);
 end;

 FObjects[FObjectCount].Left:=Left;
 FObjects[FObjectCount].Top:=Top;
 FObjects[FObjectCount].Height:=Height;
 FObjects[FObjectCount].Width:=Width;
 FObjects[FObjectCount].Metatype:=rpMetaExport;

 NewWideString(FObjects[FObjectCount].TextExpP,FObjects[FObjectCount].TextExpS,
  aText);
 FObjects[FObjectCount].Line:=Line;
 FObjects[FObjectCount].Position:=Position;
 FObjects[FObjectCount].Size:=Size;
 FObjects[FObjectCount].DoNewLine:=DoNewLine;

 inc(FObjectCount);

end;

procedure TrpMetafilePage.NewTextObject(Top,Left,Width,Height:integer;
    aText:TRpTextObject;BackColor:integer;transparent:boolean);
begin
 if FObjectCount>=High(FObjects)-1 then
 begin
  // Duplicates capacity
  SetLength(FObjects,High(FObjects)*2);
 end;
 FObjects[FObjectCount].Left:=Left;
 FObjects[FObjectCount].Top:=Top;
 FObjects[FObjectCount].Height:=Height;
 FObjects[FObjectCount].Width:=Width;
 FObjects[FObjectCount].Metatype:=rpMetaText;

 NewWideString(FObjects[FObjectCount].TextP,FObjects[FObjectCount].TextS,aText.Text);
 NewWideString(FObjects[FObjectCount].WFontNameP,
  FObjects[FObjectCount].WFontNameS,aText.WFontName);
 NewWideString(FObjects[FObjectCount].LFontNameP,
  FObjects[FObjectCount].LFontNameS,aText.LFontName);
 FObjects[FObjectCount].FontSize:=aText.FontSize;
 FObjects[FObjectCount].FontRotation:=aText.FontRotation;
 FObjects[FObjectCount].FontStyle:=aText.FontStyle;
 FObjects[FObjectCount].Type1Font:=aText.Type1Font;
 FObjects[FObjectCount].FontColor:=aText.FontColor;
 FObjects[FObjectCount].BackColor:=BackColor;
 FObjects[FObjectCount].Transparent:=Transparent;
 FObjects[FObjectCount].CutText:=aText.CutText;
 FObjects[FObjectCount].Alignment:=aText.Alignment;
 FObjects[FObjectCount].WordWrap:=aText.WordWrap;
 FObjects[FObjectCount].RightToLeft:=aText.RightToLeft;
 FObjects[FObjectCount].PrintStep:=aText.PrintStep;


 inc(FObjectCount);

end;

function TrpMetafilePage.GetText(arecord:TRpMetaObject):widestring;
begin
 Result:=Copy(FPool,arecord.TextP,arecord.TextS);
end;

function TrpMetafilePage.GetWFontName(arecord:TRpMetaObject):widestring;
begin
 Result:=Copy(FPool,arecord.WFontNameP,arecord.WFontNameS);
end;

function TrpMetafilePage.GetLFontName(arecord:TRpMetaObject):widestring;
begin
 Result:=Copy(FPool,arecord.LFontNameP,arecord.LFontNameS);
end;

procedure TrpMetafilePage.DeleteObject(index:integer);
var
 i:integer;
begin
 if index<0 then
  Raise Exception.Create(SRpMetaIndexObjectOutofBounds);
 if index>FObjectCount-1 then
  Raise Exception.Create(SRpMetaIndexObjectOutofBounds);
 dec(FObjectCount);
 for i:=index to FObjectCount-1 do
 begin
  FObjects[index]:=FObjects[index+1];
 end;
end;

function TrpMetafilePage.GetObject(index:integer):TrpMetaObject;
begin
 if index<0 then
  Raise Exception.Create(SRpMetaIndexObjectOutofBounds);
 if index>FObjectCount-1 then
  Raise Exception.Create(SRpMetaIndexObjectOutofBounds);
 Result:=FObjects[index];
end;


function TRpMetafileReport.GetPageCount:integer;
begin
 Result:=FPages.count;
end;

procedure TRpMetafileReport.NewPage;
begin
 InsertPage(FPages.Count);
end;

function TRpMetafileReport.GetPage(Index:integer):TRpMetafilePage;
begin
 if index<0 then
  Raise Exception.Create(SRpMetaIndexPageOutofBounds);
 if index>FPages.Count-1 then
  Raise Exception.Create(SRpMetaIndexPageOutofBounds);
 Result:=TRpMetafilePage(FPages.Items[index]);
end;

procedure TRpMetafileReport.SetCurrentPage(index:integer);
begin
 if index<0 then
  Raise Exception.Create(SRpMetaIndexPageOutofBounds);
 if index>FPages.Count-1 then
  Raise Exception.Create(SRpMetaIndexPageOutofBounds);
 FCurrentPage:=index;
end;

procedure TRpMetafileReport.InsertPage(index:integer);
var
 FPage:TRpMetafilePage;
begin
 if index<0 then
  Raise Exception.Create(SRpMetaIndexPageOutofBounds);
 if index>FPages.Count then
  Raise Exception.Create(SRpMetaIndexPageOutofBounds);
 FPage:=TRpMetafilePage.Create;
 if index=FPages.Count then
  FPages.Add(FPage)
 else
  FPages.Insert(index,FPage);
 FCurrentPage:=index;
end;

procedure TRpMetafileReport.DeletePage(index:integer);
begin
 if (index>FPages.count-1) then
  Raise Exception.Create(SRpMetaIndexPageOutofBounds);
 TObject(FPages.items[index]).free;
 FPages.Delete(index);
 if FCurrentPage<FPages.count-1 then
  FCurrentPage:=FPages.count-1;
end;

constructor TRpMetafileReport.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FCurrentPage:=-1;
 // Standard sizes
 CustomX:=12047;
 CustomY:=17039;
 OpenDrawerBefore:=false;
 OpenDrawerAfter:=false;

 FPages:=TList.Create;
end;

procedure TRpMetafileReport.Clear;
var
 i:integer;
begin
 for i:=0 to FPages.Count-1 do
 begin
  TRpMetafilePage(FPages.Items[i]).Clear;
  TRpMetafilePage(Fpages.Items[i]).Free;
 end;
 FPages.clear;


 FCurrentPage:=-1;
end;

destructor TRpMetafileReport.Destroy;
begin
 Clear;

 FPages.free;
 inherited Destroy;
end;




procedure TRpMetafileReport.SaveToStream(Stream:TStream);
{$IFDEF USEZLIB}
var
 zstream:TCompressionStream;
{$ENDIF}
begin
 // Get the time
{$IFDEF MSWINDOWS}
 mmfirst:=TimeGetTime;
{$ENDIF}
{$IFDEF LINUX}
 milifirst:=now;
{$ENDIF}
{$IFDEF USEZLIB}
 zstream:=TCompressionStream.Create(clDefault,Stream);
 try
  IntSaveToStream(zstream,Stream);
 finally
  zstream.free;
 end;
{$ENDIF}
{$IFNDEF USEZLIB}
  IntSaveToStream(Stream,Stream);
{$ENDIF}
end;

procedure TRpMetafileReport.IntSaveToStream(Stream:TStream;SaveStream:TStream);
var
 separator:integer;
 i:integer;
 acount:integer;
 ainteger:integer;
begin
 WriteStringToStream(rpSignature,Stream);
 separator:=integer(rpFHeader);
 Stream.Write(separator,sizeof(separator));
 // Report header
 Stream.Write(PageSize,sizeof(pagesize));
 Stream.Write(CustomX,sizeof(CustomX));
 Stream.Write(CustomY,sizeof(CustomY));
 ainteger:=Integer(Orientation);
 Stream.Write(ainteger,sizeof(integer));
 Stream.Write(BackColor,sizeof(BackColor));
 Stream.Write(PaperSource,sizeof(PaperSource));
 Stream.Write(LinesPerInch,sizeof(LinesPerInch));
 Stream.Write(Duplex,sizeof(Duplex));
 ainteger:=Integer(PrinterSelect);
 Stream.Write(ainteger,sizeof(integer));
 ainteger:=Integer(PreviewStyle);
 Stream.Write(ainteger,sizeof(integer));
 ainteger:=Integer(PreviewWindow);
 Stream.Write(ainteger,sizeof(Integer));
 Stream.Write(OpenDrawerBefore,sizeof(OpenDrawerBefore));
 Stream.Write(OpenDrawerAfter,sizeof(OpenDrawerAfter));
 if FPreviewAbout then
  ainteger:=1;
 Stream.Write(ainteger,sizeof(Integer));
 ainteger:=0;
 if FPreviewMargins then
  ainteger:=1;
 Stream.Write(ainteger,sizeof(Integer));

 // Pages
 // Write pagecount
 acount:=FPages.Count;
 Stream.Write(acount,sizeof(acount));
 for i:=0 to FPages.count-1 do
 begin
  separator:=integer(rpFPage);
  Stream.Write(separator,sizeof(separator));

  TRpMetafilePage(FPages.items[i]).SaveToStream(Stream);
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
    FOnProgress(Self,SaveStream.Position,SaveStream.Position,i+1);
   end;
  end;
 end;
end;

procedure TRpMetafileReport.LoadFromStream(Stream:TStream;clearfirst:boolean=true);
var
{$IFDEF USEZLIB}
 zStream:TDeCompressionStream;
{$ENDIF}
 memstream:TMemoryStream;
begin
 if clearfirst then
  Clear;
 memstream:=TMemoryStream.Create;
 try
  memstream.CopyFrom(Stream,Stream.Size);
  memstream.Seek(0,soFromBeginning);
  // Get the time
{$IFDEF MSWINDOWS}
  mmfirst:=TimeGetTime;
{$ENDIF}
{$IFDEF LINUX}
  milifirst:=now;
{$ENDIF}
  if IsCompressed(memstream) then
  begin
{$IFDEF USEZLIB}
   zStream:=TDeCompressionStream.Create(memStream);
   try
    IntLoadFromStream(zStream,Stream);
   finally
    zStream.free;
   end;
{$ENDIF}
{$IFNDEF USEZLIB}
   Raise Exception.Create(SRpZLibNotSupported);
{$ENDIF}
  end
  else
  begin
   IntLoadFromStream(memstream,memstream);
  end;
 finally
  memstream.free;
 end;
end;

procedure TRpMetafileReport.IntLoadFromStream(Stream:TStream;LoadStream:TStream;clearfirst:boolean=true);
var
 separator:integer;
 buf:array of Byte;
 bufstring:String;
 bytesread:integer;
 fpage:TRpMetafilePage;
 acount:integer;
 i,ainteger:integer;
 apagesizeqt:TPageSizeQt;
begin
 // Clears the report metafile
 if clearfirst then
  Clear;
 FVersion2_2:=false;
 SetLength(buf,RP_SIGNATURELENGTH);
 bytesread:=Stream.Read(buf[0],RP_SIGNATURELENGTH);
 if (bytesread<RP_SIGNATURELENGTH) then
  Raise Exception.Create(SRpBadSignature);
 bufstring:='';
 for i:=0 to RP_SIGNATURELENGTH-1 do
 begin
  bufstring:=bufstring+Char(buf[i]);
 end;
 if (bufstring<>rpSignature) then
 begin
  if bufstring=RpSignature2_2 then
   FVersion2_2:=true
  else
   Raise Exception.Create(SRpBadSignature);
 end;
 if (sizeof(separator)<>Stream.Read(separator,sizeof(separator))) then
  Raise Exception.Create(SRpBadFileHeader);
 if (separator<>integer(rpFHeader)) then
  Raise Exception.Create(SRpBadFileHeader);
 // Report header
 if (sizeof(pagesize)<>Stream.Read(PageSize,sizeof(pagesize))) then
  Raise Exception.Create(SRpBadFileHeader);
 if (sizeof(CustomX)<>Stream.Read(CustomX,sizeof(CustomX))) then
  Raise Exception.Create(SRpBadFileHeader);
 if (sizeof(CustomY)<>Stream.Read(CustomY,sizeof(CustomY))) then
  Raise Exception.Create(SRpBadFileHeader);
 if (sizeof(integer)<>Stream.Read(ainteger,sizeof(Integer))) then
  Raise Exception.Create(SRpBadFileHeader);
 Orientation:=TRpOrientation(ainteger);
 if (sizeof(BackColor)<>Stream.Read(BackColor,sizeof(BackColor))) then
  Raise Exception.Create(SRpBadFileHeader);
 if (sizeof(PaperSource)<>Stream.Read(PaperSource,sizeof(PaperSource))) then
  Raise Exception.Create(SRpBadFileHeader);
 if (sizeof(LinesPerInch)<>Stream.Read(LinesPerInch,sizeof(LinesPerInch))) then
  Raise Exception.Create(SRpBadFileHeader);
 if (LinesPerInch<=0) then
  LinesPerInch:=6;
 if (sizeof(Duplex)<>Stream.Read(Duplex,sizeof(Duplex))) then
  Raise Exception.Create(SRpBadFileHeader);
 if (sizeof(integer)<>Stream.Read(ainteger,sizeof(integer))) then
  Raise Exception.Create(SRpBadFileHeader);
 PrinterSelect:=TRpPrinterSelect(ainteger);
 if (sizeof(integer)<>Stream.Read(ainteger,sizeof(integer))) then
  Raise Exception.Create(SRpBadFileHeader);
 PreviewStyle:=TRpPreviewStyle(ainteger);
 if (sizeof(integer)<>Stream.Read(ainteger,sizeof(integer))) then
  Raise Exception.Create(SRpBadFileHeader);
 PreviewWindow:=TRpPreviewWindowStyle(ainteger);
 if (sizeof(OpenDrawerBefore)<>Stream.Read(OpenDrawerBefore,sizeof(OpenDrawerBefore))) then
  Raise Exception.Create(SRpBadFileHeader);
 if (sizeof(OpenDrawerAfter)<>Stream.Read(OpenDrawerAfter,sizeof(OpenDrawerAfter))) then
  Raise Exception.Create(SRpBadFileHeader);
 if (sizeof(integer)<>Stream.Read(ainteger,sizeof(Integer))) then
  Raise Exception.Create(SRpBadFileHeader);
 FPreviewAbout:=true;
 if ainteger=0 then
  FPreviewAbout:=False;
 if (sizeof(integer)<>Stream.Read(ainteger,sizeof(Integer))) then
  Raise Exception.Create(SRpBadFileHeader);
 FPreviewMargins:=true;
 if ainteger=0 then
  FPreviewMargins:=False;

 // If there is no pages then end of read
 // Read pagecount
 if (sizeof(acount)<>Stream.Read(acount,sizeof(acount))) then
  Raise Exception.Create(SRpBadFileHeader);


 // Pages
 bytesread:=Stream.Read(separator,sizeof(separator));
 while (bytesread>0) do
 begin
  if bytesread<>sizeof(separator) then
   Raise ERpBadFileFormat.CreatePos(SrpMtPageSeparatorExpected,Stream.Position,0);
  if (separator<>integer(rpFPage)) then
   Raise ERpBadFileFormat.CreatePos(SrpMtPageSeparatorExpected,Stream.Position,0);
  // New page and load from stream
  fpage:=TRpMetafilePage.Create;
  FPages.Add(fpage);
  FPage.Fversion2_2:=Fversion2_2;
  FPage.FUpdatedPageSize:=false;
  FPage.FOrientation:=Orientation;
  aPageSizeqt.Indexqt:=PageSize;
  aPageSizeqt.Custom:=true;
  aPageSizeqt.CustomWidth:=CustomX;
  aPageSizeqt.Customheight:=CustomY;
  aPageSizeqt.PhysicWidth:=CustomX;
  aPageSizeqt.PhysicHeight:=CustomY;
  aPageSizeqt.PaperSource:=0;
  aPageSizeqt.ForcePaperName:='';
  aPageSizeqt.Duplex:=0;

  FPage.PageSizeqt:=apagesizeqt;
  fpage.LoadFromStream(Stream);

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
    FOnProgress(Self,LoadStream.Position,LoadStream.Size,pagecount);
   end;
 end;
  bytesread:=Stream.Read(separator,sizeof(separator));
 end;
 if Fpages.Count>0 then
  FCurrentPage:=0;
end;


procedure TRpMetafilePage.SaveToStream(Stream:TStream);
var
 separator:integer;
 asize:int64;
 wsize:integer;
 byteswrite:integer;
 abytes:array of Byte;
begin
 // Objects
 // Save all objects
 separator:=integer(rpFObject);
 Stream.Write(separator,sizeof(separator));
 Stream.Write(FMark,sizeof(FMark));
{$IFDEF DOTNETD}
 Stream.Write(integer(forientation),sizeof(integer(forientation)));
 Stream.Write(fpagesizeqt.Indexqt,sizeof(fpagesizeqt.Indexqt));
 Stream.Write(fpagesizeqt.Custom,sizeof(fpagesizeqt.Custom));
 Stream.Write(fpagesizeqt.CustomWidth,sizeof(fpagesizeqt.CustomWidth));
 Stream.Write(fpagesizeqt.CustomHeight,sizeof(fpagesizeqt.CustomHeight));
 Stream.Write(fpagesizeqt.PhysicWidth,sizeof(fpagesizeqt.PhysicWidth));
 Stream.Write(fpagesizeqt.PhysicHeight,sizeof(fpagesizeqt.PhysicHeight));
 Stream.Write(fpagesizeqt.PaperSource,sizeof(fpagesizeqt.PaperSource));
 Stream.Write(fpagesizeqt.PaperSource,sizeof(fpagesizeqt.PaperSource));
 byteswrite:=61;
 System.Array.Copy(fpagesizeqt.ForcePaperName,abytes,byteswrite);
 if byteswrite<>Stream.Write(abytes[0],byteswrite) then
  Raise Exception.Create(SRpErrorWritingPage);
//  ForcePaperName:array [0..60] of char;
 Stream.Write(fpagesizeqt.Duplex,sizeof(fpagesizeqt.Duplex));
{$ENDIF}
{$IFNDEF DOTNETD}
 Stream.Write(forientation,sizeof(forientation));
 Stream.Write(fpagesizeqt,sizeof(fpagesizeqt));
{$ENDIF}
 Stream.Write(FUpdatedPageSize,sizeof(FUpdatedPageSize));
 Stream.Write(FObjectCount,sizeof(FObjectCount));
 byteswrite:=sizeof(TRpMetaObject)*FObjectCount;
 SetLength(abytes,byteswrite);
 if byteswrite>0 then
 begin
{$IFDEF DOTNETD}
  System.Array.Copy(FObjects,abytes,byteswrite);
{$ENDIF}
{$IFNDEF DOTNETD}
  Move(FObjects[0],abytes[0],byteswrite);
{$ENDIF}
  if byteswrite<>Stream.Write(abytes[0],byteswrite) then
   Raise Exception.Create(SRpErrorWritingPage);
 end;
 wsize:=Length(FPool)*2;
 Stream.Write(wsize,sizeof(wsize));
 if wsize>0 then
 begin
  WriteWideStringToStream(FPool,Stream);
 end;
 asize:=FMemStream.Size;
 FMemStream.Seek(0,soFromBeginning);
 Stream.Write(asize,sizeof(asize));
{$IFDEF DOTNETD}
 Stream.Write(FMemStream.Memory,FMemStream.Size);
{$ENDIF}
{$IFNDEF DOTNETD}
 Stream.Write(FMemStream.Memory^,FMemStream.Size);
{$ENDIF}
end;

procedure TRpMetafilePage.LoadFromStream(Stream:TStream);
var
 separator:integer;
 bytesread:integer;
 objcount:integer;
 asize:int64;
 wsize:integer;
{$IFDEF DOTNETD}
 i:integer;
{$ENDIF}
 abytes:array of Byte;
begin
 SetLength(abytes,200);
 // read the object separator
 bytesread:=Stream.Read(separator,sizeof(separator));
 if (bytesread<>sizeof(separator)) then
  Raise ERpBadFileFormat.CreatePos(SrpMtObjectSeparatorExpected,Stream.Position,0);
 if (separator<>integer(rpFObject)) then
  Raise ERpBadFileFormat.CreatePos(SrpMtObjectSeparatorExpected,Stream.Position,0);
 bytesread:=Stream.Read(FMark,sizeof(FMark));
 if (bytesread<>sizeof(FMark)) then
  Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
 if Not FVersion2_2 then
 begin
{$IFDEF DOTNETD}
  bytesread:=Stream.Read(separator,sizeof(separator));
  if (bytesread<>sizeof(separator)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
  FOrientation:=TRpOrientation(separator);
  bytesread:=Stream.Read(fpagesizeqt.Indexqt,sizeof(fpagesizeqt.IndexQt));
  if (bytesread<>sizeof(fpagesizeqt.Indexqt)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
  bytesread:=Stream.Read(fpagesizeqt.Custom,sizeof(fpagesizeqt.Custom));
  if (bytesread<>sizeof(fpagesizeqt.Custom)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
  bytesread:=Stream.Read(fpagesizeqt.CustomWidth,sizeof(fpagesizeqt.CustomWidth));
  if (bytesread<>sizeof(fpagesizeqt.CustomWidth)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
  bytesread:=Stream.Read(fpagesizeqt.CustomHeight,sizeof(fpagesizeqt.CustomHeight));
  if (bytesread<>sizeof(fpagesizeqt.CustomHeight)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
  bytesread:=Stream.Read(fpagesizeqt.PhysicWidth,sizeof(fpagesizeqt.PhysicWidth));
  if (bytesread<>sizeof(fpagesizeqt.PhysicWidth)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
  bytesread:=Stream.Read(fpagesizeqt.PhysicHeight,sizeof(fpagesizeqt.PhysicHeight));
  if (bytesread<>sizeof(fpagesizeqt.PhysicHeight)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
  bytesread:=Stream.Read(fpagesizeqt.PaperSource,sizeof(fpagesizeqt.PaperSource));
  if (bytesread<>sizeof(fpagesizeqt.PaperSource)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
  SetLength(abytes,bytesread);
  bytesread:=61;
  if (bytesread<>Stream.Read(abytes[0],bytesread)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
  System.Array.Copy(abytes,fpagesizeqt.ForcePaperName,bytesread);
  bytesread:=Stream.Read(fpagesizeqt.Duplex,sizeof(fpagesizeqt.Duplex));
  if (bytesread<>sizeof(fpagesizeqt.Duplex)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
{$ENDIF}
{$IFNDEF DOTNETD}
  bytesread:=Stream.Read(FOrientation,sizeof(Forientation));
  if (bytesread<>sizeof(Forientation)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
  bytesread:=Stream.Read(Fpagesizeqt,sizeof(Fpagesizeqt));
  if (bytesread<>sizeof(Fpagesizeqt)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
{$ENDIF}
  bytesread:=Stream.Read(FUpdatedPageSize,sizeof(FUpdatedPageSize));
  if (bytesread<>sizeof(FUpdatedPageSize)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
 end;
 bytesread:=Stream.Read(objcount,sizeof(objcount));
 if (bytesread<>sizeof(objcount)) then
  Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
 if (objcount<0) then
  Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
 if High(FObjects)-1<objcount then
  SetLength(FObjects,objcount+1);
 // Read then whole array
 bytesread:=objcount*sizeof(TRpMetaObject);
 SetLength(abytes,bytesread);
 if bytesread>0 then
 begin
  if (bytesread<>Stream.Read(abytes[0],bytesread)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
 {$IFDEF DOTNETD}
  System.Array.Copy(abytes,FObjects,bytesread);
 {$ENDIF}
 {$IFNDEF DOTNETD}
  Move(abytes[0],FObjects[0],bytesread);
 {$ENDIF}
 end;
 // Read string pool
 if (sizeof(wsize)<>Stream.Read(wsize,sizeof(wsize))) then
  Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
 if wsize<0 then
  Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
 SetLength(FPool,wsize div 2);
 SetLength(abytes,wsize);
 if wsize>0 then
 begin
{$IFDEF DOTNETD}
  if (wsize<>Stream.Read(abytes[0],wsize)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
  for i:=0 to wsize div 2 do
  begin
   // Revise byte order
   FPool[i+1]:=WideChar((Integer(abytes[i+1]) shl 8)+abytes[i]);
  end;
   // System.Array.Copy(abytes,FPool,wsize);
{$ENDIF}
{$IFNDEF DOTNETD}
  if (wsize<>Stream.Read(abytes[0],wsize)) then
   Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
 // Copy the pool
  Move(abytes[0],FPool[1],wsize);
{$ENDIF}
 end;
 FPoolPos:=(wsize div 2)+1;
 // The Stream
 if (sizeof(asize)<>Stream.Read(asize,sizeof(asize))) then
  Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
 if asize<0 then
  Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
 if asize=0 then
 begin
  FMemStream.Free;
  FMemStream:=TMemoryStream.Create;
 end
 else
  FMemStream.SetSize(asize);
 if asize>0 then
 begin
{$IFDEF DOTNETD}
  if (asize<>Stream.Read(FMemStream.Memory[0],asize)) then
{$ENDIF}
{$IFNDEF DOTNETD}
  if (asize<>Stream.Read(FMemStream.Memory^,asize)) then
{$ENDIF}
  Raise ERpBadFileFormat.CreatePos(SrpStreamErrorPage,Stream.Position,0);
 end;
 FObjectCount:=objcount;
end;

constructor ErpBadFileFormat.CreatePos(Msg:String;APosition,Pos2:LongInt);
begin
 inherited Create(Msg);
 FPosition:=Position;
end;


procedure TRpMetafileReport.SaveToFile(filename:string);
var
 fstream:TFileStream;
begin
 fstream:=TFileStream.Create(filename,fmCreate);
 try
  SaveToStream(fstream);
 finally
  fstream.free;
 end
end;

procedure TRpMetafileReport.LoadFromFile(filename:string;clearfirst:boolean=true);
var
 fstream:TFileStream;
begin
 fstream:=TFileStream.Create(filename,fmOpenRead);
 try
  LoadFromStream(fstream,clearfirst);
 finally
  fstream.free;
 end
end;


procedure TRpMetafileReport.DrawPageOnly(IDriver:IRpPrintDriver);
var
 FPage:TRpMetafilePage;
begin
 if FCurrentPage<0 then
  exit;
 FPage:=TRpMetafilePage(FPages.items[FCurrentPage]);
 IDriver.DrawPage(FPage);
end;


procedure TRpMetafileReport.DrawPage(IDriver:IRpPrintDriver);
begin
 IDriver.NewDocument(self,1,false);
 try
  DrawPageOnly(IDriver);
  IDriver.EndPage;
  IDriver.EndDocument;
 except
  IDriver.AbortDocument;
  raise;
 end;
end;

procedure TRpMetafileReport.DrawAll(IDriver:IRpPrintDriver);
var
 i:integeR;
begin
 IDriver.NewDocument(self,1,false);
 try
  for i:=0 to PageCount-1 do
  begin
   if i>0 then
    IDriver.NewPage(Pages[i]);
   CurrentPage:=i;
   DrawPageOnly(IDriver);
  end;
  IDriver.EndPage;

  IDriver.EndDocument;
 except
  IDriver.AbortDocument;
  raise;
 end;
end;

procedure TRpMetafileReport.UpdateTotalPages(alist:TList);
var
 i,index:integer;
 aobject:TTotalPagesObject;
 apage:TrpMetafilePage;
 astring:widestring;
 oldtexts:integer;
 tempstring:widestring;
begin
 for i:=0 to alist.count-1 do
 begin
  aobject:=TTotalPagesObject(alist.Items[i]);
  apage:=Pages[aobject.PageIndex];
  index:=apage.Objects[aobject.ObjectIndex].TextP;
  if Length(aobject.displayformat)>0 then
   astring:=FormatCurr(aobject.displayformat,PageCount)
  else
   astring:=IntToStr(PageCount);
//  oldtexts:=apage.Objects[aobject.ObjectIndex].TextS;
  oldtexts:=9;
  apage.FObjects[aobject.ObjectIndex].TextS:=Length(astring);
  astring:=astring+'                                      ';
  tempstring:=Copy(apage.Pool,1,index-1);
  tempstring:=tempstring+Copy(astring,1,oldtexts);
  tempstring:=tempstring+Copy(apage.Pool,index+oldtexts,Length(apage.Pool));
  apage.FPool:=tempstring;
 end;
end;

procedure TRpMetafileReport.UpdateTotalPagesPCount(alist:TList;pcount:integer);
var
 i,index:integer;
 aobject:TTotalPagesObject;
 apage:TrpMetafilePage;
 astring:widestring;
 oldtexts:integer;
 tempstring:widestring;
begin
 for i:=0 to alist.count-1 do
 begin
  aobject:=TTotalPagesObject(alist.Items[i]);
  apage:=Pages[aobject.PageIndex];
  index:=apage.Objects[aobject.ObjectIndex].TextP;
  if Length(aobject.displayformat)>0 then
   astring:=FormatCurr(aobject.displayformat,PCount)
  else
   astring:=IntToStr(PCount);
//  oldtexts:=apage.Objects[aobject.ObjectIndex].TextS;
  oldtexts:=9;
  apage.FObjects[aobject.ObjectIndex].TextS:=Length(astring);
  astring:=astring+'                                      ';
  tempstring:=Copy(apage.Pool,1,index-1);
  tempstring:=tempstring+Copy(astring,1,oldtexts);
  tempstring:=tempstring+Copy(apage.Pool,index+oldtexts,Length(apage.Pool));
  apage.FPool:=tempstring;
 end;
end;

procedure TRpMetafileReport.Assign(Source:TPersistent);
var
 memstream:TMemoryStream;
begin
 if Not (Source is TRpMetafileReport) then
 begin
  inherited Assign(Source);
  exit;
 end;
 memstream:=TMemoryStream.Create;
 try
  TRpMetafileReport(Source).SaveToStream(memstream);
  memstream.Seek(0,soFromBeginning);
  LoadFromStream(memstream);
 finally
  memstream.free;
 end;
end;

{function CalcTextExtent(adriver:IRpPrintDriver;maxextent:TPoint;obj:TRpTextObject):integer;
var
 newextent:TPoint;
 currentPos,lasttested:Integer;
 delimiters:string;
 originalstring:WideString;
begin
 delimiters:=' '+'.'+','+'-'+'/'+'\'+'='+')'+'('+'*'+'+'+'-';
 currentpos:=Length(obj.Text);
 originalstring:=obj.Text;
 obj.Text:=Copy(originalstring,1,currentpos);
 newextent:=maxextent;
 adriver.TextExtent(obj,newextent);

 lasttested:=CurrentPos;
 // Speed enhacement to cut at least lot of size testing
 while (newextent.Y>maxextent.Y) do
 begin
  currentpos:=currentpos div 2;
  while currentpos>0 do
  begin
   Dec(currentpos);
   if currentpos<1 then
    break;
   if isdelimiter(delimiters,obj.Text,currentpos) then
    break;
  end;

  if currentpos<1 then
   break;
  obj.Text:=Copy(originalstring,1,currentpos);
  newextent:=maxextent;
  adriver.TextExtent(obj,newextent);
  if newextent.Y<=maxextent.Y then
   break
  else
   lasttested:=currentpos;
 end;
 currentpos:=lasttested;
 obj.Text:=Copy(originalstring,1,currentpos);
 newextent:=maxextent;
 adriver.TextExtent(obj,newextent);
 while newextent.Y>maxextent.Y do
 begin
  while currentpos>0 do
  begin
   Dec(currentpos);
   if isdelimiter(delimiters,obj.Text,currentpos) then
    break;
  end;

  if currentpos<1 then
   break;
  obj.Text:=Copy(originalstring,1,currentpos);
  newextent:=maxextent;
  adriver.TextExtent(obj,newextent);
 end;
 if currentpos<1 then
  Result:=Length(obj.Text)
 else
 begin
  Result:=CurrentPos;
 end;
end;
}

function isadelimiter(achar:WideChar):Boolean;
const
 delimiters:string=' .,-/\=)(*,-'+#10;
var
 nchar:Char;
 i:integer;
begin
 Result:=false;
 nchar:=Char(achar);
 for i:=1 to Length(delimiters) do
 begin
  if nchar=delimiters[i] then
  begin
   Result:=true;
   break;
  end;
 end;
end;

function CalcTextExtent(adriver:IRpPrintDriver;maxextent:TPoint;obj:TRpTextObject):integer;
var
 newextent:TPoint;
 currentPos,lasttested,oldcurrentpos:Integer;
// delimiters:string;
 originalstring:WideString;
 minpos,maxpos:integer;
begin
// delimiters:=' '+'.'+','+'-'+'/'+'\'+'='+')'+'('+'*'+'+'+'-'+#10;
 currentpos:=Length(obj.Text);
 originalstring:=Copy(obj.Text,1,currentpos);
 obj.Text:=Copy(originalstring,1,currentpos);
 newextent:=maxextent;
 adriver.TextExtent(obj,newextent);

 lasttested:=CurrentPos;
 minpos:=0;
 maxpos:=CurrentPos;
 oldcurrentpos:=0;
 // Speed enhacement to cut at least lot of size testing
 while (minpos<maxpos) do
 begin
  // The first test is performed
  CurrentPos:=(minpos+maxpos) div 2;
  // Word Break
  while currentpos>1 do
  begin
   Dec(currentpos);
   if isadelimiter(originalstring[currentpos]) then
    break;
  end;
  if oldcurrentpos=currentpos then
   break;
  oldcurrentpos:=currentpos;
  obj.Text:=Copy(originalstring,1,currentpos);
  newextent:=maxextent;
  adriver.TextExtent(obj,newextent);
  if newextent.Y<=maxextent.Y then
   minpos:=currentpos
  else
  begin
   lasttested:=currentpos;
   maxpos:=lasttested;
  end;
 end;
 currentpos:=lasttested;
 obj.Text:=Copy(originalstring,1,currentpos);
 newextent:=maxextent;
 adriver.TextExtent(obj,newextent);
 while newextent.Y>maxextent.Y do
 begin
  while currentpos>0 do
  begin
   Dec(currentpos);
   if currentpos<1 then
    break;
   if isadelimiter(obj.Text[currentpos]) then
    break;
  end;

  if currentpos<1 then
   break;
  obj.Text:=Copy(originalstring,1,currentpos);
  newextent:=maxextent;
  adriver.TextExtent(obj,newextent);
 end;
 if currentpos<1 then
  Result:=Length(obj.Text)
 else
 begin
  Result:=CurrentPos;
 end;
end;


procedure TRpMetafileReport.PageRange(frompage,topage:integer);
var
 newpagecount:integer;
begin
 // Delete to first page
 newpagecount:=topage-frompage+1;
 if newpagecount=0 then
 begin
  Clear;
  exit;
 end;
 while frompage>1 do
 begin
  DeletePage(0);
  Dec(frompage);
  if pagecount<1 then
   break;
 end;
 while newpagecount<PageCount do
 begin
  DeletePage(PageCount-1);
 end;
end;

{$IFDEF MSWINDOWS}
{$IFNDEF DOTNETD}
procedure  MetafileToExe(metafile:TRpMetafileReport;filename:String);
var
 tempfile:String;
begin
 tempfile:=RpTempFileName;
 tempfile:=changeFileExt(tempfile,'.rpmf');
 metafile.SaveToFile(tempfile);
 try
  ReportFileToExe(tempfile,filename,true,true,true,true);
 finally
  DeleteFile(tempfile);
 end;
end;
{$ENDIF}
{$ENDIF}


end.
