{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpmetafile                                      }
{       TRpMetafileReport: A Metafile report            }
{       is a collection of pages that can be printed    }
{       using a printer driver interface                }
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

{
 The Report Metafile is a file that contains all
 the info to print a complete report.
 File Format Binary

 Signature='RPMETAFILE01'
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

uses Classes,Sysutils,rpconsts,rpzlib;

const
 RP_SIGNATURELENGTH=13;
 RpSignature:array[0..RP_SIGNATURELENGTH-1] of char=('R','P','M','E','T','A','F','I','L',
  'E','0','1',#0);
const
 FIRST_ALLOCATION_OBJECTS=500;
 FIRST_ALLOCATED_WIDESTRING=1000;
type

 ERpBadFileFormat=class(Exception)
  private
   FPosition:integer;
  public
   constructor Create(Msg:String;APosition:LongInt);
   property position:LongInt read FPosition;
  end;
 TrpMetafileReport=class;
 TrpMetafilePage=class;


 TRpMetaObjectType=(rpMetaText,rpMetaDraw,rpMetaImage);

 TRpMetaSeparator=(rpFHeader,rpFPage,rpFObject);

 TRpMetaObject=record
  Top,Left,Width,Height:integer;
  case Metatype:TRpMetaObjectType of
   rpMetaText:
    (TextP,TextS:integer;
    LFontNameP,LFontNameS:integer;
    WFontNameP,WFontNameS:integer;
    FontSize:integer;
    FontStyle:integer;
    FontColor:integer;
    BackColor:integer;
    Transparent:boolean;
    CutText:boolean;Alignment:integer;WordWrap:boolean);
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
     StreamPos:int64;
     StreamSize:int64);
 end;

 IRpPrintDriver=interface
 ['{11EF15B0-5CDE-40F0-A204-973A25B38B81}']
  procedure NewDocument(report:TrpMetafileReport);stdcall;
  procedure EndDocument;stdcall;
  procedure AbortDocument;stdcall;
  procedure NewPage;stdcall;
  procedure EndPage;stdcall;
  procedure DrawObject(page:TRpMetaFilePage;obj:TRpMetaObject);stdcall;
  function AllowCopies:boolean;stdcall;
 end;


 TRpMetafilePage=class(TObject)
  private
   FObjects:array of TRpMetaObject;
   FObjectCount:Integer;
   FPool:Widestring;
   FPoolPos:integer;
   FStreamPos:int64;
   FMemStream:TMemoryStream;
   FIntStream:TMemoryStream;
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
    Text:widestring;WFontName:widestring;LFontName:widestring;FontSize:integer;FontStyle:integer;
    FontColor:integer;BackColor:integer;transparent:boolean;cuttext:boolean;Alignment:integer;WordWrap:boolean);
   procedure NewDrawObject(Top,Left,Width,Height:integer;
    DrawStyle:integer;BrushStyle:integer;BrushColor:integer;
    PenStyle:integer;PenWidth:integer; PenColor:integer);
   procedure NewImageObject(Top,Left,Width,Height:integer;
    CopyMode:integer;DrawImageStyle:integer;DPIres:integer;stream:TStream);
   function GetText(arecord:TRpMetaObject):widestring;
   function GetWFontName(arecord:TRpMetaObject):widestring;
   function GetLFontName(arecord:TRpMetaObject):widestring;
   function GetStream(arecord:TRpMetaObject):TMemoryStream;
   property ObjectCount:integer read FObjectCount;
   property Objects[Index:integer]:TRpMetaObject read GetObject;
  end;


 TRpMetafileReport=class(TComponent)
  private
   FPages:TList;
   FCurrentPage:integer;
   procedure SetCurrentPage(index:integer);
   function GetPageCount:integer;
   function GetPage(Index:integer):TRpMetafilePage;
   procedure IntSaveToStream(Stream:TStream);
   procedure IntLoadFromStream(Stream:TStream);
  public
   PageSize:integer;
   CustomX:integer;
   CustomY:integer;
   Orientation:integer;
   BackColor:integer;
   procedure Clear;
   procedure LoadFromStream(Stream:TStream);
   procedure LoadFromFile(filename:string);
   procedure SaveToStream(Stream:TStream);
   procedure SaveToFile(filename:string);
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
   property Pages[Index:integer]:TRpMetafilePage read GetPage;
  published
  end;

implementation

constructor TrpMetafilePage.Create;
begin
 SetLength(FObjects,FIRST_ALLOCATION_OBJECTS);
 FObjectCount:=0;
 FPoolPos:=1;
 FStreamPos:=0;
 FMemStream:=TMemoryStream.Create;
end;

procedure TRpMetafilePage.Clear;
begin
 FObjectCount:=0;
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
 CopyMode:integer; DrawImageStyle:integer;DPIres:integer;stream:TStream);
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


procedure TrpMetafilePage.NewTextObject(Top,Left,Width,Height:integer;
    Text:widestring;WFontName:widestring;LFontName:widestring;FontSize:integer;FontStyle:integer;
    FontColor:integer;BackColor:integer;transparent:boolean;cuttext:boolean;Alignment:integer;WordWrap:boolean);
var
 position,size:integer;
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

 NewWideString(FObjects[FObjectCount].TextP,FObjects[FObjectCount].TextS,Text);
 NewWideString(FObjects[FObjectCount].WFontNameP,
  FObjects[FObjectCount].WFontNameS,WFontName);
 NewWideString(FObjects[FObjectCount].LFontNameP,
  FObjects[FObjectCount].LFontNameS,LFontName);
 FObjects[FObjectCount].FontSize:=FontSize;
 FObjects[FObjectCount].FontStyle:=FontStyle;
 FObjects[FObjectCount].FontColor:=FontColor;
 FObjects[FObjectCount].BackColor:=BackColor;
 FObjects[FObjectCount].Transparent:=Transparent;
 FObjects[FObjectCount].CutText:=CutText;
 FObjects[FObjectCount].Alignment:=Alignment;
 FObjects[FObjectCount].WordWrap:=WordWrap;

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
 CustomX:=2100;
 CustomY:=2970;

 FPages:=TList.Create;
end;

procedure TRpMetafileReport.Clear;
var
 i:integer;
begin
 for i:=0 to FPages.Count-1 do
 begin
  TRpMetafilePage(FPages.Items[i]).Clear;
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
var
 zstream:TCompressionStream;
begin
 zstream:=TCompressionStream.Create(clDefault,Stream);
 try
  IntSaveToStream(zstream);
 finally
  zstream.free;
 end;
end;

procedure TRpMetafileReport.IntSaveToStream(Stream:TStream);
var
 separator:integer;
 i:integer;
 acount:integer;
begin
 Stream.Write(rpSignature,RP_SIGNATURELENGTH);
 separator:=integer(rpFHeader);
 Stream.Write(separator,sizeof(separator));
 // Report header
 Stream.Write(PageSize,sizeof(pagesize));
 Stream.Write(CustomX,sizeof(CustomX));
 Stream.Write(CustomY,sizeof(CustomY));
 Stream.Write(Orientation,sizeof(Orientation));
 Stream.Write(BackColor,sizeof(BackColor));
 // Pages
 // Write pagecount
 acount:=FPages.Count;
 Stream.Write(acount,sizeof(acount));
 for i:=0 to FPages.count-1 do
 begin
  separator:=integer(rpFPage);
  Stream.Write(separator,sizeof(separator));

  TRpMetafilePage(FPages.items[i]).SaveToStream(Stream);
 end;
end;

procedure TRpMetafileReport.LoadFromStream(Stream:TStream);
var
 zStream:TDeCompressionStream;
begin
 zStream:=TDeCompressionStream.Create(Stream);
 try
  IntLoadFromStream(zStream);
 finally
  zStream.free;
 end;
end;

procedure TRpMetafileReport.IntLoadFromStream(Stream:TStream);
var
 separator:integer;
 buf:array[0..RP_SIGNATURELENGTH-1] of char;
 bytesread:integer;
 fpage:TRpMetafilePage;
 acount:integer;
begin
 // Clears the report metafile
 Clear;

 bytesread:=Stream.Read(buf,RP_SIGNATURELENGTH);
 if (bytesread<RP_SIGNATURELENGTH) then
  Raise Exception.Create(SRpBadSignature);
 if (StrPas(buf)<>StrPas(rpSignature)) then
  Raise Exception.Create(SRpBadSignature);
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
 if (sizeof(Orientation)<>Stream.Read(Orientation,sizeof(Orientation))) then
  Raise Exception.Create(SRpBadFileHeader);
 if (sizeof(BackColor)<>Stream.Read(BackColor,sizeof(BackColor))) then
  Raise Exception.Create(SRpBadFileHeader);
 // If there is no pages then end of read
 // Read pagecount
 if (sizeof(acount)<>Stream.Read(acount,sizeof(acount))) then
  Raise Exception.Create(SRpBadFileHeader);


 // Pages
 bytesread:=Stream.Read(separator,sizeof(separator));
 while (bytesread>0) do
 begin
  if bytesread<>sizeof(separator) then
   Raise ERpBadFileFormat.Create(SrpMtPageSeparatorExpected,Stream.Position);
  if (separator<>integer(rpFPage)) then
   Raise ERpBadFileFormat.Create(SrpMtPageSeparatorExpected,Stream.Position);
  // New page and load from stream
  fpage:=TRpMetafilePage.Create;
  FPages.Add(fpage);

  fpage.LoadFromStream(Stream);

  bytesread:=Stream.Read(separator,sizeof(separator));
 end;
 if Fpages.Count>0 then
  FCurrentPage:=0;
end;


procedure TRpMetafilePage.SaveToStream(Stream:TStream);
var
 i:integer;
 separator:integer;
 asize:int64;
 wsize:integer;
 byteswrite:integer;
begin
 // Objects
 // Save all objects
 separator:=integer(rpFObject);
 Stream.Write(separator,sizeof(separator));
 Stream.Write(FObjectCount,sizeof(FObjectCount));
 byteswrite:=sizeof(TRpMetaObject)*FObjectCount;
 if byteswrite<>Stream.Write(FObjects[0],byteswrite) then
  Raise Exception.Create(SRpErrorWritingPage);
 wsize:=Length(FPool)*2;
 Stream.Write(wsize,sizeof(wsize));
 Stream.Write(FPool[1],wsize);
 asize:=FMemStream.Size;
 FMemStream.Seek(0,soFromBeginning);
 Stream.Write(asize,sizeof(asize));
 Stream.Write(FMemStream.Memory^,FMemStream.Size);
end;

procedure TRpMetafilePage.LoadFromStream(Stream:TStream);
var
 separator:integer;
 bytesread:integer;
 objcount:integer;
 asize:int64;
 wsize:integer;
 stringsize:integer;
begin
 // read the object separator
 bytesread:=Stream.Read(separator,sizeof(separator));
 if (bytesread<>sizeof(separator)) then
  Raise ERpBadFileFormat.Create(SrpMtObjectSeparatorExpected,Stream.Position);
 if (separator<>integer(rpFObject)) then
  Raise ERpBadFileFormat.Create(SrpMtObjectSeparatorExpected,Stream.Position);
 bytesread:=Stream.Read(objcount,sizeof(objcount));
 if (bytesread<>sizeof(objcount)) then
  Raise ERpBadFileFormat.Create(SrpStreamErrorPage,Stream.Position);
 if (objcount<0) then
  Raise ERpBadFileFormat.Create(SrpStreamErrorPage,Stream.Position);
 if High(FObjects)-1<objcount then
  SetLength(FObjects,objcount+1);
 // Read then whole array
 bytesread:=objcount*sizeof(TRpMetaObject);
 if (bytesread<>Stream.Read(FObjects[0],bytesread)) then
  Raise ERpBadFileFormat.Create(SrpStreamErrorPage,Stream.Position);
 // Read string pool
 if (sizeof(wsize)<>Stream.Read(wsize,sizeof(wsize))) then
  Raise ERpBadFileFormat.Create(SrpStreamErrorPage,Stream.Position);
 if wsize<0 then
  Raise ERpBadFileFormat.Create(SrpStreamErrorPage,Stream.Position);
 SetLength(FPool,wsize div 2);
 if (wsize<>Stream.Read(FPool[1],wsize)) then
  Raise ERpBadFileFormat.Create(SrpStreamErrorPage,Stream.Position);
 // The Stream
 if (sizeof(asize)<>Stream.Read(asize,sizeof(asize))) then
  Raise ERpBadFileFormat.Create(SrpStreamErrorPage,Stream.Position);
 if asize<0 then
  Raise ERpBadFileFormat.Create(SrpStreamErrorPage,Stream.Position);
 if asize=0 then
 begin
  FMemStream.Free;
  FMemStream:=TMemoryStream.Create;
 end
 else
  FMemStream.SetSize(asize);
 FMemStream.Seek(0,soFromBeginning);
 if (asize<>Stream.Read(FMemStream.Memory^,asize)) then
  Raise ERpBadFileFormat.Create(SrpStreamErrorPage,Stream.Position);

 FObjectCount:=objcount;
end;
constructor ErpBadFileFormat.Create(Msg:String;APosition:LongInt);
begin
 FPosition:=Position;
 inherited Create(Msg);
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

procedure TRpMetafileReport.LoadFromFile(filename:string);
var
 fstream:TFileStream;
begin
 fstream:=TFileStream.Create(filename,fmOpenRead);
 try
  LoadFromStream(fstream);
 finally
  fstream.free;
 end
end;


procedure TRpMetafileReport.DrawPageOnly(IDriver:IRpPrintDriver);
var
 FPage:TRpMetafilePage;
 i:integer;
begin
 if FCurrentPage<0 then
  exit;
 FPage:=TRpMetafilePage(FPages.items[FCurrentPage]);
 for i:=0 to FPage.ObjectCount-1 do
 begin
  IDriver.DrawObject(FPage,FPage.Objects[i])
 end;
end;


procedure TRpMetafileReport.DrawPage(IDriver:IRpPrintDriver);
begin
 IDriver.NewDocument(self);
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
 IDriver.NewDocument(self);
 try
  for i:=0 to PageCount-1 do
  begin
   if i>0 then
    IDriver.NewPage;
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


end.
