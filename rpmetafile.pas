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

uses Classes,Sysutils,rpconsts;

const
 RP_SIGNATURELENGTH=13;
 RpSignature:array[0..RP_SIGNATURELENGTH-1] of char=('R','P','M','E','T','A','F','I','L',
  'E','0','1',#0);

type
 ERpBadFileFormat=class(Exception)
  private
   FPosition:integer;
  public
   constructor Create(Msg:String;APosition:LongInt);
   property position:LongInt read FPosition;
  end;
 TRpMetafileobject=class;
 TrpMetafileReport=class;

 IRpPrintDriver=interface
 ['{11EF15B0-5CDE-40F0-A204-973A25B38B81}']
  procedure NewDocument(report:TrpMetafileReport);stdcall;
  procedure EndDocument;stdcall;
  procedure AbortDocument;stdcall;
  procedure NewPage;stdcall;
  procedure EndPage;stdcall;
  procedure DrawObject(obj:TRpMetafileObject);stdcall;
  function AllowCopies:boolean;stdcall;
 end;

 TRpMetaObjectType=(rpMetaText,rpMetaDraw,rpMetaImage);

 TRpMetaSeparator=(rpFHeader,rpFPage,rpFObject);

 TRpMetafileObject=class(TObject)
  public
   Metatype:TRpMetaObjectType;
   Top,Left,Width,Height:integer;
   Text:widestring;
   FontName:widestring;
   FontSize:integer;
   FontStyle:integer;
   FontColor:integer;
   BackColor:integer;
   Transparent:boolean;
   DrawStyle:integer;
   BrushStyle:integer;
   BrushColor:integer;
   PenWidth:integer;
   PenColor:integer;
   StreamedImage:TMemoryStream;
   procedure LoadFromStream(stream:TStream);
   procedure SaveToStream(stream:TStream);
   destructor Destroy;override;
  end;

 TRpMetafilePage=class(TObject)
  private
   FObjects:TList;
   function GetObjectCount:integer;
   function GetObject(index:integer):TRpMetafileObject;
  public
   procedure LoadFromStream(Stream:TStream);
   procedure SaveToStream(Stream:TStream);
   procedure DeleteObject(index:integer);
   constructor Create;
   destructor Destroy;override;
   procedure NewTextObject(Top,Left,Width,Height:integer;
    Text:widestring;FontName:widestring;FontSize:integer;FontStyle:integer;
    FontColor:integer;BackColor:integer;transparent:boolean);
   procedure NewDrawObject(Top,Left,Width,Height:integer;
    DrawStyle:integer;BrushStyle:integer;BrushColor:integer;
    PenWidth:integer; PenColor:integer);
   procedure NewImageObject(Top,Left,Width,Height:integer;
    stream:TStream);

   property ObjectCount:integer read GetObjectCount;
   property Objects[Index:integer]:TRpMetafileObject read GetObject;
  end;


 TRpMetafileReport=class(TComponent)
  private
   FPages:TList;
   FCurrentPage:integer;
   procedure SetCurrentPage(index:integer);
   function GetPageCount:integer;
   function GetPage(Index:integer):TRpMetafilePage;
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
   procedure DrawPageOnly(IDriver:IRpPrintDriver);
   procedure InsertPage(index:integer);
   procedure DeletePage(index:integer);
   property CurrentPage:integer read FCurrentPage write SetCurrentPage;
   property PageCount:integer read GetPageCount;
   property Pages[Index:integer]:TRpMetafilePage read GetPage;
  published
  end;

implementation


procedure TrpMetafilePage.NewImageObject(Top,Left,Width,Height:integer;
    stream:TStream);
var
 FObject:TrpMetafileObject;
begin
 FObject:=TRpMetafileObject.Create;
 FObject.Left:=Left;
 FObject.Top:=Top;
 FObject.Width:=Width;
 FObject.Height:=Height;
 FObject.StreamedImage:=TMemoryStream.Create;
 Stream.Seek(soFromBeginning,0);
 FObject.StreamedImage.LoadFromStream(Stream);

 FObjects.Add(FObject);
end;

procedure TrpMetafilePage.NewDrawObject(Top,Left,Width,Height:integer;
    DrawStyle:integer;BrushStyle:integer;BrushColor:integer;
    PenWidth:integer; PenColor:integer);
var
 FObject:TrpMetafileObject;
begin
 FObject:=TRpMetafileObject.Create;
 FObject.Left:=Left;
 FObject.Top:=Top;
 FObject.Width:=Width;
 FObject.Height:=Height;
 FObject.DrawStyle:=DrawStyle;
 FObject.BrushStyle:=BrushStyle;
 FObject.BrushColor:=BrushColor;
 FObject.PenColor:=PenColor;
 FObject.PenWidth:=PenWidth;

 FObjects.Add(FObject);
end;

procedure TrpMetafilePage.NewTextObject(Top,Left,Width,Height:integer;
    Text:widestring;FontName:widestring;FontSize:integer;FontStyle:integer;
    FontColor:integer;BackColor:integer;transparent:boolean);
var
 FObject:TrpMetafileObject;
begin
 FObject:=TRpMetafileObject.Create;
 FObject.Left:=Left;
 FObject.Top:=Top;
 FObject.Width:=Width;
 FObject.Height:=Height;
 FObject.Text:=Text;
 FObject.FontName:=FontName;
 FObject.FontColor:=FontColor;
 FObject.FontStyle:=FontStyle;
 FObject.BackColor:=BackColor;
 FObject.FontSize:=FontSize;
 FObject.Transparent:=Transparent;
 FObjects.Add(FObject);
end;

procedure TrpMetafilePage.DeleteObject(index:integer);
begin
 if index<0 then
  Raise Exception.Create(SRpMetaIndexObjectOutofBounds);
 if index>FObjects.Count-1 then
  Raise Exception.Create(SRpMetaIndexObjectOutofBounds);
 TObject(Fobjects.items[index]).free;
 FObjects.delete(index);
end;

function TrpMetafilePage.GetObject(index:integer):TrpMetafileObject;
begin
 if index<0 then
  Raise Exception.Create(SRpMetaIndexObjectOutofBounds);
 if index>FObjects.Count-1 then
  Raise Exception.Create(SRpMetaIndexObjectOutofBounds);
 Result:=TRpMetafileObject(FObjects.items[index]);
end;

function TrpMetafilePage.GetObjectCount:integer;
begin
 Result:=FObjects.count;
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
  TObject(FPages.Items[i]).free;
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



constructor TRpMetafilePage.Create;
begin
 FObjects:=TList.Create;
end;

destructor TRpMetafilePage.Destroy;
var
 i:integer;
begin
 for i:=0 to FObjects.Count-1 do
 begin
  TObject(FObjects.Items[i]).free;
 end;
 FObjects.clear;
 FObjects.free;

 inherited Destroy;
end;

procedure TRpMetafileReport.SaveToStream(Stream:TStream);
var
 separator:integer;
 i:integer;
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
 for i:=0 to FPages.count-1 do
 begin
  separator:=integer(rpFPage);
  Stream.Write(separator,sizeof(separator));

  TRpMetafilePage(FPages.items[i]).SaveToStream(Stream);
 end;
end;


procedure TRpMetafileReport.LoadFromStream(Stream:TStream);
var
 separator:integer;
 buf:array[0..RP_SIGNATURELENGTH-1] of char;
 bytesread:integer;
 fpage:TRpMetafilePage;
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


procedure TRpMetafilePage.LoadFromStream(Stream:TStream);
var
 separator:integer;
 bytesread:integer;
 fobject:TRpMetafileObject;
begin
 // read the object separator
 bytesread:=Stream.Read(separator,sizeof(separator));
 while (bytesread>0) do
 begin
  if (bytesread<>sizeof(separator)) then
   Raise ERpBadFileFormat.Create(SrpMtObjectSeparatorExpected,Stream.Position);
  if (separator<>integer(rpFObject)) then
   Raise ERpBadFileFormat.Create(SrpMtObjectSeparatorExpected,Stream.Position);
  // Creates a new object
  fobject:=TRpMetafileObject.Create;
  FObjects.Add(fobject);
  fobject.LoadFromStream(Stream);

  bytesread:=Stream.Read(separator,sizeof(separator));
 end;
end;

procedure TRpMetafilePage.SaveToStream(Stream:TStream);
var
 i:integer;
 separator:integer;
begin
 // Objects
 for i:=0 to FObjects.count-1 do
 begin
  separator:=integer(rpFObject);
  Stream.Write(separator,sizeof(separator));

  TRpMetafileObject(FObjects.items[i]).SaveToStream(Stream);
 end;
end;

procedure TRpMetafileObject.LoadFromStream(stream:TStream);
var
 ssize:Int64;
 numbytes:integer;
 bytesread:integer;
begin
 // Writes the meta type and the info
 bytesread:=Stream.Read(MetaType,sizeof(metatype));
 if (bytesread<>sizeof(metatype)) then
  Raise ERpBadFileFormat.Create(SrpObjectTypeError,Stream.Position);

 if (sizeof(Top)<>Stream.Read(Top,sizeof(Top))) then
  Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
 if (sizeof(Left)<>Stream.Read(Left,sizeof(Left))) then
  Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
 if (sizeof(Width)<>Stream.Read(Width,sizeof(Width))) then
  Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
 if (sizeof(Height)<>Stream.Read(Height,sizeof(Height))) then
  Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
 case metatype of
  rpMetaText:
   begin
    // Wide string is 2 bytes per char.
    if (sizeof(numbytes)<>Stream.Read(numbytes,sizeof(numbytes))) then
     Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
    SetLength(text,numbytes div 2);
    Stream.Read(PWideString(Text)^,numbytes);
    // Wide string is 2 bytes per char.
    if (sizeof(numbytes)<>Stream.Read(numbytes,sizeof(numbytes))) then
     Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
    SetLength(FontName,numbytes div 2);
    Stream.Read(PWideString(FontName)^,numbytes);
    // Font Properties
    if (sizeof(FontSize)<>Stream.Read(FontSize,sizeof(FontSize))) then
     Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
    if (sizeof(FontStyle)<>Stream.Read(FontStyle,sizeof(FontStyle))) then
     Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
    if (sizeof(FontColor)<>Stream.Read(FontColor,sizeof(FontColor))) then
     Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
    if (sizeof(BackColor)<>Stream.Read(BackColor,sizeof(BackColor))) then
     Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
    if (sizeof(Transparent)<>Stream.Read(Transparent,sizeof(Transparent))) then
     Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
   end;
  rpMetaDraw:
   begin
    if (sizeof(PenWidth)<>Stream.Read(PenWidth,sizeof(PenWidth))) then
     Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
    if (sizeof(PenColor)<>Stream.Read(PenColor,sizeof(PenColor))) then
     Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
    if (sizeof(BrushColor)<>Stream.Read(BrushColor,sizeof(BrushColor))) then
     Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
    if (sizeof(BrushStyle)<>Stream.Read(BrushStyle,sizeof(BrushStyle))) then
     Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
    if (sizeof(DrawStyle)<>Stream.Read(DrawStyle,sizeof(DrawStyle))) then
     Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
   end;
  rpMetaImage:
   begin
     // Save the size of the stream and the stream
     if (sizeof(ssize)<>Stream.Read(ssize,sizeof(ssize))) then
      Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
     if Assigned(StreamedImage) then
     begin
      StreamedImage.free;
      StreamedImage:=nil;
     end;
     StreamedImage:=TMemoryStream.Create;
     StreamedImage.SetSize(ssize);
     if (ssize<>Stream.Read(StreamedImage.Memory^,sizeof(ssize))) then
      Raise ERpBadFileFormat.Create(SrpObjectDataError,Stream.Position);
     StreamedImage.Seek(0,soFromBeginning);
   end;
 end;

end;

procedure TRpMetafileObject.SaveToStream(stream:TStream);
var
 ssize:Int64;
 numbytes:integer;
begin
 // Writes the meta type and the info
 Stream.Write(MetaType,sizeof(metatype));

 Stream.Write(Top,sizeof(Top));
 Stream.Write(Left,sizeof(Left));
 Stream.Write(Width,sizeof(Width));
 Stream.Write(Height,sizeof(Height));
 case metatype of
  rpMetaText:
   begin
    // Wide string is 2 bytes per char.
    numbytes:=Length(Text)*2;
    Stream.Write(numbytes,sizeof(numbytes));
    Stream.Write(PWideString(Text)^,numbytes);
    // Font Properties
    numbytes:=Length(FontName)*2;
    Stream.Write(numbytes,sizeof(numbytes));
    Stream.Write(PWideString(FontName)^,numbytes);
    Stream.Write(FontSize,sizeof(FontSize));
    Stream.Write(FontStyle,sizeof(FontStyle));
    Stream.Write(FontColor,sizeof(FontColor));
    Stream.Write(BackColor,sizeof(BackColor));
    Stream.Write(Transparent,sizeof(Transparent));
   end;
  rpMetaDraw:
   begin
    Stream.Write(PenWidth,sizeof(PenWidth));
    Stream.Write(PenColor,sizeof(PenColor));
    Stream.Write(BrushColor,sizeof(BrushColor));
    Stream.Write(BrushStyle,sizeof(BrushStyle));
    Stream.Write(DrawStyle,sizeof(DrawStyle));
   end;
  rpMetaImage:
   begin
     // Save the size of the stream and the stream
     ssize:=StreamedImage.Size;
     Stream.Write(ssize,sizeof(ssize));
     StreamedImage.Seek(0,soFromBeginning);
     Stream.Write(StreamedImage.memory^,ssize);
   end;
 end;

end;

constructor ErpBadFileFormat.Create(Msg:String;APosition:LongInt);
begin
 FPosition:=Position;
 inherited Create(Msg);
end;

destructor TRpMetafileObject.Destroy;
begin
 if Assigned(StreamedImage) then
 begin
  StreamedImage.free;
  StreamedImage:=nil;
 end;
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
  IDriver.DrawObject(TRpMetafileObject(FPage.FObjects[i]))
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

end.
