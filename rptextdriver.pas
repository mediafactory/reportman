{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rptextdriver                                    }
{       TRpTextDriver: Printer driver to                }
{       generate plain text or dot matrix               }
{       prepared documents                              }
{       Limitations:                                    }
{       - Only one size/step for each line              }
{       will obtain the size from the first text        }
{       of that line                                    }
{       - Will overwrite text in same position as       }
{       designed a line can not have more than one      }
{       character in the same position                  }
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

unit rptextdriver;

interface

{$I rpconf.inc}

uses Classes,Sysutils,
{$IFDEF USEVARIANTS}
 Types,
{$ENDIF}
{$IFDEF MSWINDOWS}
 Windows,
{$ENDIF}
 rptypes,rpmetafile,
 rpmunits,rpreport,rpmdconsts;

const
 DEFAULT_LINESPERINCH=6;
type
 TRpPrintLine=record
  FontStep:TRpFontStep;
  Attributes:TStringList;
  Value:String;
 end;

 TRpAttribObject=class(TObject)
 public
  fontstyle:Integer;
  size:Integer;
  Position:Integer;
 end;

 TRpTextDriver=class(TInterfacedObject,IRpPrintDriver)
  private
   selectedprinter:TRpPrinterSelect;
   FOrientation:TRpOrientation;
   FPageWidth,FPageHeight:integer;
   FLinesPerInch:Currency;
   FLines:array of TRpPrintLine;
   FLineInfo:array of TRpLineInfo;
   FLineInfoMaxItems:integer;
   FLineInfoCount:integer;
   FOemConvert:Boolean;
   FPlainText:Boolean;
   escapecodes:array [Low(TPrinterRawOp)..High(TPrinterRawOp)] of String;
   allowedsizes:array [Low(TRpFontStep)..High(TRpFontStep)] of Boolean;
   PageQt:Integer;
   FPrinterDriver:TRpPrinterEscapeStyle;
   FPrinterDriverName:String;
   FForceDriverName:String;
   function GetLineIndex(posy:integer):integer;
   procedure RepProgress(Sender:TRpReport;var docancel:boolean);
   procedure RecalcSize;
   procedure CalculateTextExtent(text:WideString;var Rect:TRect;
    WordBreak:Boolean;singleline:Boolean;fontstep:TRpFontStep);
   procedure NewLineInfo(info:TRpLineInfo);
   procedure TextRect(ARect: TRect; Text: string;
                       Alignment: integer; Clipping: boolean;
                       Wordbreak:boolean;RightToLeft:Boolean;fontstep:TRpFontStep;fontstyle:integer);
   procedure DoTextOut(X, Y: Integer; const Text: string;LineWidth:Integer;
    FontStep:TRpFontStep;RightToLeft:Boolean;fontstyle:integer);
   function GetColumnNumber(posx:integer;FontStep:TRpFontStep):integer;
   function GetBlankLine(FontStep:TRpFontStep):String;
   procedure WriteCurrentPage;
   function EnCodeLine(Line:TRpPrintLine;index:integer):String;
   function FindEscapeStep(FontStep:TRpFontStep):String;
   function FindEscapeStyle(fontstyle:integer;FontStep:TRpFontStep):String;
   function NearestFontStep(FontStep:TRpFontStep):TRpFontStep;
   procedure UpdatePrinterConfig;
   procedure FillEspcapes(FPrinterDriverName:String);
  public
   LoadOemConvert:Boolean;
   MemStream:TMemoryStream;
   constructor Create;
   destructor Destroy;override;
   procedure NewDocument(report:TrpMetafileReport;hardwarecopies:integer;
    hardwarecollate:boolean);stdcall;
   procedure EndDocument;stdcall;
   procedure AbortDocument;stdcall;
   procedure NewPage;stdcall;
   procedure EndPage;stdcall;
   procedure DrawObject(page:TRpMetaFilePage;obj:TRpMetaObject);stdcall;
   procedure DrawPage(apage:TRpMetaFilePage);stdcall;
   function AllowCopies:boolean;stdcall;
   function GetPageSize(var PageSizeQt:Integer):TPoint;stdcall;
   function SetPagesize(PagesizeQt:TPageSizeQt):TPoint;stdcall;
   procedure TextExtent(atext:TRpTextObject;var extent:TPoint);stdcall;
   procedure GraphicExtent(Stream:TMemoryStream;var extent:TPoint;dpi:integer);stdcall;
   procedure SetOrientation(Orientation:TRpOrientation);stdcall;
   procedure SelectPrinter(printerindex:TRpPrinterSelect);stdcall;
   function SupportsCopies(maxcopies:integer):boolean;stdcall;
   function SupportsCollation:boolean;stdcall;
   property LinesPerInch:Currency read FLinesPerInch write FLinesPerInch;
   property PlainText:Boolean read FPlainText write FPlainText default false;
   property OemConvert:Boolean read FOemConvert write FOemConvert default false;
   property ForceDriverName:String read FForceDriverName write FForceDriverName;
  end;


procedure SaveMetafileToText(metafile:TRpMetafileReport;
 Stream:TStream);
procedure SaveMetafileRangeToText(metafile:TRpMetafileReport;
 allpages:boolean;frompage,topage,copies:integer;Stream:TStream);

function PrintReportToText(report:TRpReport;Caption:string;progress:boolean;
     allpages:boolean;frompage,topage,copies:integer;
     filename:string;collate:boolean;oemconvert:boolean;forcedrivername:String):Boolean;

function PrintReportToStream(report:TRpReport;Caption:string;progress:boolean;
     allpages:boolean;frompage,topage,copies:integer;
     stream:TStream;collate:Boolean;oemconvert:boolean;forcedrivername:string):Boolean;


implementation

uses Math;

const
 AlignmentFlags_SingleLine=64;
 AlignmentFlags_AlignHCenter = 4 { $4 };
 AlignmentFlags_AlignTop = 8 { $8 };
 AlignmentFlags_AlignBottom = 16 { $10 };
 AlignmentFlags_AlignVCenter = 32 { $20 };
 AlignmentFlags_AlignLeft = 1 { $1 };
 AlignmentFlags_AlignRight = 2 { $2 };

type
  TPageWidthHeight = record
    Width: Integer;
    Height: Integer;
  end;

const
  PageSizeArray: array[0..30] of TPageWidthHeight =
    (
      (Width: 8268; Height: 11693),  // psA4
      (Width: 7165; Height: 10118),  // psB5
      (Width: 8500; Height: 11000),  // psLetter
      (Width: 8500; Height: 14000),  // psLegal
      (Width: 7500; Height: 10000),  // psExecutive
      (Width: 33110; Height: 46811), // psA0
      (Width: 23386; Height: 33110), // psA1
      (Width: 16535; Height: 23386), // psA2
      (Width: 11693; Height: 16535), // psA3
      (Width: 5827; Height: 8268),   // psA5
      (Width: 4134; Height: 5827),   // psA6
      (Width: 2913; Height: 4134),   // psA7
      (Width: 2047; Height: 2913),   // psA8
      (Width: 1457; Height: 2047),   // psA9
      (Width: 40551; Height: 57323), // psB0
      (Width: 28661; Height: 40551), // psB1
      (Width: 1260; Height: 1772),   // psB10
      (Width: 20276; Height: 28661), // psB2
      (Width: 14331; Height: 20276), // psB3
      (Width: 10118; Height: 14331), // psB4
      (Width: 5039; Height: 7165),   // psB6
      (Width: 3583; Height: 5039),   // psB7
      (Width: 2520; Height: 3583),   // psB8
      (Width: 1772; Height: 2520),   // psB9
      (Width: 6417; Height: 9016),   // psC5E
      (Width: 4125; Height: 9500),   // psComm10E
      (Width: 4331; Height: 8661),   // psDLE
      (Width: 8250; Height: 13000),  // psFolio
      (Width: 17000; Height: 11000), // psLedger
      (Width: 11000; Height: 17000), // psTabloid
      (Width: -1; Height: -1)        // psNPageSize
    );



constructor TRpTextDriver.Create;
var
 i:TPrinterRawOp;
 j:TRpFontStep;
begin
 LoadOemConvert:=true;
 PageQt:=0;
 FPageWidth:= 11904;
 FPageHeight:= 16836;
 MemStream:=TMemoryStream.Create;
 FLinesPerInch:=DEFAULT_LINESPERINCH;
 selectedprinter:=pRpDefaultPrinter;
 SetLength(FLineInfo,CONS_MINLINEINFOITEMS);
 FLineInfoMaxItems:=CONS_MINLINEINFOITEMS;
 for i:=Low(TPrinterRawOp) to High(TPrinterRawOp) do
 begin
  escapecodes[i]:='';
 end;
 for j:=Low(TRpFontStep) to High(TRpFontStep) do
 begin
  i:=TPrinterRawOp(Ord(rpescape20cpi)+Ord(j));
  allowedsizes[j]:=Length(escapecodes[i])>0;
 end;
 RecalcSize;
 FPrinterDriverName:='EPSON';
end;

procedure FreeObjects(FLines:TStringList);
var
 i:integer;
begin
 for i:=0 to FLines.Count-1 do
 begin
  FLines.Objects[i].free;
 end;
end;

destructor TRpTextDriver.Destroy;
var
 i:integer;
begin
 MemStream.Free;
 for i:=0 to high(FLines) do
 begin
  FreeObjects(FLines[i].Attributes);
  FLines[i].Attributes.free;
  FLines[i].Attributes:=nil;
 end;
 SetLength(FLines,0);
end;

function TRpTextDriver.NearestFontStep(FontStep:TRpFontStep):TRpFontStep;
var
 maxallowed:TRpFontStep;
 i:TRpFontStep;
begin
 maxallowed:=rpcpi10;
 for i:=Low(TRpFontStep) to High(TRpFontStep) do
 begin
  if allowedsizes[i] then
  begin
   maxallowed:=i;
   break;
  end;
 end;
 Result:=maxallowed;
 i:=FontStep;
 while i>maxallowed do
 begin
  if allowedsizes[i] then
  begin
   Result:=i;
   break;
  end;
  dec(i);
 end;
end;


function TRpTextDriver.SupportsCollation:boolean;
begin
 Result:=false;
end;

function TRpTextDriver.SupportsCopies(maxcopies:integer):boolean;
begin
 Result:=false;
end;

procedure TRpTextDriver.NewDocument(report:TrpMetafileReport;hardwarecopies:integer;
   hardwarecollate:boolean);stdcall;
begin
 MemStream.free;
 MemStream:=TMemoryStream.Create;
 FPageWidth:=report.CustomX;
 FPageHeight:=report.CustomY;
 selectedprinter:=report.PrinterSelect;
 UpdatePrinterConfig;
 if Length(escapecodes[rpescapeinitprinter])>0 then
  MemStream.Write(escapecodes[rpescapeinitprinter][1],Length(escapecodes[rpescapeinitprinter]));
end;

procedure TRpTextDriver.EndDocument;
begin
 // Write the last page and the tear off
 WriteCurrentPage;
 // Tear off
 if Length(escapecodes[rpescapetearoff])>0 then
  MemStream.Write(escapecodes[rpescapetearoff][1],Length(escapecodes[rpescapetearoff]));
 // MemStream.Write;
 MemStream.Seek(0,soFromBeginning);
end;

procedure TRpTextDriver.AbortDocument;
begin
 MemStream.free;
 MemStream:=TMemoryStream.Create;
end;

procedure TRpTextDriver.NewPage;
begin
 // Writes the page to the stream
 WriteCurrentPage;
 // Reinitialize the page
 RecalcSize;
end;


procedure TRpTextDriver.TextExtent(atext:TRpTextObject;var extent:TPoint);
var
 singleline:boolean;
 rect:TRect;
 fontstep:TRpFontStep;
begin
 if atext.FontRotation<>0 then
  exit;
 if atext.CutText then
  exit;
 // single line
 singleline:=(atext.Alignment AND AlignmentFlags_SingleLine)>0;
 fontstep:=FontSizeToStep(atext.FontSize,atext.PrintStep);

 Rect.Left:=0;
 Rect.Top:=0;
 Rect.Bottom:=0;
 Rect.Right:=extent.X;
 CalculateTextExtent(atext.Text,Rect,atext.WordWrap,singleline,fontstep);

 extent.X:=Rect.Right;
 extent.Y:=Rect.Bottom;

end;


procedure TRpTextDriver.EndPage;
begin
 // Nothing to do
end;


function TRpTextDriver.GetLineIndex(posy:integer):integer;
var
 amax:integer;
begin
 Result:=0;
 if FPageheight<=0 then
  exit;
 aMax:=High(Flines);
 Result:=Round(posy/FPageheight*amax);
 if Result<0 then
  Result:=0;
 if Result>aMax then
  Result:=aMax;
end;

procedure TRpTextDriver.DrawObject(page:TRpMetaFilePage;obj:TRpMetaObject);
var
 posx,posy:integer;
 rec:TRect;
 aalign:integer;
 astring:String;
 fontstep:TrpFontStep;
begin
 posx:=obj.Left;
 posy:=obj.Top;
 // only text is supported
 case obj.Metatype of
  rpMetaText:
   begin
    aalign:=obj.Alignment;
    rec.Left:=posx;
    rec.Top:=posy;
    rec.Right:=posx+round(obj.Width);
    rec.Bottom:=posy+round(obj.Height);
    astring:=page.GetText(Obj);
    fontstep:=FontSizeToStep(obj.FontSize,obj.PrintStep);
    TextRect(rec,astring,aalign,obj.cuttext,
    obj.WordWrap,obj.RightToLeft,fontstep,obj.FontStyle);
   end;
 end;
end;

procedure TRpTextDriver.DrawPage(apage:TRpMetaFilePage);
var
 j:integer;
begin
 for j:=0 to apage.ObjectCount-1 do
 begin
  DrawObject(apage,apage.Objects[j]);
 end;
end;

function TRpTextDriver.AllowCopies:boolean;
begin
 Result:=false;
end;

function TRpTextDriver.GetPageSize(var PageSizeQt:Integer):TPoint;
begin
 PageSizeQt:=PageQt;
 Result.X:=FPageWidth;
 Result.Y:=FPageHeight;
end;

function TRpTextDriver.SetPagesize(PagesizeQt:TPageSizeQt):TPoint;stdcall;
var
 newwidth,newheight:integer;
begin
 // Sets the page size for the pdf file, first if it's a qt page
 PageQt:=PageSizeQt.indexqt;
 if PagesizeQt.Custom then
 begin
  PageQt:=-1;
  newwidth:=PagesizeQt.CustomWidth;
  newheight:=PagesizeQt.CustomHeight;
 end
 else
 begin
  newWidth:=Round(PageSizeArray[PagesizeQt.Indexqt].Width/1000*TWIPS_PER_INCHESS);
  newheight:=Round(PageSizeArray[PagesizeQt.Indexqt].Height/1000*TWIPS_PER_INCHESS);
 end;
 if FOrientation=rpOrientationLandscape then
 begin
  FPageWidth:=NewHeight;
  FPageHeight:=NewWidth;
 end
 else
 begin
  FPageWidth:=NewWidth;
  FPageHeight:=NewHeight;
 end;
 Result.X:=FPageWidth;
 Result.Y:=FPageHeight;
 RecalcSize;
end;

procedure TRpTextDriver.RecalcSize;
var
 i:integer;
 numberoflines:integer;
begin
 for i:=0 to high(FLines) do
 begin
  FreeObjects(FLines[i].Attributes);
  FLines[i].Attributes.free;
  FLines[i].Attributes:=nil;
 end;
 if LinesPerInch<0 then
  Raise Exception.Create(SRpLinesPerInchIncorrect);
 numberoflines:=Round(twipstoinchess(FPageHeight)*FLinesPerInch);
 SetLength(FLines,numberoflines);
 for i:=0 to high(FLines) do
 begin
  FLines[i].FontStep:=rpcpi10;
  FLines[i].Value:='';
  FLines[i].Attributes:=TStringList.Create;
  FLines[i].Attributes.Sorted:=true;
 end;
end;

procedure TRpTextDriver.SetOrientation(Orientation:TRpOrientation);
var
 atemp:Integer;
begin
 if Orientation=FOrientation then
  exit;
 if Orientation=rpOrientationDefault then
  exit;
 if Orientation=rpOrientationPortrait then
 begin
  FOrientation:=Orientation;
 end
 else
 begin
  atemp:=FPageWidth;
  FPageWidth:=FPageHeight;
  FPageHeight:=atemp;
  FOrientation:=Orientation;
 end;
end;


procedure TRpTextDriver.RepProgress(Sender:TRpReport;var docancel:boolean);
begin
{$IFDEF USEVARIANTS}
 WriteLn(SRpRecordCount+' '+IntToStr(Sender.CurrentSubReportIndex)
  +':'+SRpPage+':'+FormatFloat('#########,####',Sender.PageNum)+'-'+
  FormatFloat('#########,####',Sender.RecordCount));
{$ELSE}
 WriteLn(String(SRpRecordCount+' '+IntToStr(Sender.CurrentSubReportIndex)
  +':'+SRpPage+':'+FormatFloat('#########,####',Sender.PageNum)+'-'+
  FormatFloat('#########,####',Sender.RecordCount)));
{$ENDIF}

end;


function PrintReportToStream(report:TRpReport;Caption:string;progress:boolean;
     allpages:boolean;frompage,topage,copies:integer;
     stream:TStream;collate:Boolean;oemconvert:boolean;forcedrivername:string):Boolean;
var
 TextDriver:TRpTextDriver;
 aTextDriver:IRpPrintDriver;
 oldprogres:TRpProgressEvent;
begin
 TextDriver:=TRpTextDriver.Create;
 TextDriver.OemConvert:=oemconvert;
 TextDriver.LoadOemConvert:=false;
 TextDriver.ForceDriverName:=Trim(forcedrivername);
 aTextDriver:=TextDriver;
 // If report progress must print progress
 oldprogres:=report.OnProgress;
 try
  if progress then
   report.OnProgress:=TextDriver.RepProgress;
  report.PrintRange(aTextDriver,allpages,frompage,topage,copies,collate);
 finally
  report.OnProgress:=oldprogres;
 end;
 Stream.Write(TextDriver.MemStream.Memory^,TextDriver.MemStream.Size);
 Result:=True;
end;


function PrintReportToText(report:TRpReport;Caption:string;progress:boolean;
     allpages:boolean;frompage,topage,copies:integer;
     filename:string;collate:Boolean;oemconvert:boolean;forcedrivername:string):Boolean;
var
 TextDriver:TRpTextDriver;
 aTextDriver:IRpPrintDriver;
 oldprogres:TRpProgressEvent;
begin
 if Length(Trim(filename))<0 then
  Raise Exception.Create(SRpNoFileNameProvided+':TXT');
 TextDriver:=TRpTextDriver.Create;
 TextDriver.LoadOemConvert:=false;
 TextDriver.OemConvert:=oemconvert;
 TextDriver.ForceDriverName:=Trim(forcedrivername);
 aTextDriver:=TextDriver;
 // If report progress must print progress
 oldprogres:=report.OnProgress;
 try
  if progress then
   report.OnProgress:=TextDriver.RepProgress;
  report.PrintRange(aTextDriver,allpages,frompage,topage,copies,collate);
 finally
  report.OnProgress:=oldprogres;
 end;
 TextDriver.MemStream.SaveToFile(filename);
 Result:=True;
end;

procedure TRpTextDriver.FillEspcapes(FPrinterDriverName:String);
var
 i:TPrinterRawOp;
begin
 for i:=Low(TPrinterRawOp) to High(TPrinterRawOp) do
 begin
  escapecodes[i]:='';
 end;
 FPrinterDriverName:=UpperCase(FPrinterDriverName);
 if FPrinterDriverName='EPSON' then
 begin
  // Init Printer-Line spacing to 1/6 - Draft mode
  escapecodes[rpescapeinitprinter]:=#27+#64+#27+'2'+#27+'x'+#0;
  escapecodes[rpescapelinefeed]:=#13+#10;
//  escapecodes[rpescapeformfeed]:=#12;
  escapecodes[rpescapebold]:=#27+'E';
  escapecodes[rpescapeunderline]:=#27+#45+#1;
  escapecodes[rpescapeitalic]:=#27+'4';
  // Underline off-Bold off-Italic off
  escapecodes[rpescapenormal]:=#27+#45+#0+
   #27+'F'+#27+'5';
  escapecodes[rpescape5cpi]:=#27+'P'+#27+'W'+#1+#18;
  escapecodes[rpescape6cpi]:=#27+'M'+#27+'W'+#1+#18;
  escapecodes[rpescape10cpi]:=#27+'P'+#27+'W'+#0+#18;
  escapecodes[rpescape12cpi]:=#27+'M'+#27+'W'+#0+#18;
  // 15 cpi not supported in LX models
//  escapecodes[rpescape15cpi]:=#27+'g'+#27+'W'+#0;
  escapecodes[rpescape17cpi]:=#27+'P'+#27+'W'+#0+#15;
  escapecodes[rpescape20cpi]:=#27+'M'+#27+'W'+#0+#15;

  // Open drawer
  escapecodes[rpescapepulse]:=#27+#112+#0+#100+#100;
 end
 else
 if FPrinterDriverName='EPSON-QUALITY' then
 begin
  // Init Printer-Line spacing to 1/6 - Draft mode
  escapecodes[rpescapeinitprinter]:=#27+#64+#27+'2'+#27+'x'+#1;
  escapecodes[rpescapelinefeed]:=#13+#10;
//  escapecodes[rpescapeformfeed]:=#12;
  escapecodes[rpescapebold]:=#27+'E';
  escapecodes[rpescapeunderline]:=#27+#45+#1;
  escapecodes[rpescapeitalic]:=#27+'4';
  // Underline off-Bold off-Italic off
  escapecodes[rpescapenormal]:=#27+#45+#0+
   #27+'F'+#27+'5';
  escapecodes[rpescape5cpi]:=#27+'P'+#27+'W'+#1+#18;
  escapecodes[rpescape6cpi]:=#27+'M'+#27+'W'+#1+#18;
  escapecodes[rpescape10cpi]:=#27+'P'+#27+'W'+#0+#18;
  escapecodes[rpescape12cpi]:=#27+'M'+#27+'W'+#0+#18;
  // 15 cpi not supported in LX models
//  escapecodes[rpescape15cpi]:=#27+'g'+#27+'W'+#0;
  escapecodes[rpescape17cpi]:=#27+'P'+#27+'W'+#0+#15;
  escapecodes[rpescape20cpi]:=#27+'M'+#27+'W'+#0+#15;

  // Open drawer
  escapecodes[rpescapepulse]:=#27+#112+#0+#100+#100;
 end
 else
 if FPrinterDriverName='EPSONTM88II' then
 begin
  // Init Printer-Line spacing to 1/6
  escapecodes[rpescapeinitprinter]:=#27+#64+#27+#50;
  escapecodes[rpescapelinefeed]:=#13+#10;
//  escapecodes[rpescapeformfeed]:=#12;
  escapecodes[rpescapebold]:=#27+#69+#1;
  escapecodes[rpescapeunderline]:=#27+#45+#1;
  // Underline off-Bold off-Italic off
  escapecodes[rpescapenormal]:=#27+#45+#0+#27+#69+#0;
  // Open drawer
  escapecodes[rpescapepulse]:='#27#112#0#100#100';
 end;
end;


procedure TRpTextDriver.UpdatePrinterConfig;
var
 i:TPrinterRawOp;
 j:TRpFontStep;
begin
 FForceDriverName:=Trim(FForceDriverName);
 if Length(FForceDriverName)<0 then
 begin
  FPrinterDriver:=GetPrinterEscapeStyleOption(selectedprinter);
  FPlainText:=FPrinterDriver in [rpPrinterDefault,rpPrinterPlain];
 end
 else
 begin
  if UpperCase(FForceDriverName)='PLAIN' then
  begin
   FPrinterDriver:=rpPrinterPlain;
   FPlainText:=true;
  end
  else
  begin
   FPrinterDriver:=rpPrinterDatabase;
   FPlainText:=false;
   FPrinterDriverName:=FForceDriverName;
  end;
 end;
 if FPlainText then
 begin
  for i:=Low(TPrinterRawOp) to High(TPrinterRawOp) do
  begin
   escapecodes[i]:='';
  end;
 end
 else
 begin
  if FPrinterDriver=rpPrinterCustom then
  begin
   for i:=Low(TPrinterRawOp) to High(TPrinterRawOp) do
   begin
    escapecodes[i]:=GetPrinterRawOp(selectedprinter,i);
   end;
  end
  else
  begin
   FPrinterDriverName:=GetPrinterEscapeStyleDriver(selectedprinter);
   FillEspcapes(FPrinterDriverName);
  end;
 end;
 for j:=Low(TRpFontStep) to High(TRpFontStep) do
 begin
  allowedsizes[j]:=Length(escapecodes[TPrinterRawOp(Ord(rpescape20cpi)+Ord(j))])>0;
 end;
 If LoadOemConvert then
 begin
  oemconvert:=GetPrinterOemConvertOption(selectedprinter);
 end;
end;

procedure TRpTextDriver.SelectPrinter(printerindex:TRpPrinterSelect);stdcall;
var
 i:TPrinterRawOp;
begin
 selectedprinter:=printerindex;
 UpdatePrinterConfig;
 for i:=Low(TPrinterRawOp) to High(TPrinterRawOp) do
 begin
  escapecodes[i]:=GetPrinterRawOp(selectedprinter,i);
 end;
end;

procedure TRpTextDriver.GraphicExtent(Stream:TMemoryStream;var extent:TPoint;dpi:integer);
begin
 // Graphics not supported in text mode
 extent.X:=0;
 extent.Y:=0;
end;

procedure SaveMetafileRangeToText(metafile:TRpMetafileReport;
 allpages:boolean;frompage,topage,copies:integer;Stream:TStream);
var
 adriver:TRpTextDriver;
 i:integer;
 j:integer;
begin
 if allpages then
 begin
  frompage:=0;
  topage:=metafile.PageCount-1;
 end
 else
 begin
  frompage:=frompage-1;
  topage:=topage-1;
  if topage>metafile.PageCount-1 then
   topage:=metafile.PageCount-1;
 end;
 if copies<0 then
  copies:=1;
 adriver:=TRpTextDriver.Create;
 adriver.NewDocument(metafile,1,false);
 try
  for i:=frompage to topage do
  begin
   for j:=0 to copies-1 do
   begin
    adriver.DrawPage(metafile.Pages[i]);
    if ((i<metafile.PageCount-1) or (j<copies-1)) then
    begin
     adriver.NewPage;
    end;
   end;
  end;
  adriver.EndDocument;
 except
  adriver.AbortDocument;
  raise;
 end;
 Stream.Write(adriver.MemStream.Memory^,adriver.MemStream.Size);
end;


procedure SaveMetafileToText(metafile:TRpMetafileReport;
 Stream:TStream);
begin
 SaveMetafileRangeToText(metafile,false,1,99999,1,Stream);
end;

procedure SaveMetafileToTextStream(metafile:TRpMetafileReport;
 Stream:TStream;compressed:boolean);
var
 adriver:TRpTextDriver;
 i:integer;
begin
 adriver:=TRpTextDriver.Create;
 adriver.NewDocument(metafile,1,false);
 try
  for i:=0 to metafile.PageCount-1 do
  begin
   adriver.DrawPage(metafile.Pages[i]);
   if i<metafile.PageCount-1 then
    adriver.NewPage;
  end;
  adriver.EndDocument;
  adriver.MemStream.Seek(0,soFromBeginning);
  Stream.Write(adriver.MemStream.Memory^,adriver.MemStream.Size);
 except
  adriver.AbortDocument;
  raise;
 end;
end;

function steptotwips(step:TRpFontStep):double;
begin
  Result:=TWIPS_PER_INCHESS/10;
 case step of
  rpcpi20:
   Result:=TWIPS_PER_INCHESS/20;
  rpcpi17:
   Result:=TWIPS_PER_INCHESS/17.14;
  rpcpi15:
   Result:=TWIPS_PER_INCHESS/15;
  rpcpi12:
   Result:=TWIPS_PER_INCHESS/12;
  rpcpi10:
   Result:=TWIPS_PER_INCHESS/10;
  rpcpi6:
   Result:=TWIPS_PER_INCHESS/6;
  rpcpi5:
   Result:=TWIPS_PER_INCHESS/5;
 end;
end;

function CalcCharWidth(charcode:char;step:TRpFOntStep):double;
begin
 if charcode in [#0,#13,#10] then
 begin
  Result:=0;
  exit;
 end;
 Result:=steptotwips(step);
end;

procedure TRpTextDriver.CalculateTextExtent(text:WideString;var Rect:TRect;
    WordBreak:Boolean;singleline:Boolean;fontstep:TRpFontStep);
var
 astring:string;
 i:integer;
 asize:double;
 arec:TRect;
 position:integer;
 info:TRpLineInfo;
 maxwidth:double;
 newsize:double;
 recwidth:double;
 linebreakpos:integer;
 nextline:boolean;
 alastsize:double;
 lockspace:boolean;
begin
 // Text extent for the simple strings, wide strings not supported
 fontstep:=NearestFontStep(fontstep);
 astring:=Text;
 arec:=Rect;
 arec.Left:=0;
 arec.Top:=0;
 arec.Bottom:=0;

 asize:=0;

 FLineInfoCount:=0;
 position:=1;
 linebreakpos:=0;
 maxwidth:=0;
 recwidth:=(rect.Right-rect.Left);
 nextline:=false;
 i:=1;
 alastsize:=0;
 lockspace:=false;
 while i<=Length(astring) do
 begin
  newsize:=CalcCharWidth(astring[i],fontstep);
  if (Not (astring[i] in [' ',#10,#13])) then
   lockspace:=false;
  if wordbreak then
  begin
   if asize+newsize>recwidth then
   begin
    if linebreakpos>0 then
    begin
     i:=linebreakpos;
     nextline:=true;
     asize:=alastsize;
     linebreakpos:=0;
    end;
   end
   else
   begin
    if astring[i] in ['.',',','-',' '] then
    begin
     linebreakpos:=i;
     if astring[i]=' ' then
     begin
      if not lockspace then
      begin
       alastsize:=asize;
       lockspace:=true;
      end;
     end
     else
     begin
      alastsize:=asize+newsize;
     end;
    end;
    asize:=asize+newsize;
   end;
  end
  else
  begin
   asize:=asize+newsize;
  end;
  if not singleline then
   if astring[i]=#10 then
    nextline:=true;
  if asize>maxwidth then
   maxwidth:=asize;
  if nextline then
  begin
   nextline:=false;
   info.Position:=position;
   info.Size:=i-position+1;
   info.Width:=Round((asize));
   info.height:=Round((TWIPS_PER_INCHESS/FLinesPerInch));
   info.TopPos:=arec.Bottom;
   arec.Bottom:=arec.Bottom+info.height;
   asize:=0;
   position:=i+1;
   NewLineInfo(info);
  end;
  inc(i);
 end;
 arec.Right:=Round((maxwidth+1));
 if Position<=Length(astring) then
 begin
  info.Position:=position;
  info.Size:=Length(astring)-position+1;
  info.Width:=Round((asize+1));
  info.height:=Round((TWIPS_PER_INCHESS/FLinesPerInch));
  info.TopPos:=arec.Bottom;
  arec.Bottom:=arec.Bottom+info.height;
  NewLineInfo(info);
 end;
 rect:=arec;
end;

procedure TRpTextDriver.NewLineInfo(info:TRpLineInfo);
begin
 if FLineInfoMaxItems<=FLineInfoCount-1 then
 begin
  SetLength(FLineInfo,FLineInfoMaxItems*2);
  FLineInfoMaxItems:=FLineInfoMaxItems*2;
 end;
 FLineInfo[FLineInfoCount]:=info;
 inc(FLineInfoCount);
end;

procedure TRpTextDriver.TextRect(ARect: TRect; Text: string;
                       Alignment: integer; Clipping: boolean;Wordbreak:boolean;
                       RightToLeft:Boolean;fontstep:TRpFontStep;fontstyle:integer);
var
 recsize:TRect;
 i:integer;
 posx,posY:integer;
 singleline:boolean;
 astring:String;
begin
 singleline:=(Alignment AND AlignmentFlags_SingleLine)>0;
 if singleline then
  wordbreak:=false;
 // Calculates text extent and apply alignment
 recsize:=ARect;
 CalculateTextExtent(Text,recsize,wordbreak,singleline,fontstep);
 // Align bottom or center
 PosY:=ARect.Top;
 if (AlignMent AND AlignmentFlags_AlignBottom)>0 then
 begin
  PosY:=ARect.Bottom-recsize.bottom;
 end;
 if (AlignMent AND AlignmentFlags_AlignVCenter)>0 then
 begin
  PosY:=ARect.Top+(((ARect.Bottom-ARect.Top)-recsize.Bottom) div 2);
 end;
 for i:=0 to FLineInfoCount-1 do
 begin
  posX:=ARect.Left;
  // Aligns horz.
  if  ((Alignment AND AlignmentFlags_AlignRight)>0) then
  begin
   // recsize.right contains the width of the full text
   PosX:=ARect.Right-FLineInfo[i].Width;
  end;
  // Aligns horz.
  if (Alignment AND AlignmentFlags_AlignHCenter)>0 then
  begin
   PosX:=ARect.Left+(((Arect.Right-Arect.Left)-FLineInfo[i].Width) div 2);
  end;
  astring:=Copy(Text,FLineInfo[i].Position,FLineInfo[i].Size);
  DoTextOut(PosX,PosY+FLineInfo[i].TopPos,astring,FLineInfo[i].Width,fontstep,RightToLeft,fontstyle);
 end;
end;


function TRpTextDriver.GetColumnNumber(posx:integer;FontStep:TRpFontStep):integer;
begin
 if FPlainText then
  FontStep:=rpcpi10;
 Result:=Round(posx/steptotwips(fontstep));
end;

function TRpTextDriver.GetBlankLine(FontStep:TRpFontStep):String;
var
 charcount:integer;
begin
 if FPlainText then
  FontStep:=rpcpi10;
 charcount:=Round(FPageWidth/steptotwips(fontstep));
 SetLength(Result,charcount);
 FillChar(Result[1],charcount,' ');
end;

procedure TRpTextDriver.DoTextOut(X, Y: Integer; const Text: string;LineWidth:Integer;
  FontStep:TRpFontStep;RightToLeft:Boolean;fontstyle:integer);
var
 astring,atpos:String;
 lineindex,index:integer;
 columnnumber:integer;
 toposition:integer;
 i:integer;
 attr:TRpAttribObject;
begin
 astring:=Text;
 if RightToLeft then
 begin
  astring:=DoReverseString(astring);
 end;
 lineindex:=getlineindex(Y);
 if FLines[lineindex].Attributes.Count<1 then
 begin
  FLines[lineindex].FontStep:=NearestFontStep(FontStep);
  FLines[lineindex].Value:=GetBlankLine(FLines[lineindex].FontStep);
 end;
 columnnumber:=GetColumnNumber(X,FLines[lineindex].FontStep);
 atpos:=FormatFloat('00000',columnnumber+1);
 index:=FLines[lineindex].Attributes.IndexOf(atpos);
 if index<0 then
 begin
  attr:=TRpAttribObject.Create;
  attr.FontStyle:=fontStyle;
  attr.Position:=columnnumber+1;
  attr.size:=0;
  FLines[lineindex].Attributes.AddObject(atpos,attr);
 end
 else
 begin
  attr:=TRpAttribObject(FLines[lineindex].Attributes.Objects[index]);
 end;
 toposition:=Length(astring);
 if columnnumber+toposition>=Length(FLines[lineindex].Value) then
  toposition:=Length(FLines[lineindex].Value)-columnnumber;
 if toposition>attr.size then
  attr.size:=toposition;
 for i:=1 to toposition do
 begin
  FLines[lineindex].Value[columnnumber+i]:=astring[i];
 end;
end;


function TRpTextDriver.FindEscapeStep(FontStep:TRpFontStep):String;
begin
 if FPlainText then
 begin
  Result:='';
  exit;
 end;
 Result:=escapecodes[rpescape10cpi];
 case FontStep of
  rpcpi20:
   Result:=escapecodes[rpescape10cpi];
  rpcpi17:
   Result:=escapecodes[rpescape17cpi];
  rpcpi15:
   Result:=escapecodes[rpescape15cpi];
  rpcpi12:
   Result:=escapecodes[rpescape12cpi];
  rpcpi10:
   Result:=escapecodes[rpescape10cpi];
  rpcpi6:
   Result:=escapecodes[rpescape6cpi];
  rpcpi5:
   Result:=escapecodes[rpescape5cpi];
 end;
end;

function TRpTextDriver.FindEscapeStyle(fontstyle:integer;FontStep:TRpFontStep):String;
begin
 Result:='';
 if FPlainText then
  exit;
 Result:=escapecodes[rpescapenormal];
 if (fontstyle and 1)>0 then
  Result:=Result+escapecodes[rpescapebold];
 if (fontstyle and (1 shl 1))>0 then
  Result:=Result+escapecodes[rpescapeitalic];
 if (fontstyle and (1 shl 2))>0 then
  Result:=Result+escapecodes[rpescapeunderline];
 if (fontstyle and (1 shl 3))>0 then
  Result:=Result+escapecodes[rpescapestrikeout];
end;



function TRpTextDriver.EnCodeLine(Line:TRpPrintLine;index:integer):String;
var
 attr:TRpAttribObject;
 encoded:string;
 i:integer;
 atposition:integer;
 atindex:integer;
 res:PChar;
 wasunderline:boolean;
 currentcount:integer;
begin
{$IFDEF MSWINDOWS}
 if OemConvert then
 begin
  res:=AllocMem(Length(Line.Value)+1);
  StrPCopy(res,Line.Value);
  CharToOEM(PChar(res),PChar(res));
  Line.Value:=StrPas(res);
  FreeMem(res);
 end;
{$ENDIF}
 encoded:='';
 // Only set size if previos size is different
// if index>0 then
// begin
//  if Line.FontStep<>FLines[index-1].FontStep then
//   encoded:=encoded+FindEscapeStep(Line.FontStep);
// end
// else
  encoded:=encoded+FindEscapeStep(Line.FontStep);
 if Line.Attributes.Count<1 then
 begin
  Result:=encoded+escapecodes[rpescapelinefeed];
  exit;
 end;
 encoded:=encoded+escapecodes[rpescapenormal];
 atindex:=0;
 i:=1;
 atposition:=StrToInt(Line.Attributes.Strings[atindex]);
 while ((i<=Length(Line.Value)) and (i<atposition)) do
 begin
  encoded:=encoded+Line.Value[i];
  inc(i);
 end;
 while (i<=Length(Line.Value)) do
 begin
  attr:=TRpAttribObject(Line.Attributes.Objects[atindex]);
  wasunderline:=(attr.fontstyle and (1 shl 2))>0;
  currentcount:=1;
  encoded:=encoded+FindEscapeStyle(attr.fontstyle,Line.FontStep);
  inc(atindex);
  if atindex<Line.Attributes.Count then
  begin
   atposition:=StrToInt(Line.Attributes.Strings[atindex]);
  end
  else
  begin
   atposition:=Length(Line.Value)+1;
  end;
  while ((i<=Length(Line.Value)) and (i<atposition)) do
  begin
   encoded:=encoded+Line.Value[i];
   inc(i);
   inc(currentcount);
   if wasunderline then
   begin
    if currentcount>attr.size then
    begin
     encoded:=encoded+FindEscapeStyle(attr.fontstyle and (Not (1 shl 2)),Line.FontStep);
     wasunderline:=false;
    end;
   end;
  end;
 end;
 While Length(encoded)>0 do
 begin
  if encoded[Length(encoded)]<>' ' then
   break
  else
   encoded:=Copy(encoded,1,Length(encoded)-1);
 end;
 encoded:=encoded+escapecodes[rpescapelinefeed];
 Result:=encoded;
end;

procedure TRpTextDriver.WriteCurrentPage;
var
 i:integer;
 codedstring:String;
begin
 for i:=0 to High(FLines) do
 begin
  codedstring:=EnCodeLine(FLines[i],i);
  if Length(codedstring)>0 then
   MemStream.Write(codedstring[1],Length(codedstring));
  codedstring:=escapecodes[rpescapeformfeed];
  if Length(codedstring)>0 then
   MemStream.Write(codedstring[1],Length(codedstring));
 end;
end;

end.
