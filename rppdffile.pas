{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       TRppdffile                                      }
{       PDF Generator                                   }
{                                                       }
{       Code Base From Nishita's PDF Creation (TNPDF)   }
{       info@nishita.com                                }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       Converted to CLX and added lot functionality    }
{       and bug fixes                                   }
{       Changed names to borland coding style           }
{       Added Canvas object                             }
{                                                       }
{       Added:                                          }
{               -Font Color                             }
{               -Text parsing                           }
{               -Filled Regions, pen color and b.color  }
{               -Pen Style                              }
{               -Resolution 1440 p.p.i                  }
{               -Exact position for text...             }
{               -Text clipping                          }
{               -Ellipse, true Rectangle                }
{               -Text alignment                         }
{               -Multiline and wordbreak                }
{               -Underline and strokeout                }
{               -Type1 Font selection bold/italic       }
{                                                       }
{                                                       }
{       Still Missing:                                  }
{               -Brush Patterns                         }
{               -Bitmaps                                }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rppdffile;

interface

{$I rpconf.inc}

uses Classes,Sysutils,
{$IFDEF MSWINDOWS}
 Windows,
{$ENDIF}
{$IFDEF USEVARIANTS}
 Types,
{$ENDIF}
 rpmzlib,rpconsts,rptypes;


const
 PDF_HEADER:string='%PDF-1.4';
 CONS_PDFRES=72;
 CONS_MINLINEINFOITEMS=400;
 CONS_UNDERLINEWIDTH=0.1;
 CONS_SRIKEOUTWIDTH=0.05;
 CONS_UNDERLINEPOS=1.1;
 CONS_STRIKEOUTPOS=0.7;
type
 TRpPDFFont=class(TObject)
  public
   Name:TRpType1Font;
   Size:integer;
   Color:integer;
   Italic:Boolean;
   Underline:boolean;
   Bold:boolean;
   StrikeOut:boolean;
   constructor Create;
  end;

 TWinAnsiWidthsArray=array [32..255] of integer;
 PWinAnsiWidthsArray= ^TWinAnsiWidthsArray;

 TRpPDFFile=class;

 TRpLineInfo=record
  Position:integer;
  Size:integer;
  Width:integer;
  height:integer;
  TopPos:integer;
 end;

 TRpPDFCanvas=class(TObject)
  private
   FFont:TRpPDFFont;
   FFile:TRpPDFFile;
   FResolution:integer;
   FLineInfo:array of TRpLineInfo;
   FLineInfoMaxItems:integer;
   FLineInfoCount:integer;
   procedure NewLineInfo(info:TRpLineInfo);
   procedure SetDash;
   procedure SaveGraph;
   procedure RestoreGraph;
  public
   PenColor:integer;
   PenStyle:integer;
   PenWidth:integer;
   BrushColor:integer;
   BrushStyle:integer;
   function UnitsToTextX(Value:integer):string;
   function UnitsToTextY(Value:integer):string;
   function UnitsToTextText(Value:integer;FontSize:integer):string;
   procedure Line(x1,y1,x2,y2:Integer);
   procedure TextOut(X, Y: Integer; const Text: string;LineWidth,Rotation:integer);
   procedure TextRect(ARect: TRect; Text: string;
                       Alignment: integer; Clipping: boolean;
                       Wordbreak:boolean;Rotation:integer=0);
   procedure Rectangle(x1,y1,x2,y2:Integer);
//   procedure StretchDraw(rec:TRect;abitmap:TBitmap);
   procedure Ellipse(X1, Y1, X2, Y2: Integer);
   constructor Create(AFile:TRpPDFFile);
   destructor Destroy;override;
   procedure TextExtent(const Text:WideString;var Rect:TRect;wordbreak:boolean;
    singleline:boolean);
   function CalcCharWidth(charcode:char):double;
  public

   property Font:TRpPDFFont read FFOnt;
  end;

 TRpPDFFile=class(TComponent)
  private
   FCanvas:TRpPDFCanvas;
   FPrinting:Boolean;
   FCompressed:boolean;
   FFilename:string;
   FDocTitle:string;
   FDocAuthor:string;
   FDocCreator:string;
   FDocKeywords:string;
   FDocSubject:string;
   FDocProducer:string;
   FMainPDF:TMemoryStream;
   FStreamValid:boolean;
   FTempStream:TMemoryStream;
   FsTempStream:TMemoryStream;
   FPage:integer;
   FPages:TStringList;
   FObjectOffsets:TStringList;
   FObjectCount:integer;
   FObjectOffset:integer;
   FStreamSize1,FStreamSize2:LongInt;
   FOutlinesNum:integer;
   FFontCount:integer;
   FFontList:TStringList;
   FParentNum:integer;
   FImageCount:integer;
   FResourceNum,FCatalogNum:integer;
   FCurrentSetPageObject:integer;
   FCompressionStream:TCompressionStream;
   FResolution:integer;
   // Minimum page size in 72 dpi 18x18
   // Maximum page size in 72 dpi 14.400x14.400
   FPageWidth,FPageHeight:integer;
   function GetStream:TMemoryStream;
   procedure CheckPrinting;
   procedure AddToOffset(offset:LongInt);
   procedure StartStream;
   procedure EndStream;
   procedure SetOutLine;
   procedure SetFontType;
   procedure CreateFont(Subtype,BaseFont,Encoding:string);
   procedure SetPages;
   procedure SetPageObject;
   procedure SetArray;
   procedure SetCatalog;
   procedure SetXref;
   function GetOffsetNumber(offset:string):string;
   procedure SetResolution(Newres:integer);
 public
   procedure BeginDoc;
   procedure NewPage;
   procedure EndDoc;
   procedure AbortDoc;
   constructor Create(AOwner:TComponent);override;
   destructor Destroy;override;
   property Canvas:TRpPDFCanvas read FCanvas;
   property Printing:Boolean read FPrinting;
   property Stream:TMemoryStream read GetStream;
   property StreamValid:Boolean read FStreamValid;
  published
   // General properties
   property Compressed:boolean read FCompressed write FCompressed default true;
   property Filename:string read FFilename write FFilename;
   // Doc Info Props
   property DocTitle:string read FDocTitle write FDocTitle;
   property DocAuthor:string read FDocAuthor write FDocAuthor;
   property DocCreator:string read FDocCreator write FDocCreator;
   property DocKeywords:string read FDocKeywords write FDocKeywords;
   property DocSubject:string read FDocSubject write FDocSubject;
   property DocProducer:string read FDocProducer write FDocProducer;
   // Document physic
   property PageWidth:integer read FPageWidth write FPageWidth;
   property PageHeight:integer read FPageHeight write FPageHeight;
   property Resolution:integer read FResolution write SetResolution default 1440;
  end;



function PDFCompatibleText(astring:string):string;
function NumberToText(Value:double):string;


implementation


const
 AlignmentFlags_SingleLine=64;
 AlignmentFlags_AlignHCenter = 4 { $4 };
 AlignmentFlags_AlignTop = 8 { $8 };
 AlignmentFlags_AlignBottom = 16 { $10 };
 AlignmentFlags_AlignVCenter = 32 { $20 };
 AlignmentFlags_AlignLeft = 1 { $1 };
 AlignmentFlags_AlignRight = 2 { $2 };

// Font sizes (point 10)

var
  Helvetica_Widths: TWinAnsiWidthsArray = (
    278,278,355,556,556,889,667,191,333,333,389,584,278,333,
    278,278,556,556,556,556,556,556,556,556,556,556,278,278,584,584,
    584,556,1015,667,667,722,722,667,611,778,722,278,500,667,556,833,
    722,778,667,778,722,667,611,722,667,944,667,667,611,278,278,278,
    469,556,333,556,556,500,556,556,278,556,556,222,222,500,222,833,
    556,556,556,556,333,500,278,556,500,722,500,500,500,334,260,334,
    584,0,556,0,222,556,333,1000,556,556,333,1000,667,333,1000,0,
    611,0,0,222,222,333,333,350,556,1000,333,1000,500,333,944,0,
    500,667,0,333,556,556,556,556,260,556,333,737,370,556,584,0,
    737,333,400,584,333,333,333,556,537,278,333,333,365,556,834,834,
    834,611,667,667,667,667,667,667,1000,722,667,667,667,667,278,278,
    278,278,722,722,778,778,778,778,778,584,778,722,722,722,722,667,
    667,611,556,556,556,556,556,556,889,500,556,556,556,556,278,278,
    278,278,556,556,556,556,556,556,556,584,611,556,556,556,556,500,
    556,500);

 Default_Font_Width:integer=600;

 Helvetica_Bold_Widths: TWinAnsiWidthsArray = (
    278,333,474,556,556,889,722,238,333,333,389,584,278,333,
    278,278,556,556,556,556,556,556,556,556,556,556,333,333,584,584,
    584,611,975,722,722,722,722,667,611,778,722,278,556,722,611,833,
    722,778,667,778,722,667,611,722,667,944,667,667,611,333,278,333,
    584,556,333,556,611,556,611,556,333,611,611,278,278,556,278,889,
    611,611,611,611,389,556,333,611,556,778,556,556,500,389,280,389,
    584,0,556,0,278,556,500,1000,556,556,333,1000,667,333,1000,0,
    611,0,0,278,278,500,500,350,556,1000,333,1000,556,333,944,0,
    500,667,0,333,556,556,556,556,280,556,333,737,370,556,584,0,
    737,333,400,584,333,333,333,611,556,278,333,333,365,556,834,834,
    834,611,722,722,722,722,722,722,1000,722,667,667,667,667,278,278,
    278,278,722,722,778,778,778,778,778,584,778,722,722,722,722,667,
    667,611,556,556,556,556,556,556,889,556,556,556,556,556,278,278,
    278,278,611,611,611,611,611,611,611,584,611,611,611,611,611,556,
    611,556);

 Helvetica_Italic_Widths: TWinAnsiWidthsArray = (
    278,278,355,556,556,889,667,191,333,333,389,584,278,333,
    278,278,556,556,556,556,556,556,556,556,556,556,278,278,584,584,
    584,556,1015,667,667,722,722,667,611,778,722,278,500,667,556,833,
    722,778,667,778,722,667,611,722,667,944,667,667,611,278,278,278,
    469,556,333,556,556,500,556,556,278,556,556,222,222,500,222,833,
    556,556,556,556,333,500,278,556,500,722,500,500,500,334,260,334,
    584,0,556,0,222,556,333,1000,556,556,333,1000,667,333,1000,0,
    611,0,0,222,222,333,333,350,556,1000,333,1000,500,333,944,0,
    500,667,0,333,556,556,556,556,260,556,333,737,370,556,584,0,
    737,333,400,584,333,333,333,556,537,278,333,333,365,556,834,834,
    834,611,667,667,667,667,667,667,1000,722,667,667,667,667,278,278,
    278,278,722,722,778,778,778,778,778,584,778,722,722,722,722,667,
    667,611,556,556,556,556,556,556,889,500,556,556,556,556,278,278,
    278,278,556,556,556,556,556,556,556,584,611,556,556,556,556,500,
    556,500);

  Helvetica_BoldItalic_Widths: TWinAnsiWidthsArray = (
    278,333,474,556,556,889,722,238,333,333,389,584,278,333,
    278,278,556,556,556,556,556,556,556,556,556,556,333,333,584,584,
    584,611,975,722,722,722,722,667,611,778,722,278,556,722,611,833,
    722,778,667,778,722,667,611,722,667,944,667,667,611,333,278,333,
    584,556,333,556,611,556,611,556,333,611,611,278,278,556,278,889,
    611,611,611,611,389,556,333,611,556,778,556,556,500,389,280,389,
    584,0,556,0,278,556,500,1000,556,556,333,1000,667,333,1000,0,
    611,0,0,278,278,500,500,350,556,1000,333,1000,556,333,944,0,
    500,667,0,333,556,556,556,556,280,556,333,737,370,556,584,0,
    737,333,400,584,333,333,333,611,556,278,333,333,365,556,834,834,
    834,611,722,722,722,722,722,722,1000,722,667,667,667,667,278,278,
    278,278,722,722,778,778,778,778,778,584,778,722,722,722,722,667,
    667,611,556,556,556,556,556,556,889,556,556,556,556,556,278,278,
    278,278,611,611,611,611,611,611,611,584,611,611,611,611,611,556,
    611,556);

  TimesRoman_Widths: TWinAnsiWidthsArray = (
    250,333,408,500,500,833,778,180,333,333,500,564,250,333,
    250,278,500,500,500,500,500,500,500,500,500,500,278,278,564,564,
    564,444,921,722,667,667,722,611,556,722,722,333,389,722,611,889,
    722,722,556,722,667,556,611,722,722,944,722,722,611,333,278,333,
    469,500,333,444,500,444,500,444,333,500,500,278,278,500,278,778,
    500,500,500,500,333,389,278,500,500,722,500,500,444,480,200,480,
    541,0,500,0,333,500,444,1000,500,500,333,1000,556,333,889,0,
    611,0,0,333,333,444,444,350,500,1000,333,980,389,333,722,0,
    444,722,0,333,500,500,500,500,200,500,333,760,276,500,564,0,
    760,333,400,564,300,300,333,500,453,250,333,300,310,500,750,750,
    750,444,722,722,722,722,722,722,889,667,611,611,611,611,333,333,
    333,333,722,722,722,722,722,722,722,564,722,722,722,722,722,722,
    556,500,444,444,444,444,444,444,667,444,444,444,444,444,278,278,
    278,278,500,500,500,500,500,500,500,564,500,500,500,500,500,500,
    500,500);

  TimesRoman_Italic_Widths: TWinAnsiWidthsArray = (
    250,333,420,500,500,833,778,214,333,333,500,675,250,333,
    250,278,500,500,500,500,500,500,500,500,500,500,333,333,675,675,
    675,500,920,611,611,667,722,611,611,722,722,333,444,667,556,833,
    667,722,611,722,611,500,556,722,611,833,611,556,556,389,278,389,
    422,500,333,500,500,444,500,444,278,500,500,278,278,444,278,722,
    500,500,500,500,389,389,278,500,444,667,444,444,389,400,275,400,
    541,0,500,0,333,500,556,889,500,500,333,1000,500,333,944,0,
    556,0,0,333,333,556,556,350,500,889,333,980,389,333,667,0,
    389,556,0,389,500,500,500,500,275,500,333,760,276,500,675,0,
    760,333,400,675,300,300,333,500,523,250,333,300,310,500,750,750,
    750,500,611,611,611,611,611,611,889,667,611,611,611,611,333,333,
    333,333,722,667,722,722,722,722,722,675,722,722,722,722,722,556,
    611,500,500,500,500,500,500,500,667,444,444,444,444,444,278,278,
    278,278,500,500,500,500,500,500,500,675,500,500,500,500,500,444,
    500,444);

  TimesRoman_Bold_Widths: TWinAnsiWidthsArray = (
    250,333,555,500,500,1000,833,278,333,333,500,570,250,333,
    250,278,500,500,500,500,500,500,500,500,500,500,333,333,570,570,
    570,500,930,722,667,722,722,667,611,778,778,389,500,778,667,944,
    722,778,611,778,722,556,667,722,722,1000,722,722,667,333,278,333,
    581,500,333,500,556,444,556,444,333,500,556,278,333,556,278,833,
    556,500,556,556,444,389,333,556,500,722,500,500,444,394,220,394,
    520,0,500,0,333,500,500,1000,500,500,333,1000,556,333,1000,0,
    667,0,0,333,333,500,500,350,500,1000,333,1000,389,333,722,0,
    444,722,0,333,500,500,500,500,220,500,333,747,300,500,570,0,
    747,333,400,570,300,300,333,556,540,250,333,300,330,500,750,750,
    750,500,722,722,722,722,722,722,1000,722,667,667,667,667,389,389,
    389,389,722,722,778,778,778,778,778,570,778,722,722,722,722,722,
    611,556,500,500,500,500,500,500,722,444,444,444,444,444,278,278,
    278,278,500,556,500,500,500,500,500,570,500,556,556,556,556,500,
    556,500);

  TimesRoman_BoldItalic_Widths: TWinAnsiWidthsArray = (
    250,389,555,500,500,833,778,278,333,333,500,570,250,333,
    250,278,500,500,500,500,500,500,500,500,500,500,333,333,570,570,
    570,500,832,667,667,667,722,667,667,722,778,389,500,667,611,889,
    722,722,611,722,667,556,611,722,667,889,667,611,611,333,278,333,
    570,500,333,500,500,444,500,444,333,500,556,278,278,500,278,778,
    556,500,500,500,389,389,278,556,444,667,500,444,389,348,220,348,
    570,0,500,0,333,500,500,1000,500,500,333,1000,556,333,944,0,
    611,0,0,333,333,500,500,350,500,1000,333,1000,389,333,722,0,
    389,611,0,389,500,500,500,500,220,500,333,747,266,500,606,0,
    747,333,400,570,300,300,333,576,500,250,333,300,300,500,750,750,
    750,500,667,667,667,667,667,667,944,667,667,667,667,667,389,389,
    389,389,722,722,722,722,722,722,722,570,722,722,722,722,722,611,
    611,500,500,500,500,500,500,500,722,444,444,444,444,444,278,278,
    278,278,500,556,500,500,500,500,500,570,500,556,556,556,556,444,
    500,444);


function PDFCompatibleText(astring:string):string;
var
 i:integer;
begin
 Result:='';
 for i:=1 to Length(astring) do
 begin
  if astring[i] in ['(',')','\'] then
   Result:=Result+'\';
  Result:=Result+astring[i];
 end;
end;

constructor TrpPdfFont.Create;
begin
 Name:=poCourier;
 Size:=10;
end;

// Writes a line into a Stream that is add #13+#10
procedure SWriteLine(Stream:TStream;astring:string);
begin
 astring:=astring+#13+#10;
 Stream.Write(astring[1],Length(astring));
end;



constructor TrpPDFCanvas.Create(AFile:TRpPDFFile);
begin
 FFont:=TRpPDFFont.Create;
 FFile:=AFile;
 SetLength(FLineInfo,CONS_MINLINEINFOITEMS);
 FLineInfoMaxItems:=CONS_MINLINEINFOITEMS;
end;


destructor TrpPDFCanvas.Destroy;
begin
 FFont.free;
 FFont:=nil;
 inherited Destroy;
end;




constructor TRpPDFFile.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FCanvas:=TRpPDFCanvas.Create(Self);
 FMainPDF:=TMemoryStream.Create;
 FTempStream:=TMemoryStream.Create;
 FsTempStream:=TMemoryStream.Create;
 FObjectOffsets:=TStringList.Create;
 FFontList:=TStringList.Create;
 FPages:=TStringList.Create;
 FPageWidth:= 12048;
 FPageHeight:= 17039;
 FResolution:=1440;
 FCanvas.FResolution:=1440;
end;

destructor TRpPDFFile.Destroy;
begin
 FMainPDF.Free;
 FTempStream.Free;
 FsTempStream.Free;
 FCanvas.free;
 FObjectOffsets.free;
 FPages.Free;
 FFOntList.Free;

 inherited Destroy;
end;

procedure TRpPDFFile.SetResolution(Newres:integer);
begin
 FResolution:=NewRes;
 FCanvas.FResolution:=NewRes;
end;


function TrpPDFFile.GetStream:TMemoryStream;
begin
 if Not FStreamValid then
  Raise Exception.Create(SRpStreamNotValid);
 Result:=FMainPDF;
end;

procedure TRpPDFFile.BeginDoc;
begin
 FPrinting:=true;
 FStreamValid:=false;
 FMainPDF.Clear;
 FObjectOffsets.Clear;
 FObjectCount:=0;
 FObjectOffset:=0;
 FPages.Clear;
 FFontList.Clear;
 FFOntCount:=0;
 FCurrentSetPageObject:=0;
 FImageCount:=0;
 FPage:=1;
 // Writes the header
 SWriteLine(FMainPDF,PDF_HEADER);
 AddToOffset(Length(PDF_HEADER));
 // Writes Doc info
 FObjectCount:=FObjectCount+1;
 FTempStream.Clear;
 SWriteLine(FTempStream,IntToStr(FObjectCount)+' 0 obj');
 SWriteLine(FTempStream,'<<');
 SWriteLine(FTempStream,'/Producer ('+FDocProducer+')');
 SWriteLine(FTempStream,'/Author ('+FDocAuthor+')');
 SWriteLine(FTempStream,'/CreationDate (D:'+FormatDateTime('YYYYMMDDHHmmSS',now)+')');
 SWriteLine(FTempStream,'/Creator ('+FDocCreator+')');
 SWriteLine(FTempStream,'/Keywords ('+FDocKeywords+')');
 SWriteLine(FTempStream,'/Subject ('+FDocSubject+')');
 SWriteLine(FTempStream,'/Title ('+FDocTitle+')');
 SWriteLine(FTempStream,'/ModDate ()');
 SWriteLine(FTempStream,'>>');
 SWriteLine(FTempStream,'endobj');
 AddToOffset(FTempStream.Size);
 FTempStream.SaveToStream(FMainPDF);
 StartStream;
end;

procedure TRpPDFFile.StartStream;
begin
 // Starting of the stream
 FObjectCount:=FObjectCount+1;
 FTempStream.Clear;
 SWriteLine(FTempStream,IntToStr(FObjectCount)+' 0 obj');
 SWriteLine(FTempStream,'<< /Length '+IntToStr(FObjectCount+1)+' 0 R');
 if FCompressed then
  SWriteLine(FTempStream,'/Filter [/FlateDecode]');
 SWriteLine(FTempStream,' >>');
 FStreamSize1:=FTempStream.Size;
 SWriteLine(FTempStream,'stream');
 FsTempStream.Clear;
end;

procedure TRpPDFFile.EndStream;
var TempSize: LongInt;
begin
 if FCompressed then
 begin
  FCompressionStream := TCompressionStream.Create(clDefault,FTempStream);
  try
   FCompressionStream.CopyFrom(FsTempStream, 0);
  finally
   FCompressionStream.Free;
  end;
 end
 else
  FsTempStream.SaveToStream(FTempStream);

 FsTempStream.Clear;

 SWriteLine(FTempStream,'endstream');
 SWriteLine(FTempStream,'endobj');
 FStreamSize2:=6;
 AddToOffset(FTempStream.Size);
 FTempStream.SaveToStream(FMainPDF);

 TempSize:=FTempStream.Size-FStreamSize1-FStreamSize2-Length('Stream')-Length('endstream')-6;
 FObjectCount:=FObjectCount+1;
 FTempStream.Clear;
 SWriteLine(FTempStream,IntToStr(FObjectCount)+' 0 obj');
 SWriteLine(FTempStream,IntToStr(TempSize));
 SWriteLine(FTempStream,'endobj');
 AddToOffset(FTempStream.Size);
 FTempStream.SaveToStream(FMainPDF);
end;


procedure TRpPDFFile.AddToOffset(offset:LongInt);
begin
 FObjectOffset:=FObjectOffset+offset;
 FObjectOffsets.Add(IntToStr(FObjectOffset));
end;


procedure TRpPDFFile.NewPage;
var
 TempSize:LongInt;
begin
 CheckPrinting;

 FPage:=FPage+1;

 if FCompressed then
 begin
  FCompressionStream := TCompressionStream.Create(clDefault,FTempStream);
  try
   FCompressionStream.CopyFrom(FsTempStream, 0);
  finally
   FCompressionStream.Free;
  end;
 end
 else
  FsTempStream.SaveToStream(FTempStream);

 FsTempStream.Clear;
 SWriteLine(FTempStream,'endstream');
 SWriteLine(FTempStream,'endobj');
 FStreamSize2:=6;
 AddToOffset(FTempStream.Size);
 FTempStream.SaveToStream(FMainPDF);
 TempSize:=FTempStream.Size-FStreamSize1-FStreamSize2-Length('Stream')-Length('endstream')-6;
 FObjectCount:=FObjectCount+1;
 FTempStream.Clear;
 SWriteLine(FTempStream,IntToStr(FObjectCount)+' 0 obj');
 SWriteLine(FTempStream,IntToStr(TempSize));
 SWriteLine(FTempStream,'endobj');
 AddToOffset(FTempStream.Size);
 FTempStream.SaveToStream(FMainPDF);

 FObjectCount:=FObjectCount+1;
 FTempStream.Clear;
 SWriteLine(FTempStream,IntToStr(FObjectCount)+' 0 obj');
 SWriteLine(FTempStream,'<< /Length '+IntToStr(FObjectCount+1)+' 0 R');
 if Compressed then
  SWriteLine(FTempStream,'/Filter [/FlateDecode]');

 SWriteLine(FTempStream,' >>');

 FStreamSize1:=FTempStream.Size;
 SWriteLine(FTempStream,'stream');
end;


procedure TRpPDFFile.CheckPrinting;
begin
 if Not FPrinting then
  Raise Exception.Create(SRpNotPrintingPDF);
end;

procedure TRpPDFFile.SetOutLine;
begin
 FObjectCount:=FObjectCount+1;
 FOutLinesNum:=FObjectCount;
 FTempStream.Clear;
 SWriteLine(FTempStream,IntToStr(FObjectCount)+' 0 obj');
 SWriteLine(FTempStream,'<< /Type /Outlines');
 SWriteLine(FTempStream,'/Count 0');
 SWriteLine(FTempStream,'>>');
 SWriteLine(FTempStream,'endobj');
 AddToOffset(FTempStream.Size);
 FTempStream.SaveToStream(FMainPDF);
end;

procedure TrpPDFFile.CreateFont(Subtype,BaseFont,Encoding:string);
begin
 FFontCount:=FFontCount+1;
 FObjectCount:=FObjectCount+1;
 FFontList.Add(IntToStr(FObjectCount));
 FTempStream.Clear;
 SWriteLine(FTempStream,IntToStr(FObjectCount)+' 0 obj');
 SWriteLine(FTempStream,'<< /Type /Font');
 SWriteLine(FTempStream,'/Subtype /'+Subtype);
 SWriteLine(FTempStream,'/Name /F'+IntToStr(FFontCount));
 SWriteLine(FTempStream,'/BaseFont /'+BaseFont);
 SWriteLine(FTempStream,'/Encoding /'+Encoding);
 SWriteLine(FTempStream,'>>');
 SWriteLine(FTempStream,'endobj');
 AddToOffset(FTempStream.Size);
 FTempStream.SaveToStream(FMainPDF);
end;


procedure TRpPDFFile.SetFontType;
begin
 CreateFont('Type1','Helvetica','WinAnsiEncoding');
 CreateFont('Type1','Helvetica-Bold','WinAnsiEncoding');
 CreateFont('Type1','Helvetica-Oblique','WinAnsiEncoding');
 CreateFont('Type1','Helvetica-BoldOblique','WinAnsiEncoding');
 CreateFont('Type1','Courier','WinAnsiEncoding');
 CreateFont('Type1','Courier-Bold','WinAnsiEncoding');
 CreateFont('Type1','Courier-Oblique','WinAnsiEncoding');
 CreateFont('Type1','Courier-BoldOblique','WinAnsiEncoding');
 CreateFont('Type1','Times-Roman','WinAnsiEncoding');
 CreateFont('Type1','Times-Bold','WinAnsiEncoding');
 CreateFont('Type1','Times-Italic','WinAnsiEncoding');
 CreateFont('Type1','Times-BoldItalic','WinAnsiEncoding');
 CreateFont('Type1','Symbol','WinAnsiEncoding');
 CreateFont('Type1','ZapfDingbats','WinAnsiEncoding');
end;

procedure TrpPDFFile.SetPages;
var
 i,PageObjNum:Integer;
begin
 FObjectCount:=FObjectCount+1;
 FParentNum:=FObjectCount;
 FTempStream.Clear;
 SWriteLine(FTempStream,IntToStr(FObjectCount)+' 0 obj');
 SWriteLine(FTempStream,'<< /Type /Pages');
 SWriteLine(FTempStream,'/Kids [');

 PageObjNum:=2;
 for i:= 1 to FPage do
 begin
  SWriteLine(FTempStream,IntToStr(FObjectCount+i+1+FImageCount)+' 0 R');
  FPages.Add(IntToStr(PageObjNum));
  PageObjNum:=PageObjNum+2;
 end;
 SWriteLine(FTempStream,']');
 SWriteLine(FTempStream,'/Count '+IntToStr(FPage));
 SWriteLine(FTempStream,'>>');
 SWriteLine(FTempStream,'endobj');
 AddToOffset(FTempStream.Size);
 FTempStream.SaveToStream(FMainPDF);
end;


procedure TrpPDFFile.SetArray;
var
 i:Integer;
begin
 FObjectCount:=FObjectCount+1;
 FResourceNum:=FObjectCount;
 FTempStream.Clear;
 SWriteLine(FTempStream,IntToStr(FObjectCount)+' 0 obj');
 SWriteLine(FTempStream,'<< /ProcSet [ /PDF /Text /ImageC]');
 SWriteLine(FTempStream,'/XObject << ');
 for i:=1 to FImageCount do
  SWriteLine(FTempStream,'/Im'+IntToStr(i)+' '+IntToStr(FObjectCount+i)+' 0 R');
 SWriteLine(FTempStream,'>>');
 SWriteLine(FTempStream,'/Font << ');

 for i:=1 to FFontCount do
  SWriteLine(FTempStream,'/F'+IntToStr(i)+' '+FFontList.Strings[i-1]+' 0 R ');

 SWriteLine(FTempStream,'>>');
 SWriteLine(FTempStream,'>>');
 SWriteLine(FTempStream,'endobj');
 AddToOffset(FTempStream.Size);
 FTempStream.SaveToStream(FMainPDF);
end;

procedure TrpPDFFile.SetPageObject;
begin
 FObjectCount:=FObjectCount+1;
 FTempStream.Clear;
 SWriteLine(FTempStream,IntToStr(FObjectCount)+' 0 obj');
 SWriteLine(FTempStream,'<< /Type /Page');
 SWriteLine(FTempStream,'/Parent '+IntToStr(FParentNum)+' 0 R');
 SWriteLine(FTempStream,'/MediaBox [ 0 0 '+Canvas.UnitsToTextX(FPageWidth)+' '+Canvas.UnitsToTextX(FPageHEight)+']');
 SWriteLine(FTempStream,'/Contents '+FPages.Strings[FCurrentSetPageObject]+' 0 R');
 SWriteLine(FTempStream,'/Resources '+IntToStr(FResourceNum)+' 0 R');
 SWriteLine(FTempStream,'>>');
 SWriteLine(FTempStream,'endobj');
 AddToOffset(FTempStream.Size);
 FTempStream.SaveToStream(FMainPDF);
 FCurrentSetPageObject:=FCurrentSetPageObject+1;
end;

function TRpPDFCanvas.UnitsToTextText(Value:integer;FontSize:integer):string;
var
 olddecimalseparator:char;
begin
 olddecimalseparator:=decimalseparator;
 decimalseparator:='.';
 try
  Result:=FormatCurr('######0.00',(((FFile.FPageHeight-Value)/FResolution)*CONS_PDFRES)-FontSize);
 finally
  decimalseparator:=olddecimalseparator;
 end;
end;

function NumberToText(Value:double):string;
var
 olddecimalseparator:char;
begin
 olddecimalseparator:=decimalseparator;
 decimalseparator:='.';
 try
  Result:=FormatCurr('######0.00',Value);
 finally
  decimalseparator:=olddecimalseparator;
 end;
end;

function TRpPDFCanvas.UnitsToTextX(Value:integer):string;
var
 olddecimalseparator:char;
begin
 olddecimalseparator:=decimalseparator;
 decimalseparator:='.';
 try
  Result:=FormatCurr('######0.00',(Value/FResolution)*CONS_PDFRES);
 finally
  decimalseparator:=olddecimalseparator;
 end;
end;

function TRpPDFCanvas.UnitsToTextY(Value:integer):string;
var
 olddecimalseparator:char;
begin
 olddecimalseparator:=decimalseparator;
 decimalseparator:='.';
 try
  Result:=FormatCurr('######0.00',((FFile.FPageHeight-Value)/FResolution)*CONS_PDFRES);
 finally
  decimalseparator:=olddecimalseparator;
 end;
end;


procedure TrpPDFFile.SetCatalog;
begin
 FObjectCount:=FObjectCount+1;
 FCatalogNum:=FObjectCount;
 FTempStream.Clear;
 SWriteLine(FTempStream,IntToStr(FObjectCount)+' 0 obj');
 SWriteLine(FTempStream,'<< /Type /Catalog');
 SWriteLine(FTempStream,'/Pages '+IntToStr(FParentNum)+' 0 R');
 SWriteLine(FTempStream,'/Outlines '+IntToStr(FOutlinesNum)+' 0 R');
 SWriteLine(FTempStream,'>>');
 SWriteLine(FTempStream,'endobj');
 AddToOffset(FTempStream.Size);
 FTempStream.SaveToStream(FMainPDF);
end;


function TrpPDFFile.GetOffsetNumber(offset:string):string;
var
 x,y:LongInt;
begin
 x:=Length(offset);
 result:='';
 for y:= 1 to 10-x do
  result:=result+'0';
 result:=result+offset;
end;

procedure TrpPDFFile.SetXref;
var i:Integer;
begin
 FObjectCount:=FObjectCount+1;
 FTempStream.Clear;
 SWriteLine(FTempStream,'xref');
 SWriteLine(FTempStream,'0 '+IntToStr(FObjectCount));
 SWriteLine(FTempStream,'0000000000 65535 f');

 for i:=0 to FObjectCount-2 do
  SWriteLine(FTempStream,GetOffsetNumber(trim(FObjectOffsets.Strings[i]))+' 00000 n');

 SWriteLine(FTempStream,'trailer');
 SWriteLine(FTempStream,'<< /Size '+IntToStr(FObjectCount));
 SWriteLine(FTempStream,'/Root '+IntToStr(FCatalogNum)+' 0 R');
 SWriteLine(FTempStream,'/Info 1 0 R');
 SWriteLine(FTempStream,'>>');
 SWriteLine(FTempStream,'startxref');
 SWriteLine(FTempStream,trim(FObjectOffsets.Strings[FObjectCount-1]));
 FTempStream.SaveToStream(FmainPDF);
end;


procedure TRpPDFFile.EndDoc;
var
 i:integer;
begin
 CheckPrinting;
 FPrinting:=false;
 // Writes the trailing zone
 EndStream;
 SetOutLine;
 SetFontType;
 SetPages;
 SetArray;
// for i:= 1 to FImageCount do
//  WriteBitmap(i);
 for i:= 1 to FPage do
 begin
  SetPageObject;
 end;
 SetCatalog;
 SetXref;
 SWriteLine(FMainPDF,'%%EOF');


 // Save to disk if filename assigned
 FStreamValid:=True;
 FMainPDF.Seek(0,soFromBeginning);
 if Length(Trim(FFilename))>0 then
 begin
  FMainPDF.SaveToFile(FFilename);
  FMainPDF.Seek(0,soFromBeginning);
 end;
end;

procedure TRpPDFFile.AbortDoc;
begin
 FMainPDF.Clear;
 FStreamValid:=false;
 FPrinting:=false;
end;


function RGBToFloats(color:integer):string;
var
 r,g,b:byte;
 acolor:LongWord;
 olddecimal:char;
begin
 olddecimal:=decimalseparator;
 try
  decimalseparator:='.';
  acolor:=color;
  r:=byte(acolor);
  Result:=FormatCurr('0.00',r/256);
  g:=byte(acolor shr 8);
  Result:=Result+' '+FormatCurr('0.00',g/256);
  b:=byte(acolor shr 16);
  Result:=Result+' '+FormatCurr('0.00',b/256);
 finally
  decimalseparator:=olddecimal;
 end;
end;


procedure TRpPDFCanvas.SetDash;
begin
 case PenStyle of
  // Dash
  1:
   begin
    SWriteLine(FFile.FsTempStream,'[16 8] 0 d');
   end;
  // Dot
  2:
   begin
    SWriteLine(FFile.FsTempStream,'[3] 0 d');
   end;
  // Dash dot
  3:
   begin
    SWriteLine(FFile.FsTempStream,'[8 7 2 7] 0 d');
   end;
  // Dash dot dot
  4:
   begin
    SWriteLine(FFile.FsTempStream,'[8 4 2 4 2 4] 0 d');
   end;
  // Clear
  5:
   begin
   end;
  else
   begin
    SWriteLine(FFile.FsTempStream,'[] 0 d');
   end;
 end;
end;

procedure TRpPDFCanvas.Line(x1,y1,x2,y2:Integer);
var
 LineWidth:integer;

procedure DoWriteLine;
begin
 SWriteLine(FFile.FsTempStream,RGBToFloats(PenColor)+' RG');
 SWriteLine(FFile.FsTempStream,RGBToFloats(PenColor)+' rg');
 SWriteLine(FFile.FsTempStream,UnitsToTextX(x1)+' '+UnitsToTextY(y1)+' m');
 SWriteLine(FFile.FsTempStream,UnitsToTextX(x2)+' '+UnitsToTextY(y2)+' l');
 // S-Solid,  D-Dashed, B-Beveled, I-Inset, U-Underline
 SWriteLine(FFile.FsTempStream,'S');
end;

begin
 if PenStyle=5 then
  exit;
 SetDash;
 LineWidth:=1;
 If (PenWidth>0) then
  LineWidth:=PenWidth;
 SWriteLine(FFile.FsTempStream,UnitsToTextX(LineWidth)+' w');
 DoWriteLine;
end;

procedure TRpPDFCanvas.Ellipse(X1, Y1, X2, Y2: Integer);
var
 LineWidth:integer;
 W,H:integer;
 opfill:string;
begin
 if ((PenStyle=5) and (BrushStyle=1)) then
  exit;
 SetDash;
 W:=X2-X1;
 H:=Y2-Y1;
 LineWidth:=1;
 If (PenWidth>0) then
  LineWidth:=PenWidth;
 SWriteLine(FFile.FsTempStream,UnitsToTextX(LineWidth)+' w');
 SWriteLine(FFile.FsTempStream,RGBToFloats(PenColor)+' RG');
 SWriteLine(FFile.FsTempStream,RGBToFloats(BrushColor)+' rg');
 // Draws a ellipse in 4 pass
 SWriteLine(FFile.FsTempStream,UnitsToTextX(X1)+' '+
  UnitsToTextY(y1+(H div 2))+' m');
 SWriteLine(FFile.FsTempStream,
  UnitsToTextX(X1)+' '+UnitsToTextY(y1+(H div 2)-Round(H/2*11/20))+' '+
  UnitsToTextX(X1+(W div 2)-Round(W/2*11/20))+' '+UnitsToTextY(y1)+' '+
  UnitsToTextX(X1+(W div 2))+' '+UnitsToTextY(y1)+
  ' c');
 SWriteLine(FFile.FsTempStream,
  UnitsToTextX(X1+(W div 2)+Round(W/2*11/20))+' '+UnitsToTextY(y1)+' '+
  UnitsToTextX(X1+W)+' '+UnitsToTextY(y1+(H div 2)-Round(H/2*11/20))+' '+
  UnitsToTextX(X1+W)+' '+UnitsToTextY(y1+(H div 2))+
  ' c');
 SWriteLine(FFile.FsTempStream,
  UnitsToTextX(X1+W)+' '+UnitsToTextY(y1+(H div 2)+Round(H/2*11/20))+' '+
  UnitsToTextX(X1+(W div 2)+Round(W/2*11/20))+' '+UnitsToTextY(y1+H)+' '+
  UnitsToTextX(X1+(W div 2))+' '+UnitsToTextY(y1+H)+
  ' c');
 SWriteLine(FFile.FsTempStream,
  UnitsToTextX(X1+(W div 2)-Round(W/2*11/20))+' '+UnitsToTextY(y1+H)+' '+
  UnitsToTextX(X1)+' '+UnitsToTextY(y1+(H div 2)+Round(H/2*11/20))+' '+
  UnitsToTextX(X1)+' '+UnitsToTextY(y1+(H div 2))+
  ' c');

 opfill:='B';
 if PenStyle=5 then
 begin
  opfill:='f';
 end;
 // Bsclear
 if BrushStyle=1 then
  SWriteLine(FFile.FsTempStream,'S')
 else
 // BsSolid
  SWriteLine(FFile.FsTempStream,opfill);
end;

procedure TRpPDFCanvas.Rectangle(x1,y1,x2,y2:Integer);
var
 LineWidth:integer;
 opfill:string;
begin
 if ((PenStyle=5) and (BrushStyle=1)) then
  exit;
 SetDash;
 LineWidth:=1;
 If (PenWidth>0) then
  LineWidth:=PenWidth;
 SWriteLine(FFile.FsTempStream,UnitsToTextX(LineWidth)+' w');
 SWriteLine(FFile.FsTempStream,RGBToFloats(PenColor)+' RG');
 SWriteLine(FFile.FsTempStream,RGBToFloats(BrushColor)+' rg');
 SWriteLine(FFile.FsTempStream,UnitsToTextX(x1)+' '+UnitsToTextY(y1)+
  ' '+UnitsToTextX(x2-x1)+' '+UnitsToTextX(-(y2-y1))+' re');
 opfill:='B';
 if PenStyle=5 then
 begin
  opfill:='f';
 end;
 // Bsclear
 if BrushStyle=1 then
  SWriteLine(FFile.FsTempStream,'S')
 else
 // BsSolid
  SWriteLine(FFile.FsTempStream,opfill);
end;


procedure TRpPDFCanvas.SaveGraph;
begin
 SWriteLine(FFile.FsTempStream,'q');
end;

procedure TRpPDFCanvas.RestoreGraph;
begin
 SWriteLine(FFile.FsTempStream,'Q');
end;

procedure TRpPDFCanvas.TextRect(ARect: TRect; Text: string;
                       Alignment: integer; Clipping: boolean;Wordbreak:boolean;
                       Rotation:integer=0);
var
 recsize:TRect;
 i:integer;
 posx,posY:integer;
 singleline:boolean;
begin
 FFile.CheckPrinting;

 if (Clipping or (Rotation<>0)) then
 begin
  SaveGraph;
 end;
 try
  if Clipping then
  begin
   // Clipping rectangle
   SWriteLine(FFile.FsTempStream,UnitsToTextX(ARect.Left)+' '+UnitsToTextY(ARect.Top)+
   ' '+UnitsToTextX(ARect.Right-ARect.Left)+' '+UnitsToTextX(-(ARect.Bottom-ARect.Top))+' re');
   SWriteLine(FFile.FsTempStream,'h'); // ClosePath
   SWriteLine(FFile.FsTempStream,'W'); // Clip
   SWriteLine(FFile.FsTempStream,'n'); // NewPath
  end;
  singleline:=(Alignment AND AlignmentFlags_SingleLine)>0;
  if singleline then
   wordbreak:=false;
  // Calculates text extent and apply alignment
  recsize:=ARect;
  TextExtent(Text,recsize,wordbreak,singleline);
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
   TextOut(PosX,PosY+FLineInfo[i].TopPos,Copy(Text,FLineInfo[i].Position,FLineInfo[i].Size),FLineInfo[i].Width,Rotation);
  end;
 finally
  if (Clipping or (Rotation<>0)) then
  begin
   RestoreGraph;
  end;
 end;
end;


function Type1FontTopdfFontName(Type1Font:TRpType1Font;oblique,bold:boolean):integer;
begin
 Result:=0;
 case Type1Font of
  poHelvetica:
   begin
    Result:=0;
   end;
  poCourier:
   begin
    Result:=4;
   end;
  poTimesRoman:
   begin
    Result:=8;
   end;
  poSymbol:
   begin
    Result:=12;
   end;
  poZapfDingbats:
   begin
    Result:=13;
   end;
 end;
 if (Type1Font in [poHelvetica..poTimesRoman]) then
 begin
  if bold then
   Result:=Result+1;
  if oblique then
   Result:=Result+2;
 end;
end;


procedure TRpPDFCanvas.TextOut(X, Y: Integer; const Text: string;LineWidth,Rotation:integer);
var
 rotrad,fsize:double;
 rotstring:string;
 PosLine,PosLineX1,PosLineY1,PosLineX2,PosLineY2:integer;
begin
 FFile.CheckPrinting;
 if (Rotation<>0) then
 begin
  SaveGraph;
 end;
 try
  SWriteLine(FFile.FsTempStream,RGBToFloats(Font.Color)+' RG');
  SWriteLine(FFile.FsTempStream,RGBToFloats(Font.Color)+' rg');
  SWriteLine(FFile.FsTempStream,'BT');
  SWriteLine(FFile.FsTempStream,'/F'+
  IntToStr(Type1FontTopdfFontName(Font.Name,Font.Italic,Font.Bold)+1)+' '+
   IntToStr(Font.Size)+ ' Tf');

  // Rotates
  if Rotation<>0 then
  begin
   rotstring:='1 0 0 1 '+
    UnitsToTextX(X)+' '+
    UnitsToTextText(Y,Font.Size);
   SWriteLine(FFile.FsTempStream,rotstring+' cm');
   rotrad:=Rotation/10*(2*PI/360);
   rotstring:=NumberToText(cos(rotrad))+' '+
    NumberToText(sin(rotrad))+' '+
    NumberToText(-sin(rotrad))+' '+
    NumberToText(cos(rotrad))+' 0 0';
   SWriteLine(FFile.FsTempStream,rotstring+' cm');
  end
  else
   SWriteLine(FFile.FsTempStream,UnitsToTextX(X)+' '+UnitsToTextText(Y,Font.Size)+' Td');
  SWriteLine(FFile.FsTempStream,'('+PDFCompatibleText(Text)+') Tj');
  SWriteLine(FFile.FsTempStream,'ET');
 finally
  if (Rotation<>0) then
  begin
   RestoreGraph;
  end;
 end;
 // Underline and strikeout
 if FFont.Underline then
 begin
  PenStyle:=0;
  PenWidth:=Round((Font.Size/CONS_PDFRES*FResolution)*CONS_UNDERLINEWIDTH);
  PenColor:=FFont.Color;
  if Rotation=0 then
  begin
   Posline:=Round(CONS_UNDERLINEPOS*(Font.Size/CONS_PDFRES*FResolution));
   Line(X,Y+Posline,X+LineWidth,Y+Posline);
  end
  else
  begin
   Y:=Y+Round(CONS_UNDERLINEPOS*(Font.Size/CONS_PDFRES*FResolution));
   rotrad:=Rotation/10*(2*PI/360);
   fsize:=CONS_UNDERLINEPOS*Font.Size/CONS_PDFRES*FResolution-Font.Size/CONS_PDFRES*FResolution;
   PosLineX1:=-Round(fsize*cos(rotrad));
   PosLineY1:=-Round(fsize*sin(rotrad));
   PosLineX2:=Round(LineWidth*cos(rotrad));
   PoslineY2:=-Round(LineWidth*sin(rotrad));
   Line(X+PosLineX1,Y+PosLineY1,X+PosLineX2,Y+PosLineY2);
   Y:=Y-Round(CONS_UNDERLINEPOS*(Font.Size/CONS_PDFRES*FResolution));
  end;
 end;
 if FFont.StrikeOut then
 begin
  PenStyle:=0;
  PenWidth:=Round((Font.Size/CONS_PDFRES*FResolution)*CONS_UNDERLINEWIDTH);
  PenColor:=FFont.Color;
  if Rotation=0 then
  begin
   Posline:=Round(CONS_STRIKEOUTPOS*(Font.Size/CONS_PDFRES*FResolution));
   Line(X,Y+Posline,X+LineWidth,Y+Posline);
  end
  else
  begin
   Y:=Y+Round(CONS_UNDERLINEPOS*(Font.Size/CONS_PDFRES*FResolution));
   rotrad:=Rotation/10*(2*PI/360);
   fsize:=CONS_UNDERLINEPOS*Font.Size/CONS_PDFRES*FResolution-Font.Size/CONS_PDFRES*FResolution;
   PosLineX1:=-Round(fsize*cos(rotrad));
   PosLineY1:=-Round(fsize*sin(rotrad));
   PosLineX2:=Round(LineWidth*cos(rotrad));
   PoslineY2:=-Round(LineWidth*sin(rotrad));
   fsize:=(1-CONS_STRIKEOUTPOS)*Font.Size/CONS_PDFRES*FResolution;
   PosLineX1:=X+PosLineX1;
   PosLineY1:=Y+PosLineY1;
   PosLineX2:=X+PosLineX2;
   PosLineY2:=Y+PosLineY2;
   PoslineX1:=PosLineX1-Round(fsize*sin(rotrad));
   PoslineY1:=PosLineY1-Round(fsize*cos(rotrad));
   PoslineX2:=PosLineX2-Round(fsize*sin(rotrad));
   PoslineY2:=PosLineY2-Round(fsize*cos(rotrad));
   Line(PoslineX1,PosLineY1,PosLineX2,PosLineY2);
  end;
 end;
end;

{procedure TRpPDFCanvas.StretchDraw(rec:TRect;abitmap:TBitmap);
var
 tempsx,tempsy:double;
begin
 FFile.CheckPrinting;
// if (PageHeight > PageWidth) then begin
//  tempsx:=((PageWidth)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSX)*10));
//  tempsy:=((PageHeight)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSY)*11.900));
//end
//else begin
//  tempsx:=((PageWidth)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSX)* 13));
//  tempsy:=((PageHeight)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSY)*8));
//end;

 FFile.FImageCount:=FFile.FImageCount+1;
 SWriteLine(FFile.FsTempStream,'q');
 SWriteLine(FFile.FsTempStream,UnitsToTextX(rec.Right-rec.Left)+
' 0 0 '+UnitsToTextX(rec.Bottom-rec.Top)+
 ' '+UnitsToTextX(rec.Left)+' '+UnitsToTextY(rec.Bottom)
 +' cm');
 SWriteLine(FFile.FsTempStream,'/Im'+IntToStr(FFile.FImageCount)+' Do');
 SWriteLine(FFile.FsTempStream,'Q');
// SetBitmap(ABitmap);
end;
}


function TRpPDFCanvas.CalcCharWidth(charcode:char):double;
var
 intvalue:Byte;
 defaultwidth:integer;
 parray:PWinAnsiWidthsArray;
begin
 if charcode in [#0,#13,#10] then
 begin
  Result:=0;
  exit;
 end;
 parray:=nil;
 if (FFont.Name=poHelvetica) then
 begin
  parray:=@Helvetica_Widths;
  if FFont.Bold then
  begin
   if FFont.Italic then
    parray:=@Helvetica_BoldItalic_Widths;
  end
  else
   if FFont.Italic then
    parray:=@Helvetica_Italic_Widths;
 end
 else
 if (FFont.Name=poTimesRoman) then
 begin
  parray:=@TimesRoman_Widths;
  if FFont.Bold then
  begin
   if FFont.Italic then
    parray:=@TimesRoman_BoldItalic_Widths;
  end
  else
   if FFont.Italic then
    parray:=@TimesRoman_Italic_Widths;
 end;
 defaultwidth:=Default_Font_Width;
 intvalue:=Byte(charcode);
 if assigned(parray) then
 begin
  if intvalue<32 then
   Result:=defaultwidth
  else
   Result:=parray^[intvalue];
 end
 else
  Result:=defaultwidth;
 Result:=Result*FFont.Size/1000;
end;

procedure TRpPDFCanvas.TextExtent(const Text:WideString;var Rect:TRect;
 wordbreak:boolean;singleline:boolean);
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
 // Text extent for the simpe strings, wide strings not supported
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
 recwidth:=(rect.Right-rect.Left)/FResolution*CONS_PDFRES;
 nextline:=false;
 i:=1;
 alastsize:=0;
 lockspace:=false;
 while i<=Length(astring) do
 begin
  newsize:=CalcCharWidth(astring[i]);
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
   info.Width:=Round((asize)/CONS_PDFRES*FResolution);
   info.height:=Round((Font.Size)/CONS_PDFRES*FResolution);
   info.TopPos:=arec.Bottom;
   arec.Bottom:=arec.Bottom+info.height;
   asize:=0;
   position:=i+1;
   NewLineInfo(info);
  end;
  inc(i);
 end;
 arec.Right:=Round((maxwidth+1)/CONS_PDFRES*FResolution);
 if Position<=Length(astring) then
 begin
  info.Position:=position;
  info.Size:=Length(astring)-position+1;
  info.Width:=Round((asize+1)/CONS_PDFRES*FResolution);
  info.height:=Round((Font.Size)/CONS_PDFRES*FResolution);
  info.TopPos:=arec.Bottom;
  arec.Bottom:=arec.Bottom+info.height;
  NewLineInfo(info);
 end;
 rect:=arec;
end;

procedure TRpPDFCanvas.NewLineInfo(info:TRpLineInfo);
begin
 if FLineInfoMaxItems<=FLineInfoCount-1 then
 begin
  SetLength(FLineInfo,FLineInfoMaxItems*2);
  FLineInfoMaxItems:=FLineInfoMaxItems*2;
 end;
 FLineInfo[FLineInfoCount]:=info;
 inc(FLineInfoCount);
end;

end.
