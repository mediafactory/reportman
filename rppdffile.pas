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
{                                                       }
{                                                       }
{       Still Missing:                                  }
{               -TEXT ALIGNMENT                         }
{               -Underline, bold, italic                }
{               -Brush Patterns                         }
{               -Multiline text                         }
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
{$IFDEF USEVCL}
 graphics,
{$ELSE}
 QGraphics,
{$ENDIF}
 rpzlib,types;

resourcestring
 SRpStreamNotValid='PDF Stream not valid';
 SRpNotPrintingPDF='Not in pdf printing state';

const
 PDF_HEADER:string='%PDF-1.4';
 CONS_PDFRES=72;
type
 TRpPDFFont=class(TObject)
  public
   Name:integer;
   Size:integer;
   Color:integer;
   constructor Create;
  end;

 TRpPDFFile=class;

 TRpPDFCanvas=class(TObject)
  private
   FFont:TRpPDFFont;
   FFile:TRpPDFFile;
   FResolution:integer;
   procedure SetDash;
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
   procedure TextOut(X, Y: Integer; const Text: string);
   procedure TextRect(ARect: TRect; Text: string;
                       Alignment: integer; Clipping: boolean);
   procedure Rectangle(x1,y1,x2,y2:Integer);
   procedure StretchDraw(rec:TRect;abitmap:TBitmap);
   procedure Ellipse(X1, Y1, X2, Y2: Integer);
   constructor Create(AFile:TRpPDFFile);
   destructor Destroy;override;
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


implementation


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
 Name:=4;
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


procedure TRpPDFCanvas.TextRect(ARect: TRect; Text: string;
                       Alignment: integer; Clipping: boolean);

procedure SaveGraph;
begin
 SWriteLine(FFile.FsTempStream,'q');
end;

procedure RestoreGraph;
begin
 SWriteLine(FFile.FsTempStream,'Q');
end;


begin
 FFile.CheckPrinting;

 if Clipping then
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
  // Underline
  TextOut(ARect.Left,ARect.Top,Text);
 finally
  if Clipping then
  begin
   RestoreGraph;
  end;
 end;
end;



procedure TRpPDFCanvas.TextOut(X, Y: Integer; const Text: string);
begin
 FFile.CheckPrinting;
 SWriteLine(FFile.FsTempStream,RGBToFloats(Font.Color)+' RG');
 SWriteLine(FFile.FsTempStream,RGBToFloats(Font.Color)+' rg');
 SWriteLine(FFile.FsTempStream,'BT');


 SWriteLine(FFile.FsTempStream,'/F'+IntToStr((Integer(Font.Name)+1))+' '+
  IntToStr(Font.Size)+ ' Tf');

 SWriteLine(FFile.FsTempStream,UnitsToTextX(X)+' '+UnitsToTextText(Y,Font.Size)+' Td');
 SWriteLine(FFile.FsTempStream,'('+PDFCompatibleText(Text)+') Tj');

 SWriteLine(FFile.FsTempStream,'ET');
end;

procedure TRpPDFCanvas.StretchDraw(rec:TRect;abitmap:TBitmap);
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

{ FFile.FImageCount:=FFile.FImageCount+1;
 SWriteLine(FFile.FsTempStream,'q');
 SWriteLine(FFile.FsTempStream,UnitsToTextX(rec.Right-rec.Left)+
' 0 0 '+UnitsToTextX(rec.Bottom-rec.Top)+
 ' '+UnitsToTextX(rec.Left)+' '+UnitsToTextY(rec.Bottom)
 +' cm');
 SWriteLine(FFile.FsTempStream,'/Im'+IntToStr(FFile.FImageCount)+' Do');
 SWriteLine(FFile.FsTempStream,'Q');
}// SetBitmap(ABitmap);
end;

end.
