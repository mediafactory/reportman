{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpdrawitem                                      }
{       TRpImage printable image component              }
{       TRpShape printable simple drawingv              }
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

unit rpdrawitem;

interface

{$I rpconf.inc}

uses Sysutils,
{$IFDEF MSWINDOWS}
 windows,
{$ENDIF}
{$IFDEF USEVARIANTS}
  Variants,Types,
{$ENDIF}
 Classes,rptypes,rpprintitem,rpmdconsts,rpmetafile,rpeval,
 rptypeval,db;

const
 DEF_DRAWWIDTH=500;
 DEFAULT_DPI=100;

// TCopyMode = (cmBlackness, cmDstInvert, cmMergeCopy, cmMergePaint,
// cmNotSrcCopy, cmNotSrcErase, cmPatCopy, cmPatInvert,
// cmPatPaint, cmSrcAnd, cmSrcCopy, cmSrcErase,
// cmSrcInvert, cmSrcPaint, cmWhiteness, cmCreateMask);
 DEF_COPYMODE=10;

type
 TRpShape=class(TRpCommonPosComponent)
  private
   FBrushStyle:integer;
   FBrushColor:integer;
   FPenStyle:integer;
   FPenColor:integer;
   FShape:TRpShapeType;
   FPenWidth:integer;
  protected
   procedure DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);override;
  public
   constructor Create(AOwner:TComponent);override;
   destructor Destroy;override;
  published
   property BrushStyle:integer read FBrushStyle write FBrushStyle default 0;
   property BrushColor:integer read FBrushColor write FBrushColor default $FFFFFF;
   property PenStyle:integer read FPenStyle write FPenStyle default 0;
   property PenColor:integer read FPenColor write FPenColor default 0;
   property Shape:TRpShapeType read FShape write FShape default rpsRectangle;
   property PenWidth:integer read FPenWidth write FPenWidth default 10;
  end;

 TRpImage=class(TRpCommonPosComponent)
  private
   FExpression:WideString;
   FStream:TMemoryStream;
   FDrawStyle:TRpImageDrawStyle;
   Fdpires:integer;
   FCopyMode:integer;
   FRotation:SmallInt;
   procedure ReadStream(AStream:TStream);
   procedure WriteStream(AStream:TStream);
   function GetStream:TMemoryStream;
   procedure WriteExpression(Writer:TWriter);
   procedure ReadExpression(Reader:TReader);
  public
   constructor Create(AOwner:TComponent);override;
   procedure SetStream(Value:TMemoryStream);
   destructor Destroy;override;
  protected
   procedure DefineProperties(Filer: TFiler);override;
   procedure DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);override;
  public
   function GetExtension(adriver:IRpPrintDriver):TPoint;override;
   property Stream:TMemoryStream read FStream write SetStream;
   property Expression:WideString read FExpression write FExpression;
  published
   // Rotating bitmaps still not implemented
   property Rotation:smallint read FRotation write FRotation default 0;
   property DrawStyle:TRpImageDrawStyle read FDrawStyle write FDrawStyle
    default rpDrawCrop;
   property dpires:integer read   Fdpires write Fdpires default DEfAULT_DPI;
   property CopyMode:integer read FCopyMode write FCopyMode default 10;
  end;

implementation

uses rpreport;

{ Paradox graphic BLOB header }

type
  TGraphicHeader = record
    Count: Word;                { Fixed at 1 }
    HType: Word;                { Fixed at $0100 }
    Size: Longint;              { Size not including header }
  end;

constructor TRpShape.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 Width:=DEF_DRAWWIDTH;
 Height:=DEF_DRAWWIDTH;
 FBrushColor:=$FFFFFF;
end;

destructor TRpShape.Destroy;
begin

 inherited destroy;
end;

procedure TRpShape.DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);
begin
 metafile.Pages[metafile.CurrentPage].NewDrawObject(aposy,aposx,Width,Height,
  integer(Shape),BrushStyle,BrushColor,PenStyle,PenWidth,PenColor);
end;

constructor TRpImage.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 Width:=DEF_DRAWWIDTH;
 Height:=DEF_DRAWWIDTH;
 FStream:=TMemoryStream.Create;
 FCopyMode:=DEF_COPYMODE;
 Fdpires:=DEFAULT_DPI;
end;


destructor TRpImage.Destroy;
begin
 FStream.free;

 inherited Destroy;
end;

procedure TRpImage.SetStream(Value:TMemoryStream);
begin
 FStream.LoadFromStream(Value);
end;

procedure TRpImage.WriteExpression(Writer:TWriter);
begin
 WriteWideString(Writer, FExpression);
end;

procedure TRpImage.ReadExpression(Reader:TReader);
begin
 FExpression:=ReadWideString(Reader);
end;


procedure TRpImage.DefineProperties(Filer: TFiler);
begin
 inherited;
 
 Filer.DefineProperty('Expression',ReadExpression,WriteExpression,True);
 Filer.DefineBinaryProperty('Stream', ReadStream, WriteStream, true);
end;

procedure TRpImage.ReadStream(AStream:TStream);
var
 ssize:Int64;
begin
 if (sizeof(ssize)<>AStream.Read(ssize,sizeof(ssize))) then
  Raise Exception.Create(SRpInvalidStreaminRpImage);
 FStream.SetSize(ssize);
 FStream.Seek(0,soFromBeginning);
 if ssize<>AStream.Read(FStream.memory^,ssize) then
  Raise Exception.Create(SRpInvalidStreaminRpImage);
end;

procedure TRpImage.WriteStream(AStream:TStream);
var
 ssize:Int64;
begin
 ssize:=FStream.Size;
 AStream.Write(ssize,sizeof(ssize));
 FStream.Seek(0,soFromBeginning);
 AStream.Write(FStream.Memory^,ssize);
end;

function TRpImage.GetStream:TMemoryStream;
var
 evaluator:TRpEvaluator;
 iden:TIdentifier;
 afield:TField;
 AStream:TStream;
 Size,readed: Longint;
 Header: TGraphicHeader;
 FMStream:TMemoryStream;
 aValue:Variant;
 afilename:TFilename;
begin
 try
  Result:=nil;
  if Length(Trim(Expression))>0 then
  begin
   // If the expression is a field
   if Not Assigned(TRpReport(GetReport).Evaluator) then
    Exit;
   evaluator:=TRpReport(GetReport).evaluator;
   iden:=evaluator.SearchIdentifier(Expression);
   if Not Assigned(iden) then
   begin
    // Looks for a string (path to file)
    aValue:=evaluator.EvaluateText(Expression);
    if (VarType(aValue)<>varString) then
     Raise Exception.Create(SRpFieldNotFound+FExpression);
    afilename:=aValue;
    FMStream:=TMemoryStream.Create;
    try
     AStream:=TFileStream.Create(afilename,fmOpenread or fmShareDenyWrite);
     try
      Size := AStream.Size;
      FMStream.SetSize(Size);
      if Size >= SizeOf(TGraphicHeader) then
      begin
        AStream.Read(Header, SizeOf(Header));
        if (Header.Count <> 1) or (Header.HType <> $0100) or
          (Header.Size <> Size - SizeOf(Header)) then
          AStream.Position := 0
        else
         FMStream.SetSize(AStream.Size-SizeOf(Header));
      end;
      FMStream.Seek(0,soFromBeginning);
      readed:=AStream.Read(FMStream.Memory^,FMStream.Size);
      if readed<>FMStream.Size then
       Raise Exception.Create(SRpErrorReadingFromFieldStream);
      FMStream.Seek(0,soFromBeginning);
     finally
      AStream.free;
     end;
     Result:=FMStream;
    except
     FMStream.free;
     Raise;
    end;
   end
   else
   begin
    if (Not (iden is TIdenField)) then
     Raise Exception.Create(SRpNotAField+FExpression);
    AField:=(iden As TIdenField).Field;
    if (Not (AField is TBlobField)) then
     Raise Exception.Create(SRpNotBinary+FExpression);
    if AField.isnull then
     exit;
    FMStream:=TMemoryStream.Create;
    try
     AStream:=AField.DataSet.CreateBlobStream(AField,bmRead);
     try
      Size := AStream.Size;
      FMStream.SetSize(Size);
      if Size >= SizeOf(TGraphicHeader) then
      begin
        AStream.Read(Header, SizeOf(Header));
        if (Header.Count <> 1) or (Header.HType <> $0100) or
          (Header.Size <> Size - SizeOf(Header)) then
          AStream.Position := 0
        else
         FMStream.SetSize(AStream.Size-SizeOf(Header));
      end;
      FMStream.Seek(0,soFromBeginning);
      readed:=AStream.Read(FMStream.Memory^,FMStream.Size);
      if readed<>FMStream.Size then
       Raise Exception.Create(SRpErrorReadingFromFieldStream);
      FMStream.Seek(0,soFromBeginning);
     finally
      AStream.free;
     end;
     Result:=FMStream;
    except
     FMStream.free;
     Raise;
    end;
   end;
  end
  else
  begin
   if FStream.Size=0 then
    exit;
   Result:=FStream;
  end;
 except
  on E:Exception do
  begin
   Raise TRpReportException.Create(E.Message+':'+SRpSExpression+' '+Name,self,SRpSImage);
  end;
 end;
end;



procedure TRpImage.DoPrint(aposx,aposy:integer;metafile:TRpMetafileReport);
var
 FMStream:TMemoryStream;
begin
 if Not Assigned(FStream) then
  exit;
 FMStream:=GetStream;
 if Not Assigned(FMStream) then
  exit;
 try
  metafile.Pages[metafile.CurrentPage].NewImageObject(aposy,aposx,
   Width,Height,Integer(CopyMode),Integer(DrawStyle),Integer(dpires),FMStream);
 finally
  if FMStream<>FStream then
   FMStream.free;
 end;
end;

function TRpImage.GetExtension(adriver:IRpPrintDriver):TPoint;
var
 FMStream:TMemoryStream;
begin
 Result:=inherited GetExtension(adriver);

 if (DrawStyle in [rpDrawCrop,rpDrawStretch,rpDrawTile]) then
  exit;
 FMStream:=GetStream;
 if Not Assigned(FMStream) then
  exit;
 try
  adriver.GraphicExtent(FMStream,Result,dpires);
  LastExtent:=Result;
 finally
  if FMStream<>FStream then
   FMStream.free;
 end;
end;

end.
