{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rplabelitem                                     }
{       TRpLabel printable component constant text      }
{       TRpExpression printable expression              }
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

unit rpdrawitem;

interface

uses Sysutils,Classes,rptypes,rpprintitem,rpconsts,rpmetafile;

const
 DEF_DRAWWIDTH=500;
type
 TRpShape=class(TRpCommonPosComponent)
  private
   FBrushStyle:integer;
   FBrushColor:integer;
   FPenStyle:integer;
   FPenColor:integer;
   FShape:integer;
   FPenWidth:integer;
  public
   constructor Create(AOwner:TComponent);override;
   destructor Destroy;override;
   procedure Print(aposx,aposy:integer;metafile:TRpMetafileReport);override;
  published
   property BrushStyle:integer read FBrushStyle write FBrushStyle default 0;
   property BrushColor:integer read FBrushColor write FBrushColor default $FFFFFF;
   property PenStyle:integer read FPenStyle write FPenStyle default 0;
   property PenColor:integer read FPenColor write FPenColor default 0;
   property Shape:integer read FShape write FShape default 0;
   property PenWidth:integer read FPenWidth write FPenWidth default 0;

  end;

 TRpImage=class(TRpCommonPosComponent)
  private
   FExpression:WideString;
   FStream:TMemoryStream;
   FDrawStyle:TRpImageDrawStyle;
   procedure ReadStream(AStream:TStream);
   procedure WriteStream(AStream:TStream);
  public
   constructor Create(AOwner:TComponent);override;
   procedure SetStream(Value:TMemoryStream);
   destructor Destroy;override;
  protected
   procedure DefineProperties(Filer: TFiler);override;
  public
   property Stream:TMemoryStream read FStream write SetStream;
  published
   property Expression:WideString read FExpression write FExpression;
   property DrawStyle:TRpImageDrawStyle read FDrawStyle write FDrawStyle
    default rpDrawCrop;
  end;
implementation

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

procedure TRpShape.Print(aposx,aposy:integer;metafile:TRpMetafileReport);
var
 Text:string;
begin
 metafile.Pages[metafile.CurrentPage].NewDrawObject(aposy+PosY,aposx+PosX,Width,Height,
  Shape,BrushStyle,BrushColor,PenStyle,PenWidth,PenColor);
end;

constructor TRpImage.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 Width:=DEF_DRAWWIDTH;
 Height:=DEF_DRAWWIDTH;
 FStream:=TMemoryStream.Create;
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

procedure TRpImage.DefineProperties(Filer: TFiler);
begin
 Filer.DefineBinaryProperty('Stream', ReadStream, WriteStream, true);
end;

procedure TRpImage.ReadStream(AStream:TStream);
var
 ssize:Int64;
begin
 if (sizeof(ssize)<>AStream.Read(ssize,sizeof(ssize))) then
  Raise Exception.Create(SRpInvalidStreaminRpImage);
 FStream.SetSize(ssize);
 FStream.Seek(soFromBeginning,0);
 if ssize<>AStream.Read(FStream.memory^,ssize) then
  Raise Exception.Create(SRpInvalidStreaminRpImage);
end;

procedure TRpImage.WriteStream(AStream:TStream);
var
 ssize:Int64;
begin
 ssize:=FStream.Size;
 AStream.Write(ssize,sizeof(ssize));
 FStream.Seek(soFromBeginning,0);
 AStream.Write(FStream.Memory^,ssize);
end;

end.
