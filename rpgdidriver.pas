{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpgdidriver                                     }
{       TRpWinGDIDriver: Printer driver for  MSWindows  }
{                                                       }
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

unit rpgdidriver;

interface

uses classes,sysutils,windows,printers,rpmetafile,graphics,rpconsts;


type
 // TRpPreview GDI Driver prints to a Metafile canvas
 TRpWinGDIDriver=class(TInterfacedObject,IRpPrintDriver)
  public
   meta:TMetafile;
   Canvas:TCanvas;
   procedure NewDocument(report:TrpMetafileReport);stdcall;
   procedure EndDocument;stdcall;
   procedure AbortDocument;stdcall;
   procedure NewPage;stdcall;
   procedure EndPage;stdcall;
   procedure DrawObject(obj:TRpMetafileObject);stdcall;
   function AllowCopies:boolean;stdcall;
   procedure DrawMetaToBitmapStream(Width,Height:integer;stream:TStream);
  end;


implementation

procedure TRpWinGDIDriver.NewDocument(report:TrpMetafileReport);
begin
 if assigned(Canvas) then
 begin
  canvas.free;
  canvas:=nil;
 end;
 if assigned(meta) then
 begin
  meta.free;
  meta:=nil;
 end;
 meta:=TMetafile.Create;
 meta.Width:=report.CustomX;
 meta.Height:=report.CustomY;
 Canvas:=TMetafileCanvas.Create(meta,0);
end;

procedure TRpWinGDIDriver.EndDocument;
begin
 if assigned(canvas) then
 begin
  canvas.free;
  canvas:=nil;
 end;
end;

procedure TRpWinGDIDriver.AbortDocument;
begin
 if assigned(meta) then
  meta.free;
 meta:=nil;
end;

procedure TRpWinGDIDriver.NewPage;
begin
 meta.Clear;
 Canvas:=TMetafileCanvas.Create(meta,0);
end;

procedure TRpWinGDIDriver.EndPage;
begin
 if assigned(Canvas) then
 begin
  Canvas.free;
  Canvas:=nil;
 end;
end;

procedure TRpWinGDIDriver.DrawObject(obj:TRpMetafileObject);
begin
 if not Assigned(canvas) then
  Raise Exception.Create(SRpWinGDINotInit);
 case obj.Metatype of
  rpMetaText:
   begin
    Canvas.TextOut(obj.Left,obj.Top,obj.Text);
   end;
  rpMetaDraw:
   begin

   end;
  rpMetaImage:
   begin

   end;
 end;
end;

function TRpWinGDIDriver.AllowCopies:boolean;
begin
 Result:=false;
end;

procedure TrpWinGDIDriver.DrawMetaToBitmapStream(Width,Height:integer;stream:TStream);
var
 bitmap:TBitmap;
begin
 bitmap:=TBitmap.Create;
 try
  bitmap.PixelFormat:=pf24bit;
  bitmap.Width:=Width;
  bitmap.Height:=Height;
  bitmap.Canvas.Draw(0,0,meta);
  bitmap.SaveToStream(Stream);
 finally
  bitmap.Free;
 end;
end;

end.
