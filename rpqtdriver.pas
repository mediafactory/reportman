{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpgdidriver                                     }
{       TRpQTDriver: Printer driver for  QT Libs        }
{       can be used for windows and linux               }
{       it includes printer and bitmap support          }
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

unit rpqtdriver;

interface

uses classes,sysutils,rpmetafile,rpconsts,QGraphics,QForms,
 rpmunits,QPrinters;


type
 TRpQtDriver=class(TInterfacedObject,IRpPrintDriver)
  private
   intdpix,intdpiy:integer;
  public
   bitmap:TBitmap;
   dpi:integer;
   toprinter:boolean;
   procedure NewDocument(report:TrpMetafileReport);stdcall;
   procedure EndDocument;stdcall;
   procedure AbortDocument;stdcall;
   procedure NewPage;stdcall;
   procedure EndPage;stdcall;
   procedure DrawObject(obj:TRpMetafileObject);stdcall;
   function AllowCopies:boolean;stdcall;
   constructor Create;
  end;


implementation

constructor TRpQtDriver.Create;
begin
 // By default 1:1 scale
 dpi:=Screen.PixelsPerInch;
end;

procedure TRpQtDriver.NewDocument(report:TrpMetafileReport);
begin
 if ToPrinter then
 begin
  printer.BeginDoc;
  intdpix:=printer.XDPI;
  intdpiy:=printer.YDPI;
 end
 else
 begin
  if assigned(bitmap) then
  begin
   bitmap.free;
   bitmap:=nil;
  end;
  // The scale is the screen resolution by default
  // in dots per inch but the paper size is in tenths of
  // milimeter
  bitmap:=TBitmap.Create;
  bitmap.PixelFormat:=pf32bit;
  bitmap.Width:=Round(report.CustomX*dpi/(CMS_PER_INCHESS*100));
  bitmap.Height:=Round(report.CustomY*dpi/(CMS_PER_INCHESS*100));
 end;
end;

procedure TRpQtDriver.EndDocument;
begin
 if toprinter then
 begin
  printer.EndDoc;
 end
 else
 begin
  // Does nothing because the last bitmap can be usefull
 end;
end;

procedure TRpQtDriver.AbortDocument;
begin
 if toprinter then
 begin
  printer.Abort;
 end
 else
 begin
  if assigned(bitmap) then
   bitmap.free;
  bitmap:=nil;
 end;
end;

procedure TRpQtDriver.NewPage;
begin
 if toprinter then
 begin
  printer.NewPage;
 end
 else
 begin
  bitmap.free;
  bitmap:=nil;
  bitmap:=TBitmap.create;
  bitmap.PixelFormat:=pf32bit;
 end;
end;

procedure TRpQtDriver.EndPage;
begin
 // Does nothing
end;

procedure TRpQtDriver.DrawObject(obj:TRpMetafileObject);
var
 x,y:integer;
 dpix,dpiy:integer;
 Canvas:TCanvas;
begin
 if (toprinter) then
 begin
  if not printer.Printing then
   Raise Exception.Create(SRpQtDriverNotInit);
  dpix:=intdpix;
  dpiy:=intdpiy;
  Canvas:=printer.canvas;
 end
 else
 begin
  if not Assigned(bitmap) then
   Raise Exception.Create(SRpQtDriverNotInit);
  Canvas:=bitmap.canvas;
  dpix:=dpi;
  dpiy:=dpi;
 end;
 case obj.Metatype of
  rpMetaText:
   begin
    // Switch to device points
    x:=round(obj.Left*dpix/TWIPS_PER_INCHESS);
    y:=round(obj.Top*dpiy/TWIPS_PER_INCHESS);
    Canvas.TextOut(x,y,obj.Text);
   end;
  rpMetaDraw:
   begin

   end;
  rpMetaImage:
   begin

   end;
 end;
end;

function TRpQtDriver.AllowCopies:boolean;
begin
 Result:=false;
end;


end.

