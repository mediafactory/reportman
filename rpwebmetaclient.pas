{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpwebmetaclient                                 }
{       Metafile reading and printing                   }
{       From a http address                             }
{                                                       }
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

// Install lang boolean
// Language install

unit rpwebmetaclient;


interface

{$I rpconf.inc}

uses classes,SysUtils,Windows,graphics,controls,forms,
 rptypes,
{$IFNDEF USEVARIANTS}
 Types
{$ENDIF}
 rpmetafile,rpreport,rpfmainmetaviewvcl,rpmdconsts,
 IdHttp,rpgdidriver,rpmdprintconfigvcl,rpmdshfolder;


type
 TRpWebMetaPrint=class(TCustomControl)
  private
   FCaption:WideString;
   FPrinterConfig:Boolean;
   FFontName:String;
   FFontSize:integer;
   FMetaUrl:String;
   FPort:integer;
   FPreview:Boolean;
   FInstall:Boolean;
   procedure DoInstall;
   procedure SetCaption(Value:WideString);
  protected
   procedure Paint;override;
  public
   aForm:TWinControl;
   constructor Create(AOwner:TComponent);override;
   procedure Execute;
  published
   property Left;
   property Top;
   property Width;
   property Height;
   property Install:Boolean read FInstall write FInstall;
   property PrinterConfig:Boolean read FPrinterConfig write FPrinterConfig;
   property Caption:WideString read FCaption write SetCaption;
   property MetaUrl:String read FMetaUrl write FMetaUrl;
   property Port:integer read FPort write FPort default 90;
   property FontSize:Integer read FFontSize write FFontSize default 0;
   property FontName:String read FFontName write FFontName;
   property Preview:Boolean read FPreview write FPreview default false;
  end;

procedure PrintHttpReport(httpstring:String);



implementation




procedure TRpWebMetaPrint.Execute;
var
 connect:TIdHttp;
 astream:TMemoryStream;
 metafile:TrpMetafileReport;
begin
 if FPrinterconfig then
 begin
  ShowPrintersConfiguration;
  exit;
 end;
 if install then
 begin
  DoInstall;
  exit;
 end;
 connect:=TIdHttp.Create(nil);
 try
  connect.Port:=FPort;
  astream:=TMemoryStream.Create;
  try
   connect.Get(MetaUrl,astream);
   metafile:=TrpMetafileReport.Create(nil);
   try
    astream.Seek(0,soFromBeginning);
    metafile.LoadFromStream(astream);
    if preview then
    begin
     PreviewMetafile(metafile,aform);
    end
    else
     rpgdidriver.PrintMetafile(metafile,'Printing',true,true,0,1,1,true,false);
   finally
    metafile.free;
   end;
  finally
   astream.free;
  end;
 finally
  connect.free;
 end;
end;

procedure TRpWebMetaPrint.SetCaption(Value:WideString);
begin
 FCaption:=Value;
 Invalidate;
end;

constructor TRpWebMetaPrint.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FPreview:=false;
 FFontSize:=0;
 FPort:=80;
end;

procedure TRpWebMetaPrint.Paint;
var
 rec:TRect;
begin
 if FontSize>0 then
  Font.Size:=FontSize;
 if Length(FFontName)>0 then
  Font.Name:=FontName;
 rec:=GetClientRect;
 Canvas.Brush.Style:=bsClear;
 Canvas.Pen.Color:=clWindowText;
 Canvas.Rectangle(rec.Left,rec.Top,rec.Right,rec.Bottom);
 Canvas.Font.Color:=clWindowText;
 Canvas.Brush.Color:=clBtnFace;
 Canvas.TextRect(rec,0,1,Caption);
end;

procedure PrintHttpReport(httpstring:String);
var
 connect:TIdHttp;
 astream:TMemoryStream;
 metafile:TrpMetafileReport;
begin
 connect:=TIdHttp.Create(nil);
 try
  astream:=TMemoryStream.Create;
  try
   connect.Get(httpstring,astream);
   metafile:=TrpMetafileReport.Create(nil);
   try
    astream.Seek(0,soFromBeginning);
    metafile.LoadFromStream(astream);
    rpgdidriver.PrintMetafile(metafile,'Printing',true,true,0,1,1,true,false);
   finally
    metafile.free;
   end;
  finally
   astream.free;
  end;
 finally
  connect.free;
 end;
end;

procedure TRpWebMetaPrint.DoInstall;
var
 connect:TIdHttp;
 sysdir:String;
 astream:TMemoryStream;
begin
 // Install need the url to install languages from
 connect:=TIdHttp.Create(nil);
 try
  connect.Port:=FPort;
  astream:=TMemoryStream.Create;
  try
   sysdir:=GetTheSystemDirectory;
   connect.Get(MetaUrl+'/reportmanres.es',astream);
   if astream.size=0 then
    Raise Exception.Create(SRpNotFound+' - '+MetaUrl);
   astream.Seek(0,soFromBeginning);
   astream.SaveToFile(sysdir+DIR_SEPARATOR+'reportmanres.es');
  finally
   astream.free;
  end;
 finally
  connect.free;
 end;
end;


end.
