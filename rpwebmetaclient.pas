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
{$IFDEF USEVARIANTS}
 Types,
{$ENDIF}
 rpmetafile,rpfmainmetaviewvcl,rpmdconsts,
 IdHttp,rpgdidriver,rpmdprintconfigvcl,rpmdshfolder,
 rpfmetaviewvcl;

//const
// READ_TIMEOUT=10000;
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
   FShowProgress:Boolean;
   FShowPrintDialog:Boolean;
   FCopies:Integer;
   errormessage:String;
   procedure DoInstall;
   procedure SetCaption(Value:WideString);
  protected
   procedure Paint;override;
  public
   aForm:TWinControl;
   Meta:TFRpMetaVCL;
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
   property Copies:Integer read FCopies write FCopies default 1;
   property ShowProgress:Boolean read FShowProgress write FShowProgress
    default true;
   property ShowPrintDialog:Boolean read FShowPrintDialog
    write FShowPrintDialog default false;
  end;

procedure PrintHttpReport(httpstring:String);



implementation




procedure TRpWebMetaPrint.Execute;
var
 connect:TIdHttp;
 astream:TMemoryStream;
 metafile:TrpMetafileReport;
 frompage,topage,copies:integer;
 allpages,collate:boolean;
 rpPageSize:TPageSizeQt;
 okselected:Boolean;
begin
 errormessage:='';
 try
  if FPrinterconfig then
  begin
   ShowPrintersConfiguration;
  end
  else
  if install then
  begin
   DoInstall;
  end
  else
  begin
   connect:=TIdHttp.Create(nil);
   try
{$IFNDEF DOTNETD}
    connect.Port:=FPort;
{$ENDIF}
  //  connect.ReadTimeout:=READ_TIMEOUT;
    astream:=TMemoryStream.Create;
    try
     connect.Get(MetaUrl,astream);
     metafile:=TrpMetafileReport.Create(nil);
     try
      astream.Seek(0,soFromBeginning);
      metafile.LoadFromStream(astream);
      if preview then
      begin
       Meta:=PreviewMetafile(metafile,aform,ShowPrintDialog,false);
      end
      else
      begin
       // Prints the report
       rpPageSize.Custom:=metafile.PageSize<0;
       rpPageSize.Indexqt:=metafile.PageSize;
       rpPageSize.CustomWidth:=metafile.CustomX;
       rpPageSize.CustomHeight:=metafile.CustomY;
       frompage:=1;
       topage:=999999;
       allpages:=true;
       collate:=false;
       copies:=FCopies;
       rpgdidriver.PrinterSelection(metafile.PrinterSelect,metafile.papersource,metafile.duplex);
       rpgdidriver.PageSizeSelection(rpPageSize);
       rpgdidriver.OrientationSelection(metafile.orientation);

       okselected:=true;
       if ShowPrintDialog then
        okselected:=rpgdidriver.DoShowPrintDialog(allpages,frompage,topage,copies,collate);
       if okselected then
        rpgdidriver.PrintMetafile(metafile,SRpPrintingFile,FShowProgress,allpages,
         frompage,topage,copies,collate,
         GetDeviceFontsOption(metafile.PrinterSelect),metafile.PrinterSelect);
      end;
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
 except
  On E:Exception do
  begin
   errormessage:=E.Message;
   raise;
  end;
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
 FShowProgress:=True;
 FCopies:=1;
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
 if Length(errormessage)>0 then
 begin
  rec.Top:=rec.Top+30;
  Canvas.TextRect(rec,0,1,RM_VERSION);
  rec.Top:=rec.Top+30;
  Canvas.TextRect(rec,0,1,errormessage);
 end;
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
{$IFNDEF DOTNETD}
  connect.Port:=FPort;
{$ENDIF}
  astream:=TMemoryStream.Create;
  try
   sysdir:=GetTheSystemDirectory;
   connect.Get(MetaUrl+'/reportmanres.es',astream);
   if astream.size=0 then
    Raise Exception.Create(SRpNotFound+' - es '+MetaUrl);
   astream.Seek(0,soFromBeginning);
   astream.SaveToFile(sysdir+DIR_SEPARATOR+'reportmanres.es');
   //
   connect.Get(MetaUrl+'/reportmanres.cat',astream);
   if astream.size=0 then
    Raise Exception.Create(SRpNotFound+' - cat '+MetaUrl);
   astream.Seek(0,soFromBeginning);
   astream.SaveToFile(sysdir+DIR_SEPARATOR+'reportmanres.cat');
   //
   connect.Get(MetaUrl+'/reportmanres.fr',astream);
   if astream.size=0 then
    Raise Exception.Create(SRpNotFound+' - fr '+MetaUrl);
   astream.Seek(0,soFromBeginning);
   astream.SaveToFile(sysdir+DIR_SEPARATOR+'reportmanres.fr');
   //
   connect.Get(MetaUrl+'/reportmanres.pt',astream);
   if astream.size=0 then
    Raise Exception.Create(SRpNotFound+' - pt '+MetaUrl);
   astream.Seek(0,soFromBeginning);
   astream.SaveToFile(sysdir+DIR_SEPARATOR+'reportmanres.pt');
   //
   connect.Get(MetaUrl+'/reportmanres.de',astream);
   if astream.size=0 then
    Raise Exception.Create(SRpNotFound+' - de '+MetaUrl);
   astream.Seek(0,soFromBeginning);
   astream.SaveToFile(sysdir+DIR_SEPARATOR+'reportmanres.de');
   //
   connect.Get(MetaUrl+'/reportmanres.it',astream);
   if astream.size=0 then
    Raise Exception.Create(SRpNotFound+' - it '+MetaUrl);
   astream.Seek(0,soFromBeginning);
   astream.SaveToFile(sysdir+DIR_SEPARATOR+'reportmanres.it');
   //
   connect.Get(MetaUrl+'/reportmanres.en',astream);
   if astream.size=0 then
    Raise Exception.Create(SRpNotFound+' - en '+MetaUrl);
   astream.Seek(0,soFromBeginning);
   astream.SaveToFile(sysdir+DIR_SEPARATOR+'reportmanres.en');
{   connect.Get(MetaUrl+'/WebReportManX.ocx.manifest',astream);
   if astream.size=0 then
    Raise Exception.Create(SRpNotFound+' - '+MetaUrl);
   astream.Seek(0,soFromBeginning);
   astream.SaveToFile(sysdir+DIR_SEPARATOR+'WebReportManX.ocx.manifest');
}  finally
   astream.free;
  end;
 finally
  connect.free;
 end;
end;


end.
