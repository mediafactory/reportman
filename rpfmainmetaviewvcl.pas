{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpfmainMetaviewvcl                              }
{       TFRpMetaVCL                                     }
{       A form to include the frame for                 }
{        report metafiles                               }
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

unit rpfmainmetaviewvcl;

interface

{$I rpconf.inc}

uses
  SysUtils,Inifiles,
  Windows,Dialogs,rpgdidriver,ShellApi,rpgraphutilsvcl,
  Types, Classes, Graphics, Controls, Forms,
  StdCtrls,rpmetafile, ComCtrls,ExtCtrls,rpmdclitreevcl,
  ActnList, ImgList,Printers,rpmdconsts,rptypes, Menus,
  rpmdfaboutvcl,rpmdshfolder,rpmdprintconfigvcl,
  ToolWin,rpfmetaviewvcl;

type
  TFRpMainMetaVCL = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MFrame:TFRpMetaVCL;
  end;

var
 FRpMainMetaVCL:TFRpMainMetaVCL;

procedure PreviewMetafile(metafile:TRpMetafileReport;aform:TWinControl);

implementation

uses rppdfdriver;

{$R *.dfm}

procedure PreviewMetafile(metafile:TRpMetafileReport;aform:TWinControl);
var
 dia:TFRpMainMetaVCL;
 memstream:TMemoryStream;
 MFrame:TFRpMetaVCL;
 FForm:TWinControl;
begin
 if not assigned(aform) then
 begin
  dia:=TFRpMainMetaVCL.Create(Application);
  MFrame:=dia.MFrame;
  FForm:=dia;
 end
 else
 begin
  dia:=nil;
  MFrame:=TFRpMetaVCL.Create(aform);
  MFrame.Parent:=aform;
  MFrame.SetMenu:=False;
  MFrame.AForm:=aform;
  FForm:=aform;
 end;
 try
  memstream:=TMemoryStream.Create;
  try
   metafile.SaveToStream(memstream);
   memstream.Seek(0,soFromBeginning);
   MFrame.metafile.LoadFromStream(memstream);
   MFrame.ASave.Enabled:=True;
   MFrame.APrint.Enabled:=True;
   MFrame.AFirst.Enabled:=True;
   MFrame.APrevious.Enabled:=True;
   MFrame.ANext.Enabled:=True;
   MFrame.ALast.Enabled:=True;
   MFrame.pagenum:=1;
   MFrame.AViewConnect.Checked:=false;
   MFrame.AViewConnect.Enabled:=false;
   MFrame.Splitter1.Visible:=false;
{$IFNDEF FORWEBAX}
   MFrame.clitree.visible:=false;
{$ENDIF}
   if metafile.PreviewWindow=spwMaximized then
   begin
    if (FForm is TForm) then
     TForm(FForm).WindowState:=wsMaximized;
   end;
   MFrame.AScale100.Checked:=False;
   MFrame.AScaleFull.Checked:=False;
   MFrame.AScaleWide.Checked:=False;
   case metafile.PreviewStyle of
    spNormal:
     begin
      MFrame.AScale100.Checked:=True;
      MFrame.gdidriver.PreviewStyle:=spNormal;
     end;
    spEntirePage:
     begin
      MFrame.AScaleFull.Checked:=True;
      MFrame.gdidriver.PreviewStyle:=spEntirePage;
     end
    else
      MFrame.AScaleWide.Checked:=True;
   end;
   MFrame.PrintPage;
   MFrame.FormResize(dia);
   if not assigned(aform) then
    dia.ShowModal;
  finally
   memstream.free;
  end;
 finally
  if not assigned(aform) then
   dia.free;
 end;
end;


procedure TFRpMainMetaVCL.FormCreate(Sender: TObject);
begin
 MFrame:=TFRpMetaVCL.Create(Self);
 MFrame.AForm:=self;
 MFrame.Parent:=Self;
 Caption:=SRpRepMetafile;
end;


end.
