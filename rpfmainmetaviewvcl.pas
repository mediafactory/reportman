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

procedure PreviewMetafile(metafile:TRpMetafileReport);

implementation

uses rppdfdriver;

{$R *.dfm}

procedure PreviewMetafile(metafile:TRpMetafileReport);
var
 dia:TFRpMainMetaVCL;
 memstream:TMemoryStream;
begin
 dia:=TFRpMainMetaVCL.Create(Application);
 try
  memstream:=TMemoryStream.Create;
  try
   metafile.SaveToStream(memstream);
   memstream.Seek(0,soFromBeginning);
   dia.MFrame.metafile.LoadFromStream(memstream);
   dia.MFrame.ASave.Enabled:=True;
   dia.MFrame.APrint.Enabled:=True;
   dia.MFrame.AFirst.Enabled:=True;
   dia.MFrame.APrevious.Enabled:=True;
   dia.MFrame.ANext.Enabled:=True;
   dia.MFrame.ALast.Enabled:=True;
   dia.MFrame.pagenum:=1;
   dia.MFrame.AViewConnect.Checked:=false;
   dia.MFrame.AViewConnect.Enabled:=false;
   dia.MFrame.Splitter1.Visible:=false;
   dia.MFrame.clitree.visible:=false;
   if metafile.PreviewWindow=spwMaximized then
    dia.WindowState:=wsMaximized;
   dia.MFrame.AScale100.Checked:=False;
   dia.MFrame.AScaleFull.Checked:=False;
   dia.MFrame.AScaleWide.Checked:=False;
   case metafile.PreviewStyle of
    spNormal:
     begin
      dia.MFrame.AScale100.Checked:=True;
      dia.MFrame.gdidriver.PreviewStyle:=spNormal;
     end;
    spEntirePage:
     begin
      dia.MFrame.AScaleFull.Checked:=True;
      dia.MFrame.gdidriver.PreviewStyle:=spEntirePage;
     end
    else
      dia.MFrame.AScaleWide.Checked:=True;
   end;
   dia.MFrame.PrintPage;
   dia.MFrame.FormResize(dia);
   dia.ShowModal;
  finally
   memstream.free;
  end;
 finally
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
