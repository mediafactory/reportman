{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpfmainMetaview                                 }
{       TFRpMeta                                        }
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

unit rpfmainmetaview;

interface

{$I rpconf.inc}

uses
  SysUtils,Inifiles,
{$IFDEF MSWINDOWS}
  Windows,Dialogs,rpgdidriver,
{$ENDIF}
  Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls,rpmetafile, QComCtrls,rpqtdriver, QExtCtrls,rpmdclitree,
  QActnList, QImgList,QPrinters,Qt,rpmdconsts,rptypes, QMenus,
  rpmdfabout,QTypes,QStyle,rpmdshfolder,rpmdprintconfig,
  rpmdfhelpform,rpfmetaview;

type
  TFRpMainMeta = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MFrame:TFRpMeta;
  end;

var
 FRpMainMeta:TFRpMainMeta;

procedure PreviewMetafile(metafile:TRpMetafileReport);

implementation

uses rpprintdia,rppdfdriver;

{$R *.xfm}

procedure PreviewMetafile(metafile:TRpMetafileReport);
var
 dia:TFRpMainMeta;
 memstream:TMemoryStream;
begin
 dia:=TFRpMainMeta.Create(Application);
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
      dia.MFrame.qtdriver.PreviewStyle:=spNormal;
     end;
    spEntirePage:
     begin
      dia.MFrame.AScaleFull.Checked:=True;
      dia.MFrame.qtdriver.PreviewStyle:=spEntirePage;
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



procedure TFRpMainMeta.FormCreate(Sender: TObject);
begin
 MFrame:=TFRpMeta.Create(Self);
 MFrame.AForm:=self;
 MFrame.Parent:=Self;
 Caption:=SRpRepMetafile;
end;


end.
