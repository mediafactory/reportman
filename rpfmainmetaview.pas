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

procedure PreviewMetafile(metafile:TRpMetafileReport;aform:TForm;ShowPrintDialog:Boolean);

implementation

uses rpprintdia,rppdfdriver;

{$R *.xfm}

procedure PreviewMetafile(metafile:TRpMetafileReport;aform:TForm;ShowPrintDialog:Boolean);
var
 dia:TFRpMainMeta;
 memstream:TMemoryStream;
 MFrame:TFRpMeta;
 FForm:TForm;
begin
 if not assigned(aform) then
 begin
  dia:=TFRpMainMeta.Create(Application);
  MFrame:=dia.MFrame;
  FForm:=dia;
 end
 else
 begin
  dia:=nil;
  MFrame:=TFRpMeta.Create(aform);
  MFrame.Parent:=aform;
  MFrame.SetMenu:=false;
  MFrame.AForm:=aform;
  FForm:=aform;
 end;
 try
  MFrame.ShowPrintDialog:=ShowPrintDialog;
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
   MFrame.printerindex:=metafile.PrinterSelect;
   MFrame.UpdatePrintSel;
   MFrame.clitree.visible:=false;
   if metafile.PreviewWindow=spwMaximized then
    FForm.WindowState:=wsMaximized;
   MFrame.AScale100.Checked:=False;
   MFrame.AScaleFull.Checked:=False;
   MFrame.AScaleWide.Checked:=False;
   case metafile.PreviewStyle of
    spNormal:
     begin
      MFrame.AScale100.Checked:=True;
      MFrame.qtdriver.PreviewStyle:=spNormal;
     end;
    spEntirePage:
     begin
      MFrame.AScaleFull.Checked:=True;
      MFrame.qtdriver.PreviewStyle:=spEntirePage;
     end
    else
      MFrame.AScaleWide.Checked:=True;
   end;
   MFrame.PrintPage;
   MFrame.FormResize(dia);
   if Not Assigned(aform) then
    dia.ShowModal;
  finally
   memstream.free;
  end;
 finally
  if not assigned(aform) then
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
