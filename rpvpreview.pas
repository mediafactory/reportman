{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpvpreview                                      }
{       VCL Preview the report                          }
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

// Implement page size and showprint dialog and font style

unit rpvpreview;

interface

{$I rpconf.inc}

uses
  SysUtils,
  windows,
{$IFDEF USEVARIANTS}
  Types,
{$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,rpreport,rpmetafile, ComCtrls,
  rpgdidriver, ExtCtrls,Menus,
  ActnList, ImgList,Printers,rpconsts, ToolWin;
                       
type
  TFRpVPreview = class(TForm)
    BToolBar: TToolBar;
    ImageContainer: TScrollBox;
    AImage: TImage;
    ImageList1: TImageList;
    ActionList1: TActionList;
    AFirst: TAction;
    APrevious: TAction;
    ANext: TAction;
    ALast: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    EPageNum: TEdit;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    APrint: TAction;
    ToolButton6: TToolButton;
    ASave: TAction;
    SaveDialog1: TSaveDialog;
    ToolButton7: TToolButton;
    ACancel: TAction;
    BCancel: TButton;
    PBar: TProgressBar;
    AExit: TAction;
    BSeparator: TToolButton;
    ToolButton9: TToolButton;
    ToolButton8: TToolButton;
    AParams: TAction;
    ToolButton10: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AFirstExecute(Sender: TObject);
    procedure ANextExecute(Sender: TObject);
    procedure APreviousExecute(Sender: TObject);
    procedure ALastExecute(Sender: TObject);
    procedure EPageNumKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure APrintExecute(Sender: TObject);
    procedure ASaveExecute(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure ACancelExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure AExitExecute(Sender: TObject);
    procedure AParamsExecute(Sender: TObject);
  private
    { Private declarations }
    cancelled:boolean;
    printed:boolean;
    enableparams:boolean;
    procedure AppIdle(Sender:TObject;var done:boolean);
    procedure RepProgress(Sender:TRpReport;var docancel:boolean);
    procedure MetProgress(Sender:TRpMetafileReport;Position,Size:int64;page:integer);
    procedure DisableControls(enablebar:boolean);
    procedure EnableControls;
  public
    { Public declarations }
    pagenum:integer;
    report:TRpReport;
    gdidriver:TRpgdidriver;
    agdidriver:IRpPrintDriver;
    bitmap:TBitmap;
    procedure PrintPage;
  end;


function ShowPreview(report:TRpReport;caption:string):boolean;

implementation

uses rprfvparams, rppdfdriver;

{$R *.dfm}

function ShowPreview(report:TRpReport;caption:string):boolean;
var
 dia:TFRpVPreview;
 oldprogres:TRpProgressEvent;
 hasparams:boolean;
 i:integer;
begin
 dia:=TFRpVPreview.Create(Application);
 try
  dia.caption:=caption;
  oldprogres:=report.OnProgress;
  try
   dia.report:=report;
   hasparams:=false;
   for i:=0 to report.params.count-1 do
   begin
    if report.params.items[i].Visible then
    begin
     hasparams:=true;
     break;
    end;
   end;
   dia.AParams.Enabled:=hasparams;
   dia.enableparams:=hasparams;
   report.OnProgress:=dia.RepProgress;
   Application.OnIdle:=dia.AppIdle;
   dia.ShowModal;
   Result:=dia.printed;
  finally
   report.OnProgress:=oldprogres;
  end;
 finally
  dia.Free;
 end;
end;

procedure TFRpVPreview.PrintPage;
begin
 try
  if report.Metafile.PageCount>=pagenum then
  begin
   report.Metafile.CurrentPage:=pagenum-1;
  end
  else
  begin
   if Not report.LastPage then
   begin
    cancelled:=false;
    DisableControls(false);
    try
     while report.Metafile.PageCount<pagenum do
     begin
      // Canbe canceled
      if report.PrintNextPage then
      begin
       report.EndPrint;
       break;
      end;
     end;
    finally
     EnableControls;
    end;
   end;
   if report.Metafile.PageCount<pagenum then
    pagenum:=report.Metafile.PageCount;
   report.Metafile.CurrentPage:=pagenum-1;
  end;
  report.metafile.DrawPage(gdidriver);
  if Assigned(gdidriver.bitmap) then
  begin
   AImage.Width:=Round(gdidriver.bitmap.Width);
   AImage.Height:=Round(gdidriver.bitmap.Height);
   AImage.Picture.Bitmap.Assign(gdidriver.bitmap);
   AImage.Invalidate;
  end;
  EPageNum.Text:=IntToStr(PageNum);
 except
  EPageNum.Text:='0';
  raise;
 end;
end;

procedure TFRpVPreview.AppIdle(Sender:TObject;var done:boolean);
begin
 Application.OnIdle:=nil;
 done:=false;
 try
  report.OnProgress:=RepProgress;
  if report.TwoPass then
  begin
   CalcReportWidthProgress(report);
  end
  else
  begin
   report.BeginPrint(gdidriver);
  end;
  pagenum:=1;
  PrintPage;
  printed:=true;
 except
  on E:Exception do
  begin
   Close;
   Raise;
  end;
 end;
end;

procedure TFRpVPreview.FormCreate(Sender: TObject);
begin
 APrevious.ShortCut:=ShortCut(VK_PRIOR, []);
 ANext.ShortCut:=ShortCut(VK_NEXT, []);
 AFirst.ShortCut:=ShortCut(VK_HOME, []);
 ALast.ShortCut:=ShortCut(VK_END, []);
 gdidriver:=TRpgdidriver.Create;
 agdidriver:=gdidriver;
 bitmap:=TBitmap.Create;
 bitmap.PixelFormat:=pf32bit;
 AImage.Picture.Bitmap:=bitmap;
end;

procedure TFRpVPreview.FormDestroy(Sender: TObject);
begin
 report.EndPrint;
 bitmap.free;
end;

procedure TFRpVPreview.AFirstExecute(Sender: TObject);
begin
 pagenum:=1;
 PrintPage;
end;

procedure TFRpVPreview.ANextExecute(Sender: TObject);
begin
 inc(pagenum);
 PrintPage;
end;

procedure TFRpVPreview.APreviousExecute(Sender: TObject);
begin
 dec(pagenum);
 if pagenum<1 then
  pagenum:=1;
 PrintPage;
end;

procedure TFRpVPreview.ALastExecute(Sender: TObject);
begin
 pagenum:=MaxInt;
 PrintPage;
end;

procedure TFRpVPreview.EPageNumKeyPress(Sender: TObject; var Key: Char);
begin
 if Key=chr(13) then
 begin
  pagenum:=StrToInt(EPageNum.Text);
  PrintPage;
 end;
end;

procedure TFRpVPreview.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 gdidriver:=nil;
end;

procedure TFRpVPreview.APrintExecute(Sender: TObject);
var
 adone:boolean;
 allpages,collate:boolean;
 frompage,topage,copies:integer;
begin
 allpages:=true;
 collate:=report.CollateCopies;
 frompage:=1; topage:=999999;
 copies:=report.Copies;
 if Not DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
  exit;
 report.EndPrint;
 PrintReport(report,Caption,true,allpages,frompage,topage,copies,collate);
 AppIdle(Self,adone);
end;

procedure TFRpVPreview.ASaveExecute(Sender: TObject);
var
 oldonprogress:TRpMetafileStreamProgres;
 adone:boolean;
begin
 // Saves the metafile
 if SaveDialog1.Execute then
 begin
  oldonprogress:=report.Metafile.OnProgress;
  try
   report.Metafile.OnProgress:=MetProgress;
   DisableControls(true);
   try
    if SaveDialog1.FilterIndex=1 then
    begin
     ALastExecute(Self);
     report.Metafile.SaveToFile(SaveDialog1.Filename)
    end
    else
     if SaveDialog1.FilterIndex in [2,3] then
     begin
      report.EndPrint;
      ExportReportToPDF(report,SaveDialog1.Filename,true,true,1,9999999,
       true,SaveDialog1.Filename,SaveDialog1.FilterIndex=2);
      AppIdle(Self,adone);
     end;
   finally
    EnableControls;
   end;
  finally
   report.Metafile.OnProgress:=oldonprogress;
  end;
 end;
end;

procedure TFRpVPreview.RepProgress(Sender:TRpReport;var docancel:boolean);
begin
 BCancel.Caption:=IntToStr(Sender.CurrentSubReportIndex)+' '+SRpPage+':'+
  FormatFloat('####,####',report.PageNum)+':'
  +FormatFloat('####,####',report.RecordCount)+'-'+SRpCancel;
{$IFDEF MSWINDOWS}
 if ((GetKeyState(VK_ESCAPE) AND $80)>0) then
  cancelled:=true;
{$ENDIF}
 Application.ProcessMessages;
 if cancelled then
  docancel:=true;
end;

procedure TFRpVPreview.BCancelClick(Sender: TObject);
begin
 cancelled:=true;
end;

procedure TFRpVPreview.ACancelExecute(Sender: TObject);
begin
 cancelled:=true;
end;

procedure TFRpVPreview.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
 if Not ANext.Enabled then
 begin
  cancelled:=true;
 end;
end;

procedure TFRpVPreview.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
 increment:integer;
begin
 if (ssShift in Shift) then
  increment:=1
 else
  increment:=ImageContainer.VertScrollBar.Increment;
 if Key=VK_DOWN then
 begin
  if ImageContainer.VertScrollBar.Position+increment>ImageContainer.VertScrollBar.Range-ImageContainer.Height then
   ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Range-ImageContainer.Height+increment
  else
   ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Position+Increment;
 end;
 if Key=VK_UP then
 begin
  ImageContainer.VertScrollBar.Position:=ImageContainer.VertScrollBar.Position-Increment;
 end;
 if Key=VK_RIGHT then
 begin
  if ImageContainer.HorzScrollBar.Position+increment>ImageContainer.HorzScrollBar.Range-ImageContainer.Width then
   ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Range-ImageContainer.Width+increment
  else
   ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Position+Increment;
 end;
 if Key=VK_LEFT then
 begin
  ImageContainer.HorzScrollBar.Position:=ImageContainer.HorzScrollBar.Position-Increment;
 end;
end;

procedure TFRpVPreview.DisableControls(enablebar:boolean);
begin
 BCancel.Left:=BSeparator.Left+BSeparator.Width;
 BCancel.Visible:=true;
 AFirst.Enabled:=false;
 ALast.Enabled:=false;
 APrint.Enabled:=false;
 ANext.Enabled:=false;
 APrevious.Enabled:=false;
 EPageNum.Enabled:=false;
 ASave.Enabled:=false;
 AParams.Enabled:=false;
 PBar.Position:=0;
 PBar.Visible:=enablebar;
end;

procedure TFRpVPreview.EnableControls;
begin
 BCancel.Visible:=false;
 AFirst.Enabled:=true;
 ALast.Enabled:=true;
 APrint.Enabled:=true;
 ANext.Enabled:=true;
 APrevious.Enabled:=true;
 EPageNum.Enabled:=true;
 ASave.Enabled:=true;
 AParams.Enabled:=enableparams;
 PBar.Visible:=false;
end;


procedure TFRpVPreview.MetProgress(Sender:TRpMetafileReport;Position,Size:int64;page:integer);
begin
 BCancel.Caption:=SRpPage+':'+FormatFloat('####,#####',page)+
  ' -'+FormatFloat('######,####',Position div 1024)+SRpKbytes+' '+SrpCancel;
 if Position=size then
 begin
  PBar.Position:=page;
  PBar.Max:=Sender.PageCount;
 end
 else
 begin
  PBar.Position:=Position;
  PBar.Max:=Size;
 end;
 Application.ProcessMessages;
{$IFDEF MSWINDOWS}
 if ((GetKeyState(VK_ESCAPE) AND $80)>0) then
  cancelled:=true;
{$ENDIF}
 if cancelled then
  Raise Exception.Create(SRpOperationAborted);
end;


procedure TFRpVPreview.AExitExecute(Sender: TObject);
begin
 Close;
end;

procedure TFRpVPreview.AParamsExecute(Sender: TObject);
var
 adone:boolean;
begin
 if ShowUserParams(report) then
 begin
  // Reexecutes the report
  AppIdle(Self,adone);
 end;
end;

end.