{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rppreviewcontrol                                }
{       VCL Preview control                             }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2005 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rppreviewcontrol;

interface

uses Classes,Graphics,Controls,Forms,rppreviewmeta,rpbasereport,rpgdidriver,
 rpmetafile;

type
 TRpPreviewControl=class(TRpPreviewMeta)
  private
   FReport:TRpBasereport;
   procedure SetReport(Avalue:TRpBaseReport);
  protected
   procedure Notification(AComponent:TComponent;Operation:TOperation);override;
  public
   procedure RefreshMetafile;override;
   constructor Create(AOwner:TComponent);override;
   destructor Destroy;override;
   property Report:TRpBaseReport read FReport write SetReport;
  end;

implementation

constructor TRpPreviewControl.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

end;

destructor TRpPreviewControl.Destroy;
begin
 Metafile:=nil;
 if Assigned(FReport) then
  Report.EndPrint;
 inherited Destroy;
end;

procedure TRpPreviewControl.SetReport(Avalue:TRpBaseReport);
var
  adriver:IRpPrintDriver;
begin
 if Assigned(FReport) then
  Report.EndPrint;
 FReport:=Avalue;
 if Assigned(FReport) then
 begin
  adriver:=prdriver_internal;
  adriver._AddRef;
  FReport.BeginPrint(adriver);
  Metafile:=FReport.metafile;
 end;
end;

procedure TRpPreviewControl.RefreshMetafile;
begin
 Report:=FReport;
 inherited RefreshMetafile;
end;

procedure TRpPreviewControl.Notification(AComponent:TComponent;Operation:TOperation);
begin
 inherited Notification(AComponent,Operation);
 if assigned(Report) then
 begin
  if Operation=OpRemove then
  begin
   if (AComponent=Report) then
   begin
    Report:=nil;
   end;
  end;
 end;
end;

end.
