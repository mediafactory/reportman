{*******************************************************}
{                                                       }
{       Report Manager Service data module              }
{                                                       }
{       urepservice                                     }
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

unit urepservice;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  urepserver;

type
  TReportService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServicePause(Sender: TService; var Paused: Boolean);
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceContinue(Sender: TService; var Continued: Boolean);
  private
    { Private declarations }
    amod:TModServer;
    procedure Onlog(Sender:TObject;aMessage:WideString);
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  ReportService: TReportService;

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ReportService.Controller(CtrlCode);
end;

function TReportService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TReportService.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
 if Assigned(amod) then
 begin
  amod.free;
  amod:=nil;
 end;
 amod:=StartServer(OnLog);
end;

procedure TReportService.Onlog(Sender:TObject;aMessage:WideString);
begin
 LogMessage(aMessage,EVENTLOG_INFORMATION_TYPE,0);
end;


procedure TReportService.ServiceShutdown(Sender: TService);
begin
 amod.free;
 amod:=nil;
end;

procedure TReportService.ServicePause(Sender: TService;
  var Paused: Boolean);
begin
 amod.free;
 amod:=nil;
 Paused:=True;
end;

procedure TReportService.ServiceExecute(Sender: TService);
begin
 if Assigned(amod) then
 begin
  amod.free;
  amod:=nil;
 end;
 amod:=StartServer(OnLog);
end;

procedure TReportService.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
 amod.free;
 amod:=nil;
 Stopped:=True;
end;

procedure TReportService.ServiceContinue(Sender: TService;
  var Continued: Boolean);
begin
 if Assigned(amod) then
 begin
  amod.free;
  amod:=nil;
 end;
 amod:=StartServer(OnLog);
 Continued:=True;
end;

end.
