{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       urepserver                                      }
{                                                       }
{       Report Manager Net Server implementation        }
{       Routines and main interface to implement        }
{       the Report Manager Server                       }
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

unit urepserver;


interface

{$I rpconf.inc}

uses
  SysUtils, Classes, IdUserAccounts, IdThreadMgr, IdThreadMgrDefault,
  IdBaseComponent, IdComponent,rpmdconsts,SyncObjs,
  IdTCPServer,
{$IFDEF USEBDE}
  dbtables,
{$ENDIF}
  rptranslator,rpmdshfolder,IniFiles,rpmdprotocol,
  rpreport,rppdfdriver, IdThreadMgrPool;

const
 DEFAULT_MILIS_PROGRESS=10000;

type
  TRpClient=class(TObject)  // Object holding data of client (see events)
    DNS: String;
    Connected,                           { Time of connect }
    LastAction  : TDateTime;             { Time of last transaction }
    Thread      : TIdPeerThread;               { Pointer to thread }
    Auth:boolean;
    Username:string;
    Password:string;
    CurrentAlias:String;
    CurrentPath:String;
    CurrentReport:TRpReport;
    FromPage,ToPage:integer;
    Copies:integer;
    CompressedPDF:boolean;
    APDFDriver:TRpPDFDriver;
    cancelled:boolean;
    procedure OnProgress(Sender:TRpReport;var docancel:boolean);
  public
    constructor Create;
    destructor Destroy;override;
  end;


  Tmodserver = class(TDataModule)
    RepServer: TIdTCPServer;
    UsMan: TIdUserManager;
    ThreadMan: TIdThreadMgrPool;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure RepServerExecute(AThread: TIdPeerThread);
    procedure RepServerConnect(AThread: TIdPeerThread);
    procedure RepServerDisconnect(AThread: TIdPeerThread);
  private
    { Private declarations }
    // Data for all clients
    Clients:TThreadList;
    FInitEvent:TEvent;
    EventName:String;
    FHostName:String;
    LAliases,LUsers:TStringList;
    FOnlog:TRpLogMessageEvent;
    FLogFilename:TFilename;
    FLogFile:TFileStream;
    FFileNameConfig:String;
    procedure InitConfig;
    procedure WriteLog(aMessage:WideString);
  public
    property LogFileName:TFileName read FLogFileName;
    property FileNameConfig:String read FFilenameconfig;
    { Public declarations }
    property HostName:String read FHostName;
  end;

function StartServer(OnLog:TRpLogMessageEvent):TModServer;
procedure StopServer(modserver:TModServer);

implementation

{$R *.xfm}

constructor TRpClient.Create;
begin
 CurrentReport:=TRpReport.Create(nil);
 CurrentReport.OnProgress:=OnProgress;
 CurrentReport.MilisProgres:=DEFAULT_MILIS_PROGRESS;
 APDFDriver:=TRpPdfDriver.Create;
{$IFDEF USEBDE}
 CurrentReport.DatabaseInfo.BDESession:=TSession.Create(CurrentReport);
 CurrentReport.DatabaseInfo.BDESession.AutoSessionName:=True;
 CurrentReport.DatabaseInfo.BDESession.Active:=True;
{$ENDIF}
{$IFDEF MSWINDOWS}
 CurrentReport.LoadFromFile('c:\prog\toni\cvsroot\reportman\reportman\repman\repsamples\sample6.rep');
{$ENDIF}
{$IFDEF LINUX}
 CurrentReport.LoadFromFile('/home/toni/cvsroot/reportman/repman/repsamples/sample6.rep');
{$ENDIF}
end;

destructor TRpClient.Destroy;
begin
 CurrentReport.Free;

 inherited Destroy;
end;

procedure TRpClient.OnProgress(Sender:TRpReport;var docancel:boolean);
var
 astring:WideString;
 CB:TRpComBLock;
begin
 // Sends the message progress to the thread
 astring:=IntToStr(Sender.CurrentSubReportIndex)+' '+SRpPage+':'+
 FormatFloat('####,####',Currentreport.PageNum)+':'
 +FormatFloat('####,####',Currentreport.RecordCount);
 CB:=GenerateCBErrorMessage(astring);
 Thread.Connection.WriteBuffer(CB,Sizeof(CB));
 docancel:=cancelled;
end;

function StartServer(onlog:TRpLogMessageEvent):TModServer;
var
 amod:TModServer;
{$IFDEF MSWINDOWS}
 wresult:TWaitResult;
{$ENDIF}
begin
 amod:=TModServer.Create(nil);
 try
  // Look for any other server running in this server
  // Check if the server is running
{$IFDEF MSWINDOWS}
  wresult:=amod.FInitEvent.WaitFor(100);
  if wresult<>wrSignaled then
   Raise Exception.Create(SRpServerAlreadyRunning);
{$ENDIF}
  amod.InitConfig;
  amod.RepServer.Active:=True;
  amod.FOnLog:=OnLog;
  amod.FHostname:=amod.RepServer.LocalName;
  amod.WriteLog(SRpServerStarted);
 except
  amod.free;
  raise;
 end;
 Result:=amod;
end;

procedure StopServer(modserver:TModServer);
begin
 if modserver.RepServer.Active then
 begin
  modserver.RepServer.Active:=false;
  // Releases the event
  modserver.WriteLog(SRpServerStoped);
  modserver.FInitEvent.SetEvent;
 end;
 modserver.Free;
end;


procedure Tmodserver.WriteLog(aMessage:WideString);
var
 messa:WideString;
 ansimessa:String;
begin
 messa:=FormatDateTime('dd/mm/yyyy hh:nn:ss - ',Now)+aMessage;
 if Assigned(FOnlog) then
  FOnLog(Self,messa);
 if Assigned(FLogFile) then
 begin
  ansimessa:=messa;
  ansimessa:=ansimessa+#10;
  FLogFile.Write(ansimessa[1],Length(ansimessa));
 end;
end;


procedure Tmodserver.DataModuleCreate(Sender: TObject);
begin
 Clients:=TTHreadList.Create;
 LAliases:=TStringList.Create;
 LAliases.CaseSensitive:=false;
 LUsers:=TStringList.Create;
 LUsers.CaseSensitive:=false;
 // Creates the event
 eventname:='REPORTMANRUNNINGEVENT';
 FInitEvent:=TEvent.Create(nil,false,true,eventname);
 // Gets the log file and try to create it
 FLogFilename:=Obtainininamecommonconfig('','','reportmanlog');
 if Not (FileExists(FLogFileName)) then
 begin
  try
   FLogFile:=TFileStream.Create(FLogFilename,fmOpenReadWrite or fmCreate);
  except
   // If fails try with local filename
   FLogFilename:=Obtainininamelocalconfig('','','reportmanlog');
   if Not (FileExists(FLogFileName)) then
    FLogFile:=TFileStream.Create(FLogFilename,fmOpenReadWrite or fmCreate);
  end;
  FLogFile.Free;
  FLogFile:=nil;
 end;
 FLogFile:=TFileStream.Create(FLogFilename,fmOpenReadWrite or fmShareDenyWrite);
 FLogFile.Seek(0,soFromEnd);
end;

// Read users and passwords
// Also read directories and aliases
procedure TmodServer.InitConfig;
var
 inif:TMemInifile;
begin
 Ffilenameconfig:=Obtainininamecommonconfig('','','reportmanserver');
 inif:=TMemInifile.Create(filenameconfig);
 try
  inif.CaseSensitive:=false;
  inif.ReadSection('USERS',lusers);
  inif.ReadSection('ALIASES',laliases);
  if lusers.IndexOfName('ADMIN')<0 then
   lusers.Add('ADMIN=');
 finally
  inif.free;
 end;
end;


procedure Tmodserver.DataModuleDestroy(Sender: TObject);
begin
 Clients.Free;
 LAliases.free;
 LUsers.free;
 FInitEvent.Free;
 if assigned(FLogFile) then
 begin
  FLogFile.Free;
  FLogFile:=nil;
 end;
end;

procedure Tmodserver.RepServerExecute(AThread: TIdPeerThread);
var
 CB:TRpComBlock;
 astream:TMemoryStream;
 alist:TStringList;
 username,password:string;
 correct:boolean;
 index:integer;
 ActClient:TRpClient;
 aarray:TDynamicRpComBlock;
 i:integer;
begin
 // Execution of commands
 if (AThread.Terminated) or (Not AThread.Connection.Connected) then
  exit;
 astream:=TMemoryStream.Create;
 try
  repeat
   AThread.Connection.ReadBuffer (CB, SizeOf (TRpComBlock));
   astream.Write(CB.Data,CB.Datasize);
  until CB.PacketNum=0;
  astream.Seek(0,soFromBeginning);
  ActClient := TRpClient(AThread.Data);
  ActClient.LastAction := Now;  // update the time of last action
  // if is a auth message return the key
  case CB.Command of
   repauth:
    begin
     alist:=TStringList.Create;
     try
      alist.LoadFromStream(astream);
      username:='Admin';
      password:='';
      if alist.count>0 then
      begin
       username:=Alist.Names[0];
       password:=Alist.Values[Alist.Names[0]];
      end;
     finally
      alist.free;
     end;
     // Looks if the user exists
     correct:=false;
     index:=LUsers.IndexOfName(username);
     if index>=0 then
     begin
      If LUsers.ValueFromIndex[index]=password then
       correct:=true;
     end;
     if correct then
     begin
      ActClient.UserName:=username;
      ActClient.Password:=password;
      ActClient.Auth:=True;
      // Sends the authorization message with a list of the aliases
      GenerateCBArray(repauth,LAliases,@aarray);
      for i:=0 to High(aarray) do
      begin
       AThread.Connection.WriteBuffer(aarray[i],Sizeof(CB));
      end;
     end
     else
     begin
      // Sends a error message
      ActClient.Auth:=False;
      CB:=GenerateCBErrorMessage(SRpAuthFailed);
      AThread.Connection.WriteBuffer(CB,Sizeof(CB));
     end;
    end;
   repgetusers:
    begin
     GenerateCBArray(repgetusers,lusers,@aarray);
     for i:=0 to High(aarray) do
     begin
      AThread.Connection.WriteBuffer(aarray[i],Sizeof(CB));
     end;
    end;
   repgetaliases:
    begin
     GenerateCBArray(repgetaliases,laliases,@aarray);
     for i:=0 to High(aarray) do
     begin
      AThread.Connection.WriteBuffer(aarray[i],Sizeof(CB));
     end;
    end;
   repexecutereportmeta:
    begin
     ActClient.cancelled:=false;
     ActClient.CurrentReport.PrintAll(ActClient.APDFDriver);
     astream.Clear;
     ActClient.CurrentReport.Metafile.SaveToStream(astream);
     astream.Seek(0,soFromBeginning);
     GenerateCBArray(repexecutereportmeta,astream,@aarray);
     for i:=0 to High(aarray) do
     begin
      AThread.Connection.WriteBuffer(aarray[i],Sizeof(CB));
     end;
    end;
   repexecutereportpdf:
    begin
     ActClient.cancelled:=false;
     ActClient.CurrentReport.PrintRange(ActClient.apdfdriver,false,ActClient.FromPage,ActClient.ToPage,ActClient.Copies);
     GenerateCBArray(repauth,ActClient.APDFDriver.PDFFile.MainPDF,@aarray);
     for i:=0 to High(aarray) do
     begin
      AThread.Connection.WriteBuffer(aarray[i],Sizeof(CB));
     end;
    end;
  end;
 finally
  astream.free;
 end;
end;

procedure Tmodserver.RepServerConnect(AThread: TIdPeerThread);
var
 NewClient: TRpClient;
begin
 NewClient:=TRpClient.Create;
 NewClient.DNS:= AThread.Connection.LocalName;
 NewClient.Connected:=Now;
 NewClient.LastAction:=NewClient.Connected;
 NewClient.Thread:=AThread;
 NewClient.Auth:=False;
 NewClient.FromPage:=1;
 NewClient.ToPage:=9999999;
 NewClient.CompressedPDF:=true;
 NewClient.Copies:=1;
 NewClient.UserName:='';
 NewClient.Password:='';
 AThread.Data:=NewClient;
 try
  Clients.LockList.Add(NewClient);
 finally
  Clients.UnlockList;
 end;
end;

procedure Tmodserver.RepServerDisconnect(AThread: TIdPeerThread);
var
 ActClient: TRpClient;
begin
  ActClient:= TRpClient(AThread.Data);
  try
   Clients.LockList.Remove(ActClient);
  finally
   Clients.UnlockList;
  end;
  ActClient.free;
  AThread.Data := nil;
end;

end.
