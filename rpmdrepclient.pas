{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpmdrepclient                                   }
{                                                       }
{       Report Manager Client for Report Manager Server }
{       Routines and main interface to implement        }
{       the report client                               }
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

unit rpmdrepclient;

interface

uses
  SysUtils, Classes, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, rptranslator,rpmdprotocol,rpmdconsts,SyncObjs;

type
  TRpClientHandleThread=class;


  Tmodclient = class(TDataModule)
    RepClient: TIdTCPClient;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure RepClientDisconnected(Sender: TObject);
  private
    { Private declarations }
    FStream:TMemoryStream;
    FEndReport:TEvent;
    FAuthorized:Boolean;
    FOnError:TRpLogMessageEvent;
    FOnLog:TRpLogMessageEvent;
    FOnExecute:TNotifyEvent;
    FAliases:TStringList;
    FOnAuthorization:TNotifyEvent;
    ClientHandleThread:TRpClientHandleThread;
  public
    { Public declarations }
    asynchronous:boolean;
    procedure Execute;
    property Aliases:TStringList read FAliases;
    property OnError:TRpLogMessageEvent read FOnError write FOnError;
    property OnLog:TRpLogMessageEvent read FOnLog write FOnLog;
    property OnAuthorization:TNotifyEvent read FOnAuthorization write FOnAuthorization;
    property OnExecute:TNotifyEvent read FOnExecute write FOnExecute;
    property Authorized:boolean read FAuthorized;
    property Stream:TMemoryStream read FStream;
  end;


  TRpClientHandleThread = class(TThread)
  private
   amod:TModClient;
   CB:PRpComBlock;
   data:TMemoryStream;
   FEndreport:TEvent;
   syncexec:boolean;
   procedure HandleInput;
   procedure LogReceive;
  protected
   procedure Execute; override;
  end;


function Connect(hostname:string;user:string;password:string):TModClient;
procedure Disconnect(amod:TModClient);

implementation

{$R *.xfm}


procedure TRpClientHandleThread.LogReceive;
begin
 if Assigned(amod.FOnLog) then
  amod.FOnLog(amod,SRpReceivedPacket);
end;

procedure TRpClientHandleThread.HandleInput;
var
 amessage:WideString;
begin
 // Handles the input this is VCLX thread safe
 case CB.Command of
  reperror:
   if Assigned(amod.OnError) then
   begin
    SetLength(amessage,CB^.Datasize div 2);
    move(CB^.Data^,amessage[1],CB^.Datasize);
    amod.OnError(amod,amessage);
   end;
  replog:
   if Assigned(amod.OnLog) then
   begin
    SetLength(amessage,CB^.Datasize div 2);
    move(CB^.Data^,amessage[1],CB^.Datasize);
    amod.OnLog(amod,amessage);
   end;
  repauth:
   if Assigned(amod.OnAuthorization) then
   begin
    amod.FAuthorized:=true;
    amod.OnAuthorization(amod);
    amod.FAliases.LoadFromStream(data);
   end;
  repexecutereportmeta:
   begin
    amod.Stream.Clear;
    amod.Stream.SetSize(data.Size);
    amod.Stream.Write(data.Memory^,data.Size);
    amod.Stream.Seek(0,soFromBeginning);
    if Assigned(amod.OnExecute) then
    begin
     amod.OnExecute(amod);
    end;
   end;
 end;
end;

procedure TRpClientHandleThread.Execute;
begin
 data:=TMemoryStream.Create;
 try
  while not Terminated do
  begin
   if not assigned(amod) then
   begin
    Terminate;
    break;
   end;
   if not amod.RepClient.Connected then
    Terminate
   else
   begin
    try
     amod.RepClient.ReadStream(data);
     data.Seek(0,soFromBeginning);
     CB:=AllocMem(data.Size);
     try
      data.Read(CB^,data.size);
      data.Clear;
      data.SetSize(CB^.Datasize);
      data.Write((@CB^.Data)^,data.Size);
      data.Seek(0,soFromBeginning);
      if syncexec then
      begin
       if CB.Command in [repexecutereportmeta,repexecutereportpdf] then
       begin
        FEndReport.SetEvent;
        Synchronize(HandleInput);
        syncexec:=false;
       end;
      end
      else
       Synchronize(HandleInput);
      data.Clear;
     finally
      FreeMem(CB);
     end;
    except
    end;
   end;
  end;
 finally
  data.free;
 end;
end;


function Connect(hostname:string;user:string;password:string):TModClient;
var
 amod:TModClient;
 arec:PRpComBlock;
begin
 amod:=TModClient.Create(nil);
 try
  amod.RepClient.Host:=hostname;
  amod.RepClient.Connect;
  // Send user and password message
  amod.FAuthorized:=false;
  amod.ClientHandleThread := TRpClientHandleThread.Create(True);
  amod.ClientHandleThread.amod:=amod;
  amod.CLientHandleThread.FEndreport:=amod.FEndReport;
  amod.ClientHandleThread.FreeOnTerminate:=True;
  amod.ClientHandleThread.Resume;

  arec:=GenerateUserNameData(user,password);
  try
   SendBlock(amod.RepClient,arec);
  finally
   FreeBlock(arec);
  end;
//  amod.RepClient.WriteBuffer(arec,sizeof(arec));
 except
  amod.free;
  raise;
 end;
 Result:=amod
end;

procedure Disconnect(amod:TModClient);
begin
 if amod.RepClient.Connected then
  amod.RepClient.Disconnect;
 amod.free;
end;



procedure Tmodclient.DataModuleCreate(Sender: TObject);
begin
 FEndReport:=TEvent.Create(nil,false,false,'');
 FStream:=TMemoryStream.Create;
 FAliases:=TStringList.Create;
end;

procedure Tmodclient.DataModuleDestroy(Sender: TObject);
begin
 FEndReport.SetEvent;
 FEndReport.Free;
 FStream.Free;
 FAliases.Free;
 if Assigned(ClientHandleThread) then
  CLientHandleThread.amod:=nil;
end;


procedure TModClient.Execute;
var
 arec:PRpComBlock;
 alist:TStringList;
begin
 alist:=TStringList.Create;
 try
  alist.add('areport');
  arec:=GenerateBlock(repexecutereportmeta,alist);
  try
   if asynchronous then
   begin
    SendBlock(RepClient,arec);
   end
   else
   begin
    ClientHandleThread.syncexec:=true;
    FEndReport.ReSetEvent;
    // Sets an event and waits for its signal
    SendBlock(RepClient,arec);
    FEndReport.WaitFor($FFFFFFFF);
   end;
  finally
   FreeBlock(arec);
  end;
 finally
  alist.free;
 end;
end;

procedure Tmodclient.RepClientDisconnected(Sender: TObject);
begin
 if assigned(ClientHandleThread.FEndReport) then
  ClientHandleThread.FEndReport.SetEvent;
end;

end.