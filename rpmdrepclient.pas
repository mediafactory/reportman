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

{$I rpconf.inc}

uses
  SysUtils, Classes, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, rptranslator,rpmdprotocol,rpmdconsts,
{$IFNDEF USEVARIANTS}
  forms,
{$ENDIF}
  SyncObjs,rpparams;

type
  TRpClientHandleThread=class;

  TGetStringList=procedure (alist:TStringList) of object;
  TGetStream=procedure (astream:TMemoryStream) of object;

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
    FOnGetTree:TGetStringList;
    FAliases:TStringList;
    FTree:TStringList;
    FOnGetUsers:TGetStringList;
    FOnGetAliases:TGetStringList;
    FOnAuthorization:TNotifyEvent;
    FOnGetParams:TGetStream;
    FPDF:Boolean;
    ClientHandleThread:TRpClientHandleThread;
  public
    { Public declarations }
    asynchronous:boolean;
    dirseparator:char;
    threadsafeexec:boolean;
    property PDF:Boolean read FPDF write FPDF default false;
    procedure GetUsers;
    procedure GetParams;
    procedure ModifyParams(compo:TRpParamComp);
    procedure GetTree(aliasname:string);
    procedure AddUser(username,password:string);
    procedure AddAlias(aliasname,path:string);
    procedure GetAliases;
    procedure OpenReport(aliasname,reportname:string);
    procedure Execute(aliasname,reportname:string);overload;
    procedure Execute;overload;
    procedure DeleteUser(username:string);
    procedure DeleteAlias(aliasname:string);
    property Aliases:TStringList read FAliases;
    property LastTree:TStringList read FTree;
    property OnError:TRpLogMessageEvent read FOnError write FOnError;
    property OnLog:TRpLogMessageEvent read FOnLog write FOnLog;
    property OnAuthorization:TNotifyEvent read FOnAuthorization write FOnAuthorization;
    property OnExecute:TNotifyEvent read FOnExecute write FOnExecute;
    property OnGetUsers:TGetStringList read FOnGetUsers write FOnGetUsers;
    property OnGetTree:TGetStringList read FOnGetTree write FOnGetTree;
    property OnGetParams:TGetStream read FOnGetparams write FOnGetParams;
    property OnGetAliases:TGetStringList read FOnGetAliases write FOnGetAliases;
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
   errormessage:widestring;
   threadsafeexec:boolean;
   procedure HandleInput;
   procedure DoErrorMessage;
  protected
   procedure Execute; override;
  end;


function Connect(hostname:string;user:string;password:string;port:integer):TModClient;
procedure Disconnect(amod:TModClient);

implementation

{$IFDEF USEVARIANTS}
{$R *.xfm}
{$ENDIF}

{$IFNDEF USEVARIANTS}
{$R *.dfm}
{$ENDIF}


procedure TRpClientHandleThread.DoErrorMessage;
begin
 if Assigned(amod) then
 begin
  if Assigned(amod.OnLog) then
  begin
   amod.OnLog(amod,errormessage);
  end;
 end;
end;

procedure TRpClientHandleThread.HandleInput;
var
 amessage:WideString;
 alist:TStringList;
begin
 // Handles the input this is VCLX thread safe
 case CB.Command of
  reperror:
   if Assigned(amod.OnError) then
   begin
    SetLength(amessage,CB^.Datasize div 2);
    move((@CB^.Data)^,amessage[1],CB^.Datasize);
    amod.OnError(amod,amessage);
   end;
  replog:
   if Assigned(amod.OnLog) then
   begin
    SetLength(amessage,CB^.Datasize div 2);
    move((@CB^.Data)^,amessage[1],CB^.Datasize);
    amod.OnLog(amod,amessage);
   end;
  repauth:
   begin
    amod.FAuthorized:=true;
    alist:=TStringList.Create;
    try
     if Assigned(amod.FOnAuthorization) then
      amod.FOnAuthorization(amod);
     alist.LoadFromStream(data);
     if alist.count>0 then
      if Length(alist.strings[0])>0 then
       amod.dirseparator:=alist.strings[0][1];
     alist.Delete(0);
     amod.FAliases.Assign(alist);
     if assigned(amod.FOnGetAliases) then
      amod.FOnGetAliases(amod.FAliases);
    finally
     alist.free;
    end;
   end;
  repgetaliases:
   begin
    amod.FAliases.LoadFromStream(data);
    if assigned(amod.FOnGetAliases) then
     amod.FOnGetAliases(amod.FAliases);
   end;
  repgetparams:
   begin
    if assigned(amod.FOnGetParams) then
     amod.FOnGetParams(data);
   end;
  repgettree:
   begin
    amod.FTree.LoadFromStream(data);
    if assigned(amod.FOnGetTree) then
     amod.FOnGetTree(amod.FTree);
   end;
  repgetusers:
   if Assigned(amod.FOnGetUsers) then
   begin
    alist:=TStringList.Create;
    try
     alist.LoadFromStream(data);
     amod.FOnGetUsers(alist);
    finally
     alist.free;
    end;
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
       if CB.Command in [repexecutereportmeta,repexecutereportpdf,repopenreport,reperror] then
       begin
        if threadsafeexec then
        begin
         HandleInput;
         FEndReport.SetEvent;
        end
        else
        begin
         FEndReport.SetEvent;
         Synchronize(HandleInput);
        end;
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
     on E:Exception do
     begin
      errormessage:=E.Message;
      if threadsafeexec then
       DoErrorMessage
      else
       Synchronize(DoErrorMessage);
     end;
    end;
   end;
  end;
 finally
  data.free;
 end;
end;


function Connect(hostname:string;user:string;password:string;port:integer):TModClient;
var
 amod:TModClient;
 arec:PRpComBlock;
begin
 amod:=TModClient.Create(nil);
 try
  amod.RepClient.Host:=hostname;
  amod.RepClient.Port:=port;
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
 Result:=amod;
end;

procedure Disconnect(amod:TModClient);
begin
 if amod.RepClient.Connected then
  amod.RepClient.Disconnect;
 amod.free;
end;



procedure Tmodclient.DataModuleCreate(Sender: TObject);
begin
 dirseparator:=C_DIRSEPARATOR;
 FPDF:=False;
 FEndReport:=TEvent.Create(nil,false,false,'');
 FStream:=TMemoryStream.Create;
 FAliases:=TStringList.Create;
 FTree:=TStringList.Create;
end;

procedure Tmodclient.DataModuleDestroy(Sender: TObject);
begin
 FEndReport.SetEvent;
 FEndReport.Free;
 FStream.Free;
 FAliases.Free;
 FTree.Free;
 if Assigned(ClientHandleThread) then
  CLientHandleThread.amod:=nil;
end;


procedure TModClient.Execute(aliasname,reportname:string);
var
 arec:PRpComBlock;
 alist:TStringList;
begin
 alist:=TStringList.Create;
 try
  alist.add(aliasname+'='+reportname);
  arec:=GenerateBlock(repexecutereportmeta,alist);
  try
   ClientHandleThread.threadsafeexec:=threadsafeexec;
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

procedure TModClient.OpenReport(aliasname,reportname:string);
var
 arec:PRpComBlock;
 alist:TStringList;
begin
 alist:=TStringList.Create;
 try
  alist.add(aliasname+'='+reportname);
  arec:=GenerateBlock(repopenreport,alist);
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

procedure TModClient.Execute;
var
 arec:PRpComBlock;
 alist:TStringList;
begin
 alist:=TStringList.Create;
 try
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

procedure Tmodclient.GetUsers;
var
 arec:PRpComBlock;
 alist:TStringList;
begin
 // Get the users
 alist:=TStringList.Create;
 try
  arec:=GenerateBlock(repgetusers,alist);
  try
   SendBlock(RepClient,arec);
  finally
   FreeBlock(arec);
  end;
 finally
  alist.free;
 end;
end;

procedure Tmodclient.GetParams;
var
 arec:PRpComBlock;
 alist:TStringList;
begin
 // Get the users
 alist:=TStringList.Create;
 try
  arec:=GenerateBlock(repgetparams,alist);
  try
   SendBlock(RepClient,arec);
  finally
   FreeBlock(arec);
  end;
 finally
  alist.free;
 end;
end;


procedure Tmodclient.GetTree(aliasname:string);
var
 arec:PRpComBlock;
 alist:TStringList;
begin
 // Get the users
 alist:=TStringList.Create;
 try
  alist.Add(aliasname+'=');
  arec:=GenerateBlock(repgettree,alist);
  try
   SendBlock(RepClient,arec);
  finally
   FreeBlock(arec);
  end;
 finally
  alist.free;
 end;
end;

procedure Tmodclient.GetAliases;
var
 arec:PRpComBlock;
 alist:TStringList;
begin
 // Get the users
 alist:=TStringList.Create;
 try
  arec:=GenerateBlock(repgetaliases,alist);
  try
   SendBlock(RepClient,arec);
  finally
   FreeBlock(arec);
  end;
 finally
  alist.free;
 end;
end;

procedure Tmodclient.AddAlias(aliasname,path:string);
var
 arec:PRpComBlock;
 alist:TStringList;
begin
 // Get the users
 alist:=TStringList.Create;
 try
  alist.Add(aliasname+'='+path);
  arec:=GenerateBlock(repaddalias,alist);
  try
   SendBlock(RepClient,arec);
  finally
   FreeBlock(arec);
  end;
 finally
  alist.free;
 end;
end;

procedure Tmodclient.AddUser(username,password:string);
var
 arec:PRpComBlock;
 alist:TStringList;
begin
 alist:=TStringList.Create;
 try
  alist.Add(username+'='+password);
  arec:=GenerateBlock(repadduser,alist);
  try
   SendBlock(RepClient,arec);
  finally
   FreeBlock(arec);
  end;
 finally
  alist.free;
 end;
end;

procedure Tmodclient.DeleteAlias(aliasname:string);
var
 arec:PRpComBlock;
 alist:TStringList;
begin
 alist:=TStringList.Create;
 try
  alist.Add(aliasname+'=');
  arec:=GenerateBlock(repdeletealias,alist);
  try
   SendBlock(RepClient,arec);
  finally
   FreeBlock(arec);
  end;
 finally
  alist.free;
 end;
end;

procedure Tmodclient.DeleteUser(username:string);
var
 arec:PRpComBlock;
 alist:TStringList;
begin
 // Get the users
 alist:=TStringList.Create;
 try
  alist.Add(username+'=');
  arec:=GenerateBlock(repdeleteuser,alist);
  try
   SendBlock(RepClient,arec);
  finally
   FreeBlock(arec);
  end;
 finally
  alist.free;
 end;
end;

procedure Tmodclient.ModifyParams(compo:TRpParamComp);
var
 writer:TWriter;
 astream:TMemoryStream;
 arec:PRpComBlock;
begin
 astream:=TMemoryStream.Create;
 try
  writer:=TWriter.Create(astream,4096);
  try
   writer.WriteRootComponent(compo);
  finally
   writer.free;
  end;
  astream.Seek(0,soFromBeginning);
  arec:=GenerateBlock(repsetparams,astream);
  try
   SendBlock(RepClient,arec);
  finally
   FreeBlock(arec);
  end;
 finally
  astream.Free;
 end;
end;

end.
