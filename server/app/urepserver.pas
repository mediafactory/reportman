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
  rpreport,rppdfdriver, IdThreadMgrPool,rptypes,rpparams;

const
 DEFAULT_MILIS_PROGRESS=10000;

type
  TRpClient=class(TObject)  // Object holding data of client (see events)
    DNS: String;
    Connected,                           { Time of connect }
    LastAction  : TDateTime;             { Time of last transaction }
    Thread      : TIdPeerThread;               { Pointer to thread }
    Auth:boolean;
    IsAdmin:boolean;
    Username:string;
    Password:string;
    CurrentAlias:String;
    CurrentPath:String;
    CurrentReport:TRpReport;
    FromPage,ToPage:integer;
    Copies:integer;
    CompressedPDF:boolean;
    cancelled:boolean;
{$IFDEF USEBDE}
    ASession:TSession;
{$ENDIF}
    procedure OnProgress(Sender:TRpReport;var docancel:boolean);
    procedure CreateReport;
  public
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
    fport:integer;
    procedure InitConfig;
    procedure WriteConfig;
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

procedure TRpClient.CreateReport;
begin
{$IFDEF USEBDE}
 if Not Assigned(ASession) then
 begin
  // If can not create session omit it
  try
   ASession:=TSession.Create(CurrentReport);
   ASession.AutoSessionName:=True;
  except
   ASession.free;
   ASession:=nil;
  end;
 end;
{$ENDIF}
 if Assigned(CurrentReport) then
 begin
  CurrentReport.free;
  CurrentReport:=nil;
 end;
 CurrentReport:=TRpReport.Create(nil);
 CurrentReport.OnProgress:=OnProgress;
 CurrentReport.MilisProgres:=DEFAULT_MILIS_PROGRESS;
{$IFDEF USEBDE}
 if Assigned(ASession) then
  CurrentReport.DatabaseInfo.BDESession:=ASession;
{$ENDIF}
{$IFDEF LINUX}
// CurrentReport.LoadFromFile('/home/toni/cvsroot/reportman/repman/repsamples/sample6.rep');
{$ENDIF}
end;


destructor TRpClient.Destroy;
begin
 if Assigned(CurrentReport) then
 begin
  CurrentReport.Free;
  CurrentReport:=nil;
 end;

 inherited Destroy;
end;

procedure TRpClient.OnProgress(Sender:TRpReport;var docancel:boolean);
var
 astring:WideString;
 CB:PRpComBLock;
begin
 // Sends the message progress to the thread
 astring:=IntToStr(Sender.CurrentSubReportIndex)+' '+SRpPage+':'+
 FormatFloat('####,####',Currentreport.PageNum)+':'
 +FormatFloat('####,####',Currentreport.RecordCount);
 CB:=GenerateCBLogMessage(astring);
 try
  SendBlock(Thread.Connection,CB);
 finally
  FreeBlock(CB);
 end;
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
  amod.RepServer.DefaultPort:=amod.fport;
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
 fport:=3060;
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
 i:integer;
begin
 Ffilenameconfig:=Obtainininamecommonconfig('','','reportmanserver');
 if Not FileExists(FFilenameConfig) then
  Ffilenameconfig:=Obtainininamelocalconfig('','','reportmanserver');
 ForceDirectories(ExtractFilePath(ffilenameconfig));
 inif:=TMemInifile.Create(filenameconfig);
 try
  laliases.clear;
  lusers.clear;
  inif.CaseSensitive:=false;
  fport:=inif.ReadInteger('CONFIG','TCPPORT',3060);
  inif.ReadSectionValues('USERS',lusers);
  inif.ReadSectionValues('ALIASES',laliases);
  i:=0;
  while i<lusers.count do
  begin
   if Length(Trim(lusers.strings[i]))<1 then
    LUsers.delete(i)
   else
    inc(i);
  end;
  i:=0;
  while i<laliases.count do
  begin
   if Length(Trim(laliases.strings[i]))<1 then
    laliases.delete(i)
   else
    inc(i);
  end;
  for i:=0 to lusers.count-1 do
  begin
   if Length(lusers.Names[i])<1 then
    lusers.Strings[i]:=lusers.Strings[i]+'=';
  end;
  for i:=0 to laliases.count-1 do
  begin
   if Length(laliases.Names[i])<1 then
    laliases.Strings[i]:=laliases.Strings[i]+'=';
  end;
  if lusers.IndexOfName('ADMIN')<0 then
   lusers.Add('ADMIN=');
 finally
  inif.free;
 end;
end;

// Write users and passwords and aliases
procedure TmodServer.WriteConfig;
var
 inif:TMemInifile;
 i:integer;
 adups:TStringList;
begin
 Ffilenameconfig:=Obtainininamecommonconfig('','','reportmanserver');
 if Not FileExists(FFilenameConfig) then
  Ffilenameconfig:=Obtainininamelocalconfig('','','reportmanserver');
 ForceDirectories(ExtractFilePath(ffilenameconfig));
 inif:=TMemInifile.Create(filenameconfig);
 try
  inif.WriteInteger('CONFIG','TCPPORT',fport);
  adups:=TStringList.Create;
  try
   if lusers.IndexOfName('ADMIN')<0 then
    lusers.Add('ADMIN=');
   inif.CaseSensitive:=false;
   inif.EraseSection('USERS');
   adups.clear;
   for i:=0 to lusers.Count-1 do
   begin
    if Length(lusers.Names[i])>0 then
    begin
     if adups.Indexof(lusers.Names[i])<0 then
     begin
      adups.Add(lusers.Names[i]);
      inif.WriteString('USERS',lusers.Names[i],lusers.Values[lusers.Names[i]]);
     end;
    end;
   end;
   inif.EraseSection('ALIASES');
   adups.clear;
   for i:=0 to laliases.Count-1 do
   begin
    if Length(laliases.Names[i])>0 then
    begin
     if adups.Indexof(laliases.Names[i])<0 then
     begin
      adups.Add(laliases.Names[i]);
      inif.WriteString('ALIASES',laliases.Names[i],laliases.Values[laliases.Names[i]]);
     end;
    end;
   end;
   inif.UpdateFile;
  finally
   adups.free;
  end;
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
 CB,ACB:PRpComBlock;
 astream:TMemoryStream;
 alist:TStringList;
 username,password:string;
 aliasname,apath:string;
 correct:boolean;
 index:integer;
 ActClient:TRpClient;
 i:integer;
 APDFDriver:TRpPDFDriver;
 acompo:TRpParamComp;
 writer:TWriter;
 reader:TReader;
begin
 // Execution of commands
 if (AThread.Terminated) or (Not AThread.Connection.Connected) then
  exit;
 try
  astream:=TMemoryStream.Create;
  try
   AThread.Connection.ReadStream(astream);
   ACB:=AllocMem(astream.Size);
   try
    astream.Seek(0,soFromBeginning);
    astream.Read(ACB^,astream.size);
    astream.Clear;
    astream.SetSize(ACB^.Datasize);
    astream.Write((@(ACB^.Data))^,astream.size);
    astream.Seek(0,soFromBeginning);
    ActClient := TRpClient(AThread.Data);
    ActClient.LastAction := Now;  // update the time of last action
    // if is a auth message return the key
    case ACB^.Command of
     repauth:
      begin
       username:='';
       password:='';
       alist:=TStringList.Create;
       try
        alist.LoadFromStream(astream);
        if alist.count>0 then
        begin
         username:=Uppercase(Alist.Names[0]);
         password:=Alist.Values[Alist.Names[0]];
        end;
       finally
        alist.free;
       end;
       // Looks if the user exists
       correct:=false;
       if length(username)>0 then
       begin
        index:=LUsers.IndexOfName(username);
        if index>=0 then
        begin
         If LUsers.ValueFromIndex[index]=password then
         begin
          correct:=true;
         end;
        end;
       end;
       if correct then
       begin
        ActClient.UserName:=username;
        ActClient.Password:=password;
        ActClient.Auth:=True;
        // Admin can administer aliases-users
        ActClient.IsAdmin:=UpperCase(username)='ADMIN';
        // Sends the authorization message with a list of the aliases
        CB:=GenerateBlock(repauth,LAliases);
        try
         SendBlock(AThread.Connection,CB);
        finally
         FreeBlock(CB);
        end;
       end
       else
       begin
        // Sends a error message
        ActClient.Auth:=False;
        CB:=GenerateCBErrorMessage(SRpAuthFailed);
        try
         SendBlock(AThread.COnnection,CB);
        finally
         FreeBlock(CB);
        end;
       end;
      end;
     repgetusers:
      begin
       if ActClient.IsAdmin then
       begin
        alist:=TStringList.Create;
        try
         for i:=0 to LUsers.Count-1 do
          alist.Add(LUsers.Names[i]);
         CB:=GenerateBlock(repgetusers,alist);
         try
          SendBlock(AThread.COnnection,CB);
         finally
          FreeBlock(CB);
         end;
        finally
         alist.free;
        end;
       end;
      end;
     repgetaliases:
      begin
       CB:=GenerateBlock(repgetaliases,LAliases);
       try
        SendBlock(AThread.COnnection,CB);
       finally
        FreeBlock(CB);
       end;
      end;
     repaddalias:
      begin
       // Add a alias (only admin)
       if ActClient.IsAdmin then
       begin
        alist:=TStringList.Create;
        try
         alist.LoadFromStream(astream);
         if alist.count>0 then
         begin
          aliasname:=UpperCase(AList.Names[0]);
          apath:=Alist.Values[aliasname];
          if LAliases.IndexOfName(aliasname)<0 then
          begin
           if length(apath)>0 then
           begin
            LAliases.Add(aliasname+'='+apath);
            WriteConfig;
            InitConfig;
           end;
          end;
         end;
        finally
         alist.free;
        end;
       end;
      end;
     repdeletealias:
      begin
       // Add a alias (only admin)
       if ActClient.IsAdmin then
       begin
        alist:=TStringList.Create;
        try
         alist.LoadFromStream(astream);
         if alist.count>0 then
         begin
          aliasname:=UpperCase(AList.Names[0]);
          index:=LAliases.IndexOfName(aliasname);
          if index>=0 then
          begin
           LAliases.Delete(index);
           WriteConfig;
           InitConfig;
          end;
         end;
        finally
         alist.free;
        end;
       end;
      end;
     repadduser:
      begin
       // Add a alias (only admin)
       if ActClient.IsAdmin then
       begin
        alist:=TStringList.Create;
        try
         alist.LoadFromStream(astream);
         if alist.count>0 then
         begin
          username:=Trim(UpperCase(AList.Names[0]));
          password:=Alist.Values[username];
          index:=LUsers.IndexOfName(username);
          if index>=0 then
           LUsers.Delete(index);
          LUsers.Add(username+'='+password);
          WriteConfig;
          InitConfig;
         end;
        finally
         alist.free;
        end;
       end;
      end;
     repdeleteuser:
      begin
       // Add a alias (only admin)
       if ActClient.IsAdmin then
       begin
        alist:=TStringList.Create;
        try
         alist.LoadFromStream(astream);
         if alist.count>0 then
         begin
          username:=Trim(Uppercase(AList.Names[0]));
          if username<>'ADMIN' then
          begin
           index:=LUsers.IndexOfName(username);
           if index>=0 then
           begin
            LUsers.Delete(index);
            WriteConfig;
            InitConfig;
            // Break user connections?
            //Clients.LockList;
           end;
          end;
         end;
        finally
         alist.free;
        end;
       end;
      end;
     repopenreport:
      begin
       alist:=TStringList.Create;
       try
        alist.LoadFromStream(astream);
        if alist.count>0 then
        begin
         aliasname:=alist.Names[0];
         index:=LAliases.IndexOfName(aliasname);
         if index>=0 then
         begin
          apath:=LAliases.Values[LAliases.Names[index]];
          ActClient.CreateReport;
          ActClient.CurrentReport.LoadFromFile(apath+C_DIRSEPARATOR+alist.Values[alist.Names[0]]);
         end;
        end;
        CB:=GenerateBlock(repopenreport,alist);
        try
         SendBlock(AThread.COnnection,CB);
        finally
         FreeBlock(CB);
        end;
       finally
        alist.Free;
       end;
      end;
     repgetparams:
      begin
       if Assigned(ActClient.CurrentReport) then
       begin
        astream.clear;
        acompo:=TRpParamcomp.Create(nil);
        try
         acompo.Params.Assign(ActClient.CurrentReport.Params);
         writer:=TWriter.Create(astream,4096);
         try
          writer.WriteRootComponent(acompo);
         finally
          writer.free;
         end;
        finally
         acompo.free;
        end;
        astream.Seek(0,soFromBeginning);
        CB:=GenerateBlock(repgetparams,astream);
        try
         SendBlock(AThread.COnnection,CB);
        finally
         FreeBlock(CB);
        end;
       end;
      end;
     repsetparams:
      begin
       if Assigned(ActClient.CurrentReport) then
       begin
        acompo:=TRpParamcomp.Create(nil);
        try
         acompo.Params.Assign(ActClient.CurrentReport.Params);
         reader:=TReader.Create(astream,4096);
         try
          Reader.ReadRootComponent(acompo);
         finally
          Reader.free;
         end;
         ActClient.CurrentReport.Params.Assign(acompo.Params);
        finally
         acompo.free;
        end;
        alist:=TStringList.Create;
        try
         CB:=GenerateBlock(repsetparams,alist);
         try
          SendBlock(AThread.COnnection,CB);
         finally
          FreeBlock(CB);
         end;
        finally
         alist.free;
        end;
       end;
      end;
     repexecutereportmeta:
      begin
       alist:=TStringList.Create;
       try
        alist.LoadFromStream(astream);
        if alist.count>0 then
        begin
         aliasname:=alist.Names[0];
         index:=LAliases.IndexOfName(aliasname);
         if index>=0 then
         begin
          apath:=LAliases.Values[LAliases.Names[index]];
          ActClient.CreateReport;
          ActClient.CurrentReport.LoadFromFile(apath+C_DIRSEPARATOR+alist.Values[alist.Names[0]]);
         end;
        end;
        if Assigned(ActClient.CurrentReport) then
        begin
         ActClient.cancelled:=false;
         APDFDriver:=TRpPdfDriver.Create;
         ActClient.CurrentReport.PrintAll(APDFDriver);
         astream.Clear;
         ActClient.CurrentReport.Metafile.SaveToStream(astream);
         astream.Seek(0,soFromBeginning);
         CB:=GenerateBlock(repexecutereportmeta,astream);
         try
          SendBlock(AThread.COnnection,CB);
         finally
          FreeBlock(CB);
         end;
        end;
       finally
        alist.Free;
       end;
      end;
     repexecutereportpdf:
      begin
       alist:=TStringList.Create;
       try
        alist.LoadFromStream(astream);
        if alist.count>0 then
        begin
         aliasname:=alist.Names[0];
         index:=LAliases.IndexOfName(aliasname);
         if index>=0 then
         begin
          apath:=LAliases.Values[LAliases.Names[index]];
          ActClient.CreateReport;
          ActClient.CurrentReport.LoadFromFile(apath+C_DIRSEPARATOR+alist.Values[alist.Names[0]]);
         end;
        end;
        if assigned(ActClient.CurrentReport) then
        begin
         ActClient.cancelled:=false;
         ActClient.CurrentReport.Metafile.SaveToStream(astream);
         ActClient.cancelled:=false;
         APDFDriver:=TRpPdfDriver.Create;
         ActClient.CurrentReport.PrintRange(apdfdriver,false,ActClient.FromPage,ActClient.ToPage,ActClient.Copies);
         CB:=GenerateBlock(repexecutereportpdf,APDFDriver.PDFFile.MainPDF);
         try
          SendBlock(AThread.COnnection,CB);
         finally
          FreeBlock(CB);
         end;
        end;
       finally
        alist.Free;
       end;
      end;
     repgettree:
      begin
       alist:=TStringList.Create;
       try
        alist.LoadFromStream(astream);
        if alist.count>0 then
        begin
         index:=LAliases.IndexOfName(alist.Names[0]);
         if index>=0 then
         begin
          apath:=LAliases.Values[alist.Names[0]];
          alist.clear;
          rptypes.FillTreeDir(apath,alist);
          CB:=GenerateBlock(repgettree,alist);
          try
           SendBlock(AThread.COnnection,CB);
          finally
           FreeBlock(CB);
          end;
         end;
        end;
       finally
        alist.free;
       end;
      end;
    end;
   finally
    FreeMem(ACB);
   end;
  finally
   astream.free;
  end;
 except
  // Sends the exception via socket as a error
  on E:Exception do
  begin
   CB:=GenerateCBErrorMessage(SRpError+' - '+E.Message);
   try
    SendBlock(AThread.COnnection,CB);
   finally
    FreeBlock(CB);
   end;
  end;
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
