{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpmdprotocol                                    }
{                                                       }
{       Report Manager Client protocol                  }
{       for Report Manager Server                       }
{       routines to send and receive data               }
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

unit rpmdprotocol;

interface

uses SysUtils,Classes,rpmdconsts,IdTCPConnection;

const
  REPSERBUFSIZE=4096;

type
  TRepCommand=(repauth,repopenalias,repopenreport,repexecutereportmeta,
   repexecutereportpdf,reperror,replog,repgetusers,repgetaliases,repaddalias,
   repdeletealias,repadduser,repdeleteuser,repgettree,repgetparams,repsetparams);

  TRpLogMessageEvent=procedure (Sender:TObject;aMessage:WideString) of object;

  PRpComBlock = ^TRpComBlock;
  TRpComBlock = record
   Size:integer;
   Command:TRepCommand;
   Datasize:integer;
   Data:PByte;
  end;

  // The real data of the command is stored at the end
  // of the communications block

  // Report commands

  // repauth
  // Client:Sends string list containing  user and password

  // repopenalias
  // Client:Sends a string containing an alias to the connection
  //        if no alias defined must be the path where the reports are
  //        in the server
  // Server:Returns the available reports in a string list

  // repopenreport
  // Client:Sends a string containing the report name to open
  // Server:Returns the available parameters


  // repexecutereportmeta
  // Client:Sends nothing (only the key)
  // Server:Returns the executed report in Metafileformat

  // repexecutereportpdf
  // Client:Sends nothing (only the key)
  // Server:Returns the executed report in pdf format


function GenerateUserNameData(user,password:string):PRpComBlock;
function GenerateCBErrorMessage(amessage:WideString):PRpComBLock;
function GenerateCBLogMessage(amessage:WideString):PRpComBLock;
function GenerateBlock(command:TRepCommand;Stream:TMemoryStream):PRpComBlock;overload;
function GenerateBlock(command:TRepCommand;alist:TStringList):PRpComBlock;overload;
procedure FreeBlock(ablock:PRpComBlock);
procedure SendBlock(AConnection:TIdTCPConnection;CB:PRpComBlock);

implementation


function GenerateUserNameData(user,password:string):PRpComBlock;
var
 alist:TStringList;
 arec:PRpComBlock;
 mem2:TMemoryStream;
 asize:integer;
begin
 mem2:=TMemoryStream.Create;
 try
  alist:=TStringList.Create;
  try
   alist.Add(user+'='+password);
   alist.SaveToStream(mem2);
  finally
   alist.free;
  end;
  asize:=sizeof(TRpComBlock)-sizeof(PByte)+mem2.Size;
  arec:=AllocMem(asize);
  arec^.Size:=asize;
  arec^.Command:=repauth;
  mem2.Seek(0,soFromBeginning);
  arec^.Datasize:=mem2.Size;
  mem2.Read((@(arec^.Data))^,mem2.size);
 finally
  mem2.free;
 end;
 Result:=arec;
end;

function GenerateCBErrorMessage(amessage:WideString):PRpComBLock;
var
 arec:PRpComBlock;
 asize:integer;
begin
 asize:=sizeof(TRpComBlock)-sizeof(PByte)+Length(amessage)*2;
 arec:=AllocMem(asize);
 arec^.size:=asize;
 arec^.Command:=reperror;
 arec^.Datasize:=Length(amessage)*2;
 move(amessage[1],(@(arec^.Data))^,arec^.Datasize);
 Result:=arec;
end;

function GenerateCBLogMessage(amessage:WideString):PRpComBLock;
var
 arec:PRpComBlock;
 asize:integer;
begin
 asize:=sizeof(TRpComBlock)-sizeof(PByte)+Length(amessage)*2;
 arec:=AllocMem(asize);
 arec^.size:=asize;
 arec^.Command:=replog;
 arec^.Datasize:=Length(amessage)*2;
 move(amessage[1],(@(arec^.Data))^,arec^.Datasize);
 Result:=arec;
end;

function GenerateBlock(command:TRepCommand;Stream:TMemoryStream):PRpComBlock;overload;
var
 arec:PRpComBlock;
 asize:integer;
begin
 Stream.Seek(0,soFromBeginning);
 asize:=sizeof(TRpComBlock)-sizeof(PByte)+Stream.Size;
 arec:=AllocMem(asize);
 arec^.Size:=asize;
 arec^.cOmmand:=command;
 arec^.Datasize:=Stream.Size;
 Stream.Read((@(arec^.Data))^,Stream.size);
 Result:=arec;
end;

function GenerateBlock(command:TRepCommand;alist:TStringList):PRpComBlock;overload;
var
 mems:TMemoryStream;
 arec:PRpComBlock;
 asize:integer;
begin
 mems:=TMemoryStream.Create;
 try
  alist.SaveToStream(mems);
  mems.Seek(0,soFromBeginning);
  asize:=sizeof(TRpComBlock)-sizeof(PByte)+mems.Size;
  arec:=AllocMem(asize);
  arec^.Size:=asize;
  arec^.cOmmand:=command;
  arec^.Datasize:=mems.Size;
  mems.Read((@(arec^.Data))^,mems.size);
 finally
  mems.free;
 end;
 Result:=arec;
end;

procedure FreeBlock(ablock:PRpComBlock);
begin
 FreeMem(ablock);
end;

procedure SendBlock(AConnection:TIdTCPConnection;CB:PRpComBlock);
var
 memstream:TMemoryStream;
begin
 memstream:=TMemoryStream.Create;
 try
  memstream.SetSize(CB^.Size);
  memstream.Write(CB^,CB^.Size);
  AConnection.WriteStream(memstream,true,true);
 finally
  memstream.free;
 end;
end;


end.
