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

uses SysUtils,Classes,rpmdconsts;

const
  REPSERBUFSIZE=4096;

type
  TRepCommand=(repauth,repopenalias,repopenreport,repexecutereportmeta,
   repexecutereportpdf,reperror,replog,repgetusers,repgetaliases);

  TRpLogMessageEvent=procedure (Sender:TObject;aMessage:WideString) of object;

  TRpComBlock = record
   PacketNum:integer;
   Command:TRepCommand;
   Datasize:integer;
   Data:array [0..REPSERBUFSIZE-1] of Byte;
  end;

  TDynamicRpComBlock=array of TRpComBlock;
  PDynamicRpComBlock=^TDynamicRpComBlock;
  // The real data of the command is stored at the end
  // of the communications block

  // Report commands

  // repauth
  // Client:Sends string list containing  user and password
  // Server:Return a key code to comunicate with the server (Key)

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


function GenerateUserNameData(user,password:string):TRpComBlock;
function GenerateCBErrorMessage(amessage:WideString):TRpComBLock;
procedure GenerateCBArray(command:TRepCommand;Stream:TMemoryStream;parray:PDynamicRpComBlock);overload;
procedure GenerateCBArray(command:TRepCommand;alist:TStringList;parray:PDynamicRpComBlock);overload;

implementation


function GenerateUserNameData(user,password:string):TRpComBlock;
var
 alist:TStringList;
 arec:TRpComBlock;
 mem2:TMemoryStream;
begin
 arec.PacketNum:=0;
 arec.Command:=repauth;
 mem2:=TMemoryStream.Create;
 try
  alist:=TStringList.Create;
  try
   alist.Add(user+'='+password);
   alist.SaveToStream(mem2);
  finally
   alist.free;
  end;
  mem2.Seek(0,soFromBeginning);
  arec.Datasize:=mem2.Size;
  mem2.Read(arec.Data,mem2.size);
 finally
  mem2.free;
 end;
 Result:=arec;
end;

function GenerateCBErrorMessage(amessage:WideString):TRpComBLock;
var
 arec:TRpComBlock;
begin
 arec.PacketNum:=0;
 arec.Command:=reperror;
 arec.Datasize:=Length(amessage)*2;
 move(amessage[1],arec.Data,arec.Datasize);
 Result:=arec;
end;

procedure GenerateCBArray(command:TRepCommand;alist:TStringList;parray:PDynamicRpComBlock);
var
 mems:TMemoryStream;
begin
 mems:=TMemoryStream.Create;
 try
  alist.SaveToStream(mems);
  mems.Seek(0,soFromBeginning);
  GenerateCBArray(command,mems,parray);
 finally
  mems.free;
 end;
end;

procedure GenerateCBArray(command:TRepCommand;Stream:TMemoryStream;parray:PDynamicRpComBlock);
var
 i:integer;
 bytestowrite:integer;
 position:integer;
 streamsize:integer;
begin
 i:=0;
 Stream.Seek(0,soFromBeginning);
 streamsize:=Stream.size;
 position:=0;
 repeat
  SetLength(parray^,i+1);
  parray^[i].PacketNum:=i+1;
  if (streamsize-position)<=REPSERBUFSIZE then
   bytestowrite:=streamsize-position
  else
   bytestowrite:=REPSERBUFSIZE;
  parray^[i].Datasize:=bytestowrite;
  parray^[i].Command:=command;
  Stream.Read(parray^[i].Data,bytestowrite);
  inc(i);
  position:=position+bytestowrite;
 until position>=streamsize;
 parray^[i-1].PacketNum:=0;
end;

end.
