{*******************************************************}
{                                                       }
{       Rpwriter                                         }
{       Utilities to write, read convert reports        }
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

unit rpwriter;

interface

uses Classes,SysUtils,rpmzlib;


procedure FileReportToPlainText (reportfile,plainfile:string);
procedure PlainTextToFileReport (plainfile,reportfile:string);

implementation

procedure FileReportToPlainText(reportfile,plainfile:string);
var
 deststream:TFileStream;
 stream:TMemoryStream;
 memstream:TMemoryStream;
 zlibs:TDeCompressionStream;
 buf:pointer;
 readed:LongInt;
begin
 stream:=TMemoryStream.Create;
 try
  stream.LoadFromFile(reportfile);
  stream.Seek(0,soFromBeginning);
  memstream:=TMemoryStream.Create;
  try
   zlibs:=TDeCompressionStream.Create(Stream);
   try
    buf:=AllocMem(120000);
    try
     repeat
      readed:=zlibs.Read(buf^,120000);
      memstream.Write(buf^,readed);
     until readed<120000;
    finally
     freemem(buf);
    end;
    memstream.Seek(0,soFrombeginning);
    deststream:=TFileStream.Create(plainfile,fmCreate);
    try
     ObjectBinaryToText(memstream,deststream);
    finally
     deststream.Free;
    end;
   finally
    zlibs.free;
   end;
  finally
   memstream.free;
  end;
 finally
  stream.free;
 end;
end;

procedure PlainTextToFileReport(plainfile,reportfile:string);
var
 sourcestream,deststream:TFileStream;
 zstream:TCompressionStream;
begin
 sourcestream:=TFileStream.Create(plainfile,fmOpenRead or fmShareDenyWrite);
 try
  deststream:=TFileStream.Create(reportfile,fmCreate);
  try
   zstream:=TCompressionStream.Create(clDefault,deststream);
   try
    ObjectTextToBinary(sourcestream,zstream);
   finally
    zstream.free;
   end;
  finally
   deststream.Free;
  end;
 finally
  sourcestream.free;
 end;
end;

end.
