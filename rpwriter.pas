{*******************************************************}
{                                                       }
{       Rpwrite                                         }
{       Utilities to write, read convert reports        }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}

unit rpwriter;

interface

uses Classes,SysUtils;


procedure FileReportToPlainText(reportfile,plainfile:string);
procedure PlainTextToFileReport(plainfile,reportfile:string);

implementation

procedure FileReportToPlainText(reportfile,plainfile:string);
var
 sourcestream,deststream:TFileStream;
begin
 sourcestream:=TFileStream.Create(reportfile,fmOpenRead or fmShareDenyWrite);
 try
  deststream:=TFileStream.Create(plainfile,fmCreate);
  try
   ObjectBinaryToText(sourcestream,deststream);
  finally
   deststream.Free;
  end;
 finally
  sourcestream.free;
 end;
end;

procedure PlainTextToFileReport(plainfile,reportfile:string);
var
 sourcestream,deststream:TFileStream;
begin
 sourcestream:=TFileStream.Create(plainfile,fmOpenRead or fmShareDenyWrite);
 try
  deststream:=TFileStream.Create(reportfile,fmCreate);
  try
   ObjectTextToBinary(sourcestream,deststream);
  finally
   deststream.Free;
  end;
 finally
  sourcestream.free;
 end;
end;

end.
