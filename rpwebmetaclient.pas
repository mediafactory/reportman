{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpwebmetaclient                                 }
{       Metafile reading and printing                   }
{       From a http address                             }
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

unit rpwebmetaclient;


interface

uses classes,rptypes,rpmetafile,rpreport,
 IdHttp,rpgdidriver;

procedure PrintHttpReport(httpstring:String);


implementation



procedure PrintHttpReport(httpstring:String);
var
 connect:TIdHttp;
 astream:TMemoryStream;
 metafile:TrpMetafileReport;
begin
 connect:=TIdHttp.Create(nil);
 try
  astream:=TMemoryStream.Create;
  try
   connect.Get(TestString,astream);
   metafile:=TrpMetafileReport.Create(nil);
   try
    astream.Seek(0,soFromBeginning);
    metafile.LoadFromStream(astream);
    rpgdidriver.PrintMetafile(metafile,'Printing',true,true,0,1,1,true,false);
   finally
    metafile.free;
   end;
  finally
   astream.free;
  end;
 finally
  connect.free;
 end;
end;

end.
