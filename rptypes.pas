{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rptypes                                         }
{       TRpTypes: Generic type definitions used by      }
{       common components of Report manager             }
{                                                       }
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

unit rptypes;

interface

uses Sysutils,Classes,rpconsts;

const
 MAX_LANGUAGES=3;

type
 TRpTwips=integer;
 TRpOrientation=(rpOrientationDefault,rpOrientationPortrait,rpOrientationLandscape);

 TRpPageSize=(rpPageSizeDefault,rpPageSizeCustom);

 TRpColor=integer;

 // How to show preview
 TRpPreviewStyle = (spWide,spNormal,spEntirePage);


// Compares 2 streams and returns true if they are equal
function StreamCompare(Stream1:TStream;Stream2:TStream):Boolean;
procedure Generatenewname(Component:TComponent);

// Language identifiers
var
 rplangids:array [0..MAX_LANGUAGES-1] of string=('EN','ES','CAT');
 rplangdesc:array [0..MAX_LANGUAGES-1] of string=(SRpEnglish,SRpSpanish,SRpCatalan);


implementation

function StreamCompare(Stream1:TStream;Stream2:TStream):Boolean;
const
 SIZE_BUF=4096;
var
 buf1,buf2:array [0..SIZE_BUF] of char;
 readcount:integer;
begin
 Result:=True;
 if Stream1.Size<>Stream2.Size then
 begin
  Result:=False;
  Exit;
 end;
 // If the same size then compare memory
 Stream1.Seek(soFromBeginning,0);
 Stream2.Seek(soFromBeginning,0);
 readcount:=Stream1.Read(buf1,SIZE_BUF);
 Stream2.Read(buf2,SIZE_BUF);
 while (readcount<>0) do
 begin
  if Not CompareMem(@buf1,@buf2,readcount) then
  begin
   result:=False;
   break;
  end;

  readcount:=Stream1.Read(buf1,SIZE_BUF);
  Stream2.Read(buf2,SIZE_BUF);
 end;
end;

procedure Generatenewname(Component:TComponent);
var i:integer;
    name1:string;
begin
 i:=0;
 name1:=Component.ClassName;
 while (nil<>Component.Owner.FindComponent(name1+IntToStr(i))) do
  inc(i);
 Component.Name:=name1+IntToStr(i);
end;

end.
