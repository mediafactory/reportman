{*******************************************************}
{                                                       }
{       Rpregv                                          }
{                                                       }
{       Units that registers the reportmanager engine   }
{       (visual controls into the component palette)    }
{                                                       }
{       Copyright (c) 1997-2001 Techni-Web              }
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}

unit rpregv;

interface

uses
  Classes,
  rpexpredlg,
  rpruler;
procedure Register;


implementation

procedure Register;
begin
 RegisterComponents('Reportman', [TRpExpreDialog]);
 RegisterComponents('Reportman', [TRpRuler]);
end;

end.
