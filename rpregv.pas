{*******************************************************}
{                                                       }
{       Rpregv                                          }
{                                                       }
{       Units that registers the reportmanager engine   }
{       (visual controls into the component palette)    }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir              }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rpregv;

interface

uses
  Classes,
  rpexpredlg,
  rpruler,
  rpclxreport;
procedure Register;


implementation

procedure Register;
begin
 RegisterComponents('Reportman', [TRpExpreDialog]);
 RegisterComponents('Reportman', [TRpRuler]);
 RegisterComponents('Reportman', [TCLXReport]);
end;

end.
