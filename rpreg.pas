{*******************************************************}
{                                                       }
{       Rpreg                                         }
{                                                       }
{       Units that registers the reportmanager engine   }
{       into the Delphi component palette               }
{                                                       }
{       Copyright (c) 1997-2001 Techni-Web              }
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}

unit rpreg;

interface

uses
  Classes,
  rpparser,rpeval,
  rpevalfunc,rpalias,rptypeval,rplastsav;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Reportman', [TRpEvaluator]);
  RegisterComponents('Reportman', [TRpAlias]);
  RegisterComponents('Reportman', [TRpLastUsedStrings]);
end;

end.
