{*******************************************************}
{                                                       }
{       Report Manager Designer                         }
{                                                       }
{       fdesign                                         }
{       Design frame of the Main form                   }
{       Used by a subreport                             }
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

unit fdesign;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs, QMenus,
  QTypes;

type
  TFDesignFrame = class(TFrame)
    ScrollBox1: TScrollBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.xfm}

end.
