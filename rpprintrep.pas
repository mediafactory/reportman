{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpprintrep                                      }
{                                                       }
{       This units print the report that is converts    }
{       it to a rpmetafile                              }
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

unit rpprintrep;

interface

uses rpreport,rpmetafile;

procedure PrintReport(report:TRpReport;metafile:TRpMetafileReport);

implementation


procedure PrintReport(report:TRpReport;metafile:TRpMetafileReport);
begin
 if Not Assigned(report) then
  exit;
 if Not Assigned(metafile) then
  exit;
 // DeActivates datasets
 report.ActivateDatasets;
 try
  metafile.NewPage;
  metafile.Pages[metafile.CurrentPage].NewTextObject(1440,1440,1440,1440,'Hola','Arial',32,0,0,$FFFF,False);
 finally
  report.DeActivateDatasets;
 end;
end;


end.
