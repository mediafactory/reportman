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

uses rpreport,rpmetafile,rpsubreport,rpsection,rpmunits,rptypes,
 rpeval;

procedure BeginPrint(report:TRpReport);
procedure EndPrint(report:TRpReport);
procedure PrintNextPage(report:TRpReport;metafile:TRpMetafileReport);

implementation

procedure EndPrint(report:TRpReport);
begin
 report.DeActivateDatasets;
 report.Evaluator.Free;
 report.Evaluator:=nil;
 report.printing:=false;
end;


procedure BeginPrint(report:TRpReport);
var
 i:integer;
begin
 EndPrint(report);
 report.PageNum:=-1;
 report.CurrentSubReportIndex:=0;
 report.CurrentSectionIndex:=0;
 report.ActivateDatasets;
 // Evaluator
 report.Evaluator:=TRpEvaluator.Create(report);
 // Insert params into rpEvaluator
 for i:=0 to report.Params.Count-1 do
 begin
  report.Evaluator.NewVariable(report.params.items[i].Name,report.params.items[i].Value);
 end;
 report.printing:=True;
end;


procedure PrintNextPage(report:TRpReport;metafile:TRpMetafileReport);
var
 printedsomething:boolean;
 pageposy:integer;
begin
 pageposy:=0;
 printedsomething:=false;
 inc(report.Pagenum);
 if metafile.PageCount<=report.PageNum then
 begin
  metafile.NewPage;
 end;
 metafile.CurrentPage:=report.PageNum;
 // Tries to print at least a report header

end;




end.
