{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rppdfreport                                     }
{       Report component for pdf export only            }
{       It does not need VCL or VisualCLX so            }
{       you do not need a X Server                      }
{       and you can use it in console apps              }
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

unit rppdfreport;

interface

uses Classes,Sysutils,rpreport,rpconsts,rpcompobase,
 rppdfdriver,rpalias;

type
 TPDFReport=class(TCBaseReport)
  private
   fpdffilename:string;
   FCompressed:Boolean;
   FFromPage,FToPAge:integer;
   FCopies:integer;
  protected
  public
   function Execute:boolean;override;
   procedure PrinterSetup;override;
   function ShowParams:boolean;override;
   function PrintRange(frompage:integer;topage:integer;
     copies:integer;collate:boolean):boolean;override;
   constructor Create(AOwner:TComponent);override;
  published
   property Filename;
//   property Preview;
//   property ShowProgress;
   property Title;
   property ShowPrintDialog;
   property AliasList;
   property Language;
   property PDFFilename:string read FPDFFilename write FPDFFilename;
   property Compressed:Boolean read FCompressed write FCompressed default True;
   property FromPage:integer read FFromPage write FFromPage default 1;
   property ToPage:integer read FFromPage write FToPage default 9999999;
   property Copies:integer read FCopies write FCopies default 9999999;
  end;

implementation

constructor TPDFReport.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 Preview:=false;
 ShowPrintDialog:=false;
 FCompressed:=true;
 FFromPage:=1;
 FToPage:=9999999;
 FCopies:=1;
end;

procedure TPDFReport.PrinterSetup;
begin
 // No implemented may be implement asking the printer
 // first page and last page
 Raise Exception.Create(SRpSNotYetImplemented+':TPDFReport.PrinterSetup');
end;

function TPDFReport.ShowParams:boolean;
begin
 CheckLoaded;
 // No implemented may be implement asking the parameters
 // in consolemode with Readln
 Raise Exception.Create(SRpSNotYetImplemented+':TPDFReport.ShowParams');
end;

function TPDFReport.Execute:boolean;
begin
 inherited Execute;
 if FFromPage<1 then
  FFromPage:=1;
 if FToPage<FFromPage then
  FToPage:=FFromPage;
 if FCopies<1 then
  FCopies:=1;
 if Preview then
 begin
  // No implemented may be implement asking the parameters
  // in consolemode with Readln ignore
  Raise Exception.Create(SRpSNotYetImplemented+':TPDFReport.Preview');
 end;
 begin
  if ShowPrintDialog then
  begin
   // No implemented may be implement asking the parameters
   // in consolemode with Readln
   Raise Exception.Create(SRpSNotYetImplemented+':TPDFReport.ShowPrintDialog');
//   if DoShowPrintDialog(allpages,frompage,topage,copies,collate) then
//   begin
//    Result:=PrintReport(report,Title,Showprogress,allpages,frompage,
//     topage,copies,collate);
//   end
//   else
//    Result:=false;
  end
  else
  begin
    Result:=PrintReportPDF(report,Title,Showprogress,false,ffrompage,
     ftopage,fcopies,FPDFFilename,FCompressed);
  end;
 end;
end;


function TPDFReport.PrintRange(frompage:integer;topage:integer;
    copies:integer;collate:boolean):boolean;
begin
 Result:=rppdfdriver.PrintReportPDF(Report,Title,ShowProgress,false,
  frompage,topage,copies,fpdffilename,compressed);
end;

//procedure TVCLReport.SaveToPDF(filename:string);
//begin
// CheckLoaded;
// rpgdidriver.ExportReportToPDF(report,filename,true,true,1,999999,
//  false,filename,true)
//end;

end.
