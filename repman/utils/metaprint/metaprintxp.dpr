{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       MetaprintXP                                     }
{                                                       }
{       Prints a metafile report                        }
{       you can select the pages to print               }
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

program metaprintxp;

{$APPTYPE CONSOLE}
uses
  Classes,SysUtils,
  rpgdidriver in '..\..\..\rpgdidriver.pas',
  rpmetafile in '..\..\..\rpmetafile.pas',
  rpreport in '..\..\..\rpreport.pas',
  rptypes in '..\..\..\rptypes.pas',
  rpmdconsts in '..\..\..\rpmdconsts.pas';

var
 metafile:TRpMetafileReport;

procedure PrintHelp;
begin
 Writeln(SRpMetaPrint1);
 Writeln(SRpMetaPrint2);
 Writeln(SRpMetaPrint3);
 Writeln(SRpMetaPrint4);
 Writeln(SRpMetaPrint5);
 Writeln(SRpMetaPrint6);
 Writeln(SRpMetaPrint7);
 Writeln(SRpMetaPrint8);
 Writeln(SRpMetaPrint9);
end;


var
 indexparam:integer;
 showprogress:boolean;
 dodeletefile:boolean;
 filename:string;
 allpages:boolean;
 frompage:integer;
 topage:integer;
 copies:integer;
 collate:boolean;
 printerindex:TRpPrinterSelect;
begin
 try
  { TODO -oUser -cConsole Main : Insert code here }
  printerindex:=pRpDefaultPrinter;
  if ParamCount<1 then
   PrintHelp
  else
  begin
   showprogress:=true;
   collate:=false;
   allpages:=true;
   frompage:=1;
   topage:=999999999;
   copies:=1;
   dodeletefile:=false;
   metafile:=TRpMetafileReport.Create(nil);
   try
    indexparam:=1;
    filename:='';
    // Get the options
    while indexparam<ParamCount+1 do
    begin
     if ParamStr(indexparam)='-q' then
      showprogress:=false
     else
      if ParamStr(indexparam)='-d' then
       dodeletefile:=true
      else
      if ParamStr(indexparam)='-from' then
      begin
       inc(indexparam);
       if indexparam>=Paramcount+1 then
        Raise Exception.Create(SRpNumberexpected);
       frompage:=StrToInt(ParamStr(indexparam));
       allpages:=false;
      end
      else
      if ParamStr(indexparam)='-to' then
      begin
       inc(indexparam);
       if indexparam>=Paramcount+1 then
        Raise Exception.Create(SRpNumberexpected);
       topage:=StrToInt(ParamStr(indexparam));
       allpages:=false;
      end
      else
      if ParamStr(indexparam)='-copies' then
      begin
       inc(indexparam);
       if indexparam>=Paramcount+1 then
        Raise Exception.Create(SRpNumberexpected);
       copies:=StrToInt(ParamStr(indexparam));
       if copies<=0 then
        copies:=1;
      end
      else
      // Printer selection
      if ParamStr(indexparam)='-p' then
      begin
       inc(indexparam);
       if indexparam>=Paramcount+1 then
        Raise Exception.Create(SRpNumberexpected);
       printerindex:=TRpPrinterSelect(StrToInt(ParamStr(indexparam)));
      end
      else
      if ParamStr(indexparam)='-collate' then
      begin
       collate:=true;
      end
      else
      begin
       filename:=ParamStr(indexparam);
       inc(indexparam);
       break;
      end;
     inc(indexparam);
    end;
    if indexparam<ParamCount+1 then
    begin
     Raise Exception.Create(SRpTooManyParams)
    end;
    if Length(filename)<1 then
    begin
     PrintHelp;
    end
    else
    begin
     metafile.LoadFromFile(filename);
     try
      if ShowProgress then
      begin
       WriteLn(SRpPrintingFile+':'+filename);
      end;
      if PrintMetafile(metafile,filename,ShowProgress,allpages,
       frompage,topage,copies,collate,false,printerindex) then
       if ShowProgress then
       begin
        WriteLn(SRpPrinted);
       end;
     finally
      if dodeletefile then
       if DeleteFile(filename) then
        if ShowProgress then
        begin
         WriteLn(SRpPrintedFileDeleted);
        end;
     end;
    end;
   finally
    metafile.free;
   end;
  end;
 except
  On E:Exception do
  begin
   Writeln(SRPError,E.Message);
   raise;
  end;
 end;
end.