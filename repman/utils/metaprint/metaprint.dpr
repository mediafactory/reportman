program metaprint;

{$APPTYPE CONSOLE}
uses
  Classes,SysUtils,
{$IFDEF MSWINDOWS}
  rpqtdriver in '..\..\..\rpqtdriver.pas',
  rpmetafile in '..\..\..\rpmetafile.pas',
  rpreport in '..\..\..\rpreport.pas',
  rpconsts in '..\..\..\rpconsts.pas';
{$ENDIF}

{$IFDEF LINUX}
  rpqtdriver in '../../../rpqtdriver.pas',
  rpmetafile in '../../../rpmetafile.pas',
  rpreport in '../../../rpreport.pas',
  rpconsts in '../../../rpconsts.pas';
{$ENDIF}

var
 metafile:TRpMetafileReport;

procedure PrintHelp;
begin
 Writeln(SRpMetaPrint1);
 Writeln(SRpMetaPrint2);
 Writeln(SRpMetaPrint3);
 Writeln(SRpMetaPrint4);
 Writeln(SRpMetaPrint5);
end;


var
 indexparam:integer;
 showprogress:boolean;
 dodeletefile:boolean;
 filename:string;
begin
 try
  { TODO -oUser -cConsole Main : Insert code here }
  if ParamCount<1 then
   PrintHelp
  else
  begin
   showprogress:=true;
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
      if PrintMetafile(metafile,filename,ShowProgress) then
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
