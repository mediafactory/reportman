program compilerep;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  rpcompilerep in '..\..\..\rpcompilerep.pas';


procedure WriteHelp;
begin
 WriteLn('compilerep Compiles a report into a .exe file');
 WriteLn('Syntax: compilerep filename.rep [-preview] [-showparams] [-compress] [-metafile]');
end;

var
 destinationexe:String;
 correctparam:Boolean;
 docompress:Boolean;
 metaprint:Boolean;
 i:integer;
 dopreview,doshowparams:Boolean;
 filename:String;
begin
  { TODO -oUser -cConsole Main : Insert code here }
  docompress:=false;
  metaprint:=false;
  dopreview:=false;
  doshowparams:=false;
  try
   if ParamCount<1 then
   begin
    WriteHelp;
    Raise Exception.Create('Incorrect Syntax');
   end;
   filename:=ParamStr(1);
   i:=2;
   correctparam:=True;
   while i<=paramcount do
   begin
    correctparam:=false;
    if UpperCase(ParamStr(i))='-COMPRESS' then
    begin
     docompress:=true;
     correctparam:=true;
    end
    else
    if UpperCase(ParamStr(i))='-METAFILE' then
    begin
     metaprint:=true;
     correctparam:=true;
    end
    else
    if UpperCase(ParamStr(i))='-PREVIEW' then
    begin
     dopreview:=true;
     correctparam:=true;
    end
    else
    if UpperCase(ParamStr(i))='-SHOWPARAMS' then
    begin
     doshowparams:=true;
     correctparam:=true;
    end;
    if not correctparam then
     break;
    inc(i);
   end;
   if not correctparam then
   begin
    WriteHelp;
   end
   else
   begin
    destinationexe:=ChangeFileExt(ParamStr(1),'.exe');
    ReportFileToExe(filename,destinationexe,doshowparams,
     dopreview,metaprint,docompress);
   end;
  except
   on E:Exception do
   begin
    WriteToStdError(E.Message+LINE_FEED);
    ExitCode:=1;
   end;
  end;
end.
