program compilerep;

{$APPTYPE CONSOLE}

uses
  SysUtils,Windows;

const
 MAX_F_SIZE=10000000;

procedure WriteHelp;
begin
 WriteLn('compilerep Compiles a report into a .exe file');
 WriteLn('Syntax: compilerep filename.rep [-preview] [-showparams] [-compress] [-metafile]');
end;

var
 destinationexe:String;
 filename:String;
 amem:PChar;
 han:THandle;
 afile:integer;
 readed,i:integer;
 correctparam:Boolean;
 docompress:Boolean;
 metaprint:Boolean;
 isok:Boolean;
 dopreview,doshowparams:Boolean;
begin
  { TODO -oUser -cConsole Main : Insert code here }
  docompress:=false;
  metaprint:=false;
  dopreview:=false;
  doshowparams:=false;
  try
   amem:=AllocMem(MAX_F_SIZE);
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
     afile:=FileOpen(filename,fmOpenRead or fmShareDenyNone);
     if afile=-1 then
      RaiseLastOsError;
     try
      readed:=FileRead(afile,amem^,MAX_F_SIZE);
      if readed=0 then
       RaiseLastOsError;
     finally
       FileClose(afile);
     end;
     // First duplicates printrepxp.exe or metaprintxp
     if metaprint then
      isok:=CopyFile('metaprintxp.exe',PChar(destinationexe),false)
     else
      isok:=CopyFile('printrepxp.exe',PChar(destinationexe),false);
     if not isok then
      RaiseLastOsError;
     han:=BeginUpdateResource(PChar(destinationexe),false);
     if han=0 then
      RaiseLastOsError;
     try
      if Not UpdateResource(han,RT_RCDATA,PCHAR(100),0,amem,readed) then
       RaiseLastOSError;
      if dopreview then
       if Not UpdateResource(han,RT_RCDATA,PCHAR(101),0,amem,2) then
        RaiseLastOSError;
      if doshowparams then
       if Not UpdateResource(han,RT_RCDATA,PCHAR(102),0,amem,2) then
        RaiseLastOSError;
     finally
      EndUpdateResource(han,False);
     end;
    end;
    // CHeck for UPX
    if docompress then
     WinExec(PChar('upx '+destinationexe),SW_HIDE);
   finally
    FreeMem(amem);
   end;
  except
   on E:Exception do
    begin
     WriteLn(E.Message);
   end;
  end;
end.
