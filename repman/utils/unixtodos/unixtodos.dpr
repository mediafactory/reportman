{ Converts files from UNIX to DOS format }
{ Also repairs DOS format files lost #13 }

program unixtodos;

{$APPTYPE CONSOLE}

uses
  SysUtils,Classes;

resourcestring
 SRpmUnixToDos='unixtodos - a conversion tool for text files from UNIX to DOS';
 SRpmUnixToDos2='Usage: unixtodos pattern_file [pattern_file] ...';
 SRpmSourcefile='Source file';
 SRpmDestinationfile='Destination file';
 SRpmOriginSize='Source file size';
 SRpmBytes='Bytes';
 SRpmCRIgnored='CR ignored';
 SRpmConverted='CR converted';
 SRpmCRDFormatdos='CR DOS';
 SRpmBytesWrite='Bytes wrote';
 SRpmFilesChanged='Files changed';
 SRpmFilesProcessed='Files processed';
 SRpmLastFilename='Last filename';
 SRpmExceptionError='Exception';

procedure ShowHelp;
begin
 WriteLn(SRpmUnixToDos);
 WriteLn(SRpmUnixToDos2);
end;

var
 asource,adestination:string;
 converted,crignored,i,asize:integer;
 crformatdos:integer;
 astream:TFileStream;
 ifile:integer;
 attributes:integer;
 srec:TSearchRec;
 processed:integer;
 changed:integer;
 retvalue:integer;
 afilename:TFileName;
 prefix:string;


procedure UnixToDosFile(afilename:String);
var
 haschanged:boolean;
begin
 astream:=TFileStream.Create(afilename,fmOpenRead or fmShareDenyWrite);
 try
  SetLength(asource,astream.size);
  astream.Read(asource[1],astream.size);
 finally
  astream.Free;
 end;
 converted:=0;
 crignored:=0;
 crformatdos:=0;
 adestination:='';
 asize:=Length(asource);
 i:=1;
 while i<=asize do
 begin
  // Ignores the #13
  if asource[i]=#13 then
  begin
   if (i+1)<=asize then
   begin
    if asource[i+1]=#10 then
    begin
     adestination:=adestination+#13+#10;
     inc(crformatdos);
     inc(i);
    end
    else
     inc(crignored)
   end
   else
    inc(crignored)
  end
  else
  begin
   if asource[i]=#10 then
   begin
    adestination:=adestination+#13+#10;
   end
   else
    adestination:=adestination+asource[i];
  end;
  inc(i);
 end;
 haschanged:=((crignored>0) or (converted>0));
 if haschanged then
 begin
  // Finally write
  astream:=TFileStream.Create(afilename,fmCreate);
  try
   astream.Write(adestination[1],Length(adestination));
  finally
   astream.Free;
  end;
  Writeln(SRpmSourcefile+':'+afilename);
  Writeln(SRpmOriginSize+':'+FormatFloat('####,####0',Length(adestination))+' '+SRpmBytes);
  WriteLn(SRpmBytesWrite+':'+FormatFloat('####,###0',Length(adestination)));
  Writeln(SRpmCRIgnored+':'+FormatFloat('####,####0',crignored));
  Writeln(SRpmConverted+':'+FormatFloat('####,####0',converted));
  Writeln(SRpmCRDFormatdos+':'+FormatFloat('####,####0',crformatdos));
  inc(changed);
 end;
end;




begin
 processed:=0;
 changed:=0;
 afilename:='';
  { TODO -oUser -cConsole Main : Insert code here }
 try
   if (ParamCount)<1 then
   begin
    ShowHelp;
    exit;
   end;
   for ifile:=1 to ParamCount do
   begin
    prefix:='';
    prefix:=ExtractFilePath(ParamStr(ifile));
    attributes:=0;
    retvalue:=FindFirst(ParamStr(ifile),attributes,srec);
    if retvalue=0 then
    begin
     try
      repeat
       inc(processed);
       afilename:=srec.Name;
       afilename:=prefix+afilename;
       UnixToDosFile(afilename);
       retvalue:=FindNext(srec);
      until retvalue<>0;
     finally
      FindClose(srec);
     end;
    end;
    Writeln(SRpmFilesProcessed+':'+FormatFloat('####,####0',processed));
    Writeln(SRpmFilesChanged+':'+FormatFloat('####,####0',changed));
   end;
 except
  on E:Exception do
  begin
   WriteLn(SRpmExceptionError+':'+E.Message);
   Writeln(SRpmFilesProcessed+':'+FormatFloat('####,####0',processed));
   Writeln(SRpmFilesChanged+':'+FormatFloat('####,####0',changed));
   Writeln(SRpmLastFilename+':'+afilename);
  end;
 end;
end.
