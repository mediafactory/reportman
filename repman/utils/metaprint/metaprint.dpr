program metaprint;

{$APPTYPE CONSOLE}
uses
  Classes,
{$IFDEF MSWINDOWS}
  rpqtdriver in '..\..\..\rpqtdriver.pas',
  rpqtdriver in '..\..\..\rpmetafile.pas',
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
end;



begin
  { TODO -oUser -cConsole Main : Insert code here }
  if ParamCount<>1 then
   PrintHelp
  else
  begin
   metafile:=TRpMetafileReport.Create(nil);
   try
    metafile.LoadFromFile(ParamStr(1));
    PrintMetafile(metafile,ParamStr(1),true);
   finally
    metafile.free;
   end;
  end;
end.
