unit rpwebpages;

{$I rpconf.inc}

interface

uses SysUtils,Classes,HTTPApp,rpmdconsts,Inifiles,rpalias,
{$IFNDEF USEVARIANTS}
 Windows,FileCtrl,asptlb,
{$ENDIF}
 rpmdshfolder,rptypes,rpreport,rppdfdriver,rpparams,rptextdriver,rpsvgdriver,
 rpcsvdriver,
{$IFDEF USEVARIANTS}
 Variants,
{$ENDIF}
{$IFDEF USEBDE}
  dbtables,
{$ENDIF}
{$IFNDEF FORCECONSOLE}
{$IFDEF MSWINDOWS}
  rpgdidriver,
 {$ENDIF}
 {$IFDEF LINUX}
  rpqtdriver,
 {$ENDIF}
{$ENDIF}
 rpmetafile;

const
 REPMAN_LOGIN_LABEL='ReportManagerLoginLabel';
 REPMAN_USER_LABEL='UserNameLabel';
 REPMAN_PASSWORD_LABEL='PasswordLabel';
 REPMAN_INDEX_LABEL='ReportManagerIndexLabel';
 REPMAN_WEBSERVER='RepManWebServer';
 REPMAN_AVAILABLE_ALIASES='AvailableAliasesLabel';
 REPMAN_REPORTS_LABEL='ReportManagerReportsLabel';
 REPMAN_REPORTSLOC_LABEL='ReportsLocationAlias';
 REPMAN_PARAMSLABEL='ReportManagerParamsLabel';
 REPMAN_PARAMSLOCATION='ReportsParamsTableLocation';
 REPMAN_EXECUTELABEL='RepManExecuteLabel';
 REPMAN_HIDDEN='ReportHiddenLocation';
 REPMAN_REPORTTITLE='ReportTitleLocation';
type
 TRpWebPage=(rpwLogin,rpwIndex,rpwVersion,rpwShowParams,rpwShowAlias);

 TRpWebPageLoader=class(TObject)
  private
   Owner:TComponent;
   Ffilenameconfig:string;
   fport:integer;
   laliases:TStringList;
   lusers,lgroups,LUserGroups,LAliasGroups:TStringList;
   aresult:TStringList;
   FPagesDirectory:String;
   initreaded:boolean;
   InitErrorMessage:string;
   LogFileErrorMessage:String;
   loginpage:string;
   indexpage:string;
   showaliaspage:string;
   paramspage:string;
   isadmin:boolean;
   FRpAliasLibs:TRpAlias;
{$IFDEF USEBDE}
   ASession:TSession;
   BDESessionDir:String;
   BDESessionDirOK:String;
{$ENDIF}
   logfileerror:boolean;
   FLogFilename:String;
   function CreateReport:TRpReport;
   procedure InitConfig;
   procedure CheckInitReaded;
   function GenerateError(errmessage:String):string;
   function LoadLoginPage(Request: TWebRequest):string;
   function LoadIndexPage(Request: TWebRequest):string;
   function LoadAliasPage(Request: TWebRequest):string;
   function LoadParamsPage(Request: TWebRequest):string;
   function CheckPrivileges(username,aliasname:String):Boolean;
   procedure ClearLists;
   procedure LoadReport(pdfreport:TRpReport;aliasname,reportname:String);
  public
   procedure WriteLog(aMessage:String);
   procedure ExecuteReport(Request: TWebRequest;Response:TWebResponse);
   procedure CheckLogin(Request:TWebRequest);
   procedure GetWebPage(Request: TWebRequest;apage:TRpWebPage;Response:TWebResponse);
   constructor Create(AOwner:TComponent);
   destructor Destroy;override;
  end;


implementation


procedure TRpWebPageLoader.ClearLists;
var
 i:integer;
begin
 laliases.clear;
 lusers.clear;
 lgroups.Clear;
 for i:=0 to LUserGroups.Count-1 do
 begin
  TStringList(LUserGroups.Objects[i]).free;
 end;
 LUserGroups.Clear;
 for i:=0 to LAliasGroups.Count-1 do
 begin
  TStringList(LAliasGroups.Objects[i]).Free;
 end;
 LAliasGroups.Clear;
end;

function TRpWebPageLoader.CheckPrivileges(username,aliasname:String):Boolean;
var
 i,index:integer;
 lugroups:TStringList;
 lagroups:TStringList;
begin
 Result:=true;
 if username='ADMIN' then
  exit;
 index:=LUserGroups.IndexOf(username);
 if index<0 then
  Raise Exception.Create(SRpAuthFailed+' - '+username);
 lugroups:=TStringList(LUserGroups.Objects[index]);
 index:=LAliasGroups.IndexOf(aliasname);
 if index<0 then
  Raise Exception.Create(SRpAuthFailed+' - '+aliasname);
 lagroups:=TStringList(LAliasGroups.Objects[index]);
 if ((lagroups.Count>0) and (lugroups.Count>0)) then
 begin
  Result:=false;
  for i:=0 to lugroups.Count-1 do
  begin
   if lagroups.IndexOfName(lugroups.Names[i])>=0 then
   begin
    Result:=true;
    break;
   end;
  end;
 end;
end;

procedure TRpWebPageLoader.CheckLogin(Request:TWebRequest);
var
 username,password:string;
 aliasname:String;
 index:integer;
 logincorrect:boolean;
begin
 logincorrect:=false;
 username:=UpperCase(Request.QueryFields.Values['username']);
 password:=Request.QueryFields.Values['password'];
 aliasname:=Request.QueryFields.Values['aliasname'];
 index:=LUsers.IndexOfName(username);
 if index>=0 then
 begin
  if LUsers.Values[username]=password then
  begin
   logincorrect:=true;
   isadmin:=username='ADMIN';
  end;
 end;
 if Not LoginCorrect then
 begin
  Raise Exception.Create(TranslateStr(848,'Incorrect user name or password'));
//   ' User: '+username+' Password: '+password+' Index: '+IntToStr(index));
 end;
 if Length(aliasname)>0 then
 begin
  if not CheckPrivileges(username,aliasname) then
   Raise Exception.Create(TranslateStr(848,'Incorrect user name or password'));
 end;
end;

procedure TRpWebPageLoader.CheckInitReaded;
begin
 if not initreaded then
  Raise Exception.Create(TranslateStr(839,'Configuration file error')+
   '-'+InitErrorMessage+' - '+FFileNameConfig);
end;



function TRpWebPageLoader.LoadLoginPage(Request: TWebRequest):string;
var
 astring:String;
begin
 if Length(FPagesDirectory)<1 then
 begin
  astring:=loginpage;
 end
 else
 begin
  aresult.LoadFromFile(FPagesDirectory+'rplogin.html');
  astring:=aresult.Text;
 end;
 // Substitute translations
 astring:=StringReplace(astring,REPMAN_LOGIN_LABEL,
  TranslateStr(838,'Report Manager Login'),[rfReplaceAll]);
 astring:=StringReplace(astring,REPMAN_USER_LABEL,
  TranslateStr(751,'User Name'),[rfReplaceAll]);
 astring:=StringReplace(astring,REPMAN_PASSWORD_LABEL,
  TranslateStr(752,'Password'),[rfReplaceAll]);
 astring:=StringReplace(astring,REPMAN_WEBSERVER,
  TranslateStr(837,'Report Manager Web Server'),[rfReplaceAll]);

 Result:=astring;
end;

function TRpWebPageLoader.LoadIndexPage(Request: TWebRequest):string;
var
 astring,username:String;
 aliasesstring:String;
 i:integer;
begin
 username:=UpperCase(Request.QueryFields.Values['username']);
 if Length(FPagesDirectory)<1 then
 begin
  astring:=indexpage;
 end
 else
 begin
  aresult.LoadFromFile(FPagesDirectory+'rpindex.html');
  astring:=aresult.Text;
 end;
 astring:=StringReplace(astring,REPMAN_WEBSERVER,
  TranslateStr(837,'Report Manager Web Server'),[rfReplaceAll]);
 astring:=StringReplace(astring,REPMAN_INDEX_LABEL,
  TranslateStr(846,'Report Manager Index'),[rfReplaceAll]);

 aliasesstring:=TranslateStr(847,'Available Report Groups');
 for i:=0 to laliases.Count-1 do
 begin
  if CheckPrivileges(username,laliases.Names[i]) then
   aliasesstring:=aliasesstring+#10+'<p><a href="./showalias?aliasname='+
    laliases.Names[i]+'&'+Request.Query+'">'+laliases.Names[i]+'</a></p>';
 end;

 astring:=StringReplace(astring,REPMAN_AVAILABLE_ALIASES,
  aliasesstring,[rfReplaceAll]);

 Result:=astring;
end;




procedure TRpWebPageLoader.GetWebPage(Request: TWebRequest;apage:TRpWebPage;
 Response:TWebResponse);
var
 astring:string;
 atemp:string;
 i:integer;
begin
 try
  CheckInitReaded;
  if Not (apage in [rpwVersion,rpwLogin]) then
   CheckLogin(Request);
  case apage of
   rpwVersion:
    begin
     astring:='<html><body>'+TranslateStr(837,'Report Manager Web Server')+#10+
      '<p></p>'+TranslateStr(91,'Version')+' '+RM_VERSION+#10+'<p></p>'+
      TranslateStr(743,'Configuration File')+': '+Ffilenameconfig;
     if Length(FLogFileName)>0 then
      astring:=astring+'<p>Log File:'+FLogFileName+'</p>';

     if Length(LogFileErrorMessage)>0 then
     begin
      astring:=astring+'<p>'+LogFileErrorMessage+'</p>';
     end;
     // Configuration
     astring:=astring+'<p>[CONFIG]PAGESDIR='+FPagesDirectory+'</p>';
     astring:=astring+'<p>Configured libs:'+ThousandSeparator;
     // Configured libs
     for i:=0 to FRpAliasLibs.Connections.Count-1 do
     begin
      astring:=astring+'|'+FRpAliasLibs.Connections.Items[i].Alias;
     end;
     astring:=astring+'</p>';
     astring:=astring+'<p>Decimal separator:'+DecimalSeparator+'</p>';
     astring:=astring+'<p>Thousand separator:'+ThousandSeparator+'</p>';
     // Environment variables
     atemp:=GetEnvironmentVariable('LANG');
     astring:=astring+'<p>LANG='+atemp+'</p>';
     atemp:=GetEnvironmentVariable('OLD_LC_NUMERIC');
     astring:=astring+'<p>OLD_LC_NUMERIC='+atemp+'</p>';
     atemp:=GetEnvironmentVariable('LC_NUMERIC');
     astring:=astring+'<p>LC_NUMERIC='+atemp+'</p>';
     atemp:=GetEnvironmentVariable('KYLIX_DEFINEDENVLOCALES');
     astring:=astring+'<p>KYLIX_DEFINEDENVLOCALES='+atemp+'</p>';
     atemp:=GetEnvironmentVariable('KYLIX_THOUSAND_SEPARATOR');
     astring:=astring+'<p>KYLIX_THOUSAND_SEPARATOR='+atemp+'</p>';
     atemp:=GetEnvironmentVariable('KYLIX_DECIMAL_SEPARATOR');
     astring:=astring+'<p>KYLIX_DECIMAL_SEPARATOR='+atemp+'</p>';
     atemp:=GetEnvironmentVariable('KYLIX_DATE_SEPARATOR');
     astring:=astring+'<p>KYLIX_DATE_SEPARATOR='+atemp+'</p>';
     atemp:=GetEnvironmentVariable('KYLIX_TIME_SEPARATOR');
     astring:=astring+'<p>KYLIX_TIME_SEPARATOR='+atemp+'</p>';
     astring:=astring+'</body></html>';
     Response.Content:=astring;
    end;
   rpwLogin:
    begin
     astring:=LoadLoginPage(Request);
     Response.Content:=astring;
    end;
   rpwIndex:
    begin
     astring:=LoadIndexPage(Request);
     Response.Content:=astring;
    end;
   rpwShowAlias:
    begin
     astring:=LoadAliasPage(Request);
     Response.Content:=astring;
    end;
   rpwShowParams:
    begin
     astring:=LoadParamsPage(Request);
     if Length(astring)<1 then
      ExecuteReport(Request,Response)
     else
      Response.Content:=astring;
    end;
  end;
 except
  On E:Exception do
  begin
   Response.Content:=GenerateError(E.Message);
  end;
 end;
end;

function TRpWebPageLoader.GenerateError(errmessage:String):string;
begin
 Result:='Error: '+errmessage;
end;

constructor TRpWebPageLoader.Create(AOwner:TComponent);
begin
 inherited Create;
 Owner:=AOwner;
 initreaded:=false;
 FRpAliasLibs:=TRpAlias.Create(nil);

 lusers:=TStringList.Create;
 lgroups:=TStringList.Create;
 lusergroups:=TStringList.Create;
 laliasgroups:=TStringList.Create;
 laliases:=TStringList.Create;
 aresult:=TStringList.Create;

 aresult.clear;
 aresult.Add('<html>');
 aresult.Add('<head>');
 aresult.Add('<title>RepManWebServer</title>');
 aresult.Add('<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">');
 aresult.Add('</head>');

 aresult.Add('<body bgcolor="#FFFFFF">');
 aresult.Add('<h3 align="center">ReportManagerLoginLabel</h3>');
 aresult.Add('<form method="get" action="./index">');
 aresult.Add('<table width="90%" border="1">');
 aresult.Add('<tr>');
 aresult.Add('<td>UserNameLabel</td>');
 aresult.Add('<td>');
 aresult.Add('<input type="text" name="username">');
 aresult.Add('</td>');
 aresult.Add('</tr>');
 aresult.Add('<tr>');
 aresult.Add('<td>PasswordLabel</td>');
 aresult.Add('<td>');
 aresult.Add('<input type="password" name="password">');
 aresult.Add('</td>');
 aresult.Add('</tr>');
 aresult.Add('</table>');
 aresult.Add('<p>');
 aresult.Add('<input type="submit" value="ReportManagerLoginLabel">');
 aresult.Add('</p>');
 aresult.Add('</form>');
 aresult.Add('<p>&nbsp; </p>');
 aresult.Add('</body>');
 aresult.Add('</html>');
 loginpage:=aresult.Text;

 aresult.clear;
 aresult.Add('<html>');
 aresult.Add('<head>');
 aresult.Add('<title>RepManWebServer</title>');
 aresult.Add('<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">');
 aresult.Add('</head>');
 aresult.Add('<body bgcolor="#FFFFFF">');
 aresult.Add('<h3 align="center"> ReportManagerIndexLabel</h3>');
 aresult.Add('<p>AvailableAliasesLabel</p>');
 aresult.Add('</body>');
 aresult.Add('</html>');
 indexpage:=aresult.Text;


 aresult.clear;
 aresult.Add('<html>');
 aresult.Add('<head>');
 aresult.Add('<title>RepManWebServer</title>');
 aresult.Add('<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">');
 aresult.Add('</head>');
 aresult.Add('<body bgcolor="#FFFFFF">');
 aresult.Add('<h3 align="center">ReportManagerReportsLabel</h3>');
 aresult.Add('<p>ReportsLocationAlias</p>');
 aresult.Add('</body>');
 aresult.Add('</html>');
 showaliaspage:=aresult.text;

 aresult.clear;
 aresult.Add('<html>');
 aresult.Add('<head>');
 aresult.Add('<title>RepManWebServer</title>');
 aresult.Add('<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">');
 aresult.Add('</head>');
 aresult.Add('<body bgcolor="#FFFFFF">');
 aresult.Add('<h2>ReportTitleLocation<h2>');
 aresult.Add('<p><h3 align="center">ReportManagerParamsLabel</h3></p>');
 aresult.Add('<form method="get" action="./execute.pdf">');
 aresult.Add('ReportHiddenLocation');
 aresult.Add('<p>ReportsParamsTableLocation</p>');
 aresult.Add('<p>');
 aresult.Add('<input type="submit" value="RepManExecuteLabel">');
 aresult.Add('</p>');

 aresult.Add('</form>');
 aresult.Add('</body>');
 aresult.Add('</html>');
 paramspage:=aresult.text;

 InitConfig;
end;

destructor TRpWebPageLoader.Destroy;
begin
 FRpAliasLibs.free;
 ClearLists;
 lusers.free;
 lgroups.free;
 LUserGroups.free;
 LAliasGroups.free;
 laliases.Free;
 aresult.free;

 inherited Destroy;
end;

procedure TRpWebPageLoader.WriteLog(aMessage:String);
var
 messa:String;
 FLogFile:TFileStream;
begin
 if logfileerror then
  exit;
 if Length(FLogFileName)<1 then
  exit;
 messa:=FormatDateTime('dd/mm/yyyy hh:nn:ss - ',Now)+aMessage;
{$IFDEF MSWINDOWS}
 messa:=messa+#10+#13;
{$ENDIF}
{$IFDEF LINUX}
 messa:=messa+#10;
{$ENDIF}
 FLogFile:=TFileStream.Create(FLogFilename,fmOpenReadWrite or fmShareDenyNone);
 try
  FLogFile.Seek(0,soFromEnd);
  FLogFile.Write(messa[1],Length(messa));
 finally
  FLogFile.Free;
 end;
end;

procedure TRpWebPageLoader.InitConfig;
var
 inif:TMemInifile;
 i:integer;
 FLogFile:TFileStream;
 alist:TStringList;
begin
 try
  Ffilenameconfig:=Obtainininamecommonconfig('','','reportmanserver');
  ForceDirectories(ExtractFilePath(ffilenameconfig));
  inif:=TMemInifile.Create(ffilenameconfig);
  try
   ClearLists;
{$IFDEF USEVARIANTS}
   inif.CaseSensitive:=false;
{$ENDIF}
   FPagesDirectory:=Trim(inif.Readstring('CONFIG','PAGESDIR',''));
   fport:=inif.ReadInteger('CONFIG','TCPPORT',3060);
   inif.ReadSectionValues('USERS',lusers);
   inif.ReadSectionValues('GROUPS',lgroups);
   inif.ReadSectionValues('ALIASES',laliases);
   i:=0;
   while i<lusers.count do
   begin
    if Length(Trim(lusers.strings[i]))<1 then
     LUsers.delete(i)
    else
     inc(i);
   end;
   while i<lgroups.count do
   begin
    if Length(Trim(lgroups.strings[i]))<1 then
     LGroups.delete(i)
    else
     inc(i);
   end;
   i:=0;
   while i<laliases.count do
   begin
    if Length(Trim(laliases.strings[i]))<1 then
     laliases.delete(i)
    else
     inc(i);
   end;
   for i:=0 to lusers.count-1 do
   begin
    if Length(lusers.Names[i])<1 then
     lusers.Strings[i]:=lusers.Strings[i]+'=';
   end;
   for i:=0 to laliases.count-1 do
   begin
    if Length(laliases.Names[i])<1 then
     laliases.Strings[i]:=laliases.Strings[i]+'=';
   end;
   if lusers.IndexOfName('ADMIN')<0 then
    lusers.Add('ADMIN=');
   // Read privilege lists
   for i:=0 to lusers.Count-1 do
   begin
    if lusers.Names[i]<>'ADMIN' then
    begin
     alist:=TStringList.Create;
     LUserGroups.AddObject(lusers.Names[i],alist);
     inif.ReadSectionValues('USERGROUPS'+lusers.Names[i],alist);
    end;
   end;
   for i:=0 to LAliases.Count-1 do
   begin
    alist:=TStringList.Create;
    LAliasGroups.AddObject(LAliases.Names[i],alist);
    inif.ReadSectionValues('GROUPALLOW'+LAliases.Names[i],alist);
   end;


   // Gets the log file and try to create it
   logfileerror:=false;
   LogFileErrorMessage:='';
   FLogFilename:=inif.Readstring('CONFIG','LOGFILE','');
   if Length(FLogFilename)>0 then
   begin
    if Not (FileExists(FLogFileName)) then
    begin
     try
      FLogFile:=TFileStream.Create(FLogFilename,fmOpenReadWrite or fmCreate);
      FLogFile.Free;
     except
      On E:Exception do
      begin
       logfileerror:=true;
      LogFileErrorMessage:=E.Message;
      end;
     end;
    end;
   end;
  finally
   inif.free;
  end;
  FRpAliasLibs.Connections.LoadFromFile(FFilenameConfig);
  initreaded:=true;
 except
  on E:Exception do
  begin
   InitErrorMessage:=E.Message;
  end;
 end;
end;


function TRpWebPageLoader.LoadAliasPage(Request: TWebRequest):string;
var
 astring,reportname:String;
 reportlist:String;
 aliasname:String;
 i:integer;
 alist:TStringList;
 dirpath:String;
begin
 if Length(FPagesDirectory)<1 then
 begin
  astring:=showaliaspage;
 end
 else
 begin
  aresult.LoadFromFile(FPagesDirectory+'rpalias.html');
  astring:=aresult.Text;
 end;
 astring:=StringReplace(astring,REPMAN_WEBSERVER,
  TranslateStr(837,'Report Manager Web Server'),[rfReplaceAll]);
 astring:=StringReplace(astring,REPMAN_REPORTS_LABEL,
  TranslateStr(837,'Reports'),[rfReplaceAll]);
 reportlist:='';
 aliasname:=Request.QueryFields.Values['aliasname'];
 if Length(aliasname)>0 then
 begin
  dirpath:=laliases.Values[aliasname];
  alist:=TStringList.Create;
  try
   if Length(dirpath)<1 then
    Raise Exception.Create(SRPAliasNotExists);
   if dirpath[1]=':' then
    FRpAliasLibs.Connections.FillTreeDir(Copy(dirpath,2,Length(dirpath)),alist)
   else
   begin
    if Not DirectoryExists(dirpath) then
     Raise Exception.Create(SrpDirectoryNotExists+ ' - '+dirpath);
    FillTreeDir(dirpath,alist);
   end;
   for i:=0 to alist.Count-1 do
   begin
    reportname:=alist.Strings[i];
    if Length(reportname)>0 then
    begin
     if reportname[1]=C_DIRSEPARATOR then
      reportname:=Copy(reportname,2,Length(reportname));
     reportlist:=reportlist+#10+'<p><a href="./showparams?reportname='+
      alist.Strings[i]+'&'+Request.Query+'">'+reportname+'</a></p>';
    end;
   end;
  finally
   alist.free;
  end;
 end;
 astring:=StringReplace(astring,REPMAN_REPORTSLOC_LABEL,
  reportlist,[rfReplaceAll]);
 Result:=astring;
end;

procedure TRpWebPageLoader.LoadReport(pdfreport:TRpReport;aliasname,reportname:String);
var
 dirpath:String;
 astream:TStream;
begin
 dirpath:=laliases.Values[aliasname];
 if Length(dirpath)<1 then
  Raise Exception.Create(SRPAliasNotExists);
 if dirpath[1]=':' then
 begin
  astream:=FRpAliasLibs.Connections.GetReportStream(Copy(dirpath,2,Length(dirpath)),
   ExtractFileName(reportname));
  try
   pdfreport.LoadFromStream(astream);
  finally
   astream.free;
  end;
 end
 else
 begin
  reportname:=dirpath+C_DIRSEPARATOR+reportname;
  reportname:=ChangeFileExt(reportname,'.rep');
  pdfreport.LoadFromFile(reportname);
 end;
end;

// returns emptystring
function TRpWebPageLoader.LoadParamsPage(Request: TWebRequest):string;
var
 pdfreport:TRpReport;
 aliasname,reportname,areportname:string;
 visibleparam:Boolean;
 i,k,selectedindex:integer;
 astring,inputstring:String;
 aparamstring:String;
 aparam:TRpParam;
begin
 aliasname:=Request.QueryFields.Values['aliasname'];
 reportname:=Request.QueryFields.Values['reportname'];
 Result:='';
 // Load the report
 pdfreport:=TRpReport.Create(Owner);
 try
  if Length(aliasname)>0 then
  begin
   LoadReport(pdfreport,aliasname,reportname);
   pdfreport.Params.UpdateLookup;
   // Count visible parameters
   visibleparam:=false;
   for i:=0 to pdfreport.Params.Count-1 do
   begin
    if ((pdfreport.Params.Items[i].Visible) and
     (not pdfreport.Params.Items[i].NeverVisible)) then
    begin
     visibleparam:=true;
     break;
    end;
   end;
   if visibleparam then
   begin
    // Creates the parameters form
    if Length(FPagesDirectory)<1 then
    begin
     astring:=paramspage;
    end
    else
    begin
     aresult.LoadFromFile(FPagesDirectory+'rpparams.html');
     astring:=aresult.Text;
    end;
    astring:=StringReplace(astring,REPMAN_WEBSERVER,
     TranslateStr(837,'Report Manager Web Server'),[rfReplaceAll]);
    astring:=StringReplace(astring,REPMAN_PARAMSLABEL,
     TranslateStr(135,'Parameter values'),[rfReplaceAll]);
    astring:=StringReplace(astring,REPMAN_EXECUTELABEL,
     TranslateStr(779,'Execute'),[rfReplaceAll]);
    areportname:=Request.QueryFields.Values['reportname'];
    if Length(areportname)>0 then
    begin
     if areportname[1]=C_DIRSEPARATOR then
      areportname:=Copy(areportname,2,Length(areportname));
    end;
    astring:=StringReplace(astring,REPMAN_REPORTTITLE,
     areportname,[rfReplaceAll]);

    inputstring:='<input type="hidden" name="reportname" '+
    'value="'+Request.QueryFields.Values['reportname']+'">';
    inputstring:=inputstring+'<input type="hidden" name="aliasname" '+
    'value="'+Request.QueryFields.Values['aliasname']+'">';
    inputstring:=inputstring+'<input type="hidden" name="username" '+
    'value="'+Request.QueryFields.Values['username']+'">';
    inputstring:=inputstring+'<input type="hidden" name="password" '+
    'value="'+Request.QueryFields.Values['password']+'">';
    astring:=StringReplace(astring,REPMAN_HIDDEN,
     inputstring,[rfReplaceAll]);

    // Add previous parameters
    aparamstring:='<table width="90%" border="1">'+#10;
    for i:=0 to pdfreport.Params.Count-1 do
    begin
     aparam:=pdfreport.Params.Items[i];
     if ((aparam.Visible) and (not aparam.NeverVisible))  then
     begin
      aparamstring:=aparamstring+'<tr>'+#10+
       '<td>'+HtmlEncode(aparam.Description)+'</td>'+#10+
       '<td>'+#10;
      case aparam.ParamType of
       rpParamBool:
        begin
         aparamstring:=aparamstring+
          '<select name="Param'+aparam.Name+'" ';
         if Length(aparam.Hint)>0 then
          aparamstring:=aparamstring+' alt="'+HtmlEncode(aparam.Hint)+'" ';
         if aparam.IsReadOnly then
          aparamstring:=aparamstring+' readonly ';
         aparamstring:=aparamstring+'>'+#10;
         aparamstring:=aparamstring+'<option value="'+
           BoolToStr(false,true)+'" ';
         if Not VarIsNull(aparam.Value) then
          if Not aparam.Value then
           aparamstring:=aparamstring+' selected ';
         aparamstring:=aparamstring+'>'+HtmlEncode(SRpYes)+
          '</option>'+#10;
         aparamstring:=aparamstring+'<option value="'+
           BoolToStr(true,true)+'" ';
         if Not VarIsNull(aparam.Value) then
          if aparam.Value then
           aparamstring:=aparamstring+' selected ';
         aparamstring:=aparamstring+'>'+HtmlEncode(SRpNo)+
          '</option>'+#10;
         aparamstring:=aparamstring+
          '</select>'+#10;
        end;
       rpParamMultiple:
        begin
         aparamstring:=aparamstring+'<select name="Param'+
          aparam.Name+'" ';
         aparamstring:=aparamstring+' multiple ';
         if Length(aparam.Hint)>0 then
           aparamstring:=aparamstring+' alt="'+HtmlEncode(aparam.Hint)+'" ';
         if aparam.Isreadonly then
          aparamstring:=aparamstring+' readonly ';
         aparamstring:=aparamstring+'>'+#10;
         for k:=0 to aparam.Items.Count-1 do
         begin
          aparamstring:=aparamstring+'<option value="'+
            IntToStr(k)+'" ';
          if aparam.Selected.IndexOf(IntToStr(k))>=0 then
           aparamstring:=aparamstring+' selected ';
          aparamstring:=aparamstring+'>'+HtmlEncode(aparam.Items.Strings[k])+
           '</option>'+#10;
         end;
         aparamstring:=aparamstring+'</select>'+#10;
        end;
       rpParamList:
        begin
         aparamstring:=aparamstring+'<select name="Param'+
          aparam.Name+'" ';
         if Length(aparam.Hint)>0 then
           aparamstring:=aparamstring+' alt="'+HtmlEncode(aparam.Hint)+'" ';
         if aparam.Isreadonly then
          aparamstring:=aparamstring+' readonly ';
         aparamstring:=aparamstring+'>'+#10;
         if Not VarIsNull(aparam.value) then
         begin
          if VarType(aparam.Value)=varInteger then
           selectedindex:=aparam.Value
          else
           selectedindex:=aparam.Values.IndexOf(String(aparam.Value));
         end
         else
          selectedindex:=-1;
         for k:=0 to aparam.Items.Count-1 do
         begin
          aparamstring:=aparamstring+'<option value="'+
            IntToStr(k)+'" ';
          if k=selectedindex then
           aparamstring:=aparamstring+' selected ';
          aparamstring:=aparamstring+'>'+HtmlEncode(aparam.Items.Strings[k])+
           '</option>'+#10;
         end;
         aparamstring:=aparamstring+'</select>'+#10;
        end;
       else
       begin
        aparamstring:=aparamstring+
         '<input type="text" name="Param'+
         aparam.Name+'" ';
        if Length(aparam.Hint)>0 then
         aparamstring:=aparamstring+' alt="'+HtmlEncode(aparam.Hint)+'" ';
        aparamstring:=aparamstring+
         ' value="'+HtmlEncode(aparam.AsString)+'">';
       end;
       if aparam.IsReadOnly then
        aparamstring:=aparamstring+' readonly >'+#10;
      end;
      aparamstring:=aparamstring+'</td>'+#10;
      if pdfreport.Params.Items[i].AllowNulls then
      begin
       aparamstring:=aparamstring+
        '<td>'+#10+
        '<input type="checkbox" name="NULLParam'+
        pdfreport.Params.Items[i].Name+'" value="NULL"';
       if pdfreport.Params.Items[i].Value=Null then
        aparamstring:=aparamstring+' checked ';
       aparamstring:=aparamstring+'>'+#10+
        ' '+TranslateStr(196,'Null value')+'</td>'+#10+
        '</tr>';
      end;
{    <tr>
      <td>LabelParam2</td>
      <td>
        <select name="Param2Value">
          <option value="True">True</option>
          <option value="False">False</option>
        </select>
      </td>
      <td>
        <input type="checkbox" name="Param1Null2" value="Null" checked>
        Null </td>
    </tr>
}    end;
    end;
    aparamstring:=aparamstring+
     '</table>'+#10;
    // Insert the params table
    astring:=StringReplace(astring,REPMAN_PARAMSLOCATION,
     aparamstring,[rfReplaceAll]);
    Result:=astring;
   end;
  end;
 finally
  pdfreport.Free;
 end;
end;

procedure TRpWebPageLoader.ExecuteReport(Request: TWebRequest;Response:TWebResponse);
var
 pdfreport:TRpReport;
 username,reportname:string;
 aliasname:string;
 astream:TMemoryStream;
 paramname,paramvalue:string;
 dometafile:boolean;
 dosvg,dotxt,docsv:Boolean;
 i,index,pageindex,k:integer;
 paramisnull:boolean;
 adriver:TRpPDFDriver;
 param:TRpParam;
begin
 dometafile:=false;
 docsv:=false;
 dosvg:=false;
 dotxt:=false;
 username:=UpperCase(Request.QueryFields.Values['username']);
 reportname:='';
 try
  aliasname:=Request.QueryFields.Values['aliasname'];
  reportname:=Request.QueryFields.Values['reportname'];
  pdfreport:=CreateReport;
  try
   LoadReport(pdfreport,aliasname,reportname);
   // Assigns parameters to the report
   for i:=0 to Request.QueryFields.Count-1 do
   begin
    if Pos('Param',Request.QueryFields.Names[i])=1 then
    begin
     paramname:=Copy(Request.QueryFields.Names[i],6,Length(Request.QueryFields.Names[i]));
     paramvalue:=Request.QueryFields.Values[Request.QueryFields.Names[i]];
     paramisnull:=false;
     index:=Request.QueryFields.IndexOfName('NULLParam'+paramname);
     if index>=0 then
     begin
      if Request.QueryFields.Values[Request.QueryFields.Names[index]]='NULL' then
       paramisnull:=True;
     end;
     param:=pdfreport.Params.ParamByName(paramname);
     if param.ParamType=rpParamMultiple then
     begin
      param.Selected.Clear;
      for k:=0 to Request.QueryFields.Count-1 do
      begin
       if Request.QueryFields.Names[k]='Param'+paramname then
       begin
        param.Selected.Add(Request.QueryFields.ValueFromIndex[k]);
       end;
      end;
     end
     else
     if paramisnull then
      param.Value:=Null
     else
     begin
      // Assign the parameter as a string
      if param.ParamType=rpParamList then
       param.Value:=param.Values[StrToInt(paramvalue)]
      else
       pdfreport.Params.ParamByName(paramname).AsString:=paramvalue;
     end;
    end;
    if Uppercase(Request.QueryFields.Names[i])='METAFILE' then
    begin
     dometafile:=Request.QueryFields.Values['METAFILE']='1';
     docsv:=Request.QueryFields.Values['METAFILE']='2';
     dotxt:=Request.QueryFields.Values['METAFILE']='3';
     dosvg:=Request.QueryFields.Values['METAFILE']='4';
    end;
   end;
   // Assigns pusername param if exists
   index:=pdfreport.Params.IndexOf('PUSERNAME');
   if index>=0 then
    pdfreport.Params.ParamByName('PUSERNAME').Value:=username;
   astream:=TMemoryStream.Create;
   astream.Clear;
   if dometafile then
   begin
{$IFDEF FORCECONSOLE}
    rppdfdriver.PrintReportMetafileStream(pdfreport,'',false,true,1,9999,1,
     astream,true,false);
{$ENDIF}
{$IFNDEF FORCECONSOLE}
 {$IFDEF MSWINDOWS}
    rpgdidriver.ExportReportToPDFMetaStream(pdfreport,'',
     false,true,1,9999,1,false,astream,true,false,true);
 {$ENDIF}
 {$IFDEF LINUX}
    rpqtdriver.ExportReportToPDFMetaStream(pdfreport,'',
     false,true,1,9999,1,false,astream,true,false,true);
 {$ENDIF}
{$ENDIF}
    Response.Content:='Executed, size:'+IntToStr(astream.size);
    Response.ContentType := 'application/rpmf';
    astream.Seek(0,soFromBeginning);
    Response.ContentStream:=astream;
    WriteLog(reportname+' Executed Metafile');
   end
   else
   if dotxt then
   begin
    rptextdriver.PrintReportToStream(pdfreport,'',false,true,1,9999,1,
     astream,true,Request.QueryFields.Values['OEMCONVERT']='1','PLAIN');
    Response.Content:='Executed, size:'+IntToStr(astream.size);
    Response.ContentType := 'text/plain';
    astream.Seek(0,soFromBeginning);
    Response.ContentStream:=astream;
    WriteLog(reportname+' Executed Text');
   end
   else
   if docsv then
   begin
    adriver:=TRpPdfDriver.Create;
    pdfreport.TwoPass:=true;
    pdfreport.PrintAll(adriver);
    rpcsvdriver.ExportMetafileToCSVStream(pdfreport.metafile,
     astream,false,true,1,9999);
    Response.Content:='Executed, size:'+IntToStr(astream.size);
    Response.ContentType := 'text/plain';
    astream.Seek(0,soFromBeginning);
    Response.ContentStream:=astream;
    WriteLog(reportname+' Executed CSV');
   end
   else
   if dosvg then
   begin
    adriver:=TRpPdfDriver.Create;
    pdfreport.TwoPass:=true;
    pdfreport.PrintAll(adriver);
    if Request.QueryFields.Values['PAGEINDEX']='' then
     pageindex:=1
    else
     pageindex:=StrToInt(Request.QueryFields.Values['PAGEINDEX']);
    rpsvgdriver.MetafilePageToSVG(pdfreport.metafile,pageindex,astream,'','');
    Response.Content:='Executed, size:'+IntToStr(astream.size);
    Response.ContentType := 'application/svg';
    astream.Seek(0,soFromBeginning);
    Response.ContentStream:=astream;
    WriteLog(reportname+' Executed SVG');
   end
   else
   begin
{$IFDEF FORCECONSOLE}
    rppdfdriver.PrintReportPDFStream(pdfreport,'',false,true,1,9999,1,
     astream,true,false);
{$ENDIF}
{$IFNDEF FORCECONSOLE}
 {$IFDEF MSWINDOWS}
    rpgdidriver.ExportReportToPDFMetaStream(pdfreport,'',
     false,true,1,9999,1,false,astream,true,false,false);
 {$ENDIF}
 {$IFDEF LINUX}
    rpqtdriver.ExportReportToPDFMetaStream(pdfreport,'',
     false,true,1,9999,1,false,astream,true,false,false);
 {$ENDIF}
{$ENDIF}
    astream.Seek(0,soFromBeginning);
    Response.Content:='Executed, size:'+IntToStr(astream.size);
    Response.ContentType := 'application/pdf';
    Response.ContentStream:=astream;
    WriteLog(reportname+' Executed PDF');
   end;
  finally
   pdfreport.Free;
  end;
 except
  On E:Exception do
  begin
   Response.Content:=GenerateError(E.Message);
  end;
 end;
end;

// Returns parameter page if no params available

function TRpWebPageLoader.CreateReport:TRpReport;
{$IFDEF USEBDE}
var
 sesname:string;
{$ENDIF}
begin
{$IFDEF USEBDE}
 if Not Assigned(ASession) then
 begin
  // If can not create session omit it
  try
   ASession:=TSession.Create(Owner);
   ASession.AutoSessionName:=True;
   ASession.Open;
   sesname:=ASession.SessionName;
   ASession.Close;
   ASession.PrivateDir:=ChangeFileExt(Obtainininamecommonconfig('','BDESessions','Session'+ASession.SessionName),'');
   BDESessionDir:=ASession.PrivateDir;
   ForceDirectories(ASession.PrivateDir);
   ASession.Open;
   BDESessionDirOk:=ASession.PrivateDir;
  except
   ASession.free;
   ASession:=nil;
  end;
 end;
{$ENDIF}
 Result:=TRpReport.Create(nil);
{$IFDEF USEBDE}
 if Assigned(ASession) then
  Result.DatabaseInfo.BDESession:=ASession;
{$ENDIF}
end;


end.


