unit rpwebpages;

{$I rpconf.inc}

interface

uses SysUtils,Classes,HTTPApp,rpmdconsts,Inifiles,
 rpmdshfolder,rptypes,rpreport,rppdfdriver,Variants,
{$IFDEF USEBDE}
  dbtables,
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
   lusers:TStringList;
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
  public
   procedure WriteLog(aMessage:String);
   procedure ExecuteReport(Request: TWebRequest;Response:TWebResponse);
   procedure CheckLogin(Request:TWebRequest);
   procedure GetWebPage(Request: TWebRequest;apage:TRpWebPage;Response:TWebResponse);
   constructor Create(AOwner:TComponent);
   destructor Destroy;override;
  end;


implementation

procedure TRpWebPageLoader.CheckLogin(Request:TWebRequest);
var
 username,password:string;
 index:integer;
 logincorrect:boolean;
begin
 logincorrect:=false;
 username:=UpperCase(Request.QueryFields.Values['username']);
 password:=Request.QueryFields.Values['password'];
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
 astring:String;
 aliasesstring:String;
 i:integer;
begin
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
 Owner:=AOwner;
 initreaded:=false;
 lusers:=TStringList.Create;
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
 aresult.Add('<form method="get" action="./execute">');
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
 lusers.free;
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
begin
 try
  Ffilenameconfig:=Obtainininamecommonconfig('','','reportmanserver');
  ForceDirectories(ExtractFilePath(ffilenameconfig));
  inif:=TMemInifile.Create(ffilenameconfig);
  try
   laliases.clear;
   lusers.clear;
   inif.CaseSensitive:=false;
   FPagesDirectory:=Trim(inif.Readstring('CONFIG','PAGESDIR',''));
   fport:=inif.ReadInteger('CONFIG','TCPPORT',3060);
   inif.ReadSectionValues('USERS',lusers);
   inif.ReadSectionValues('ALIASES',laliases);
   i:=0;
   while i<lusers.count do
   begin
    if Length(Trim(lusers.strings[i]))<1 then
     LUsers.delete(i)
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
  finally
  inif.free;
  end;
  initreaded:=true;
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
 astring:=showaliaspage;
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
   FillTreeDir(dirpath,alist);
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

// returns emptystring
function TRpWebPageLoader.LoadParamsPage(Request: TWebRequest):string;
var
 pdfreport:TRpReport;
 dirpath,reportname,areportname:string;
 aliasname:string;
 visibleparam:Boolean;
 i:integer;
 astring,inputstring:String;
 aparamstring:String;
begin
 Result:='';
 // Load the report
 pdfreport:=TRpReport.Create(Owner);
 try
  aliasname:=Request.QueryFields.Values['aliasname'];
  if Length(aliasname)>0 then
  begin
   dirpath:=laliases.Values[aliasname];
   reportname:=dirpath+C_DIRSEPARATOR+Request.QueryFields.Values['reportname'];
   reportname:=ChangeFileExt(reportname,'.rep');
   pdfreport.LoadFromFile(reportname);
   // Count visible parameters
   visibleparam:=false;
   for i:=0 to pdfreport.Params.Count-1 do
   begin
    if pdfreport.Params.Items[i].Visible then
    begin
     visibleparam:=true;
     break;
    end;
   end;
   if visibleparam then
   begin
    // Creates the parameters form
    astring:=paramspage;
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
     if pdfreport.Params.Items[i].Visible then
     begin
      aparamstring:=aparamstring+'<tr>'+#10+
       '<td>'+pdfreport.Params.Items[i].Description+'</td>'+#10+
       '<td>'+#10;
      aparamstring:=aparamstring+
       '<input type="text" name="Param'+
       pdfreport.Params.Items[i].Name+'" value="'+
       pdfreport.Params.Items[i].AsString+'">'+#10+
       '</td>'+#10;
      aparamstring:=aparamstring+
       '<td>'+#10+
       '<input type="checkbox" name="NULLParam'+
       pdfreport.Params.Items[i].Name+'" value="NULL"';
      if pdfreport.Params.Items[i].Value=Null then
       aparamstring:=aparamstring+' checked ';
      aparamstring:=aparamstring+'>'+#10+
       ' '+TranslateStr(196,'Null value')+'</td>'+#10+
       '</tr>';
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
 dirpath,reportname:string;
 aliasname:string;
 astream:TMemoryStream;
 paramname,paramvalue:string;
 i,index:integer;
 paramisnull:boolean;
begin
 reportname:='';
 try
  aliasname:=Request.QueryFields.Values['aliasname'];
  if Length(aliasname)>0 then
  begin
   dirpath:=laliases.Values[aliasname];
   reportname:=dirpath+C_DIRSEPARATOR+Request.QueryFields.Values['reportname'];
//   Response.Content:=reportname;
   pdfreport:=CreateReport;
   try
    reportname:=ChangeFileExt(reportname,'.rep');
    pdfreport.LoadFromFile(reportname);
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
      if paramisnull then
       pdfreport.Params.ParamByName(paramname).Value:=Null
      else
      begin
       // Assign the parameter as a string
       pdfreport.Params.ParamByName(paramname).AsString:=paramvalue;
      end;
     end;
    end;
    astream:=TMemoryStream.Create;
    astream.Clear;
    rppdfdriver.PrintReportPDFStream(pdfreport,'',false,true,1,9999,1,
     astream,true);
    astream.Seek(0,soFromBeginning);
    Response.ContentType := 'application/pdf';
    Response.ContentStream:=astream;
    WriteLog(reportname+' Executed ');
   finally
    pdfreport.Free;
   end;
  end;
 except
  On E:Exception do
  begin
   Response.Content:=GenerateError(E.Message);
   WriteLog(SRpError+' - '+reportname+' - '+E.Message);
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


