unit rpwebpages;

interface

uses SysUtils,Classes,HTTPApp,rpmdconsts,Inifiles,
 rpmdshfolder,rptypes;

const
 REPMAN_LOGIN_LABEL='ReportManagerLoginLabel';
 REPMAN_USER_LABEL='UserNameLabel';
 REPMAN_PASSWORD_LABEL='PasswordLabel';
 REPMAN_INDEX_LABEL='ReportManagerIndexLabel';
 REPMAN_WEBSERVER='RepManWebServer';
 REPMAN_CONFIG_LABEL='ConfigIndexLabel';
 REPMAN_AVAILABLE_ALIASES='AvailableAliasesLabel';
 REPMAN_CONFIGUR_LABEL='RepManConfigLabel';
 REPMAN_REPORTS_LABEL='ReportManagerReportsLabel';
 REPMAN_REPORTSLOC_LABEL='ReportsLocationAlias';
type
 TRpWebPage=(rpwLogin,rpwConfig,rpwIndex,rpwVersion,rpwShowAlias);

 TRpWebPageLoader=class(TObject)
  private
   Ffilenameconfig:string;
   fport:integer;
   laliases:TStringList;
   lusers:TStringList;
   aresult:TStringList;
   FPagesDirectory:String;
   initreaded:boolean;
   InitErrorMessage:string;
   loginpage:string;
   indexpage:string;
   configpage:string;
   showaliaspage:string;
   isadmin:boolean;
   procedure InitConfig;
   procedure CheckInitReaded;
   function GenerateError(errmessage:String):string;
   function LoadLoginPage(Request: TWebRequest):string;
   function LoadIndexPage(Request: TWebRequest):string;
   function LoadConfigPage(Request: TWebRequest):string;
   function LoadAliasPage(Request: TWebRequest):string;
  public
   procedure CheckLogin(Request:TWebRequest);
   function GetWebPage(Request: TWebRequest;apage:TRpWebPage):String;
   constructor Create;
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
 adminstring:String;
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
 // Substitute translations
 if isadmin then
  adminstring:='<p><a href="./config">'+
  TranslateStr(849,'Report Manager Configuration')+'</a></p>'
 else
  adminstring:='';
 astring:=StringReplace(astring,REPMAN_CONFIG_LABEL,adminstring
  ,[rfReplaceAll]);
 astring:=StringReplace(astring,'./config',
  './config?'+Request.Query,[rfReplaceAll]);

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

function TRpWebPageLoader.LoadConfigPage(Request: TWebRequest):string;
var
 astring:String;
begin
 if Length(FPagesDirectory)<1 then
 begin
  astring:=configpage;
 end
 else
 begin
  aresult.LoadFromFile(FPagesDirectory+'rpconfig.html');
  astring:=aresult.Text;
 end;
 // Substitute translations
 astring:=StringReplace(astring,REPMAN_WEBSERVER,
  TranslateStr(837,'Report Manager Web Server'),[rfReplaceAll]);

 Result:=astring;
end;


function TRpWebPageLoader.GetWebPage(Request: TWebRequest;apage:TRpWebPage):String;
begin
 try
  CheckInitReaded;
  if Not (apage in [rpwVersion,rpwLogin]) then
   CheckLogin(Request);
  case apage of
   rpwVersion:
    Result:='<html><body>'+TranslateStr(837,'Report Manager Web Server')+#10+
     '<p></p>'+TranslateStr(91,'Version')+' '+RM_VERSION+#10+'<p></p>'+
     TranslateStr(743,'Configuration File')+': '+Ffilenameconfig+'</body></html>';
   rpwLogin:
    Result:=LoadLoginPage(Request);
   rpwIndex:
    Result:=LoadIndexPage(Request);
   rpwConfig:
    Result:=LoadConfigPage(Request);
   rpwShowAlias:
    Result:=LoadAliasPage(Request);
  end;
 except
  On E:Exception do
  begin
   Result:=GenerateError(E.Message);
  end;
 end;
end;

function TRpWebPageLoader.GenerateError(errmessage:String):string;
begin
 Result:='Error: '+errmessage;
end;

constructor TRpWebPageLoader.Create;
begin
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
 aresult.Add('ConfigIndexLabel');
 aresult.Add('<p>AvailableAliasesLabel</p>');
 aresult.Add('</body>');
 aresult.Add('</html>');
 indexpage:=aresult.Text;

 aresult.clear;
 aresult.Add('<html>');
 aresult.Add('<head>');
 aresult.Add('<title>RepManebServer</title>');
 aresult.Add('<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">');
 aresult.Add('</head>');
 aresult.Add('<body bgcolor="#FFFFFF">');
 aresult.Add('<h3 align="center"> RepManConfigLabel</h3>');
 aresult.Add('<p></p>');
 aresult.Add('</body>');
 aresult.Add('</html>');
 configpage:=aresult.Text;

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


 InitConfig;
end;

destructor TRpWebPageLoader.Destroy;
begin
 lusers.free;
 laliases.Free;
 aresult.free;

 inherited Destroy;
end;

procedure TRpWebPageLoader.InitConfig;
var
 inif:TMemInifile;
 i:integer;
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
 except
  on E:Exception do
  begin
   InitErrorMessage:=E.Message;
  end;
 end;
end;


function TRpWebPageLoader.LoadAliasPage(Request: TWebRequest):string;
var
 astring:String;
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
    reportlist:=reportlist+#10+'<p><a href="./showparams?reportname='+
     alist.Strings[i]+'&'+Request.Query+'">'+alist.Strings[i]+'</a></p>';
   end;
  finally
   alist.free;
  end;
 end;
 astring:=StringReplace(astring,REPMAN_REPORTSLOC_LABEL,
  reportlist,[rfReplaceAll]);
 Result:=astring;
end;


end.
