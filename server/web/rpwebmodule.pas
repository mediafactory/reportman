unit rpwebmodule;

interface

uses
  SysUtils, Classes, HTTPApp,rptypes,rpmdconsts,inifiles,
  rpmdshfolder,rpwebpages;

type
  Trepwebmod = class(TWebModule)
    procedure repwebmodaversionAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
    procedure repwebmodaindexAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure repwebmodaloginAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure repwebmodaconfigAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
    pageloader:TRpWebPageLoader;
  public
    { Public declarations }
  end;

var
  repwebmod: Trepwebmod;

implementation

{$R *.dfm}

procedure Trepwebmod.repwebmodaversionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
 Response.Content:=pageloader.GetWebPage(Request,rpwVersion);
end;


procedure Trepwebmod.WebModuleCreate(Sender: TObject);
begin
 pageloader:=TRpWebPageLoader.Create;
end;




procedure Trepwebmod.WebModuleDestroy(Sender: TObject);
begin
 pageloader.free;
end;

procedure Trepwebmod.repwebmodaindexAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
 Response.Content:=pageloader.GetWebPage(Request,rpwIndex);
end;

procedure Trepwebmod.repwebmodaloginAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
 Response.Content:=pageloader.GetWebPage(Request,rpwLogin);
end;

procedure Trepwebmod.repwebmodaconfigAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
 Response.Content:=pageloader.GetWebPage(Request,rpwConfig);
end;

end.
