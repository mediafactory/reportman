unit unewalias;

interface

uses SysUtils, Classes, QGraphics, QForms,
  QButtons, QExtCtrls, QControls, QStdCtrls,rpmdconsts;

type
  TFNewAlias = class(TForm)
    BOk: TButton;
    BCancel: TButton;
    LAlias: TLabel;
    EAliasName: TEdit;
    EPath: TEdit;
    LPath: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BOkClick(Sender: TObject);
  private
    { Private declarations }
    dook:boolean;
  public
    { Public declarations }
  end;


function AskForNewAlias(var aliasname,path:string):boolean;

implementation

{$R *.xfm}


function AskForNewAlias(var aliasname,path:string):boolean;
var
 dia:TFNewAlias;
begin
 Result:=false;
 dia:=TFNewAlias.Create(Application);
 try
  dia.ActiveControl:=dia.EAliasName;
  dia.EAliasName.Text:=aliasname;
  dia.EPath.Text:=path;
  dia.showmodal;
  if dia.dook then
  begin
   aliasname:=Trim(Uppercase(dia.EAliasName.Text));
   path:=Copy(Trim(dia.EPath.Text),1,250);
   Result:=True;
  end;
 finally
  dia.free;
 end;
end;

procedure TFNewAlias.FormCreate(Sender: TObject);
begin
 SetInitialBounds;
 Caption:=TranslateStr(806,Caption);
 LAlias.Caption:=TranslateStr(807,LAlias.Caption);
 LPath.Caption:=TranslateStr(808,LPath.Caption);
 BOK.Caption:=TranslateStr(93,BOK.Caption);
 BCancel.Caption:=TranslateStr(94,BCancel.Caption);

end;

procedure TFNewAlias.BOkClick(Sender: TObject);
begin
 if Length(Trim(EPath.Text))<1 then
 begin
  EPath.SetFocus;
  Raise Exception.Create(SRpAPathMustBeAssigned);
 end;
 if Length(Trim(EAliasname.Text))<1 then
 begin
  EAliasName.SetFocus;
  Raise Exception.Create(SRpAAliasnameBeAssigned);
 end;
 dook:=true;
 Close;
end;

end.