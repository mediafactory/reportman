{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpmdclitree                                     }
{                                                       }
{       Report Manager Client Frame,                    }
{       for Report Manager Server to manage login       }
{       and execution, integrated in metaview.exe       }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rpmdclitree;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, rptranslator, QStdCtrls, QExtCtrls,rpmdrepclient,
  rpmdconsts, QActnList, QImgList, QComCtrls;

type
  TFRpCliTree = class(TFrame)
    PTop: TPanel;
    GUser: TGroupBox;
    LUserName: TLabel;
    EUserName: TEdit;
    EPassword: TEdit;
    LPassword: TLabel;
    BConnect: TButton;
    PClient: TPanel;
    LHost: TLabel;
    ComboHost: TComboBox;
    EPath: TEdit;
    LPath: TLabel;
    PMessages: TPanel;
    LMessages: TMemo;
    Splitter1: TSplitter;
    PTree: TPanel;
    BToolBar: TToolBar;
    ImageList1: TImageList;
    ActionList1: TActionList;
    AConnect: TAction;
    ADisconnect: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    AParameters: TAction;
    AExecute: TAction;
    ToolButton4: TToolButton;
    LTree: TTreeView;
    procedure BExecuteClick(Sender: TObject);
    procedure BConnectClick(Sender: TObject);
    procedure BDisconnectClick(Sender: TObject);
  private
    { Private declarations }
    FStream:TMemoryStream;
    FOnExecuteServer:TNotifyEvent;
    amod:TModClient;
    loadedreport:Boolean;
    procedure InsertMessage(aMessage:WideString);
    procedure OnError(Sender:TObject;aMessage:WideString);
    procedure OnAuthorization(Sender:TObject);
    procedure OnExecute(Sender:TObject);
  public
    { Public declarations }
    initialwidth:integer;
    asynchrohous:boolean;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property OnExecuteServer:TNotifyEvent read FOnExecuteServer write FOnExecuteServer;
    property Stream:TMemoryStream read FStream;
  end;

implementation

{$R *.xfm}

constructor TFRpCliTree.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FStream:=TMemoryStream.Create;
 LHost.Caption:=TranslateStr(747,LHost.Caption);
 LUserName.Caption:=TranslateStr(751,LUserName.Caption);
 LPassword.Caption:=TranslateStr(752,LPassword.Caption);
 LPath.Caption:=TranslateStr(760,LPath.Caption);
 GUser.Caption:=TranslateStr(750,Guser.Caption);
 AConnect.Caption:=TranslateStr(753,AConnect.Caption);
 ADisconnect.Caption:=TranslateStr(777,ADisconnect.Caption);
 ADisconnect.Hint:=TranslateStr(778,ADisconnect.Hint);
 AExecute.Caption:=TranslateStr(779,AExecute.Caption);
 AExecute.Hint:=TranslateStr(780,AExecute.Hint);
 AParameters.Caption:=TranslateStr(135,AParameters.Caption);
 AParameters.Hint:=TranslateStr(136,AParameters.Hint);

 InitialWidth:=Width;
end;



destructor TFRpCliTree.Destroy;
begin
 FStream.Free;
 if assigned(amod) then
  Disconnect(amod);
 inherited Destroy;
end;


procedure TFRpCliTree.InsertMessage(aMessage:WideString);
begin
 if (csDestroying in ComponentState) then
  exit;
 LMessages.Lines.Insert(0,FormatDateTime('dd/mm/yyyy hh:nn:ss - ',Now)+aMessage);
 if LMessages.Lines.Count>1000 then
  LMEssages.Lines.Delete(LMessages.Lines.Count-1);
end;


procedure TFRpCliTree.OnError(Sender:TObject;aMessage:WideString);
begin
 InsertMessage(aMessage);
end;

procedure TFRpCliTree.OnAuthorization(Sender:TObject);
begin
 InsertMessage(SRpAuthorized);
// LAliases.Lines.Assign((Sender As TModClient).Aliases);
end;

procedure TFRpCliTree.OnExecute(Sender:TObject);
begin
 stream.Clear;
 stream.SetSize(amod.Stream.Size);
 stream.Write(amod.Stream.Memory^,amod.Stream.Size);
 stream.Seek(0,soFromBeginning);
 if Assigned(OnExecuteServer) then
  OnExecuteServer(Self);
 loadedreport:=true;
end;

procedure TFRpCliTree.BExecuteClick(Sender: TObject);
begin
 if assigned(amod) then
 begin
  amod.asynchronous:=asynchrohous;
  amod.Execute;
 end;
end;

procedure TFRpCliTree.BConnectClick(Sender: TObject);
begin
 amod:=Connect(ComboHost.Text,EUserName.Text,EPassword.Text);
 amod.OnAuthorization:=OnAuthorization;
 amod.OnExecute:=OnExecute;
 amod.asynchronous:=asynchrohous;
 amod.OnError:=OnError;
 amod.OnLog:=OnError;
 PTop.Visible:=False;
 LTree.Visible:=True;
 BToolBar.Visible:=True;
end;

procedure TFRpCliTree.BDisconnectClick(Sender: TObject);
begin
 Disconnect(amod);
 amod:=nil;
 LTree.Visible:=false;
 BToolBar.Visible:=False;
 PTop.Visible:=true;
end;

end.
