{*******************************************************}
{                                                       }
{       Rpexpredlg                                      }
{       A Helper for building expresions with help      }
{       Report Manager                                  }
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

unit rpexpredlg;

interface

uses
  SysUtils, Classes, QGraphics,QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls,
  rpalias,rpeval, QButtons,rptypeval,
  rpmdconsts;

const
 FMaxlisthelp=5;
type
  TRpRecHelp=class(TObject)
  public
    rfunction:string;
    help:string;
    model:string;
    params:string;
  end;

  TRpExpreDialog = class(TComponent)
  private
    { Private declarations }
    FExpresion:TStrings;
    FRpalias:TRpalias;
    Fevaluator:TRpEvaluator;
    procedure setexpresion(valor:TStrings);
    procedure SetRpalias(Rpalias1:TRpalias);
  protected
    { Protected declarations }
    procedure Notification(AComponent:TComponent;Operation:TOperation);override;
  public
    { Public declarations }
    constructor create(AOwner:TComponent);override;
    destructor destroy;override;
    function Execute:Boolean;
  published
    { Published declarations }
    property Expresion:TStrings read FExpresion write setexpresion;
    property Rpalias:TRpalias read FRpalias
     write SetRpalias;
    property evaluator:TRpEvaluator read Fevaluator write Fevaluator;
  end;

  TFrpExpredialog = class(TForm)
    Label1: TLabel;
    MemoExpre: TMemo;
    LCategory: TListBox;
    Label2: TLabel;
    LItems: TListBox;
    Label3: TLabel;
    LModel: TLabel;
    LHelp: TLabel;
    BCheckSyn: TButton;
    BShowResult: TButton;
    LParams: TLabel;
    BCancel: TButton;
    BOK: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LCategoryClick(Sender: TObject);
    procedure LItemsClick(Sender: TObject);
    procedure BCheckSynClick(Sender: TObject);
    procedure BShowResultClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure LItemsDblClick(Sender: TObject);
  private
    { Private declarations }
    Fevaluator:TRpCustomEvaluator;
    llistes:array[0..FMaxlisthelp-1] of TStringlist;
    procedure Setevaluator(aval:TRpCustomEvaluator);
  public
    { Public declarations }
    property evaluator:TRpCustomEvaluator read fevaluator write setevaluator;
  end;

function ChangeExpression(formul:string;aval:TRpCustomEvaluator):string;



implementation

{$R *.xfm}

constructor TRpExpreDialog.create(AOwner:TComponent);
begin
 inherited create(AOwner);
 Fevaluator:=TRpEvaluator.Create(Self);
 FExpresion:=TStringList.Create;
end;

destructor TRpExpreDialog.destroy;
begin
 FExpresion.free;

 inherited destroy;
end;

procedure TRpExpreDialog.SetRpalias(Rpalias1:TRpalias);
begin
 FRpalias:=Rpalias1;
end;

procedure TRpExpreDialog.setexpresion(valor:TStrings);
begin
 FExpresion.assign(valor);
end;

procedure TRpExpreDialog.Notification(AComponent:TComponent;Operation:TOperation);
begin
 inherited Notification(AComponent,Operation);
 if Operation=opRemove then
 begin
  if AComponent=FRpalias then
   Rpalias:=nil
  else
   if AComponent=Fevaluator then
    Fevaluator:=nil;
 end;
end;


procedure TFrpExpredialog.FormCreate(Sender: TObject);
var
 i:integer;
begin
 inherited;

 for i:=0 to FMaxlisthelp-1 do
 begin
  llistes[i]:=TStringList.create;
 end;
 SetInitialBounds;
end;

procedure TFrpExpredialog.Setevaluator(aval:TRpCustomEvaluator);
var
 lista1:Tstringlist;
 i:integer;
 iden:TIdentifier;
 rec:TRpRecHelp;
begin
 Fevaluator:=Aval;
 for i:=0 to FMaxlisthelp-1 do
 begin
  llistes[i].clear;
 end;
 lista1:=llistes[0];
 if aval.Rpalias<>nil then
 begin
  aval.Rpalias.fillwithfields(lista1);
  for i:=0 to lista1.count -1 do
  begin
   rec:=TRpRecHelp.Create;
   rec.rfunction:=lista1.strings[i];
   lista1.Objects[i]:=rec;
  end;
 end;
 for i:=0 to aval.identifiers.Count -1 do
 begin
  iden:=aval.identifiers.objects[i] as TIdentifier;
  case iden.RType of
   RTypeidenfunction:
    begin
    lista1:=llistes[1];
    end;
   RTypeidenvariable:
    begin
     lista1:=llistes[2];
    end;
   RTypeidenconstant:
    begin
     lista1:=llistes[3];
    end;
  end;
  rec:=TRpRecHelp.Create;
  rec.rfunction:=iden.Idenname;
  rec.help:=iden.Help;
  rec.model:=iden.model;
  rec.params:=iden.aparams;
  lista1.addobject(rec.rfunction,rec);
 end;
 lista1:=llistes[4];
 // +
 rec:=TRpRecHelp.create;
 rec.rfunction:='+';
 rec.help:=SRpOperatorSum;
 lista1.addobject(rec.rfunction,rec);
 // -
 rec:=TRpRecHelp.create;
 rec.rfunction:='-';
 rec.help:=SRpOperatorDif;
 lista1.addobject(rec.rfunction,rec);
 // *
 rec:=TRpRecHelp.create;
 rec.rfunction:='*';
 rec.help:=SRpOperatorMul;
 lista1.addobject(rec.rfunction,rec);
 // /
 rec:=TRpRecHelp.create;
 rec.rfunction:='/';
 rec.help:=SRpOperatorDiv;
 lista1.addobject(rec.rfunction,rec);
 // =
 rec:=TRpRecHelp.create;
 rec.rfunction:='=';
 rec.help:=SRpOperatorComp;
 lista1.addobject(rec.rfunction,rec);
 // ==
 rec:=TRpRecHelp.create;
 rec.rfunction:='==';
 rec.help:=SRpOperatorComp;
 lista1.addobject(rec.rfunction,rec);
 // >=
 rec:=TRpRecHelp.create;
 rec.rfunction:='>=';
 rec.help:=SRpOperatorComp;
 lista1.addobject(rec.rfunction,rec);
 // <=
 rec:=TRpRecHelp.create;
 rec.rfunction:='<=';
 rec.help:=SRpOperatorComp;
 lista1.addobject(rec.rfunction,rec);
 // >
 rec:=TRpRecHelp.create;
 rec.rfunction:='>';
 rec.help:=SRpOperatorComp;
 lista1.addobject(rec.rfunction,rec);
 // <
 rec:=TRpRecHelp.create;
 rec.rfunction:='<';
 rec.help:=SRpOperatorComp;
 lista1.addobject(rec.rfunction,rec);
 // <>
 rec:=TRpRecHelp.create;
 rec.rfunction:='<>';
 rec.help:=SRpOperatorComp;
 lista1.addobject(rec.rfunction,rec);
 // AND
 rec:=TRpRecHelp.create;
 rec.rfunction:='AND';
 rec.help:=SRpOperatorLog;
 lista1.addobject(rec.rfunction,rec);
 // OR
 rec:=TRpRecHelp.create;
 rec.rfunction:='OR';
 rec.help:=SRpOperatorLog;
 lista1.addobject(rec.rfunction,rec);
 // NOT
 rec:=TRpRecHelp.create;
 rec.rfunction:='NOT';
 rec.help:=SRpOperatorLog;
 lista1.addobject(rec.rfunction,rec);
 // |
 rec:=TRpRecHelp.create;
 rec.rfunction:='|';
 rec.help:=SRpOperatorSep;
 rec.params:=SRpOperatorSepP;
 lista1.addobject(rec.rfunction,rec);
 // IIF
 rec:=TRpRecHelp.create;
 rec.rfunction:='IIF';
 rec.help:=SRpOperatorDec;
 rec.Model:=SRpOperatorDecM;
 rec.params:=SRpOperatorDecM;
 lista1.addobject(rec.rfunction,rec);

 LCategory.Itemindex:=0;
 LCategoryClick(self);
end;

procedure TFrpExpredialog.FormDestroy(Sender: TObject);
var
 i,j:integer;
begin
  inherited;
 for i:=0 to FMaxlisthelp-1 do
 begin
  for j:=0 to llistes[i].count-1 do
  begin
   llistes[i].objects[j].freE;
  end;
  llistes[i].free;
 end;
end;

procedure TFrpExpredialog.LCategoryClick(Sender: TObject);
begin
  inherited;
 Litems.items.Assign(llistes[lcategory.itemindex]);
 Lhelp.Caption:='';
 Lparams.caption:='';
 Lmodel.caption:='';
end;

procedure TFrpExpredialog.LItemsClick(Sender: TObject);
begin
  inherited;
 if litems.itemindex>-1 then
 begin
  Lhelp.caption:=(llistes[lcategory.itemindex].objects[litems.itemindex]
      As TRpRecHelp).help;
  Lparams.caption:=(llistes[lcategory.itemindex].objects[litems.itemindex]
      As TRpRecHelp).params;
  Lmodel.caption:=(llistes[lcategory.itemindex].objects[litems.itemindex]
      As TRpRecHelp).model;
 end
 else
 begin
  Lhelp.Caption:='';
  Lparams.caption:='';
  Lmodel.caption:='';
 end;
end;

procedure TFrpExpredialog.BCheckSynClick(Sender: TObject);
begin
  inherited;
 evaluator.Expression:=Memoexpre.text;
 try
  evaluator.CheckSyntax;
 except
  MemoExpre.SetFocus;
  MemoExpre.SelStart:=evaluator.PosError;
  MemoExpre.SelLength:=0;
  Raise;
 end;
end;

procedure TFrpExpredialog.BShowResultClick(Sender: TObject);
begin
 evaluator.Expression:=Memoexpre.text;
 try
  evaluator.evaluate;
 except
  MemoExpre.SetFocus;
  MemoExpre.SelStart:=evaluator.PosError;
  MemoExpre.SelLength:=0;
  Raise;
 end;
 showmessage(TRpValueToString(evaluator.EvalResult));
end;

procedure TFrpExpredialog.BitBtn1Click(Sender: TObject);
begin
  inherited;
 if litems.itemindex>-1 then
  memoexpre.text:=memoexpre.text+litems.Items.strings[litems.itemindex];
end;

procedure TFrpExpredialog.LItemsDblClick(Sender: TObject);
begin
  inherited;
 if litems.itemindex>-1 then
  memoexpre.text:=memoexpre.text+litems.Items.strings[litems.itemindex];
end;


function ChangeExpression(formul:string;aval:TRpCustomEvaluator):string;
var
 dia:TFRpExpredialog;
begin
  dia:=TFRpExpredialog.create(Application);
  try
   dia.evaluator:=aval;
   dia.MemoExpre.text:=formul;
   result:=formul;
   if dia.showmodal=mrok then
    result:=dia.MemoExpre.text;
  finally
   dia.freE;
  end;
end;

function TRpExpreDialog.Execute:Boolean;
var
 dia:TFRpExpredialog;
begin
  Fevaluator.Rpalias:=FRpalias;
  dia:=TFRpExpredialog.create(Application);
  try
   dia.evaluator:=Fevaluator;
   dia.MemoExpre.text:=Expresion.text;
   result:=dia.showmodal=mrok;
   if result then
    Expresion.text:=dia.MemoExpre.text;
  finally
   dia.freE;
  end;
end;

end.
