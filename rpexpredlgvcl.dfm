object FRpExpredialogVCL: TFRpExpredialogVCL
  Left = 245
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 365
  ClientWidth = 489
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LExpression: TLabel
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Expression'
  end
  object LabelCategory: TLabel
    Left = 4
    Top = 108
    Width = 42
    Height = 13
    Caption = 'Category'
  end
  object LOperation: TLabel
    Left = 124
    Top = 108
    Width = 46
    Height = 13
    Caption = 'Operation'
  end
  object LModel: TLabel
    Left = 8
    Top = 248
    Width = 35
    Height = 13
    Caption = 'LModel'
  end
  object LHelp: TLabel
    Left = 8
    Top = 224
    Width = 22
    Height = 13
    Caption = 'Help'
  end
  object LParams: TLabel
    Left = 8
    Top = 272
    Width = 461
    Height = 49
    AutoSize = False
    Caption = 'Params'
  end
  object MemoExpre: TMemo
    Left = 4
    Top = 24
    Width = 457
    Height = 53
    Lines.Strings = (
      'MemoExpre')
    TabOrder = 0
    WordWrap = False
  end
  object LItems: TListBox
    Left = 120
    Top = 124
    Width = 337
    Height = 89
    ItemHeight = 13
    TabOrder = 1
    OnClick = LItemsClick
    OnDblClick = LItemsDblClick
  end
  object BCheckSyn: TButton
    Left = 148
    Top = 80
    Width = 145
    Height = 25
    Hint = 'Syntax check the expresion'
    Caption = 'Syntax check'
    TabOrder = 2
    OnClick = BCheckSynClick
  end
  object BShowResult: TButton
    Left = 312
    Top = 80
    Width = 149
    Height = 25
    Hint = 'Evaluates the expresion and shows the result'
    Caption = 'Show Result'
    TabOrder = 3
    OnClick = BShowResultClick
  end
  object BCancel: TButton
    Left = 96
    Top = 324
    Width = 85
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object BOK: TButton
    Left = 4
    Top = 324
    Width = 85
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 5
    OnClick = BOKClick
  end
  object BAdd: TButton
    Left = 4
    Top = 80
    Width = 129
    Height = 25
    Caption = 'Add selection'
    TabOrder = 6
    OnClick = BitBtn1Click
  end
  object LCategory: TListBox
    Left = 4
    Top = 124
    Width = 109
    Height = 89
    ItemHeight = 13
    Items.Strings = (
      'Database fields'
      'Functions'
      'Variables'
      'Constants'
      'Operators')
    TabOrder = 7
    OnClick = LCategoryClick
  end
end
