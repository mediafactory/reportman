object FNewAliasVCL: TFNewAliasVCL
  Left = 304
  Top = 174
  Width = 572
  Height = 174
  HorzScrollBar.Range = 525
  VertScrollBar.Range = 109
  ActiveControl = BOk
  AutoScroll = False
  Caption = 'New server directory alias'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LAlias: TLabel
    Left = 12
    Top = 16
    Width = 51
    Height = 13
    Caption = 'Alias name'
  end
  object LPath: TLabel
    Left = 12
    Top = 48
    Width = 67
    Height = 13
    Caption = 'Directory Path'
  end
  object LAliasbase: TLabel
    Left = 120
    Top = 72
    Width = 52
    Height = 13
    Caption = 'LAliasBase'
  end
  object BOk: TButton
    Left = 11
    Top = 96
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = BOkClick
  end
  object BCancel: TButton
    Left = 135
    Top = 96
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object EAliasName: TEdit
    Left = 120
    Top = 8
    Width = 201
    Height = 21
    TabOrder = 0
    Text = 'EAliasName'
  end
  object EPath: TEdit
    Left = 120
    Top = 44
    Width = 405
    Height = 21
    TabOrder = 1
    Text = 'EAliasName'
  end
end
