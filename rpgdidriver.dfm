object FRpVCLProgress: TFRpVCLProgress
  Left = 156
  Top = 200
  Width = 491
  Height = 188
  HorzScrollBar.Range = 453
  VertScrollBar.Range = 109
  ActiveControl = CancelBtn
  AutoScroll = False
  Caption = 'Print progress'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 40
    Width = 52
    Height = 13
    Caption = 'Processing'
  end
  object LRecordCount: TLabel
    Left = 76
    Top = 40
    Width = 305
    Height = 21
    AutoSize = False
  end
  object Label2: TLabel
    Left = 8
    Top = 4
    Width = 23
    Height = 13
    Caption = 'Tittle'
    Visible = False
  end
  object LTittle: TLabel
    Left = 76
    Top = 4
    Width = 377
    Height = 25
    AutoSize = False
  end
  object CancelBtn: TButton
    Left = 216
    Top = 120
    Width = 97
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    OnClick = CancelBtnClick
  end
  object OKBtn: TButton
    Left = 116
    Top = 123
    Width = 77
    Height = 22
    Caption = 'OK'
    Default = True
    TabOrder = 1
    Visible = False
    OnClick = OKBtnClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 4
    Width = 257
    Height = 113
    Caption = 'Print Range'
    TabOrder = 2
    Visible = False
    object Label5: TLabel
      Left = 120
      Top = 85
      Width = 13
      Height = 13
      Caption = 'To'
    end
    object Label4: TLabel
      Left = 14
      Top = 84
      Width = 23
      Height = 13
      Caption = 'From'
    end
    object EFrom: TEdit
      Left = 56
      Top = 80
      Width = 53
      Height = 21
      TabOrder = 0
    end
    object ETo: TEdit
      Left = 152
      Top = 81
      Width = 73
      Height = 21
      TabOrder = 1
    end
    object RadioAll: TRadioButton
      Left = 12
      Top = 20
      Width = 157
      Height = 25
      Caption = 'All pages'
      TabOrder = 2
    end
    object RadioRange: TRadioButton
      Left = 12
      Top = 48
      Width = 105
      Height = 25
      Caption = 'Range'
      TabOrder = 3
    end
  end
end
