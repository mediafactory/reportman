object FRpQtProgress: TFRpQtProgress
  Left = 156
  Top = 200
  Width = 514
  Height = 154
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
    Left = 207
    Top = 84
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 4
    TabOrder = 0
    OnClick = CancelBtnClick
  end
end
