object FRpRunTimeParams: TFRpRunTimeParams
  Left = 17
  Top = 30
  Width = 579
  Height = 271
  VertScrollBar.Range = 41
  ActiveControl = OKBtn
  AutoScroll = False
  Caption = 'Dialog'
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PModalButtons: TPanel
    Left = 0
    Top = 203
    Width = 571
    Height = 41
    Align = alBottom
    TabOrder = 0
    object OKBtn: TButton
      Left = 59
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = OKBtnClick
    end
    object CancelBtn: TButton
      Left = 203
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object MainScrollBox: TScrollBox
    Left = 0
    Top = 0
    Width = 571
    Height = 203
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 1
  end
end
