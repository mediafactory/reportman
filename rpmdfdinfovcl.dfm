object FRpDInfoVCL: TFRpDInfoVCL
  Left = 183
  Top = 112
  Width = 568
  Height = 446
  Caption = 'Database connections and datasets'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PBottom: TPanel
    Left = 0
    Top = 380
    Width = 560
    Height = 39
    Align = alBottom
    TabOrder = 0
    object BOk: TButton
      Left = 12
      Top = 8
      Width = 89
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = BOkClick
    end
    object BCancel: TButton
      Left = 116
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object PControl: TPageControl
    Left = 0
    Top = 0
    Width = 560
    Height = 380
    ActivePage = TabConnections
    Align = alClient
    TabOrder = 1
    OnChange = PControlChange
    object TabConnections: TTabSheet
      Caption = 'Connections'
    end
    object TabDatasets: TTabSheet
      Caption = 'Datasets'
      ImageIndex = 1
    end
  end
end
