object FRpDInfoVCL: TFRpDInfoVCL
  Left = 183
  Top = 112
  Caption = 'Database connections and datasets'
  ClientHeight = 562
  ClientWidth = 784
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PBottom: TPanel
    Left = 0
    Top = 523
    Width = 784
    Height = 39
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 383
    ExplicitWidth = 584
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
      TabOrder = 1
      OnClick = BCancelClick
    end
  end
  object PControl: TPageControl
    Left = 0
    Top = 0
    Width = 784
    Height = 523
    ActivePage = TabConnections
    Align = alClient
    TabOrder = 1
    OnChange = PControlChange
    ExplicitWidth = 584
    ExplicitHeight = 383
    object TabConnections: TTabSheet
      Caption = 'Connections'
      ExplicitWidth = 576
      ExplicitHeight = 355
    end
    object TabDatasets: TTabSheet
      Caption = 'Datasets'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
