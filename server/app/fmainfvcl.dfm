object FSerMainVCL: TFSerMainVCL
  Left = 36
  Top = 82
  Width = 544
  Height = 354
  VertScrollBar.Range = 117
  ActiveControl = LMEssages
  AutoScroll = False
  Caption = 'Report Manager Server application'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LMEssages: TMemo
    Left = 0
    Top = 149
    Width = 536
    Height = 178
    Align = alClient
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 536
    Height = 149
    Align = alTop
    TabOrder = 1
    DesignSize = (
      536
      149)
    object LLog: TLabel
      Left = 8
      Top = 72
      Width = 34
      Height = 13
      Caption = 'Log file'
    end
    object LHost: TLabel
      Left = 8
      Top = 48
      Width = 22
      Height = 13
      Caption = 'Host'
    end
    object LHostName: TLabel
      Left = 132
      Top = 48
      Width = 15
      Height = 13
      Caption = '     '
    end
    object LConfigFile: TLabel
      Left = 8
      Top = 96
      Width = 78
      Height = 13
      Caption = 'Configuration file'
    end
    object LPort: TLabel
      Left = 12
      Top = 124
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object LPortNumber: TLabel
      Left = 176
      Top = 120
      Width = 145
      Height = 21
      AutoSize = False
    end
    object BStartServer: TButton
      Left = 8
      Top = 8
      Width = 133
      Height = 29
      Caption = 'Start Server'
      TabOrder = 0
      OnClick = BStartServerClick
    end
    object BStopServer: TButton
      Left = 156
      Top = 8
      Width = 133
      Height = 29
      Caption = 'Stop Server'
      Enabled = False
      TabOrder = 1
      OnClick = BStopServerClick
    end
    object ELogFIle: TEdit
      Left = 176
      Top = 68
      Width = 349
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 2
    end
    object EConfigFile: TEdit
      Left = 176
      Top = 92
      Width = 349
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 3
    end
  end
end