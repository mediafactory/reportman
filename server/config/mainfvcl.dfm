object FMainVCL: TFMainVCL
  Left = 134
  Top = 83
  Width = 500
  Height = 456
  HorzScrollBar.Range = 389
  VertScrollBar.Range = 161
  ActiveControl = ComboHost
  AutoScroll = False
  Caption = 'Report Manager Server configuration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LHost: TLabel
    Left = 4
    Top = 12
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object GUser: TGroupBox
    Left = 8
    Top = 44
    Width = 381
    Height = 117
    Caption = 'User information'
    TabOrder = 1
    object LUserName: TLabel
      Left = 8
      Top = 24
      Width = 51
      Height = 13
      Caption = 'User name'
    end
    object LPassword: TLabel
      Left = 8
      Top = 52
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object EUserName: TEdit
      Left = 112
      Top = 20
      Width = 205
      Height = 21
      TabOrder = 0
      Text = 'Admin'
    end
    object EPassword: TEdit
      Left = 112
      Top = 48
      Width = 205
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
    end
    object BConnect: TButton
      Left = 8
      Top = 84
      Width = 113
      Height = 25
      Caption = 'Connect'
      TabOrder = 1
      OnClick = BConnectClick
    end
  end
  object GServerinfo: TGroupBox
    Left = 8
    Top = 44
    Width = 469
    Height = 301
    Caption = 'Server information'
    TabOrder = 2
    Visible = False
    object GReportDirectories: TGroupBox
      Left = 8
      Top = 124
      Width = 461
      Height = 133
      Caption = 'Report server directories'
      TabOrder = 1
      object DBGrid1: TDBGrid
        Left = 4
        Top = 16
        Width = 445
        Height = 81
        DataSource = SDirectories
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object BAddAlias: TButton
        Left = 4
        Top = 97
        Width = 133
        Height = 28
        Caption = 'Add'
        TabOrder = 1
        OnClick = BAddAliasClick
      end
      object BDeleteAlias: TButton
        Left = 140
        Top = 96
        Width = 133
        Height = 30
        Caption = 'Delete'
        TabOrder = 2
        OnClick = BDeleteAliasClick
      end
      object BPreviewTree: TButton
        Left = 276
        Top = 96
        Width = 173
        Height = 30
        Caption = 'Preview Report Tree'
        TabOrder = 3
        OnClick = BPreviewTreeClick
      end
    end
    object GUsers: TGroupBox
      Left = 8
      Top = 16
      Width = 453
      Height = 105
      Caption = 'Users'
      TabOrder = 0
      object LUsers: TListBox
        Left = 8
        Top = 16
        Width = 305
        Height = 85
        ItemHeight = 13
        TabOrder = 0
      end
      object BDeleteUser: TButton
        Left = 316
        Top = 72
        Width = 133
        Height = 29
        Caption = 'Delete'
        TabOrder = 3
        OnClick = BDeleteUserClick
      end
      object BAddUser: TButton
        Left = 316
        Top = 17
        Width = 133
        Height = 28
        Caption = 'Add'
        TabOrder = 1
        OnClick = BAddUserClick
      end
      object BChangePassword: TButton
        Left = 316
        Top = 45
        Width = 133
        Height = 28
        Caption = 'Change Password'
        TabOrder = 2
        OnClick = BChangePasswordClick
      end
    end
    object BCloseConnection: TButton
      Left = 12
      Top = 261
      Width = 173
      Height = 33
      Caption = 'Close connection'
      TabOrder = 2
      OnClick = BCloseConnectionClick
    end
  end
  object ComboHost: TComboBox
    Left = 92
    Top = 8
    Width = 213
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      'localhost')
  end
  object LMessages: TListBox
    Left = 8
    Top = 348
    Width = 469
    Height = 53
    ItemHeight = 13
    TabOrder = 3
  end
  object Trans: TRpTranslator
    Active = False
    Filename = 'reportmanres'
    Left = 408
    Top = 8
  end
  object DDirectories: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 276
    Top = 236
    object DDirectoriesAlias: TStringField
      DisplayLabel = 'Server alias'
      DisplayWidth = 15
      FieldName = 'Alias'
      Size = 30
    end
    object DDirectoriesServerPath: TStringField
      DisplayLabel = 'Server Path'
      DisplayWidth = 50
      FieldName = 'ServerPath'
      Size = 250
    end
  end
  object SDirectories: TDataSource
    DataSet = DDirectories
    Left = 232
    Top = 240
  end
end
