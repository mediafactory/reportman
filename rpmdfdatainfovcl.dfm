object FRpDatainfoconfigVCL: TFRpDatainfoconfigVCL
  Left = 131
  Top = 64
  ActiveControl = LConnections
  AutoScroll = False
  Caption = 'Data access configuration'
  ClientHeight = 464
  ClientWidth = 557
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GConnections: TGroupBox
    Left = 0
    Top = 0
    Width = 557
    Height = 129
    Align = alTop
    Caption = 'Database connections'
    TabOrder = 0
    DesignSize = (
      557
      129)
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 43
      Height = 13
      Caption = 'Available'
    end
    object LConnectionString: TLabel
      Left = 8
      Top = 16
      Width = 84
      Height = 13
      Caption = 'Connection String'
      Visible = False
    end
    object LConnections: TListBox
      Left = 172
      Top = 12
      Width = 117
      Height = 69
      ItemHeight = 13
      TabOrder = 0
      OnClick = LConnectionsClick
    end
    object BAddCon: TButton
      Left = 128
      Top = 16
      Width = 37
      Height = 25
      Caption = '>'
      TabOrder = 1
      OnClick = BAddConClick
    end
    object BDeletecon: TButton
      Left = 128
      Top = 52
      Width = 37
      Height = 25
      Caption = '<'
      TabOrder = 2
      OnClick = BDeleteconClick
    end
    object ComboAvailable: TComboBox
      Left = 8
      Top = 32
      Width = 117
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
    end
    object BConfig: TButton
      Left = 8
      Top = 56
      Width = 117
      Height = 25
      Caption = 'Configure'
      TabOrder = 4
      OnClick = BConfigClick
    end
    object CheckLoginPrompt: TCheckBox
      Left = 292
      Top = 16
      Width = 157
      Height = 17
      Caption = 'Login prompt'
      TabOrder = 5
      OnClick = CheckLoginPromptClick
    end
    object CheckLoadParams: TCheckBox
      Left = 292
      Top = 36
      Width = 157
      Height = 17
      Caption = 'Load params'
      TabOrder = 6
      OnClick = CheckLoginPromptClick
    end
    object CheckLoadDriverParams: TCheckBox
      Left = 292
      Top = 60
      Width = 157
      Height = 17
      Caption = 'Load driver params'
      TabOrder = 7
      OnClick = CheckLoginPromptClick
    end
    object GDriver: TRadioGroup
      Left = 2
      Top = 86
      Width = 553
      Height = 41
      Align = alBottom
      Caption = 'Driver'
      Columns = 6
      Items.Strings = (
        'DbExpress'
        'MyBase'
        'IBX'
        'BDE'
        'ADO'
        'IBO')
      TabOrder = 8
      OnClick = GDriverClick
    end
    object EConnectionString: TEdit
      Left = 8
      Top = 32
      Width = 117
      Height = 21
      TabOrder = 11
      Visible = False
      OnChange = EConnectionStringChange
    end
    object BCancel: TButton
      Left = 472
      Top = 48
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 9
      OnClick = BCancelClick
    end
    object BOK: TButton
      Left = 472
      Top = 20
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      TabOrder = 10
      OnClick = BOKClick
    end
  end
  object GDatasets: TGroupBox
    Left = 0
    Top = 129
    Width = 557
    Height = 335
    Align = alClient
    TabOrder = 1
    object GDataProps: TGroupBox
      Left = 2
      Top = 90
      Width = 553
      Height = 243
      Align = alClient
      TabOrder = 0
      Visible = False
      object PControl: TPageControl
        Left = 2
        Top = 66
        Width = 549
        Height = 175
        ActivePage = TabSQL
        Align = alClient
        TabOrder = 0
        object TabSQL: TTabSheet
          Caption = 'SQL'
          object MSQL: TMemo
            Left = 0
            Top = 0
            Width = 541
            Height = 147
            Align = alClient
            Lines.Strings = (
              'MSQL')
            TabOrder = 0
            WordWrap = False
            OnChange = MSQLChange
          end
        end
        object TabBDEType: TTabSheet
          Caption = 'BDE Type'
          ImageIndex = 2
          object RBDEType: TRadioGroup
            Left = 0
            Top = 0
            Width = 541
            Height = 37
            Align = alTop
            Columns = 2
            Items.Strings = (
              'Query'
              'Table')
            TabOrder = 0
            OnClick = MSQLChange
          end
          object Panel2: TPanel
            Left = 0
            Top = 37
            Width = 541
            Height = 110
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            object PBDEFilter: TPanel
              Left = 0
              Top = 0
              Width = 541
              Height = 17
              Align = alTop
              Alignment = taLeftJustify
              BevelOuter = bvNone
              Caption = 'BDE Filter'
              TabOrder = 0
            end
            object MBDEFilter: TMemo
              Left = 0
              Top = 17
              Width = 541
              Height = 93
              Align = alClient
              Lines.Strings = (
                'MBDEFilter')
              ScrollBars = ssBoth
              TabOrder = 1
              WordWrap = False
              OnChange = MSQLChange
            end
          end
        end
        object TabBDETable: TTabSheet
          Caption = 'Table and order'
          ImageIndex = 1
          object LBDEIndexFields: TLabel
            Left = 5
            Top = 58
            Width = 56
            Height = 13
            Caption = 'Index Fields'
          end
          object LIndexName: TLabel
            Left = 4
            Top = 32
            Width = 57
            Height = 13
            Caption = 'Index Name'
          end
          object LTable: TLabel
            Left = 4
            Top = 10
            Width = 27
            Height = 13
            Caption = 'Table'
          end
          object LMasterFields: TLabel
            Left = 4
            Top = 82
            Width = 59
            Height = 13
            Caption = 'Master fields'
          end
          object LNote: TLabel
            Left = 4
            Top = 104
            Width = 199
            Height = 13
            Caption = 'Note: Write fields separated by ; character'
          end
          object EBDEIndexFields: TComboBox
            Left = 116
            Top = 54
            Width = 137
            Height = 21
            ItemHeight = 13
            TabOrder = 1
            OnChange = MSQLChange
            OnDropDown = EBDEIndexFieldsDropDown
            Items.Strings = (
              '')
          end
          object EBDEIndexName: TComboBox
            Left = 116
            Top = 28
            Width = 137
            Height = 21
            ItemHeight = 13
            TabOrder = 3
            OnChange = MSQLChange
            OnDropDown = EBDEIndexNameDropDown
            Items.Strings = (
              '')
          end
          object EBDETable: TComboBox
            Left = 116
            Top = 2
            Width = 137
            Height = 21
            ItemHeight = 13
            TabOrder = 0
            OnChange = MSQLChange
            OnDropDown = EBDETableDropDown
            Items.Strings = (
              '')
          end
          object EBDEMasterFields: TEdit
            Left = 116
            Top = 78
            Width = 137
            Height = 21
            TabOrder = 2
            Text = 'EBDEMasterFields'
            OnChange = MSQLChange
          end
        end
        object TabMySQL: TTabSheet
          Caption = 'MySQL'
          ImageIndex = 3
          object LIndexFields: TLabel
            Left = 0
            Top = 70
            Width = 53
            Height = 13
            Caption = 'Index fields'
          end
          object LMyBase: TLabel
            Left = 0
            Top = 6
            Width = 83
            Height = 13
            Caption = 'MyBase Filename'
          end
          object EMyBase: TEdit
            Left = 0
            Top = 22
            Width = 485
            Height = 21
            TabOrder = 0
            OnChange = MSQLChange
          end
          object EIndexFields: TEdit
            Left = 0
            Top = 82
            Width = 185
            Height = 21
            TabOrder = 1
            OnChange = MSQLChange
          end
          object BMyBase: TButton
            Left = 0
            Top = 42
            Width = 109
            Height = 25
            Caption = 'Search...'
            TabOrder = 2
            OnClick = BMyBaseClick
          end
        end
      end
      object Panel1: TPanel
        Left = 2
        Top = 15
        Width = 549
        Height = 51
        Align = alTop
        TabOrder = 1
        object LMasterDataset: TLabel
          Left = 204
          Top = 24
          Width = 70
          Height = 13
          Caption = 'Master dataset'
        end
        object LConnection: TLabel
          Left = 4
          Top = 24
          Width = 54
          Height = 13
          Caption = 'Connection'
        end
        object LDataprops: TLabel
          Left = 4
          Top = 4
          Width = 87
          Height = 13
          Caption = 'Dataset Properties'
        end
        object BShowData: TButton
          Left = 428
          Top = 16
          Width = 105
          Height = 29
          Caption = 'Show data'
          TabOrder = 0
          OnClick = BShowDataClick
        end
        object ComboDataSource: TComboBox
          Left = 308
          Top = 20
          Width = 117
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          OnChange = MSQLChange
        end
        object ComboConnection: TComboBox
          Left = 88
          Top = 20
          Width = 113
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          OnChange = MSQLChange
        end
      end
    end
    object Panel4: TPanel
      Left = 2
      Top = 15
      Width = 553
      Height = 75
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object LRDataset: TLabel
        Left = 4
        Top = 4
        Width = 77
        Height = 13
        Caption = 'Report Datasets'
      end
      object BParams: TButton
        Left = 316
        Top = 44
        Width = 109
        Height = 25
        Caption = 'Parameters'
        TabOrder = 0
        OnClick = BParamsClick
      end
      object BAdd: TButton
        Left = 204
        Top = 16
        Width = 97
        Height = 25
        Caption = 'Add'
        TabOrder = 1
        OnClick = BAddClick
      end
      object LDatasets: TListBox
        Left = 4
        Top = 20
        Width = 197
        Height = 49
        ItemHeight = 13
        TabOrder = 2
        OnClick = LDatasetsClick
      end
      object BDelete: TButton
        Left = 204
        Top = 44
        Width = 97
        Height = 25
        Caption = 'Delete'
        TabOrder = 3
        OnClick = BDeleteClick
      end
      object BRename: TButton
        Left = 316
        Top = 16
        Width = 109
        Height = 25
        Caption = 'Rename'
        TabOrder = 4
        OnClick = BRenameClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'cds'
    Filter = 'Mybase files;*.cds'
    FilterIndex = 0
    Title = 'Open'
    Left = 268
    Top = 300
  end
end
