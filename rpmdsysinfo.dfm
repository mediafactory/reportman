object FRpSysInfo: TFRpSysInfo
  Left = 245
  Top = 108
  BorderStyle = bsDialog
  Caption = 'System information'
  ClientHeight = 340
  ClientWidth = 499
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BOK: TButton
    Left = 123
    Top = 252
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 477
    Height = 237
    Caption = 'Selected Printer'
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 44
      Width = 63
      Height = 13
      Caption = 'Printer Status'
    end
    object Label2: TLabel
      Left = 8
      Top = 20
      Width = 61
      Height = 13
      Caption = 'Printer Name'
    end
    object Label3: TLabel
      Left = 8
      Top = 68
      Width = 34
      Height = 13
      Caption = 'Device'
    end
    object Label4: TLabel
      Left = 8
      Top = 92
      Width = 28
      Height = 13
      Caption = 'Driver'
    end
    object Label5: TLabel
      Left = 8
      Top = 116
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label6: TLabel
      Left = 8
      Top = 140
      Width = 136
      Height = 13
      Caption = 'Printer max. hardware copies'
    end
    object LMaxCopies: TLabel
      Left = 184
      Top = 140
      Width = 137
      Height = 17
      AutoSize = False
    end
    object Label7: TLabel
      Left = 8
      Top = 160
      Width = 115
      Height = 13
      Caption = 'Printer supports collation'
    end
    object LCollation: TLabel
      Left = 184
      Top = 160
      Width = 137
      Height = 17
      AutoSize = False
    end
    object Label8: TLabel
      Left = 8
      Top = 200
      Width = 69
      Height = 13
      Caption = 'Color selection'
    end
    object Label9: TLabel
      Left = 8
      Top = 180
      Width = 150
      Height = 13
      Caption = 'Printer resolution dpi (HorzxVert)'
    end
    object LColor: TLabel
      Left = 184
      Top = 200
      Width = 137
      Height = 17
      AutoSize = False
    end
    object LResolution: TLabel
      Left = 184
      Top = 180
      Width = 137
      Height = 17
      AutoSize = False
    end
    object EStatus: TEdit
      Left = 108
      Top = 40
      Width = 365
      Height = 21
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 0
    end
    object EPrinterName: TEdit
      Left = 108
      Top = 16
      Width = 365
      Height = 21
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 1
    end
    object EDevice: TEdit
      Left = 108
      Top = 64
      Width = 365
      Height = 21
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 2
    end
    object EDriver: TEdit
      Left = 108
      Top = 88
      Width = 365
      Height = 21
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 3
    end
    object EPort: TEdit
      Left = 108
      Top = 112
      Width = 365
      Height = 21
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 4
    end
  end
end
