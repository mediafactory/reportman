object FRpPrinterConfigVCL: TFRpPrinterConfigVCL
  Left = 245
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 475
  ClientWidth = 519
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LSelectPrinter: TLabel
    Left = 216
    Top = 4
    Width = 63
    Height = 13
    Caption = 'Select Printer'
  end
  object LOperations: TLabel
    Left = 220
    Top = 199
    Width = 98
    Height = 13
    Caption = 'Operations after print'
  end
  object LExample: TLabel
    Left = 220
    Top = 219
    Width = 278
    Height = 13
    Caption = 'Example, TM200 Open Drawer: #27#112#48#160#160#4'
  end
  object LExample2: TLabel
    Left = 220
    Top = 235
    Width = 266
    Height = 13
    Caption = 'Example, TM88 Open Drawer: #27#112#48#40#200#4'
  end
  object Label1: TLabel
    Left = 216
    Top = 80
    Width = 72
    Height = 13
    Caption = 'Text only driver'
  end
  object BOK: TButton
    Left = 8
    Top = 431
    Width = 81
    Height = 29
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = BOKClick
  end
  object BCancel: TButton
    Left = 128
    Top = 431
    Width = 81
    Height = 29
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object LSelPrinter: TListBox
    Left = 4
    Top = 4
    Width = 205
    Height = 301
    ItemHeight = 13
    TabOrder = 2
    OnClick = LSelPrinterClick
  end
  object ComboPrinters: TComboBox
    Left = 216
    Top = 20
    Width = 281
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = ComboPrintersChange
  end
  object CheckPrinterFonts: TCheckBox
    Left = 216
    Top = 48
    Width = 281
    Height = 21
    Caption = 'Printer Fonts (Windows GDI Only)'
    TabOrder = 4
    OnClick = CheckPrinterFontsClick
  end
  object GConfigFile: TGroupBox
    Left = 4
    Top = 315
    Width = 493
    Height = 101
    Caption = 'Configuraiton file'
    TabOrder = 5
    object EConfigFile: TEdit
      Left = 4
      Top = 72
      Width = 477
      Height = 21
      Color = clBtnFace
      TabOrder = 0
      Text = 'EConfigFile'
    end
    object RadioUser: TRadioButton
      Left = 8
      Top = 16
      Width = 329
      Height = 25
      Caption = 'User configuration'
      TabOrder = 1
      OnClick = RadioUserClick
    end
    object RadioSystem: TRadioButton
      Left = 8
      Top = 40
      Width = 329
      Height = 25
      Caption = 'System configuration'
      TabOrder = 2
      OnClick = RadioUserClick
    end
  end
  object GPageMargins: TGroupBox
    Left = 216
    Top = 119
    Width = 285
    Height = 73
    Caption = 'Position adjustment'
    TabOrder = 6
    object LLeft: TLabel
      Left = 16
      Top = 20
      Width = 18
      Height = 13
      Caption = 'Left'
    end
    object LTop: TLabel
      Left = 16
      Top = 48
      Width = 19
      Height = 13
      Caption = 'Top'
    end
    object LMetrics3: TLabel
      Left = 168
      Top = 20
      Width = 23
      Height = 13
      Caption = 'inch.'
    end
    object LMetrics4: TLabel
      Left = 168
      Top = 44
      Width = 23
      Height = 13
      Caption = 'inch.'
    end
    object ELeftMargin: TEdit
      Left = 80
      Top = 16
      Width = 77
      Height = 21
      TabOrder = 1
      OnChange = ELeftMarginChange
    end
    object ETopMargin: TEdit
      Left = 80
      Top = 44
      Width = 77
      Height = 21
      TabOrder = 0
      OnChange = ELeftMarginChange
    end
  end
  object CheckCutPaper: TCheckBox
    Left = 216
    Top = 255
    Width = 145
    Height = 21
    Caption = 'Cut paper'
    TabOrder = 7
    OnClick = CheckCutPaperClick
  end
  object ECutPaper: TEdit
    Left = 368
    Top = 255
    Width = 129
    Height = 21
    TabOrder = 8
    OnChange = ECutPaperChange
  end
  object CheckOpenDrawer: TCheckBox
    Left = 216
    Top = 279
    Width = 145
    Height = 21
    Caption = 'Open drawer'
    TabOrder = 9
    OnClick = CheckCutPaperClick
  end
  object EOpenDrawer: TEdit
    Left = 368
    Top = 279
    Width = 129
    Height = 21
    TabOrder = 10
    OnChange = ECutPaperChange
  end
  object ComboTextOnly: TComboBox
    Left = 308
    Top = 76
    Width = 129
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 11
    OnChange = ComboTextOnlyChange
  end
  object CheckOem: TCheckBox
    Left = 440
    Top = 75
    Width = 77
    Height = 21
    Caption = 'Oem'
    TabOrder = 12
    OnClick = CheckOemClick
  end
end
