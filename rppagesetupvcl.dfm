object FRpPageSetupVCL: TFRpPageSetupVCL
  Left = 245
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 460
  ClientWidth = 455
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SColor: TShape
    Left = 388
    Top = 320
    Width = 33
    Height = 33
    OnMouseDown = SColorMouseDown
  end
  object LRLang: TLabel
    Left = 12
    Top = 400
    Width = 79
    Height = 13
    Caption = 'Report language'
  end
  object LCopies: TLabel
    Left = 8
    Top = 296
    Width = 32
    Height = 13
    Caption = 'Copies'
  end
  object LPrinterFonts: TLabel
    Left = 12
    Top = 372
    Width = 158
    Height = 13
    Caption = 'Printer Fonts (Windows GDI Only)'
  end
  object LSelectPrinter: TLabel
    Left = 8
    Top = 12
    Width = 63
    Height = 13
    Caption = 'Select Printer'
  end
  object BOK: TButton
    Left = 112
    Top = 424
    Width = 101
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = BOKClick
  end
  object BCancel: TButton
    Left = 220
    Top = 424
    Width = 97
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = BCancelClick
  end
  object RPageSize: TRadioGroup
    Left = 8
    Top = 40
    Width = 177
    Height = 93
    Caption = 'Page size'
    Items.Strings = (
      'Default'
      'Custom'
      'User defined')
    TabOrder = 2
    OnClick = RPageSizeClick
  end
  object GPageSize: TGroupBox
    Left = 192
    Top = 40
    Width = 233
    Height = 65
    Caption = 'Custom size'
    TabOrder = 3
    Visible = False
    object ComboPageSize: TComboBox
      Left = 4
      Top = 32
      Width = 225
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object RPageOrientation: TRadioGroup
    Left = 8
    Top = 144
    Width = 177
    Height = 67
    Caption = 'Page orientation'
    Items.Strings = (
      'Default'
      'Custom')
    TabOrder = 4
    OnClick = RPageOrientationClick
  end
  object RCustomOrientation: TRadioGroup
    Left = 192
    Top = 144
    Width = 177
    Height = 67
    Caption = 'Custom page orientation'
    Items.Strings = (
      'Portrait'
      'Landscape')
    TabOrder = 5
    Visible = False
  end
  object BBackground: TButton
    Left = 228
    Top = 320
    Width = 149
    Height = 33
    Caption = 'Background color'
    TabOrder = 6
    OnClick = BBackgroundClick
  end
  object ComboLanguage: TComboBox
    Left = 228
    Top = 396
    Width = 201
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 7
  end
  object GPageMargins: TGroupBox
    Left = 8
    Top = 216
    Width = 413
    Height = 69
    Caption = 'Page Margins'
    TabOrder = 8
    object LLeft: TLabel
      Left = 16
      Top = 16
      Width = 18
      Height = 13
      Caption = 'Left'
    end
    object LTop: TLabel
      Left = 16
      Top = 44
      Width = 19
      Height = 13
      Caption = 'Top'
    end
    object LMetrics3: TLabel
      Left = 168
      Top = 16
      Width = 23
      Height = 13
      Caption = 'inch.'
    end
    object LMetrics4: TLabel
      Left = 168
      Top = 40
      Width = 23
      Height = 13
      Caption = 'inch.'
    end
    object LMetrics5: TLabel
      Left = 360
      Top = 16
      Width = 23
      Height = 13
      Caption = 'inch.'
    end
    object LRight: TLabel
      Left = 212
      Top = 16
      Width = 25
      Height = 13
      Caption = 'Right'
    end
    object LBottom: TLabel
      Left = 212
      Top = 44
      Width = 33
      Height = 13
      Caption = 'Bottom'
    end
    object LMetrics6: TLabel
      Left = 360
      Top = 44
      Width = 23
      Height = 13
      Caption = 'inch.'
    end
    object ELeftMargin: TEdit
      Left = 80
      Top = 12
      Width = 77
      Height = 21
      TabOrder = 0
    end
    object ETopMargin: TEdit
      Left = 80
      Top = 40
      Width = 77
      Height = 21
      TabOrder = 2
    end
    object ERightMargin: TEdit
      Left = 272
      Top = 12
      Width = 77
      Height = 21
      TabOrder = 1
    end
    object EBottomMargin: TEdit
      Left = 272
      Top = 40
      Width = 77
      Height = 21
      TabOrder = 3
    end
  end
  object ECopies: TEdit
    Left = 88
    Top = 292
    Width = 69
    Height = 21
    TabOrder = 9
  end
  object CheckCollate: TCheckBox
    Left = 176
    Top = 292
    Width = 213
    Height = 21
    Caption = 'Collate copies'
    TabOrder = 10
  end
  object CheckTwoPass: TCheckBox
    Left = 12
    Top = 320
    Width = 209
    Height = 21
    Caption = 'Two pass report'
    TabOrder = 11
  end
  object ComboPrinterFonts: TComboBox
    Left = 228
    Top = 368
    Width = 201
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 12
    Items.Strings = (
      'Default'
      'Always use printer fonts'
      'Never use printer fonts')
  end
  object GUserDefined: TGroupBox
    Left = 188
    Top = 40
    Width = 241
    Height = 73
    Caption = 'Custom page size (Windows only)'
    TabOrder = 13
    Visible = False
    object LMetrics7: TLabel
      Left = 190
      Top = 24
      Width = 23
      Height = 13
      Caption = 'inch.'
    end
    object LMetrics8: TLabel
      Left = 190
      Top = 48
      Width = 23
      Height = 13
      Caption = 'inch.'
    end
    object LWidth: TLabel
      Left = 16
      Top = 24
      Width = 28
      Height = 13
      Caption = 'Width'
    end
    object LHeight: TLabel
      Left = 16
      Top = 48
      Width = 31
      Height = 13
      Caption = 'Height'
    end
    object EPageheight: TEdit
      Left = 100
      Top = 44
      Width = 77
      Height = 21
      TabOrder = 0
    end
    object EPageWidth: TEdit
      Left = 100
      Top = 20
      Width = 77
      Height = 21
      TabOrder = 1
    end
  end
  object ComboSelPrinter: TComboBox
    Left = 132
    Top = 8
    Width = 181
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 14
  end
  object BConfigure: TButton
    Left = 316
    Top = 8
    Width = 113
    Height = 21
    Caption = 'Configure'
    TabOrder = 15
    OnClick = BConfigureClick
  end
  object CheckPrintOnlyIfData: TCheckBox
    Left = 12
    Top = 344
    Width = 209
    Height = 21
    Caption = 'Print only if data available'
    TabOrder = 16
  end
  object ColorDialog1: TColorDialog
    Left = 360
    Top = 320
  end
end
