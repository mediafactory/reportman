object FRpPageSetupVCL: TFRpPageSetupVCL
  Left = 245
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 376
  ClientWidth = 442
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PControl: TPageControl
    Left = 0
    Top = 0
    Width = 442
    Height = 335
    ActivePage = TabOptions
    Align = alClient
    TabOrder = 0
    object TabPage: TTabSheet
      Caption = 'Page setup'
      object SColor: TShape
        Left = 164
        Top = 248
        Width = 33
        Height = 33
        OnMouseDown = SColorMouseDown
      end
      object RPageSize: TRadioGroup
        Left = 4
        Top = 8
        Width = 177
        Height = 93
        Caption = 'Page size'
        Items.Strings = (
          'Default'
          'Custom'
          'User defined')
        TabOrder = 0
        OnClick = RPageSizeClick
      end
      object GPageSize: TGroupBox
        Left = 184
        Top = 12
        Width = 233
        Height = 65
        Caption = 'Custom size'
        TabOrder = 2
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
        Left = 4
        Top = 104
        Width = 177
        Height = 67
        Caption = 'Page orientation'
        Items.Strings = (
          'Default'
          'Custom')
        TabOrder = 3
        OnClick = RPageOrientationClick
      end
      object RCustomOrientation: TRadioGroup
        Left = 188
        Top = 104
        Width = 177
        Height = 67
        Caption = 'Custom page orientation'
        Items.Strings = (
          'Portrait'
          'Landscape')
        TabOrder = 4
        Visible = False
      end
      object BBackground: TButton
        Left = 4
        Top = 248
        Width = 149
        Height = 33
        Caption = 'Background color'
        TabOrder = 6
        OnClick = BBackgroundClick
      end
      object GPageMargins: TGroupBox
        Left = 4
        Top = 172
        Width = 413
        Height = 69
        Caption = 'Page Margins'
        TabOrder = 5
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
        object ELeftMargin: TRpMaskEdit
          Left = 80
          Top = 12
          Width = 77
          Height = 21
          TabOrder = 0
          EditType = tecurrency
        end
        object ETopMargin: TRpMaskEdit
          Left = 80
          Top = 40
          Width = 77
          Height = 21
          TabOrder = 2
          EditType = tecurrency
        end
        object ERightMargin: TRpMaskEdit
          Left = 272
          Top = 12
          Width = 77
          Height = 21
          TabOrder = 1
          EditType = tecurrency
        end
        object EBottomMargin: TRpMaskEdit
          Left = 272
          Top = 40
          Width = 77
          Height = 21
          TabOrder = 3
          EditType = tecurrency
        end
      end
      object GUserDefined: TGroupBox
        Left = 184
        Top = 8
        Width = 241
        Height = 73
        Caption = 'Custom page size (Windows only)'
        TabOrder = 1
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
        object EPageheight: TRpMaskEdit
          Left = 100
          Top = 44
          Width = 77
          Height = 21
          TabOrder = 1
          EditType = tecurrency
        end
        object EPageWidth: TRpMaskEdit
          Left = 100
          Top = 20
          Width = 77
          Height = 21
          TabOrder = 0
          EditType = tecurrency
        end
      end
    end
    object TabPrint: TTabSheet
      Caption = 'Print setup'
      ImageIndex = 1
      object LSelectPrinter: TLabel
        Left = 8
        Top = 92
        Width = 63
        Height = 13
        Caption = 'Select Printer'
      end
      object LCopies: TLabel
        Left = 8
        Top = 148
        Width = 32
        Height = 13
        Caption = 'Copies'
      end
      object LPrinterFonts: TLabel
        Left = 8
        Top = 8
        Width = 158
        Height = 13
        Caption = 'Printer Fonts (Windows GDI Only)'
      end
      object LRLang: TLabel
        Left = 8
        Top = 36
        Width = 79
        Height = 13
        Caption = 'Report language'
      end
      object LPreview: TLabel
        Left = 8
        Top = 60
        Width = 126
        Height = 13
        Caption = 'Preview window and scale'
      end
      object ComboSelPrinter: TComboBox
        Left = 224
        Top = 88
        Width = 201
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 3
      end
      object BConfigure: TButton
        Left = 8
        Top = 112
        Width = 213
        Height = 25
        Caption = 'Configure printers'
        TabOrder = 5
        OnClick = BConfigureClick
      end
      object CheckPrintOnlyIfData: TCheckBox
        Left = 8
        Top = 220
        Width = 209
        Height = 21
        Caption = 'Print only if data available'
        TabOrder = 9
      end
      object CheckTwoPass: TCheckBox
        Left = 8
        Top = 196
        Width = 209
        Height = 21
        Caption = 'Two pass report'
        TabOrder = 8
      end
      object ECopies: TRpMaskEdit
        Left = 152
        Top = 144
        Width = 69
        Height = 21
        TabOrder = 6
        EditType = teinteger
      end
      object CheckCollate: TCheckBox
        Left = 8
        Top = 172
        Width = 213
        Height = 21
        Caption = 'Collate copies'
        TabOrder = 7
      end
      object ComboPrinterFonts: TComboBox
        Left = 224
        Top = 4
        Width = 201
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        Items.Strings = (
          'Default'
          'Always use printer fonts'
          'Never use printer fonts')
      end
      object ComboLanguage: TComboBox
        Left = 224
        Top = 32
        Width = 201
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 1
      end
      object ComboPreview: TComboBox
        Left = 224
        Top = 60
        Width = 100
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        Items.Strings = (
          'Normal'
          'Maxmized')
      end
      object ComboStyle: TComboBox
        Left = 328
        Top = 60
        Width = 97
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        Items.Strings = (
          'Wide'
          'Normal'
          'Page')
      end
      object CheckDrawerAfter: TCheckBox
        Left = 8
        Top = 268
        Width = 373
        Height = 21
        Caption = 'Open drawer after printing'
        TabOrder = 11
      end
      object CheckDrawerBefore: TCheckBox
        Left = 8
        Top = 244
        Width = 405
        Height = 21
        Caption = 'Open drawer before printing'
        TabOrder = 10
      end
      object CheckPreviewAbout: TCheckBox
        Left = 212
        Top = 168
        Width = 205
        Height = 21
        Caption = 'About box in preview'
        TabOrder = 12
      end
      object CheckMargins: TCheckBox
        Left = 212
        Top = 192
        Width = 205
        Height = 21
        Caption = 'Printable margins in preview'
        TabOrder = 13
      end
    end
    object TabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 2
      object LPreferedFormat: TLabel
        Left = 8
        Top = 12
        Width = 98
        Height = 13
        Caption = 'Prefered save format'
      end
      object ComboFormat: TComboBox
        Left = 124
        Top = 8
        Width = 181
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 335
    Width = 442
    Height = 41
    Align = alBottom
    TabOrder = 1
    object BOK: TButton
      Left = 8
      Top = 8
      Width = 101
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = BOKClick
    end
    object BCancel: TButton
      Left = 116
      Top = 8
      Width = 97
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = BCancelClick
    end
  end
  object ColorDialog1: TColorDialog
    Ctl3D = True
    Left = 336
    Top = 288
  end
end
