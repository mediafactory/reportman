object FRpWizardVCL: TFRpWizardVCL
  Left = 193
  Top = 114
  Width = 611
  Height = 465
  Caption = 'New Report Wizard'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PControl: TPageControl
    Left = 0
    Top = 0
    Width = 603
    Height = 431
    ActivePage = TabReportType
    Align = alClient
    TabOrder = 0
    object TabInstructions: TTabSheet
      Caption = 'Instructions'
      object LDesign: TLabel
        Left = 4
        Top = 12
        Width = 282
        Height = 13
        Caption = 'To design a report with this wizard you must follow this steps'
      end
      object LPass1: TLabel
        Left = 4
        Top = 36
        Width = 347
        Height = 13
        Caption = 
          '1. Open database connections, follow the instuctions in Connecti' +
          'os page.'
      end
      object LPass2: TLabel
        Left = 4
        Top = 56
        Width = 267
        Height = 13
        Caption = '2. Open datasets, follow the instuctions at datasets page'
      end
      object LPass3: TLabel
        Left = 4
        Top = 76
        Width = 328
        Height = 13
        Caption = 
          '3. Select dataset fields to print, follow instructions at select' +
          ' fields page'
      end
      object LPass4: TLabel
        Left = 4
        Top = 96
        Width = 153
        Height = 13
        Caption = '4. Select report type and options'
      end
      object LBegin: TLabel
        Left = 4
        Top = 120
        Width = 176
        Height = 13
        Caption = 'To begin the wizard click Next button'
      end
      object PBottom2: TPanel
        Left = 0
        Top = 360
        Width = 595
        Height = 43
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          595
          43)
        object BCancel: TButton
          Left = 331
          Top = 9
          Width = 97
          Height = 29
          Anchors = [akRight, akBottom]
          Caption = 'Cancel'
          TabOrder = 0
          OnClick = BCancelClick
        end
        object BNext1: TButton
          Left = 491
          Top = 9
          Width = 97
          Height = 29
          Anchors = [akRight, akBottom]
          Caption = 'Next'
          TabOrder = 1
          OnClick = BNext1Click
        end
      end
    end
    object TabConnections: TTabSheet
      Caption = 'Connections'
      ImageIndex = 1
      object PBottom3: TPanel
        Left = 0
        Top = 366
        Width = 595
        Height = 37
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          595
          37)
        object Button1: TButton
          Left = 347
          Top = 4
          Width = 97
          Height = 29
          Anchors = [akRight, akBottom]
          Caption = 'Cancel'
          TabOrder = 0
          OnClick = BCancelClick
        end
        object BNext2: TButton
          Left = 491
          Top = 4
          Width = 97
          Height = 29
          Anchors = [akRight, akBottom]
          Caption = 'Next'
          TabOrder = 1
          OnClick = BNext1Click
        end
      end
    end
    object TabDatasets: TTabSheet
      Caption = 'Datasets'
      ImageIndex = 2
      DesignSize = (
        595
        403)
      object BNExt3: TButton
        Left = 491
        Top = 365
        Width = 97
        Height = 29
        Anchors = [akRight, akBottom]
        Caption = 'Next'
        TabOrder = 0
        OnClick = BNext1Click
      end
      object Button2: TButton
        Left = 359
        Top = 365
        Width = 97
        Height = 29
        Anchors = [akRight, akBottom]
        Caption = 'Cancel'
        TabOrder = 1
        OnClick = BCancelClick
      end
    end
    object TabFields: TTabSheet
      Caption = 'Fields'
      ImageIndex = 3
      DesignSize = (
        595
        403)
      object BNext4: TButton
        Left = 491
        Top = 365
        Width = 97
        Height = 29
        Anchors = [akRight, akBottom]
        Caption = 'Next'
        TabOrder = 0
        OnClick = BNext1Click
      end
      object Button3: TButton
        Left = 359
        Top = 365
        Width = 97
        Height = 29
        Anchors = [akRight, akBottom]
        Caption = 'Cancel'
        TabOrder = 1
        OnClick = BCancelClick
      end
    end
    object TabReportType: TTabSheet
      Caption = 'Report type'
      ImageIndex = 4
      DesignSize = (
        595
        403)
      object BFinish: TButton
        Left = 491
        Top = 365
        Width = 97
        Height = 29
        Anchors = [akRight, akBottom]
        Caption = 'Finish'
        TabOrder = 0
        OnClick = BFinishClick
      end
      object Button4: TButton
        Left = 359
        Top = 365
        Width = 97
        Height = 29
        Anchors = [akRight, akBottom]
        Caption = 'Cancel'
        TabOrder = 1
        OnClick = BCancelClick
      end
    end
  end
end
