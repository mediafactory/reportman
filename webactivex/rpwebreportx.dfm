object WebReportMan: TWebReportMan
  Left = 194
  Top = 114
  Width = 428
  Height = 307
  Caption = 'WebReportMan'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = ActiveFormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object webmetaprint: TRpWebMetaPrint
    Left = 4
    Top = 12
    Width = 413
    Height = 77
    Install = False
    PrinterConfig = False
    Port = 80
  end
  object XPManifest1: TXPManifest
    Left = 104
    Top = 144
  end
end
