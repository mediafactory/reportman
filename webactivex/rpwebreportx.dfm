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
    Left = 28
    Top = 180
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
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 108
    Top = 52
  end
end
