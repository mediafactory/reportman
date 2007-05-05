object Form1: TForm1
  Left = 264
  Top = 110
  Width = 696
  Height = 480
  Caption = 'Twain Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object XPManifest1: TXPManifest
    Left = 260
    Top = 76
  end
  object MainMenu1: TMainMenu
    Left = 336
    Top = 228
    object Archivo1: TMenuItem
      Caption = 'Archivo'
      object Empezar1: TMenuItem
        Caption = 'Empezar'
        OnClick = Empezar1Click
      end
    end
  end
end
