object FTwainHelp: TFTwainHelp
  Left = 282
  Top = 197
  BorderStyle = bsDialog
  Caption = 'Twain'
  ClientHeight = 102
  ClientWidth = 310
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 262
    Height = 13
    Caption = 'Esperando la recepci'#243'n de una imagen del origen twain'
  end
  object BCancel: TButton
    Left = 115
    Top = 64
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancelar'
    TabOrder = 0
    OnClick = BCancelClick
  end
end
