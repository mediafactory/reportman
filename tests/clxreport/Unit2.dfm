object Form1: TForm1
  Left = 192
  Top = 107
  Width = 544
  Height = 375
  Caption = 'Form1'
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
  object Label1: TLabel
    Left = 80
    Top = 16
    Width = 35
    Height = 13
    Caption = 'Report '
  end
  object Button1: TButton
    Left = 176
    Top = 88
    Width = 85
    Height = 41
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object EReportname: TEdit
    Left = 80
    Top = 36
    Width = 441
    Height = 21
    TabOrder = 1
    Text = 'sample4.rep'
  end
  object VCLReport1: TVCLReport
    Filename = 'sample4.rep'
    Title = 'Untitled'
    Left = 32
    Top = 60
  end
  object Table1: TTable
    Active = True
    DatabaseName = 'DBDEMOS'
    TableName = 'animals.dbf'
    Left = 168
    Top = 164
  end
end
