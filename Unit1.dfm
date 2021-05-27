object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 299
  ClientWidth = 679
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object mmLog: TMemo
    Left = 240
    Top = 16
    Width = 425
    Height = 275
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnTestBcdAdo: TButton
    Left = 8
    Top = 14
    Width = 121
    Height = 25
    Caption = ' BCD Test ADO'
    TabOrder = 1
    OnClick = btnTestBcdAdoClick
  end
  object btnBcdTestFDac: TButton
    Left = 8
    Top = 45
    Width = 121
    Height = 25
    Caption = ' BCD Test FDac'
    TabOrder = 2
    OnClick = btnBcdTestFDacClick
  end
  object conFDac: TFDConnection
    Params.Strings = (
      'DriverID=MSAcc')
    LoginPrompt = False
    Left = 16
    Top = 144
  end
  object TableFireDac: TFDTable
    Connection = conFDac
    UpdateOptions.UpdateTableName = 'PACK'
    TableName = 'PACK'
    Left = 80
    Top = 144
  end
  object conAdo: TADOConnection
    LoginPrompt = False
    Mode = cmReadWrite
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 16
    Top = 200
  end
  object TableAdo: TADOTable
    Connection = conAdo
    TableName = 'PACK'
    Left = 80
    Top = 200
  end
end
