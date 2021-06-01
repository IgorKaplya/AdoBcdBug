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
  end
  object btnSimpleAdo: TButton
    Left = 8
    Top = 76
    Width = 121
    Height = 25
    Caption = 'Simple ADO'
    TabOrder = 3
    OnClick = btnSimpleAdoClick
  end
  object btnSimpleFDac: TButton
    Left = 8
    Top = 107
    Width = 121
    Height = 25
    Caption = 'Simple FDac'
    TabOrder = 4
    OnClick = btnSimpleFDacClick
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
    ConnectionString = 
      'Provider=Microsoft.ACE.OLEDB.12.0;User ID=Admin;Data Source=C:\U' +
      'sers\igor.kaplya\Desktop\AdoBcdBug\Win32\Debug\TestVBA_OtherBase' +
      '.mdb;Mode=ReadWrite;Persist Security Info=False;Jet OLEDB:System' +
      ' database="";Jet OLEDB:Registry Path="";Jet OLEDB:Database Passw' +
      'ord="";Jet OLEDB:Engine Type=5;Jet OLEDB:Database Locking Mode=1' +
      ';Jet OLEDB:Global Partial Bulk Ops=2;Jet OLEDB:Global Bulk Trans' +
      'actions=1;Jet OLEDB:New Database Password="";Jet OLEDB:Create Sy' +
      'stem Database=False;Jet OLEDB:Encrypt Database=False;Jet OLEDB:D' +
      'on'#39't Copy Locale on Compact=False;Jet OLEDB:Compact Without Repl' +
      'ica Repair=False;Jet OLEDB:SFP=False;Jet OLEDB:Support Complex D' +
      'ata=False'
    LoginPrompt = False
    Provider = 'Microsoft.ACE.OLEDB.12.0'
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
