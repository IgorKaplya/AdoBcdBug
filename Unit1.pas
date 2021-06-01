unit Unit1;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  Data.DB, FireDAC.Comp.Client, Data.Win.ADODB, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MSAcc,
  FireDAC.Phys.MSAccDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet;

type
  TForm1 = class(TForm)
    conFDac: TFDConnection;
    TableFireDac: TFDTable;
    mmLog: TMemo;
    conAdo: TADOConnection;
    TableAdo: TADOTable;
    btnTestBcdAdo: TButton;
    btnBcdTestFDac: TButton;
    btnSimpleAdo: TButton;
    btnSimpleFDac: TButton;
    procedure btnSimpleAdoClick(Sender: TObject);
    procedure btnSimpleFDacClick(Sender: TObject);
    procedure btnTestBcdAdoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTestField: string;
    FTestTable: string;
    procedure AddInvitationMessage;
    procedure EnsureTestTableExists;
    procedure InitializeAdoConnection;
    procedure InitializeFDacConnection;
    procedure SetupTableComponents;
    procedure TestBCD(const ATable: TDataSet);
    procedure TestSimple(const ATable: TDataSet);
    { Private declarations }
  public
    property TestField: string read FTestField write FTestField;
    property TestTable: string read FTestTable write FTestTable;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Data.FmtBcd;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddInvitationMessage();
  InitializeAdoConnection;
  EnsureTestTableExists();
  InitializeFDacConnection;
  SetupTableComponents();
end;

procedure TForm1.AddInvitationMessage;
const
  description_message =
    'Hi, the bug appears when you have Windows regional settings: dot as grouping symbol and comma as decimal sepearator';
  affected_message: array[Boolean] of string = ('NOT AFFECTED', 'AFFECTED');
var
  systemIsAffected: Boolean;
begin
  mmLog.Lines.Add(description_message);
  mmLog.Lines.Add(Format('  Your grouping symbol: %s', [QuotedStr(FormatSettings.ThousandSeparator)]));
  mmLog.Lines.Add(Format('  Your decimal separator: %s', [QuotedStr(FormatSettings.DecimalSeparator)]));

  systemIsAffected :=
    SameText(FormatSettings.ThousandSeparator, '.') and
    SameText(FormatSettings.DecimalSeparator, ',');

  mmLog.Lines.Add('This system should be '+ affected_message[systemIsAffected]);
end;

procedure TForm1.btnSimpleAdoClick(Sender: TObject);
begin
  TestSimple(TableAdo);
end;

procedure TForm1.btnSimpleFDacClick(Sender: TObject);
begin
  TestSimple(TableFireDac);
end;

procedure TForm1.InitializeFDacConnection;
begin
  conFDac.DriverName := 'MSAcc';
  conFDac.Params.Database := '.\TestBase.mdb';
  conFDac.Connected := True;
  TableFireDac.Connection := conFDac;
end;

procedure TForm1.InitializeAdoConnection;
const
  ado_jet_connection_string =
    'Provider=Microsoft.Jet.OLEDB.4.0;'+
    'Data Source=TestBase.mdb;'+
    'Mode=ReadWrite;'+
    'Persist Security Info=False;';
  ado_ace_connection_string =
    'Provider=Microsoft.ACE.OLEDB.12.0;'+
    'Data Source=TestBase.mdb;'+
    'Mode=ReadWrite;'+
    'Persist Security Info=False;';
begin
  conAdo.ConnectionString := ado_ace_connection_string;
  conAdo.Connected := True;
  TableAdo.Connection := conAdo;
end;

procedure TForm1.EnsureTestTableExists;
var
  dummy: Integer;
  tables: TStringList;
begin
  TestTable := 'test_table';
  TestField := 'test_field';

  tables := TStringList.Create;
  tables.Sorted := true;
  try
    conAdo.GetTableNames(tables);
    if tables.Find(TestTable, dummy) then
      conAdo.Execute(format('drop table %s', [TestTable]));
    conAdo.Execute(format('create table %s (%s decimal(15,3))', [TestTable, TestField]));
  finally
    tables.Free;
  end;
end;

procedure TForm1.SetupTableComponents;
begin
  TableAdo.TableName := TestTable;
  TableFireDac.TableName := TestTable;
end;

procedure TForm1.TestBCD(const ATable: TDataSet);
var
  doubleValue: Double;
  bcd, normalizedBcd: TBCD;
  postedValue, reloadedValue: string;
begin
  mmLog.Lines.Add('');
  mmLog.Lines.Add('TestBCD: '+ATable.Name);

  doubleValue := 12.34567;
  mmLog.Lines.Add('Double: '+ doubleValue.ToString);

  bcd := DoubleToBcd(doubleValue);
  mmLog.Lines.Add('BCD: '+ BcdToStr(bcd));

  NormalizeBcd(bcd, normalizedBcd, 15, 3);
  mmLog.Lines.Add('Normalized BCD: '+BcdToStr(normalizedBcd));

  ATable.Open;
  ATable.Insert;
  ATable.FieldByName(TestField).AsBCD := normalizedBcd;
  ATable.Post;
  postedValue := ATable.FieldByName(TestField).AsString;
  mmLog.Lines.Add('Posted: ' + postedValue);

  ATable.Close;
  ATable.Open;
  ATable.Last;
  reloadedValue := ATable.FieldByName(TestField).AsString;
  mmLog.Lines.Add('Reloaded: '+reloadedValue);

  Assert(reloadedValue.Equals(postedValue), 'Reloaded value is not equal to posted.');
end;

procedure TForm1.btnTestBcdAdoClick(Sender: TObject);
begin
  TestBCD(TableAdo);
end;

procedure TForm1.TestSimple(const ATable: TDataSet);
var
  stringValue: String;
  postedValue, reloadedValue: string;
begin
  mmLog.Lines.Add('');
  mmLog.Lines.Add('TestSimple: '+ATable.Name);

  stringValue := '12,345';
  mmLog.Lines.Add('String: '+ stringValue);

  ATable.Open;
  ATable.Insert;
  ATable[TestField] := stringValue;
  ATable.Post;
  postedValue := ATable.FieldByName(TestField).AsString;
  mmLog.Lines.Add('Posted: ' + postedValue);

  ATable.Close;
  ATable.Open;
  ATable.Last;
  reloadedValue := ATable.FieldByName(TestField).AsString;
  mmLog.Lines.Add('Reloaded: '+reloadedValue);

  Assert(reloadedValue.Equals(postedValue), 'Reloaded value is not equal to posted.');
end;

end.
