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
    procedure btnBcdTestFDacClick(Sender: TObject);
    procedure btnTestBcdAdoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure TestBCD(const ATable: TDataSet);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Data.FmtBcd;

{$R *.dfm}

procedure TForm1.btnBcdTestFDacClick(Sender: TObject);
begin
  TestBCD(TableFireDac);
end;

procedure TForm1.btnTestBcdAdoClick(Sender: TObject);
begin
  TestBCD(TableAdo);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  conAdo.ConnectionString :=
    'Provider=Microsoft.Jet.OLEDB.4.0;'+
    'Data Source=TestBase.mdb;'+
    'Mode=ReadWrite;'+
    'Persist Security Info=False;';
  conAdo.Connected := True;
  TableAdo.Connection := conAdo;

  conFDac.DriverName := 'MSAcc';
  conFDac.Params.AddPair('Database','.\TestBase.mdb');
  conFDac.Connected := True;
  TableFireDac.Connection := conFDac;
end;

procedure TForm1.TestBCD(const ATable: TDataSet);
var
  doubleValue: Double;
  bcd, normalizedBcd: TBCD;
begin
  mmLog.Lines.Add('');
  mmLog.Lines.Add(ATable.Name);

  doubleValue := 12.34567;
  mmLog.Lines.Add('Double: '+ doubleValue.ToString);

  bcd := DoubleToBcd(doubleValue);
  mmLog.Lines.Add('BCD: '+ BcdToStr(bcd));

  NormalizeBcd(bcd, normalizedBcd, 15, 3);
  mmLog.Lines.Add('Normalized BCD: '+BcdToStr(normalizedBcd));

  ATable.Open;
  ATable.Insert;
  ATable['PROD'] := DateTimeToStr(Now);
  ATable.FieldByName('QTY').AsBCD := normalizedBcd;
  ATable['KG'] := 0;
  ATable['DEFAULT'] := 0;
  ATable.Post;
  mmLog.Lines.Add('Posted: '+ATable.FieldByName('QTY').AsString);

  ATable.Close;
  ATable.Open;
  ATable.Last;
  mmLog.Lines.Add('Reloaded: '+ATable.FieldByName('QTY').AsString);
end;


end.
