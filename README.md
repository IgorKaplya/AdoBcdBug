# AdoBcdBug

The bug appears when you have Windows regional settings: dot as grouping symbol and comma as decimal sepearator.
When you try to set value to Decimal field in MS ACCESS using AdoConnection + AdoTable, you get actual value multiplied
with precision, ie: you set 12,354 you get 12354 for Decimal(18,3) field in MDB.

>	table['Filed'] := 12.354

FireDac MSAcc driver doesn't meet this bug.
The bug appears on both providers
	Microsoft.Jet.OLEDB.4.0
	Microsoft.ACE.OLEDB.12.0

Bug tracking: https://quality.embarcadero.com/browse/RSP-34075