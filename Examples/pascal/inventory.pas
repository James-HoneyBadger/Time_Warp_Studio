program InventorySystem;
{ =========================================================
  PASCAL INVENTORY MANAGEMENT SYSTEM
  Demonstrates: arrays, loops, procedures, string handling,
  format specifiers, and computed totals.
  ========================================================= }

const
  MAX_ITEMS = 10;

var
  ItemName: array[1..MAX_ITEMS] of String;
  ItemCat: array[1..MAX_ITEMS] of String;
  ItemQty: array[1..MAX_ITEMS] of Integer;
  ItemCost: array[1..MAX_ITEMS] of Real;
  ItemCount: Integer;
  TotalValue: Real;
  I: Integer;

procedure LoadSampleData;
begin
  ItemCount := 6;
  ItemName[1] := 'Laptop Pro 15';
  ItemCat[1] := 'Electronics';
  ItemQty[1] := 12;
  ItemCost[1] := 899.99;
  ItemName[2] := 'Wireless Mouse';
  ItemCat[2] := 'Accessories';
  ItemQty[2] := 45;
  ItemCost[2] := 29.99;
  ItemName[3] := 'USB-C Hub';
  ItemCat[3] := 'Accessories';
  ItemQty[3] := 3;
  ItemCost[3] := 49.99;
  ItemName[4] := 'Monitor 27in';
  ItemCat[4] := 'Electronics';
  ItemQty[4] := 8;
  ItemCost[4] := 549.99;
  ItemName[5] := 'Keyboard Mech';
  ItemCat[5] := 'Accessories';
  ItemQty[5] := 30;
  ItemCost[5] := 79.99;
  ItemName[6] := 'SSD 1TB';
  ItemCat[6] := 'Storage';
  ItemQty[6] := 15;
  ItemCost[6] := 109.99;
end;

procedure CalcTotal;
begin
  TotalValue := 0;
  for I := 1 to ItemCount do
    TotalValue := TotalValue + (ItemQty[I] * ItemCost[I]);
end;

procedure PrintReport;
begin
  writeln('=== INVENTORY MANAGEMENT SYSTEM v1.0 ===');
  writeln;
  writeln('--- SUMMARY ---');
  writeln('Total items in catalog: ', ItemCount);
  writeln('Total inventory value:  $', TotalValue:0:2);
  writeln;
  writeln('--- INVENTORY LISTING ---');
  for I := 1 to ItemCount do
    writeln('  ', ItemName[I], ' (', ItemCat[I], ') Qty:', ItemQty[I], ' $', ItemCost[I]:0:2);
  writeln;
  writeln('Inventory system complete.');
end;

begin
  LoadSampleData;
  CalcTotal;
  PrintReport;
end.
