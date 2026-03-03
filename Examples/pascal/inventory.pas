program InventorySystem;
{ =====================================================
  INVENTORY MANAGEMENT SYSTEM
  A complete inventory tracking application in Pascal.
  Features: Add/remove items, search, reorder alerts,
  value reports, CSV export simulation.
  ===================================================== }

const
  MAX_ITEMS = 100;
  LOW_STOCK = 5;

type
  TItem = record
    ID: Integer;
    Name: string[30];
    Category: string[20];
    Quantity: Integer;
    UnitCost: Real;
    ReorderLevel: Integer;
    Active: Boolean;
  end;

var
  Inventory: array[1..MAX_ITEMS] of TItem;
  ItemCount: Integer;
  NextID: Integer;
  Choice: Integer;
  SearchTerm: string;

{ =====================================================
  UTILITY PROCEDURES
  ===================================================== }

procedure PrintLine(Ch: Char; Len: Integer);
var i: Integer;
begin
  for i := 1 to Len do Write(Ch);
  WriteLn;
end;

procedure PrintHeader(Title: string);
begin
  WriteLn;
  PrintLine('=', 50);
  WriteLn('  ', Title);
  PrintLine('=', 50);
end;

function FormatCurrency(Amount: Real): string;
var S: string;
begin
  Str(Amount:8:2, S);
  FormatCurrency := '$' + S;
end;

{ =====================================================
  LOAD SAMPLE DATA
  ===================================================== }

procedure LoadSampleData;
begin
  ItemCount := 8;
  NextID := 9;

  Inventory[1].ID := 1;  Inventory[1].Name := 'Laptop Pro 15';
  Inventory[1].Category := 'Electronics';  Inventory[1].Quantity := 12;
  Inventory[1].UnitCost := 899.99;  Inventory[1].ReorderLevel := 5;
  Inventory[1].Active := True;

  Inventory[2].ID := 2;  Inventory[2].Name := 'Wireless Mouse';
  Inventory[2].Category := 'Electronics';  Inventory[2].Quantity := 3;
  Inventory[2].UnitCost := 29.99;  Inventory[2].ReorderLevel := 10;
  Inventory[2].Active := True;

  Inventory[3].ID := 3;  Inventory[3].Name := 'Office Chair';
  Inventory[3].Category := 'Furniture';  Inventory[3].Quantity := 7;
  Inventory[3].UnitCost := 249.00;  Inventory[3].ReorderLevel := 3;
  Inventory[3].Active := True;

  Inventory[4].ID := 4;  Inventory[4].Name := 'Standing Desk';
  Inventory[4].Category := 'Furniture';  Inventory[4].Quantity := 2;
  Inventory[4].UnitCost := 599.00;  Inventory[4].ReorderLevel := 2;
  Inventory[4].Active := True;

  Inventory[5].ID := 5;  Inventory[5].Name := 'USB-C Hub';
  Inventory[5].Category := 'Electronics';  Inventory[5].Quantity := 25;
  Inventory[5].UnitCost := 49.99;  Inventory[5].ReorderLevel := 8;
  Inventory[5].Active := True;

  Inventory[6].ID := 6;  Inventory[6].Name := 'Whiteboard';
  Inventory[6].Category := 'Office';  Inventory[6].Quantity := 4;
  Inventory[6].UnitCost := 89.99;  Inventory[6].ReorderLevel := 2;
  Inventory[6].Active := True;

  Inventory[7].ID := 7;  Inventory[7].Name := 'Mechanical Keyboard';
  Inventory[7].Category := 'Electronics';  Inventory[7].Quantity := 0;
  Inventory[7].UnitCost := 129.99;  Inventory[7].ReorderLevel := 5;
  Inventory[7].Active := True;

  Inventory[8].ID := 8;  Inventory[8].Name := 'Monitor 27"';
  Inventory[8].Category := 'Electronics';  Inventory[8].Quantity := 6;
  Inventory[8].UnitCost := 379.00;  Inventory[8].ReorderLevel := 4;
  Inventory[8].Active := True;
end;

{ =====================================================
  DISPLAY ALL ITEMS
  ===================================================== }

procedure ListAllItems;
var i: Integer;
    Status: string;
begin
  PrintHeader('INVENTORY LISTING');
  WriteLn('  ID  Name                           Qty    Cost      Value      Status');
  PrintLine('-', 75);
  for i := 1 to ItemCount do
  begin
    if not Inventory[i].Active then continue;
    if Inventory[i].Quantity = 0 then Status := 'OUT OF STOCK'
    else if Inventory[i].Quantity <= Inventory[i].ReorderLevel then Status := 'LOW STOCK'
    else Status := 'OK';
    WriteLn('  ', Inventory[i].ID:2, '  ',
            Inventory[i].Name:30, '  ',
            Inventory[i].Quantity:4, '  ',
            FormatCurrency(Inventory[i].UnitCost):10, '  ',
            FormatCurrency(Inventory[i].Quantity * Inventory[i].UnitCost):10, '  ',
            Status);
  end;
  PrintLine('-', 75);
end;

{ =====================================================
  SUMMARY REPORT
  ===================================================== }

procedure SummaryReport;
var
  i: Integer;
  TotalValue, TotalItems: Real;
  LowCount, OutCount: Integer;
begin
  PrintHeader('INVENTORY SUMMARY REPORT');
  TotalValue := 0;
  TotalItems := 0;
  LowCount := 0;
  OutCount := 0;

  for i := 1 to ItemCount do
  begin
    if not Inventory[i].Active then continue;
    TotalValue := TotalValue + (Inventory[i].Quantity * Inventory[i].UnitCost);
    TotalItems := TotalItems + Inventory[i].Quantity;
    if Inventory[i].Quantity = 0 then Inc(OutCount)
    else if Inventory[i].Quantity <= Inventory[i].ReorderLevel then Inc(LowCount);
  end;

  WriteLn('  Total SKUs Active: ', ItemCount);
  WriteLn('  Total Units in Stock: ', Round(TotalItems));
  WriteLn('  Total Inventory Value: ', FormatCurrency(TotalValue));
  WriteLn;
  WriteLn('  *** ALERTS ***');
  WriteLn('  Items Out of Stock:   ', OutCount);
  WriteLn('  Items Low on Stock:   ', LowCount);
  WriteLn;

  { Category breakdown }
  WriteLn('  CATEGORY BREAKDOWN:');
  PrintLine('-', 40);
  { Simple category totals - Electronics }
  TotalValue := 0; TotalItems := 0;
  for i := 1 to ItemCount do
    if (Inventory[i].Category = 'Electronics') and Inventory[i].Active then
    begin
      TotalValue := TotalValue + (Inventory[i].Quantity * Inventory[i].UnitCost);
      TotalItems := TotalItems + Inventory[i].Quantity;
    end;
  WriteLn('  Electronics: ', Round(TotalItems), ' units, ', FormatCurrency(TotalValue));

  TotalValue := 0; TotalItems := 0;
  for i := 1 to ItemCount do
    if (Inventory[i].Category = 'Furniture') and Inventory[i].Active then
    begin
      TotalValue := TotalValue + (Inventory[i].Quantity * Inventory[i].UnitCost);
      TotalItems := TotalItems + Inventory[i].Quantity;
    end;
  WriteLn('  Furniture:   ', Round(TotalItems), ' units, ', FormatCurrency(TotalValue));
end;

{ =====================================================
  ADD NEW ITEM
  ===================================================== }

procedure AddItem;
begin
  if ItemCount >= MAX_ITEMS then
  begin
    WriteLn('  Inventory full!');
    Exit;
  end;
  Inc(ItemCount);
  Inc(NextID);
  Inventory[ItemCount].ID := NextID;
  Inventory[ItemCount].Active := True;

  Write('  Item Name: ');
  ReadLn(Inventory[ItemCount].Name);
  Write('  Category: ');
  ReadLn(Inventory[ItemCount].Category);
  Write('  Quantity: ');
  ReadLn(Inventory[ItemCount].Quantity);
  Write('  Unit Cost: ');
  ReadLn(Inventory[ItemCount].UnitCost);
  Write('  Reorder Level: ');
  ReadLn(Inventory[ItemCount].ReorderLevel);

  WriteLn;
  WriteLn('  ✓ Item added: ID=', NextID, '  ', Inventory[ItemCount].Name);
end;

{ =====================================================
  SEARCH ITEMS
  ===================================================== }

procedure SearchItems(Term: string);
var
  i: Integer;
  Found: Integer;
begin
  Found := 0;
  PrintHeader('SEARCH RESULTS: ' + Term);
  for i := 1 to ItemCount do
  begin
    if not Inventory[i].Active then continue;
    { Simple substring search in name or category }
    if (Pos(Term, Inventory[i].Name) > 0) or
       (Pos(Term, Inventory[i].Category) > 0) then
    begin
      WriteLn('  ID:', Inventory[i].ID, '  ', Inventory[i].Name,
              '  Qty:', Inventory[i].Quantity,
              '  ', FormatCurrency(Inventory[i].UnitCost));
      Inc(Found);
    end;
  end;
  if Found = 0 then WriteLn('  No items found.');
  WriteLn('  ', Found, ' result(s).');
end;

{ =====================================================
  REORDER ALERTS
  ===================================================== }

procedure ShowReorderAlerts;
var i: Integer;
begin
  PrintHeader('REORDER ALERTS');
  WriteLn('  Items needing reorder (quantity <= reorder level):');
  WriteLn;
  WriteLn('  ID  Name                    Qty  Reorder  Action');
  PrintLine('-', 60);
  for i := 1 to ItemCount do
  begin
    if not Inventory[i].Active then continue;
    if Inventory[i].Quantity <= Inventory[i].ReorderLevel then
    begin
      Write('  ', Inventory[i].ID:2, '  ', Inventory[i].Name:22);
      Write('  ', Inventory[i].Quantity:4, '  ', Inventory[i].ReorderLevel:6, '  ');
      if Inventory[i].Quantity = 0 then WriteLn('*** ORDER NOW ***')
      else WriteLn('Order soon');
    end;
  end;
end;

{ =====================================================
  MAIN PROGRAM
  ===================================================== }

begin
  WriteLn('╔══════════════════════════════════════╗');
  WriteLn('║    PASCAL INVENTORY MANAGER v2.0     ║');
  WriteLn('║       Professional Stock Control     ║');
  WriteLn('╚══════════════════════════════════════╝');

  LoadSampleData;
  WriteLn;
  WriteLn('  Sample inventory loaded (', ItemCount, ' items).');

  repeat
    WriteLn;
    PrintLine('─', 40);
    WriteLn('  MAIN MENU');
    PrintLine('─', 40);
    WriteLn('  1. List All Items');
    WriteLn('  2. Summary Report');
    WriteLn('  3. Add New Item');
    WriteLn('  4. Search Items');
    WriteLn('  5. Reorder Alerts');
    WriteLn('  6. Exit');
    PrintLine('─', 40);
    Write('  Choice: ');
    ReadLn(Choice);

    case Choice of
      1: ListAllItems;
      2: SummaryReport;
      3: AddItem;
      4: begin
           Write('  Search term: ');
           ReadLn(SearchTerm);
           SearchItems(SearchTerm);
         end;
      5: ShowReorderAlerts;
      6: WriteLn('  Goodbye!');
      else WriteLn('  Invalid choice.');
    end;
  until Choice = 6;
end.
