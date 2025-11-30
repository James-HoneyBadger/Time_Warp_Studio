{
  Variables and Data Types
  
  Demonstrates:
  - Variable declarations (Var section)
  - Different types: Integer, Real, Char, String
  - Assignment with :=
  - Formatted output
}

Program VariablesAndTypes;
Var
  age: Integer;
  price: Real;
  grade: Char;
  name: String;
  year: Integer;
  temperature: Real;

Begin
  { Initialize variables }
  name := 'Alice';
  age := 25;
  grade := 'A';
  year := 2025;
  price := 19.99;
  temperature := 72.5;
  
  WriteLn('=== Variable Examples ===');
  WriteLn;
  
  WriteLn('Name: ', name);
  WriteLn('Age: ', age, ' years');
  WriteLn('Grade: ', grade);
  WriteLn('Year: ', year);
  WriteLn('Price: $', price:0:2);
  WriteLn('Temperature: ', temperature:0:1, '°F');
  
  { Arithmetic operations }
  WriteLn;
  WriteLn('Calculations:');
  WriteLn('Next year: ', year + 1);
  WriteLn('Two items: $', (price * 2):0:2);
End.
