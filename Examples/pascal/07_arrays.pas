{
  Arrays - Working with Collections
  
  Demonstrates:
  - Array declarations
  - Accessing array elements
  - Iterating through arrays
  - Multi-dimensional arrays
}

Program Arrays;
Var
  numbers: Array[1..5] Of Integer;
  scores: Array[1..10] Of Integer;
  table: Array[1..5, 1..5] Of Integer;
  i, j, sum, max: Integer;
  average: Real;

Begin
  { ARRAY BASICS }
  WriteLn('=== Array Basics ===');
  numbers[1] := 10;
  numbers[2] := 20;
  numbers[3] := 30;
  numbers[4] := 40;
  numbers[5] := 50;
  
  WriteLn('Array elements:');
  For i := 1 To 5 Do
    WriteLn('numbers[', i, '] = ', numbers[i]);
  WriteLn;
  
  { ARRAY OPERATIONS }
  WriteLn('=== Array Operations ===');
  
  { Sum }
  sum := 0;
  For i := 1 To 5 Do
    sum := sum + numbers[i];
  WriteLn('Sum: ', sum);
  
  { Average }
  average := sum / 5.0;
  WriteLn('Average: ', average:0:1);
  
  { Find maximum }
  max := numbers[1];
  For i := 2 To 5 Do
    If numbers[i] > max Then
      max := numbers[i];
  WriteLn('Maximum: ', max);
  WriteLn;
  
  { STORING SCORES }
  WriteLn('=== Student Scores ===');
  WriteLn('Enter 10 test scores:');
  For i := 1 To 10 Do
  Begin
    Write('Score ', i, ': ');
    ReadLn(scores[i]);
  End;
  
  { Calculate average }
  sum := 0;
  For i := 1 To 10 Do
    sum := sum + scores[i];
  WriteLn;
  WriteLn('Class average: ', (sum / 10.0):0:1);
  WriteLn;
  
  { 2D ARRAY - Multiplication Table }
  WriteLn('=== 2D Array: Multiplication Table ===');
  
  { Fill table }
  For i := 1 To 5 Do
    For j := 1 To 5 Do
      table[i, j] := i * j;
  
  { Print table }
  Write('     ');
  For j := 1 To 5 Do
    Write(j:3, ' ');
  WriteLn;
  WriteLn('   -------------------');
  
  For i := 1 To 5 Do
  Begin
    Write(i, ' | ');
    For j := 1 To 5 Do
      Write(table[i, j]:3, ' ');
    WriteLn;
  End;
End.
