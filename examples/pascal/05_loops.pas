{
  Loops - For, While, Repeat-Until
  
  Demonstrates:
  - For loop (To and DownTo)
  - While loop
  - Repeat-Until loop
  - Loop control
}

Program Loops;
Var
  i, sum, number: Integer;

Begin
  { FOR LOOP - counting 1 to 10 }
  WriteLn('=== For Loop: Count to 10 ===');
  For i := 1 To 10 Do
    Write(i, ' ');
  WriteLn;
  WriteLn;
  
  { FOR LOOP - countdown }
  WriteLn('=== Countdown ===');
  For i := 10 DownTo 1 Do
    Write(i, '... ');
  WriteLn('Liftoff!');
  WriteLn;
  
  { FOR LOOP - sum of numbers }
  WriteLn('=== Sum of 1 to 100 ===');
  sum := 0;
  For i := 1 To 100 Do
    sum := sum + i;
  WriteLn('Sum: ', sum);
  WriteLn;
  
  { WHILE LOOP - multiplication table }
  WriteLn('=== 7 Times Table ===');
  i := 1;
  While i <= 12 Do
  Begin
    WriteLn('7 × ', i, ' = ', 7 * i);
    i := i + 1;
  End;
  WriteLn;
  
  { REPEAT-UNTIL LOOP - input validation }
  WriteLn('=== Input Validation ===');
  Repeat
    Write('Enter a positive number: ');
    ReadLn(number);
    If number <= 0 Then
      WriteLn('That''s not positive! Try again.');
  Until number > 0;
  WriteLn('Thank you! You entered: ', number);
  WriteLn;
  
  { Nested loops - pattern }
  WriteLn('=== Star Pattern ===');
  For i := 1 To 5 Do
  Begin
    For number := 1 To i Do
      Write('* ');
    WriteLn;
  End;
End.
