{
  Conditional Statements
  
  Demonstrates:
  - If-Then-Else statements
  - Case statement (Pascal's switch)
  - Boolean expressions
  - Nested conditionals
}

Program Conditionals;
Var
  score: Integer;
  grade: Char;

Begin
  WriteLn('=== Grade Calculator ===');
  WriteLn;
  
  Write('Enter your score (0-100): ');
  ReadLn(score);
  
  { Validate input }
  If (score < 0) Or (score > 100) Then
  Begin
    WriteLn('Error: Score must be between 0 and 100!');
    Halt(1);
  End;
  
  { Determine letter grade }
  If score >= 90 Then
  Begin
    grade := 'A';
    WriteLn('Excellent! Grade: ', grade);
  End
  Else If score >= 80 Then
  Begin
    grade := 'B';
    WriteLn('Good job! Grade: ', grade);
  End
  Else If score >= 70 Then
  Begin
    grade := 'C';
    WriteLn('Average. Grade: ', grade);
  End
  Else If score >= 60 Then
  Begin
    grade := 'D';
    WriteLn('Below average. Grade: ', grade);
  End
  Else
  Begin
    grade := 'F';
    WriteLn('Failing. Grade: ', grade);
  End;
  
  { Case statement example }
  WriteLn;
  Write('Feedback: ');
  Case grade Of
    'A': WriteLn('Outstanding work!');
    'B': WriteLn('Keep it up!');
    'C': WriteLn('Room for improvement.');
    'D': WriteLn('Need more study time.');
    'F': WriteLn('Please see instructor.');
  Else
    WriteLn('Unknown grade.');
  End;
End.
