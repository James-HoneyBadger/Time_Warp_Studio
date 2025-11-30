{
  Input and Output
  
  Demonstrates:
  - ReadLn for user input
  - Write vs WriteLn
  - Working with different data types
  - Formatted output with :0:n
}

Program InputOutput;
Var
  name: String;
  age: Integer;
  height: Real;
  ageInMonths: Integer;

Begin
  WriteLn('=== Personal Information ===');
  WriteLn;
  
  { Get user input }
  Write('Enter your name: ');
  ReadLn(name);
  
  Write('Enter your age: ');
  ReadLn(age);
  
  Write('Enter your height in feet: ');
  ReadLn(height);
  
  { Display formatted output }
  WriteLn;
  WriteLn('=== Your Information ===');
  WriteLn('Name: ', name);
  WriteLn('Age: ', age, ' years old');
  WriteLn('Height: ', height:0:1, ' feet');
  
  { Calculate and display }
  ageInMonths := age * 12;
  WriteLn('Age in months: ', ageInMonths);
End.
