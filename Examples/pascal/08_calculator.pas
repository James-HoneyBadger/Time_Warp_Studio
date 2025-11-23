{
  Simple Calculator
  
  Complete program demonstrating:
  - Menu-driven interface
  - Functions and procedures
  - Case statement
  - Loops for continuous operation
  - Input validation
}

Program Calculator;
Var
  choice: Integer;
  num1, num2, result: Real;
  continueCalc: Char;

{ Function: Add two numbers }
Function Add(a, b: Real): Real;
Begin
  Add := a + b;
End;

{ Function: Subtract two numbers }
Function Subtract(a, b: Real): Real;
Begin
  Subtract := a - b;
End;

{ Function: Multiply two numbers }
Function Multiply(a, b: Real): Real;
Begin
  Multiply := a * b;
End;

{ Function: Divide two numbers }
Function Divide(a, b: Real): Real;
Begin
  Divide := a / b;
End;

{ Procedure: Display menu }
Procedure DisplayMenu;
Begin
  WriteLn('Choose an operation:');
  WriteLn('1. Addition (+)');
  WriteLn('2. Subtraction (-)');
  WriteLn('3. Multiplication (×)');
  WriteLn('4. Division (÷)');
End;

{ Main program }
Begin
  WriteLn('=================================');
  WriteLn('   SIMPLE CALCULATOR PROGRAM');
  WriteLn('=================================');
  WriteLn;
  
  Repeat
    DisplayMenu;
    Write('Enter your choice (1-4): ');
    ReadLn(choice);
    
    If (choice < 1) Or (choice > 4) Then
    Begin
      WriteLn('Invalid choice! Please try again.');
      WriteLn;
      Continue;
    End;
    
    Write('Enter first number: ');
    ReadLn(num1);
    Write('Enter second number: ');
    ReadLn(num2);
    
    Case choice Of
      1: Begin
           result := Add(num1, num2);
           WriteLn;
           WriteLn(num1:0:2, ' + ', num2:0:2, ' = ', result:0:2);
         End;
      2: Begin
           result := Subtract(num1, num2);
           WriteLn;
           WriteLn(num1:0:2, ' - ', num2:0:2, ' = ', result:0:2);
         End;
      3: Begin
           result := Multiply(num1, num2);
           WriteLn;
           WriteLn(num1:0:2, ' × ', num2:0:2, ' = ', result:0:2);
         End;
      4: Begin
           If num2 = 0 Then
             WriteLn('Error: Cannot divide by zero!')
           Else
           Begin
             result := Divide(num1, num2);
             WriteLn;
             WriteLn(num1:0:2, ' ÷ ', num2:0:2, ' = ', result:0:2);
           End;
         End;
    End;
    
    WriteLn;
    Write('Continue? (y/n): ');
    ReadLn(continueCalc);
    WriteLn;
    
  Until (continueCalc <> 'y') And (continueCalc <> 'Y');
  
  WriteLn('Thank you for using the calculator!');
End.
