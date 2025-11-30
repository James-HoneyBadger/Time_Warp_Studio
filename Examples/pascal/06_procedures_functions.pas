{
  Procedures and Functions
  
  Demonstrates:
  - Procedure declarations (no return value)
  - Function declarations (with return value)
  - Parameters (value and var)
  - Local vs global variables
}

Program ProceduresAndFunctions;
Var
  num, result: Integer;
  tempC, tempF: Real;

{ Function: Returns the square of a number }
Function Square(x: Integer): Integer;
Begin
  Square := x * x;
End;

{ Function: Returns maximum of two numbers }
Function Max(a, b: Integer): Integer;
Begin
  If a > b Then
    Max := a
  Else
    Max := b;
End;

{ Function: Convert Celsius to Fahrenheit }
Function CelsiusToFahrenheit(c: Real): Real;
Begin
  CelsiusToFahrenheit := (c * 9.0 / 5.0) + 32.0;
End;

{ Procedure: Display a banner }
Procedure PrintBanner(message: String);
Var
  i: Integer;
Begin
  { Top border }
  For i := 1 To Length(message) + 4 Do
    Write('=');
  WriteLn;
  
  { Message }
  WriteLn('| ', message, ' |');
  
  { Bottom border }
  For i := 1 To Length(message) + 4 Do
    Write('=');
  WriteLn;
End;

{ Function: Calculate factorial }
Function Factorial(n: Integer): Integer;
Var
  i, result: Integer;
Begin
  result := 1;
  For i := 2 To n Do
    result := result * i;
  Factorial := result;
End;

{ Main program }
Begin
  WriteLn('=== Function Examples ===');
  WriteLn;
  
  { Using square function }
  num := 5;
  result := Square(num);
  WriteLn('Square of ', num, ' = ', result);
  
  { Using max function }
  WriteLn('Max of 15 and 23 = ', Max(15, 23));
  
  { Temperature conversion }
  WriteLn;
  tempC := 25.0;
  tempF := CelsiusToFahrenheit(tempC);
  WriteLn(tempC:0:1, '°C = ', tempF:0:1, '°F');
  
  { Using void procedure }
  WriteLn;
  PrintBanner('Welcome to Pascal!');
  
  { Factorial calculations }
  WriteLn;
  WriteLn('=== Factorials ===');
  For num := 1 To 10 Do
    WriteLn(num, '! = ', Factorial(num));
End.
