PROGRAM PascalDemo;
VAR
  i, total : INTEGER;
  n1, n2, sum : INTEGER;

BEGIN
  WRITELN('Time Warp Studio - Pascal Demo');
  WRITELN;
  
  WRITELN('1. Arithmetic:');
  n1 := 10;
  n2 := 20;
  sum := n1 + n2;
  WRITELN('10 + 20 = ', sum);
  WRITELN;
  
  WRITELN('2. Loops:');
  WRITE('Counting: ');
  FOR i := 1 TO 5 DO
    WRITE(i, ' ');
  WRITELN;
  WRITELN;
  
  WRITELN('3. Summation:');
  total := 0;
  FOR i := 1 TO 5 DO
    total := total + i;
  WRITELN('Sum of 1-5: ', total);
  WRITELN;
  
  WRITELN('4. Conditional:');
  IF total > 10 THEN
    WRITELN('Sum is greater than 10')
  ELSE
    WRITELN('Sum is 10 or less');
  WRITELN;
  
  WRITELN('Demo complete!');
END.
