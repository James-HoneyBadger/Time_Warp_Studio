program FibonacciDemo;
var
  i, n, t1, t2, nextTerm: integer;

begin
  writeln('Fibonacci Series Demo');
  n := 10;
  t1 := 0;
  t2 := 1;
  
  writeln('First 10 terms:');
  
  for i := 1 to n do
  begin
    write(t1);
    write(' ');
    nextTerm := t1 + t2;
    t1 := t2;
    t2 := nextTerm;
  end;
  writeln('');
end.
