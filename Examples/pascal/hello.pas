program HelloPascal;

var x, y, i, total: integer;
  name: string;

procedure greet(who: string);
begin
  writeln('Hello, ', who, '!');
end;

function double(n: integer): integer;
begin
  double := n * 2;
end;

begin
  writeln('===== HELLO WORLD =====');
  writeln('Hello from Pascal!');
  writeln('===== VARIABLES =====');
  x := 10;
  y := 3;
  name := 'Pascal';
  writeln('x = ', x);
  writeln('y = ', y);
  writeln('name = ', name);
  writeln('===== ARITHMETIC =====');
  writeln('x + y = ', x + y);
  writeln('x * y = ', x * y);
  writeln('x div y = ', x div y);
  writeln('x mod y = ', x mod y);
  writeln('===== STRINGS =====');
  writeln('length: ', length(name));
  writeln('upper: ', upcase('hello'));
  writeln('===== FOR LOOP =====');
  total := 0;
  for i := 1 to 5 do
  begin
    writeln('i = ', i);
    total := total + i;
  end;
  writeln('total = ', total);
  writeln('===== WHILE LOOP =====');
  i := 1;
  while i <= 3 do
  begin
    writeln('while i = ', i);
    i := i + 1;
  end;
  writeln('===== CONDITIONALS =====');
  if x > 5 then
    writeln('x is greater than 5')
  else
    writeln('x is not greater than 5');
  writeln('===== CASE =====');
  case y of
    1: writeln('one');
    2: writeln('two');
    3: writeln('three');
  end;
  writeln('===== FUNCTIONS =====');
  writeln('double(5) = ', double(5));
  writeln('double(7) = ', double(7));
  writeln('===== PROCEDURES =====');
  greet('World');
  greet('Pascal');
  writeln('===== MATH =====');
  writeln('abs(-5) = ', abs(-5));
  writeln('sqr(7) = ', sqr(7));
  writeln('===== DONE =====');
end.
