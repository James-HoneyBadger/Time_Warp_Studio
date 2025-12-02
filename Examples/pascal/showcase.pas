program ShowcasePascal;
{
  Time Warp Pascal Showcase
  Demonstrates: structured program, functions, arrays, user input, looping
}

uses SysUtils;

const
  MAX = 10;

type
  TIntArray = array[1..MAX] of Integer;

var
  nums: TIntArray;
  i, sum, avg: Integer;

function ReadInt(prompt: string): Integer;
var
  s: string;
  v: Integer;
begin
  Write(prompt);
  ReadLn(s);
  Val(s, v, i);
  ReadInt := v;
end;

procedure FillAndShow(var a: TIntArray);
var
  i: Integer;
begin
  for i := 1 to MAX do
  begin
    a[i] := Random(100) + 1;
    WriteLn('Item ', i, ': ', a[i]);
  end;
end;

function SumArray(const a: TIntArray): Integer;
var
  i, s: Integer;
begin
  s := 0;
  for i := 1 to MAX do s := s + a[i];
  SumArray := s;
end;

begin
  Randomize;
  WriteLn('Time Warp Pascal Showcase');
  FillAndShow(nums);
  sum := SumArray(nums);
  avg := sum div MAX;
  WriteLn('Sum = ', sum, ' Average = ', avg);
  i := ReadInt('Enter a threshold to report numbers >= threshold: ');
  WriteLn('Numbers >= ', i, ':');
  for i := 1 to MAX do if nums[i] >= i then WriteLn('  ', nums[i]);
  WriteLn('Done.');
end.
