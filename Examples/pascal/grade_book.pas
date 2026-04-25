{ ============================================================ }
{ GRADE BOOK — Pascal Language Showcase                         }
{ Student records, grade analysis, statistics, histogram        }
{ Time Warp Studio — Pascal Language Demo                       }
{ ============================================================ }

program GradeBook;

const
  MAX_STUDENTS = 20;
  PASS_SCORE   = 60;

type
  TGrade = record
    Name    : string;
    Score   : integer;
    Letter  : char;
    GPA     : real;
  end;

var
  Students : array[1..MAX_STUDENTS] of TGrade;
  N        : integer;
  i, j     : integer;
  Total    : real;
  Avg      : real;
  Highest  : integer;
  Lowest   : integer;
  Variance : real;
  StdDev   : real;
  Passing  : integer;
  Failing  : integer;
  Temp     : TGrade;

{ ===== Procedures ===== }

procedure AssignLetter(var G : TGrade);
begin
  if G.Score >= 97 then begin G.Letter := 'A'; G.GPA := 4.0 end
  else if G.Score >= 93 then begin G.Letter := 'A'; G.GPA := 4.0 end
  else if G.Score >= 90 then begin G.Letter := 'A'; G.GPA := 3.7 end
  else if G.Score >= 87 then begin G.Letter := 'B'; G.GPA := 3.3 end
  else if G.Score >= 83 then begin G.Letter := 'B'; G.GPA := 3.0 end
  else if G.Score >= 80 then begin G.Letter := 'B'; G.GPA := 2.7 end
  else if G.Score >= 77 then begin G.Letter := 'C'; G.GPA := 2.3 end
  else if G.Score >= 73 then begin G.Letter := 'C'; G.GPA := 2.0 end
  else if G.Score >= 70 then begin G.Letter := 'C'; G.GPA := 1.7 end
  else if G.Score >= 67 then begin G.Letter := 'D'; G.GPA := 1.3 end
  else if G.Score >= 60 then begin G.Letter := 'D'; G.GPA := 1.0 end
  else begin G.Letter := 'F'; G.GPA := 0.0 end;
end;

procedure PrintBar(count, max_count : integer; width : integer);
var
  k, bar_len : integer;
begin
  bar_len := (count * width) div max_count;
  for k := 1 to bar_len do write('█');
  for k := bar_len + 1 to width do write(' ');
end;

procedure SortByScore;
{ Bubble sort descending }
var
  swapped : boolean;
begin
  repeat
    swapped := false;
    for i := 1 to N - 1 do
      if Students[i].Score < Students[i+1].Score then begin
        Temp := Students[i];
        Students[i] := Students[i+1];
        Students[i+1] := Temp;
        swapped := true;
      end;
  until not swapped;
end;

{ ===== Main Program ===== }

begin
  writeln('============================================================');
  writeln('  GRADE BOOK — Pascal Language Showcase');
  writeln('  Student records, statistics, distribution analysis');
  writeln('============================================================');
  writeln;

  { ---- Load student data ---- }
  N := 20;

  Students[1].Name  := 'Alice Chen';       Students[1].Score  := 97;
  Students[2].Name  := 'Bob Martinez';     Students[2].Score  := 84;
  Students[3].Name  := 'Carol Williams';   Students[3].Score  := 91;
  Students[4].Name  := 'David Kim';        Students[4].Score  := 72;
  Students[5].Name  := 'Emma Davis';       Students[5].Score  := 88;
  Students[6].Name  := 'Frank Brown';      Students[6].Score  := 63;
  Students[7].Name  := 'Grace Taylor';     Students[7].Score  := 79;
  Students[8].Name  := 'Henry Wilson';     Students[8].Score  := 95;
  Students[9].Name  := 'Isabella Lee';     Students[9].Score  := 82;
  Students[10].Name := 'James Anderson';   Students[10].Score := 55;
  Students[11].Name := 'Kate Johnson';     Students[11].Score := 76;
  Students[12].Name := 'Liam Thomas';      Students[12].Score := 89;
  Students[13].Name := 'Maya Jackson';     Students[13].Score := 68;
  Students[14].Name := 'Noah White';       Students[14].Score := 93;
  Students[15].Name := 'Olivia Harris';    Students[15].Score := 71;
  Students[16].Name := 'Peter Clark';      Students[16].Score := 58;
  Students[17].Name := 'Quinn Lewis';      Students[17].Score := 86;
  Students[18].Name := 'Rose Walker';      Students[18].Score := 77;
  Students[19].Name := 'Sam Hall';         Students[19].Score := 62;
  Students[20].Name := 'Tara Allen';       Students[20].Score := 98;

  { Assign letter grades }
  for i := 1 to N do
    AssignLetter(Students[i]);

  { ---- Section 1: Full grade roster ---- }
  writeln('SECTION 1: COMPLETE ROSTER');
  writeln('------------------------------------------------------------');
  writeln('  #   Name                 Score   Letter   GPA');
  writeln('  --  -------------------  ------  ------  -----');
  for i := 1 to N do
    writeln('  ', i:2, '  ', Students[i].Name:-20, ' ',
            Students[i].Score:5, '     ', Students[i].Letter,
            '      ', Students[i].GPA:4:1);

  { ---- Section 2: Statistics ---- }
  writeln;
  writeln('SECTION 2: CLASS STATISTICS');
  writeln('------------------------------------------------------------');

  Total := 0;
  Highest := Students[1].Score;
  Lowest  := Students[1].Score;
  Passing := 0;

  for i := 1 to N do begin
    Total := Total + Students[i].Score;
    if Students[i].Score > Highest then Highest := Students[i].Score;
    if Students[i].Score < Lowest  then Lowest  := Students[i].Score;
    if Students[i].Score >= PASS_SCORE then Passing := Passing + 1;
  end;

  Avg := Total / N;
  Failing := N - Passing;

  { Calculate variance and std deviation }
  Variance := 0;
  for i := 1 to N do
    Variance := Variance + (Students[i].Score - Avg) * (Students[i].Score - Avg);
  Variance := Variance / N;
  StdDev   := sqrt(Variance);

  writeln('  Class size:       ', N);
  writeln('  Average score:    ', Avg:6:2);
  writeln('  Highest score:    ', Highest);
  writeln('  Lowest score:     ', Lowest);
  writeln('  Range:            ', Highest - Lowest);
  writeln('  Std deviation:    ', StdDev:6:2);
  writeln('  Variance:         ', Variance:6:2);
  writeln('  Passing (>=60):   ', Passing, ' (', Round(Passing*100/N), '%)');
  writeln('  Failing (<60):    ', Failing, ' (', Round(Failing*100/N), '%)');

  { Median }
  SortByScore;
  if N mod 2 = 1
    then writeln('  Median score:     ', Students[(N+1) div 2].Score)
    else writeln('  Median score:     ',
        (Students[N div 2].Score + Students[N div 2 + 1].Score) / 2.0:4:1);

  { ---- Section 3: Top 5 and Bottom 5 ---- }
  writeln;
  writeln('SECTION 3: TOP 5 STUDENTS');
  writeln('------------------------------------------------------------');
  writeln('  Rank  Name                 Score   Letter  GPA');
  writeln('  ----  -------------------  ------  ------  ---');
  for i := 1 to 5 do
    writeln('   ', i, '    ', Students[i].Name:-20, ' ',
            Students[i].Score:5, '     ', Students[i].Letter,
            '     ', Students[i].GPA:3:1);

  writeln;
  writeln('  --- Bottom 5 Students ---');
  writeln('  Rank  Name                 Score   Letter  GPA');
  writeln('  ----  -------------------  ------  ------  ---');
  for i := N downto N - 4 do
    writeln('  ', N-i+1:2, '    ', Students[i].Name:-20, ' ',
            Students[i].Score:5, '     ', Students[i].Letter,
            '     ', Students[i].GPA:3:1);

  { ---- Section 4: Grade distribution histogram ---- }
  writeln;
  writeln('SECTION 4: GRADE DISTRIBUTION');
  writeln('------------------------------------------------------------');

  var CountA, CountB, CountC, CountD, CountF : integer;
  CountA := 0; CountB := 0; CountC := 0; CountD := 0; CountF := 0;

  for i := 1 to N do
    case Students[i].Letter of
      'A': CountA := CountA + 1;
      'B': CountB := CountB + 1;
      'C': CountC := CountC + 1;
      'D': CountD := CountD + 1;
      'F': CountF := CountF + 1;
    end;

  writeln;
  write('  A (90-100):  '); PrintBar(CountA, N, 30); writeln(' ', CountA);
  write('  B (80-89):   '); PrintBar(CountB, N, 30); writeln(' ', CountB);
  write('  C (70-79):   '); PrintBar(CountC, N, 30); writeln(' ', CountC);
  write('  D (60-69):   '); PrintBar(CountD, N, 30); writeln(' ', CountD);
  write('  F (0-59):    '); PrintBar(CountF, N, 30); writeln(' ', CountF);

  { ---- Section 5: Score distribution by 10s ---- }
  writeln;
  writeln('SECTION 5: SCORE HISTOGRAM (by 10s)');
  writeln('------------------------------------------------------------');

  var Bin : array[0..9] of integer;
  for i := 0 to 9 do Bin[i] := 0;
  for i := 1 to N do begin
    j := Students[i].Score div 10;
    if j > 9 then j := 9;
    Bin[j] := Bin[j] + 1;
  end;

  for i := 0 to 9 do begin
    write('  ', i*10, '-', i*10+9, ': ');
    write('  ');
    PrintBar(Bin[i], N, 25);
    writeln(' ', Bin[i]);
  end;

  { ---- Section 6: GPA Summary ---- }
  writeln;
  writeln('SECTION 6: GPA REPORT');
  writeln('------------------------------------------------------------');
  var TotalGPA : real;
  TotalGPA := 0;
  for i := 1 to N do TotalGPA := TotalGPA + Students[i].GPA;
  writeln('  Class average GPA: ', TotalGPA / N:4:2, ' / 4.00');
  if TotalGPA / N >= 3.5 then writeln('  Dean''s List eligible: check individual GPAs')
  else if TotalGPA / N >= 3.0 then writeln('  Good standing overall')
  else writeln('  Academic support recommended for some students');

  writeln;
  writeln('============================================================');
  writeln('  Grade book analysis complete — ', N, ' students processed');
  writeln('============================================================');
end.
