{ ============================================================ }
{ GRADE BOOK - Pascal Language Showcase                        }
{ Student analysis, statistics, grade distribution             }
{ Time Warp Studio - Pascal Language Demo                      }
{ ============================================================ }

program GradeBook;

const
  N = 20;
  PASS_SCORE = 60;

var
  Names   : array[1..20] of string;
  Scores  : array[1..20] of integer;
  Letters : array[1..20] of char;
  GPAs    : array[1..20] of real;
  i, j           : integer;
  TempScore      : integer;
  TempName       : string;
  TempGPA        : real;
  TempLetter     : char;
  Total, Passing, Failing : integer;
  Avg, Highest, Lowest, Variance, StdDev, TotalGPA : real;
  swapped        : boolean;
  CountA, CountB, CountC, CountD, CountF : integer;
  bar_len, k     : integer;
  score_val      : integer;

begin
  writeln("============================================================");
  writeln("  GRADE BOOK - Pascal Language Showcase");
  writeln("  Student analysis, statistics, grade distribution");
  writeln("============================================================");
  writeln;

  Names[1]  := "Alice Chen";
  Scores[1] := 97;
  Names[2]  := "Bob Martinez";
  Scores[2] := 84;
  Names[3]  := "Carol Williams";
  Scores[3] := 91;
  Names[4]  := "David Kim";
  Scores[4] := 72;
  Names[5]  := "Emma Davis";
  Scores[5] := 88;
  Names[6]  := "Frank Brown";
  Scores[6] := 63;
  Names[7]  := "Grace Taylor";
  Scores[7] := 79;
  Names[8]  := "Henry Wilson";
  Scores[8] := 95;
  Names[9]  := "Isabella Lee";
  Scores[9] := 82;
  Names[10] := "James Anderson";
  Scores[10] := 55;
  Names[11] := "Kate Johnson";
  Scores[11] := 76;
  Names[12] := "Liam Thomas";
  Scores[12] := 89;
  Names[13] := "Maya Jackson";
  Scores[13] := 68;
  Names[14] := "Noah White";
  Scores[14] := 93;
  Names[15] := "Olivia Harris";
  Scores[15] := 71;
  Names[16] := "Peter Clark";
  Scores[16] := 58;
  Names[17] := "Quinn Lewis";
  Scores[17] := 86;
  Names[18] := "Rose Walker";
  Scores[18] := 77;
  Names[19] := "Sam Hall";
  Scores[19] := 62;
  Names[20] := "Tara Allen";
  Scores[20] := 98;

  for i := 1 to N do begin
    score_val := Scores[i];
    if score_val >= 90 then begin
      Letters[i] := "A";
      GPAs[i] := 4.0;
    end
    else if score_val >= 80 then begin
      Letters[i] := "B";
      GPAs[i] := 3.0;
    end
    else if score_val >= 70 then begin
      Letters[i] := "C";
      GPAs[i] := 2.0;
    end
    else if score_val >= 60 then begin
      Letters[i] := "D";
      GPAs[i] := 1.0;
    end
    else begin
      Letters[i] := "F";
      GPAs[i] := 0.0;
    end;
  end;

  writeln("SECTION 1: COMPLETE ROSTER");
  writeln("------------------------------------------------------------");
  writeln("  #   Name              Score  Letter  GPA");
  writeln("  --  ----------------  -----  ------  ---");
  for i := 1 to N do
    writeln("  ", i, "   ", Names[i], "  ", Scores[i], "    ", Letters[i], "   ", GPAs[i]);

  writeln;
  writeln("SECTION 2: CLASS STATISTICS");
  writeln("------------------------------------------------------------");

  Total   := 0;
  Highest := Scores[1];
  Lowest  := Scores[1];
  Passing := 0;

  for i := 1 to N do begin
    Total := Total + Scores[i];
    if Scores[i] > Highest then Highest := Scores[i];
    if Scores[i] < Lowest  then Lowest  := Scores[i];
    if Scores[i] >= PASS_SCORE then Passing := Passing + 1;
  end;

  Avg    := Total / N;
  Failing := N - Passing;

  Variance := 0;
  for i := 1 to N do
    Variance := Variance + (Scores[i] - Avg) * (Scores[i] - Avg);
  Variance := Variance / N;
  StdDev   := sqrt(Variance);

  writeln("  Class size:       ", N);
  writeln("  Average score:    ", Avg);
  writeln("  Highest score:    ", Highest);
  writeln("  Lowest score:     ", Lowest);
  writeln("  Std deviation:    ", StdDev);
  writeln("  Passing (>=60):   ", Passing);
  writeln("  Failing (<60):    ", Failing);

  repeat
    swapped := false;
    for i := 1 to N - 1 do begin
      if Scores[i] < Scores[i+1] then begin
        TempScore    := Scores[i];
        Scores[i]    := Scores[i+1];
        Scores[i+1]  := TempScore;
        TempName     := Names[i];
        Names[i]     := Names[i+1];
        Names[i+1]   := TempName;
        TempLetter   := Letters[i];
        Letters[i]   := Letters[i+1];
        Letters[i+1] := TempLetter;
        TempGPA      := GPAs[i];
        GPAs[i]      := GPAs[i+1];
        GPAs[i+1]    := TempGPA;
        swapped := true;
      end;
    end;
  until not swapped;

  writeln;
  writeln("SECTION 3: TOP 5 AND BOTTOM 5 (sorted)");
  writeln("------------------------------------------------------------");
  for i := 1 to 5 do
    writeln("  ", i, "  ", Names[i], "  score=", Scores[i], "  ", Letters[i], "  GPA=", GPAs[i]);
  writeln;
  for i := N downto N - 4 do
    writeln("  ", N-i+1, "  ", Names[i], "  score=", Scores[i], "  ", Letters[i], "  GPA=", GPAs[i]);

  writeln;
  writeln("SECTION 4: GRADE DISTRIBUTION");
  writeln("------------------------------------------------------------");

  CountA := 0;
  CountB := 0;
  CountC := 0;
  CountD := 0;
  CountF := 0;

  for i := 1 to N do begin
    if Letters[i] = "A" then CountA := CountA + 1;
    if Letters[i] = "B" then CountB := CountB + 1;
    if Letters[i] = "C" then CountC := CountC + 1;
    if Letters[i] = "D" then CountD := CountD + 1;
    if Letters[i] = "F" then CountF := CountF + 1;
  end;

  write("  A (90-100): ");
  bar_len := CountA * 3;
  for k := 1 to bar_len do write("#");
  writeln(" (", CountA, ")");

  write("  B (80-89):  ");
  bar_len := CountB * 3;
  for k := 1 to bar_len do write("#");
  writeln(" (", CountB, ")");

  write("  C (70-79):  ");
  bar_len := CountC * 3;
  for k := 1 to bar_len do write("#");
  writeln(" (", CountC, ")");

  write("  D (60-69):  ");
  bar_len := CountD * 3;
  for k := 1 to bar_len do write("#");
  writeln(" (", CountD, ")");

  write("  F (0-59):   ");
  bar_len := CountF * 3;
  for k := 1 to bar_len do write("#");
  writeln(" (", CountF, ")");

  writeln;
  writeln("SECTION 5: GPA REPORT");
  writeln("------------------------------------------------------------");
  TotalGPA := 0;
  for i := 1 to N do
    TotalGPA := TotalGPA + GPAs[i];
  writeln("  Class average GPA: ", TotalGPA / N);

  if TotalGPA / N >= 3.5 then
  else if TotalGPA / N >= 3.0 then
    writeln("  Good standing overall")
  else
    writeln("  Academic support recommended for some students");

  writeln;
  writeln("============================================================");
  writeln("  Grade book analysis complete - ", N, " students processed");
  writeln("============================================================");
end.
