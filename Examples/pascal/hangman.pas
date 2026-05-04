program HangmanDemo;
{ =====================================================
  HANGMAN - Pascal Word Guessing Game Demo
  Demonstrates: arrays, procedures, string handling,
  boolean logic, and ASCII art in Pascal.
  Simulates a complete game with preset guesses.
  ===================================================== }

const
  MAX_WRONG  = 6;
  SECRET_LEN = 6;
  NUM_GUESSES = 9;

var
  Secret      : string;
  Guessed     : array[1..26] of Boolean;
  WrongCount  : Integer;
  GoodCount   : Integer;
  Guesses     : array[1..NUM_GUESSES] of Char;
  i, j, g     : Integer;
  IsGood      : Boolean;
  GameWon     : Boolean;
  GameLost    : Boolean;
  C           : Char;
  LetterIdx   : Integer;

procedure DrawGallows(Wrong: Integer);
begin
  WriteLn('  +------+');
  if Wrong >= 1 then WriteLn('  |      O') else WriteLn('  |       ');
  if Wrong >= 3 then WriteLn('  |     /|\') else
    if Wrong >= 2 then WriteLn('  |      |') else WriteLn('  |       ');
  if Wrong >= 5 then WriteLn('  |     / \') else
    if Wrong >= 4 then WriteLn('  |     /  ') else WriteLn('  |       ');
  WriteLn('  |');
  WriteLn('  ========');
  Write('  Hearts: ');
  for i := 1 to MAX_WRONG - Wrong do Write('<3 ');
  WriteLn;
end;

procedure ShowWord;
begin
  Write('  Word: ');
  for i := 1 to SECRET_LEN do
  begin
    LetterIdx := Ord(Secret[i]) - Ord('A') + 1;
    if Guessed[LetterIdx] then
      Write(Secret[i], ' ')
    else
      Write('_ ');
  end;
  WriteLn;
end;

procedure ShowGuessed;
var
  k: Integer;
  HasAny: Boolean;
begin
  HasAny := False;
  Write('  Guessed: ');
  for k := 1 to 26 do
    if Guessed[k] then
    begin
      Write(Chr(Ord('A') + k - 1), ' ');
      HasAny := True;
    end;
  if not HasAny then Write('(none)');
  WriteLn;
end;

function CheckWin: Boolean;
var
  k: Integer;
  All: Boolean;
begin
  All := True;
  for k := 1 to SECRET_LEN do
  begin
    LetterIdx := Ord(Secret[k]) - Ord('A') + 1;
    if not Guessed[LetterIdx] then
      All := False;
  end;
  CheckWin := All;
end;

begin
  Secret := 'PASCAL';
  WriteLn('======================================');
  WriteLn('      PASCAL HANGMAN DEMO');
  WriteLn('   Simulated game: word = PASCAL');
  WriteLn('======================================');
  WriteLn;
  WriteLn('  Word length: ', SECRET_LEN, ' letters');
  WriteLn('  Hint: A structured programming language');
  WriteLn;

  for i := 1 to 26 do
    Guessed[i] := False;

  Guesses[1] := 'X';
  Guesses[2] := 'Z';
  Guesses[3] := 'P';
  Guesses[4] := 'A';
  Guesses[5] := 'S';
  Guesses[6] := 'C';
  Guesses[7] := 'Q';
  Guesses[8] := 'L';
  Guesses[9] := 'K';

  WrongCount := 0;
  GoodCount  := 0;
  GameWon    := False;
  GameLost   := False;

  g := 1;
  while (g <= NUM_GUESSES) and (not GameWon) and (not GameLost) do
  begin
    C := Guesses[g];
    WriteLn('  --- Guess: ', C, ' ---');

    LetterIdx := Ord(C) - Ord('A') + 1;
    Guessed[LetterIdx] := True;

    IsGood := False;
    for j := 1 to SECRET_LEN do
    begin
      if Secret[j] = C then
        IsGood := True;
    end;

    if IsGood then
    begin
      WriteLn('  CORRECT! "', C, '" is in the word.');
      GoodCount := GoodCount + 1;
    end else begin
      WriteLn('  WRONG!   "', C, '" is not in the word.');
      WrongCount := WrongCount + 1;
    end;

    DrawGallows(WrongCount);
    ShowWord;
    ShowGuessed;
    WriteLn;

    GameWon  := CheckWin;
    GameLost := (WrongCount >= MAX_WRONG);
    g := g + 1;
  end;

  if GameWon then
  begin
    WriteLn('  ============================');
    WriteLn('       ** YOU WIN! **');
    WriteLn('  ============================');
    WriteLn('  Correct guesses: ', GoodCount);
    WriteLn('  Wrong guesses:   ', WrongCount);
    WriteLn('  Score: ', (MAX_WRONG - WrongCount) * 10, ' points');
  end else begin
    WriteLn('  ============================');
    WriteLn('       ** GAME OVER **');
    WriteLn('  ============================');
    WriteLn('  The word was: ', Secret);
  end;

  WriteLn;
  WriteLn('  === Gallows Gallery (0 to 6 wrong) ===');
  WriteLn;
  for i := 0 to MAX_WRONG do
  begin
    WriteLn('  Wrong = ', i, ':');
    DrawGallows(i);
    WriteLn;
  end;

  WriteLn('  Demo complete!');
end.
