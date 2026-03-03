program HangmanGame;
{ =====================================================
  HANGMAN - Classic Word Guessing Game in Pascal
  Features: Word categories, difficulty levels,
  ASCII art gallows, score tracking, hints.
  A fully playable classic game!
  ===================================================== }

const
  MAX_WRONG = 7;
  WORD_COUNT = 20;

var
  Words: array[1..WORD_COUNT] of string[20];
  Clues: array[1..WORD_COUNT] of string[40];
  Secret: string;
  Clue: string;
  Guessed: array[1..26] of Boolean;
  WrongGuesses: Integer;
  CorrectGuesses: Integer;
  GoodGuess: Boolean;
  GameWon: Boolean;
  GameLost: Boolean;
  Letter: Char;
  Score: Integer;
  Round: Integer;
  PlayAgain: Boolean;
  I, J: Integer;
  Done: Boolean;
  WordLen: Integer;

procedure InitWords;
begin
  Words[1]  := 'PASCAL';     Clues[1]  := 'A structured programming language';
  Words[2]  := 'ALGORITHM';  Clues[2]  := 'Step-by-step problem solving method';
  Words[3]  := 'RECURSION';  Clues[3]  := 'A function that calls itself';
  Words[4]  := 'COMPILER';   Clues[4]  := 'Translates source code to machine code';
  Words[5]  := 'VARIABLE';   Clues[5]  := 'Named storage for a value';
  Words[6]  := 'KEYBOARD';   Clues[6]  := 'Input device with keys';
  Words[7]  := 'ELEPHANT';   Clues[7]  := 'Largest land mammal with a trunk';
  Words[8]  := 'SYMPHONY';   Clues[8]  := 'Orchestral musical composition';
  Words[9]  := 'PYTHON';     Clues[9]  := 'Snake or popular programming language';
  Words[10] := 'QUANTUM';    Clues[10] := 'Discrete unit in physics';
  Words[11] := 'SPECTRUM';   Clues[11] := 'Range of colors in light';
  Words[12] := 'ABSTRACT';   Clues[12] := 'Existing as an idea, not physical';
  Words[13] := 'DATABASE';   Clues[13] := 'Organized collection of data';
  Words[14] := 'NETWORK';    Clues[14] := 'Interconnected system of nodes';
  Words[15] := 'FORTRESS';   Clues[15] := 'A strong defensive structure';
  Words[16] := 'CRYSTAL';    Clues[16] := 'Regular atomic structure solid';
  Words[17] := 'VOLCANO';    Clues[17] := 'Mountain that erupts lava';
  Words[18] := 'PARADOX';    Clues[18] := 'Self-contradictory statement';
  Words[19] := 'MOLECULE';   Clues[19] := 'Smallest unit of a chemical compound';
  Words[20] := 'LABYRINTH';  Clues[20] := 'Complex maze or network of paths';
end;

procedure DrawGallows(Wrong: Integer);
begin
  WriteLn('  ╔════════════╗');
  WriteLn('  ║            |');
  if Wrong >= 1 then WriteLn('  ║            O') else WriteLn('  ║             ');
  if Wrong >= 3 then
  begin
    if Wrong >= 4 then WriteLn('  ║           \|/')
    else if Wrong >= 3 then WriteLn('  ║            |/')
    else WriteLn('  ║            | ');
  end else WriteLn('  ║             ');
  if Wrong >= 2 then WriteLn('  ║            |') else WriteLn('  ║             ');
  if Wrong >= 6 then WriteLn('  ║           / \')
  else if Wrong >= 5 then WriteLn('  ║           /  ')
  else WriteLn('  ║             ');
  WriteLn('  ║');
  WriteLn('  ╚══════╧══════╝');
  Write('  Lives left: ');
  for I := 1 to MAX_WRONG - Wrong do Write('♥ ');
  for I := 1 to Wrong do Write('  ');
  WriteLn;
end;

procedure DrawWord;
var i: Integer;
begin
  Write('  Word: ');
  for i := 1 to Length(Secret) do
  begin
    if Guessed[Ord(Secret[i]) - Ord('A') + 1] then
      Write(Secret[i], ' ')
    else
      Write('_ ');
  end;
  WriteLn;
end;

procedure DrawGuessed;
var
  i: Integer;
begin
  Write('  Guessed: ');
  for i := 1 to 26 do
    if Guessed[i] then Write(Chr(Ord('A') + i - 1), ' ');
  WriteLn;
end;

function CheckWin: Boolean;
var i: Integer;
begin
  CheckWin := True;
  for i := 1 to Length(Secret) do
    if not Guessed[Ord(Secret[i]) - Ord('A') + 1] then
    begin
      CheckWin := False;
      Exit;
    end;
end;

procedure PlayRound(WordNum: Integer);
begin
  { Initialize }
  Secret := Words[WordNum];
  Clue := Clues[WordNum];
  WrongGuesses := 0;
  CorrectGuesses := 0;
  for I := 1 to 26 do Guessed[I] := False;
  GameWon := False;
  GameLost := False;

  WriteLn;
  WriteLn('  *** ROUND ', Round, ' ***');
  WriteLn('  Word length: ', Length(Secret), ' letters');
  WriteLn('  Hint: ', Clue);
  WriteLn;

  repeat
    { Draw game state }
    DrawGallows(WrongGuesses);
    WriteLn;
    DrawWord;
    WriteLn;
    DrawGuessed;
    WriteLn;

    { Get guess }
    Write('  Enter a letter (or ? for hint): ');
    ReadLn(Letter);
    Letter := UpCase(Letter);

    if Letter = '?' then
    begin
      WriteLn('  Hint: The word relates to "', Copy(Clue, 1, 20), '"');
      continue;
    end;

    if (Letter < 'A') or (Letter > 'Z') then
    begin
      WriteLn('  Please enter a letter A-Z.');
      continue;
    end;

    I := Ord(Letter) - Ord('A') + 1;

    if Guessed[I] then
    begin
      WriteLn('  You already guessed "', Letter, '"!');
      continue;
    end;

    Guessed[I] := True;
    GoodGuess := False;

    for J := 1 to Length(Secret) do
      if Secret[J] = Letter then GoodGuess := True;

    if GoodGuess then
    begin
      WriteLn('  ✓ Correct! "', Letter, '" is in the word!');
      Inc(CorrectGuesses);
    end else begin
      WriteLn('  ✗ Wrong! "', Letter, '" is not in the word.');
      Inc(WrongGuesses);
    end;

    GameWon := CheckWin;
    GameLost := (WrongGuesses >= MAX_WRONG);

  until GameWon or GameLost;

  { End of round }
  DrawGallows(WrongGuesses);
  DrawWord;
  WriteLn;

  if GameWon then
  begin
    WriteLn('  ╔══════════════════════════╗');
    WriteLn('  ║   🎉 YOU WON! 🎉         ║');
    WriteLn('  ╚══════════════════════════╝');
    Score := Score + (MAX_WRONG - WrongGuesses) * 10;
    WriteLn('  Points this round: ', (MAX_WRONG - WrongGuesses) * 10);
  end else begin
    WriteLn('  ╔══════════════════════════╗');
    WriteLn('  ║   💀 YOU LOST!           ║');
    WriteLn('  ╚══════════════════════════╝');
    WriteLn('  The word was: ', Secret);
  end;
  WriteLn('  Total Score: ', Score);
end;

begin
  InitWords;
  Score := 0;
  Round := 0;

  WriteLn('╔══════════════════════════════════╗');
  WriteLn('║      PASCAL HANGMAN DELUXE       ║');
  WriteLn('║   Guess the word before hang!    ║');
  WriteLn('╚══════════════════════════════════╝');

  PlayAgain := True;
  while PlayAgain do
  begin
    Inc(Round);
    { Pick a random-ish word based on round }
    I := ((Round - 1) mod WORD_COUNT) + 1;
    PlayRound(I);

    WriteLn;
    Write('  Play again? (Y/N): ');
    ReadLn(Letter);
    PlayAgain := (UpCase(Letter) = 'Y');
  end;

  WriteLn;
  WriteLn('  ╔══════════════════════════════╗');
  WriteLn('  ║          FINAL SCORE         ║');
  WriteLn('  ║  Rounds played: ', Round:3, '         ║');
  WriteLn('  ║  Total score:   ', Score:3, '         ║');
  WriteLn('  ╚══════════════════════════════╝');
  WriteLn('  Thanks for playing Hangman!');
end.
