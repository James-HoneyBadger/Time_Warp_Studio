program NumberGuessingGame;
{ Interactive number guessing game demonstrating
  Pascal control structures, random numbers, and I/O }

var
  secret, guess, attempts, maxNum, score, round: integer;
  playing: boolean;
  response: string;

procedure PrintBanner;
begin
  writeln('╔══════════════════════════════════════════╗');
  writeln('║     🎯 Number Guessing Game 🎯          ║');
  writeln('║     Guess the number 1-100              ║');
  writeln('╚══════════════════════════════════════════╝');
  writeln;
end;

procedure PrintHint(guess, secret: integer);
var
  diff: integer;
begin
  diff := abs(guess - secret);
  if diff <= 3 then
    writeln('🔥 Very hot! You are within 3!')
  else if diff <= 10 then
    writeln('♨️  Warm — getting closer!')
  else if diff <= 25 then
    writeln('❄️  Cold — still far away.')
  else
    writeln('🧊 Freezing! Way off!');

  if guess < secret then
    writeln('📈 Try higher!')
  else
    writeln('📉 Try lower!');
end;

function ScoreRound(attempts: integer): integer;
begin
  if attempts = 1 then
    ScoreRound := 100
  else if attempts <= 3 then
    ScoreRound := 75
  else if attempts <= 5 then
    ScoreRound := 50
  else if attempts <= 7 then
    ScoreRound := 25
  else
    ScoreRound := 10;
end;

begin
  PrintBanner;
  score := 0;
  playing := true;
  round := 0;
  maxNum := 100;

  while playing do
  begin
    round := round + 1;
    secret := random(maxNum) + 1;
    attempts := 0;
    guess := 0;

    writeln('━━━ Round ', round, ' ━━━');
    writeln('I am thinking of a number between 1 and ', maxNum, '.');
    writeln;

    while guess <> secret do
    begin
      attempts := attempts + 1;
      write('Attempt ', attempts, ' — Your guess: ');
      readln(guess);

      if guess = secret then
      begin
        writeln;
        writeln('🎉 Correct! You got it in ', attempts, ' attempt(s)!');
        score := score + ScoreRound(attempts);
        writeln('Round score: +', ScoreRound(attempts), '  Total: ', score);
      end
      else
        PrintHint(guess, secret);

      writeln;
    end;

    writeln('Play again? (yes/no): ');
    readln(response);
    if (response <> 'yes') and (response <> 'y') then
      playing := false;
  end;

  writeln;
  writeln('╔══════════════════════════════════════════╗');
  writeln('║            📊 Final Results             ║');
  writeln('╚══════════════════════════════════════════╝');
  writeln('  Rounds played: ', round);
  writeln('  Total score:   ', score);
  writeln;
  writeln('Thanks for playing! 🎮');
end.
