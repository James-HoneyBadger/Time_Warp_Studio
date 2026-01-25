10 REM ========================================
20 REM  Time Warp Studio — BASIC Showcase
30 REM  A large, feature-rich demo demonstrating many language features
40 REM  - Variables, strings, math, arrays
50 REM  - Input validation
60 REM  - Loops (FOR, WHILE), branching
70 REM  - Subroutines, modular code
80 REM  - Mini-games (Guessing, Math quiz), ASCII-art, simple drawing grid
90 REM ========================================

100 REM --- Initialization ---
110 DIM HITS(10)
120 REM RANDOMIZE not required — interpreter provides RND
130 LET GAME_COUNT = 0
140 LET TOTAL_SCORE = 0
150 LET PLAYER$ = ""

160 GOSUB 1000  : REM Welcome & Get Player Name

170 REM --- Main Menu ---
180 MENU:
190 PRINT
200 PRINT "BASIC SHOWCASE — Main Menu"
210 PRINT "  1) Play Guessing Game"
220 PRINT "  2) Math Challenge"
230 PRINT "  3) Draw Pattern (ASCII Canvas)"
240 PRINT "  4) Random Data Demo & Stats"
250 PRINT "  5) Display Credits and Exit"
260 INPUT "Choose an option (1-5): "; CHOICE
270 IF CHOICE < 1 THEN PRINT "Please choose a number 1..5" : GOTO 260
280 IF CHOICE > 5 THEN PRINT "Please choose a number 1..5" : GOTO 260

290 IF CHOICE = 1 THEN GOSUB 2000
300 IF CHOICE = 2 THEN GOSUB 3000
310 IF CHOICE = 3 THEN GOSUB 4000
320 IF CHOICE = 4 THEN GOSUB 5000
330 IF CHOICE = 5 THEN GOSUB 9000

340 GOTO 180

1000 REM --- Welcome / Get Player ---
1010 PRINT "\nWelcome to the Time Warp BASIC Showcase!";
1020 INPUT "  Enter your name: "; PLAYER$
1030 IF LEN(PLAYER$) = 0 THEN LET PLAYER$ = "Friend"
1040 PRINT "Hi "; PLAYER$; " — enjoy the demo!"
1050 RETURN

2000 REM === Guessing Game (improved) ===
2010 LET GAME_COUNT = GAME_COUNT + 1
2020 LET SECRET = INT(RND * 100) + 1
2030 LET GUESSES = 0
2040 PRINT "\nGUESSING GAME — I'm thinking of a number 1..100"
2050 REM Allow up to 10 tries with hints
2060 FOR TRY = 1 TO 10
2070   INPUT "Your guess: "; GUESS
2080   LET GUESSES = GUESSES + 1
2090   IF GUESS = SECRET THEN GOSUB 2100 : RETURN
2100   IF GUESS < SECRET THEN PRINT "  Too low!" : GOTO 2200
2110   IF GUESS > SECRET THEN PRINT "  Too high!" : GOTO 2200
2120 NEXT TRY
2130 PRINT "Out of tries — the number was "; SECRET
2140 RETURN

2200 REM --- after guess (continuation) ---
2210 IF GUESS <> SECRET THEN PRINT "  Keep trying — tries: "; GUESSES
2220 IF GUESS = SECRET THEN GOSUB 2300
2230 RETURN

2300 REM --- Correct guess processing ---
2310 PRINT "\n✅ Correct! You guessed it in "; GUESSES; " tries."
2320 LET SCORE = MAX(0, 11 - GUESSES) : REM better score for fewer tries
2330 LET TOTAL_SCORE = TOTAL_SCORE + SCORE
2340 PRINT "Score this round: "; SCORE; ", total score: "; TOTAL_SCORE
2350 REM store hit count
2360 LET HITS(GUESSES) = HITS(GUESSES) + 1
2370 RETURN

3000 REM === Math Challenge ===
3010 PRINT "\nMath Challenge — Multi-stage quiz"
3020 LET QS = 3
3030 LET SCORE = 0
3040 FOR I = 1 TO QS
3050   LET A = INT(RND * 100) + 1
3060   LET B = INT(RND * 20) + 1
3070   LET OP = INT(RND * 4) + 1
3080   IF OP = 1 THEN LET ANSWER = A + B : LET OP$ = "+"
3090   IF OP = 2 THEN LET ANSWER = A - B : LET OP$ = "-"
3100   IF OP = 3 THEN LET ANSWER = A * B : LET OP$ = "*"
3110   IF OP = 4 THEN LET ANSWER = INT(A / B) : LET OP$ = "/ (int)"
3120   INPUT "What is "; A; " "; OP$; " "; B; "? "; RESP
3130   IF RESP = ANSWER THEN PRINT "Correct!" : LET SCORE = SCORE + 1 ELSE PRINT "Incorrect — answer=", ANSWER
3140 NEXT I
3150 PRINT "Quiz complete — you scored "; SCORE; " out of "; QS
3160 LET TOTAL_SCORE = TOTAL_SCORE + SCORE
3170 RETURN

4000 REM === ASCII Canvas Drawing ===
4010 PRINT "\nASCII Canvas — Draw a pattern using simple loops"
4020 LET W = 40 : LET H = 10
4030 PRINT "Canvas: "; W; " x "; H
4040 PRINT "Enter a pattern type: 1=Checker, 2=Spiral, 3=Gradient";
4050 INPUT "Choice: "; PT
4060 IF PT < 1 OR PT > 3 THEN PRINT "Invalid choice" : RETURN
4070 REM Draw
4080 FOR Y = 1 TO H
4090   LINE$ = ""
4100   FOR X = 1 TO W
4110     IF PT = 1 THEN IF (X + Y) MOD 2 = 0 THEN LINE$ = LINE$ + "#" ELSE LINE$ = LINE$ + " "
4120     IF PT = 2 THEN IF (X - Y) MOD 2 = 0 THEN LINE$ = LINE$ + "." ELSE LINE$ = LINE$ + " "
4130     IF PT = 3 THEN LET C = (X * 255) / W : LINE$ = LINE$ + CHR$(32 + INT(C * 0.25))
4140   NEXT X
4150   PRINT LINE$
4160 NEXT Y
4170 RETURN

5000 REM === Random Data Demo & Stats ===
5010 PRINT "\nRandom data sample and summary statistics"
5020 FOR I = 1 TO 20
5030   LET N = INT(RND * 1000)
5040   LET A = I MOD 10 + 1
5050   LET HITS(A) = HITS(A) + 1
5060   PRINT USING "Item # ## -> ##"; I; N
5070 NEXT I
5080 REM display histogram
5090 PRINT "\nHistogram of last digit counts (1..10):"
5100 FOR I = 1 TO 10
5110   PRINT I; " -> "; STRING$(HITS(I), "*")
5120 NEXT I
5130 RETURN

9000 REM === Credits & Exit ===
9010 PRINT "\nTime Warp BASIC Showcase!"
9020 PRINT "Author: Time Warp Studio — Showcase Program"
9030 PRINT "Thanks for playing, "; PLAYER$; " — final score: "; TOTAL_SCORE
9040 END
