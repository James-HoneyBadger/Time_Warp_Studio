\ ============================================================
\ CELLULAR AUTOMATA — Forth Language Showcase
\ Elementary 1D automata: Rule 30, Rule 110, Rule 90
\ Time Warp Studio — Forth Language Demo
\ ============================================================

\ --- Constants ---
50 CONSTANT WIDTH
30 CONSTANT ROWS
0 CONSTANT DEAD
1 CONSTANT ALIVE

\ --- Tape storage (current and next generations) ---
CREATE TAPE    WIDTH ALLOT
CREATE NEXTTAPE WIDTH ALLOT

\ --- Initialize tape to all zeros ---
: CLEAR-TAPE ( -- )
  WIDTH 0 DO  0 TAPE I + C!  LOOP ;

\ --- Set a single cell in TAPE ---
: TAPE! ( val idx -- ) TAPE + C! ;

\ --- Get a cell from TAPE ---
: TAPE@ ( idx -- val )
  DUP 0 < IF DROP 0 EXIT THEN
  DUP WIDTH >= IF DROP 0 EXIT THEN
  TAPE + C@ ;

\ --- Get a cell from NEXTTAPE ---
: NEXT! ( val idx -- ) NEXTTAPE + C! ;

\ --- Apply a rule number to three bits: left, center, right ---
\ The rule maps the 3-bit pattern (L C R) to 0 or 1
: APPLY-RULE ( left center right rule -- newval )
  >R                      \ save rule
  2 * +                   \ center * 2
  2 * SWAP + SWAP +       \ left*4 + center*2 + right = pattern 0-7
  \ Note: pattern bits: 4=left 2=center 1=right
  R> SWAP               \ rule pattern
  RSHIFT 1 AND ;        \ (rule >> pattern) & 1

\ --- Wait, need to fix the shift order ---
\ pattern = left*4 + center*2 + right
: COMPUTE-PATTERN ( left center right -- pattern )
  ROT 4 * ROT 2 * + + ;

\ --- Apply rule to single cell position ---
: STEP-CELL ( pos rule -- )
  OVER >R              \ save pos to return stack; stack: pos rule
  SWAP                 \ rule pos
  DUP 1 - TAPE@       \ rule pos left
  OVER TAPE@           \ rule pos left center
  OVER 1 + TAPE@       \ rule pos left center right
  COMPUTE-PATTERN      \ rule pos pattern
  NIP                  \ rule pattern  (drop pos)
  RSHIFT 1 AND         \ newval  (rule >> pattern & 1)
  R> NEXT! ;           \ NEXT![pos] = newval

\ --- Advance one generation using given rule ---
: STEP-GENERATION ( rule -- )
  WIDTH 0 DO
    I OVER STEP-CELL
  LOOP
  DROP
  \ copy NEXTTAPE back to TAPE
  WIDTH 0 DO
    NEXTTAPE I + C@  TAPE I + C!
  LOOP ;

\ --- Print current generation ---
: PRINT-TAPE ( -- )
  WIDTH 0 DO
    TAPE I + C@ IF
      [CHAR] # EMIT
    ELSE
      [CHAR] . EMIT
    THEN
  LOOP
  CR ;

\ --- Seed: single cell in the center ---
: SEED-CENTER ( -- )
  CLEAR-TAPE
  ALIVE WIDTH 2 / TAPE! ;

\ --- Seed: alternating pattern ---
: SEED-ALTERNATE ( -- )
  CLEAR-TAPE
  WIDTH 0 DO
    I 2 MOD 0= IF ALIVE I TAPE! THEN
  LOOP ;

\ --- Seed: random-ish (use XOR pattern) ---
: SEED-XOR ( -- )
  CLEAR-TAPE
  WIDTH 0 DO
    I 3 MOD 0= I 7 MOD 0= OR IF ALIVE I TAPE! THEN
  LOOP ;

\ --- Run an automaton for N rows and display ---
: RUN-RULE ( rule rows -- )
  ROWS 0 DO
    PRINT-TAPE
    OVER STEP-GENERATION
  LOOP
  2DROP ;

\ ============================================================
\ MAIN PROGRAM
\ ============================================================

." ============================================================" CR
." CELLULAR AUTOMATA — Forth Language Showcase" CR
." 1D Elementary Automata: Rule 30, 90, 110, 184" CR
." ============================================================" CR CR

\ ---- Rule 30: chaotic, used in RNG ----
." RULE 30 — Chaotic (used as random number generator in Mathematica)" CR
." Produces unpredictable output from simple start" CR CR
SEED-CENTER
30 ROWS RUN-RULE

CR ." ----" CR CR

\ ---- Rule 90: Sierpinski triangle ----
." RULE 90 — Sierpinski Triangle" CR
." Produces the famous fractal pattern" CR CR
SEED-CENTER
90 ROWS RUN-RULE

CR ." ----" CR CR

\ ---- Rule 110: Turing-complete ----
." RULE 110 — Turing Complete" CR
." One of the simplest known Turing-complete systems" CR CR
SEED-CENTER
110 ROWS RUN-RULE

CR ." ----" CR CR

\ ---- Rule 184: Traffic flow ----
." RULE 184 — Traffic Flow Model" CR
." Models single-lane traffic: 1 = car moving, 0 = empty road" CR CR
SEED-ALTERNATE
184 ROWS RUN-RULE

CR ." ----" CR CR

\ ---- Rule 73: Interesting symmetric pattern ----
." RULE 73 — Symmetric repetitive pattern" CR CR
SEED-CENTER
73 ROWS RUN-RULE

CR ." ----" CR CR

\ ---- All 8 rules side effects demo ----
." ALL RULES COMPARISON (rows 0-14 each):" CR
." Rule 30 (chaotic) | Rule 90 (Sierpinski) | Rule 110 (Turing)" CR CR

." -- Rule 30 (8 rows) --" CR
SEED-CENTER
30 8 RUN-RULE
CR

." -- Rule 90 (8 rows) --" CR
SEED-CENTER
90 8 RUN-RULE
CR

." -- Rule 110 (8 rows) --" CR
SEED-CENTER
110 8 RUN-RULE
CR

\ ---- Cellular automata facts ----
." ============================================================" CR
." FACTS ABOUT 1D CELLULAR AUTOMATA" CR
." ============================================================" CR
." Total rules possible:  256 (2^8 combinations)" CR
." Wolfram classes:" CR
."   Class 1: Stable (e.g. Rule 0 — everything dies)" CR
."   Class 2: Periodic (e.g. Rule 4 — stable oscillation)" CR
."   Class 3: Chaotic  (e.g. Rule 30 — unpredictable)" CR
."   Class 4: Complex  (e.g. Rule 110 — Turing complete)" CR CR
." Rule 30 is used as the default RNG in Wolfram Mathematica." CR
." Rule 110 was proven Turing-complete by Matthew Cook in 2004." CR
." Rule 90 generates Sierpinski's triangle from a single cell." CR
." ============================================================" CR
