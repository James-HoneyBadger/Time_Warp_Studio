# Time Warp IDE - Student Guide

Welcome! This guide helps you learn PILOT, BASIC, and Logo while making cool programs.

## What you can do

- Make text adventures with PILOT
- Do math and logic with BASIC
- Draw graphics with Logo (turtle graphics!)

## Start here

1. Open the IDE and pick the Editor tab.
2. Load a sample from `examples/` or type your own code.
3. Click Run (▶️). Your output shows in the Output tab; graphics show on the canvas.

## Input prompts

- When your program asks for input (BASIC `INPUT` or PILOT `A:`), an 📝 prompt appears.
- Type your answer and press Enter. Numbers are stored as numbers; otherwise as text.

## Mini language cheatsheets

### PILOT

- `T:text` show text
- `A:VAR` accept input into `VAR`
- `U:VAR=expr` set variable
- `C:cond` compute and store condition
- `Y:` next `T:` only if last condition true
- `N:` next `T:` only if last condition false
- `J:label` jump to label
- `L:label` define label
- `E:` end program

Try: `examples/pilot_quiz.pilot`

### BASIC

- `PRINT` shows text or values
- `INPUT X` stores input into `X`
- `LET X = expr` sets variable
- `IF cond THEN ...` conditional
- `FOR I = a TO b` loop
- `GOTO n` jump to line number
- `GOSUB n` / `RETURN` subroutine
- `LINE x1,y1,x2,y2` and `CIRCLE x,y,r` draw graphics

Try: `examples/basic_guess.bas` and `examples/basic_graphics.bas`

### Logo

- `FORWARD n`, `BACK n` move
- `LEFT n`, `RIGHT n` turn
- `PENUP`, `PENDOWN`
- `PENWIDTH n`, `SETCOLOR <name|#hex>`
- `REPEAT n [ ... ]`
- `TO NAME ... END` define procedures

Try: `examples/logo_spirograph.logo`, `examples/logo_house.logo`

## Challenges

- Modify the color quiz to add more colors.
- Add an attempt counter to Guess-the-Number.
- Draw a custom pattern using nested `REPEAT` in Logo.

Have fun and save your graphics as PNG via View → Save Canvas as PNG…
