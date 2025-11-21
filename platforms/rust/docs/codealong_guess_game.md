# Time Warp IDE - Code-Along Exercise: Number Guessing Game

**Activity:** Build a complete text game together as a class  
**Duration:** 30 minutes  
**Skills:** INPUT, IF/THEN, variables, loops

---

## Goal

Create a BASIC program where:

- The computer picks a random number 1–100
- The player guesses
- The computer gives "too high" or "too low" hints
- Game ends when the player guesses correctly

---

## Step-by-step

### 1. Setup (2 min)

Create `basic_guess_game.bas`

### 2. Pick the secret number (5 min)

```basic
10 PRINT "I'm thinking of a number between 1 and 100"
20 LET SECRET = int(rand()*100)+1
30 PRINT "Secret picked! Start guessing."
```

**Explain:** `rand()` gives 0–1. Multiply by 100, add 1, truncate → 1–100.

### 3. Get user input (5 min)

```basic
40 INPUT GUESS
```

Add this line. Run and test that it waits for input.

### 4. Compare guess to secret (10 min)

```basic
50 IF GUESS = SECRET THEN 100
60 IF GUESS < SECRET THEN 80
70 PRINT "Too high"
75 GOTO 40
80 PRINT "Too low"
85 GOTO 40
100 PRINT "Correct! The number was", SECRET
110 END
```

**Explain:** Line 50 jumps to win message if correct. Lines 60–85 give hints and loop back to INPUT.

### 5. Test and refine (8 min)

Run the game together. Try:

- Guessing too high
- Guessing too low
- Guessing correctly

**Optional enhancement:** Add a counter for number of guesses.

```basic
15 LET TRIES = 0
45 LET TRIES = TRIES + 1
105 PRINT "You guessed in", TRIES, "tries!"
```

---

## Wrap-up

The class now has a complete, playable game. Discuss:

- How loops work (GOTO creates a cycle)
- Why conditionals matter (IF/THEN branches logic)
- How to add features (attempt counter, difficulty levels)

Save the file and encourage students to customize (different ranges, hints, ASCII art).

---

## Extension Ideas

- Add a "play again?" prompt (Y/N match)
- Limit attempts to 10 tries
- Mix with PILOT for story elements ("The wizard asks for your guess...")
