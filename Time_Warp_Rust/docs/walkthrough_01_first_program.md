# Time Warp IDE - Classroom Walkthrough #1: First Program

**Topic:** Writing your first PILOT program  
**Duration:** 20 minutes  
**Skills:** Text output, variables, conditionals

---

## Setup (5 min)

1. Launch Time Warp IDE: `cargo run`
2. Open the Editor tab
3. Create a new file: `pilot_hello.pilot`

---

## Step 1: Hello World (5 min)

Type this code:

```pilot
T:Hello, World!
T:Welcome to Time Warp!
E:
```

Click Run (▶️). You should see the two text lines in the Output tab.

**Explain:** `T:` displays text. `E:` ends the program.

---

## Step 2: Variables (5 min)

Extend the program:

```pilot
U:NAME=Alice
T:Hello, *NAME*!
E:
```

Run again. Notice how `*NAME*` becomes "Alice".

**Explain:** `U:VAR=value` sets a variable. `*VAR*` interpolates it into text.

---

## Step 3: Input and Conditionals (5 min)

Replace with:

```pilot
T:What is your name?
A:NAME
T:Nice to meet you, *NAME*!
M:ALICE
Y:
T:Alice is a great name!
N:
T:All names are wonderful!
E:
```

Run. Type "Alice" when prompted. See the special message.

**Explain:** `A:VAR` prompts for input. `M:pattern` matches text. `Y:` executes the next `T:` if match succeeded.

---

## Challenge

Modify the program to ask for a favorite color and respond with a unique message for RED, BLUE, and GREEN.

**Solution hint:** Use multiple `M:` / `Y:` / `N:` blocks.

---

## Wrap-up

Students now know:

- `T:` for output
- `U:` and `*VAR*` for variables
- `A:` for input
- `M:` / `Y:` / `N:` for conditionals

Next: BASIC and loops.
