# Lesson System Guide

Learn how to use the integrated lesson system in Time Warp Studio for structured, guided learning with automatic verification.

---

## Overview

The Lesson System provides structured programming education with:

- **Step-by-step lessons** - Follow guided tutorials with specific learning objectives
- **Checkpoints** - Verify your code solution at each step
- **Hints** - Get contextual hints when you're stuck
- **Progress tracking** - Track your completion and learning path
- **Auto-verification** - Automatic checking of your solutions

---

## Getting Started with Lessons

### Opening the Lesson System

1. Launch Time Warp Studio
2. Go to **File → Lessons** or press **Ctrl+Shift+L**
3. Browse available lessons by category and difficulty
4. Click **Start Lesson** to begin

### Lesson Interface

The lesson panel shows:

```
┌─────────────────────────────────┐
│ Lesson: BASIC Fundamentals      │
├─────────────────────────────────┤
│ Progress: 3 of 5 checkpoints    │
│ Difficulty: Beginner            │
├─────────────────────────────────┤
│ Current Objective:              │
│ Create a program that adds      │
│ two numbers and prints result   │
├─────────────────────────────────┤
│ Hint (Press H)                  │
│ Try using: LET X = 5 + 3        │
├─────────────────────────────────┤
│ [Get Hint] [Verify] [Skip]      │
└─────────────────────────────────┘
```

---

## Lesson Categories

### BASIC Lessons

**Fundamentals** (Beginner)
- Variables and assignment
- Basic arithmetic
- Input and output
- String operations

**Control Flow** (Beginner)
- Conditional statements (IF/THEN)
- Loops (FOR, WHILE)
- Comparison operators

**Procedures** (Intermediate)
- Function definition
- Parameters and return values
- Scope and variables
- Recursion basics

**Advanced** (Intermediate)
- Arrays and lists
- Working with strings
- File operations
- Debugging techniques

### Logo Lessons

**Turtle Basics** (Beginner)
- Drawing simple shapes
- Movement commands
- Turning and angles
- Pen control

**Procedures** (Intermediate)
- Defining procedures with parameters
- Recursive graphics
- Using variables in procedures
- Code organization

**Graphics** (Intermediate)
- Color and styling
- Complex patterns
- Mathematical designs
- Artistic drawings

### PILOT Lessons

**Interactive Teaching** (Beginner)
- Program structure
- Output and input
- Branching logic
- Interactive elements

**Advanced** (Intermediate)
- Complex matching
- Nested conditionals
- Full programs
- Educational designs

### Language-Specific

- **C Lessons** - Systems programming basics
- **Pascal Lessons** - Structured programming
- **Prolog Lessons** - Logic programming
- **Forth Lessons** - Stack-based programming

---

## Working Through a Lesson

### Step 1: Read the Objective

Each checkpoint starts with a clear learning objective explaining what you need to accomplish.

**Example:**
```
Objective: Write a program that asks for your name and greets you.
Expected output: "Hello, [name]!"
Language: BASIC
```

### Step 2: Write Your Code

Type your solution in the code editor:

```basic
PRINT "What is your name?"
INPUT NAME$
PRINT "Hello, " + NAME$ + "!"
```

### Step 3: Test Your Solution

1. Press **Ctrl+R** to run your code
2. Check the output in the Output panel
3. Compare with the expected output shown in the lesson

### Step 4: Verify Your Code

Click **Verify** button or press **Enter** in the lesson panel.

The system will:
- Run your code
- Capture the output
- Compare with expected output
- Show results

### Step 5: Get Hints (if needed)

If you're stuck:

1. Click **Get Hint** or press **H**
2. Read the contextual hint
3. Continue working on your solution
4. You can get multiple hints for each checkpoint

### Step 6: Move Forward

Once verified successfully:
- Click **Next Checkpoint** or press **N**
- Move to the next learning objective
- Track your progress

---

## Checkpoint Verification

### How Verification Works

When you click **Verify**, Time Warp Studio:

1. **Runs your code** in the appropriate language interpreter
2. **Captures output** from your program
3. **Compares** with expected output
4. **Shows results** - success or differences

### Understanding Results

**✅ Correct**
```
Your output matches the expected result.
Checkpoint completed!
Progress: 4 of 5
```

**❌ Incorrect**
```
Your output:
Hello, Alice

Expected:
Hello, Alice!

Hint: Check for punctuation in the greeting.
```

**⚠️ Runtime Error**
```
Error on line 3: Undefined variable 'NAME'
Make sure to use INPUT before printing.
```

---

## Hints System

### Getting Hints

Each checkpoint has multiple hint levels:

**Level 1: Concept Hint**
- General guidance about the topic
- No code directly given
- Encourages thinking

**Example:**
```
Hint 1: Remember that you need to get input from the user
before you can use that value in your program.
```

**Level 2: Strategy Hint**
- More specific guidance
- Suggests approach
- May show syntax patterns

**Example:**
```
Hint 2: Use INPUT statement to get user input.
Try: INPUT variable_name
```

**Level 3: Code Example**
- Shows similar code
- Not the exact solution
- Gives you the pattern to follow

**Example:**
```
Hint 3: Here's a pattern:
INPUT X$
PRINT "You entered: " + X$
```

### Hint Limitations

- Each checkpoint has 3 hints available
- Hints encourage learning, not replacement
- Using hints doesn't prevent checkpoint completion
- Hint count shown in progress tracking

---

## Tracking Your Progress

### Progress Display

The lesson panel shows:
```
Lesson: BASIC Fundamentals
Progress: 3 of 5 checkpoints completed
Last worked: 2 hours ago
Difficulty: ⭐⭐ (Intermediate)
Estimated time: 45 minutes
```

### Progress Tracking

Your progress is automatically saved:
- Which lessons started
- Which checkpoints completed
- Hints used per checkpoint
- Time spent on each checkpoint
- Current position in lesson

### Viewing Statistics

Go to **File → Settings → Learning Statistics** to see:
- Lessons completed
- Total programming time
- Topics mastered
- Estimated skill level

---

## Lesson Organization

### By Difficulty

**🟢 Beginner** (1-2 stars)
- Learn basic concepts
- Simple, guided examples
- ~30-45 min per lesson

**🟡 Intermediate** (2-3 stars)
- Build real projects
- More independent work
- ~1-2 hours per lesson

**🔴 Advanced** (3+ stars)
- Complex problems
- Minimal guidance
- 2-4+ hours per lesson

### By Language

Each supported language has lessons:
- BASIC (10+ lessons)
- Logo (8+ lessons)
- PILOT (6+ lessons)
- C (6+ lessons)
- Pascal (4+ lessons)
- Prolog (4+ lessons)
- Forth (4+ lessons)

### By Topic

Organized thematically:
- Fundamentals
- Control Flow
- Data Structures
- Procedures/Functions
- Graphics
- Advanced Topics

---

## Tips for Success

### Before Starting

1. **Choose appropriate difficulty** - Start with Beginner lessons
2. **Set aside time** - Plan 30-60 minutes per lesson
3. **Have reference ready** - Keep language guide handy
4. **Eliminate distractions** - Focus on learning

### During the Lesson

1. **Read objectives carefully** - Understand what's expected
2. **Work through examples first** - See how it's done
3. **Try without hints initially** - Stretch your understanding
4. **Test frequently** - Run code often, not just at end
5. **Use hints strategically** - Get unstuck, not answers

### After Completing

1. **Review your solution** - Understand what you wrote
2. **Experiment** - Modify code to explore variations
3. **Compare with hints** - See if there's a better approach
4. **Move forward** - Don't get stuck on perfection

---

## Customizing Your Lessons

### Lesson Preferences

Go to **File → Settings → Lessons** to adjust:

**Appearance:**
- Theme (matches editor theme)
- Font size
- Panel position (right or bottom)

**Behavior:**
- Auto-run code on changes
- Auto-verify checkpoints
- Hint level (conservative, moderate, generous)
- Progress saving (automatic, manual)

**Language:**
- Preferred lesson language
- Source code language

### Skipping Lessons

You can skip checkpoints by clicking **Skip**, but:
- Progress still counts as incomplete
- Later lessons may require earlier knowledge
- Skipped checkpoints remain available

---

## Lesson Best Practices

### As a Student

✅ **Do:**
- Start with fundamentals
- Complete lessons sequentially
- Use hints only when stuck
- Review successful solutions
- Experiment with variations

❌ **Don't:**
- Skip ahead without understanding
- Copy hint code directly
- Rush through checkpoints
- Give up after one attempt
- Ignore error messages

### As an Educator

✅ **Do:**
- Assign appropriate lessons by level
- Set checkpoints progressively
- Provide context before lessons
- Monitor student progress
- Offer additional resources

❌ **Don't:**
- Force all students through same path
- Skip prerequisites
- Demand perfection
- Ignore individual learning speeds
- Use lessons as only assessment

---

## Troubleshooting Lessons

### "Verification not matching expected output"

**Issue:** Your code runs but output doesn't match exactly.

**Solutions:**
1. Check for trailing spaces or extra lines
2. Verify capitalization matches
3. Ensure correct punctuation
4. Look for number format differences

**Example:**
```
Your output: "Hello, Alice !" (extra space)
Expected:   "Hello, Alice!" (no space)
```

### "Code runs but lesson says error"

**Issue:** Your code has syntax that works but is non-standard.

**Solution:**
- Check the hint for correct syntax
- Some variations may not be recognized
- Use exact command forms from documentation

### "Lost progress"

**Issue:** Progress not saved from previous session.

**Solution:**
1. Progress is auto-saved to `~/.Time_Warp/config.json`
2. Check if file was recently modified
3. Verify write permissions on home directory
4. Restart the IDE to reload progress

### "Can't find a lesson"

**Issue:** Expected lesson not visible in catalog.

**Solutions:**
1. Check filter settings (language, difficulty)
2. Some lessons appear only when prerequisites met
3. Use search function (Ctrl+F)
4. Check documentation for lesson availability

---

## Advanced Lesson Features

### Creating Custom Lessons (Advanced)

Coming in future versions:
- Create personalized lesson sequences
- Add custom checkpoints
- Share lessons with students
- Track custom lesson progress

### Collaborative Lessons

Coming in future versions:
- Work on lessons with peers
- Share solutions for review
- Compare different approaches
- Learn from others' solutions

---

## Related Resources

- **BASIC Tutorial:** [../../docs/tutorials/basic.md](../tutorials/basic.md)
- **Logo Tutorial:** [../../docs/tutorials/logo.md](../tutorials/logo.md)
- **IDE Basics:** [02-ide-basics.md](02-ide-basics.md)
- **Examples:** [../../Examples/README.md](../../Examples/README.md)

---

**Last Updated:** January 2026  
**Status:** Fully Implemented  
**Lessons Available:** 50+
