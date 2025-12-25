# Programming Tutorials

Learn to program in any of 7 languages with hands-on tutorials.

## Choose Your Language

| Language | Best For | Difficulty | Time |
|----------|----------|-----------|------|
| [**Logo**](logo.md) | Visual, immediate results | Beginner | 30 min |
| [**BASIC**](basic.md) | Learning fundamentals | Beginner | 1 hour |
| [**PILOT**](pilot.md) | Pattern matching, AI | Intermediate | 1 hour |
| [**Pascal**](pascal.md) | Structured programming | Intermediate | 2 hours |
| [**Prolog**](prolog.md) | Logic programming | Advanced | 2 hours |
| [**Forth**](forth.md) | Stack-based, games | Advanced | 2 hours |
| [**C**](c.md) | Performance, systems | Advanced | 3 hours |

## Learning Path

**Recommended progression:**

### Week 1: Fundamentals
1. **Logo** (30 min) - Get visual feedback immediately
2. **BASIC** (1 hour) - Learn core programming concepts
3. **Mini Project** - Combine Logo + BASIC

### Week 2: Intermediate Concepts  
1. **PILOT** (1 hour) - Pattern matching and control flow
2. **Pascal** (2 hours) - Structured programming
3. **Mini Project** - Build interactive program

### Week 3: Advanced Topics
1. **Prolog** (2 hours) - Logic and reasoning
2. **Forth** (2 hours) - Stack-based thinking
3. **C** (3 hours) - Performance programming
4. **Capstone Project** - Combine multiple languages

## Quick Start: Hello World

### Logo
```logo
PRINT "Hello World!"
```

### BASIC
```basic
PRINT "Hello World!"
```

### PILOT
```pilot
T: Hello World!
E:
```

### Pascal
```pascal
BEGIN
  WRITELN('Hello World!')
END.
```

### Prolog
```prolog
?- write('Hello World!').
```

### Forth
```forth
: HELLO ." Hello World!" ;
HELLO
```

### C
```c
#include <stdio.h>
main() {
  printf("Hello World!\n");
}
```

## Common Concepts

### Variables

**Logo**
```logo
MAKE "X 10
PRINT :X
```

**BASIC**
```basic
X = 10
PRINT X
```

**PILOT**
```pilot
ACCEPT X(N)
PRINT X
```

### Loops

**Logo**
```logo
REPEAT 5 [PRINT "Hi"]
```

**BASIC**
```basic
FOR I = 1 TO 5
  PRINT "Hi"
NEXT I
```

**PILOT**
```pilot
N: 1
L: PRINT "Hi"
   ACCEPT X
   J: PRINT X
```

### Conditionals

**Logo**
```logo
IF :X > 5 [PRINT "Big"]
```

**BASIC**
```basic
IF X > 5 THEN PRINT "Big"
```

**PILOT**
```pilot
ACCEPT X(N)
MATCH: X
  C: 5 → PRINT "Big"
  C: ELSE → PRINT "Small"
```

## Full Tutorials

Click a language below for complete tutorial:

- [**Logo Tutorial**](logo.md) - Graphics and turtle movements
- [**BASIC Tutorial**](basic.md) - Variables, loops, functions
- [**PILOT Tutorial**](pilot.md) - Pattern matching and branching
- [**Pascal Tutorial**](pascal.md) - Procedures and structured code
- [**Prolog Tutorial**](prolog.md) - Logic and facts/rules
- [**Forth Tutorial**](forth.md) - Stack-based programming
- [**C Tutorial**](c.md) - Functions and systems programming

## Projects by Language

### Logo Projects
- Draw geometric shapes
- Create fractals (Sierpinski, Mandelbrot)
- Animated drawing programs

### BASIC Projects
- Number guessing game
- Temperature converter
- Grade calculator

### PILOT Projects
- Quiz program
- Chat bot
- Pattern matching game

### Multi-Language Projects
- **Game** - Logo for graphics, BASIC for logic
- **Data Tool** - Pascal for structure, BASIC for display
- **Interactive Art** - Forth for control, Logo for graphics

## Tips for Learning

1. **Start small** - Begin with simple, single-concept programs
2. **Modify examples** - Change existing code to learn
3. **Use immediate mode** - Test commands one at a time
4. **Read error messages** - They tell you what went wrong
5. **Compare languages** - See how different languages solve the same problem
6. **Practice graphics** - Visual feedback helps learning
7. **Build projects** - Complete programs, not just snippets

## Getting Help

- **Stuck?** - Check the example in the Examples/ folder
- **Syntax error?** - Compare to a working example
- **Runtime error?** - Read the error message carefully
- **Want to learn more?** - Check online tutorials for your language

## Next Steps

Pick a language from the table above and dive in!

Start with **Logo** if you're new to programming - it's fun and visual.

---

Happy learning! 🎓
