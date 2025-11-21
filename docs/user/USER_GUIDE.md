# Time Warp IDE User Guide

Time Warp IDE is an educational programming environment that supports three classic languages: BASIC, PILOT, and Logo. It provides both a graphical IDE and an interactive REPL for learning and experimentation.

## Getting Started

### Launching the IDE

**Python Implementation (Primary):**
```bash
# From the project directory
./run.sh          # Launch the graphical IDE
./run.sh ide      # Same as above
./run.sh repl     # Launch the interactive REPL
```

**System Installation:**
If installed system-wide, simply run:
```bash
timewarp           # Launch the IDE
timewarp-repl      # Launch the REPL (if available)
```

### Interface Overview

- **Code Editor**: Write and edit programs in BASIC (.bas), PILOT (.pilot), or Logo (.logo)
- **Output Panel**: View program execution results and error messages
- **Graphics Canvas**: Display turtle graphics and visual output
- **REPL**: Interactive command-line interface for immediate execution

## Supported Languages

### BASIC

BASIC (Beginner's All-purpose Symbolic Instruction Code) is a simple, line-numbered language perfect for beginners.

**File Extension:** `.bas`

**Basic Syntax:**
```basic
10 PRINT "Hello, World!"
20 LET X = 5
30 PRINT "X = "; X
40 END
```

**Key Commands:**
- `PRINT` - Display text or variables
- `LET` - Assign values to variables
- `INPUT` - Get user input
- `IF...THEN...ELSE` - Conditional execution
- `FOR...NEXT` - Loops
- `GOTO` - Jump to line numbers
- `END` - End program

### PILOT

PILOT (Programmed Inquiry, Learning Or Teaching) is designed for educational applications and interactive lessons.

**File Extension:** `.pilot`

**Basic Syntax:**
```pilot
T: Welcome to PILOT!
A: What is your name?
T: Hello, #ANS! Nice to meet you.
J: *
```

**Key Commands:**
- `T:` - Type (display) text
- `A:` - Accept (input) from user
- `M:` - Match user input against patterns
- `J:` - Jump to another section
- `U:` - Use (call) a subroutine
- `E:` - End program

### Logo

Logo is famous for its turtle graphics and is great for teaching geometry and programming concepts.

**File Extension:** `.logo`

**Basic Syntax:**
```logo
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
```

**Key Commands:**
- `FORWARD` / `FD` - Move turtle forward
- `BACK` / `BK` - Move turtle backward
- `RIGHT` / `RT` - Turn turtle right
- `LEFT` / `LT` - Turn turtle left
- `PENUP` / `PU` - Lift pen (stop drawing)
- `PENDOWN` / `PD` - Lower pen (start drawing)
- `REPEAT` - Repeat commands
- `TO` / `END` - Define procedures

## Using the IDE

### Loading and Running Programs

1. **File Menu**: Use "Open" to load .bas, .pilot, or .logo files
2. **Examples Menu**: Browse included sample programs
3. **Run Button**: Execute the current program
4. **Stop Button**: Halt execution

### Language Detection

The IDE automatically detects the language based on file extension:
- `.bas` → BASIC
- `.pilot` → PILOT
- `.logo` → Logo

### Graphics and Turtle Commands

All three languages support turtle graphics commands for drawing:

**Movement:**
- `FORWARD n` / `FD n` - Move forward n units
- `BACK n` / `BK n` - Move backward n units
- `RIGHT angle` / `RT angle` - Turn right by angle degrees
- `LEFT angle` / `LT angle` - Turn left by angle degrees

**Pen Control:**
- `PENUP` / `PU` - Stop drawing
- `PENDOWN` / `PD` - Start drawing
- `PENCOLOR r g b` - Set pen color (RGB 0-255)
- `PENWIDTH width` - Set pen thickness

**Drawing Shapes:**
- `CIRCLE x y radius` - Draw a circle
- `RECT x y width height` - Draw a rectangle
- `SETXY x y` - Move to specific coordinates

**Display:**
- `CLS` - Clear the screen
- `HOME` - Return turtle to center
- `HIDETURTLE` / `SHOWTURTLE` - Hide/show turtle icon

## Using the REPL

The REPL (Read-Eval-Print Loop) allows interactive execution:

```bash
$ ./run.sh repl
Time Warp REPL
> 10 PRINT "Hello!"
Hello!
> RUN
> FORWARD 50
> RIGHT 90
> FORWARD 50
>
```

- Type commands and press Enter to execute them immediately
- Use `RUN` to execute multi-line programs
- Use `CLEAR` to reset the environment
- Use `EXIT` or `QUIT` to exit

## Examples

The IDE includes many example programs in the `examples/` directory:

- `basic_*.bas` - BASIC programming examples
- `pilot_*.pilot` - PILOT lesson examples
- `logo_*.logo` - Logo graphics examples
- `*_demo.tc` - Mixed language demonstrations

## Tips for Learning

1. **Start with Logo**: The turtle graphics make programming visual and fun
2. **Try PILOT for Lessons**: Great for creating interactive educational content
3. **Use BASIC for Logic**: Learn fundamental programming concepts
4. **Experiment in REPL**: Test commands interactively before writing full programs
5. **Study Examples**: Each example demonstrates specific language features

## Troubleshooting

**Program won't run:**
- Check file extension matches the language (.bas, .pilot, .logo)
- Look for syntax errors in the output panel
- Ensure all commands are properly formatted

**Graphics not showing:**
- Make sure you're using turtle graphics commands
- Check that the canvas panel is visible
- Try `CLS` to clear and reset the display

**REPL not responding:**
- Press Ctrl+C to interrupt long-running commands
- Use `CLEAR` to reset the environment
- Check for syntax errors in your input

## Advanced Features

- **File I/O**: Read and write text files
- **Math Functions**: sin, cos, tan, sqrt, etc.
- **String Operations**: concatenation, substring, search
- **Lists and Arrays**: store multiple values
- **Procedures**: define reusable code blocks
- **Error Handling**: graceful handling of runtime errors

For more advanced features, see the Programming Guide and example programs.
