# Time Warp Studio - Example Programs

## Overview

This directory contains example programs for all supported languages in Time Warp Studio desktop application. Each example demonstrates language features and has been verified to work with the current interpreter.

## How to Use Examples

1. Launch Time Warp Studio: `python Platforms/Python/time_warp_ide.py`
2. Open File → Open or use Ctrl+O
3. Navigate to the appropriate language folder in `Examples/`
4. Select an example program
5. Click Run (or press F5/Ctrl+R)

## Language Examples

### BASIC Examples
- **01_hello_world.bas** - Basic output with PRINT command
- **02_variables_and_types.bas** - Variable declaration, types, arrays
- **03_arithmetic.bas** - Arithmetic operations and expressions
- **04_input_output.bas** - User input and formatted output
- **05_conditionals.bas** - IF/THEN/ELSE statements
- **06_loops.bas** - FOR, WHILE, DO loops
- **07_subroutines.bas** - SUB/FUNCTION procedures with parameters
- **08_strings.bas** - String operations and manipulation
- **09_arrays.bas** - Array declaration and operations
- **10_guessing_game.bas** - Interactive game with logic
- **11_showcase.bas** - Comprehensive feature demonstration

### Logo Examples
- **01_hello_world.logo** - Basic PRINT and procedure definition
- **02_squares.logo** - Procedures with parameters, REPEAT loops
- **03_polygons.logo** - Drawing multiple polygon shapes
- **04_spirals.logo** - Mathematical spiral patterns
- **05_trees.logo** - Recursive tree drawing
- **06_patterns.logo** - Complex geometric patterns
- **07_geometric.logo** - Various geometric shapes
- **08_artistic.logo** - Artistic and creative patterns
- **09_showcase.logo** - Comprehensive Logo feature demo
- **10_graphics_demo.logo** - NEW: Comprehensive graphics with colors
- **colors.logo** - Color demonstrations and pen control
- **sample.logo** - Classic Logo programming example

### PILOT Examples
- **01_hello_world.pilot** - Basic input/output and program structure
- **02_variables.pilot** - Variable declaration and usage
- **03_arithmetic.pilot** - Numeric calculations with COMPUTE
- **04_conditionals.pilot** - MATCH statement for branching
- **05_loops.pilot** - LOOP and iteration structures
- **06_subroutines.pilot** - JUMP/JUMPT for procedures
- **07_strings.pilot** - String manipulation
- **08_guessing_game.pilot** - Interactive number guessing
- **09_showcase.pilot** - All PILOT features combined

### C Examples
- **01_hello_world.c** - Basic program structure and printf()
- **02_variables_types.c** - Data types and variable usage
- **03_input_output.c** - Input/output operations
- **04_conditionals.c** - if/else statements
- **05_loops.c** - for, while, do-while loops
- **06_functions.c** - Function definition and calling
- **07_arrays.c** - Array declaration and operations
- **08_calculator.c** - Calculator program with functions
- **showcase.c** - Comprehensive C features demo

### Pascal Examples
- **01_hello_world.pas** - Program structure and WriteLn
- **02_variables_types.pas** - Variable declaration and types
- **03_input_output.pas** - Input/output operations
- **04_conditionals.pas** - if/then/else statements
- **05_loops.pas** - for, while, repeat loops
- **06_procedures_functions.pas** - Procedures with parameters
- **07_arrays.pas** - Array operations
- **08_calculator.pas** - Calculator program
- **showcase.pas** - Comprehensive Pascal features

### Forth Examples
- **01_hello_world.f** - Basic output with ." and CR
- **02_arithmetic.f** - Updated: Arithmetic in Reverse Polish Notation
- **03_loops.f** - Updated: Counting loops and iteration
- **04_graphics.f** - Graphics operations
- **showcase.f** - Comprehensive Forth features

### Prolog Examples
- **01_facts_rules.pl** - Facts and rules definitions
- **02_family_tree.pl** - Family relationship database
- **03_lists.pl** - List operations and unification
- **04_recursion.pl** - Recursive predicates
- **05_arithmetic.pl** - Arithmetic evaluation

## Command Reference

### Logo Commands
**Movement:**
- FORWARD n / FD n - Move forward n pixels
- BACK n / BK n - Move backward n pixels
- LEFT n / LT n - Turn left n degrees
- RIGHT n / RT n - Turn right n degrees
- HOME - Return to center
- SETXY x y - Move to position (x, y)

**Pen Control:**
- PENUP / PU - Stop drawing
- PENDOWN / PD - Start drawing
- SETPENCOLOR name/hex/RGB - Set color
- SETPENWIDTH n - Set pen thickness
- SETBGCOLOR color - Set background color

**Other:**
- REPEAT n [commands] - Repeat commands n times
- HIDETURTLE / HT - Hide turtle cursor
- SHOWTURTLE / ST - Show turtle cursor
- PRINT [text] - Output text

### BASIC Commands
**Output:**
- PRINT expr - Display output
- INPUT var - Get user input

**Variables:**
- LET var = expr - Assign value
- DIM array(size) - Declare array

**Control:**
- IF condition THEN statements - Conditional execution
- FOR i = start TO end - Loop counter
- WHILE condition - While loop
- SUB name(...) ... END SUB - Procedure
- FUNCTION name(...) ... END FUNCTION - Function

**Graphics:**
- SETPENCOLOR color - Set drawing color
- SETPENWIDTH width - Set pen thickness

### PILOT Commands
**I/O:**
- PRINT text - Output text
- ACCEPT var text - Get user input

**Computation:**
- COMPUTE var expression - Calculate values

**Control:**
- MATCH pattern / WRONG text - Conditional branching
- JUMP label / JUMPT label - Unconditional jump
- LOOP [commands] - Loop structure

**Graphics:**
- G: FORWARD distance - Move forward
- G: SETPENCOLOR r,g,b - Set pen color
- G: SETBGCOLOR r,g,b - Set background color

## Updated Features (December 2025)

### Recent Improvements
1. **Color Commands** - All languages now support modern color syntax
   - Named colors (RED, GREEN, BLUE, etc.)
   - Hex colors (#RRGGBB)
   - RGB values (255, 128, 64)

2. **Logo Graphics** - Enhanced with new demo examples
   - Comprehensive graphics demonstration
   - Shape drawing procedures
   - Grid-based layouts

3. **PILOT Graphics** - Added SETPENCOLOR support
   - Full color control for graphics
   - Compatible with Logo color names

4. **Documentation** - All examples include:
   - Clear header comments
   - Feature descriptions
   - Inline code explanations

## How to Run Examples

### In Time Warp Studio
1. Open Time Warp Studio
2. Select language (BASIC, Logo, PILOT, C, Pascal, Forth, Prolog)
3. Load example file from Examples directory
4. Click Run or press Execute

### Command Line
```bash
cd Examples/[language]/
cat [filename].[ext]
# Then copy/paste into IDE
```

## Testing Status

All example programs have been verified to work with current language implementations:
- ✅ BASIC - All examples tested
- ✅ Logo - All examples tested  
- ✅ PILOT - All examples tested
- ✅ C - All examples tested
- ✅ Pascal - All examples tested
- ✅ Forth - All examples tested
- ✅ Prolog - All examples verified

## Feature Matrix

| Feature | BASIC | Logo | PILOT | C | Pascal | Forth | Prolog |
|---------|-------|------|-------|---|--------|-------|--------|
| Variables | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Arithmetic | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Conditionals | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Loops | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Procedures | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Graphics | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ |
| Color | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ |

## Notes

- All examples use current command syntax verified with December 2025 builds
- Color commands now support multiple input formats across languages
- Logo procedures support parameterization and recursion
- BASIC includes modern array and string operations
- PILOT supports full graphics integration

## Recommendations for Learning

1. **Beginners** - Start with 01_hello_world examples
2. **Intermediate** - Study variables, loops, and procedures
3. **Graphics** - Explore Logo and PILOT graphics examples
4. **Advanced** - Review showcase programs and complex patterns

---

**Last Updated:** December 25, 2025  
**Verification Status:** ✅ All Examples Tested  
**Language Support:** BASIC, Logo, PILOT, C, Pascal, Forth, Prolog
