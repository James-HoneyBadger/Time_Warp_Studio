# Pascal Programming Examples

Educational Pascal programs demonstrating various programming techniques.

## Files Overview

| File | Topic | Techniques Demonstrated |
|------|-------|------------------------|
| `01_hello_world.pas` | Hello World | Program structure, Begin/End, WriteLn |
| `02_variables_types.pas` | Variables & Types | Integer, Real, Char, String, Var section |
| `03_input_output.pas` | Input/Output | ReadLn, WriteLn, Write, formatting |
| `04_conditionals.pas` | Conditionals | If-Then-Else, Case, Boolean expressions |
| `05_loops.pas` | Loops | For To/DownTo, While, Repeat-Until |
| `06_procedures_functions.pas` | Procedures/Functions | Parameters, return values, local variables |
| `07_arrays.pas` | Arrays | 1D arrays, 2D arrays, Array[x..y] syntax |
| `08_calculator.pas` | Complete Program | Menu-driven, Case statement, validation |

## Compiling and Running

### Using Free Pascal Compiler
```bash
fpc 01_hello_world.pas
./01_hello_world
```

### With optimization
```bash
fpc -O2 01_hello_world.pas
```

### Using Time Warp IDE
1. Open the `.pas` file in Time Warp IDE
2. Click "Run" or press F5
3. View output in the console

## Concepts Covered

### Basic Syntax
- Program structure (`Program`, `Begin`, `End.`)
- Comments with `{ }` or `(* *)`
- Case-insensitive keywords
- Statement separators (semicolons)

### Data Types
- Integer (whole numbers)
- Real (floating point)
- Char (single character)
- String (text)
- Boolean (True/False)

### Input/Output
- `WriteLn()` - output with newline
- `Write()` - output without newline
- `ReadLn()` - input with enter
- Formatting with `:width:decimals`

### Control Flow
- If-Then-Else statements
- Case statements (multi-way branches)
- For loops (To and DownTo)
- While loops (condition before)
- Repeat-Until loops (condition after)

### Procedures and Functions
- Procedure (no return value)
- Function (returns value)
- Parameters (value parameters)
- Local variables
- Forward declarations

### Arrays
- Declaration: `Array[1..10] Of Integer`
- Multi-dimensional: `Array[1..5, 1..5]`
- Accessing elements
- Array operations

## Learning Path

**Beginners**: Start with files 01-04
- Understand Pascal structure
- Learn Var declarations
- Master Begin/End blocks
- Practice If-Then-Else

**Intermediate**: Continue with files 05-07
- For/While/Repeat loops
- Procedure vs Function
- Array indexing

**Advanced**: Study file 08
- Complete program design
- Case statements
- Input validation
- Modular code

## Common Patterns

### Variable Declaration
```pascal
Var
  age: Integer;
  price: Real;
  name: String;
```

### Conditional Logic
```pascal
If score >= 90 Then
  grade := 'A'
Else If score >= 80 Then
  grade := 'B'
Else
  grade := 'C';
```

### For Loop
```pascal
For i := 1 To 10 Do
Begin
  WriteLn(i);
End;
```

### Repeat-Until (Input Validation)
```pascal
Repeat
  Write('Enter positive: ');
  ReadLn(num);
Until num > 0;
```

## Pascal vs C Differences

| Pascal | C | Purpose |
|--------|---|---------|
| `:=` | `=` | Assignment |
| `=` | `==` | Comparison |
| `Begin/End` | `{ }` | Block |
| `WriteLn` | `printf` | Output |
| `ReadLn` | `scanf` | Input |
| `And/Or/Not` | `&&/\|\|/!` | Logic |
| `Procedure` | `void function` | No return |
| `Function` | `function` | Returns value |

## Tips for Success

1. **Case doesn't matter**: `WriteLn` = `writeln` = `WRITELN`
2. **Declare before use**: All variables in `Var` section
3. **Block structure**: Use `Begin/End` for multi-statement blocks
4. **Assignment vs equality**: `:=` to assign, `=` to compare
5. **End with period**: Last `End` must be `End.`

## Resources

- [Pascal Programming Guide](../../docs/user/01-programming-guide.md)
- [Quick Reference](../../docs/user/02-quick-reference.md)
- [Student Workbook](../../docs/student/00-workbook.md)

## Practice Exercises

Try modifying the examples:
- Create a contact list program
- Build a quiz game
- Implement selection sort
- Make a temperature converter
- Design a simple database
