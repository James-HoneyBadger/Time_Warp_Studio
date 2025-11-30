# C Programming Examples

Educational C programs demonstrating various programming techniques.

## Files Overview

| File | Topic | Techniques Demonstrated |
|------|-------|------------------------|
| `01_hello_world.c` | Hello World | Basic structure, printf(), main() |
| `02_variables_types.c` | Variables & Types | int, float, char, string, format specifiers |
| `03_input_output.c` | Input/Output | scanf(), printf(), user interaction |
| `04_conditionals.c` | Conditionals | if/else if/else, switch, comparison operators |
| `05_loops.c` | Loops | for, while, do-while, break, continue |
| `06_functions.c` | Functions | Declaration, definition, parameters, return values |
| `07_arrays.c` | Arrays | 1D arrays, 2D arrays, iteration, operations |
| `08_calculator.c` | Complete Program | Menu-driven interface, functions, validation |

## Compiling and Running

### Using GCC (Linux/Mac)
```bash
gcc 01_hello_world.c -o hello
./hello
```

### Compile with warnings enabled
```bash
gcc -Wall -Wextra 01_hello_world.c -o hello
```

### Using Time Warp IDE
1. Open the `.c` file in Time Warp IDE
2. Click "Run" or press F5
3. View output in the console

## Concepts Covered

### Basic Syntax
- Program structure (`#include`, `main()`, `return`)
- Comments (`//` and `/* */`)
- Semicolons and braces

### Data Types
- Integers (`int`)
- Floating point (`float`, `double`)
- Characters (`char`)
- Strings (character arrays)

### Input/Output
- `printf()` - formatted output
- `scanf()` - formatted input
- Format specifiers (`%d`, `%f`, `%c`, `%s`)

### Control Flow
- Conditional statements (`if`, `else if`, `else`)
- Switch statements
- For loops
- While loops
- Do-while loops

### Functions
- Function prototypes
- Parameters (pass by value)
- Return values
- Void functions

### Arrays
- Declaration and initialization
- Accessing elements
- Multi-dimensional arrays
- Array operations (sum, average, max)

## Learning Path

**Beginners**: Start with files 01-04
- Understand basic syntax
- Learn variables and types
- Master input/output
- Practice conditionals

**Intermediate**: Continue with files 05-07
- Loop constructs
- Function design
- Array manipulation

**Advanced**: Study file 08
- Complete program structure
- Menu-driven interfaces
- Error handling
- Code organization

## Common Patterns

### Error Checking
```c
if (value < 0 || value > 100) {
    printf("Error: Invalid value!\n");
    return 1;
}
```

### Input Validation
```c
do {
    printf("Enter positive number: ");
    scanf("%d", &num);
} while (num <= 0);
```

### Array Iteration
```c
for (i = 0; i < length; i++) {
    printf("%d ", array[i]);
}
```

## Tips for Success

1. **Compile often**: Catch errors early
2. **Use meaningful names**: `score` not `x`
3. **Comment your code**: Explain complex logic
4. **Test edge cases**: Zero, negative, very large values
5. **Format consistently**: Use proper indentation

## Resources

- [C Programming Guide](../../docs/user/01-programming-guide.md)
- [Quick Reference](../../docs/user/02-quick-reference.md)
- [Student Workbook](../../docs/student/00-workbook.md)

## Practice Exercises

Try modifying the examples:
- Add new calculator operations
- Create a grade book system
- Implement bubble sort
- Build a number guessing game
- Create a temperature conversion table
