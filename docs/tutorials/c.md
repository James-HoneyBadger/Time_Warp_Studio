# C Tutorial

**C** is a powerful, efficient language that teaches you how computers actually work. It's the foundation for many modern languages and systems software.

## Getting Started

### Hello, World! 👋

```c
#include <stdio.h>

int main() {
    printf("Hello, World!\n");
    return 0;
}
```

**Output:**
```
Hello, World!
```

Key elements:
- `#include <stdio.h>` - Include input/output library
- `int main()` - Main function (where program starts)
- `printf()` - Print formatted text
- `return 0;` - Exit successfully
- `{ }` - Code block
- `;` - End statement

## Variables and Types

### Basic Data Types

```c
int AGE = 25;           // Whole number (-2^31 to 2^31-1)
float PI = 3.14159;     // Decimal (6 digits precision)
double PRICE = 19.99;   // Decimal (15 digits precision)
char GRADE = 'A';       // Single character
char NAME[] = "Alice";  // String (array of chars)
```

### Type Modifiers

```c
unsigned int POSITIVE = 100;    // Only positive (0 to 2^32-1)
short X = 10;                   // Smaller integer
long BIG = 1000000;             // Larger integer
const int MAX = 100;            // Cannot change
```

### Declaring Multiple Variables

```c
int X = 5, Y = 10, Z = 15;
float A, B, C;
A = 1.5;
```

## Input and Output

### Printing (printf)

```c
printf("Hello\n");              // Basic print
printf("Number: %d\n", 42);     // Integer
printf("Float: %.2f\n", 3.14);  // 2 decimal places
printf("Text: %s\n", "Hi");     // String
printf("Char: %c\n", 'X');      // Character
```

**Format specifiers:**
- `%d` - Integer
- `%f` - Float/Double
- `%.2f` - Float with 2 decimals
- `%s` - String
- `%c` - Character
- `%x` - Hexadecimal
- `%%` - Literal percent sign

### Reading Input (scanf)

```c
int X;
printf("Enter a number: ");
scanf("%d", &X);        // Note the & for integer
printf("You entered: %d\n", X);

float F;
scanf("%f", &F);        // Float

char NAME[50];
scanf("%s", NAME);      // String (no & needed)
```

## Math Operations

```c
#include <math.h>

int A = 10, B = 3;
printf("Sum: %d\n", A + B);           // 13
printf("Difference: %d\n", A - B);    // 7
printf("Product: %d\n", A * B);       // 30
printf("Division: %d\n", A / B);      // 3 (integer division)
printf("Remainder: %d\n", A % B);     // 1

printf("Power: %.0f\n", pow(2, 3));   // 8.0
printf("Square root: %.2f\n", sqrt(16.0));  // 4.00
printf("Absolute: %d\n", abs(-5));    // 5
```

## Strings

### String Basics

```c
#include <string.h>

char STR1[] = "Hello";
char STR2[] = "World";

printf("String: %s\n", STR1);

// String length
printf("Length: %lu\n", strlen(STR1));  // 5

// Copy string
char COPY[50];
strcpy(COPY, STR1);
printf("Copy: %s\n", COPY);  // Hello

// Concatenate
strcat(STR1, " ");
strcat(STR1, STR2);
printf("Combined: %s\n", STR1);  // Hello World
```

### Character by Character

```c
char TEXT[] = "ABC";
int I;

for (I = 0; I < 3; I++) {
    printf("Char %d: %c\n", I, TEXT[I]);
}
```

**Output:**
```
Char 0: A
Char 1: B
Char 2: C
```

## Conditionals

### IF/ELSE

```c
int X = 10;

if (X > 5) {
    printf("X is greater than 5\n");
} else if (X == 5) {
    printf("X equals 5\n");
} else {
    printf("X is less than 5\n");
}
```

### Comparison Operators

```c
if (X == 5) ...     // Equal
if (X != 5) ...     // Not equal
if (X > 5) ...      // Greater
if (X < 5) ...      // Less
if (X >= 5) ...     // Greater or equal
if (X <= 5) ...     // Less or equal
```

### Logical Operators

```c
if (X > 5 && Y < 10) ...   // AND (both true)
if (X > 5 || Y < 10) ...   // OR (either true)
if (!(X == 5)) ...         // NOT
```

### Switch Statement

```c
int CHOICE;
scanf("%d", &CHOICE);

switch (CHOICE) {
    case 1:
        printf("You chose 1\n");
        break;
    case 2:
        printf("You chose 2\n");
        break;
    default:
        printf("Invalid choice\n");
}
```

## Loops

### FOR Loop

```c
int I;
for (I = 1; I <= 5; I++) {
    printf("I = %d\n", I);
}
```

**Output:**
```
I = 1
I = 2
I = 3
I = 4
I = 5
```

**Counting backwards:**
```c
for (I = 5; I >= 1; I--) {
    printf("%d\n", I);
}
```

### WHILE Loop

```c
int X = 1;
while (X <= 5) {
    printf("X = %d\n", X);
    X++;
}
```

### DO-WHILE Loop

```c
int X = 1;
do {
    printf("X = %d\n", X);
    X++;
} while (X <= 5);
```

Executes at least once.

## Arrays

### Single Dimension Array

```c
int NUMBERS[5] = {10, 20, 30, 40, 50};
printf("Element 0: %d\n", NUMBERS[0]);  // 10
printf("Element 2: %d\n", NUMBERS[2]);  // 30

// Modify
NUMBERS[1] = 25;
printf("Modified: %d\n", NUMBERS[1]);   // 25
```

### Array with Loop

```c
int ARR[5] = {5, 10, 15, 20, 25};
int I;

for (I = 0; I < 5; I++) {
    printf("ARR[%d] = %d\n", I, ARR[I]);
}
```

### 2D Array

```c
int MATRIX[3][3] = {
    {1, 2, 3},
    {4, 5, 6},
    {7, 8, 9}
};

printf("Element [1][2]: %d\n", MATRIX[1][2]);  // 6
```

## Functions

### Defining Functions

```c
int ADD(int A, int B) {
    return A + B;
}

int main() {
    int RESULT = ADD(3, 5);
    printf("Sum: %d\n", RESULT);
    return 0;
}
```

### Function with No Return

```c
void GREET(char NAME[]) {
    printf("Hello, %s!\n", NAME);
}

int main() {
    GREET("Alice");
    return 0;
}
```

### Multiple Parameters

```c
int MULTIPLY(int A, int B, int C) {
    return A * B * C;
}

int main() {
    printf("Product: %d\n", MULTIPLY(2, 3, 4));  // 24
    return 0;
}
```

### Recursive Functions

```c
int FACTORIAL(int N) {
    if (N <= 1)
        return 1;
    else
        return N * FACTORIAL(N - 1);
}

int main() {
    printf("5! = %d\n", FACTORIAL(5));  // 120
    return 0;
}
```

## Pointers

### Pointer Basics

```c
int X = 5;
int *PTR;           // Declare pointer
PTR = &X;           // Get address of X

printf("Value: %d\n", X);        // 5
printf("Pointer value: %d\n", *PTR);  // 5 (dereference)
printf("Address: %p\n", PTR);    // 0x... (address)
```

`&` = "address of"
`*` = "dereference" or "pointer to"

### Pointers with Functions

```c
void INCREMENT(int *NUM) {
    *NUM = *NUM + 1;
}

int main() {
    int X = 5;
    INCREMENT(&X);
    printf("X = %d\n", X);  // 6
    return 0;
}
```

## Comments

```c
// Single line comment

/* Block comment
   that spans
   multiple lines */

int X = 5;  // Inline comment
```

## Complete Example: Grade Calculator

```c
#include <stdio.h>

int main() {
    float SCORE1, SCORE2, SCORE3, AVERAGE;
    char GRADE;
    
    printf("Enter three scores: ");
    scanf("%f %f %f", &SCORE1, &SCORE2, &SCORE3);
    
    AVERAGE = (SCORE1 + SCORE2 + SCORE3) / 3;
    
    if (AVERAGE >= 90)
        GRADE = 'A';
    else if (AVERAGE >= 80)
        GRADE = 'B';
    else if (AVERAGE >= 70)
        GRADE = 'C';
    else
        GRADE = 'F';
    
    printf("Average: %.2f\n", AVERAGE);
    printf("Grade: %c\n", GRADE);
    
    return 0;
}
```

## Complete Example: Prime Checker

```c
#include <stdio.h>

int IS_PRIME(int N) {
    if (N <= 1) return 0;
    if (N <= 3) return 1;
    
    int I;
    for (I = 2; I * I <= N; I++) {
        if (N % I == 0)
            return 0;
    }
    return 1;
}

int main() {
    int NUM;
    
    printf("Check if prime: ");
    scanf("%d", &NUM);
    
    if (IS_PRIME(NUM))
        printf("%d is prime\n", NUM);
    else
        printf("%d is not prime\n", NUM);
    
    return 0;
}
```

## Common Commands Reference

### I/O
| Command | Purpose |
|---------|---------|
| `printf()` | Print formatted output |
| `scanf()` | Read formatted input |
| `getchar()` | Read single character |
| `putchar()` | Print single character |

### Math
| Function | Purpose |
|----------|---------|
| `abs()` | Absolute value |
| `sqrt()` | Square root |
| `pow()` | Power |
| `sin()`, `cos()` | Trigonometry |
| `rand()` | Random number |

### String
| Function | Purpose |
|----------|---------|
| `strlen()` | String length |
| `strcpy()` | Copy string |
| `strcat()` | Concatenate strings |
| `strcmp()` | Compare strings |

### Type Conversion
```c
int X = 5;
float F = (float) X / 2;    // Cast to float
char C = (char) 65;         // 'A'
```

## Tips for Learning C

1. **Include headers** - `#include` what you need
2. **Understand pointers** - They're powerful but tricky
3. **Check array bounds** - C doesn't check for you
4. **Use meaningful names** - Code is for humans too
5. **Test incrementally** - Build and test step by step
6. **Read compiler errors** - They point to real problems

## Common Mistakes

| Mistake | Problem | Fix |
|---------|---------|-----|
| `if (X = 5)` | Assignment, not comparison | Use `==` for comparison |
| Missing `return 0;` | Not following convention | Always return from main |
| `char STR[50]` without length | Might overflow | Count characters |
| Missing `&` with scanf | Compiler error | Use `&` for non-strings |
| Array out of bounds | Undefined behavior | Check loop condition |

## Next Steps

1. ✅ Learn variables and types
2. ✅ Master loops and conditionals
3. ✅ Build functions
4. ✅ Understand pointers
5. 📂 Try examples from `Examples/c/`
6. 🏗️ Build a complete program

---

Happy C coding! 💻
