# C Programming with Time Warp Studio

C is a powerful, fundamental programming language. Time Warp Studio provides experimental C support for those who want to explore systems programming concepts.

## Quick Start

A simple C program:

```c
#include <stdio.h>

int main() {
    printf("Hello, World!\n");
    return 0;
}
```

Run this:
1. Select "C" from the language dropdown
2. Paste the code
3. Click Run (Ctrl+R)

## Basic Syntax

### Structure

Every C program needs a `main()` function:

```c
#include <stdio.h>

int main() {
    // Your code here
    return 0;
}
```

### Headers and Libraries

```c
#include <stdio.h>   // Standard I/O (printf, scanf)
#include <stdlib.h>  // Standard library (malloc, free)
#include <math.h>    // Math functions (sin, cos, sqrt)
#include <string.h>  // String functions (strlen, strcpy)
```

### Comments

```c
// Single line comment

/*
 * Multi-line comment
 * can span multiple lines
 */
```

### Variables and Data Types

```c
int age = 25;           // Integer
float height = 5.9;     // Floating point
double pi = 3.14159;    // Double precision
char initial = 'A';     // Single character
```

### Output: printf()

```c
#include <stdio.h>

int main() {
    printf("Hello, World!\n");
    
    int age = 25;
    printf("Age: %d\n", age);
    
    float height = 5.9;
    printf("Height: %.1f\n", height);
    
    char name[] = "Alice";
    printf("Name: %s\n", name);
    
    return 0;
}
```

**Format Specifiers:**
- `%d` - integer
- `%f` - float/double
- `%.2f` - float with 2 decimal places
- `%s` - string
- `%c` - character
- `%x` - hexadecimal

### Input: scanf()

```c
#include <stdio.h>

int main() {
    int age;
    printf("Enter your age: ");
    scanf("%d", &age);
    printf("You are %d years old\n", age);
    
    float height;
    printf("Enter your height: ");
    scanf("%f", &height);
    printf("Your height is %.1f\n", height);
    
    return 0;
}
```

**Important**: Use `&` before variable name in scanf()

## Operators

### Arithmetic

```c
int a = 10, b = 3;

int sum = a + b;       // 13
int diff = a - b;      // 7
int product = a * b;   // 30
int quotient = a / b;  // 3
int remainder = a % b; // 1
```

### Comparison

```c
int x = 10;

if (x == 10)  printf("Equal\n");
if (x != 5)   printf("Not equal\n");
if (x > 5)    printf("Greater than\n");
if (x < 20)   printf("Less than\n");
if (x >= 10)  printf("Greater or equal\n");
if (x <= 15)  printf("Less or equal\n");
```

### Logical

```c
int age = 25;
int is_student = 1;  // 1 = true, 0 = false

if (age >= 18 && is_student) {
    printf("Adult student\n");
}

if (age < 18 || is_student) {
    printf("Young or student\n");
}

if (!is_student) {
    printf("Not a student\n");
}
```

## Control Flow

### if/else

```c
#include <stdio.h>

int main() {
    int score = 85;
    
    if (score >= 90) {
        printf("Grade: A\n");
    } else if (score >= 80) {
        printf("Grade: B\n");
    } else if (score >= 70) {
        printf("Grade: C\n");
    } else {
        printf("Grade: F\n");
    }
    
    return 0;
}
```

### switch Statement

```c
#include <stdio.h>

int main() {
    int choice = 2;
    
    switch (choice) {
        case 1:
            printf("Option 1\n");
            break;
        case 2:
            printf("Option 2\n");
            break;
        case 3:
            printf("Option 3\n");
            break;
        default:
            printf("Invalid option\n");
    }
    
    return 0;
}
```

### Loops

**for loop:**

```c
for (int i = 0; i < 5; i++) {
    printf("Number: %d\n", i);
}
```

**while loop:**

```c
int count = 0;
while (count < 5) {
    printf("Count: %d\n", count);
    count++;
}
```

**do-while loop:**

```c
int count = 0;
do {
    printf("Count: %d\n", count);
    count++;
} while (count < 5);
```

## Functions

### Basic Function

```c
#include <stdio.h>

void greet() {
    printf("Hello!\n");
}

int main() {
    greet();
    return 0;
}
```

### Function with Parameters

```c
#include <stdio.h>

void greet(char name[]) {
    printf("Hello, %s!\n", name);
}

int main() {
    greet("Alice");
    greet("Bob");
    return 0;
}
```

### Function with Return Value

```c
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int main() {
    int result = add(3, 5);
    printf("Result: %d\n", result);
    return 0;
}
```

## Arrays

### Arrays Basics

```c
#include <stdio.h>

int main() {
    int numbers[5] = {1, 2, 3, 4, 5};
    
    printf("First: %d\n", numbers[0]);
    printf("Last: %d\n", numbers[4]);
    
    // Loop through array
    for (int i = 0; i < 5; i++) {
        printf("numbers[%d] = %d\n", i, numbers[i]);
    }
    
    return 0;
}
```

### String Arrays

```c
#include <stdio.h>
#include <string.h>

int main() {
    char name[50];
    
    printf("Enter your name: ");
    scanf("%49s", name);
    
    printf("Hello, %s!\n", name);
    printf("Length: %lu\n", strlen(name));
    
    return 0;
}
```

## Complete Example Programs

### Simple Calculator

```c
#include <stdio.h>

int main() {
    float a, b;
    char operator;
    
    printf("=== Simple Calculator ===\n");
    printf("Enter first number: ");
    scanf("%f", &a);
    
    printf("Enter operator (+, -, *, /): ");
    scanf(" %c", &operator);
    
    printf("Enter second number: ");
    scanf("%f", &b);
    
    float result;
    switch (operator) {
        case '+':
            result = a + b;
            break;
        case '-':
            result = a - b;
            break;
        case '*':
            result = a * b;
            break;
        case '/':
            if (b != 0) {
                result = a / b;
            } else {
                printf("Cannot divide by zero\n");
                return 1;
            }
            break;
        default:
            printf("Invalid operator\n");
            return 1;
    }
    
    printf("Result: %.2f\n", result);
    return 0;
}
```

### Fibonacci Sequence

```c
#include <stdio.h>

int main() {
    int n;
    printf("How many Fibonacci numbers? ");
    scanf("%d", &n);
    
    int a = 0, b = 1;
    printf("Fibonacci sequence:\n");
    
    for (int i = 0; i < n; i++) {
        printf("%d ", a);
        int temp = a + b;
        a = b;
        b = temp;
    }
    printf("\n");
    
    return 0;
}
```

### Prime Number Checker

```c
#include <stdio.h>

int is_prime(int num) {
    if (num < 2) return 0;
    
    for (int i = 2; i * i <= num; i++) {
        if (num % i == 0) return 0;
    }
    return 1;
}

int main() {
    int n;
    printf("Enter a number: ");
    scanf("%d", &n);
    
    if (is_prime(n)) {
        printf("%d is prime\n", n);
    } else {
        printf("%d is not prime\n", n);
    }
    
    return 0;
}
```

## Tips and Best Practices

1. **Always initialize variables**: `int x = 0;`
2. **Check array bounds**: Don't access outside array size
3. **Free allocated memory**: Use `free()` after `malloc()`
4. **Use meaningful variable names**: `age` not `a`
5. **Add comments**: Explain complex logic

## Common Mistakes to Avoid

```c
// ❌ Wrong: Forgetting &
int age;
scanf("%d", age);  // Error!

// ✅ Right: Use &
scanf("%d", &age);

// ❌ Wrong: Array out of bounds
int arr[5] = {1, 2, 3};
printf("%d\n", arr[10]);  // Undefined behavior!

// ✅ Right: Check bounds
if (i < 5) {
    printf("%d\n", arr[i]);
}

// ❌ Wrong: Memory leak
int *ptr = malloc(100);
// ... forgot to free()

// ✅ Right: Free memory
int *ptr = malloc(100);
// Use ptr
free(ptr);
```

## Learning Resources

- **GCC Compiler**: Time Warp uses standard C compilation
- **Practice**: Start with simple I/O programs
- **Debug**: Use printf() to debug values
- **Experiment**: Modify examples to understand concepts

## Running C Programs in Time Warp Studio

1. Create a `.c` file with your program
2. Select "C" from the language dropdown
3. Paste your code or load the file
4. Click Run or press Ctrl+R
5. Interact with the program in the Output panel

## Next Steps

- Learn [BASIC for classic programming](basic.md)
- Explore [Python for general programming](python.md)
- Try [Pascal for structured programming](pascal.md)
- Learn [Logo for turtle graphics](logo.md)

Happy C programming!
