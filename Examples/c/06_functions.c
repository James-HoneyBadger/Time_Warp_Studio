/*
 * Functions and Modular Programming
 * 
 * Demonstrates:
 * - Function declaration and definition
 * - Parameters and return values
 * - Void functions
 * - Multiple functions working together
 */

#include <stdio.h>

// Function declarations (prototypes)
int square(int x);
int max(int a, int b);
float celsius_to_fahrenheit(float c);
void print_banner(char *message);
int factorial(int n);

int main() {
    int num, result;
    float temp_c, temp_f;
    
    printf("=== Function Examples ===\n\n");
    
    // Using square function
    num = 5;
    result = square(num);
    printf("Square of %d = %d\n", num, result);
    
    // Using max function
    int a = 15, b = 23;
    printf("Max of %d and %d = %d\n", a, b, max(a, b));
    
    // Temperature conversion
    printf("\n");
    temp_c = 25.0;
    temp_f = celsius_to_fahrenheit(temp_c);
    printf("%.1f°C = %.1f°F\n", temp_c, temp_f);
    
    // Void function (no return value)
    printf("\n");
    print_banner("Welcome to C!");
    
    // Factorial calculation
    printf("\n=== Factorials ===\n");
    for (int i = 1; i <= 10; i++) {
        printf("%d! = %d\n", i, factorial(i));
    }
    
    return 0;
}

// Function definitions

int square(int x) {
    return x * x;
}

int max(int a, int b) {
    if (a > b) {
        return a;
    } else {
        return b;
    }
}

float celsius_to_fahrenheit(float c) {
    return (c * 9.0 / 5.0) + 32.0;
}

void print_banner(char *message) {
    int len = 0;
    
    // Calculate length
    while (message[len] != '\0') {
        len++;
    }
    
    // Print top border
    for (int i = 0; i < len + 4; i++) {
        printf("=");
    }
    printf("\n");
    
    // Print message
    printf("| %s |\n", message);
    
    // Print bottom border
    for (int i = 0; i < len + 4; i++) {
        printf("=");
    }
    printf("\n");
}

int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    int result = 1;
    for (int i = 2; i <= n; i++) {
        result *= i;
    }
    return result;
}
